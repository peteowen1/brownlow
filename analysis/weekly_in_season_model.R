# Week-by-week in-season model, properly tuned ---------------------------------
#
# Pete's methodology, 2026-09-02, replacing two shortcuts in the first attempt
# (analysis/in_count_update_value.R / in_season_model_value.R):
#
#   1. That attempt reused hyperparameters tuned for the 259-feature pre-season
#      model on a 268-feature in-season model. colsample_bytree = 0.6 was
#      tuned for the smaller feature count; adding 9 more columns changes
#      every feature's chance of being sampled at a split without anyone
#      deciding that on purpose. Fixed here with R/honest_tuning.R, a leak-free
#      hyperparameter search reusing create_time_aware_cv() (already correct —
#      the bug was that the existing tuner never used it, POSTMORTEM §5g).
#
#   2. That attempt tested ONE fixed cutoff (round 14) to match an earlier
#      test's design. Pete's actual proposal was round-by-round: usable from
#      "every round after the first one comes in". This sweeps cutoffs
#      10/12/14/16/18 — the same grid the persistence regression used, for
#      direct comparability with that result.
#
# Cost control: the stage-2 model is trained ONCE per season (walk-forward),
# then evaluated at every cutoff by varying which rows are front/back and what
# the frozen in-season snapshot is — not refit per cutoff. N_SIMS reduced from
# 5000 to 2000 for this sweep (5 cutoffs x 7 seasons x 2 arms = 70 simulation
# calls); still enough to resolve the top 20 in a 200+ player field.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

CUTOFFS <- c(10, 12, 14, 16, 18)
N_SIMS  <- 2000
FIRST   <- 2017
MAX_POS <- 20

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- setdiff(enhanced$feature_cols, "player_coef")
target   <- enhanced$target_col

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(round_num = suppressWarnings(dplyr::if_else(match_round == "Opening Round", 0,
                                                      as.numeric(match_round))))
stopifnot(!anyNA(all_data$round_num))

actual_2025 <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                        show_col_types = FALSE) %>%
  transmute(date = as.Date(match_date, format = "%d/%m/%Y"),
            team = player_team_std, player_name = full_player_name, actual_votes = Actual)

seasons <- sort(unique(all_data$season))
seasons <- seasons[seasons >= FIRST]

# ---- stage 1: walk-forward pre-season model + probability matrix ------------
cat("=== stage 1: walk-forward pre-season model ===\n")
season_data <- lapply(seasons, function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  rounds <- optimal_nrounds(tr, feats, target, tuning$best_params,
                            max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
  set.seed(1)
  m <- xgb.train(tuning$best_params, xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = rounds, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))
  te <- te %>%
    mutate(team_std = standardize_team_name(team), pred_votes = as.numeric(pm %*% c(0, 1, 2, 3)),
           raw_prob_0 = pm[, 1], raw_prob_1 = pm[, 2], raw_prob_2 = pm[, 3], raw_prob_3 = pm[, 4]) %>%
    left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
              by = c("date", "team_std" = "team", "player_name")) %>%
    mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
    dplyr::filter(!is.na(actual))
  cat(sprintf("  %d  (%d rounds)\n", S, rounds))
  te
}) %>% bind_rows()

season_data_is <- add_in_season_vote_features(season_data, pred_col = "pred_votes")
assert_in_season_clean(season_data_is)
inseason_cols <- grep("^inseason_", names(season_data_is), value = TRUE)
stage2_feats  <- c(feats, inseason_cols)
cat(sprintf("stage-2 features: %d base + %d in-season = %d\n",
            length(feats), length(inseason_cols), length(stage2_feats)))

# ---- honest hyperparameter search for the stage-2 feature set ---------------
tune_cache <- "models/stage2_honest_tuning.rds"
if (file.exists(tune_cache)) {
  cat("\n=== loading cached stage-2 tuning ===\n")
  stage2_tune <- readRDS(tune_cache)
} else {
  cat("\n=== honest hyperparameter search (leak-free, expanding-window folds) ===\n")
  # Tune on everything before 2025, so 2025 stays a genuine held-out check.
  tune_data <- season_data_is %>% dplyr::filter(season < 2025)
  param_grid <- list(
    max_depth = c(3, 4, 5, 6), eta = c(0.01, 0.025, 0.05, 0.1, 0.2),
    subsample = c(0.4, 0.6, 0.8), colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
    min_child_weight = c(1, 3, 5), gamma = c(0, 0.1, 0.2),
    lambda = c(0.1, 1, 2), alpha = c(0, 0.1, 0.5)
  )
  stage2_tune <- tune_xgboost_honest(tune_data, stage2_feats, target, param_grid,
                                     n_candidates = 15, n_folds = 6,
                                     max_rounds = 1500, early_stopping_rounds = 50)
  saveRDS(stage2_tune, tune_cache)
}
stage2_params <- stage2_tune$best_params
cat(sprintf("\nstage-2 tuned params: depth=%s eta=%s subsample=%s colsample=%s (mean fold mlogloss %.5f)\n",
            stage2_params$max_depth, stage2_params$eta, stage2_params$subsample,
            stage2_params$colsample_bytree, stage2_tune$best_score))

# ---- per-season, per-cutoff comparison ---------------------------------------
score_season <- function(S) {
  te <- season_data_is %>% dplyr::filter(season == S)
  max_r <- max(te$round_num)

  train2 <- season_data_is %>% dplyr::filter(season < S)
  if (dplyr::n_distinct(train2$season) < 2) {
    cat(sprintf("  %d: skipped, fewer than 2 prior seasons\n", S)); return(NULL)
  }
  rounds2 <- optimal_nrounds(train2, stage2_feats, target, stage2_params,
                             max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
  set.seed(1)
  m2 <- xgb.train(stage2_params, xgb.DMatrix(as.matrix(train2[, stage2_feats]), label = train2[[target]]),
                  nrounds = rounds2, verbose = 0)
  cat(sprintf("  %d  (stage2 rounds=%d)\n", S, rounds2))

  raw_dir <- readRDS(sprintf("analysis/harness/backtest_raw_%d.rds", S))
  ord <- raw_dir$finish_order

  lapply(CUTOFFS[CUTOFFS < max_r - 2], function(cutoff) {
    front <- te %>% dplyr::filter(round_num <= cutoff)
    back  <- te %>% dplyr::filter(round_num > cutoff)

    front_totals <- front %>% group_by(player_name, team) %>%
      summarise(front_actual = sum(actual), .groups = "drop")

    frozen <- front %>% group_by(player_name, team) %>%
      summarise(dplyr::across(dplyr::all_of(inseason_cols), ~dplyr::last(.x)), .groups = "drop")
    back_frozen <- back %>% select(-dplyr::all_of(inseason_cols)) %>%
      left_join(frozen, by = c("player_name", "team")) %>%
      mutate(dplyr::across(dplyr::all_of(inseason_cols), ~tidyr::replace_na(.x, 0)))

    pm2 <- as_prob_matrix(predict(m2, as.matrix(back_frozen[, stage2_feats])), nrow(back_frozen))
    back_baseline <- back %>% mutate(sim_prob_0 = raw_prob_0, sim_prob_1 = raw_prob_1,
                                     sim_prob_2 = raw_prob_2, sim_prob_3 = raw_prob_3)
    back_stage2   <- back_frozen %>% mutate(sim_prob_0 = pm2[, 1], sim_prob_1 = pm2[, 2],
                                            sim_prob_2 = pm2[, 3], sim_prob_3 = pm2[, 4])

    front_by_key <- setNames(front_totals$front_actual,
                             paste(front_totals$player_name, front_totals$team, sep = "_"))
    totals_from <- function(back_frame) {
      prep <- prepare_season_sim(back_frame, prob_prefix = "sim_prob_")
      sim  <- simulate_seasons_fast(prep, n_sims = N_SIMS, seed = S * 100 + cutoff)
      all_keys  <- union(rownames(sim), names(front_by_key))
      front_vec <- front_by_key[all_keys]; front_vec[is.na(front_vec)] <- 0
      idx <- match(all_keys, rownames(sim))
      sim_full <- sim[idx, , drop = FALSE]; sim_full[is.na(sim_full)] <- 0
      rownames(sim_full) <- all_keys
      sim_full + front_vec
    }

    eval_arm <- function(totals) weighted_position_logloss(
      position_probabilities_from_totals(totals, max_pos = MAX_POS), ord)$weighted_logloss

    base_ll <- eval_arm(totals_from(back_baseline))
    s2_ll   <- eval_arm(totals_from(back_stage2))
    cat(sprintf("    cutoff %2d: baseline %.4f  stage2(tuned) %.4f  delta %+.4f\n",
                cutoff, base_ll, s2_ll, base_ll - s2_ll))

    tibble(season = S, cutoff = cutoff, baseline_wll = base_ll, stage2_wll = s2_ll,
           improvement = base_ll - s2_ll)
  }) %>% bind_rows()
}

results <- lapply(seasons, score_season) %>% bind_rows()

cat("\n=== by cutoff ===\n")
by_cut <- results %>% group_by(cutoff) %>%
  summarise(n = n(), mean_improvement = mean(improvement),
           se = sd(improvement) / sqrt(n()),
           t = mean(improvement) / (sd(improvement) / sqrt(n())), .groups = "drop")
print(as.data.frame(by_cut), row.names = FALSE, digits = 4)

cat("\n=== pooled ===\n")
tt <- t.test(results$improvement)
cat(sprintf("  observations: %d season-cutoff pairs\n", nrow(results)))
cat(sprintf("  mean improvement: %+.4f, se %.4f, t = %.2f, p = %.3f\n",
            mean(results$improvement), sd(results$improvement) / sqrt(nrow(results)),
            tt$statistic, tt$p.value))
cat(sprintf("\n  for reference: single-cutoff-14, stale hyperparameters: mean -0.0405, t=-4.02, p=0.007\n"))

write_csv(results, "analysis/harness/weekly_in_season_model.csv")
cat("\nwritten: analysis/harness/weekly_in_season_model.csv\n")
