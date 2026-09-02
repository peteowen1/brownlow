# Does a properly trained in-season model beat the failed hand-tuned tilt? -----
#
# analysis/in_count_update_value.R tested one global linear slope applied via
# probability tilting: mean weighted top-20 log-loss change -0.0229 (t=-1.16,
# p=0.28) against the pre-season-only baseline. Not significant, point estimate
# negative.
#
# Pete's alternative, 2026-09-02: train a real model WITH rolling in-season
# features (R/in_season_features.R) as inputs, and let it learn how much to
# trust momentum conditional on context, rather than one slope applied to
# everyone. This cannot go in the datathon submission — the features use the
# current season's own actual votes — but is legitimate for an in-play model.
#
# Same experimental design as the tilt test, so the comparison is a fair
# rematch, not a different question: CUTOFF = 14, front-half votes are the real
# known total (identical across every arm), only the back-half mechanism
# differs.
#
#   baseline arm  : back-half simulated from the pre-season-only model, as
#                   before (no in-season information at all)
#   stage-2 arm   : back-half simulated from a SEPARATE model trained on
#                   [pre-season features + in-season rolling features], with
#                   those in-season features FROZEN at their cutoff-14 values
#                   for every back-half row of a player (a single decision
#                   point, matching the tilt test's design — not a
#                   round-by-round path-dependent update, which is a
#                   reasonable next refinement but a different experiment)
#
# Training discipline: the stage-2 model is trained walk-forward (seasons < S)
# using each training row's NATURAL per-row-cumulative in-season features
# (their own true state at that point in that season) — freezing only applies
# to the evaluation season's back half, where a single cutoff is being tested.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

CUTOFF  <- 14      # identical to in_count_update_value.R, for a fair rematch
N_SIMS  <- 5000
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

# ---- stage 1: walk-forward base model + full probability matrix per season --
cat("=== stage 1: walk-forward base model ===\n")
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
    mutate(team_std = standardize_team_name(team),
           pred_votes = as.numeric(pm %*% c(0, 1, 2, 3)),
           raw_prob_0 = pm[, 1], raw_prob_1 = pm[, 2], raw_prob_2 = pm[, 3], raw_prob_3 = pm[, 4]) %>%
    left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
              by = c("date", "team_std" = "team", "player_name")) %>%
    mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
    dplyr::filter(!is.na(actual))
  cat(sprintf("  %d  (%d rounds)\n", S, rounds))
  te
}) %>% bind_rows()

# ---- in-season features for every row, natural per-row-cumulative -----------
season_data_is <- add_in_season_vote_features(season_data, pred_col = "pred_votes")
assert_in_season_clean(season_data_is)
inseason_cols <- grep("^inseason_", names(season_data_is), value = TRUE)
cat(sprintf("\nin-season columns: %d\n", length(inseason_cols)))

stage2_feats <- c(feats, inseason_cols)

# ---- per-season comparison ---------------------------------------------------
score_season <- function(S) {
  te <- season_data_is %>% dplyr::filter(season == S)
  max_r <- max(te$round_num)
  if (max_r < CUTOFF + 3) {
    cat(sprintf("  %d: skipped, season too short\n", S)); return(NULL)
  }

  front <- te %>% dplyr::filter(round_num <= CUTOFF)
  back  <- te %>% dplyr::filter(round_num > CUTOFF)

  front_totals <- front %>% group_by(player_name, team) %>%
    summarise(front_actual = sum(actual), .groups = "drop")

  # Freeze in-season features at their cutoff-14 state for every back-half row
  # of a player: the same value each of that player's remaining matches gets,
  # matching the single-decision-point design of the tilt test.
  frozen <- front %>% group_by(player_name, team) %>%
    summarise(dplyr::across(dplyr::all_of(inseason_cols), ~dplyr::last(.x)), .groups = "drop")
  back_frozen <- back %>%
    select(-dplyr::all_of(inseason_cols)) %>%
    left_join(frozen, by = c("player_name", "team")) %>%
    mutate(dplyr::across(dplyr::all_of(inseason_cols), ~tidyr::replace_na(.x, 0)))

  # Train stage 2 on every row from seasons < S, natural per-row features.
  # optimal_nrounds() needs >= 2 distinct seasons in train2 for its own internal
  # fit/validate split (val_season = max(train2$season), fit on everything
  # before it) - the season_data_is frame itself only starts at FIRST, so S ==
  # FIRST or FIRST+1 cannot be scored for stage 2 even though stage 1 covers
  # them.
  train2 <- season_data_is %>% dplyr::filter(season < S)
  if (dplyr::n_distinct(train2$season) < 2) {
    cat(sprintf("  %d: skipped for stage 2, fewer than 2 prior in-season-feature seasons\n", S))
    return(NULL)
  }
  rounds2 <- optimal_nrounds(train2, stage2_feats, target, tuning$best_params,
                             max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
  set.seed(1)
  m2 <- xgb.train(tuning$best_params,
                  xgb.DMatrix(as.matrix(train2[, stage2_feats]), label = train2[[target]]),
                  nrounds = rounds2, verbose = 0)
  pm2 <- as_prob_matrix(predict(m2, as.matrix(back_frozen[, stage2_feats])), nrow(back_frozen))

  back_baseline <- back %>% mutate(sim_prob_0 = raw_prob_0, sim_prob_1 = raw_prob_1,
                                   sim_prob_2 = raw_prob_2, sim_prob_3 = raw_prob_3)
  back_stage2   <- back_frozen %>% mutate(sim_prob_0 = pm2[, 1], sim_prob_1 = pm2[, 2],
                                          sim_prob_2 = pm2[, 3], sim_prob_3 = pm2[, 4])

  front_by_key <- setNames(front_totals$front_actual,
                           paste(front_totals$player_name, front_totals$team, sep = "_"))
  totals_from <- function(back_frame) {
    prep <- prepare_season_sim(back_frame, prob_prefix = "sim_prob_")
    sim  <- simulate_seasons_fast(prep, n_sims = N_SIMS, seed = S)
    all_keys  <- union(rownames(sim), names(front_by_key))
    front_vec <- front_by_key[all_keys]; front_vec[is.na(front_vec)] <- 0
    idx <- match(all_keys, rownames(sim))
    sim_full <- sim[idx, , drop = FALSE]; sim_full[is.na(sim_full)] <- 0
    rownames(sim_full) <- all_keys
    sim_full + front_vec
  }

  raw_dir <- readRDS(sprintf("analysis/harness/backtest_raw_%d.rds", S))
  ord <- raw_dir$finish_order

  eval_arm <- function(totals, label) {
    pp  <- position_probabilities_from_totals(totals, max_pos = MAX_POS)
    wll <- weighted_position_logloss(pp, ord)
    cat(sprintf("    %-9s weighted LL %.4f\n", label, wll$weighted_logloss))
    wll$weighted_logloss
  }

  cat(sprintf("  %d  (stage2 rounds=%d)\n", S, rounds2))
  base_ll <- eval_arm(totals_from(back_baseline), "baseline")
  s2_ll   <- eval_arm(totals_from(back_stage2), "stage2")

  tibble(season = S, baseline_wll = base_ll, stage2_wll = s2_ll,
         improvement = base_ll - s2_ll)
}

results <- lapply(seasons, score_season) %>% bind_rows()

cat("\n=== per season ===\n")
print(as.data.frame(results), row.names = FALSE, digits = 4)

cat("\n=== summary ===\n")
cat(sprintf("  seasons scored             : %d\n", nrow(results)))
cat(sprintf("  mean weighted LL, baseline : %.4f\n", mean(results$baseline_wll)))
cat(sprintf("  mean weighted LL, stage2   : %.4f\n", mean(results$stage2_wll)))
cat(sprintf("  mean improvement           : %+.4f\n", mean(results$improvement)))
cat(sprintf("  improved in                : %d of %d seasons\n", sum(results$improvement > 0), nrow(results)))
tt <- t.test(results$improvement)
cat(sprintf("  paired t-test on improvement: t = %.2f, p = %.3f\n", tt$statistic, tt$p.value))
cat(sprintf("\n  for reference, the tilt test scored: mean improvement -0.0229, t=-1.16, p=0.280\n"))

write_csv(results, "analysis/harness/in_season_model_value.csv")
cat("\nwritten: analysis/harness/in_season_model_value.csv\n")
