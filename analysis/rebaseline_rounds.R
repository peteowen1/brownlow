# Re-baseline: is 454 rounds actually better than 355? ------------------------
#
# Every measurement this session used nrounds = 355, which came from the tuner's
# random 5-fold xgb.cv on time-series data. Walk-forward early stopping (train
# 2015-2023, validate 2024) says 454.
#
# 355 was also chosen on 68k training rows while the production model now trains
# on 87k, so more rounds is the expected direction rather than a surprise.
#
# If 454 wins, the baseline moves and every delta measured today needs
# re-checking against it before anything ships.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  mutate(player_coef = replace_na(player_coef, 0))

train   <- all_data %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(all_data %>%
                                    dplyr::filter(season %in% enhanced$holdout_seasons))

honest <- optimal_nrounds(train, feats, target, tuning$best_params,
                          max_rounds = 1500, early_stopping_rounds = 50)

cat(sprintf("\ntuner (random-fold xgb.cv) : %d rounds\n", tuning$best_iteration))
cat(sprintf("walk-forward early stopping: %d rounds\n\n", honest$nrounds))

run <- function(n, label) {
  cat(sprintf("  %s\n", label))
  evaluate_seeds(xgb_fit_predict(feats, target, tuning$best_params, n),
                 train, holdout, label, seeds = seq_len(N_SEEDS), normalizer = "ipf")
}

candidates <- sort(unique(c(tuning$best_iteration, honest$nrounds)))
results <- lapply(candidates, function(n) run(n, sprintf("%d rounds", n))) %>% bind_rows()

# --- within-match rank features, at the better round count -------------------
# Never properly tested. Measured once single-seed at +0.0007, before the harness
# established that the noise floor is ~0.0008 - so it has always sat in the
# "looked promising, never confirmed" bucket.
#
# The Brownlow is decided WITHIN a match: 30 disposals means something different
# depending on whether it led the ground or came fourth. Absolute counts cannot
# express that; rank, percentile and gap-to-best can.
best_n <- honest$nrounds
rank_src <- intersect(c("afl_fantasy_score", "supercoach_score", "rating_points",
                        "coaches_votes", "disposals", "contested_possessions", "goals",
                        "score_involvements", "clearances", "effective_disposals",
                        "metres_gained", "impact_score", "midfield_score", "tackles",
                        "marks"), names(all_data))

add_match_ranks <- function(d) {
  g <- d %>% group_by(match_id)
  for (v in rank_src) {
    g <- g %>% mutate(
      !!paste0("mrank_", v) := dplyr::min_rank(-.data[[v]]),
      !!paste0("mpct_",  v) := dplyr::percent_rank(.data[[v]]),
      !!paste0("mgap_",  v) := .data[[v]] - max(.data[[v]], na.rm = TRUE))
  }
  ungroup(g)
}
rank_cols <- as.vector(outer(c("mrank_", "mpct_", "mgap_"), rank_src, paste0))

ranked  <- add_match_ranks(all_data)
train_r <- ranked %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
hold_r  <- attach_holdout_actuals(ranked %>% dplyr::filter(season %in% enhanced$holdout_seasons))

cat(sprintf("\nwithin-match rank features: %d columns from %d metrics\n",
            length(rank_cols), length(rank_src)))
results <- bind_rows(results,
  evaluate_seeds(xgb_fit_predict(c(feats, rank_cols), target, tuning$best_params, best_n),
                 train_r, hold_r, sprintf("%d rounds + within-match ranks", best_n),
                 seeds = seq_len(N_SEEDS), normalizer = "ipf"))

comparison <- compare_configs(results,
                              baseline_label = sprintf("%d rounds", tuning$best_iteration))
saveRDS(results, "analysis/harness/rebaseline_rounds_raw.rds")
write_csv(comparison, "analysis/harness/rebaseline_rounds.csv")
cat("\nwritten: analysis/harness/rebaseline_rounds.csv\n")
