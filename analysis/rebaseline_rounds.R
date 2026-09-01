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

candidates <- sort(unique(c(tuning$best_iteration, honest$nrounds, 600)))
results <- lapply(candidates, function(n) run(n, sprintf("%d rounds", n))) %>% bind_rows()

comparison <- compare_configs(results,
                              baseline_label = sprintf("%d rounds", tuning$best_iteration))
saveRDS(results, "analysis/harness/rebaseline_rounds_raw.rds")
write_csv(comparison, "analysis/harness/rebaseline_rounds.csv")
cat("\nwritten: analysis/harness/rebaseline_rounds.csv\n")
