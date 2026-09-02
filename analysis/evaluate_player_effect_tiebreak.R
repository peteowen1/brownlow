# Tiebreak: is "both" really better than player_coef alone? --------------------
#
# At 5 seeds: both 0.35755, player_coef only 0.35801. Difference 0.00046 against
# a 0.00078 threshold - a coin flip. Adopting "both" costs 10 expanding-window
# fits in the pipeline, so it needs to clear the bar before it goes in.
#
# 10 seeds halves the standard error of the mean, which is enough to resolve a
# difference this size if it is real.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

seeds <- 1:10

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
bias_tbl <- readRDS("data/processed/player_bias.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- enhanced$target_col

with_coef <- enhanced$feature_cols
bias_cols <- c("player_bias", "player_prior_games")

augmented <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(player_coef = replace_na(player_coef, 0)) %>%
  left_join(bias_tbl, by = c("player_name", "season")) %>%
  mutate(player_bias = replace_na(player_bias, 0),
         player_prior_games = replace_na(player_prior_games, 0))

train   <- augmented %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(augmented %>%
                                   dplyr::filter(season %in% enhanced$holdout_seasons))

run <- function(feats, label) {
  cat(sprintf("  %s\n", label))
  evaluate_seeds(xgb_fit_predict(feats, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, label, seeds = seeds, normalizer = "ipf")
}

results <- bind_rows(
  run(with_coef,              "player_coef only"),
  run(c(with_coef, bias_cols), "both (player_coef + player_bias)")
)

comparison <- compare_configs(results, baseline_label = "player_coef only")

# A paired view is stricter than comparing two means: the same seed gives both
# configs the same subsampling draws, so the difference is less noisy than the
# levels are.
cat(sprintf("\n%d seeds each. Difference %.5f, threshold %.5f.\n",
            length(seeds), diff(rev(results$rmse)), 2 * sqrt(mean(results$sd^2))))

write_csv(comparison, "analysis/harness/player_effect_tiebreak.csv")
cat("written: analysis/harness/player_effect_tiebreak.csv\n")
