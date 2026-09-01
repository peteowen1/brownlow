# Does player_coef help, and should its missing values be imputed? -------------
#
# The GAM's player random effect is only defined for players it was fitted on.
# Coverage after regenerating on 2015-2024:
#
#   train    2.1% missing
#   test    17.3% missing
#   holdout 32.6% missing   (277 distinct 2025 players)
#
# That is a train/serve skew, not just sparsity. xgboost learns a default branch
# direction for NA from the 2% of training rows where it occurs, then applies it
# to a third of the rows it actually has to predict.
#
# A player the GAM never saw has an expected random effect of exactly zero -
# that is what partial pooling shrinks an unknown group to - so imputing 0 is
# the principled choice rather than a convenience. This measures whether it
# matters.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5
seeds <- seq_len(N_SEEDS)

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- enhanced$target_col

with_coef    <- enhanced$feature_cols
without_coef <- setdiff(with_coef, "player_coef")

holdout <- attach_holdout_actuals(enhanced$holdout)
train   <- bind_rows(enhanced$train, enhanced$test) %>%
  dplyr::filter(!season %in% enhanced$holdout_seasons)

impute_zero <- function(d) mutate(d, player_coef = tidyr::replace_na(player_coef, 0))

cat(sprintf("train %d rows | holdout %d rows | %d seeds\n", nrow(train), nrow(holdout), N_SEEDS))
cat(sprintf("player_coef missing: train %.1f%%, holdout %.1f%%\n\n",
            100 * mean(is.na(train$player_coef)), 100 * mean(is.na(holdout$player_coef))))

run <- function(feats, tr, ho, label, normalizer = "ipf") {
  cat(sprintf("  %s ...\n", label))
  evaluate_seeds(xgb_fit_predict(feats, target, tuning$best_params, tuning$best_iteration),
                 tr, ho, label, seeds = seeds, normalizer = normalizer)
}

results <- bind_rows(
  run(without_coef, train, holdout, "no player_coef"),
  run(with_coef,    train, holdout, "player_coef, NA left as NA"),
  run(with_coef,    impute_zero(train), impute_zero(holdout), "player_coef, NA imputed to 0"),
  run(with_coef,    impute_zero(train), impute_zero(holdout),
      "player_coef imputed + scale-to-6", normalizer = "scale")
)

comparison <- compare_configs(results, baseline_label = "no player_coef")

dir.create("analysis/harness", showWarnings = FALSE, recursive = TRUE)
write_csv(comparison, "analysis/harness/player_coef_comparison.csv")
cat("\nwritten: analysis/harness/player_coef_comparison.csv\n")
