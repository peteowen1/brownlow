# Do last-season-only (not pooled) features help? ------------------------------
#
# calculate_prior_season_performance_averages() already pools every prior
# season together, so a player's 10th season and their 1st weigh identically.
# calculate_last_season_performance() adds a distinct signal: just the
# immediately preceding season, which "was he good last year" wants and a
# career-long pool dilutes — plus coaches votes and Brownlow votes, which the
# pooled function does not cover at all.
#
# Tested here against the base feature set that has NOT yet had player_coef
# (04) or the GAM refit (03) touched, since these new columns aren't referenced
# by the GAM formula and don't require it — a fast go/no-go before committing
# to a full pipeline rebuild, same pattern as the season-to-date/rank feature
# tests earlier this session.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5

modeling <- readRDS("data/processed/modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- modeling$target_col
feats    <- modeling$feature_cols

new_cols <- grep("^last_season_|^played_last_season$", feats, value = TRUE)
cat(sprintf("last-season columns present in feature_cols: %d\n", length(new_cols)))
print(new_cols)
base_feats <- setdiff(feats, new_cols)

train   <- modeling$train
holdout <- attach_holdout_actuals(modeling$holdout)

run <- function(f, label) {
  cat(sprintf("  %s\n", label))
  evaluate_seeds(xgb_fit_predict(f, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, label, seeds = seq_len(N_SEEDS), normalizer = "ipf")
}

results <- bind_rows(
  run(base_feats, "without last-season features"),
  run(feats,      "with last-season features")
)

comparison <- compare_configs(results, baseline_label = "without last-season features")
write_csv(comparison, "analysis/harness/last_season_features.csv")
cat("\nwritten: analysis/harness/last_season_features.csv\n")

# ---- variable importance: does the model actually use them? -----------------
cat("\n=== variable importance, with last-season features ===\n")
set.seed(1)
m <- xgb.train(tuning$best_params,
               xgb.DMatrix(as.matrix(train[, feats]), label = train[[target]]),
               nrounds = tuning$best_iteration, verbose = 0)
imp <- xgboost::xgb.importance(model = m)
tot <- sum(imp$Gain)
ls_imp <- imp %>% dplyr::filter(Feature %in% new_cols)
cat(sprintf("last-season features: %.2f%% of total gain, %d of %d used, best rank %s\n",
            100 * sum(ls_imp$Gain) / tot, nrow(ls_imp), length(new_cols),
            if (nrow(ls_imp)) which(imp$Feature == ls_imp$Feature[1]) else "NA"))
print(as.data.frame(ls_imp %>% mutate(pct_gain = 100 * Gain / tot,
                                      overall_rank = match(Feature, imp$Feature))),
      row.names = FALSE, digits = 3)
