# Hyperparameters chosen on the scored metric ----------------------------------
#
# The expensive half of POSTMORTEM Sec 5k. Round selection moved to RMSE in
# c2e2753 and the two metrics disagreed by 3x (400 vs ~1100 rounds), which is
# reason enough to ask whether the HYPERPARAMETERS disagree too --
# tune_xgboost_honest() still ranks candidates by mean fold mlogloss.
#
# Fair comparison, deliberately: both arms get their rounds chosen by
# metric = "rmse_ipf". If the incumbent were left on mlogloss rounds we would
# be re-measuring the round change already banked in c2e2753 rather than the
# hyperparameters.
#
# Cost: rmse_ipf cannot early-stop, so every fold trains to max_rounds. 12
# candidates x 4 folds = 48 full fits plus IPF scans, roughly two hours. That
# is why this is a standalone background script and not part of the chain.
#
# A null is a fine outcome and closes Sec 5k. mlogloss and RMSE are correlated;
# they agreed on the ensemble weight once the rows were honest (Sec 5j) and
# disagreed sharply on rounds. There is no strong prior either way here.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

TUNE_OUT   <- "models/xgboost_rmse_tuning_267.rds"
MAX_ROUNDS <- 1200
ROUND_GRID <- seq(300, 1200, by = 150)

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
incumbent <- readRDS("models/xgboost_tuning_results.rds")
feats  <- enhanced$feature_cols
target <- enhanced$target_col

train_all <- bind_rows(enhanced$train, enhanced$test)
stopifnot(max(train_all$season) < min(enhanced$holdout$season),
          "match_id" %in% names(train_all))

if (file.exists(TUNE_OUT)) {
  cat("loading cached RMSE tuning\n")
  rmse_tune <- readRDS(TUNE_OUT)
} else {
  param_grid <- list(
    max_depth = c(3, 4, 5, 6), eta = c(0.01, 0.025, 0.05, 0.1),
    subsample = c(0.6, 0.8, 1.0), colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
    min_child_weight = c(1, 3, 5), gamma = c(0, 0.1, 0.2),
    lambda = c(0.1, 1, 2), alpha = c(0, 0.1, 0.5)
  )
  # Over-request folds: create_time_aware_cv() drops the last when no season
  # follows it to validate on.
  rmse_tune <- tune_xgboost_honest(train_all, feats, target, param_grid,
                                   n_candidates = 12, n_folds = 5,
                                   max_rounds = MAX_ROUNDS, metric = "rmse_ipf",
                                   round_grid = ROUND_GRID)
  saveRDS(rmse_tune, TUNE_OUT)
}

cat("\n=== incumbent (tuned on mlogloss) ===\n")
print(incumbent$best_params[c("max_depth","eta","subsample","colsample_bytree")])
cat("\n=== RMSE-tuned ===\n")
print(rmse_tune$best_params[c("max_depth","eta","subsample","colsample_bytree")])
cat(sprintf("\nmean fold rmse_ipf: %.5f | median rounds across folds: %s\n",
            rmse_tune$best_score, rmse_tune$best_nrounds))

# ---- head to head on the untouched 2025 holdout ------------------------------
holdout <- attach_holdout_actuals(enhanced$holdout)
stopifnot(!anyNA(holdout$match_id))

# Both arms get RMSE-selected rounds, so this isolates the hyperparameters.
r_inc <- optimal_nrounds(train_all, feats, target, incumbent$best_params,
                         max_rounds = 2000, metric = "rmse_ipf", verbose = TRUE)$nrounds
r_new <- optimal_nrounds(train_all, feats, target, rmse_tune$best_params,
                         max_rounds = 2000, metric = "rmse_ipf", verbose = TRUE)$nrounds
cat(sprintf("rounds: incumbent %d, rmse-tuned %d\n", r_inc, r_new))

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, incumbent$best_params, r_inc),
                 train_all, holdout, sprintf("mlogloss-tuned params (%d rounds)", r_inc),
                 seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(feats, target, rmse_tune$best_params, r_new),
                 train_all, holdout, sprintf("RMSE-tuned params (%d rounds)", r_new),
                 seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = sprintf("mlogloss-tuned params (%d rounds)", r_inc))

write_csv(rmse_tune$all_candidates %>% dplyr::select(-params),
          "analysis/harness/rmse_hyperparameter_candidates.csv")
write_csv(results, "analysis/harness/rmse_hyperparameter_search.csv")
cat("\nwritten: analysis/harness/rmse_hyperparameter_{candidates,search}.csv\n")
