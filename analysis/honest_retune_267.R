# Honest re-tune on the regenerated 267-feature data ---------------------------
#
# POSTMORTEM Sec 5g (OPEN when this script was written; CLOSED by its sibling
# analysis/adopt_honest_tuning.R on 2026-09-02): the production hyperparameters came from
# models/xgboost_tuning_results.rds, dated 25 September 2025, produced by the
# original tuner -- which calls xgb.cv with the time-aware folds commented out
# in favour of random 5-fold. That trains on future seasons to validate on past
# ones and splits a match's three vote-getters across folds.
#
# The 2026-09-02 clean rerun regenerated the FEATURES correctly (leak
# assertions passed, 267 columns) but stage 05 logged "Loading cached tuning
# results" and reused those year-old parameters, tuned on 258 pre-leak-fix
# features. Same failure Pete caught on the in-season model: parameters chosen
# for one feature set carried silently onto another.
#
# This does NOT overwrite models/xgboost_tuning_results.rds. It writes its own
# file and then measures honest-vs-stale through the harness on the real 2025
# holdout. Adopting it into stage 05 is a separate decision, made on the number
# this prints rather than on the assumption that honest tuning must win.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

TUNE_OUT <- "models/xgboost_honest_tuning_267.rds"

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
stale    <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col

cat(sprintf("features: %d | train %d | test %d | holdout %d\n",
            length(feats), nrow(enhanced$train), nrow(enhanced$test), nrow(enhanced$holdout)))

train_all <- bind_rows(enhanced$train, enhanced$test)
stopifnot(max(train_all$season) < min(enhanced$holdout$season))

# ---- honest search over expanding-window folds -------------------------------
if (file.exists(TUNE_OUT)) {
  cat("loading cached honest tuning\n")
  honest <- readRDS(TUNE_OUT)
} else {
  param_grid <- list(
    max_depth = c(3, 4, 5, 6), eta = c(0.01, 0.025, 0.05, 0.1),
    subsample = c(0.6, 0.8, 1.0), colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
    min_child_weight = c(1, 3, 5), gamma = c(0, 0.1, 0.2),
    lambda = c(0.1, 1, 2), alpha = c(0, 0.1, 0.5)
  )
  # Over-request folds: create_time_aware_cv() drops the last one when there is
  # no season past it to validate on.
  honest <- tune_xgboost_honest(train_all, feats, target, param_grid,
                                n_candidates = 12, n_folds = 6,
                                max_rounds = 600, early_stopping_rounds = 40)
  saveRDS(honest, TUNE_OUT)
}

cat("\n=== stale (2025-09-25, random k-fold, 258 feats) ===\n")
print(stale$best_params[c("max_depth","eta","subsample","colsample_bytree")])
cat("\n=== honest (expanding window, 267 feats) ===\n")
print(honest$best_params[c("max_depth","eta","subsample","colsample_bytree")])
cat(sprintf("\nhonest mean fold mlogloss: %.5f\n", honest$best_score))

# ---- head to head on the real 2025 holdout -----------------------------------
holdout <- attach_holdout_actuals(enhanced$holdout)
stopifnot(!anyNA(holdout$match_id))

rounds_stale <- optimal_nrounds(train_all, feats, target, stale$best_params,
                                max_rounds = 600, early_stopping_rounds = 40, verbose = TRUE)$nrounds
rounds_honest <- optimal_nrounds(train_all, feats, target, honest$best_params,
                                 max_rounds = 600, early_stopping_rounds = 40, verbose = TRUE)$nrounds
cat(sprintf("rounds: stale %d, honest %d\n", rounds_stale, rounds_honest))

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, stale$best_params, rounds_stale),
                 train_all, holdout, "stale params (random k-fold, 258 feats)",
                 seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(feats, target, honest$best_params, rounds_honest),
                 train_all, holdout, "honest params (expanding window, 267 feats)",
                 seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = "stale params (random k-fold, 258 feats)")

write_csv(results, "analysis/harness/honest_retune_267.csv")
cat("\nwritten: analysis/harness/honest_retune_267.csv\n")
