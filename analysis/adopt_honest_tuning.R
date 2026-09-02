# Adopt the honestly-tuned hyperparameters into the production path -----------
#
# Decision, 2026-09-02, on the numbers in analysis/harness/honest_retune_267.csv:
#
#   honest (expanding window, 267 feats)  RMSE 0.35700
#   stale  (random k-fold,   258 feats)  RMSE 0.35741   delta -0.00042
#   pooled seed sd 0.00026 -> decidable only above 0.00052
#
# So the gain is NOT proven -- it lands at 80% of the decidability bar. Adopted
# anyway, because the two configs are statistically indistinguishable and only
# one of them has a sound derivation: the incumbent came from a tuner that calls
# xgb.cv with the time-aware folds commented out in favour of random 5-fold,
# i.e. trained on future seasons to validate on past ones (POSTMORTEM Sec 5g).
# When the measurement cannot separate two options, prefer the one that isn't
# built on a known leak. Three independent checks also agree on direction:
# honest wins its own CV, the 2024 validation mlogloss (0.17143 vs 0.17153),
# and the 2025 holdout RMSE.
#
# best_iteration is set to 391 -- the optimal_nrounds() answer for these params
# on the same fit/validate split the head-to-head used, so the production model
# matches the configuration that was actually evaluated, rather than the median
# across tuning folds (which were fitted on smaller, earlier training sets).

library(dplyr)
devtools::load_all()

TUNING_FILE <- "models/xgboost_tuning_results.rds"
BACKUP_FILE <- "models/xgboost_tuning_results_stale_20250925.rds"
HONEST_FILE <- "models/xgboost_honest_tuning_267.rds"
HONEST_NROUNDS <- 391L

stale  <- readRDS(TUNING_FILE)
honest <- readRDS(HONEST_FILE)

if (!file.exists(BACKUP_FILE)) {
  saveRDS(stale, BACKUP_FILE)
  cat(sprintf("backed up incumbent -> %s\n", BACKUP_FILE))
} else {
  cat(sprintf("backup already present, not overwriting: %s\n", BACKUP_FILE))
}

adopted <- list(
  best_params    = honest$best_params,
  best_cv_score  = honest$best_score,
  best_iteration = HONEST_NROUNDS,
  all_results    = honest$all_candidates,
  model_type     = stale$model_type,
  provenance     = list(
    tuned_on      = "2026-09-02",
    method        = "tune_xgboost_honest(), expanding-window folds (R/honest_tuning.R)",
    n_features    = 267,
    n_candidates  = 12,
    n_folds       = 6,
    holdout_rmse  = 0.35700,
    replaced      = "2025-09-25 random k-fold tuning on 258 pre-leak-fix features",
    replaced_rmse = 0.35741,
    note          = "delta -0.00042 vs a 0.00052 decidability bar: within noise, adopted on derivation not measurement"
  )
)

saveRDS(adopted, TUNING_FILE)

cat("\n=== written to", TUNING_FILE, "===\n")
cat(sprintf("best_cv_score : %.5f  (was %.5f)\n", adopted$best_cv_score, stale$best_cv_score))
cat(sprintf("best_iteration: %d      (was %d)\n", adopted$best_iteration, stale$best_iteration))
cat("best_params:\n")
print(adopted$best_params[c("max_depth", "eta", "subsample", "colsample_bytree")])
cat("\nrerun 05 and 06 to regenerate models and the ensemble on these parameters.\n")
