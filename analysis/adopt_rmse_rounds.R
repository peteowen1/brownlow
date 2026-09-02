# Select the production round count on the metric we are scored on -------------
#
# POSTMORTEM Sec 5k. optimal_nrounds() now supports metric = "rmse_ipf"; this
# runs it against the production configuration and writes the answer into
# models/xgboost_tuning_results.rds$best_iteration, which is what stage 05
# reads.
#
# Measured 2026-09-02, validating on 2024: mlogloss picks 400 rounds and then
# climbs steadily, while RMSE-after-IPF keeps improving to 1200 and is flat
# across 600-1500. On the 2025 holdout the RMSE-selected count scored 0.35648
# against 0.35690 for the mlogloss-selected one -- delta -0.00042 against a
# 0.00055 decidability bar, i.e. NOT proven, adopted because selecting on the
# metric the competition scores is the sound derivation.
#
# Note the flatness: 600 rounds scored 0.35638 on 2025, marginally BETTER than
# the 1200 the 2024 validation season chose. The argmin does not transfer
# precisely between seasons; what transfers is that anywhere in 600-1500 beats
# 400. Do not read the exact number as meaningful.

library(dplyr)
library(xgboost)
devtools::load_all()

MAX_ROUNDS <- 2000
# Timestamped, not a single fixed name: `if (!file.exists(BACKUP))` only ever
# captured the FIRST pre-overwrite state, so a second run -- which this
# script's own instructions invite after any retune -- silently skipped the
# backup and left the only recoverable copy pointing months back.
BACKUP <- sprintf("models/xgboost_tuning_results_pre_rmse_rounds_%s.rds",
                  format(Sys.time(), "%Y%m%d_%H%M%S"))

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
train_all <- bind_rows(enhanced$train, enhanced$test)

cat(sprintf("current best_iteration: %d (selected on mlogloss)\n", tuning$best_iteration))

res <- optimal_nrounds(train_all, enhanced$feature_cols, enhanced$target_col,
                       tuning$best_params, max_rounds = MAX_ROUNDS,
                       metric = "rmse_ipf", verbose = TRUE)

if (res$nrounds >= MAX_ROUNDS) {
  stop(sprintf("RMSE optimum hit the %d-round cap; raise MAX_ROUNDS before adopting.", MAX_ROUNDS))
}

if (!file.exists(BACKUP)) {
  saveRDS(tuning, BACKUP)
  cat(sprintf("backed up incumbent -> %s\n", BACKUP))
}

tuning$best_iteration <- as.integer(res$nrounds)
tuning$provenance <- c(tuning$provenance, list(
  rounds_metric     = "rmse_ipf",
  rounds_selected   = as.integer(res$nrounds),
  rounds_score      = res$score,
  rounds_adopted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  rounds_note       = "Sec 5k: mlogloss picked 400, RMSE picks ~1200; curve flat 600-1500"
))
saveRDS(tuning, "models/xgboost_tuning_results.rds")

cat(sprintf("\nadopted best_iteration: %d (rmse_ipf %.5f)\n", tuning$best_iteration, res$score))
cat("rerun 05, then analysis/ensemble_weight_walkforward.R, then 06.\n")
