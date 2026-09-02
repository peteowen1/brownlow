# Should we pick the round count on the metric we are actually scored on? ------
#
# POSTMORTEM Sec 5k: the datathon scores RMSE, but optimal_nrounds() early-stops
# on validation_mlogloss and tune_xgboost_honest() ranks candidates by mean fold
# mlogloss. So the model is selected for an objective nobody scores.
#
# Same class of error as Sec 5j one level up: there the ensemble weight was
# fitted on the wrong ROWS, here the model is chosen by the wrong OBJECTIVE.
#
# This tests the cheap half -- the ROUND COUNT. Train once on seasons < 2024 to
# 600 rounds, then score the 2024 validation season at many truncations using
# predict(iterationrange=), which needs no retraining. Pick the argmin under
# each metric, then take both choices to the untouched 2025 holdout over 5
# seeds.
#
# Deliberately NOT assuming RMSE selection wins. mlogloss and RMSE are
# correlated, and in Sec 5j the two metrics agreed exactly once the rows were
# honest. A null here is a perfectly good answer and closes Sec 5k cheaply.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

VAL_SEASON <- 2024
MAX_ROUNDS <- 2500
ROUND_GRID <- seq(100, 2500, by = 100)

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col
params   <- tuning$best_params

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout)
fit <- all_data %>% dplyr::filter(season < VAL_SEASON)
val <- all_data %>% dplyr::filter(season == VAL_SEASON)
cat(sprintf("fit %d rows (seasons %s), validate %d rows (%d, %d matches)\n",
            nrow(fit), paste(range(fit$season), collapse = "-"),
            nrow(val), VAL_SEASON, dplyr::n_distinct(val$match_id)))
stopifnot(nrow(val) > 0, !anyNA(val$match_id))

set.seed(1)
m <- xgboost::xgb.train(params,
                        xgboost::xgb.DMatrix(as.matrix(fit[, feats]), label = fit[[target]]),
                        nrounds = MAX_ROUNDS, verbose = 0)

valx <- as.matrix(val[, feats])
y_val <- val[[target]]

mlogloss <- function(P, y) -mean(log(pmax(P[cbind(seq_along(y), y + 1L)], 1e-15)))

cat("\nscoring the validation season at each truncation (no retraining)\n")
curve <- lapply(ROUND_GRID, function(k) {
  # iterationrange's upper bound is capped at the rounds actually trained, so
  # c(1, k + 1) blows up at the top of the grid. The resulting off-by-one (k-1
  # vs k trees) is immaterial against 25-round grid steps.
  P <- as_prob_matrix(predict(m, valx, iterationrange = c(1, k)), nrow(val))
  votes <- normalize_votes_ipf(P, val$match_id)
  tibble(nrounds = k,
         val_mlogloss = mlogloss(P, y_val),
         val_rmse     = sqrt(mean((votes - y_val)^2)))
}) %>% bind_rows()

print(as.data.frame(curve), row.names = FALSE, digits = 6)

k_mll  <- curve$nrounds[which.min(curve$val_mlogloss)]
k_rmse <- curve$nrounds[which.min(curve$val_rmse)]
cat(sprintf("\n  mlogloss-optimal rounds: %d  (what the pipeline selects)\n", k_mll))
cat(sprintf("  RMSE-optimal rounds    : %d  (what the competition scores)\n", k_rmse))
if (k_mll == k_rmse) cat("  -> the two metrics AGREE; nothing to gain from switching.\n")

# ---- take both to the untouched 2025 holdout --------------------------------
holdout <- attach_holdout_actuals(enhanced$holdout)
train_all <- bind_rows(enhanced$train, enhanced$test)
stopifnot(max(train_all$season) < min(holdout$season))

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, params, k_mll),
                 train_all, holdout, sprintf("mlogloss-selected (%d rounds)", k_mll),
                 seeds = 1:5, normalizer = "ipf"),
  if (k_rmse != k_mll)
    evaluate_seeds(xgb_fit_predict(feats, target, params, k_rmse),
                   train_all, holdout, sprintf("RMSE-selected (%d rounds)", k_rmse),
                   seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = sprintf("mlogloss-selected (%d rounds)", k_mll))

write_csv(curve, "analysis/harness/rmse_vs_mlogloss_curve.csv")
write_csv(results, "analysis/harness/rmse_vs_mlogloss_selection.csv")
cat("\nwritten: analysis/harness/rmse_vs_mlogloss_{curve,selection}.csv\n")
