# Should the production model average over seeds? -------------------------------
#
# 05_xgboost_model.R trains ONE model at set.seed(42). With subsample = 0.8 and
# colsample_bytree = 0.6, the RNG materially changes the fit: measured seed sd
# on the 2025 holdout is ~0.00026, so the shipped predictions carry one draw of
# that randomness rather than its expectation.
#
# Averaging predictions across N seeds cuts the variance of that draw by
# roughly sqrt(N). It is the right SHAPE of idea by this session's arithmetic
# (POSTMORTEM Sec 5n): it touches every row slightly rather than fixing a rare
# profile, and it is a measurement-layer change, the category that has produced
# every gain banked so far (Sec 5g, 5j, 5k).
#
# THE COMPARISON THAT MATTERS. Not "does seed 42 beat seed 7" -- that is a coin
# flip and tells us nothing. The honest question is whether averaging beats the
# EXPECTED result of picking one seed arbitrarily, which is the mean of the
# individual seeds' RMSEs. The averaged arm has no seed variance by
# construction, which is itself part of the value: it makes the submission
# reproducible rather than contingent on an arbitrary constant.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

SEEDS <- 1:10

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col
params   <- tuning$best_params
nrounds  <- tuning$best_iteration

cat(sprintf("%d features, %d rounds, seeds %s\n",
            length(feats), nrounds, paste(range(SEEDS), collapse = "-")))

train_all <- bind_rows(enhanced$train, enhanced$test)
holdout   <- attach_holdout_actuals(enhanced$holdout)
stopifnot(max(train_all$season) < min(holdout$season), !anyNA(holdout$match_id))

dtrain <- xgboost::xgb.DMatrix(as.matrix(train_all[, feats]), label = train_all[[target]])
newx   <- as.matrix(holdout[, feats])

rmse_of <- function(P) {
  sqrt(mean((normalize_votes_ipf(P, holdout$match_id) - holdout$Actual)^2))
}

cat("\nfitting one model per seed...\n")
mats <- lapply(SEEDS, function(s) {
  set.seed(s)
  m <- xgboost::xgb.train(params, dtrain, nrounds = nrounds, verbose = 0)
  P <- as_prob_matrix(stats::predict(m, newx), nrow(holdout))
  cat(sprintf("  seed %2d: RMSE %.5f\n", s, rmse_of(P)))
  P
})

individual <- vapply(mats, rmse_of, numeric(1))

# Averaging PROBABILITIES, not the post-IPF votes: IPF is a nonlinear
# normalisation, so averaging after it would not preserve the constraints the
# averaged matrix still satisfies going in.
cat("\n=== averaging over the first N seeds ===\n")
curve <- lapply(c(2, 3, 5, 10), function(n) {
  P <- Reduce(`+`, mats[seq_len(n)]) / n
  tibble(n_seeds = n, averaged_rmse = rmse_of(P),
         mean_single = mean(individual[seq_len(n)]))
}) %>% bind_rows() %>%
  mutate(gain_vs_single = averaged_rmse - mean_single)
print(as.data.frame(curve), row.names = FALSE, digits = 6)

cat(sprintf("\nsingle-seed RMSE: mean %.5f, sd %.5f, range %.5f-%.5f\n",
            mean(individual), sd(individual), min(individual), max(individual)))
cat(sprintf("seed 42 is what ships today; the arbitrary choice is worth up to %.5f\n",
            max(individual) - min(individual)))
cat(sprintf("\n10-seed average: %.5f  (%+.5f vs the mean single seed)\n",
            curve$averaged_rmse[curve$n_seeds == 10],
            curve$gain_vs_single[curve$n_seeds == 10]))

write_csv(curve, "analysis/harness/seed_averaging_test.csv")
cat("\nwritten: analysis/harness/seed_averaging_test.csv\n")
