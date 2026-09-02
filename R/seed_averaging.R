# Averaging the production model over seeds -------------------------------------
#
# With subsample = 0.8 and colsample_bytree = 0.6, the RNG materially changes
# the fit. Measured on the 2025 holdout across 10 seeds: sd 0.00060, range
# 0.35464 to 0.35664 — a **spread of 0.00200**.
#
# That is larger than every improvement banked in this project put together
# (POSTMORTEM Sec 5g/5j/5k: 0.00042, 0.00055, 0.00042). Until now the shipped
# submission was a single draw at `set.seed(42)`, so whether that arbitrary
# constant happened to be a lucky number mattered more than any modelling
# decision made about it. That is not a defensible way to ship.
#
# Averaging the probability matrices removes the lottery. It also beats the
# expected single seed outright — the comparison that matters is against the
# MEAN of the individual seeds, not against a cherry-picked one:
#
#   seeds  averaged  mean single   gain
#       2   0.35495      0.35511   -0.00016
#       3   0.35520      0.35542   -0.00022
#       5   0.35533      0.35559   -0.00026
#      10   0.35554      0.35581   -0.00028
#
# Diminishing, as variance reduction should be: most of the benefit arrives by
# five seeds. The cost is linear in seeds — ~6 minutes per fit here.
#
# Average the PROBABILITIES, not the post-IPF votes. IPF is a nonlinear
# normalisation; an averaged probability matrix is still a valid input to it,
# whereas averaging afterwards would not preserve the constraints.

#' Train an XGBoost Model Averaged Over Seeds
#'
#' @description
#' Fits `length(seeds)` models differing only in RNG seed and returns their
#' mean predicted probability matrix, plus the first model for anything needing
#' a single booster (variable importance, for instance).
#'
#' @param train_data Training data
#' @param newdata Data to predict
#' @param feature_cols Features to train on
#' @param target_col Target column
#' @param params xgboost parameter list
#' @param nrounds Boosting rounds
#' @param seeds Integer vector of seeds to average over
#' @param verbose Print per-seed progress
#'
#' @return List with `prob_matrix` (the average), `model` (the first fit),
#'   `n_seeds` and `seeds`
#' @export
train_seed_averaged_xgboost <- function(train_data, newdata, feature_cols, target_col,
                                        params, nrounds, seeds = 1:5, verbose = TRUE) {
  if (length(seeds) < 1) stop("Need at least one seed.")
  if (anyDuplicated(seeds)) stop("Seeds must be distinct; duplicates add cost and no variance reduction.")

  dtrain <- xgboost::xgb.DMatrix(as.matrix(train_data[, feature_cols]),
                                 label = train_data[[target_col]])
  newx <- as.matrix(newdata[, feature_cols])

  total <- NULL
  first_model <- NULL
  for (s in seeds) {
    set.seed(s)
    m <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
    if (is.null(first_model)) first_model <- m
    pm <- as_prob_matrix(stats::predict(m, newx), nrow(newdata))
    total <- if (is.null(total)) pm else total + pm
    if (verbose) cat(sprintf("  seed %d fitted\n", s))
  }

  prob_matrix <- total / length(seeds)

  # Averaging preserves the row-sum-to-one property, but check rather than
  # assume: a silently unnormalised matrix would flow into IPF and produce
  # plausible nonsense.
  #
  # Tolerance is 1e-6, not 1e-8. xgboost computes softprob in single precision,
  # so individual rows already sum to 1 only within ~1e-7 before any averaging
  # — a 1e-8 bound failed on a measured 7.23e-08, which was float accumulation
  # rather than a real defect. 1e-6 still catches what this guard is for: a
  # genuinely unnormalised matrix is off by orders of magnitude, not by parts
  # per ten million.
  rs <- rowSums(prob_matrix)
  if (any(abs(rs - 1) > 1e-6)) {
    stop(sprintf("Averaged probabilities do not sum to 1 (worst row off by %.2e).",
                 max(abs(rs - 1))))
  }

  list(prob_matrix = prob_matrix, model = first_model,
       n_seeds = length(seeds), seeds = seeds)
}
