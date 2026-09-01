# Choosing the number of boosting rounds, honestly and once ---------------------
#
# Two problems this solves.
#
# 1. LEAKAGE. The existing tuner calls xgb.cv with `folds = cv_folds` commented
#    out and `nfold = 5, stratified = TRUE`, i.e. random k-fold on time-series
#    data — training on future seasons to validate on past ones, and splitting a
#    match's three vote-getters across folds. Uncommenting that line does not fix
#    it: xgb.cv's `folds` argument supplies only VALIDATION indices and trains on
#    everything else, whereas expanding-window validation must train on prior
#    seasons only. So this does the walk-forward split explicitly instead.
#
# 2. COST. Early stopping means fitting to `max_rounds` rather than to the answer,
#    which is the most expensive single step in the pipeline. The result is cached
#    against a key describing the data and parameters, so an unchanged setup pays
#    for it once.

#' Cache Key for a Rounds Search
#'
#' Describes everything that could change the answer: the parameters, the feature
#' set, the size and target distribution of the training data, the seasons it
#' spans, and the search settings. Deliberately human-readable so a stale cache
#' entry can be diagnosed by looking at it.
#'
#' @keywords internal
nrounds_cache_key <- function(train_data, feature_cols, target_col, params,
                              val_season, max_rounds, early_stopping_rounds) {
  param_str <- paste(names(params)[order(names(params))],
                     unlist(params[order(names(params))]), sep = "=", collapse = ",")
  paste(
    param_str,
    sprintf("nfeat=%d", length(feature_cols)),
    sprintf("featsum=%s", substr(rlang::hash(sort(feature_cols)), 1, 12)),
    sprintf("nrow=%d", nrow(train_data)),
    sprintf("target=%s", paste(as.integer(table(train_data[[target_col]])), collapse = "/")),
    sprintf("seasons=%s", paste(range(train_data$season), collapse = "-")),
    sprintf("val=%s", val_season),
    sprintf("max=%d,esr=%d", max_rounds, early_stopping_rounds),
    sep = " | "
  )
}

#' Find the Optimal Number of Boosting Rounds by Walk-Forward Early Stopping
#'
#' Trains on every season before `val_season`, watches `val_season`, and stops
#' when the validation metric stops improving. No future season is ever used to
#' choose the round count.
#'
#' The answer is cached in `cache_file` against a key covering the parameters,
#' features, data shape and search settings, so repeat calls on an unchanged
#' setup are free. Pass `refresh = TRUE` to force a fresh search.
#'
#' @param train_data Data with a `season` column
#' @param feature_cols Features to train on
#' @param target_col Target column
#' @param params xgboost parameter list
#' @param val_season Season to validate on; defaults to the most recent in `train_data`
#' @param max_rounds Upper bound for the search
#' @param early_stopping_rounds Rounds without improvement before stopping
#' @param cache_file Where to persist results
#' @param refresh Force a fresh search, ignoring any cached answer
#' @param verbose Print progress
#'
#' @return List with `nrounds`, `score`, `cached` and the `key` used
#' @export
optimal_nrounds <- function(train_data, feature_cols, target_col, params,
                            val_season = NULL, max_rounds = 2000,
                            early_stopping_rounds = 50,
                            cache_file = "models/nrounds_cache.rds",
                            refresh = FALSE, verbose = TRUE) {

  if (is.null(val_season)) val_season <- max(train_data$season)
  fit_seasons <- sort(unique(train_data$season[train_data$season < val_season]))
  if (length(fit_seasons) == 0) {
    stop(sprintf("No seasons before %s to train on.", val_season))
  }

  key <- nrounds_cache_key(train_data, feature_cols, target_col, params,
                           val_season, max_rounds, early_stopping_rounds)

  cache <- if (file.exists(cache_file)) readRDS(cache_file) else list()
  if (!refresh && !is.null(cache[[key]])) {
    if (verbose) {
      cat(sprintf("optimal_nrounds: cached -> %d rounds (%s %.5f)\n",
                  cache[[key]]$nrounds, cache[[key]]$metric, cache[[key]]$score))
    }
    return(c(cache[[key]], list(cached = TRUE, key = key)))
  }

  fit <- train_data[train_data$season %in% fit_seasons, ]
  val <- train_data[train_data$season == val_season, ]
  if (nrow(val) == 0) stop(sprintf("No rows for validation season %s.", val_season))

  if (verbose) {
    cat(sprintf("optimal_nrounds: searching (fit %d-%d, validate %s, max %d)\n",
                min(fit_seasons), max(fit_seasons), val_season, max_rounds))
  }

  dfit <- xgboost::xgb.DMatrix(as.matrix(fit[, feature_cols]), label = fit[[target_col]])
  dval <- xgboost::xgb.DMatrix(as.matrix(val[, feature_cols]), label = val[[target_col]])

  model <- xgboost::xgb.train(
    params = params, data = dfit, nrounds = max_rounds,
    evals = list(validation = dval),
    early_stopping_rounds = early_stopping_rounds,
    verbose = 0
  )

  # Booster objects are ALTREP-backed from xgboost 3.0, so `model$evaluation_log`
  # and `model$best_iteration` both return NULL rather than erroring — which
  # silently produces a zero-length round count. The log lives in the object's
  # attributes instead.
  log <- as.data.frame(attributes(model)$evaluation_log)
  score_col <- grep("^validation", names(log), value = TRUE)[1]
  if (is.na(score_col) || nrow(log) == 0) {
    stop(sprintf("No validation column in the evaluation log; got: %s",
                 paste(names(log), collapse = ", ")))
  }

  scores <- log[[score_col]]
  # Every metric we use here — mlogloss, rmse — is minimised.
  best <- which.min(scores)

  result <- list(nrounds = as.integer(best),
                 score   = as.numeric(scores[best]),
                 metric  = score_col)

  if (verbose) {
    cat(sprintf("optimal_nrounds: %d rounds (%s %.5f)%s\n",
                result$nrounds, result$metric, result$score,
                if (result$nrounds >= max_rounds) "  [hit max_rounds — raise it]" else ""))
  }

  cache[[key]] <- result
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(cache, cache_file)

  c(result, list(cached = FALSE, key = key))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
