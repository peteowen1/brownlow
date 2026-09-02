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
                              val_season, max_rounds, early_stopping_rounds,
                              metric = "mlogloss") {
  param_str <- paste(names(params)[order(names(params))],
                     unlist(params[order(names(params))]), sep = "=", collapse = ",")
  paste(
    param_str,
    # Without this, a cached mlogloss answer would be served for an rmse_ipf
    # request — the two disagree by 3x on this data (400 vs 1200 rounds).
    sprintf("metric=%s", metric),
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
#'   (`metric = "mlogloss"` only; the RMSE search scans truncations instead)
#' @param metric Selection criterion. `"mlogloss"` early-stops on validation
#'   multinomial log-loss — the historic behaviour. `"rmse_ipf"` selects on
#'   RMSE after IPF normalisation, which is **what the datathon actually
#'   scores** (POSTMORTEM-2025.md Sec 5k).
#' @param match_id_col Column identifying the match, required by `"rmse_ipf"`
#'   because IPF normalises within a match
#' @param round_grid Truncations to evaluate under `"rmse_ipf"`; defaults to
#'   100-step increments up to `max_rounds`
#' @param cache_file Where to persist results
#' @param refresh Force a fresh search, ignoring any cached answer
#' @param verbose Print progress
#'
#' @section Choosing the metric:
#' These disagree, and not subtly. Measured 2026-09-02 on the 267-feature data
#' with the honest hyperparameters, validating on 2024: mlogloss bottoms at
#' **400** rounds and then climbs steadily (0.17085 -> 0.17874 by 2500), while
#' RMSE-after-IPF keeps improving to **1200** and is flat across 600-1500. So
#' log-loss stops the model at a third of the trees the scored metric wants.
#'
#' Past ~400 rounds the probability estimates get worse-calibrated while the
#' IPF'd point predictions keep improving. That is a real trade, not a free
#' win: anything downstream that consumes the PROBABILITIES rather than the
#' expected votes — the ensemble blend especially — may be affected, so
#' re-derive the ensemble weight after changing this.
#'
#' @return List with `nrounds`, `score`, `metric`, `cached` and the `key` used
#' @export
optimal_nrounds <- function(train_data, feature_cols, target_col, params,
                            val_season = NULL, max_rounds = 2000,
                            early_stopping_rounds = 50,
                            metric = c("mlogloss", "rmse_ipf"),
                            match_id_col = "match_id",
                            round_grid = NULL,
                            cache_file = "models/nrounds_cache.rds",
                            refresh = FALSE, verbose = TRUE) {

  metric <- match.arg(metric)
  if (is.null(val_season)) val_season <- max(train_data$season)
  fit_seasons <- sort(unique(train_data$season[train_data$season < val_season]))
  if (length(fit_seasons) == 0) {
    stop(sprintf("No seasons before %s to train on.", val_season))
  }

  key <- nrounds_cache_key(train_data, feature_cols, target_col, params,
                           val_season, max_rounds, early_stopping_rounds, metric)

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
    cat(sprintf("optimal_nrounds: searching on %s (fit %d-%d, validate %s, max %d)\n",
                metric, min(fit_seasons), max(fit_seasons), val_season, max_rounds))
  }

  dfit <- xgboost::xgb.DMatrix(as.matrix(fit[, feature_cols]), label = fit[[target_col]])
  dval <- xgboost::xgb.DMatrix(as.matrix(val[, feature_cols]), label = val[[target_col]])

  if (metric == "rmse_ipf") {
    if (!match_id_col %in% names(val)) {
      stop(sprintf("metric='rmse_ipf' needs a '%s' column; IPF normalises within a match.",
                   match_id_col))
    }
    if (anyNA(val[[match_id_col]])) stop("match ids must never be missing.")

    # No early stopping: the whole point is that the RMSE optimum sits well past
    # where log-loss would have stopped. Train once to max_rounds, then score
    # truncations via predict(iterationrange=) rather than refitting per point.
    model <- xgboost::xgb.train(params = params, data = dfit, nrounds = max_rounds, verbose = 0)

    if (is.null(round_grid)) round_grid <- seq(100, max_rounds, by = 100)
    round_grid <- sort(unique(pmin(round_grid, max_rounds)))
    valx <- as.matrix(val[, feature_cols])
    y_val <- val[[target_col]]

    scores <- vapply(round_grid, function(k) {
      # iterationrange's upper bound is capped at the rounds actually trained.
      pm <- as_prob_matrix(stats::predict(model, valx, iterationrange = c(1, k)), nrow(val))
      votes <- normalize_votes_ipf(pm, val[[match_id_col]])
      sqrt(mean((votes - y_val)^2))
    }, numeric(1))

    best <- which.min(scores)
    result <- list(nrounds = as.integer(round_grid[best]),
                   score   = as.numeric(scores[best]),
                   metric  = "rmse_ipf")

    if (verbose) {
      cat(sprintf("optimal_nrounds: %d rounds (rmse_ipf %.5f)%s\n",
                  result$nrounds, result$score,
                  if (result$nrounds >= max_rounds) "  [hit max_rounds - raise it]" else ""))
    }

    cache[[key]] <- result
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(cache, cache_file)
    return(c(result, list(cached = FALSE, key = key)))
  }

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
