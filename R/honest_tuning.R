# Leak-free hyperparameter search --------------------------------------------
#
# tune_xgboost_hyperparameters() (R/model_utils.R) calls xgb.cv with the
# time-aware folds commented out in favour of random 5-fold — see
# POSTMORTEM-2025.md §5g. create_time_aware_cv() itself is correct (expanding
# window: train on seasons up to i, validate on season i+1); the bug was that
# the tuner never used it.
#
# Needed here specifically because reusing hyperparameters tuned for the
# 259-feature pre-season model on a 268-feature in-season model is suspect:
# colsample_bytree = 0.6 was tuned for the smaller feature count, and adding 9
# more columns changes every feature's chance of being considered at a given
# split without anyone deciding that on purpose.
#
# This is a modest random search (a handful of candidates over a handful of
# honest folds), not the original tuner's full grid — an honest search over 20
# points beats a fast search over 14,580 leaky ones for the same compute
# budget, but it is still a search, not exhaustive, and is documented as such.

#' Tune XGBoost Hyperparameters on Honest Expanding-Window Folds
#'
#' @param train_data Data with a `season` column
#' @param feature_cols Features to train on
#' @param target_col Target column
#' @param param_grid Named list of candidate values per hyperparameter
#' @param n_candidates Number of random draws from `param_grid`
#' @param n_folds Number of expanding-window folds from `create_time_aware_cv()`
#' @param max_rounds Upper bound per fold's fit
#' @param early_stopping_rounds Rounds without improvement before stopping
#'   (`metric = "mlogloss"` only)
#' @param metric Selection criterion. `"mlogloss"` ranks candidates by mean
#'   fold log-loss. `"rmse_ipf"` ranks them by mean fold RMSE after IPF — what
#'   the datathon scores (POSTMORTEM-2025.md Sec 5k).
#' @param match_id_col Column identifying the match, required by `"rmse_ipf"`
#' @param round_grid Truncations scanned per fold under `"rmse_ipf"`
#' @param seed RNG seed for the random draws
#' @param verbose Print progress
#'
#' @section Cost:
#' `"rmse_ipf"` cannot early-stop — the RMSE optimum sits well past where
#' log-loss would have halted — so every fold trains to `max_rounds`. That is
#' roughly `n_candidates * n_folds` full fits plus IPF scans, i.e. hours rather
#' than minutes. Budget accordingly and run it in the background.
#'
#' @return List with `best_params`, `best_score` (mean fold score under
#'   `metric`), `best_nrounds` (median across folds, a starting point —
#'   `optimal_nrounds()` should still be used per evaluation season), and
#'   `all_candidates` (every candidate tried, for inspection)
#' @export
tune_xgboost_honest <- function(train_data, feature_cols, target_col, param_grid,
                                n_candidates = 20, n_folds = 4, max_rounds = 1500,
                                early_stopping_rounds = 50,
                                metric = c("mlogloss", "rmse_ipf"),
                                match_id_col = "match_id", round_grid = NULL,
                                seed = 1, verbose = TRUE) {

  metric <- match.arg(metric)
  if (metric == "rmse_ipf" && !match_id_col %in% names(train_data)) {
    stop(sprintf("metric='rmse_ipf' needs a '%s' column; IPF normalises within a match.",
                 match_id_col))
  }
  if (is.null(round_grid)) round_grid <- seq(200, max_rounds, by = 100)

  folds <- create_time_aware_cv(train_data, target_col = target_col,
                                feature_cols = feature_cols, n_folds = n_folds)
  if (verbose) {
    cat(sprintf("honest tuning: %d folds (expanding window), %d candidates\n",
                length(folds), n_candidates))
    for (i in seq_along(folds)) {
      cat(sprintf("  fold %d: train seasons %s -> validate %s\n", i,
                  paste(range(folds[[i]]$train_seasons), collapse = "-"), folds[[i]]$val_season))
    }
  }

  set.seed(seed)
  candidates <- lapply(seq_len(n_candidates), function(i) {
    lapply(param_grid, function(v) sample(v, 1))
  })

  fixed <- list(objective = "multi:softprob", eval_metric = "mlogloss", num_class = 4)

  results <- lapply(seq_along(candidates), function(i) {
    params <- c(candidates[[i]], fixed)
    fold_scores <- numeric(length(folds))
    fold_rounds <- numeric(length(folds))

    for (f in seq_along(folds)) {
      tr <- train_data[folds[[f]]$train, ]
      va <- train_data[folds[[f]]$validation, ]
      dtr <- xgboost::xgb.DMatrix(as.matrix(tr[, feature_cols]), label = tr[[target_col]])

      if (metric == "rmse_ipf") {
        # No early stopping: the RMSE optimum lies past where log-loss halts.
        m <- xgboost::xgb.train(params = params, data = dtr, nrounds = max_rounds, verbose = 0)
        vax <- as.matrix(va[, feature_cols])
        y_va <- va[[target_col]]
        grid <- sort(unique(pmin(round_grid, max_rounds)))
        s <- vapply(grid, function(k) {
          pm <- as_prob_matrix(stats::predict(m, vax, iterationrange = c(1, k)), nrow(va))
          sqrt(mean((normalize_votes_ipf(pm, va[[match_id_col]]) - y_va)^2))
        }, numeric(1))
        best <- which.min(s)
        fold_scores[f] <- s[best]
        fold_rounds[f] <- grid[best]
      } else {
        dva <- xgboost::xgb.DMatrix(as.matrix(va[, feature_cols]), label = va[[target_col]])
        m <- xgboost::xgb.train(params = params, data = dtr, nrounds = max_rounds,
                                evals = list(validation = dva),
                                early_stopping_rounds = early_stopping_rounds, verbose = 0)
        log <- as.data.frame(attributes(m)$evaluation_log)
        score_col <- grep("^validation", names(log), value = TRUE)[1]
        best <- which.min(log[[score_col]])
        fold_scores[f] <- log[[score_col]][best]
        fold_rounds[f] <- best
      }
    }

    mean_score <- mean(fold_scores)
    if (verbose) cat(sprintf("  candidate %2d/%d: mean %s %.5f\n", i, length(candidates), metric, mean_score))
    tibble::tibble(candidate = i, mean_score = mean_score,
                  median_rounds = stats::median(fold_rounds), params = list(params))
  })
  results <- dplyr::bind_rows(results)

  best_idx <- which.min(results$mean_score)
  if (verbose) {
    cat(sprintf("\nbest: candidate %d, mean %s %.5f\n", best_idx, metric, results$mean_score[best_idx]))
    print(results$params[[best_idx]][names(param_grid)])
  }

  list(
    best_params    = results$params[[best_idx]],
    best_score     = results$mean_score[best_idx],
    best_nrounds   = results$median_rounds[best_idx],
    all_candidates = results
  )
}
