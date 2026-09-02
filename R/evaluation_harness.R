# Evaluation harness -----------------------------------------------------------
#
# Every claim of the form "change X improves RMSE by Y" must come from here.
#
# Why: xgboost is configured with subsample = 0.8 and colsample_bytree = 0.6, so
# the RNG state at training time changes the model. Measured run-to-run sd on the
# 2025 holdout is ~0.0004, i.e. a spread of ~0.001 — while the entire gap between
# 1st and 4th in the 2025 datathon was 0.011. A single-run comparison of two
# configs that differ by less than ~0.001 is a coin flip, not a result.

#' Coerce an xgboost Multi-Class Prediction to an n x 4 Probability Matrix
#'
#' From xgboost 2.1 `predict()` on a `multi:softprob` model already returns an
#' n x 4 matrix. Reshaping that with `matrix(pred, nrow = n, byrow = TRUE)` — as
#' the codebase did in three places — flattens it column-major and refills it
#' row-wise, which scrambles every row while leaving the column means intact.
#' Normalisation afterwards then restores healthy-looking column sums, so the
#' damage is invisible: measured on the 2023-24 test set, `cor(P(3), actual)`
#' fell from +0.7177 to +0.0001 while `colMeans` still read 0.9348 / 0.0217 /
#' 0.0217 / 0.0217. That corrupted evaluation is what drove the ensemble
#' optimiser to give XGBoost the minimum allowed weight.
#'
#' @param pred Whatever `predict()` returned
#' @param n_obs Expected number of rows
#'
#' @return An `n_obs` x 4 matrix
#' @export
as_prob_matrix <- function(pred, n_obs) {
  if (is.null(dim(pred))) {
    # Genuinely flat: xgboost returns class-major order, so byrow = TRUE is right.
    pred <- matrix(pred, nrow = n_obs, ncol = 4, byrow = TRUE)
  }
  if (nrow(pred) != n_obs || ncol(pred) != 4) {
    stop(sprintf("Expected a %d x 4 probability matrix, got %d x %d.",
                 n_obs, nrow(pred), ncol(pred)))
  }
  pred
}

#' Attach Known Actual Votes to a Holdout Season
#'
#' The datathon dataset ships the prediction season with all votes set to zero.
#' This joins the published results back on, keyed on date rather than round.
#'
#' Do not join on round: the 2025 Gold Coast v Essendon fixture was postponed by
#' Cyclone Alfred and replayed, and the two sources label it differently (Round 0
#' vs Round 24), which produces 41 duplicate keys.
#'
#' @param holdout Holdout data frame with `date`, `team`, `player_name`
#' @param results_file CSV of predictions vs actuals containing `match_date`,
#'   `player_team_std`, `full_player_name`, `Actual`
#' @param expected_votes Total votes expected after the join, for the guard
#'
#' @return `holdout` with an `Actual` column
#' @export
attach_holdout_actuals <- function(holdout,
                                   results_file = "analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                                   expected_votes = 1242) {
  actuals <- readr::read_csv(results_file, show_col_types = FALSE) %>%
    dplyr::transmute(
      date_j   = as.Date(match_date, format = "%d/%m/%Y"),
      team_j   = player_team_std,
      player_j = full_player_name,
      Actual
    )

  n_before <- nrow(holdout)
  out <- holdout %>%
    dplyr::mutate(
      date_j   = as.Date(date),
      team_j   = standardize_team_name(team),
      player_j = player_name
    ) %>%
    dplyr::left_join(actuals, by = c("date_j", "team_j", "player_j"))

  if (nrow(out) != n_before) {
    stop(sprintf("Join changed row count: %d -> %d. Duplicate keys.", n_before, nrow(out)))
  }
  if (sum(out$Actual, na.rm = TRUE) != expected_votes) {
    stop(sprintf("Joined %d votes, expected %d. %d rows failed to match.",
                 sum(out$Actual, na.rm = TRUE), expected_votes, sum(is.na(out$Actual))))
  }
  out
}

#' Convert Vote Probabilities to a Submission, Scaling Totals to 6 per Match
#'
#' The legacy method: take expected votes, clamp to 0-3, rescale so each match
#' totals 6. Enforces one constraint of the four that actually hold.
#'
#' @param prob_matrix n x 4 matrix of P(0), P(1), P(2), P(3)
#' @param match_ids Match identifier per row
#'
#' @return Numeric vector of predicted votes
#' @export
normalize_votes_scale <- function(prob_matrix, match_ids) {
  ev <- as.numeric(prob_matrix %*% c(0, 1, 2, 3))
  scaled <- lapply(split(ev, match_ids), function(v) {
    v <- pmax(0, pmin(3, v))
    if (sum(v) > 0) v <- v * 6 / sum(v)
    pmax(0, pmin(3, v))
  })
  unlist(scaled)[order(order(match_ids))]
}

#' Convert Vote Probabilities to a Submission by Iterative Proportional Fitting
#'
#' Enforces all four constraints that hold in every match, rather than only the
#' vote total. Across the n players in a match exactly one player takes 3 votes,
#' one takes 2 and one takes 1, so the column sums are (n - 3, 1, 1, 1); and each
#' player's four probabilities sum to 1. Satisfying those makes the 6-vote total
#' true automatically: 0*(n-3) + 1 + 2 + 3 = 6.
#'
#' @param prob_matrix n x 4 matrix of P(0), P(1), P(2), P(3)
#' @param match_ids Match identifier per row
#' @param iters Sinkhorn iterations per match
#'
#' @return Numeric vector of predicted votes
#' @export
normalize_votes_ipf <- function(prob_matrix, match_ids, iters = 200) {
  # split() silently discards NA keys, which would leave those rows at their
  # initialised zero and read as a confident "no votes" rather than as skipped.
  if (anyNA(match_ids)) {
    stop(sprintf("%d rows have a missing match_id; every row must belong to a match.",
                 sum(is.na(match_ids))))
  }
  if (any(prob_matrix < 0, na.rm = TRUE) || anyNA(prob_matrix)) {
    stop("prob_matrix must be non-negative and free of NA before normalisation.")
  }

  out <- numeric(nrow(prob_matrix))
  for (idx in split(seq_len(nrow(prob_matrix)), match_ids)) {
    M <- pmax(prob_matrix[idx, , drop = FALSE], 1e-12)
    n <- nrow(M)

    # Three of the n players take votes, so the column targets are (n-3,1,1,1).
    # At n = 3 the first target is 0, which drives column 1 to zero and makes
    # the next target/colSums division 0/0 — NaN then propagates through every
    # remaining iteration. At n = 2 the target goes negative and flips the sign
    # of the update, producing values outside [0,3]. Neither is recoverable, so
    # fail loudly rather than return plausible nonsense.
    if (n < 4) {
      stop(sprintf("Match has %d players; at least 4 are needed to allocate 3-2-1 votes.", n))
    }

    target <- c(n - 3, 1, 1, 1)
    for (k in seq_len(iters)) {
      M <- M / rowSums(M)
      M <- sweep(M, 2, target / colSums(M), "*")
    }
    M <- M / rowSums(M)
    out[idx] <- as.numeric(M %*% c(0, 1, 2, 3))
  }
  out
}

#' Evaluate a Model Configuration Across Seeds
#'
#' @param fit_predict Function `(train, newdata, seed)` returning an n x 4
#'   probability matrix for `newdata`
#' @param train Training data
#' @param holdout Holdout data carrying an `Actual` column and `match_id`
#' @param label Human-readable name for the configuration
#' @param seeds Integer vector of seeds
#' @param normalizer "scale" or "ipf"
#'
#' @return One-row tibble: label, normalizer, mean RMSE, sd, min, max, n seeds
#' @export
evaluate_seeds <- function(fit_predict, train, holdout, label,
                           seeds = 1:5, normalizer = c("scale", "ipf")) {
  normalizer <- match.arg(normalizer)
  stopifnot("Actual" %in% names(holdout), !anyNA(holdout$Actual))
  norm_fn <- switch(normalizer, scale = normalize_votes_scale, ipf = normalize_votes_ipf)

  scores <- vapply(seeds, function(s) {
    pm <- fit_predict(train, holdout, s)
    stopifnot(nrow(pm) == nrow(holdout), ncol(pm) == 4)
    votes <- norm_fn(pm, holdout$match_id)
    sqrt(mean((votes - holdout$Actual)^2))
  }, numeric(1))

  tibble::tibble(
    label = label, normalizer = normalizer,
    rmse = mean(scores), sd = stats::sd(scores),
    min = min(scores), max = max(scores), n_seeds = length(seeds)
  )
}

#' Build an XGBoost fit-predict Closure for the Harness
#'
#' @param feature_cols Features to train on
#' @param target_col Target column
#' @param params xgboost parameter list
#' @param nrounds Boosting rounds
#'
#' @return Function suitable for `evaluate_seeds()`
#' @export
xgb_fit_predict <- function(feature_cols, target_col, params, nrounds) {
  # The training DMatrix and the prediction matrix are identical across seeds —
  # only the RNG differs — so build each once and reuse. Converting an 87,288 x
  # 260 frame is a 181 MB allocation, and doing it per seed was repeating the
  # single most expensive non-fitting step in every harness run.
  cache <- new.env(parent = emptyenv())

  function(train, newdata, seed) {
    if (is.null(cache$dtrain) || !identical(cache$train_id, dim(train))) {
      cache$dtrain   <- xgboost::xgb.DMatrix(as.matrix(train[, feature_cols]),
                                             label = train[[target_col]])
      cache$train_id <- dim(train)
    }
    if (is.null(cache$newx) || !identical(cache$new_id, dim(newdata))) {
      cache$newx   <- as.matrix(newdata[, feature_cols])
      cache$new_id <- dim(newdata)
    }

    set.seed(seed)
    model <- xgboost::xgb.train(params = params, data = cache$dtrain,
                                nrounds = nrounds, verbose = 0)
    as_prob_matrix(stats::predict(model, cache$newx), nrow(newdata))
  }
}

#' Print a Comparison Table, Flagging Differences Inside the Noise Floor
#'
#' @param results Data frame of `evaluate_seeds()` rows
#' @param baseline_label Label to measure deltas against; defaults to the first row
#'
#' @return `results` with delta and significance columns, invisibly
#' @export
compare_configs <- function(results, baseline_label = NULL) {
  base_row <- if (is.null(baseline_label)) results[1, ] else
    results[results$label == baseline_label, ]
  pooled_sd <- sqrt(mean(results$sd^2))

  out <- results %>%
    dplyr::mutate(
      delta = rmse - base_row$rmse,
      # a delta smaller than 2 pooled sd is not distinguishable from seed noise
      verdict = dplyr::case_when(
        abs(delta) < 1e-12          ~ "baseline",
        abs(delta) < 2 * pooled_sd  ~ "within noise",
        delta < 0                   ~ "BETTER",
        TRUE                        ~ "worse"
      )
    ) %>%
    dplyr::arrange(rmse)

  cat(sprintf("\npooled seed sd = %.5f  -> differences under %.5f are not decidable\n\n",
              pooled_sd, 2 * pooled_sd))
  # any_of, not a bare select: a caller that assembled `results` without the
  # optional `normalizer` column should still get its table printed rather than
  # lose an expensive run to a tidyselect error at the very last step.
  print(as.data.frame(out %>% dplyr::select(dplyr::any_of(
          c("label", "normalizer", "rmse", "sd", "delta", "verdict")))),
        digits = 5, row.names = FALSE)
  invisible(out)
}
