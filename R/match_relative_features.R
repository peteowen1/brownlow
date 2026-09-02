# Match-relative versions of the composites the model leans on -----------------
#
# Votes are allocated WITHIN a match: exactly one 3, one 2 and one 1 among the
# 46 players. So what should matter is how a player compares to the other 45
# that day, not the absolute level of their score. The model was overwhelmingly
# reading absolute levels.
#
# Two reasons this earns its place, only one of which is a measured gain.
#
# 1. SHIFT EXPOSURE. supercoach_score (13.5% of gain) and afl_fantasy_score
#    (5.4%) are absolute scores from FIXED-WEIGHT formulas. AFL Fantasy scores
#    hitouts at +1, so removing the centre bounce in 2026 mechanically lowers
#    ruck scores without any player changing how they play. A model trained on
#    2015-2025 levels reads a 2026 score of 105 as the thing it always was. A
#    rank within the match does not care if the whole league shifts.
#    By contrast coaches_votes (35.9%, the largest feature) is already immune:
#    a fixed 5-4-3-2-1 per-match allocation is rank-like by construction.
#
# 2. MEASURED. 2025 holdout, 5 seeds: 0.35600 against 0.35642, delta -0.00041
#    on a 0.00052 bar — directionally better but NOT proven. What tipped it is
#    that seed variance FELL (sd 0.00024 vs 0.00028): unlike the efficiency
#    features (Sec 5n), this makes the model more stable rather than less.
#
# Adds no information. It re-expresses features the model already leans on in a
# form that survives a distribution shift.

#' Add Match-Relative Versions of Key Composite Features
#'
#' @description
#' For each of `cols`, adds a within-match rank (1/n for the match's best, 1.0
#' for the worst) and a share of the match maximum. Both are invariant to a
#' league-wide rescaling of the underlying statistic, which absolute values are
#' not.
#'
#' @param stats DataFrame with `match_id` and the columns named in `cols`
#' @param cols Character vector of features to express match-relatively
#' @return `stats` with `{col}_rank` and `{col}_vsmax` added per column
#' @export
add_match_relative_features <- function(stats,
                                        cols = c("supercoach_score", "afl_fantasy_score",
                                                 "impact_score", "disposals")) {
  missing <- setdiff(c("match_id", cols), names(stats))
  if (length(missing) > 0) {
    stop(sprintf("add_match_relative_features() needs: %s", paste(missing, collapse = ", ")))
  }
  if (anyNA(stats$match_id)) stop("match ids must never be missing.")

  out <- stats %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      list(
        rank  = ~dplyr::min_rank(-.x) / dplyr::n(),
        # pmax guards a match where the statistic is zero for everyone, which
        # would otherwise divide by zero and produce NaN rather than a
        # defensible "nobody stood out" value of zero.
        vsmax = ~.x / pmax(max(.x, na.rm = TRUE), 1e-9)
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    dplyr::ungroup()

  new_cols <- as.vector(outer(cols, c("rank", "vsmax"), paste, sep = "_"))
  if (any(vapply(out[new_cols], function(x) any(!is.finite(x)), logical(1)))) {
    stop("Match-relative features contain non-finite values.")
  }
  out
}

#' Assert Match-Relative Ranks Are Oriented Correctly
#'
#' @description
#' An inverted rank would silently reverse the feature's meaning while still
#' producing plausible-looking numbers in [0, 1] — the failure mode this
#' codebase keeps meeting. Checks that the match's highest raw value carries
#' the lowest rank.
#'
#' @param stats DataFrame from `add_match_relative_features()`
#' @param col Base column to verify
#' @return `stats`, invisibly, after the assertion passes
#' @export
assert_match_relative_clean <- function(stats, col = "supercoach_score") {
  rank_col <- paste0(col, "_rank")
  if (!rank_col %in% names(stats)) stop(sprintf("%s not found.", rank_col))

  bad <- stats %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(ok = which.max(.data[[col]]) == which.min(.data[[rank_col]]),
                     .groups = "drop") %>%
    dplyr::filter(!ok)

  if (nrow(bad) > 0) {
    stop(sprintf("Rank orientation wrong in %d matches: the top %s does not hold the lowest rank.",
                 nrow(bad), col))
  }
  invisible(stats)
}
