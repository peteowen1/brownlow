# In-season rolling vote features -----------------------------------------------
#
# Pete's idea, 2026-09-02: rather than a hand-tuned linear slope applied via
# probability tilting (tested in analysis/in_count_update_value.R — did not
# help, mean weighted top-20 log-loss -0.0229, t=-1.16), train a real model with
# rolling in-season features as INPUTS, and let it learn how much to trust
# momentum conditional on context (games played, career stage, how far the
# signal has been observed) rather than applying one global number everywhere.
#
# These features use the CURRENT season's own actual votes, which is exactly
# why they cannot go in the pre-season datathon model — votes for rounds not
# yet played do not exist before the season starts. They are legitimate for an
# in-play model: by the time round r+1 is being priced, rounds 1..r's votes
# have been read out and are simply known, the same way front-half actual
# votes are used as ground truth (not predicted) throughout
# analysis/in_count_update_value.R.
#
# Everything here is grouped by (player_name, season) and lagged, so it resets
# at every season boundary — no cross-season leakage, and no re-use of the
# whole-career leak pattern found and fixed in
# calculate_career_performance_summary() (see historical_features.R).

#' Add In-Season Rolling Vote Features
#'
#' @param data Data frame with `player_name`, `season`, `date`, `actual` (votes
#'   received that game) and a walk-forward pre-season model's own prediction
#'   for that row
#' @param pred_col Name of the pre-season model's prediction column
#' @param shrink_k Prior strength, in games, for the shrunk per-game rates.
#'   Defaults to 5.81 — the same constant read off the GAM's
#'   `s(player_name, bs = "re")` smoothing parameter used for `player_bias`
#'   (POSTMORTEM-2025.md §5b) — reused here as a defensible prior rather than
#'   an arbitrary one, though `career_total_games`-style features are also
#'   supplied so the model can learn its own effective shrinkage instead of
#'   trusting a single hand-picked constant.
#'
#' @return `data` with `inseason_*` columns added
#' @export
add_in_season_vote_features <- function(data, pred_col = "pred_votes", shrink_k = 5.81) {
  stopifnot(pred_col %in% names(data), "actual" %in% names(data))

  data %>%
    dplyr::arrange(player_name, season, date) %>%
    dplyr::group_by(player_name, season) %>%
    dplyr::mutate(
      inseason_games_so_far = dplyr::row_number() - 1L,

      inseason_actual_votes_so_far = cumsum(dplyr::lag(actual, default = 0)),
      inseason_pred_votes_so_far   = cumsum(dplyr::lag(.data[[pred_col]], default = 0)),
      inseason_overperformance     = inseason_actual_votes_so_far - inseason_pred_votes_so_far,

      inseason_actual_per_game    = dplyr::if_else(inseason_games_so_far > 0,
                                                    inseason_actual_votes_so_far / inseason_games_so_far, 0),
      inseason_actual_per_game_shrunk = inseason_actual_votes_so_far / (inseason_games_so_far + shrink_k),

      inseason_over_per_game        = dplyr::if_else(inseason_games_so_far > 0,
                                                      inseason_overperformance / inseason_games_so_far, 0),
      inseason_over_per_game_shrunk = inseason_overperformance / (inseason_games_so_far + shrink_k),

      # Short-window recency: last 3 games' actual votes, capturing a hot
      # streak distinctly from a whole-season average.
      inseason_actual_last3 = dplyr::lag(actual, 1, default = 0) +
                              dplyr::lag(actual, 2, default = 0) +
                              dplyr::lag(actual, 3, default = 0)
    ) %>%
    dplyr::ungroup()
}

#' Assert In-Season Features Contain No Current-Game Information
#'
#' On a player's first game of a SEASON there are no prior rounds that season,
#' so every accumulated column must be exactly zero.
#'
#' @param data Output of `add_in_season_vote_features()`
#'
#' @return `data`, invisibly. Errors on any contamination.
#' @export
assert_in_season_clean <- function(data) {
  first_games <- data %>%
    dplyr::group_by(player_name, season) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  cols <- grep("^inseason_", names(data), value = TRUE)
  cols <- setdiff(cols, "inseason_games_so_far")  # checked separately below

  for (col in cols) {
    bad <- sum(!is.na(first_games[[col]]) & first_games[[col]] != 0)
    if (bad > 0) {
      stop(sprintf(paste0("%s is non-zero on %d of %d season-opening games. ",
                          "A player's first game of a season has no prior rounds ",
                          "that season, so this must be exactly zero."),
                   col, bad, nrow(first_games)))
    }
  }
  bad_games <- sum(first_games$inseason_games_so_far != 0)
  if (bad_games > 0) {
    stop(sprintf("inseason_games_so_far is non-zero on %d season-opening games.", bad_games))
  }
  message(sprintf("In-season check passed: %d columns zero on all %d season-opening games",
                  length(cols) + 1, nrow(first_games)))
  invisible(data)
}
