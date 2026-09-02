# Season-to-date form and reputation features ---------------------------------
#
# Hypothesis: umpires do not arrive at a match blank. A player who has been
# polling well and being talked about all season carries that into the votes.
# Matt Rowell, round 7 2025, is the case that prompted this - he polled 3 votes
# on 16 disposals and ZERO coaches votes, ranked 20th in the match for disposals.
# Nothing in his box score that day explains it; his season did.
#
# Everything here is strictly prior rounds: a round 7 game sees rounds 1-6 only.
#
# Availability note. This is not live in-season forecasting - the submission is
# made once, before the count, for the whole season. Every box score is already
# known at submission time; only the votes are secret. So cumulative coaches
# votes, SuperCoach, disposals and so on are all legitimately available. Actual
# season-to-date VOTES are not, and must never be used.

#' Add Season-to-Date Rolling Features
#'
#' For each player-season, accumulates the named metrics over strictly prior
#' rounds and adds competition-wide ranks at that point in the season.
#'
#' Uses `cumsum(lag(x)) / prior_games`, never `cummean(lag(x))` — the latter
#' divides by n rather than n-1 and folds in the injected leading zero, which is
#' the bug that made every `career_avg_*` feature 50% low at a player's second
#' game.
#'
#' @param data Data frame with `player_name`, `season`, `date`
#' @param metrics Character vector of columns to accumulate
#' @param prefix Prefix for the created columns
#' @param rank_metrics Subset of `metrics` to also rank across the competition
#'
#' @return `data` with added `<prefix>_<metric>_sum`, `_avg` and `_rank` columns
#' @export
add_season_to_date_features <- function(data,
                                        metrics,
                                        prefix = "szn",
                                        rank_metrics = metrics) {
  metrics <- intersect(metrics, names(data))
  if (length(metrics) == 0) return(data)

  out <- data %>%
    dplyr::arrange(player_name, season, date) %>%
    dplyr::group_by(player_name, season)

  out <- out %>%
    dplyr::mutate(!!paste0(prefix, "_games_prior") := dplyr::row_number() - 1L)

  for (m in metrics) {
    sum_col <- paste0(prefix, "_", m, "_sum")
    avg_col <- paste0(prefix, "_", m, "_avg")
    out <- out %>%
      dplyr::mutate(
        !!sum_col := cumsum(dplyr::lag(.data[[m]], default = 0)),
        !!avg_col := dplyr::if_else(.data[[paste0(prefix, "_games_prior")]] > 0,
                                    .data[[sum_col]] / .data[[paste0(prefix, "_games_prior")]],
                                    0)
      )
  }
  out <- dplyr::ungroup(out)

  # Competition-wide standing at this point in the season. This is the "is he in
  # the conversation" signal: being third in the coaches' award in round 7 is a
  # different thing from having the same raw total in obscurity.
  for (m in intersect(rank_metrics, metrics)) {
    sum_col  <- paste0(prefix, "_", m, "_sum")
    rank_col <- paste0(prefix, "_", m, "_rank")
    out <- out %>%
      dplyr::group_by(season, .data[[paste0(prefix, "_games_prior")]]) %>%
      dplyr::mutate(!!rank_col := dplyr::min_rank(-.data[[sum_col]])) %>%
      dplyr::ungroup()
  }

  out
}

#' Assert Season-to-Date Features Contain No Current-Game Information
#'
#' On a player's first game of a season there are no prior rounds, so every
#' accumulated column must be exactly zero. If the current game leaks in, that
#' is where it shows up unambiguously.
#'
#' @param data Output of `add_season_to_date_features()`
#' @param prefix The prefix used
#'
#' @return `data`, invisibly. Errors on any contamination.
#' @export
assert_season_to_date_clean <- function(data, prefix = "szn") {
  first <- data[data[[paste0(prefix, "_games_prior")]] == 0, , drop = FALSE]
  if (nrow(first) == 0) {
    stop("No first-games-of-season found; cannot verify the features are lagged.")
  }
  cols <- grep(paste0("^", prefix, "_.*_(sum|avg)$"), names(data), value = TRUE)
  for (col in cols) {
    bad <- sum(!is.na(first[[col]]) & first[[col]] != 0)
    if (bad > 0) {
      stop(sprintf(paste0("%s is non-zero on %d of %d season-opening games. ",
                          "A player with no prior rounds must have zero accumulated."),
                   col, bad, nrow(first)))
    }
  }
  message(sprintf("Season-to-date check passed: %d columns zero on all %d season openers",
                  length(cols), nrow(first)))
  invisible(data)
}
