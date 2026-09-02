# Brownlow eligibility ---------------------------------------------------------
#
# The Brownlow goes to the "fairest and best". A player suspended by the tribunal
# during the season is INELIGIBLE to win it regardless of how many votes he
# polls. Dangerfield polled 33 in 2017 and did not win; Martin did, on 36.
#
# The simulation currently assigns win probability to players who cannot win.
# For a leaderboard metric that is harmless — eligibility does not affect the
# per-match vote predictions the datathon scores. For betting it is not: back an
# ineligible player and the bet loses whatever he does.
#
# It cuts the other way too, and that is where the value is. When a contender is
# ineligible, every other player's true win probability rises, and markets can be
# slow to reprice it.

#' Load the Ineligible-Player Register
#'
#' Reads the hand-maintained register of players suspended during a season and
#' therefore ineligible for the Brownlow.
#'
#' @param file Path to the register
#'
#' @return Data frame with `season`, `player_name`, `team`, `reason`, `source`
#' @export
load_ineligible_players <- function(file = "data/brownlow_ineligible.csv") {
  if (!file.exists(file)) {
    stop(sprintf(paste0("Eligibility register not found at %s.\n",
                        "Create it before simulating: a missing file and a season ",
                        "with no suspensions look identical, and guessing costs money."),
                 file))
  }
  reg <- utils::read.csv(file, stringsAsFactors = FALSE)
  required <- c("season", "player_name", "team", "reason", "source")
  missing <- setdiff(required, names(reg))
  if (length(missing) > 0) {
    stop(sprintf("Eligibility register is missing columns: %s",
                 paste(missing, collapse = ", ")))
  }
  reg$player_name <- trimws(reg$player_name)
  reg
}

#' Assert the Register Has Been Maintained for a Season
#'
#' An empty register and an unmaintained register look the same. Across a full
#' AFL season some player of note is always suspended, so silence is far more
#' likely to mean "nobody updated the file" than "nobody was suspended". This
#' forces the distinction to be explicit: record a row with
#' `reason = "none-checked"` to assert the season really was clean.
#'
#' @param season Season being simulated
#' @param register Output of `load_ineligible_players()`
#'
#' @return `register`, invisibly. Errors if the season has no entries at all.
#' @export
assert_eligibility_checked <- function(season, register) {
  rows <- register[register$season == season, , drop = FALSE]
  if (nrow(rows) == 0) {
    stop(sprintf(paste0("No eligibility entries for %s.\n",
                        "Either add the suspended players, or add one row with ",
                        "reason = \"none-checked\" to state that the season was ",
                        "reviewed and nobody was suspended."), season))
  }
  real <- rows[rows$reason != "none-checked", , drop = FALSE]
  message(sprintf("Eligibility: %d ineligible player(s) recorded for %s%s",
                  nrow(real), season,
                  if (nrow(real) == 0) " (season reviewed, none found)" else ""))
  invisible(register)
}

#' Zero Out Ineligible Players and Renormalise Win Probabilities
#'
#' Applied AFTER simulation, to the win probabilities rather than to the votes:
#' an ineligible player still polls, he just cannot win. Removing him therefore
#' redistributes win probability to everyone else rather than changing anyone's
#' vote total.
#'
#' @param win_probs Data frame with `player_name` and a probability column
#' @param season Season being simulated
#' @param register Output of `load_ineligible_players()`
#' @param prob_col Name of the probability column
#'
#' @return `win_probs` with ineligible players zeroed and the rest renormalised,
#'   plus an `eligible` flag column
#' @export
apply_eligibility <- function(win_probs, season, register, prob_col = "win_probability") {
  assert_eligibility_checked(season, register)

  banned <- register$player_name[register$season == season &
                                   register$reason != "none-checked"]

  unmatched <- setdiff(banned, win_probs$player_name)
  if (length(unmatched) > 0) {
    stop(sprintf(paste0("These ineligible players are not in the simulation output: %s\n",
                        "A name that does not match is silently no restriction at all."),
                 paste(unmatched, collapse = ", ")))
  }

  win_probs$eligible <- !(win_probs$player_name %in% banned)
  removed <- sum(win_probs[[prob_col]][!win_probs$eligible])

  win_probs[[prob_col]][!win_probs$eligible] <- 0
  total <- sum(win_probs[[prob_col]])
  if (total > 0) {
    win_probs[[prob_col]] <- win_probs[[prob_col]] * (100 / total)
  }

  message(sprintf("Eligibility: removed %.2f%% of win probability across %d player(s), redistributed",
                  removed, sum(!win_probs$eligible)))
  win_probs
}
