# Team form vs pre-season expectation ------------------------------------------
#
# Targets the Gold Coast pattern from POSTMORTEM-2025.md Sec 3: 6 of the 16
# worst 2025 misses were Gold Coast players, driven by a club-level step change
# (finals contention after years near the bottom) that the existing career/
# prior-season-average PLAYER features can't see, because they describe the
# player, not the team's current trajectory. Needs no external data — team_score
# and opposition_score are already in every stats row.
#
# Two numbers per team-game, both leak-free:
#   team_prior_season_win_rate       full PRIOR season's win rate — entirely
#                                     known before this season's first bounce,
#                                     so safe for every round of the season, not
#                                     just an in-season-updating one.
#   team_current_season_win_rate_so_far   this season's win rate through the
#                                     PRIOR round only (lag(default 0) before
#                                     cumsum, same pattern as every other
#                                     in-season feature this codebase uses).
# team_form_vs_expectation is their gap — the "surprise" signal.
#
# A team with no prior season (an expansion club's debut year) gets 0.5 (league
# average), not 0 or NA, so the model reads "no information" rather than
# "certain to lose every game" for that team's rows.

#' Add Team Form vs Pre-Season Expectation Features
#'
#' @param stats DataFrame with one row per player-game, containing `season`,
#'   `date`, `team`, `team_score`, `opposition_score`
#' @return `stats` with `team_prior_season_win_rate`,
#'   `team_current_season_win_rate_so_far`, `team_form_vs_expectation`,
#'   `team_no_prior_season` added
#' @export
add_team_form_vs_expectation <- function(stats) {

  stopifnot(all(c("season", "date", "team", "team_score", "opposition_score") %in% names(stats)))

  team_games <- stats %>%
    dplyr::distinct(season, date, team, team_score, opposition_score) %>%
    dplyr::mutate(team_won = as.numeric(dplyr::coalesce(team_score, 0) > dplyr::coalesce(opposition_score, 0))) %>%
    dplyr::arrange(team, season, date)

  # season_final_win_rate: full-season rate, one row per team-season, used only
  # as NEXT season's prior — computing it does not leak into games playing it.
  season_summary <- team_games %>%
    dplyr::group_by(team, season) %>%
    dplyr::summarise(season_final_win_rate = mean(team_won), .groups = "drop") %>%
    dplyr::arrange(team, season) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(team_prior_season_win_rate = dplyr::lag(season_final_win_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::select(team, season, team_prior_season_win_rate)

  team_games <- team_games %>%
    dplyr::group_by(team, season) %>%
    dplyr::mutate(
      games_so_far = dplyr::row_number() - 1,
      wins_so_far  = cumsum(dplyr::lag(team_won, default = 0)),
      current_season_rate_so_far = ifelse(games_so_far > 0, wins_so_far / games_so_far, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(season_summary, by = c("team", "season")) %>%
    dplyr::mutate(
      team_no_prior_season = as.numeric(is.na(team_prior_season_win_rate)),
      team_prior_season_win_rate = dplyr::coalesce(team_prior_season_win_rate, 0.5),
      # First game of a season: no current-season games played yet, so fall
      # back to the prior-season rate rather than an uninformative 0 or NA.
      team_current_season_win_rate_so_far = dplyr::coalesce(current_season_rate_so_far, team_prior_season_win_rate),
      team_form_vs_expectation = team_current_season_win_rate_so_far - team_prior_season_win_rate
    ) %>%
    dplyr::select(season, date, team, team_prior_season_win_rate,
                  team_current_season_win_rate_so_far, team_form_vs_expectation,
                  team_no_prior_season)

  stats %>% dplyr::left_join(team_games, by = c("season", "date", "team"))
}

#' Assert Team Form Features Are Leak-Free
#'
#' A team's first-ever game in the data must show the neutral 0.5 prior and a
#' zero gap (current-season-so-far falls back to the same 0.5), and games_so_far
#' bookkeeping (checked indirectly via win-rate range) must stay within [0, 1].
#'
#' @param stats DataFrame with team form columns from `add_team_form_vs_expectation()`
#' @return `stats`, invisibly, after assertions pass
#' @export
assert_team_form_clean <- function(stats) {
  rng <- range(c(stats$team_prior_season_win_rate, stats$team_current_season_win_rate_so_far), na.rm = TRUE)
  if (rng[1] < 0 || rng[2] > 1) {
    stop(sprintf("team win-rate features out of [0,1]: range %.3f-%.3f", rng[1], rng[2]))
  }

  debut <- stats %>%
    dplyr::filter(team_no_prior_season == 1) %>%
    dplyr::group_by(team) %>%
    dplyr::filter(date == min(date)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(team, date, team_prior_season_win_rate, team_form_vs_expectation)

  bad <- debut %>% dplyr::filter(abs(team_prior_season_win_rate - 0.5) > 1e-9 | abs(team_form_vs_expectation) > 1e-9)
  if (nrow(bad) > 0) {
    stop(sprintf("%d team debut-season rows don't show the neutral 0.5 prior / zero gap.", nrow(bad)))
  }

  invisible(stats)
}
