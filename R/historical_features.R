# Performance-Based Historical Feature Utilities
# Functions for creating features based on historical performance patterns
# NO brownlow_votes dependencies - prevents data leakage

#' Calculate Player Career Performance Summary
#'
#' Creates summary statistics of each player's career performance patterns
#'
#' @param stats DataFrame with player statistics (no votes)
#' @return DataFrame with player career performance features
#' @export
calculate_career_performance_summary <- function(stats) {
  stats %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      career_total_games = dplyr::n(),
      career_avg_disposals = mean(kicks + handballs, na.rm = TRUE),
      career_avg_goals = mean(goals, na.rm = TRUE),
      career_avg_tackles = mean(tackles, na.rm = TRUE),
      career_avg_marks = mean(marks, na.rm = TRUE),
      career_avg_fantasy = mean(fantasy_score, na.rm = TRUE),
      career_avg_impact = mean(impact_score, na.rm = TRUE),
      career_games_high_disposals = sum((kicks + handballs) >= 25, na.rm = TRUE),
      career_games_with_goals = sum(goals > 0, na.rm = TRUE),
      career_consistency_disposals = 1 / (1 + stats::sd(kicks + handballs, na.rm = TRUE)),
      career_max_disposals = max(kicks + handballs, na.rm = TRUE),
      career_max_goals = max(goals, na.rm = TRUE),
      first_season = min(season, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate Recent Performance Trends
#'
#' Creates features based on recent performance trends (no votes)
#'
#' @param stats DataFrame with player statistics, ordered by date
#' @param recent_games Integer. Number of recent games to consider (default: 10)
#' @return DataFrame with recent performance trend features
#' @export
calculate_recent_performance_trends <- function(stats, recent_games = 10) {

  stats %>%
    dplyr::arrange(player_name, date) %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Recent performance averages (exclude current game)
      recent_avg_disposals = zoo::rollmean(dplyr::lag(kicks + handballs, default = 0),
                                           k = recent_games, fill = 0, align = "right"),
      recent_avg_goals = zoo::rollmean(dplyr::lag(goals, default = 0),
                                      k = recent_games, fill = 0, align = "right"),
      recent_avg_tackles = zoo::rollmean(dplyr::lag(tackles, default = 0),
                                        k = recent_games, fill = 0, align = "right"),
      recent_avg_marks = zoo::rollmean(dplyr::lag(marks, default = 0),
                                      k = recent_games, fill = 0, align = "right"),
      recent_avg_fantasy = zoo::rollmean(dplyr::lag(fantasy_score, default = 0),
                                        k = recent_games, fill = 0, align = "right"),
      recent_avg_impact = zoo::rollmean(dplyr::lag(impact_score, default = 0),
                                       k = recent_games, fill = 0, align = "right"),

      # Performance momentum (recent vs career average)
      career_avg_disposals = dplyr::cummean(dplyr::lag(kicks + handballs, default = 0)),
      career_avg_fantasy = dplyr::cummean(dplyr::lag(fantasy_score, default = 0)),
      career_avg_impact = dplyr::cummean(dplyr::lag(impact_score, default = 0)),

      recent_vs_career_disposals = recent_avg_disposals - dplyr::lag(career_avg_disposals, 1),
      recent_vs_career_fantasy = recent_avg_fantasy - dplyr::lag(career_avg_fantasy, 1),
      recent_vs_career_impact = recent_avg_impact - dplyr::lag(career_avg_impact, 1),

      # Performance consistency in recent games
      recent_consistency_disposals = {
        # Calculate rolling standard deviation excluding current game
        rolling_sd <- zoo::rollapply(dplyr::lag(kicks + handballs, default = 0),
                                    width = recent_games, FUN = stats::sd,
                                    fill = 0, align = "right")
        1 / (1 + rolling_sd)
      },

      # Hot streak indicators (high performance games)
      high_disposal_games_last_3 = zoo::rollsum(as.numeric(dplyr::lag(kicks + handballs, default = 0) >= 25),
                                                k = 3, fill = 0, align = "right"),
      high_disposal_games_last_5 = zoo::rollsum(as.numeric(dplyr::lag(kicks + handballs, default = 0) >= 25),
                                                k = 5, fill = 0, align = "right"),
      goal_games_last_3 = zoo::rollsum(as.numeric(dplyr::lag(goals, default = 0) > 0),
                                      k = 3, fill = 0, align = "right"),
      goal_games_last_5 = zoo::rollsum(as.numeric(dplyr::lag(goals, default = 0) > 0),
                                      k = 5, fill = 0, align = "right"),

      # Current season performance (exclude current game)
      season_games_so_far = pmax(0, dplyr::row_number() - 1),
      season_disposals_so_far = cumsum(dplyr::lag(kicks + handballs, default = 0)),
      season_goals_so_far = cumsum(dplyr::lag(goals, default = 0)),
      season_avg_disposals = ifelse(season_games_so_far > 0,
                                   season_disposals_so_far / season_games_so_far, 0),
      season_avg_goals = ifelse(season_games_so_far > 0,
                               season_goals_so_far / season_games_so_far, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Form indicators removed - these features showed very low importance in XGBoost
      # on_high_disposal_streak, extended_high_disposal_form, on_goal_scoring_streak, extended_goal_scoring_form
      # Keeping the underlying rolling counts as they may still be useful for other features
    )
}

#' Calculate Position-Specific Performance Patterns
#'
#' Creates performance pattern features specific to player positions
#'
#' @param stats DataFrame with player statistics and position information
#' @return DataFrame with position-specific performance features
#' @export
calculate_position_performance_patterns <- function(stats) {

  # Determine player positions if not already available
  if (!all(c("is_midfielder", "is_forward", "is_back") %in% names(stats))) {
    stats <- stats %>%
      dplyr::mutate(
        is_midfielder = as.numeric(stringr::str_detect(tolower(player_position), "mid")),
        is_forward = as.numeric(stringr::str_detect(tolower(player_position), "for")),
        is_back = as.numeric(stringr::str_detect(tolower(player_position), "back|def"))
      )
  }

  stats %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Position-specific performance averages (exclude current game)
      disposals_as_mid = ifelse(dplyr::lag(is_midfielder, default = 0) == 1,
                               dplyr::lag(kicks + handballs, default = 0), 0),
      disposals_as_forward = ifelse(dplyr::lag(is_forward, default = 0) == 1,
                                   dplyr::lag(kicks + handballs, default = 0), 0),
      disposals_as_back = ifelse(dplyr::lag(is_back, default = 0) == 1,
                                dplyr::lag(kicks + handballs, default = 0), 0),

      # Games played in each position (exclude current game)
      games_as_mid = cumsum(dplyr::lag(is_midfielder, default = 0)),
      games_as_forward = cumsum(dplyr::lag(is_forward, default = 0)),
      games_as_back = cumsum(dplyr::lag(is_back, default = 0)),

      # Average performance by position (based on historical data only)
      avg_disposals_as_mid = ifelse(games_as_mid > 0, cumsum(disposals_as_mid) / games_as_mid, 0),
      avg_disposals_as_forward = ifelse(games_as_forward > 0, cumsum(disposals_as_forward) / games_as_forward, 0),
      avg_disposals_as_back = ifelse(games_as_back > 0, cumsum(disposals_as_back) / games_as_back, 0),

      # Best performing position (position with highest average disposals)
      best_performance_position = dplyr::case_when(
        avg_disposals_as_mid >= avg_disposals_as_forward & avg_disposals_as_mid >= avg_disposals_as_back ~ "midfielder",
        avg_disposals_as_forward >= avg_disposals_as_back ~ "forward",
        TRUE ~ "back"
      ),

      # Position versatility (how many positions played significantly)
      position_versatility = as.numeric(games_as_mid >= 5) +
                            as.numeric(games_as_forward >= 5) +
                            as.numeric(games_as_back >= 5)
    ) %>%
    dplyr::ungroup()
}

#' Calculate Prior Season Performance Averages
#'
#' Creates features based on player performance in completed seasons prior to current season
#'
#' @param stats DataFrame with player statistics and season information
#' @return DataFrame with prior season performance features
#' @export
calculate_prior_season_performance_averages <- function(stats) {

  # First, create season-level summaries for each player
  season_summaries <- stats %>%
    dplyr::group_by(player_name, season) %>%
    dplyr::summarise(
      season_games = dplyr::n(),
      season_total_disposals = sum(kicks + handballs, na.rm = TRUE),
      season_total_goals = sum(goals, na.rm = TRUE),
      season_total_tackles = sum(tackles, na.rm = TRUE),
      season_total_fantasy = sum(fantasy_score, na.rm = TRUE),
      season_total_impact = sum(impact_score, na.rm = TRUE),
      season_games_high_disposals = sum((kicks + handballs) >= 25, na.rm = TRUE),
      season_games_with_goals = sum(goals > 0, na.rm = TRUE),
      .groups = "drop"
    )

  # For each player-season combination, calculate stats from prior seasons only
  prior_season_features <- season_summaries %>%
    dplyr::arrange(player_name, season) %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Count of prior completed seasons
      prior_seasons_count = pmax(0, dplyr::row_number() - 1),

      # Calculate prior season totals (exclude current season)
      prior_seasons_total_games = pmax(0, cumsum(dplyr::lag(season_games, default = 0))),
      prior_seasons_total_disposals = pmax(0, cumsum(dplyr::lag(season_total_disposals, default = 0))),
      prior_seasons_total_goals = pmax(0, cumsum(dplyr::lag(season_total_goals, default = 0))),
      prior_seasons_total_tackles = pmax(0, cumsum(dplyr::lag(season_total_tackles, default = 0))),
      prior_seasons_total_fantasy = pmax(0, cumsum(dplyr::lag(season_total_fantasy, default = 0))),
      prior_seasons_total_impact = pmax(0, cumsum(dplyr::lag(season_total_impact, default = 0))),
      prior_seasons_high_disposal_games = pmax(0, cumsum(dplyr::lag(season_games_high_disposals, default = 0))),
      prior_seasons_goal_games = pmax(0, cumsum(dplyr::lag(season_games_with_goals, default = 0))),

      # Calculate averages from prior seasons only
      avg_disposals_per_game_prior = ifelse(prior_seasons_total_games > 0,
                                           prior_seasons_total_disposals / prior_seasons_total_games, 0),
      avg_goals_per_game_prior = ifelse(prior_seasons_total_games > 0,
                                       prior_seasons_total_goals / prior_seasons_total_games, 0),
      avg_tackles_per_game_prior = ifelse(prior_seasons_total_games > 0,
                                         prior_seasons_total_tackles / prior_seasons_total_games, 0),
      avg_fantasy_per_game_prior = ifelse(prior_seasons_total_games > 0,
                                         prior_seasons_total_fantasy / prior_seasons_total_games, 0),
      avg_impact_per_game_prior = ifelse(prior_seasons_total_games > 0,
                                        prior_seasons_total_impact / prior_seasons_total_games, 0),

      high_disposal_rate_prior = ifelse(prior_seasons_total_games > 0,
                                       prior_seasons_high_disposal_games / prior_seasons_total_games, 0),
      goal_rate_prior = ifelse(prior_seasons_total_games > 0,
                              prior_seasons_goal_games / prior_seasons_total_games, 0),

      # Prior season performance indicators
      avg_games_per_season_prior = ifelse(prior_seasons_count > 0,
                                         prior_seasons_total_games / prior_seasons_count, 0),
      avg_disposals_per_season_prior = ifelse(prior_seasons_count > 0,
                                             prior_seasons_total_disposals / prior_seasons_count, 0)
    ) %>%
    dplyr::ungroup()

  # Join back to main stats data
  stats %>%
    dplyr::left_join(prior_season_features %>%
                     dplyr::select(player_name, season, prior_seasons_count, prior_seasons_total_games,
                                   avg_disposals_per_game_prior, avg_goals_per_game_prior, avg_tackles_per_game_prior,
                                   avg_fantasy_per_game_prior, avg_impact_per_game_prior,
                                   high_disposal_rate_prior, goal_rate_prior,
                                   avg_games_per_season_prior, avg_disposals_per_season_prior),
                     by = c("player_name", "season")) %>%
    dplyr::mutate(
      # Fill NAs for players with no prior seasons
      dplyr::across(c(prior_seasons_count, prior_seasons_total_games, avg_disposals_per_game_prior,
                      avg_goals_per_game_prior, avg_tackles_per_game_prior, avg_fantasy_per_game_prior,
                      avg_impact_per_game_prior, high_disposal_rate_prior, goal_rate_prior,
                      avg_games_per_season_prior, avg_disposals_per_season_prior),
                    ~tidyr::replace_na(.x, 0)),

      # Experience-based features
      experienced_player = as.numeric(prior_seasons_count >= 3),
      rookie_player = as.numeric(prior_seasons_count == 0),
      second_year_player = as.numeric(prior_seasons_count == 1),

      # Performance classification based on prior seasons
      performance_class_prior = dplyr::case_when(
        prior_seasons_total_games < 10 ~ "insufficient_data",
        avg_disposals_per_game_prior > 25 ~ "elite_ball_winner",
        avg_disposals_per_game_prior > 20 ~ "high_ball_winner",
        avg_disposals_per_game_prior > 15 ~ "moderate_ball_winner",
        TRUE ~ "low_ball_winner"
      ),

      # Goal scoring classification
      goal_scoring_class_prior = dplyr::case_when(
        prior_seasons_total_games < 10 ~ "insufficient_data",
        avg_goals_per_game_prior > 1.5 ~ "key_forward",
        avg_goals_per_game_prior > 0.8 ~ "regular_goal_scorer",
        avg_goals_per_game_prior > 0.3 ~ "occasional_goal_scorer",
        TRUE ~ "rare_goal_scorer"
      )
    )
}

#' Calculate Lagged Career Averages
#'
#' Creates lagged career averages for top performing metrics, excluding current game/season
#' Handles game-by-game metrics differently from season-level Brownlow votes
#'
#' @param stats DataFrame with player statistics including brownlow_votes
#' @return DataFrame with lagged career average features
#' @export
calculate_lagged_career_averages <- function(stats) {

  # Define the top 14 metrics for career averages based on feature importance
  game_metrics <- c(
    "coaches_votes", "supercoach_score", "afl_fantasy_score",
    "player_disposal_share", "impact_score", "player_impact_share",
    "opposition_score", "team_out_scored_opposition", "player_goal_share",
    "goals", "score_involvement", "disposals", "rating_points", "metres_gained"
  )

  # Filter to only metrics that exist in the dataset
  available_metrics <- intersect(game_metrics, names(stats))

  if (length(available_metrics) == 0) {
    message("No target metrics found for career averages. Available columns checked:")
    message(paste(head(names(stats), 20), collapse = ", "))
    return(stats)
  }

  message(sprintf("Creating lagged career averages for %d metrics: %s",
                  length(available_metrics), paste(head(available_metrics, 5), collapse = ", ")))

  # Part 1: Game-by-game career averages (exclude current game)
  stats_with_game_averages <- stats %>%
    dplyr::arrange(player_name, date) %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Calculate career games played so far (excluding current game)
      career_games_so_far = pmax(0, dplyr::row_number() - 1),

      # For each available metric, calculate cumulative average excluding current game
      !!!purrr::map(available_metrics, function(metric) {
        if (metric %in% names(stats)) {
          # Use lag to exclude current game, then calculate cumulative mean
          rlang::expr(ifelse(career_games_so_far > 0,
                            dplyr::cummean(dplyr::lag(!!rlang::sym(metric), default = 0)),
                            0))
        } else {
          rlang::expr(0)
        }
      }) %>%
      purrr::set_names(paste0("career_avg_", available_metrics))
    ) %>%
    dplyr::ungroup()

  # Part 2: Season-level Brownlow votes career average (if available)
  if ("brownlow_votes" %in% names(stats)) {

    # Calculate season-level Brownlow summaries
    brownlow_season_summary <- stats %>%
      dplyr::group_by(player_name, season) %>%
      dplyr::summarise(
        season_total_brownlow_votes = sum(brownlow_votes, na.rm = TRUE),
        season_games = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(player_name, season) %>%
      dplyr::group_by(player_name) %>%
      dplyr::mutate(
        # Count prior complete seasons
        prior_seasons_count = pmax(0, dplyr::row_number() - 1),

        # Total votes and games from all prior seasons (exclude current season)
        prior_total_brownlow_votes = pmax(0, cumsum(dplyr::lag(season_total_brownlow_votes, default = 0))),
        prior_total_games = pmax(0, cumsum(dplyr::lag(season_games, default = 0))),

        # Career average Brownlow votes per game from prior seasons
        career_avg_brownlow_votes_prior_seasons = ifelse(
          prior_total_games > 0,
          prior_total_brownlow_votes / prior_total_games,
          NA_real_
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(player_name, season, career_avg_brownlow_votes_prior_seasons)

    # Join back to main dataset
    stats_with_game_averages <- stats_with_game_averages %>%
      dplyr::left_join(brownlow_season_summary, by = c("player_name", "season"))

    message("Added career average for brownlow_votes based on prior complete seasons")
  }

  # Validation: Check for data leakage
  validation_cols <- paste0("career_avg_", available_metrics)

  for (col in validation_cols) {
    if (col %in% names(stats_with_game_averages)) {
      # For each player, check that career average never equals current game value
      current_metric <- stringr::str_remove(col, "career_avg_")
      if (current_metric %in% names(stats_with_game_averages)) {

        # Check for potential leakage (career average exactly equals current value when > 0)
        potential_leakage <- stats_with_game_averages %>%
          dplyr::filter(
            !is.na(!!rlang::sym(col)) & !!rlang::sym(col) > 0 &
            !is.na(!!rlang::sym(current_metric)) & !!rlang::sym(current_metric) > 0 &
            abs(!!rlang::sym(col) - !!rlang::sym(current_metric)) < 0.001
          )

        if (nrow(potential_leakage) > 10) {  # Allow some tolerance for legitimate cases
          message(sprintf("Warning: Potential data leakage detected in %s (perfect matches: %d)",
                          col, nrow(potential_leakage)))
        }
      }
    }
  }

  # Feature summary
  created_features <- names(stats_with_game_averages)[!names(stats_with_game_averages) %in% names(stats)]
  message(sprintf("Created %d lagged career average features: %s",
                  length(created_features),
                  paste(head(created_features, 3), collapse = ", ")))

  return(stats_with_game_averages)
}

#' Create Comprehensive Performance-Based Historical Features
#'
#' Main function that combines all performance-based historical feature calculations
#' NO brownlow_votes dependencies in main pipeline, lagged averages handle it separately
#'
#' @param stats DataFrame with player statistics (votes optional)
#' @return DataFrame with comprehensive performance-based historical features
#' @export
create_comprehensive_performance_historical_features <- function(stats) {

  # Calculate career performance summary
  career_performance <- calculate_career_performance_summary(stats)

  # Join career features to main stats
  stats_with_history <- stats %>%
    dplyr::left_join(career_performance, by = "player_name") %>%
    dplyr::mutate(
      # Replace NAs for new players
      dplyr::across(c(career_total_games, career_avg_disposals, career_avg_goals,
                      career_avg_tackles, career_avg_marks, career_avg_fantasy,
                      career_avg_impact), ~tidyr::replace_na(.x, 0)),

      # Career stage indicators
      career_games_experience = career_total_games,
      career_stage = dplyr::case_when(
        career_total_games < 10 ~ "rookie",
        career_total_games < 50 ~ "developing",
        career_total_games < 150 ~ "established",
        career_total_games < 250 ~ "veteran",
        TRUE ~ "legend"
      )
    )

  # Add recent trends, position patterns, and prior season features
  stats_enhanced <- stats_with_history %>%
    calculate_recent_performance_trends() %>%
    calculate_position_performance_patterns() %>%
    calculate_prior_season_performance_averages() %>%
    calculate_lagged_career_averages()  # Add the new lagged career averages

  return(stats_enhanced)
}