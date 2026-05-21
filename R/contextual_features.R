# Contextual Feature Utilities
# Functions for creating contextual features (venue, time, opposition, match importance)


#' Create Venue-Specific Performance Features
#'
#' Creates features based on player performance at different venues
#'
#' @param stats DataFrame with player statistics and venue information
#' @return DataFrame with venue-specific features
#' @export
create_venue_performance_features <- function(stats) {

  stats %>%
    dplyr::group_by(player_name, venue_name) %>%
    dplyr::mutate(
      # Career performance at this venue (EXCLUDE current game)
      games_at_venue = pmax(0, dplyr::row_number() - 1),
      high_disposal_games_at_venue = cumsum(as.numeric(dplyr::lag(kicks + handballs, default = 0) >= 25)),
      high_disposal_rate_at_venue = ifelse(games_at_venue > 0, high_disposal_games_at_venue / games_at_venue, 0),

      # Performance metrics at venue (EXCLUDE current game)
      avg_impact_at_venue = cummean(dplyr::lag(impact_score, default = 0)),
      avg_disposals_at_venue = cummean(dplyr::lag(kicks + handballs, default = 0)),
      avg_goals_at_venue = cummean(dplyr::lag(goals, default = 0))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Compare venue performance to overall average (EXCLUDE current game)
      career_high_disposal_rate_temp = cummean(as.numeric(dplyr::lag(kicks + handballs, default = 0) >= 25)),
      venue_performance_advantage = high_disposal_rate_at_venue - career_high_disposal_rate_temp,
      venue_specialist = as.numeric(games_at_venue >= 5 & venue_performance_advantage > 0.1),

      # Venue familiarity indicators removed - showed very low importance (frequent_venue: 0.00000930, new_venue: 0.0000443)
      # Keeping games_at_venue as it's more informative than binary flags

      # Home ground indicators
      likely_home_venue = as.numeric(games_at_venue >= 8 & games_at_venue >= max(games_at_venue) * 0.3)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(venue_name) %>%
    dplyr::mutate(
      # Venue characteristics based on aggregate data
      venue_high_scoring = as.numeric(mean(dplyr::coalesce(team_score, 0) + dplyr::coalesce(opposition_score, 0), na.rm = TRUE) >
                                      quantile(c(team_score + opposition_score), 0.75, na.rm = TRUE)),
      venue_low_scoring = as.numeric(mean(dplyr::coalesce(team_score, 0) + dplyr::coalesce(opposition_score, 0), na.rm = TRUE) <
                                     quantile(c(team_score + opposition_score), 0.25, na.rm = TRUE)),

      # Venue voting patterns
      venue_high_disposal_rate = mean(as.numeric((kicks + handballs) >= 25), na.rm = TRUE),
      venue_competitive = as.numeric(mean(close_game, na.rm = TRUE) > 0.4)
    ) %>%
    dplyr::ungroup()
}

#' Create Time-Based Performance Features
#'
#' Creates features based on time of year, day/night, and seasonal effects
#'
#' @param stats DataFrame with date and time information
#' @return DataFrame with time-based features
#' @export
create_time_based_features <- function(stats) {

  stats %>%
    dplyr::mutate(
      # AGGRESSIVE REDUCTION: Keep only essential time features
      # Removed: weekend_game (0.0000380), weeknight_game (0.000133), early_rounds (0.000123),
      # late_rounds (0.000151), anzac_period (0.0000555), kings_birthday_period (0.00000829),
      # winter_football, most season periods

      # Keep only most essential
      match_month = lubridate::month(date),
      match_day_of_week = lubridate::wday(date),
      friday_night = as.numeric(match_day_of_week == 6),  # Friday night has some importance
      final_rounds = as.numeric(match_round >= 22),       # Finals context moderately important
      spring_finals = as.numeric(match_month %in% c(8, 9) & match_round > 20),

      # Define season timing variables
      early_season = as.numeric(match_round <= 6),
      late_season = as.numeric(match_round >= 18)
    ) %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Player performance by time period
      # High disposal games by season timing (removed votes dependency)
      high_disposal_early_season = cumsum((kicks + handballs >= 25) * early_season),
      high_disposal_late_season = cumsum((kicks + handballs >= 25) * late_season),
      # games_early_season = cumsum(early_season),
      # games_late_season = cumsum(late_season),

      # avg_votes_early_season = ifelse(games_early_season > 0, votes_early_season / games_early_season, 0),
      # avg_votes_late_season = ifelse(games_late_season > 0, votes_late_season / games_late_season, 0),

      # # Time-based performance patterns
      # early_season_performer = as.numeric(games_early_season >= 3 & avg_votes_early_season > avg_votes_late_season),
      # late_season_performer = as.numeric(games_late_season >= 3 & avg_votes_late_season > avg_votes_early_season),
      # consistent_across_season = as.numeric(games_early_season >= 3 & games_late_season >= 3 &
      #                                       abs(avg_votes_early_season - avg_votes_late_season) < 0.1),

      # # Prime time performance
      # Performance in prime time games
      high_disposal_prime_time = cumsum((kicks + handballs >= 25) * friday_night),
      # games_prime_time = cumsum(friday_night),
      # avg_votes_prime_time = ifelse(games_prime_time > 0, votes_prime_time / games_prime_time, 0),
      # prime_time_performer = as.numeric(games_prime_time >= 3 & avg_votes_prime_time > career_avg_votes_overall)
    ) %>%
    dplyr::ungroup()
}

#' Create Opposition Strength Features
#'
#' Creates features based on the strength of opposition teams
#'
#' @param stats DataFrame with team and opposition information
#' @return DataFrame with opposition strength features
#' @export
create_opposition_strength_features <- function(stats) {

  # Calculate team strength indicators by season
  team_strength <- stats %>%
    dplyr::group_by(team, season) %>%
    dplyr::summarise(
      team_win_rate = mean(team_won, na.rm = TRUE),
      team_avg_score = mean(team_score, na.rm = TRUE),
      team_avg_margin = mean(abs(team_score - opposition_score), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      # Rank teams within season
      team_strength_rank = rank(-team_win_rate, ties.method = "average"),
      strong_team = as.numeric(team_strength_rank <= 6),     # Top 6 teams
      weak_team = as.numeric(team_strength_rank >= 13),      # Bottom 6 teams
      mid_tier_team = as.numeric(team_strength_rank > 6 & team_strength_rank < 13)
    ) %>%
    dplyr::ungroup()

  # Get opposition team strength
  stats_with_opposition <- stats %>%
    dplyr::mutate(
      # Determine opposition team
      opposition_team = ifelse(is_home == 1, match_away_team, match_home_team)
    ) %>%
    dplyr::left_join(
      team_strength %>% dplyr::select(team, season, strong_team, weak_team, mid_tier_team),
      by = c("opposition_team" = "team", "season")
    ) %>%
    dplyr::rename(
      opposition_strong_team = strong_team,
      opposition_weak_team = weak_team,
      opposition_mid_tier_team = mid_tier_team
    )

  # Add opposition context features
  stats_with_opposition %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Career performance against different opposition strengths
      games_vs_strong = cumsum(dplyr::coalesce(opposition_strong_team, 0)),
      games_vs_weak = cumsum(dplyr::coalesce(opposition_weak_team, 0)),
      # Performance vs different opposition strengths
      high_disposal_vs_strong = cumsum((kicks + handballs >= 25) * dplyr::coalesce(opposition_strong_team, 0)),
      high_disposal_vs_weak = cumsum((kicks + handballs >= 25) * dplyr::coalesce(opposition_weak_team, 0)),
      #
      # avg_votes_vs_strong = ifelse(games_vs_strong > 0, votes_vs_strong / games_vs_strong, 0),
      # avg_votes_vs_weak = ifelse(games_vs_weak > 0, votes_vs_weak / games_vs_weak, 0),

      # # Opposition-specific performance patterns
      # big_game_player = as.numeric(games_vs_strong >= 3 & avg_votes_vs_strong > avg_votes_vs_weak),
      # flat_track_bully = as.numeric(games_vs_weak >= 3 & avg_votes_vs_weak > avg_votes_vs_strong),
      # consistent_vs_all_opposition = as.numeric(games_vs_strong >= 2 & games_vs_weak >= 2 &
      #                                           abs(avg_votes_vs_strong - avg_votes_vs_weak) < 0.15)
    ) %>%
    dplyr::ungroup()
}

#' Create Match Importance Features
#'
#' Creates features based on match importance and context
#'
#' @param stats DataFrame with match and season information
#' @return DataFrame with match importance features
#' @export
create_match_importance_features <- function(stats) {

  stats %>%
    dplyr::mutate(
      # Finals context (approximate - matches in final rounds of season)
      potential_finals = as.numeric(match_round >= 23),  # Finals
      finals_contention = as.numeric(match_round >= 20), # End of season pressure

      # Rivalry and derby games (based on match-ups - simplified)
      melbourne_derby = as.numeric(
        (team %in% c("Richmond", "Collingwood", "Carlton", "Essendon", "Melbourne", "Hawthorn", "St Kilda") &
         opposition_team %in% c("Richmond", "Collingwood", "Carlton", "Essendon", "Melbourne", "Hawthorn", "St Kilda"))),

      adelaide_derby = as.numeric(
        (team %in% c("Adelaide", "Port Adelaide") & opposition_team %in% c("Adelaide", "Port Adelaide"))),

      perth_derby = as.numeric(
        (team %in% c("West Coast", "Fremantle") & opposition_team %in% c("West Coast", "Fremantle"))),

      any_derby = as.numeric(melbourne_derby == 1 | adelaide_derby == 1 | perth_derby == 1),

      # Most special occasion features removed - very low importance
      # anzac_day_game (gain: 0.0000196), queens_birthday_game, good_friday_game all removed

      # AGGRESSIVE REMOVAL: Most contextual features have very low importance
      # Removed: good_friday_game (0.0000103), special_occasion (0.000000972),
      # must_win_context (0.0000531), dead_rubber_context (0.0000231)
      # low_profile_match (0.000104), high_profile_match (0.0000983)

      # Keep only the most essential contextual features
      high_profile_match = as.numeric(friday_night == 1 | any_derby == 1),
      special_occasion = as.numeric(any_derby == 1 | finals_contention == 1)
    ) %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Calculate career baseline
      career_high_disposal_rate_overall = cummean(as.numeric(dplyr::lag(kicks + handballs, default = 0) >= 25)),

      # Player performance in big games
      games_high_profile = cumsum(high_profile_match),
      high_disposal_high_profile = cumsum((kicks + handballs >= 25) * high_profile_match),
      high_disposal_rate_high_profile = ifelse(games_high_profile > 0, high_disposal_high_profile / games_high_profile, 0),

      games_special_occasion = cumsum(special_occasion),
      high_disposal_special_occasion = cumsum((kicks + handballs >= 25) * special_occasion),
      high_disposal_rate_special_occasion = ifelse(games_special_occasion > 0, high_disposal_special_occasion / games_special_occasion, 0),

      # Big game player indicators (based on performance rates)
      big_occasion_player = as.numeric(games_high_profile >= 3 & high_disposal_rate_high_profile > career_high_disposal_rate_overall),
      special_occasion_performer = as.numeric(games_special_occasion >= 2 & high_disposal_rate_special_occasion > career_high_disposal_rate_overall),

      # Consistency under pressure (based on performance rates)
      pressure_game_consistent = as.numeric(games_high_profile >= 5 &
                                            abs(high_disposal_rate_high_profile - career_high_disposal_rate_overall) < 0.05)
    ) %>%
    dplyr::ungroup()
}

#' Create Comprehensive Contextual Features
#'
#' Main function that combines all contextual feature calculations
#'
#' @param stats DataFrame with comprehensive match and player data
#' @return DataFrame with comprehensive contextual features
#' @export
create_comprehensive_contextual_features <- function(stats_df) {

  message("Creating comprehensive contextual features...")

  # Check required columns
  required_cols <- c("venue_name", "date", "team")
  missing_cols <- setdiff(required_cols, names(stats_df))

  if (length(missing_cols) > 0) {
    message(sprintf("Missing required columns for contextual features: %s",
                    paste(missing_cols, collapse = ", ")))
    return(stats_df)
  }

  # Apply all contextual feature functions in sequence
  result <- stats_df %>%
    create_venue_performance_features() %>%
    create_time_based_features() %>%
    create_opposition_strength_features() %>%
    create_match_importance_features()

  # Count contextual features created
  contextual_features <- names(result)[stringr::str_detect(names(result),
    "venue_|time_|opposition_|derby|special_|occasion|profile|season_|round|early_|late_|prime_")]

  message(sprintf("Created %d contextual features: %s",
                  length(contextual_features),
                  paste(head(contextual_features, 5), collapse = ", ")))

  return(result)
}

#' Validate Contextual Features
#'
#' Validates contextual features are within expected ranges
#'
#' @param stats DataFrame with contextual features
#' @return List with validation results and cleaned data
#' @export
validate_contextual_features <- function(stats) {

  validation_results <- list()

  # Find contextual feature columns
  contextual_feature_cols <- names(stats)[stringr::str_detect(names(stats),
    "venue_|opposition_|derby|special_|occasion|profile")]

  if (length(contextual_feature_cols) == 0) {
    return(list(
      validation_results = list(message = "No contextual features found"),
      cleaned_data = stats
    ))
  }

  # Check binary features are 0/1 (updated pattern for new feature names)
  binary_features <- contextual_feature_cols[!stringr::str_detect(contextual_feature_cols, "avg_|games_|rate_|high_disposal_")]

  for (feat in binary_features) {
    if (feat %in% names(stats)) {
      unique_vals <- unique(stats[[feat]][!is.na(stats[[feat]])])
      if (!all(unique_vals %in% c(0, 1))) {
        validation_results[[paste0(feat, "_invalid_binary")]] <- unique_vals
      }
    }
  }

  # Clean the data
  cleaned_stats <- stats %>%
    dplyr::mutate(
      # Ensure binary features are 0/1
      dplyr::across(dplyr::all_of(binary_features), ~ifelse(is.na(.x), 0, pmin(1, pmax(0, .x))))
    )

  validation_results$contextual_features_created <- length(contextual_feature_cols)
  validation_results$sample_features <- head(contextual_feature_cols, 10)

  return(list(
    validation_results = validation_results,
    cleaned_data = cleaned_stats
  ))
}
