# Weather Feature Utilities
# Functions for creating weather-related features from meteorological data


#' Create Weather Categorical Features
#'
#' Creates categorical weather features from weather data
#'
#' @param stats DataFrame with weather columns from enhanced dataset
#' @return DataFrame with categorical weather features
#' @export
create_weather_categorical_features <- function(stats) {

  # Find available weather columns
  weather_cols <- names(stats)[stringr::str_detect(names(stats), "weather|temp|wind|rain")]

  if (length(weather_cols) == 0) {
    message("No weather columns found - returning original data")
    return(stats)
  }

  message(sprintf("Found %d weather columns: %s", length(weather_cols), paste(weather_cols[1:min(5, length(weather_cols))], collapse = ", ")))

  stats_with_weather <- stats

  # Create general weather condition features if weather description available
  weather_desc_cols <- names(stats)[stringr::str_detect(names(stats), "weather.*type|match_weather$")]
  if (length(weather_desc_cols) > 0) {
    weather_desc_col <- weather_desc_cols[1]  # Use first weather description column

    stats_with_weather <- stats_with_weather %>%
      dplyr::mutate(
        weather_condition = dplyr::coalesce(.data[[weather_desc_col]], "Unknown"),
        weather_fine = as.numeric(stringr::str_detect(stringr::str_to_lower(weather_condition), "fine|sunny|clear|fair")),
        weather_wet = as.numeric(stringr::str_detect(stringr::str_to_lower(weather_condition), "rain|shower|wet|drizzle")),
        weather_windy = as.numeric(stringr::str_detect(stringr::str_to_lower(weather_condition), "wind|breezy|gusty")),
        weather_overcast = as.numeric(stringr::str_detect(stringr::str_to_lower(weather_condition), "cloud|overcast|grey")),
        # weather_extreme removed - very low importance (gain: 0.0000149)
      )
  }

  # Create temperature features if temperature data available
  temp_cols <- names(stats)[stringr::str_detect(names(stats), "temp")]
  if (length(temp_cols) > 0) {
    temp_col <- temp_cols[1]  # Use first temperature column

    stats_with_weather <- stats_with_weather %>%
      dplyr::mutate(
        temperature = as.numeric(.data[[temp_col]]),
        temp_cold = as.numeric(!is.na(temperature) & temperature < 15),      # Below 15°C
        temp_mild = as.numeric(!is.na(temperature) & temperature >= 15 & temperature < 25),  # 15-25°C
        temp_warm = as.numeric(!is.na(temperature) & temperature >= 25 & temperature < 30),  # 25-30°C
        temp_hot = as.numeric(!is.na(temperature) & temperature >= 30),      # Above 30°C
        temp_extreme_cold = as.numeric(!is.na(temperature) & temperature < 10),
        temp_extreme_hot = as.numeric(!is.na(temperature) & temperature > 35)
      )
  }

  # Create wind features if wind data available
  wind_cols <- names(stats)[stringr::str_detect(names(stats), "wind")]
  if (length(wind_cols) > 0) {
    wind_col <- wind_cols[1]

    stats_with_weather <- stats_with_weather %>%
      dplyr::mutate(
        wind_speed = .data[[wind_col]],
        wind_calm = as.numeric(!is.na(wind_speed) & wind_speed < 10),       # Less than 10 km/h
        wind_light = as.numeric(!is.na(wind_speed) & wind_speed >= 10 & wind_speed < 20),  # 10-20 km/h
        wind_moderate = as.numeric(!is.na(wind_speed) & wind_speed >= 20 & wind_speed < 30), # 20-30 km/h
        wind_strong = as.numeric(!is.na(wind_speed) & wind_speed >= 30)     # Above 30 km/h
      )
  }

  return(stats_with_weather)
}

#' Create Player Weather Performance Features
#'
#' Creates features based on how players perform in different weather conditions
#'
#' @param stats DataFrame with weather features and player performance
#' @return DataFrame with player weather interaction features
#' @export
create_player_weather_performance <- function(stats) {

  if (!any(c("weather_fine", "weather_wet", "weather_windy") %in% names(stats))) {
    message("Weather categorical features not found - run create_weather_categorical_features first")
    return(stats)
  }

  stats %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Career performance in different weather conditions
      games_in_fine_weather = cumsum(dplyr::coalesce(weather_fine, 0)),
      games_in_wet_weather = cumsum(dplyr::coalesce(weather_wet, 0)),
      games_in_windy_weather = cumsum(dplyr::coalesce(weather_windy, 0)),

      # High disposal rate by weather condition (removed brownlow_votes dependency)
      high_disposal_rate_fine = ifelse(games_in_fine_weather > 0,
                                       cumsum((kicks + handballs >= 25) * dplyr::coalesce(weather_fine, 0)) / games_in_fine_weather, 0),
      high_disposal_rate_wet = ifelse(games_in_wet_weather > 0,
                                     cumsum((kicks + handballs >= 25) * dplyr::coalesce(weather_wet, 0)) / games_in_wet_weather, 0),
      high_disposal_rate_windy = ifelse(games_in_windy_weather > 0,
                                       cumsum((kicks + handballs >= 25) * dplyr::coalesce(weather_windy, 0)) / games_in_windy_weather, 0),

      # Weather preference indicators (based on disposal performance)
      prefers_fine_weather = as.numeric(games_in_fine_weather >= 5 & high_disposal_rate_fine > high_disposal_rate_wet & high_disposal_rate_fine > high_disposal_rate_windy),
      prefers_wet_weather = as.numeric(games_in_wet_weather >= 3 & high_disposal_rate_wet > high_disposal_rate_fine),
      weather_adaptable = as.numeric(games_in_fine_weather >= 3 & games_in_wet_weather >= 3 &
                                     abs(high_disposal_rate_fine - high_disposal_rate_wet) < 0.05)
    ) %>%
    dplyr::ungroup()
}

#' Create Weather Context Features
#'
#' Creates match-level weather context features that may affect voting patterns
#'
#' @param stats DataFrame with weather data
#' @return DataFrame with weather context features
#' @export
create_weather_context_features <- function(stats) {

  stats %>%
    dplyr::mutate(
      # difficult_conditions removed - very low importance (gain: 0.0000124)
      # The underlying weather components (wet, windy, temperature) provide better signal

      # Ideal playing conditions (check if wind features exist)
      ideal_conditions = as.numeric(
        dplyr::coalesce(weather_fine, 0) == 1 &
        dplyr::coalesce(temp_mild, 0) == 1 &
        ifelse("wind_calm" %in% names(.), dplyr::coalesce(wind_calm, 0) == 1, TRUE)
      ),

      # Weather affecting game style
      defensive_weather = as.numeric(dplyr::coalesce(weather_wet, 0) == 1 | dplyr::coalesce(weather_windy, 0) == 1),
      attacking_weather = as.numeric(
        dplyr::coalesce(weather_fine, 0) == 1 &
        ifelse("wind_calm" %in% names(.), dplyr::coalesce(wind_calm, 0) == 1, TRUE)
      ),

      # Season context
      winter_game = as.numeric(lubridate::month(date) %in% c(6, 7, 8)),
      summer_game = as.numeric(lubridate::month(date) %in% c(12, 1, 2)),

      # Weather impact on voting (in difficult conditions, fewer players might dominate)
      # weather_impact_high removed - was based on removed difficult_conditions feature
      weather_impact_low = as.numeric(ideal_conditions == 1)
    )
}

#' Create Team Weather Performance Features
#'
#' Creates features based on team performance in different weather conditions
#'
#' @param stats DataFrame with weather and team data
#' @return DataFrame with team weather performance features
#' @export
create_team_weather_performance <- function(stats) {

  # Create team_won if it doesn't exist
  if (!"team_won" %in% names(stats)) {
    stats <- stats %>%
      dplyr::mutate(
        team_won = as.numeric(dplyr::coalesce(team_score, 0) > dplyr::coalesce(opposition_score, 0))
      )
  }

  # Create team-level weather performance summaries
  team_weather_stats <- stats %>%
    dplyr::group_by(team, season) %>%
    dplyr::summarise(
      team_games_fine_weather = sum(dplyr::coalesce(weather_fine, 0), na.rm = TRUE),
      team_games_wet_weather = sum(dplyr::coalesce(weather_wet, 0), na.rm = TRUE),
      team_wins_fine_weather = sum(dplyr::coalesce(weather_fine, 0) * dplyr::coalesce(team_won, 0), na.rm = TRUE),
      team_wins_wet_weather = sum(dplyr::coalesce(weather_wet, 0) * dplyr::coalesce(team_won, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      team_win_rate_fine = ifelse(team_games_fine_weather > 0, team_wins_fine_weather / team_games_fine_weather, 0),
      team_win_rate_wet = ifelse(team_games_wet_weather > 0, team_wins_wet_weather / team_games_wet_weather, 0),
      team_weather_advantage = team_win_rate_fine - team_win_rate_wet
    )

  # Join back to main data
  stats %>%
    dplyr::left_join(team_weather_stats, by = c("team", "season")) %>%
    dplyr::mutate(
      # Team weather context
      team_good_weather_team = as.numeric(team_weather_advantage > 0.1),
      team_all_weather_team = as.numeric(abs(team_weather_advantage) <= 0.05),
      team_struggles_weather = as.numeric(team_weather_advantage < -0.1)
    )
}

#' Create Comprehensive Weather Features
#'
#' Main function that combines all weather feature calculations
#'
#' @param stats DataFrame with weather data from enhanced dataset
#' @return DataFrame with comprehensive weather features
#' @export
create_comprehensive_weather_features <- function(stats) {

  message("Creating comprehensive weather features...")

  # Check if weather data is available
  weather_cols <- names(stats)[stringr::str_detect(names(stats), "weather|temp|wind|rain")]

  if (length(weather_cols) == 0) {
    message("No weather data found - skipping weather feature creation")
    return(stats)
  }

  # Apply all weather feature functions in sequence
  result <- stats %>%
    create_weather_categorical_features() %>%
    # create_player_weather_performance() %>%
    create_weather_context_features() %>%
    create_team_weather_performance()

  # Count weather features created
  weather_features <- names(result)[stringr::str_detect(names(result), "weather_|temp_|wind_|ideal_|difficult_|attacking_|defensive_")]

  message(sprintf("Created %d weather features: %s",
                  length(weather_features),
                  paste(head(weather_features, 5), collapse = ", ")))

  return(result)
}

#' Validate Weather Features
#'
#' Validates weather features are within expected ranges
#'
#' @param stats DataFrame with weather features
#' @return List with validation results and cleaned data
#' @export
validate_weather_features <- function(stats) {

  validation_results <- list()

  # Find weather feature columns
  weather_feature_cols <- names(stats)[stringr::str_detect(names(stats), "weather_|temp_|wind_")]

  if (length(weather_feature_cols) == 0) {
    return(list(
      validation_results = list(message = "No weather features found"),
      cleaned_data = stats
    ))
  }

  # Check binary features are 0/1
  binary_features <- weather_feature_cols[stringr::str_detect(weather_feature_cols, "weather_|temp_|wind_")]

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

  validation_results$weather_features_created <- length(weather_feature_cols)
  validation_results$sample_features <- head(weather_feature_cols, 10)

  return(list(
    validation_results = validation_results,
    cleaned_data = cleaned_stats
  ))
}
