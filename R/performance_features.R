# Player Performance Feature Utilities
# Functions for creating player-level performance and efficiency features


#' Calculate Basic Efficiency Metrics
#'
#' Creates fundamental efficiency and ratio metrics from raw statistics
#'
#' @param stats DataFrame with player statistics
#' @return DataFrame with basic efficiency features
#' @export
calculate_efficiency_metrics <- function(stats) {
  stats %>%
    dplyr::mutate(
      # Disposal efficiency
      disposal_efficiency = ifelse(kicks + handballs > 0,
                                   (kicks + handballs - clangers) / (kicks + handballs), 0),

      # Goal accuracy
      goal_accuracy = ifelse(goals + behinds > 0, goals / (goals + behinds), 0),

      # Contested possession ratio
      contested_ratio = ifelse(contested_possessions + uncontested_possessions > 0,
                               contested_possessions / (contested_possessions + uncontested_possessions), 0),

      # Clearance effectiveness
      clearance_efficiency = ifelse(contested_possessions > 0, clearances / contested_possessions, 0),

      # Time on ground percentage (assuming 120 minutes total)
      tog_percentage = pmin(dplyr::coalesce(time_on_ground_percentage, 100) / 100, 1)
    )
}

#' Calculate Impact Scores
#'
#' Creates weighted impact scores for different aspects of performance
#'
#' @param stats DataFrame with player statistics
#' @return DataFrame with impact score features
#' @export
calculate_impact_scores <- function(stats) {
  stats %>%
    dplyr::mutate(
      # Scoring impact
      score_involvement = goals * 6 + behinds + dplyr::coalesce(goal_assists, 0) * 3,

      # Defensive impact
      defensive_impact = tackles + dplyr::coalesce(one_percenters, 0) + dplyr::coalesce(spoils, 0),

      # Marking impact
      marking_impact = marks + dplyr::coalesce(contested_marks, 0) * 2 + dplyr::coalesce(marks_inside_fifty, 0) * 3,

      # Overall impact score (weighted combination of key stats)
      impact_score = (goals * 6) + (disposals * 1) + (marks * 2) + (tackles * 4) +
                     (contested_possessions * 2) + (clearances * 3) + (dplyr::coalesce(goal_assists, 0) * 3),

      # Fantasy/Supercoach points if available
      fantasy_score = dplyr::coalesce(afl_fantasy_score, supercoach_score, 0)
    )
}

#' Calculate Position-Specific Scores
#'
#' Creates position-specific performance metrics
#'
#' @param stats DataFrame with player statistics
#' @return DataFrame with position-specific features
#' @export
calculate_position_scores <- function(stats) {
  stats %>%
    dplyr::mutate(
      # Forward-specific scoring
      forward_score = goals * 6 + dplyr::coalesce(marks_inside_fifty, 0) * 3 + dplyr::coalesce(goal_assists, 0) * 3,

      # Midfielder-specific scoring
      midfield_score = clearances * 3 + contested_possessions * 2 + inside_fifties * 2,

      # Defensive-specific scoring
      defensive_score = rebounds * 2 + dplyr::coalesce(one_percenters, 0) * 2 + dplyr::coalesce(spoils, 0) * 2
    )
}

#' Calculate Per-Minute Statistics
#'
#' Normalizes statistics by time on ground
#'
#' @param stats DataFrame with player statistics including tog_percentage
#' @return DataFrame with per-minute features
#' @export
calculate_per_minute_stats <- function(stats) {
  stats %>%
    dplyr::mutate(
      # Per-minute statistics (normalized by time on ground)
      disposals_per_minute = ifelse(tog_percentage > 0, (kicks + handballs) / (tog_percentage * 120), 0),
      tackles_per_minute = ifelse(tog_percentage > 0, tackles / (tog_percentage * 120), 0),
      marks_per_minute = ifelse(tog_percentage > 0, marks / (tog_percentage * 120), 0),
      contested_poss_per_minute = ifelse(tog_percentage > 0, contested_possessions / (tog_percentage * 120), 0)
    )
}

#' Add Match Context Features
#'
#' Adds match-specific context features from results data
#'
#' @param stats DataFrame with player statistics
#' @param results DataFrame with match results
#' @return DataFrame with match context features
#' @export
add_match_context_features <- function(stats, results) {
  
  if (is.null(results)) {
    return(stats)
  }
  
  # Create match context from results
  results_context <- results %>%
    dplyr::select(date, match_home_team, match_away_team, home_score, away_score, margin, result) %>%
    dplyr::mutate(
      winning_team = dplyr::case_when(
        result == "Home Win" ~ match_home_team,
        result == "Away Win" ~ match_away_team,
        TRUE ~ NA_character_
      )
    ) %>%
    tidyr::pivot_longer(
      cols = c(match_home_team, match_away_team),
      names_to = "home_away",
      values_to = "team"
    ) %>%
    dplyr::mutate(
      is_home = ifelse(home_away == "match_home_team", 1, 0),
      is_winner = ifelse(!is.na(winning_team) & team == winning_team, 1, 0),
      team_score = ifelse(is_home == 1, home_score, away_score),
      opponent_score = ifelse(is_home == 1, away_score, home_score),
      match_margin_abs = abs(team_score - opponent_score),
      close_game = ifelse(match_margin_abs <= 12, 1, 0),  # Within 2 goals
      blowout = ifelse(match_margin_abs >= 60, 1, 0)      # 10+ goal margin
    )
  
  # Join with stats and add context features
  stats %>%
    dplyr::left_join(results_context, by = c("date", "team" = "team")) %>%
    dplyr::mutate(
      # Winner bonus (players from winning teams often get more votes)
      winner_bonus = is_winner,

      # Home ground advantage
      home_advantage = is_home,

      # Game context multipliers
      impact_score_adjusted = impact_score *
        (1 + (0.2 * is_winner) + (0.1 * is_home) - (0.1 * blowout))
    )
}

#' Create Comprehensive Player Performance Features
#'
#' Main function that combines all player performance feature calculations
#'
#' @param stats DataFrame with raw player statistics
#' @param results DataFrame with match results (optional)
#' @return DataFrame with comprehensive performance features
#' @export
create_comprehensive_performance_features <- function(stats, results = NULL) {
  
  # Preserve important match identification columns (removed brownlow_votes to prevent leakage)
  key_columns <- c("date", "season", "match_round", "team", "player_name",
                   "match_home_team", "match_away_team", "venue_name")
  preserved_columns <- intersect(key_columns, names(stats))
  
  # Ensure we have a consistent player column
  stats_enhanced <- stats %>%
    dplyr::mutate(
      player = if ("player_name" %in% names(.)) player_name else paste(player_first_name, player_last_name),
      # Create proper match_id using date + home_team + away_team
      match_id = if (all(c("match_home_team", "match_away_team") %in% names(.))) {
        paste(date, match_home_team, match_away_team, sep = "_")
      } else if ("date" %in% names(.)) {
        paste(date, team, sep = "_")  # Fallback if home/away teams not available
      } else {
        dplyr::row_number()  # Last resort
      }
    )
  
  # Apply all performance feature calculations in sequence
  result <- stats_enhanced %>%
    calculate_efficiency_metrics() %>%
    calculate_impact_scores() %>%
    calculate_position_scores() %>%
    calculate_per_minute_stats() %>%
    add_match_context_features(results)
  
  # Ensure preserved columns are maintained
  missing_preserved <- setdiff(preserved_columns, names(result))
  if (length(missing_preserved) > 0) {
    preserved_data <- stats[, missing_preserved, drop = FALSE]
    result <- result %>%
      dplyr::bind_cols(preserved_data)
  }
  
  return(result)
}

#' Create Rolling Performance Features
#'
#' Creates rolling averages and trends for key performance metrics
#'
#' @param stats DataFrame with performance features
#' @param metrics Vector of metric names to create rolling features for
#' @param windows Vector of rolling window sizes (default: c(3, 5, 10))
#' @return DataFrame with rolling performance features
#' @export
create_rolling_performance_features <- function(stats, 
                                               metrics = c("impact_score", "disposals", "goals", "tackles", 
                                                          "marks", "contested_possessions", "clearances", "fantasy_score"),
                                               windows = c(3, 5, 10)) {
  
  stats_with_rolling <- stats %>%
    dplyr::arrange(player, date) %>%
    dplyr::group_by(player, season) %>%
    dplyr::mutate(games_this_season = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Create rolling features for each window size and metric
  for (window in windows) {
    for (metric in metrics) {
      if (metric %in% names(stats_with_rolling)) {
        stats_with_rolling <- stats_with_rolling %>%
          dplyr::group_by(player) %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(
            # Rolling mean (EXCLUDE current game)
            !!paste0(metric, "_roll", window, "_mean") :=
              zoo::rollmean(dplyr::lag(.data[[metric]], default = 0), k = window, fill = NA, align = "right"),

            # Rolling standard deviation (EXCLUDE current game)
            !!paste0(metric, "_roll", window, "_sd") :=
              zoo::rollapply(dplyr::lag(.data[[metric]], default = 0), width = window, FUN = stats::sd, fill = NA, align = "right"),

            # Trend indicator (current performance above rolling average)
            !!paste0(metric, "_above_roll", window) :=
              ifelse(!is.na(.data[[paste0(metric, "_roll", window, "_mean")]]),
                     as.numeric(.data[[metric]] > .data[[paste0(metric, "_roll", window, "_mean")]]),
                     0),

            # Rolling momentum (current vs previous rolling average)
            !!paste0(metric, "_momentum", window) :=
              ifelse(!is.na(.data[[paste0(metric, "_roll", window, "_mean")]]) &
                     !is.na(dplyr::lag(.data[[paste0(metric, "_roll", window, "_mean")]])),
                     .data[[paste0(metric, "_roll", window, "_mean")]] -
                     dplyr::lag(.data[[paste0(metric, "_roll", window, "_mean")]]),
                     0)
          ) %>%
          dplyr::ungroup()
      }
    }
  }
  
  return(stats_with_rolling)
}

#' Validate Performance Features
#'
#' Validates that performance features are within expected ranges
#'
#' @param stats DataFrame with performance features
#' @return List with validation results and cleaned data
#' @export
validate_performance_features <- function(stats) {
  
  validation_results <- list()
  
  # Check for infinite and NaN values
  numeric_cols <- names(stats)[sapply(stats, is.numeric)]
  infinite_cols <- numeric_cols[sapply(numeric_cols, function(col) any(is.infinite(stats[[col]]), na.rm = TRUE))]
  nan_cols <- numeric_cols[sapply(numeric_cols, function(col) any(is.nan(stats[[col]]), na.rm = TRUE))]
  
  validation_results$infinite_columns <- infinite_cols
  validation_results$nan_columns <- nan_cols
  
  # Check ratio features are in valid ranges
  ratio_features <- c("disposal_efficiency", "goal_accuracy", "contested_ratio", "clearance_efficiency", "tog_percentage")
  existing_ratios <- intersect(ratio_features, names(stats))
  
  for (ratio in existing_ratios) {
    out_of_range <- sum(stats[[ratio]] < 0 | stats[[ratio]] > 1, na.rm = TRUE)
    if (out_of_range > 0) {
      validation_results[[paste0(ratio, "_out_of_range")]] <- out_of_range
    }
  }
  
  # Clean the data
  cleaned_stats <- stats %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~replace(.x, is.infinite(.x) | is.nan(.x), 0))) %>%
    dplyr::mutate(
      # Ensure ratios are within 0-1 range
      dplyr::across(dplyr::all_of(existing_ratios), ~pmax(0, pmin(1, .x)))
    )
  
  return(list(
    validation_results = validation_results,
    cleaned_data = cleaned_stats
  ))
}