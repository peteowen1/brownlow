# Team-Level Feature Utilities
# Functions for creating team context and umpire-related features


#' Calculate Team Performance Statistics
#'
#' Calculates team-level performance statistics for each match
#'
#' @param stats DataFrame with player statistics
#' @return DataFrame with team performance features
#' @export
calculate_team_performance_stats <- function(stats) {
  stats %>%
    dplyr::group_by(date, team, season) %>%
    dplyr::summarise(
      team_total_disposals = sum(kicks + handballs, na.rm = TRUE),
      team_total_goals = sum(goals, na.rm = TRUE),
      team_total_tackles = sum(tackles, na.rm = TRUE),
      team_total_marks = sum(marks, na.rm = TRUE),
      team_total_contested_poss = sum(contested_possessions, na.rm = TRUE),
      team_total_clearances = sum(clearances, na.rm = TRUE),
      team_total_inside_fifties = sum(inside_fifties, na.rm = TRUE),
      team_players_played = dplyr::n(),
      team_avg_fantasy_score = mean(fantasy_score, na.rm = TRUE),
      team_total_impact_score = sum(impact_score, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Add Match Context from Results
#'
#' Adds match context information from results data
#'
#' @param team_stats DataFrame with team performance statistics
#' @param results DataFrame with match results
#' @return DataFrame with match context features
#' @export
add_match_context_from_results <- function(team_stats, results) {

  if (is.null(results)) {
    return(team_stats)
  }

  # Prepare results data for joining
  team_context <- results %>%
    tidyr::pivot_longer(
      cols = c(match_home_team, match_away_team),
      names_to = "home_away",
      values_to = "team"
    ) %>%
    dplyr::mutate(
      is_home = ifelse(home_away == "match_home_team", 1, 0),
      team_score = ifelse(is_home == 1, home_score, away_score),
      opposition_score = ifelse(is_home == 1, away_score, home_score),
      team_won = dplyr::case_when(
        result == "Home Win" & is_home == 1 ~ 1,
        result == "Away Win" & is_home == 0 ~ 1,
        TRUE ~ 0
      ),
      match_margin_abs = abs(team_score - opposition_score),
      close_game = as.numeric(match_margin_abs <= 12),  # Within 2 goals
      blowout = as.numeric(match_margin_abs >= 60)      # 10+ goal margin
    ) %>%
    dplyr::select(date, team, is_home, team_score, opposition_score, team_won,
           match_margin_abs, close_game, blowout, margin, result)

  # Join with team stats
  team_stats %>%
    dplyr::left_join(team_context, by = c("date", "team"))
}

#' Calculate Player Share of Team Performance
#'
#' Calculates each player's share of their team's performance
#'
#' @param stats DataFrame with player statistics
#' @param team_stats DataFrame with team performance statistics
#' @return DataFrame with player share features
#' @export
calculate_player_team_shares <- function(stats, team_stats) {

  stats %>%
    dplyr::left_join(team_stats, by = c("date", "team", "season")) %>%
    dplyr::mutate(
      # Player's share of team performance
      player_disposal_share = ifelse(team_total_disposals > 0,
                                     (kicks + handballs) / team_total_disposals, 0),
      player_goal_share = ifelse(team_total_goals > 0, goals / team_total_goals, 0),
      player_tackle_share = ifelse(team_total_tackles > 0, tackles / team_total_tackles, 0),
      player_mark_share = ifelse(team_total_marks > 0, marks / team_total_marks, 0),
      player_contested_poss_share = ifelse(team_total_contested_poss > 0,
                                           contested_possessions / team_total_contested_poss, 0),
      player_clearance_share = ifelse(team_total_clearances > 0,
                                      clearances / team_total_clearances, 0),
      player_impact_share = ifelse(team_total_impact_score > 0,
                                   impact_score / team_total_impact_score, 0),

      # Dominance indicators (high share of team performance)
      high_disposal_share = as.numeric(player_disposal_share > 0.15),  # >15% of team's disposals
      high_goal_share = as.numeric(player_goal_share > 0.25),          # >25% of team's goals
      high_tackle_share = as.numeric(player_tackle_share > 0.20),      # >20% of team's tackles
      high_impact_share = as.numeric(player_impact_share > 0.15),      # >15% of team's impact

      # Relative performance vs team average
      above_team_avg_fantasy = as.numeric(fantasy_score > team_avg_fantasy_score),

      # Team context indicators
      key_player = as.numeric(player_disposal_share > 0.12 | player_impact_share > 0.12),
      role_player = as.numeric(player_disposal_share < 0.05 & player_impact_share < 0.05)
    )
}

#' Calculate Team Form and Trends
#'
#' Calculates rolling team form and performance trends
#'
#' @param team_stats DataFrame with team performance and match context
#' @param windows Vector of rolling window sizes (default: c(3, 5, 10))
#' @return DataFrame with team form features
#' @export
calculate_team_form_trends <- function(team_stats, windows = c(3, 5, 10)) {

  team_form <- team_stats %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      # Basic team performance trends
      team_games_played = dplyr::row_number(),
      team_wins_so_far = cumsum(team_won),
      team_win_rate = team_wins_so_far / team_games_played,

      # Recent scoring trends
      recent_scoring_avg = zoo::rollmean(team_score, k = 5, fill = NA, align = "right"),
      recent_conceding_avg = zoo::rollmean(opposition_score, k = 5, fill = NA, align = "right"),
      recent_margin_avg = zoo::rollmean(match_margin, k = 5, fill = NA, align = "right")
    ) %>%
    dplyr::ungroup()

  # Add rolling form features for each window
  for (window in windows) {
    team_form <- team_form %>%
      dplyr::group_by(team) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        # Rolling win rate
        !!paste0("team_win_rate_", window) :=
          zoo::rollmean(team_won, k = window, fill = NA, align = "right"),

        # Rolling performance metrics
        !!paste0("team_total_disposals_avg_", window) :=
          zoo::rollmean(team_total_disposals, k = window, fill = NA, align = "right"),

        !!paste0("team_total_goals_avg_", window) :=
          zoo::rollmean(team_total_goals, k = window, fill = NA, align = "right"),

        # Form indicators
        !!paste0("good_form_", window) :=
          as.numeric(.data[[paste0("team_win_rate_", window)]] > 0.6),

        !!paste0("poor_form_", window) :=
          as.numeric(.data[[paste0("team_win_rate_", window)]] < 0.4)
      ) %>%
      dplyr::ungroup()
  }

  return(team_form)
}

#' Create Top Umpire Feature Matrix
#'
#' Creates one-hot encoded features for the most active umpires
#'
#' @param stats DataFrame with umpire information
#' @param top_n Integer. Number of top umpires to create features for (default: 20)
#' @return List with umpire features and metadata
#' @export
create_umpire_feature_matrix <- function(stats, top_n = 20) {

  # Find umpire columns (more flexible pattern matching)
  umpire_cols <- names(stats)[stringr::str_detect(names(stats), "umpire|Umpire")]

  # Debug: show what columns we have
  all_cols_sample <- head(names(stats), 20)
  message(sprintf("Sample columns available: %s", paste(all_cols_sample, collapse = ", ")))

  if (length(umpire_cols) == 0) {
    message("No umpire columns found in data - checking for alternative patterns...")

    # Try alternative patterns
    alt_umpire_cols <- names(stats)[stringr::str_detect(names(stats), "field_umpire|boundary_umpire|goal_umpire")]

    if (length(alt_umpire_cols) > 0) {
      umpire_cols <- alt_umpire_cols
      message(sprintf("Found alternative umpire columns: %s", paste(umpire_cols, collapse = ", ")))
    } else {
      message("No umpire columns found - returning original data")
      return(list(
        data = stats,
        umpire_features = character(0),
        summary = "No umpire data available"
      ))
    }
  }

  message(sprintf("Found %d umpire columns: %s", length(umpire_cols), paste(umpire_cols, collapse = ", ")))

  # Count games for each umpire
  umpire_counts <- tibble::tibble()

  for (col in umpire_cols) {
    if (col %in% names(stats)) {
      col_counts <- stats %>%
        dplyr::select(!!dplyr::sym(col)) %>%
        dplyr::filter(!is.na(!!dplyr::sym(col)) & !!dplyr::sym(col) != "") %>%
        dplyr::mutate(!!dplyr::sym(col) := stringr::str_trim(!!dplyr::sym(col))) %>%  # Remove leading/trailing spaces
        dplyr::count(!!dplyr::sym(col)) %>%
        dplyr::rename(umpire = !!dplyr::sym(col), games = n)

      umpire_counts <- dplyr::bind_rows(umpire_counts, col_counts)
    }
  }

  # Find top umpires
  top_umpires <- umpire_counts %>%
    dplyr::group_by(umpire) %>%
    dplyr::summarise(total_games = sum(games), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_games)) %>%
    dplyr::slice_head(n = top_n)

  message(sprintf("Top %d umpires by games officiated:", nrow(top_umpires)))
  print(head(top_umpires, 10))

  top_umpire_names <- top_umpires$umpire

  # Clean umpire names for column names using janitor
  clean_umpire_names <- paste0("umpire_", janitor::make_clean_names(top_umpire_names))

  # Create one-hot encoded columns
  stats_with_umpires <- stats

  for (i in seq_along(top_umpire_names)) {
    umpire_name <- top_umpire_names[i]
    col_name <- clean_umpire_names[i]

    # Check if this umpire appears in any umpire column for each game
    stats_with_umpires[[col_name]] <- 0

    for (ump_col in umpire_cols) {
      if (ump_col %in% names(stats)) {
        stats_with_umpires[[col_name]] <- ifelse(
          stats_with_umpires[[ump_col]] == umpire_name & !is.na(stats_with_umpires[[ump_col]]),
          1,
          stats_with_umpires[[col_name]]
        )
      }
    }
  }

  return(list(
    data = stats_with_umpires,
    umpire_features = clean_umpire_names,
    top_umpires = top_umpires,
    summary = sprintf("Created features for %d top umpires", length(top_umpire_names))
  ))
}

#' Calculate Umpire Summary Features
#'
#' Creates summary features for umpire analysis
#'
#' @param stats DataFrame with umpire columns
#' @param umpire_cols Vector of umpire column names
#' @param top_umpire_names Vector of top umpire names
#' @return DataFrame with umpire summary features
#' @export
calculate_umpire_summary_features <- function(stats, umpire_cols, top_umpire_names) {

  stats %>%
    dplyr::mutate(
      # Number of umpires in the game
      num_umpires = rowSums(!is.na(dplyr::select(., dplyr::all_of(umpire_cols)))),

      # Count of "other" umpires (not in top list)
      other_umpires = {
        other_count <- rep(0, nrow(.))

        for (i in 1:nrow(.)) {
          count <- 0
          for (ump_col in umpire_cols) {
            if (ump_col %in% names(.)) {
              umpire_name <- .[[ump_col]][i]
              if (!is.na(umpire_name) && umpire_name != "" &&
                  !umpire_name %in% top_umpire_names) {
                count <- count + 1
              }
            }
          }
          other_count[i] <- count
        }
        other_count
      },

      # Umpire experience level (based on presence of top umpires)
      experienced_umpire_game = {
        umpire_feature_cols <- dplyr::select(., dplyr::starts_with("umpire_")) %>%
          dplyr::select_if(is.numeric)
        if (ncol(umpire_feature_cols) > 0) {
          as.numeric(rowSums(umpire_feature_cols) > 0)
        } else {
          rep(0, nrow(.))
        }
      },
      rookie_umpire_game = as.numeric(other_umpires >= num_umpires * 0.5)
    )
}

#' Create Comprehensive Team Features
#'
#' Main function that combines all team-level feature calculations
#'
#' @param stats DataFrame with player statistics
#' @param results DataFrame with match results (optional)
#' @param include_umpires Logical. Include individual umpire name features (default: FALSE)
#' @param top_umpires Integer. Number of top umpires for features (default: 0)
#' @return List with enhanced data and feature metadata
#' @export
create_comprehensive_team_features <- function(stats_df, results = NULL, include_umpires = FALSE, top_umpires = 0) {

  # Calculate team performance statistics
  message("Calculating team performance statistics...")
  team_performance <- calculate_team_performance_stats(stats_df)

  # Add match context if results available
  if (!is.null(results)) {
    message("Adding match context from results...")
    team_performance <- add_match_context_from_results(team_performance, results)

    # Calculate team form trends
    message("Calculating team form trends...")
    team_performance <- calculate_team_form_trends(team_performance)
  }

  # Calculate player shares of team performance
  message("Calculating player share of team performance...")
  stats_with_team <- calculate_player_team_shares(stats_df, team_performance)

  # Add enhanced scoring features
  message("Creating enhanced scoring features...")
  stats_with_team <- create_comprehensive_enhanced_scoring_features(stats_with_team)

  # Always create basic umpire summary features
  umpire_cols <- names(stats_df)[stringr::str_detect(names(stats_df), "umpire")]
  umpire_result <- NULL

  if (length(umpire_cols) > 0) {
    message("Creating basic umpire features...")
    # Create basic umpire summary features without individual names
    stats_with_team <- stats_with_team %>%
      dplyr::mutate(
        # Number of umpires in the game
        num_umpires = rowSums(!is.na(dplyr::select(., dplyr::all_of(umpire_cols)))),
        # Basic experience indicators (simple heuristics)
        experienced_umpire_game = as.numeric(num_umpires >= 3),  # Simplified: 3+ umpires typically means experienced crew
        rookie_umpire_game = as.numeric(num_umpires < 3)  # Simplified: fewer umpires might indicate newer crew
      )
  }

  # Add individual umpire name features if specifically requested
  if (include_umpires && top_umpires > 0) {
    message("Creating individual umpire name features...")
    umpire_result <- create_umpire_feature_matrix(stats_with_team, top_umpires)
    stats_with_team <- umpire_result$data
  }

  return(list(
    data = stats_with_team,
    team_performance = team_performance,
    umpire_info = umpire_result,
    feature_summary = list(
      team_features = c("player_disposal_share", "player_goal_share", "player_tackle_share",
                        "high_disposal_share", "high_goal_share", "key_player"),
      umpire_features = if (!is.null(umpire_result)) umpire_result$umpire_features else character(0)
    )
  ))
}

#' Calculate Enhanced Scoring Features
#'
#' Creates detailed scoring context features for teams and matches
#'
#' @param stats DataFrame with team performance and match results
#' @return DataFrame with enhanced scoring features
#' @export
calculate_enhanced_scoring_features <- function(stats) {

  stats %>%
    dplyr::mutate(
      # Total points scored in match
      total_match_points = dplyr::coalesce(team_score, 0) + dplyr::coalesce(opposition_score, 0),

      # Scoring game classification - simplified to remove lowest-importance features
      # Removed: low_scoring_game (gain: 0.00000673), high_scoring_game (gain: 0.0000190)
      moderate_scoring_game = as.numeric(total_match_points >= 150 & total_match_points < 200), # 150-200 points (gain: 0.000178)
      very_high_scoring_game = as.numeric(total_match_points >= 250),  # 250+ points

      # Individual team scoring context
      team_low_score = as.numeric(dplyr::coalesce(team_score, 0) < 70),     # Team < 70 points
      team_high_score = as.numeric(dplyr::coalesce(team_score, 0) > 120),   # Team > 120 points
      team_very_high_score = as.numeric(dplyr::coalesce(team_score, 0) > 150), # Team > 150 points

      # Opposition scoring context
      opposition_low_score = as.numeric(dplyr::coalesce(opposition_score, 0) < 70),
      opposition_high_score = as.numeric(dplyr::coalesce(opposition_score, 0) > 120),

      # Match margin and context (create these first)
      match_margin_abs = abs(dplyr::coalesce(team_score, 0) - dplyr::coalesce(opposition_score, 0)),
      close_game = as.numeric(match_margin_abs <= 12),  # Within 2 goals
      blowout = as.numeric(match_margin_abs >= 60),     # 10+ goal margin

      # Game flow indicators
      defensive_slog = as.numeric(total_match_points < 120),      # Very low scoring
      shootout = as.numeric(total_match_points > 280),           # Very high scoring
      one_sided_high_scoring = as.numeric(total_match_points >= 200 & blowout == 1),
      competitive_high_scoring = as.numeric(total_match_points >= 200 & close_game == 1),

      # Team scoring relative to opposition
      team_out_scored_opposition = as.numeric(dplyr::coalesce(team_score, 0) > dplyr::coalesce(opposition_score, 0)),
      significant_scoring_advantage = as.numeric((dplyr::coalesce(team_score, 0) - dplyr::coalesce(opposition_score, 0)) > 30),

      # Season-relative scoring
      season_avg_team_score = ave(dplyr::coalesce(team_score, 0), season, FUN = function(x) mean(x, na.rm = TRUE)),
      season_avg_total_score = ave(total_match_points, season, FUN = function(x) mean(x, na.rm = TRUE)),
      above_season_avg_team_score = as.numeric(dplyr::coalesce(team_score, 0) > season_avg_team_score),
      above_season_avg_total_score = as.numeric(total_match_points > season_avg_total_score)
    )
}

#' Calculate Player Scoring Context Features
#'
#' Creates features based on how players perform in different scoring environments
#'
#' @param stats DataFrame with player stats and enhanced scoring features
#' @return DataFrame with player scoring context features
#' @export
calculate_player_scoring_context <- function(stats) {

  if (!all(c("moderate_scoring_game", "team_score", "opposition_score") %in% names(stats))) {
    message("Enhanced scoring features not found - run calculate_enhanced_scoring_features first")
    return(stats)
  }

  stats %>%
    dplyr::group_by(player_name) %>%
    dplyr::mutate(
      # Career performance in different scoring contexts (simplified - removed low/high binary flags)
      games_team_high_score = cumsum(dplyr::coalesce(team_high_score, 0)),
      games_moderate_scoring = cumsum(dplyr::coalesce(moderate_scoring_game, 0)),

      # Performance rates in different scoring contexts (focus on team performance and moderate scoring)
      high_disposal_games_team_high_score = cumsum((kicks + handballs >= 25) * dplyr::coalesce(team_high_score, 0)),
      high_disposal_games_moderate_scoring = cumsum((kicks + handballs >= 25) * dplyr::coalesce(moderate_scoring_game, 0)),

      # Performance rates by scoring context (simplified)
      high_disposal_rate_team_high_score = ifelse(games_team_high_score > 0, high_disposal_games_team_high_score / games_team_high_score, 0),
      high_disposal_rate_moderate_scoring = ifelse(games_moderate_scoring > 0, high_disposal_games_moderate_scoring / games_moderate_scoring, 0),

      # Player preferences for scoring contexts (simplified to remove low-importance features)
      thrives_when_team_scores_well = as.numeric(games_team_high_score >= 5 & high_disposal_rate_team_high_score > high_disposal_rate_moderate_scoring),

      # Impact in different game flows
      impact_in_shootouts = cumsum(impact_score * dplyr::coalesce(shootout, 0)),
      impact_in_defensive_slogs = cumsum(impact_score * dplyr::coalesce(defensive_slog, 0)),
      games_shootouts = cumsum(dplyr::coalesce(shootout, 0)),
      games_defensive_slogs = cumsum(dplyr::coalesce(defensive_slog, 0)),

      avg_impact_shootouts = ifelse(games_shootouts > 0, impact_in_shootouts / games_shootouts, 0),
      avg_impact_defensive_slogs = ifelse(games_defensive_slogs > 0, impact_in_defensive_slogs / games_defensive_slogs, 0),

      # Adaptability removed - referenced removed low/high scoring features
      # Keeping as comment since this was a low-importance feature anyway
    ) %>%
    dplyr::ungroup()
}

#' Create Game Type Classification Features
#'
#' Creates features that classify games by type and style
#'
#' @param stats DataFrame with scoring and context features
#' @return DataFrame with game type classification features
#' @export
create_game_type_features <- function(stats) {

  stats %>%
    dplyr::mutate(
      # Comprehensive game type classification
      game_type = dplyr::case_when(
        defensive_slog == 1 & close_game == 1 ~ "defensive_thriller",
        defensive_slog == 1 & blowout == 1 ~ "defensive_blowout",
        shootout == 1 & close_game == 1 ~ "high_scoring_thriller",
        shootout == 1 & blowout == 1 ~ "high_scoring_blowout",
        close_game == 1 & moderate_scoring_game == 1 ~ "close_contest",
        blowout == 1 & moderate_scoring_game == 1 ~ "standard_blowout",
        TRUE ~ "standard_game"
      ),

      # Game type indicators for modeling
      defensive_thriller = as.numeric(game_type == "defensive_thriller"),
      high_scoring_thriller = as.numeric(game_type == "high_scoring_thriller"),
      any_thriller = as.numeric(stringr::str_detect(game_type, "thriller")),
      any_blowout = as.numeric(stringr::str_detect(game_type, "blowout")),

      # Voting distribution expectations by game type
      expected_vote_concentration_high = as.numeric(any_blowout == 1 | one_sided_high_scoring == 1),
      expected_vote_distribution_even = as.numeric(close_game == 1 & moderate_scoring_game == 1),

      # Historical context
      season_game_type_frequency = ave(as.numeric(game_type == game_type), season, game_type,
                                       FUN = function(x) length(x)),

      # Venue scoring context
      venue_avg_total_score = ave(total_match_points, venue_name, FUN = function(x) mean(x, na.rm = TRUE)),
      high_scoring_venue = as.numeric(venue_avg_total_score > quantile(venue_avg_total_score, 0.75, na.rm = TRUE)),
      low_scoring_venue = as.numeric(venue_avg_total_score < quantile(venue_avg_total_score, 0.25, na.rm = TRUE))
    )
}

#' Create Comprehensive Enhanced Scoring Features
#'
#' Main function that combines all enhanced scoring feature calculations
#'
#' @param stats DataFrame with team performance and match results
#' @return DataFrame with comprehensive enhanced scoring features
#' @export
create_comprehensive_enhanced_scoring_features <- function(stats) {

  message("Creating comprehensive enhanced scoring features...")

  # Check if basic scoring data is available
  score_cols <- names(stats)[stringr::str_detect(names(stats), "team_score|opposition_score|home_score|away_score")]

  if (!any(c("team_score", "opposition_score") %in% names(stats))) {
    # Try different score column patterns
    home_score_cols <- names(stats)[stringr::str_detect(names(stats), "home.*score")]
    away_score_cols <- names(stats)[stringr::str_detect(names(stats), "away.*score")]

    if (length(home_score_cols) > 0 && length(away_score_cols) > 0) {
      home_score_col <- home_score_cols[1]
      away_score_col <- away_score_cols[1]
      message(sprintf("Using %s/%s for enhanced scoring features...", home_score_col, away_score_col))

      # Create team_score and opposition_score from home/away scores
      stats <- stats %>%
        dplyr::mutate(
          # First determine if team is playing at home
          is_home = as.numeric(team == match_home_team),
          # Then create team and opposition scores
          team_score = ifelse(is_home == 1,
                              dplyr::coalesce(.data[[home_score_col]], 0),
                              dplyr::coalesce(.data[[away_score_col]], 0)),
          opposition_score = ifelse(is_home == 1,
                                    dplyr::coalesce(.data[[away_score_col]], 0),
                                    dplyr::coalesce(.data[[home_score_col]], 0))
        )
    } else {
      message("No team scoring data found - skipping enhanced scoring features")
      return(stats)
    }
  }

  # Apply all enhanced scoring feature functions in sequence
  result <- stats %>%
    calculate_enhanced_scoring_features() %>%
    # calculate_player_scoring_context() %>%
    create_game_type_features()

  # Count scoring features created
  scoring_features <- names(result)[stringr::str_detect(names(result),
    "scoring_|score_|shootout|slog|game_type|thriller|blowout")]

  message(sprintf("Created %d enhanced scoring features: %s",
                  length(scoring_features),
                  paste(head(scoring_features, 5), collapse = ", ")))

  return(result)
}

#' Validate Team Features
#'
