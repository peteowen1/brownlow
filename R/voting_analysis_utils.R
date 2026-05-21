# Voting Analysis Utilities
# Functions for analyzing Brownlow voting patterns and player performance distributions


#' Analyze Brownlow Voting Distribution
#'
#' Analyzes the distribution of Brownlow votes
#'
#' @param votes DataFrame with Brownlow votes
#' @param vote_col Character. Column name containing votes (default: "brownlow_votes")
#' @return List with vote distribution analysis
#' @export
analyze_voting_distribution <- function(votes, vote_col = "brownlow_votes") {
  
  if (!vote_col %in% names(votes)) {
    return(list(
      error = paste("Vote column", vote_col, "not found in data"),
      available_columns = names(votes)
    ))
  }
  
  # Vote distribution
  vote_dist <- votes %>%
    dplyr::count(.data[[vote_col]]) %>%
    dplyr::mutate(
      percentage = round(n / sum(n) * 100, 1),
      cumulative_percent = round(cumsum(percentage), 1)
    ) %>%
    dplyr::arrange(.data[[vote_col]])
  
  # Summary statistics
  total_votes_awarded = sum(votes[[vote_col]], na.rm = TRUE)
  total_games = nrow(votes)
  players_with_votes = sum(votes[[vote_col]] > 0, na.rm = TRUE)
  
  # Vote concentration analysis
  vote_concentration <- votes %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      total_votes = sum(.data[[vote_col]], na.rm = TRUE),
      games_with_votes = sum(.data[[vote_col]] > 0, na.rm = TRUE),
      avg_votes_when_polling = mean(.data[[vote_col]][.data[[vote_col]] > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_votes > 0) %>%
    dplyr::arrange(desc(total_votes))
  
  return(list(
    vote_distribution = vote_dist,
    summary_stats = list(
      total_votes_awarded = total_votes_awarded,
      total_games = total_games,
      players_with_votes = players_with_votes,
      vote_rate = round(players_with_votes / total_games * 100, 1)
    ),
    vote_concentration = vote_concentration,
    analysis_date = Sys.time()
  ))
}

#' Analyze Performance Distribution
#'
#' Analyzes player performance distribution using fantasy scores or other metrics
#'
#' @param stats DataFrame with player statistics
#' @param performance_col Character. Column name for performance metric
#' @param performance_name Character. Name for the performance metric in outputs
#' @return List with performance distribution analysis
#' @export
analyze_performance_distribution <- function(stats, performance_col = "afl_fantasy_score", 
                                           performance_name = "AFL Fantasy Score") {
  
  if (!performance_col %in% names(stats)) {
    return(list(
      error = paste("Performance column", performance_col, "not found in data"),
      available_columns = names(stats)
    ))
  }
  
  # Filter to games where they played (score > 0)
  performance_data <- stats %>%
    dplyr::filter(.data[[performance_col]] > 0, !is.na(.data[[performance_col]]))
  
  if (nrow(performance_data) == 0) {
    return(list(
      error = paste("No valid", performance_name, "data found")
    ))
  }
  
  # Performance level categories
  performance_dist <- performance_data %>%
    dplyr::mutate(
      performance_level = dplyr::case_when(
        .data[[performance_col]] >= 120 ~ "Elite (120+)",
        .data[[performance_col]] >= 100 ~ "Excellent (100-119)",
        .data[[performance_col]] >= 80 ~ "Very Good (80-99)",
        .data[[performance_col]] >= 60 ~ "Good (60-79)",
        .data[[performance_col]] >= 40 ~ "Average (40-59)",
        TRUE ~ "Poor (<40)"
      )
    ) %>%
    dplyr::count(performance_level) %>%
    dplyr::mutate(
      percentage = round(n / sum(n) * 100, 1),
      cumulative_percent = round(cumsum(percentage), 1)
    )
  
  # Summary statistics
  performance_summary <- performance_data %>%
    dplyr::summarise(
      total_performances = n(),
      mean_performance = round(mean(.data[[performance_col]], na.rm = TRUE), 1),
      median_performance = round(median(.data[[performance_col]], na.rm = TRUE), 1),
      sd_performance = round(sd(.data[[performance_col]], na.rm = TRUE), 1),
      min_performance = min(.data[[performance_col]], na.rm = TRUE),
      max_performance = max(.data[[performance_col]], na.rm = TRUE),
      q25_performance = round(quantile(.data[[performance_col]], 0.25, na.rm = TRUE), 1),
      q75_performance = round(quantile(.data[[performance_col]], 0.75, na.rm = TRUE), 1)
    )
  
  # Top performers
  top_performers <- performance_data %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      games = n(),
      avg_performance = round(mean(.data[[performance_col]], na.rm = TRUE), 1),
      max_performance = max(.data[[performance_col]], na.rm = TRUE),
      performances_100_plus = sum(.data[[performance_col]] >= 100, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games >= 10) %>%  # Minimum games threshold
    dplyr::arrange(desc(avg_performance))
  
  return(list(
    performance_distribution = performance_dist,
    performance_summary = performance_summary,
    top_performers = top_performers,
    performance_metric = performance_name,
    total_valid_performances = nrow(performance_data),
    analysis_date = Sys.time()
  ))
}

#' Identify Top Vote Getters
#'
#' Identifies and analyzes top Brownlow vote recipients
#'
#' @param votes DataFrame with Brownlow votes
#' @param min_votes Integer. Minimum total votes to be included (default: 5)
#' @param top_n Integer. Number of top players to return (default: 20)
#' @return DataFrame with top vote getters analysis
#' @export
identify_top_vote_getters <- function(votes, min_votes = 5, top_n = 20) {
  
  if (!"brownlow_votes" %in% names(votes)) {
    return(tibble::tibble(error = "brownlow_votes column not found"))
  }
  
  # Calculate player voting statistics
  top_players <- votes %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      total_votes = sum(brownlow_votes, na.rm = TRUE),
      games_played = n(),
      games_with_votes = sum(brownlow_votes > 0, na.rm = TRUE),
      avg_votes_per_game = round(total_votes / games_played, 3),
      avg_votes_when_polling = round(mean(brownlow_votes[brownlow_votes > 0], na.rm = TRUE), 3),
      max_votes_single_game = max(brownlow_votes, na.rm = TRUE),
      vote_rate = round(games_with_votes / games_played * 100, 1),
      three_vote_games = sum(brownlow_votes == 3, na.rm = TRUE),
      two_vote_games = sum(brownlow_votes == 2, na.rm = TRUE),
      one_vote_games = sum(brownlow_votes == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_votes >= min_votes) %>%
    dplyr::arrange(desc(total_votes)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Add team information if available
  if ("team" %in% names(votes)) {
    team_info <- votes %>%
      dplyr::group_by(player_name) %>%
      dplyr::summarise(
        primary_team = names(sort(table(team), decreasing = TRUE))[1],
        teams_played_for = dplyr::n_distinct(team),
        .groups = "drop"
      )
    
    top_players <- top_players %>%
      dplyr::left_join(team_info, by = "player_name")
  }
  
  return(top_players)
}

#' Analyze Team Voting Patterns
#'
#' Analyzes Brownlow voting patterns by team
#'
#' @param votes DataFrame with Brownlow votes including team information
#' @param top_n Integer. Number of top teams to analyze (default: 18)
#' @return List with team voting analysis
#' @export
analyze_team_voting_patterns <- function(votes, top_n = 18) {
  
  if (!all(c("brownlow_votes", "team") %in% names(votes))) {
    return(list(
      error = "Required columns (brownlow_votes, team) not found",
      available_columns = names(votes)
    ))
  }
  
  # Team voting summary
  team_votes <- votes %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      total_votes = sum(brownlow_votes, na.rm = TRUE),
      games_played = n(),
      players_with_votes = sum(brownlow_votes > 0, na.rm = TRUE),
      unique_vote_getters = dplyr::n_distinct(player_name[brownlow_votes > 0]),
      avg_votes_per_game = round(total_votes / dplyr::n_distinct(match_id), 2),
      vote_rate = round(players_with_votes / games_played * 100, 1),
      three_vote_performances = sum(brownlow_votes == 3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(total_votes)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(team_rank = dplyr::row_number())
  
  # Vote distribution by team
  team_vote_distribution <- votes %>%
    dplyr::filter(team %in% team_votes$team) %>%
    dplyr::group_by(team, brownlow_votes) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      total_games = sum(count),
      percentage = round(count / total_games * 100, 1)
    ) %>%
    dplyr::ungroup()
  
  # Team vote concentration (how many players contribute to team's votes)
  team_concentration <- votes %>%
    dplyr::filter(brownlow_votes > 0) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      total_team_votes = sum(brownlow_votes),
      vote_getters = dplyr::n_distinct(player_name),
      top_player_votes = max(tapply(brownlow_votes, player_name, sum)),
      top_player_share = round(top_player_votes / total_team_votes * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_team_votes > 0) %>%
    dplyr::arrange(desc(top_player_share))
  
  return(list(
    team_summary = team_votes,
    vote_distribution = team_vote_distribution,
    vote_concentration = team_concentration,
    analysis_date = Sys.time()
  ))
}

#' Analyze Temporal Voting Patterns
#'
#' Analyzes how Brownlow voting patterns change over time (by round, season)
#'
#' @param votes DataFrame with Brownlow votes including temporal information
#' @return List with temporal analysis
#' @export
analyze_temporal_voting_patterns <- function(votes) {
  
  required_cols <- c("brownlow_votes")
  missing_cols <- setdiff(required_cols, names(votes))
  if (length(missing_cols) > 0) {
    return(list(
      error = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
      available_columns = names(votes)
    ))
  }
  
  temporal_analysis <- list()
  
  # Voting by round (if round information available)
  if ("round" %in% names(votes)) {
    votes_by_round <- votes %>%
      dplyr::filter(!stringr::str_detect(round, "QF|EF|SF|PF|GF")) %>%  # Exclude finals
      dplyr::mutate(round_num = as.numeric(stringr::str_extract(round, "\\d+"))) %>%
      dplyr::filter(!is.na(round_num)) %>%
      dplyr::group_by(round_num) %>%
      dplyr::summarise(
        total_votes = sum(brownlow_votes, na.rm = TRUE),
        avg_votes_per_round = round(mean(brownlow_votes, na.rm = TRUE), 3),
        games_in_round = dplyr::n_distinct(match_id),
        vote_rate = round(sum(brownlow_votes > 0) / n() * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(round_num)
    
    temporal_analysis$round_patterns = votes_by_round
  }
  
  # Voting by season (if season information available)
  if ("season" %in% names(votes)) {
    votes_by_season <- votes %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        total_votes = sum(brownlow_votes, na.rm = TRUE),
        total_games = n(),
        avg_votes_per_game = round(mean(brownlow_votes, na.rm = TRUE), 3),
        vote_rate = round(sum(brownlow_votes > 0) / n() * 100, 1),
        three_vote_games = sum(brownlow_votes == 3, na.rm = TRUE),
        unique_vote_getters = dplyr::n_distinct(player_name[brownlow_votes > 0]),
        .groups = "drop"
      ) %>%
      dplyr::arrange(season)
    
    temporal_analysis$season_patterns = votes_by_season
  }
  
  # Monthly patterns (if date available)
  if ("date" %in% names(votes)) {
    votes_by_month <- votes %>%
      dplyr::mutate(
        month = lubridate::month(date, label = TRUE),
        year = lubridate::year(date)
      ) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(
        total_votes = sum(brownlow_votes, na.rm = TRUE),
        avg_votes = round(mean(brownlow_votes, na.rm = TRUE), 3),
        games = n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(month)
    
    temporal_analysis$monthly_patterns = votes_by_month
  }
  
  temporal_analysis$analysis_date = Sys.time()
  
  return(temporal_analysis)
}

#' Calculate Player Efficiency Metrics
#'
#' Calculates efficiency metrics for players (votes per disposal, etc.)
#'
#' @param stats DataFrame with player statistics including brownlow_votes
#' @param min_games Integer. Minimum games to be included (default: 10)
#' @param min_votes Integer. Minimum total votes to be included (default: 5)
#' @return DataFrame with player efficiency analysis
#' @export
calculate_player_efficiency <- function(stats, min_games = 10, min_votes = 5) {
  
  required_cols <- c("player_name", "brownlow_votes")
  missing_cols <- setdiff(required_cols, names(stats))
  if (length(missing_cols) > 0) {
    return(tibble::tibble(
      error = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Calculate efficiency metrics
  efficiency_data <- stats %>%
    dplyr::filter(!is.na(disposals) | !is.na(kicks) | !is.na(handballs)) %>%
    dplyr::mutate(
      # Create disposals if not available
      disposals = if ("disposals" %in% names(.)) {
        disposals
      } else if (all(c("kicks", "handballs") %in% names(.))) {
        kicks + handballs
      } else {
        NA_real_
      }
    ) %>%
    dplyr::filter(!is.na(disposals)) %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      games = n(),
      total_votes = sum(brownlow_votes, na.rm = TRUE),
      total_disposals = sum(disposals, na.rm = TRUE),
      total_goals = sum(goals, na.rm = TRUE),
      total_tackles = sum(tackles, na.rm = TRUE),
      total_marks = sum(marks, na.rm = TRUE),
      avg_votes_per_game = round(total_votes / games, 3),
      votes_per_disposal = round(total_votes / total_disposals, 4),
      votes_per_100_disposals = round(total_votes / total_disposals * 100, 2),
      disposals_per_game = round(total_disposals / games, 1),
      .groups = "drop"
    ) %>%
    dplyr::filter(games >= min_games, total_votes >= min_votes) %>%
    dplyr::arrange(desc(votes_per_disposal))
  
  # Add efficiency rankings
  efficiency_data <- efficiency_data %>%
    dplyr::mutate(
      efficiency_rank = dplyr::row_number(desc(votes_per_disposal)),
      volume_rank = dplyr::row_number(desc(total_disposals)),
      votes_rank = dplyr::row_number(desc(total_votes))
    )
  
  return(efficiency_data)
}