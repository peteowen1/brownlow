# Simulation Utilities for Brownlow Medal Prediction
#
# This file contains utility functions for Monte Carlo simulation of Brownlow Medal seasons,
# including vote sampling, constraint handling, and results analysis.

#' Sample Votes for a Player Based on Probability Distribution
#'
#' Samples votes (0, 1, 2, 3) for a single player based on their probability distribution
#'
#' @param prob_vector Numeric vector of length 4 with P(0), P(1), P(2), P(3)
#' @param n_samples Integer number of samples to draw (default: 1)
#'
#' @return Vector of sampled votes
#' @export
sample_player_votes <- function(prob_vector, n_samples = 1) {
  # Ensure probabilities are valid
  prob_vector <- pmax(0, prob_vector)
  prob_sum <- sum(prob_vector)

  if (prob_sum <= 0) {
    return(rep(0, n_samples))
  }

  # Normalize probabilities
  prob_vector <- prob_vector / prob_sum

  # Sample from multinomial distribution
  sample(c(0, 1, 2, 3), size = n_samples, prob = prob_vector, replace = TRUE)
}

#' Simulate Votes for Multiple Players in a Match
#'
#' Simulates votes for all players in a single match, ensuring exactly 6 votes total
#'
#' @param match_probs Matrix where each row is a player and columns are P(0), P(1), P(2), P(3)
#' @param match_id Character identifier for the match (optional)
#' @param method Character method for constraint application ("proportional", "threshold")
#'
#' @return Vector of simulated votes for each player
#' @export
simulate_match_votes <- function(match_probs, match_id = NULL, method = "expected_values") {
  n_players <- nrow(match_probs)
  if (n_players == 0) return(numeric(0))

  # NEW APPROACH: Use expected values instead of sampling for consistency with Script 06
  # This preserves the vote totals and relative rankings

  if (method == "expected_values") {
    # REALISTIC BROWNLOW SIMULATION: Allocate 3-2-1 votes sequentially per match
    # Properly redistributes probability mass when players don't get higher votes

    match_votes <- numeric(n_players)

    # Step 1: Allocate 3 votes (best on ground)
    prob_3_votes <- match_probs[, 4]  # 4th column = P(3 votes)
    if (sum(prob_3_votes) > 0) {
      prob_3_votes <- prob_3_votes / sum(prob_3_votes)  # Normalize
      winner_3 <- sample(1:n_players, size = 1, prob = prob_3_votes)
      match_votes[winner_3] <- 3
    }

    # Step 2: Allocate 2 votes with proper probability redistribution
    remaining_players <- setdiff(1:n_players, which(match_votes > 0))
    if (length(remaining_players) > 0) {
      # For remaining players, redistribute their 3-vote probability to 2-vote probability
      prob_2_votes <- match_probs[remaining_players, 3] + match_probs[remaining_players, 4]

      if (sum(prob_2_votes) > 0) {
        prob_2_votes <- prob_2_votes / sum(prob_2_votes)  # Normalize
        winner_2_idx <- sample(length(remaining_players), size = 1, prob = prob_2_votes)
        winner_2 <- remaining_players[winner_2_idx]
        match_votes[winner_2] <- 2
      }
    }

    # Step 3: Allocate 1 vote with proper probability redistribution
    remaining_players <- setdiff(1:n_players, which(match_votes > 0))
    if (length(remaining_players) > 0) {
      # For remaining players, redistribute their 3-vote and 2-vote probability to 1-vote
      prob_1_vote <- match_probs[remaining_players, 2] +
                     match_probs[remaining_players, 3] +
                     match_probs[remaining_players, 4]

      if (sum(prob_1_vote) > 0) {
        prob_1_vote <- prob_1_vote / sum(prob_1_vote)  # Normalize
        winner_1_idx <- sample(length(remaining_players), size = 1, prob = prob_1_vote)
        winner_1 <- remaining_players[winner_1_idx]
        match_votes[winner_1] <- 1
      }
    }

    # Verification: Should always sum to 6 (3 + 2 + 1 = 6)
    if (sum(match_votes) != 6) {
      warning(sprintf("Match votes sum to %d instead of 6", sum(match_votes)))
    }

    return(match_votes)
  }

  # LEGACY APPROACH: Sample votes (can cause low vote totals)
  # Sample initial votes for each player
  simulated_votes <- numeric(n_players)

  for (i in 1:n_players) {
    simulated_votes[i] <- sample_player_votes(match_probs[i, ])
  }

  # Apply match constraint: exactly 6 votes per match
  total_votes <- sum(simulated_votes)

  if (total_votes != 6 && method == "proportional") {
    # Use proportional scaling approach
    match_ids <- rep(if(is.null(match_id)) 1 else match_id, n_players)
    simulated_votes <- apply_voting_constraints(simulated_votes, match_ids, method = "proportional")

    # Round to integers
    simulated_votes <- round(simulated_votes)

    # Final adjustment to ensure exactly 6 votes
    current_total <- sum(simulated_votes)
    diff <- 6 - current_total

    if (diff != 0) {
      simulated_votes <- adjust_vote_total(simulated_votes, diff)
    }
  } else if (total_votes != 6 && method == "threshold") {
    # Use threshold approach: assign 3, 2, 1 to top players
    simulated_votes <- apply_threshold_constraint(simulated_votes)
  }

  return(pmax(0, pmin(3, simulated_votes)))
}

#' Adjust Vote Total to Exactly 6
#'
#' Internal function to make fine adjustments to vote totals
#'
#' @param votes Vector of current vote assignments
#' @param diff Integer difference needed (positive to add votes, negative to remove)
#'
#' @return Adjusted vote vector
adjust_vote_total <- function(votes, diff) {
  if (diff > 0) {
    # Need to add votes
    candidates <- which(votes < 3)
    if (length(candidates) > 0) {
      add_to <- sample(candidates, min(abs(diff), length(candidates)), replace = TRUE)
      for (idx in add_to) {
        if (votes[idx] < 3) votes[idx] <- votes[idx] + 1
      }
    }
  } else if (diff < 0) {
    # Need to remove votes
    candidates <- which(votes > 0)
    if (length(candidates) > 0) {
      remove_from <- sample(candidates, min(abs(diff), length(candidates)), replace = TRUE)
      for (idx in remove_from) {
        if (votes[idx] > 0) votes[idx] <- votes[idx] - 1
      }
    }
  }

  return(votes)
}

#' Apply Threshold Constraint (Top 3 Players Get Votes)
#'
#' Assigns 3, 2, 1 votes to the three players with highest expected votes
#'
#' @param votes Vector of simulated votes
#'
#' @return Adjusted vote vector with exactly 6 total votes
apply_threshold_constraint <- function(votes) {
  n_players <- length(votes)
  constrained_votes <- rep(0, n_players)

  if (n_players >= 3) {
    # Find top 3 players by simulated votes
    top_3_idx <- order(votes, decreasing = TRUE)[1:3]
    constrained_votes[top_3_idx[1]] <- 3
    constrained_votes[top_3_idx[2]] <- 2
    constrained_votes[top_3_idx[3]] <- 1
  } else if (n_players == 2) {
    top_2_idx <- order(votes, decreasing = TRUE)[1:2]
    constrained_votes[top_2_idx[1]] <- 3
    constrained_votes[top_2_idx[2]] <- 3
  } else if (n_players == 1) {
    constrained_votes[1] <- 6
  }

  return(constrained_votes)
}

#' Simulate Complete Season for All Players
#'
#' Runs Monte Carlo simulation for an entire season using probability data
#'
#' @param sim_data DataFrame with player data and probability columns
#' @param prob_prefix Character prefix for probability columns (e.g., "xgb_prob_")
#' @param match_method Character method for match constraint application
#' @param seed Integer random seed for reproducibility
#'
#' @return DataFrame with player season totals
#' @export
simulate_complete_season <- function(sim_data, prob_prefix = "xgb_prob_",
                                    match_method = "expected_values", seed = NULL) {  # Changed default

  if (!is.null(seed)) set.seed(seed)

  # Check probability columns exist
  prob_cols <- paste0(prob_prefix, 0:3)
  missing_cols <- setdiff(prob_cols, names(sim_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing probability columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Initialize player totals
  player_totals <- sim_data %>%
    dplyr::select(player_name, team) %>%
    dplyr::distinct() %>%
    dplyr::mutate(total_votes = 0)

  # Get unique matches - match_id should already be created in feature engineering
  if (!"match_id" %in% names(sim_data)) {
    sim_data <- sim_data %>%
      dplyr::mutate(match_id = if (all(c("match_home_team", "match_away_team") %in% names(.))) {
        paste(date, match_home_team, match_away_team, sep = "_")
      } else {
        paste(date, team, sep = "_")
      })
  }

  unique_matches <- unique(sim_data$match_id)

  # Simulate each match
  for (match in unique_matches) {
    match_data <- sim_data %>% dplyr::filter(match_id == match)

    if (nrow(match_data) > 0) {
      # Extract probability matrix for this match
      match_probs <- match_data %>%
        dplyr::select(dplyr::all_of(prob_cols)) %>%
        as.matrix()

      # Simulate votes for this match
      match_votes <- simulate_match_votes(match_probs, match, match_method)

      # Add votes to player totals
      for (i in 1:nrow(match_data)) {
        player_key <- paste(match_data$player_name[i], match_data$team[i], sep = "_")
        player_idx <- which(paste(player_totals$player_name, player_totals$team, sep = "_") == player_key)

        if (length(player_idx) > 0) {
          player_totals$total_votes[player_idx[1]] <- player_totals$total_votes[player_idx[1]] + match_votes[i]
        }
      }
    }
  }

  return(player_totals)
}

#' Simulate Single Season (furrr-compatible wrapper)
#'
#' Simple wrapper around simulate_complete_season for use with furrr::future_map()
#'
#' @param sim_data DataFrame with player data and probability columns
#' @param prob_prefix Character prefix for probability columns (e.g., "xgb_prob_")
#' @param simulation_id Integer simulation identifier (for reproducible seeds)
#' @param method Character method for match constraint application (default: "expected_values")
#'
#' @return DataFrame with player season totals plus simulation_id column
#' @export
simulate_single_season <- function(sim_data, prob_prefix = "xgb_prob_",
                                  simulation_id = 1, method = "expected_values") {

  # Set seed based on simulation_id for reproducibility
  seed <- simulation_id

  # Run the core simulation
  result <- simulate_complete_season(
    sim_data = sim_data,
    prob_prefix = prob_prefix,
    match_method = method,
    seed = seed
  )

  # Add simulation identifier
  result$simulation_id <- simulation_id

  return(result)
}

#' Run Multiple Season Simulations
#'
#' Runs multiple Monte Carlo season simulations and returns all results
#'
#' @param sim_data DataFrame with player data and probability columns
#' @param prob_prefix Character prefix for probability columns
#' @param n_simulations Integer number of simulations to run
#' @param match_method Character method for match constraint application
#' @param parallel Logical whether to use parallel processing
#' @param n_cores Integer number of cores to use (default: detectCores() - 1)
#' @param progress Logical whether to show progress updates
#'
#' @return List of season simulation results
#' @export
run_multiple_season_simulations <- function(sim_data, prob_prefix = "xgb_prob_",
                                          n_simulations = 1000, match_method = "expected_values",
                                          parallel = TRUE, n_cores = NULL, progress = TRUE) {
  # Implementation placeholder - this function appears to be missing its body
  stop("Function implementation missing")
}

#' Calculate Win Probabilities from Simulation Results
#'
#' Analyzes simulation results to calculate win probabilities and other statistics
#'
#' @param simulation_results List of season simulation results
#' @param model_name Character name for the model
#'
#' @return DataFrame with detailed player statistics
#' @export
calculate_win_probabilities <- function(simulation_results, model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Get all unique players
  all_players <- simulation_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Initialize results matrix
  results_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(results_matrix) <- all_players$player_key

  # Fill results matrix
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(results_matrix)) {
        results_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Calculate basic statistics
  player_stats <- data.frame(
    player_name = all_players$player_name,
    team = all_players$team,
    model = model_name,

    # Vote statistics
    mean_votes = rowMeans(results_matrix),
    median_votes = apply(results_matrix, 1, median),
    sd_votes = apply(results_matrix, 1, sd),
    min_votes = apply(results_matrix, 1, min),
    max_votes = apply(results_matrix, 1, max),

    # Percentiles
    q10_votes = apply(results_matrix, 1, quantile, probs = 0.10),
    q25_votes = apply(results_matrix, 1, quantile, probs = 0.25),
    q75_votes = apply(results_matrix, 1, quantile, probs = 0.75),
    q90_votes = apply(results_matrix, 1, quantile, probs = 0.90),

    stringsAsFactors = FALSE
  )

  # Calculate position-based statistics
  win_counts <- rep(0, nrow(all_players))
  top3_counts <- rep(0, nrow(all_players))
  top5_counts <- rep(0, nrow(all_players))
  top10_counts <- rep(0, nrow(all_players))
  position_sums <- rep(0, nrow(all_players))

  for (i in 1:n_sims) {
    season_votes <- results_matrix[, i]

    # Handle winners (including ties)
    max_votes <- max(season_votes)
    winners <- which(season_votes == max_votes)

    # Distribute win credit among tied winners
    win_credit <- 1 / length(winners)
    win_counts[winners] <- win_counts[winners] + win_credit

    # Calculate positions (1 = highest votes, ties get average rank)
    positions <- rank(-season_votes, ties.method = "average")
    position_sums <- position_sums + positions

    # Count top finishes
    top3_counts[positions <= 3] <- top3_counts[positions <= 3] + 1
    top5_counts[positions <= 5] <- top5_counts[positions <= 5] + 1
    top10_counts[positions <= 10] <- top10_counts[positions <= 10] + 1
  }

  # Add probability statistics
  player_stats$win_probability <- win_counts / n_sims * 100
  player_stats$top3_probability <- top3_counts / n_sims * 100
  player_stats$top5_probability <- top5_counts / n_sims * 100
  player_stats$top10_probability <- top10_counts / n_sims * 100
  player_stats$expected_position <- position_sums / n_sims

  # Calculate confidence intervals for win probability
  player_stats$win_prob_ci_lower <- apply(results_matrix, 1, function(votes) {
    wins <- sapply(1:n_sims, function(i) max(results_matrix[, i]) == votes[i])
    if (sum(wins) < 2) return(0)
    binom.test(sum(wins), n_sims)$conf.int[1] * 100
  })

  player_stats$win_prob_ci_upper <- apply(results_matrix, 1, function(votes) {
    wins <- sapply(1:n_sims, function(i) max(results_matrix[, i]) == votes[i])
    if (sum(wins) < 2) return(0)
    binom.test(sum(wins), n_sims)$conf.int[2] * 100
  })

  # Sort by win probability, then by expected votes
  player_stats <- player_stats %>%
    dplyr::arrange(desc(win_probability), desc(mean_votes))

  return(player_stats)
}

#' Calculate Flexible Top N Finish Probabilities
#'
#' Calculates the probability of each player finishing in the top N positions
#' for any specified values of N (extends the basic top3/5/10 analysis)
#'
#' @param simulation_results List of season simulation results
#' @param n_values Vector of integers specifying top N positions to calculate
#' @param model_name Character name for the model (optional)
#'
#' @return DataFrame with player info and probability columns for each N value
#' @export
calculate_top_n_probabilities <- function(simulation_results, n_values = c(1, 3, 5, 10, 15, 20), model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Validate n_values
  n_values <- unique(sort(n_values))
  n_values <- n_values[n_values > 0]

  if (length(n_values) == 0) {
    stop("No valid n_values provided")
  }

  # Get all unique players
  all_players <- simulation_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Initialize results matrix
  results_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(results_matrix) <- all_players$player_key

  # Fill results matrix with vote totals
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(results_matrix)) {
        results_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Initialize count matrices for each N value
  top_n_counts <- list()
  for (n in n_values) {
    top_n_counts[[as.character(n)]] <- rep(0, nrow(all_players))
  }

  # Calculate top N finishes for each simulation
  for (i in 1:n_sims) {
    season_votes <- results_matrix[, i]
    positions <- rank(-season_votes, ties.method = "average")

    # Count top N finishes for each specified N
    for (n in n_values) {
      top_n_counts[[as.character(n)]][positions <= n] <-
        top_n_counts[[as.character(n)]][positions <= n] + 1
    }
  }

  # Create results data frame
  result_df <- data.frame(
    player_name = all_players$player_name,
    team = all_players$team,
    model = model_name,
    stringsAsFactors = FALSE
  )

  # Add probability columns for each N value
  for (n in n_values) {
    col_name <- paste0("top", n, "_probability")
    result_df[[col_name]] <- top_n_counts[[as.character(n)]] / n_sims * 100
  }

  # Add expected position for context
  position_sums <- rep(0, nrow(all_players))
  for (i in 1:n_sims) {
    season_votes <- results_matrix[, i]
    positions <- rank(-season_votes, ties.method = "average")
    position_sums <- position_sums + positions
  }
  result_df$expected_position <- position_sums / n_sims

  # Sort by most relevant top N probability (use smallest N as primary sort)
  primary_col <- paste0("top", min(n_values), "_probability")
  result_df <- result_df %>%
    dplyr::arrange(desc(.data[[primary_col]]), expected_position)

  return(result_df)
}

#' Calculate Team Best Player Probabilities
#'
#' Calculates the probability of each player being the highest vote getter on their team
#'
#' @param simulation_results List of season simulation results
#' @param model_name Character name for the model (optional)
#'
#' @return DataFrame with player info and team_best_probability
#' @export
calculate_team_best_probabilities <- function(simulation_results, model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Get all unique players
  all_players <- simulation_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Get unique teams
  teams <- unique(all_players$team)

  # Initialize results matrix
  results_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(results_matrix) <- all_players$player_key

  # Fill results matrix with vote totals
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(results_matrix)) {
        results_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Initialize team best counts
  team_best_counts <- rep(0, nrow(all_players))

  # For each simulation, find the best player on each team
  for (i in 1:n_sims) {
    season_votes <- results_matrix[, i]

    for (team in teams) {
      # Get players on this team
      team_players <- all_players$player_key[all_players$team == team]
      team_player_indices <- which(rownames(results_matrix) %in% team_players)

      if (length(team_player_indices) > 0) {
        # Find the highest vote total for this team
        team_votes <- season_votes[team_player_indices]
        max_votes <- max(team_votes)

        # Find all players tied for highest on team
        best_players <- team_player_indices[team_votes == max_votes]

        # Distribute credit among tied players
        credit <- 1 / length(best_players)
        team_best_counts[best_players] <- team_best_counts[best_players] + credit
      }
    }
  }

  # Create results data frame
  result_df <- data.frame(
    player_name = all_players$player_name,
    team = all_players$team,
    model = model_name,
    team_best_probability = team_best_counts / n_sims * 100,
    stringsAsFactors = FALSE
  )

  # Add team context statistics
  team_stats <- result_df %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      team_max_probability = max(team_best_probability),
      team_most_likely_best = player_name[which.max(team_best_probability)],
      .groups = "drop"
    )

  result_df <- result_df %>%
    dplyr::left_join(team_stats, by = "team")

  # Sort by team best probability
  result_df <- result_df %>%
    dplyr::arrange(desc(team_best_probability), team, player_name)

  return(result_df)
}

#' Calculate Vote Threshold Probabilities
#'
#' Calculates the probability of each player exceeding various vote thresholds
#'
#' @param simulation_results List of season simulation results
#' @param thresholds Vector of vote thresholds to analyze
#' @param model_name Character name for the model (optional)
#'
#' @return DataFrame with player info and probability columns for each threshold
#' @export
calculate_vote_threshold_probabilities <- function(simulation_results, thresholds = c(10, 15, 20, 25, 30), model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Validate thresholds
  thresholds <- unique(sort(thresholds))
  thresholds <- thresholds[thresholds > 0]

  if (length(thresholds) == 0) {
    stop("No valid thresholds provided")
  }

  # Get all unique players
  all_players <- simulation_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Initialize results matrix
  results_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(results_matrix) <- all_players$player_key

  # Fill results matrix with vote totals
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(results_matrix)) {
        results_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Calculate threshold probabilities
  threshold_counts <- list()
  for (threshold in thresholds) {
    # Count simulations where each player exceeds the threshold
    threshold_counts[[as.character(threshold)]] <- rowSums(results_matrix > threshold)
  }

  # Create results data frame
  result_df <- data.frame(
    player_name = all_players$player_name,
    team = all_players$team,
    model = model_name,
    mean_votes = rowMeans(results_matrix),
    stringsAsFactors = FALSE
  )

  # Add probability columns for each threshold
  for (threshold in thresholds) {
    col_name <- paste0("over_", threshold, "_probability")
    result_df[[col_name]] <- threshold_counts[[as.character(threshold)]] / n_sims * 100
  }

  # Add percentile statistics for context
  result_df$p90_votes <- apply(results_matrix, 1, quantile, probs = 0.90)
  result_df$p75_votes <- apply(results_matrix, 1, quantile, probs = 0.75)
  result_df$p25_votes <- apply(results_matrix, 1, quantile, probs = 0.25)
  result_df$p10_votes <- apply(results_matrix, 1, quantile, probs = 0.10)

  # Sort by highest threshold probability (most stringent threshold)
  primary_col <- paste0("over_", max(thresholds), "_probability")
  if (primary_col %in% names(result_df)) {
    result_df <- result_df %>%
      dplyr::arrange(desc(.data[[primary_col]]), desc(mean_votes))
  } else {
    result_df <- result_df %>%
      dplyr::arrange(desc(mean_votes))
  }

  return(result_df)
}

#' Calculate Team Dominance Probabilities
#'
#' Calculates the probability of each team polling the most total votes
#'
#' @param simulation_results List of season simulation results
#' @param model_name Character name for the model (optional)
#'
#' @return DataFrame with team info and dominance statistics
#' @export
calculate_team_dominance_probabilities <- function(simulation_results, model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Get all unique teams
  all_teams <- unique(simulation_results[[1]]$team)

  if (length(all_teams) == 0) {
    stop("No teams found in simulation results")
  }

  # Initialize team dominance counts
  team_dominance_counts <- rep(0, length(all_teams))
  names(team_dominance_counts) <- all_teams

  # Track team total votes for each simulation
  team_totals_matrix <- matrix(0, nrow = length(all_teams), ncol = n_sims)
  rownames(team_totals_matrix) <- all_teams

  # Calculate team totals for each simulation
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]]

    # Calculate total votes per team for this simulation
    team_totals <- sim_result %>%
      dplyr::group_by(team) %>%
      dplyr::summarise(total_votes = sum(total_votes, na.rm = TRUE), .groups = "drop")

    # Fill matrix with team totals
    for (j in 1:nrow(team_totals)) {
      team_name <- team_totals$team[j]
      if (team_name %in% rownames(team_totals_matrix)) {
        team_totals_matrix[team_name, i] <- team_totals$total_votes[j]
      }
    }

    # Find team(s) with highest total votes
    max_votes <- max(team_totals$total_votes)
    winning_teams <- team_totals$team[team_totals$total_votes == max_votes]

    # Distribute credit among tied teams
    credit <- 1 / length(winning_teams)
    for (team in winning_teams) {
      team_dominance_counts[team] <- team_dominance_counts[team] + credit
    }
  }

  # Calculate summary statistics for each team
  team_stats <- data.frame(
    team = all_teams,
    model = model_name,
    dominance_probability = (team_dominance_counts[all_teams] / n_sims * 100),
    mean_total_votes = rowMeans(team_totals_matrix),
    median_total_votes = apply(team_totals_matrix, 1, median),
    sd_total_votes = apply(team_totals_matrix, 1, sd),
    min_total_votes = apply(team_totals_matrix, 1, min),
    max_total_votes = apply(team_totals_matrix, 1, max),
    stringsAsFactors = FALSE
  )

  # Add percentile statistics
  team_stats$p90_total_votes <- apply(team_totals_matrix, 1, quantile, probs = 0.90)
  team_stats$p75_total_votes <- apply(team_totals_matrix, 1, quantile, probs = 0.75)
  team_stats$p25_total_votes <- apply(team_totals_matrix, 1, quantile, probs = 0.25)
  team_stats$p10_total_votes <- apply(team_totals_matrix, 1, quantile, probs = 0.10)

  # Calculate relative statistics
  team_stats$votes_per_simulation <- team_stats$mean_total_votes
  team_stats$coefficient_of_variation <- team_stats$sd_total_votes / team_stats$mean_total_votes

  # Sort by dominance probability
  team_stats <- team_stats %>%
    dplyr::arrange(desc(dominance_probability), desc(mean_total_votes))

  return(team_stats)
}

#' Calculate Winning Margin Probabilities
#'
#' Calculates the probability distribution of winning margins (difference between 1st and 2nd place)
#'
#' @param simulation_results List of season simulation results
#' @param margin_ranges Vector of margin ranges to analyze (default: c(1,2,3,5,10))
#' @param model_name Character name for the model (optional)
#'
#' @return DataFrame with margin statistics and probabilities
#' @export
calculate_winning_margin_probabilities <- function(simulation_results, margin_ranges = c(1, 2, 3, 5, 10), model_name = "Unknown") {
  n_sims <- length(simulation_results)
  if (n_sims == 0) {
    stop("No simulation results provided")
  }

  # Validate margin_ranges
  margin_ranges <- unique(sort(margin_ranges))
  margin_ranges <- margin_ranges[margin_ranges > 0]

  if (length(margin_ranges) == 0) {
    stop("No valid margin ranges provided")
  }

  # Get all unique players
  all_players <- simulation_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Initialize results matrix
  results_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(results_matrix) <- all_players$player_key

  # Fill results matrix with vote totals
  for (i in 1:n_sims) {
    sim_result <- simulation_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(results_matrix)) {
        results_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Calculate margins for each simulation
  margins <- numeric(n_sims)
  winner_votes <- numeric(n_sims)
  runner_up_votes <- numeric(n_sims)

  for (i in 1:n_sims) {
    season_votes <- results_matrix[, i]
    sorted_votes <- sort(season_votes, decreasing = TRUE)

    winner_votes[i] <- sorted_votes[1]
    runner_up_votes[i] <- ifelse(length(sorted_votes) > 1, sorted_votes[2], 0)
    margins[i] <- winner_votes[i] - runner_up_votes[i]
  }

  # Count margins by ranges
  margin_counts <- list()
  margin_counts[["exact_0"]] <- sum(margins == 0)  # Ties

  for (range_val in margin_ranges) {
    if (range_val == 1) {
      # Exactly 1 vote margin
      margin_counts[["exactly_1"]] <- sum(margins == 1)
    } else {
      # Range from previous value + 1 to current value
      prev_val <- margin_ranges[which(margin_ranges == range_val) - 1]
      if (length(prev_val) == 0) prev_val <- 1
      lower <- ifelse(range_val == 1, 1, prev_val + 1)
      upper <- range_val
      margin_counts[[paste0(lower, "_to_", upper)]] <- sum(margins >= lower & margins <= upper)
    }
  }

  # Count margins above highest range
  max_range <- max(margin_ranges)
  margin_counts[[paste0("over_", max_range)]] <- sum(margins > max_range)

  # Create results data frame
  margin_categories <- names(margin_counts)
  result_df <- data.frame(
    margin_category = margin_categories,
    model = model_name,
    count = unlist(margin_counts),
    probability = unlist(margin_counts) / n_sims * 100,
    stringsAsFactors = FALSE
  )

  # Add summary statistics
  summary_stats <- data.frame(
    statistic = c("mean_margin", "median_margin", "sd_margin", "min_margin", "max_margin",
                  "mean_winner_votes", "mean_runner_up_votes", "ties_count"),
    value = c(mean(margins), median(margins), sd(margins), min(margins), max(margins),
              mean(winner_votes), mean(runner_up_votes), sum(margins == 0)),
    stringsAsFactors = FALSE
  )

  # Return list with both margin probabilities and summary statistics
  return(list(
    margin_probabilities = result_df,
    summary_statistics = summary_stats,
    raw_margins = margins,
    model = model_name
  ))
}


#' Compare Multiple Model Simulations
#'
#' Compares win probabilities and statistics across different model simulations
#'
#' @param analysis_results List of analysis results from different models
#' @param top_n Integer number of top players to focus comparison on
#'
#' @return DataFrame with comparative statistics
#' @export
compare_model_simulations <- function(analysis_results, top_n = 20) {

  if (length(analysis_results) < 2) {
    stop("Need at least 2 model results for comparison")
  }

  model_names <- names(analysis_results)

  # Get top players from each model
  all_top_players <- lapply(analysis_results, function(res) {
    res %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(player_name, team, win_probability, mean_votes, expected_position)
  })

  # Find players that appear in top N for any model
  all_player_keys <- unique(unlist(lapply(all_top_players, function(df) {
    paste(df$player_name, df$team, sep = "_")
  })))

  # Create comparison matrix
  comparison_data <- data.frame(
    player_key = all_player_keys,
    stringsAsFactors = FALSE
  )

  # Extract player names and teams
  first_model <- analysis_results[[1]]
  comparison_data <- comparison_data %>%
    dplyr::mutate(
      player_name = stringr::str_extract(player_key, "^[^_]+"),
      team = stringr::str_extract(player_key, "[^_]+$")
    )

  # Add statistics from each model
  for (model_name in model_names) {
    model_results <- analysis_results[[model_name]]
    model_results$player_key <- paste(model_results$player_name, model_results$team, sep = "_")

    comparison_data <- comparison_data %>%
      dplyr::left_join(
        model_results %>%
          dplyr::select(player_key, win_probability, mean_votes, expected_position) %>%
          dplyr::rename_with(~paste0(.x, "_", model_name), -player_key),
        by = "player_key"
      )
  }

  # Calculate agreement metrics
  if (length(model_names) == 2) {
    model1 <- model_names[1]
    model2 <- model_names[2]

    win_prob_col1 <- paste0("win_probability_", model1)
    win_prob_col2 <- paste0("win_probability_", model2)

    if (win_prob_col1 %in% names(comparison_data) && win_prob_col2 %in% names(comparison_data)) {
      comparison_data$win_prob_correlation <- cor(
        comparison_data[[win_prob_col1]],
        comparison_data[[win_prob_col2]],
        use = "complete.obs"
      )

      comparison_data$win_prob_difference <- abs(
        comparison_data[[win_prob_col1]] - comparison_data[[win_prob_col2]]
      )
    }
  }

  # Sort by highest average win probability across models
  win_prob_cols <- grep("win_probability_", names(comparison_data), value = TRUE)
  if (length(win_prob_cols) > 0) {
    comparison_data$avg_win_probability <- rowMeans(
      comparison_data[, win_prob_cols], na.rm = TRUE
    )

    comparison_data <- comparison_data %>%
      dplyr::arrange(desc(avg_win_probability))
  }

  return(comparison_data)
}

#' Compare Multiple Model Simulations with Enhanced Statistics
#'
#' Enhanced version that includes team best probabilities in the comparison
#'
#' @param analysis_results List of analysis results from different models
#' @param enhanced_analysis_results List of enhanced analysis results with team stats
#' @param top_n Integer number of top players to focus comparison on
#'
#' @return DataFrame with comparative statistics including team best probabilities
#' @export
compare_model_simulations_enhanced <- function(analysis_results, enhanced_analysis_results, top_n = 20) {

  if (length(analysis_results) < 2) {
    stop("Need at least 2 model results for comparison")
  }

  model_names <- names(analysis_results)

  # Get top players from each model
  all_top_players <- lapply(analysis_results, function(res) {
    res %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(player_name, team, win_probability, mean_votes, expected_position)
  })

  # Find players that appear in top N for any model
  all_player_keys <- unique(unlist(lapply(all_top_players, function(df) {
    paste(df$player_name, df$team, sep = "_")
  })))

  # Create comparison matrix
  comparison_data <- data.frame(
    player_key = all_player_keys,
    stringsAsFactors = FALSE
  )

  # Extract player names and teams
  comparison_data <- comparison_data %>%
    dplyr::mutate(
      player_name = sapply(strsplit(player_key, "_"), function(x) x[1]),
      team = sapply(strsplit(player_key, "_"), function(x) x[2])
    )

  # Add statistics from each model (including team best probability)
  for (model_name in model_names) {
    model_results <- analysis_results[[model_name]]
    model_results$player_key <- paste(model_results$player_name, model_results$team, sep = "_")

    # Get team best probabilities for this model
    team_best_data <- enhanced_analysis_results[[model_name]]$team_best_analysis %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_")) %>%
      dplyr::select(player_key, team_best_probability)

    # Join main results with team best probabilities
    model_results_enhanced <- model_results %>%
      dplyr::left_join(team_best_data, by = "player_key") %>%
      dplyr::mutate(team_best_probability = ifelse(is.na(team_best_probability), 0, team_best_probability))

    comparison_data <- comparison_data %>%
      dplyr::left_join(
        model_results_enhanced %>%
          dplyr::select(player_key, win_probability, mean_votes, expected_position, team_best_probability) %>%
          dplyr::rename_with(~paste0(.x, "_", model_name), -player_key),
        by = "player_key"
      )
  }

  # Calculate agreement metrics
  if (length(model_names) == 2) {
    model1 <- model_names[1]
    model2 <- model_names[2]

    win_prob_col1 <- paste0("win_probability_", model1)
    win_prob_col2 <- paste0("win_probability_", model2)

    if (win_prob_col1 %in% names(comparison_data) && win_prob_col2 %in% names(comparison_data)) {
      comparison_data$win_prob_correlation <- cor(
        comparison_data[[win_prob_col1]],
        comparison_data[[win_prob_col2]],
        use = "complete.obs"
      )

      comparison_data$win_prob_difference <- abs(
        comparison_data[[win_prob_col1]] - comparison_data[[win_prob_col2]]
      )
    }
  }

  # Calculate average statistics across models
  win_prob_cols <- grep("win_probability_", names(comparison_data), value = TRUE)
  votes_cols <- grep("mean_votes_", names(comparison_data), value = TRUE)
  team_best_cols <- grep("team_best_probability_", names(comparison_data), value = TRUE)

  if (length(win_prob_cols) > 0) {
    comparison_data$avg_win_probability <- rowMeans(
      comparison_data[, win_prob_cols], na.rm = TRUE
    )
  }

  if (length(votes_cols) > 0) {
    comparison_data$avg_expected_votes <- rowMeans(
      comparison_data[, votes_cols], na.rm = TRUE
    )
  }

  if (length(team_best_cols) > 0) {
    comparison_data$team_best_probability <- rowMeans(
      comparison_data[, team_best_cols], na.rm = TRUE
    )
  }

  # Add model count
  comparison_data$models_count <- length(model_names)

  # Sort by highest average win probability
  comparison_data <- comparison_data %>%
    dplyr::arrange(desc(avg_win_probability))

  return(comparison_data)
}

#' Export Simulation Results for Dashboard
#'
#' Exports simulation results in a format suitable for interactive dashboards
#'
#' @param analysis_results List of analysis results from different models
#' @param output_file Character path for output JSON file
#' @param top_n Integer number of top players to include
#'
#' @export
export_dashboard_data <- function(analysis_results, output_file = "results/brownlow_dashboard_data.json",
                                 top_n = 50) {

  dashboard_data <- list(
    metadata = list(
      created_date = Sys.time(),
      models = names(analysis_results),
      top_n_players = top_n
    ),
    players = list()
  )

  # Get top players from each model
  for (model_name in names(analysis_results)) {
    model_results <- analysis_results[[model_name]] %>%
      dplyr::slice_head(n = top_n)

    dashboard_data$players[[model_name]] <- model_results %>%
      dplyr::select(
        player_name, team, win_probability, mean_votes, median_votes,
        top3_probability, top5_probability, expected_position,
        q25_votes, q75_votes
      ) %>%
      as.list()
  }

  # Write JSON file
  jsonlite::write_json(dashboard_data, output_file, pretty = TRUE, auto_unbox = TRUE)

  cat(sprintf("Dashboard data exported to: %s\n", output_file))
}

#' Load Simulation Data from Model Results
#'
#' Simple function to load probability matrices from final model results (scripts 03-05)
#' for Monte Carlo simulation. Eliminates complex conditional data loading logic.
#'
#' @param target_season Integer season to simulate (e.g., 2024)
#' @param verbose Logical whether to show loading messages
#'
#' @return List with cleaned probability matrices for XGBoost, GAM, and Ensemble models
#' @export
load_simulation_data <- function(target_season, verbose = TRUE) {

  if (verbose) {
    cat(sprintf("=== LOADING SIMULATION DATA FOR SEASON %d ===\n", target_season))
  }

  # Define required result files from scripts 03-05
  result_files <- list(
    xgboost = "results/xgboost_predictions.rds",
    gam = "results/gam_predictions.rds",
    ensemble = "results/ensemble_results.rds",
    modeling_data = "data/processed/modeling_data.rds"
  )

  # Check all required files exist
  missing_files <- result_files[!sapply(result_files, file.exists)]
  if (length(missing_files) > 0) {
    stop(sprintf("Missing required model result files:\n%s\n\nPlease run scripts 03, 04, and 05 first.",
                 paste("-", unlist(missing_files), collapse = "\n")))
  }

  # Load model results
  if (verbose) cat("Loading model prediction results...\n")
  xgboost_results <- load_standardized_results("xgboost")
  gam_results <- load_standardized_results("gam")
  ensemble_results <- load_standardized_results("ensemble")
  modeling_data <- readRDS(result_files$modeling_data)

  # Get data for target season
  if (verbose) cat(sprintf("Extracting season %d data...\n", target_season))

  # Check which dataset contains the target season
  season_data <- NULL
  data_source <- ""

  if (target_season %in% modeling_data$test$season) {
    season_data <- modeling_data$test %>% dplyr::filter(season == target_season)
    data_source <- "test"
  } else if (target_season %in% modeling_data$val$season) {
    season_data <- modeling_data$val %>% dplyr::filter(season == target_season)
    data_source <- "validation"
  } else if (target_season %in% modeling_data$train$season) {
    season_data <- modeling_data$train %>% dplyr::filter(season == target_season)
    data_source <- "training"
  } else {
    # Check for predictions generated by script 06 (for future seasons)
    predictions_file <- sprintf("predictions/%d/detailed_predictions.rds", target_season)
    if (file.exists(predictions_file)) {
      if (verbose) cat(sprintf("Loading predictions from script 06 for season %d...\n", target_season))
      season_predictions <- readRDS(predictions_file)

      # Convert season predictions to season_data format expected by simulation
      season_data <- season_predictions %>%
        dplyr::select(date, season, match_round, team, player_name, any_of("match_id")) %>%
        dplyr::mutate(
          # Add any missing columns that might be needed
          brownlow_votes = NA_real_  # Future seasons don't have actual votes yet
        )
      data_source <- "predictions"

      if (verbose) cat(sprintf("✓ Loaded %d observations from predictions\n", nrow(season_data)))
    } else {
      available_seasons <- unique(c(modeling_data$train$season, modeling_data$val$season, modeling_data$test$season))
      stop(sprintf("Season %d not found in modeling data or predictions.\nAvailable seasons: %s\nFor future seasons, run script 06 first to generate predictions.",
                   target_season, paste(available_seasons, collapse = ", ")))
    }
  }

  if (verbose) {
    cat(sprintf("SUCCESS Found %d observations from %s dataset\n", nrow(season_data), data_source))
  }

  # Extract probability matrices from model results
  sim_data <- list()

  if (data_source == "predictions") {
    # For future seasons, load probabilities from script 06 predictions
    if (verbose) cat("Loading probability matrices from predictions...\n")

    # The season_predictions should already be loaded above
    # We need to convert it to the expected probability matrix format
    # This requires loading the actual probability matrices from the prediction process

    # Check if probability matrices are saved alongside predictions
    prob_file <- sprintf("predictions/%d/prediction_probabilities.rds", target_season)
    if (file.exists(prob_file)) {
      if (verbose) cat("Loading saved probability matrices...\n")
      prob_matrices <- readRDS(prob_file)

      # XGBoost probabilities
      if (!is.null(prob_matrices$xgboost_probs)) {
        xgb_probs <- prob_matrices$xgboost_probs
        colnames(xgb_probs) <- paste0("xgb_prob_", 0:3)

        sim_data$xgb <- cbind(
          season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                       any_of(c("match_home_team", "match_away_team", "match_id"))),
          as.data.frame(xgb_probs)
        ) %>%
          dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
            paste(date, team, sep = "_")
          } else match_id)

        if (verbose) cat(sprintf("SUCCESS XGBoost: %dx%d probability matrix\n", nrow(xgb_probs), ncol(xgb_probs)))
      } else {
        if (verbose) cat("WARNING XGBoost probabilities not found in predictions\n")
        sim_data$xgb <- NULL
      }
    } else {
      stop(sprintf("Probability matrices not found for season %d. Please run script 06 with probability saving enabled.", target_season))
    }

  } else {
    # For historical seasons, load from standardized model results
    # XGBoost probabilities
    prob_key <- paste0(data_source, "_pred_probs")
    if (!is.null(xgboost_results$probabilities[[prob_key]])) {
      xgb_probs <- xgboost_results$probabilities[[prob_key]]

      # Filter to target season rows
      season_indices <- which(
        if (data_source == "test") modeling_data$test$season == target_season
        else if (data_source == "val") modeling_data$val$season == target_season
        else modeling_data$train$season == target_season
      )

      xgb_probs <- xgb_probs[season_indices, , drop = FALSE]
      colnames(xgb_probs) <- paste0("xgb_prob_", 0:3)

      sim_data$xgb <- cbind(
        season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                     any_of(c("match_home_team", "match_away_team", "match_id"))),
        as.data.frame(xgb_probs)
      ) %>%
        # Create match_id if not already present
        dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
          if (all(c("match_home_team", "match_away_team") %in% names(.))) {
            paste(date, match_home_team, match_away_team, sep = "_")
          } else {
            paste(date, team, sep = "_")
          }
        } else match_id)

      if (verbose) cat(sprintf("SUCCESS XGBoost: %dx%d probability matrix\n", nrow(xgb_probs), ncol(xgb_probs)))
    } else {
      if (verbose) cat("WARNING XGBoost probabilities not found\n")
      sim_data$xgb <- NULL
    }
  }

  # GAM probabilities
  if (data_source == "predictions") {
    # For future seasons, load from saved probability matrices
    if (!is.null(prob_matrices$gam_probs)) {
      gam_probs <- prob_matrices$gam_probs
      colnames(gam_probs) <- paste0("gam_prob_", 0:3)

      sim_data$gam <- cbind(
        season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                     any_of(c("match_home_team", "match_away_team", "match_id"))),
        as.data.frame(gam_probs)
      ) %>%
        dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
          paste(date, team, sep = "_")
        } else match_id)

      if (verbose) cat(sprintf("SUCCESS GAM: %dx%d probability matrix\n", nrow(gam_probs), ncol(gam_probs)))
    } else {
      if (verbose) cat("WARNING GAM probabilities not found in predictions\n")
      sim_data$gam <- NULL
    }

  } else {
    # For historical seasons, load from standardized GAM results
    prob_key <- paste0(data_source, "_pred_probs")
    if (!is.null(gam_results$probabilities[[prob_key]])) {
      gam_probs <- gam_results$probabilities[[prob_key]]

      # Filter to target season rows
      season_indices <- which(
        if (data_source == "test") modeling_data$test$season == target_season
        else if (data_source == "val") modeling_data$val$season == target_season
        else modeling_data$train$season == target_season
      )

      gam_probs <- gam_probs[season_indices, , drop = FALSE]
      colnames(gam_probs) <- paste0("gam_prob_", 0:3)

      sim_data$gam <- cbind(
        season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                     any_of(c("match_home_team", "match_away_team", "match_id"))),
        as.data.frame(gam_probs)
      ) %>%
        # Create match_id if not already present
        dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
          if (all(c("match_home_team", "match_away_team") %in% names(.))) {
            paste(date, match_home_team, match_away_team, sep = "_")
          } else {
            paste(date, team, sep = "_")
          }
        } else match_id)

      if (verbose) cat(sprintf("SUCCESS GAM: %dx%d probability matrix\n", nrow(gam_probs), ncol(gam_probs)))
    } else {
      if (verbose) cat("WARNING GAM probabilities not found\n")
      sim_data$gam <- NULL
    }
  }

  # Ensemble probabilities (create from XGBoost + GAM if both available)
  if (!is.null(sim_data$xgb) && !is.null(sim_data$gam)) {
    # Get ensemble weights
    if (data_source == "predictions") {
      # For future seasons, try to use saved ensemble or fallback to equal weights
      if (!is.null(prob_matrices$ensemble_probs)) {
        # Use directly saved ensemble probabilities
        ensemble_probs <- prob_matrices$ensemble_probs
        colnames(ensemble_probs) <- paste0("ensemble_prob_", 0:3)

        sim_data$ensemble <- cbind(
          season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                       any_of(c("match_home_team", "match_away_team", "match_id"))),
          as.data.frame(ensemble_probs)
        ) %>%
          dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
            paste(date, team, sep = "_")
          } else match_id)

        if (verbose) cat(sprintf("SUCCESS Ensemble: %dx%d probability matrix (saved)\n", nrow(ensemble_probs), ncol(ensemble_probs)))
      } else {
        # Create ensemble from individual models with default weights
        ensemble_weights <- c(0.5, 0.5)  # Default equal weights
      }
    } else {
      # For historical seasons, get weights from standardized ensemble results
      ensemble_weights <- ensemble_results$hyperparameters$ensemble_weights
      if (is.null(ensemble_weights)) ensemble_weights <- c(0.5, 0.5)  # Default equal weights
    }

    # Create ensemble if not already loaded from saved probabilities
    if (is.null(sim_data$ensemble)) {

      # Create ensemble probabilities
      ensemble_probs <- matrix(0, nrow = nrow(sim_data$xgb), ncol = 4)
      for (i in 0:3) {
        xgb_col <- paste0("xgb_prob_", i)
        gam_col <- paste0("gam_prob_", i)
        ensemble_probs[, i+1] <- (ensemble_weights[1] * sim_data$xgb[[xgb_col]] +
                                 ensemble_weights[2] * sim_data$gam[[gam_col]])
      }

      colnames(ensemble_probs) <- paste0("ensemble_prob_", 0:3)

      sim_data$ensemble <- cbind(
        season_data %>% dplyr::select(date, season, match_round, team, player_name, brownlow_votes,
                                     any_of(c("match_home_team", "match_away_team", "match_id"))),
        as.data.frame(ensemble_probs)
      ) %>%
        # Create match_id if not already present
        dplyr::mutate(match_id = if (!"match_id" %in% names(.)) {
          if (all(c("match_home_team", "match_away_team") %in% names(.))) {
            paste(date, match_home_team, match_away_team, sep = "_")
          } else {
            paste(date, team, sep = "_")
          }
        } else match_id)

      if (verbose) cat(sprintf("SUCCESS Ensemble: %dx%d probability matrix (computed)\n", nrow(ensemble_probs), ncol(ensemble_probs)))
    }
  } else {
    if (verbose) cat("WARNING Ensemble probabilities not available (need both XGBoost and GAM)\n")
    sim_data$ensemble <- NULL
  }

  # Validation checks
  available_models <- names(sim_data)[!sapply(sim_data, is.null)]
  if (length(available_models) == 0) {
    stop("No probability matrices available for simulation")
  }

  if (verbose) {
    cat(sprintf("SUCCESS Simulation data ready: %s\n", paste(available_models, collapse = ", ")))
    cat(sprintf("SUCCESS %d unique matches, %d total observations\n",
                length(unique(sim_data[[available_models[1]]]$match_id)),
                nrow(sim_data[[available_models[1]]])))
  }

  return(sim_data)
}

#' Analyze and Save Simulation Results
#'
#' Clean function to analyze simulation results and save all outputs
#' Replaces complex result processing logic in main script
#'
#' @param simulation_results List of simulation results by model (e.g., list(xgboost=..., gam=..., ensemble=...))
#' @param target_season Integer season that was simulated
#' @param verbose Logical whether to show progress messages
#'
#' @export
analyze_and_save_results <- function(simulation_results, target_season, verbose = TRUE) {

  if (verbose) {
    cat(sprintf("\n=== ANALYZING SIMULATION RESULTS FOR SEASON %d ===\n", target_season))
  }

  # Create results directories
  dir.create("results", showWarnings = FALSE, recursive = TRUE)
  dir.create("plots/simulation", showWarnings = FALSE, recursive = TRUE)

  available_models <- names(simulation_results)[!sapply(simulation_results, is.null)]
  if (length(available_models) == 0) {
    stop("No simulation results provided")
  }

  if (verbose) {
    cat(sprintf("Analyzing %d model(s): %s\n", length(available_models), paste(available_models, collapse = ", ")))
  }

  # Analyze each model's results
  analysis_results <- list()
  enhanced_analysis_results <- list()

  for (model_name in available_models) {
    if (verbose) cat(sprintf("Analyzing %s simulation results...\n", model_name))

    # Original analysis
    analysis_results[[model_name]] <- calculate_win_probabilities(
      simulation_results[[model_name]],
      model_name
    )

    # Enhanced analysis with new functions
    if (verbose) cat(sprintf("  - Calculating enhanced statistics for %s...\n", model_name))

    enhanced_analysis_results[[model_name]] <- list(
      # Flexible top N probabilities
      top_n_analysis = calculate_top_n_probabilities(
        simulation_results[[model_name]],
        n_values = c(1, 3, 5, 10, 15, 20),
        model_name = model_name
      ),

      # Team best player probabilities
      team_best_analysis = calculate_team_best_probabilities(
        simulation_results[[model_name]],
        model_name = model_name
      ),

      # Vote threshold probabilities
      vote_threshold_analysis = calculate_vote_threshold_probabilities(
        simulation_results[[model_name]],
        thresholds = c(10, 15, 20, 25, 30, 35, 40),
        model_name = model_name
      ),

      # Team dominance probabilities
      team_dominance_analysis = calculate_team_dominance_probabilities(
        simulation_results[[model_name]],
        model_name = model_name
      ),

      # Winning margin probabilities
      margin_analysis = calculate_winning_margin_probabilities(
        simulation_results[[model_name]],
        margin_ranges = c(1, 2, 3, 4, 6, 10),
        model_name = model_name
      )
    )
  }

  if (verbose) cat("SUCCESS Analysis complete for all models\n")

  # Create visualizations
  if (verbose) cat("Creating visualizations...\n")

  for (model_name in available_models) {
    model_results <- analysis_results[[model_name]]

    # Win probabilities plot
    plot_win_probabilities(model_results, top_n = 20, model_name = model_name, save_plot = TRUE)

    # Expected votes distribution
    plot_expected_votes_distribution(model_results, top_n = 25, model_name = model_name, save_plot = TRUE)

    # Position distribution
    plot_position_distribution(model_results, top_n = 20, model_name = model_name, save_plot = TRUE)

    # Top finish probabilities
    plot_top_finish_probabilities(model_results, top_n = 20, model_name = model_name, save_plot = TRUE)

    # Vote distributions for top 6 candidates
    top_players <- model_results %>% dplyr::slice_head(n = 6)
    if (nrow(top_players) > 0) {
      plot_vote_distributions(
        simulation_results[[model_name]],
        top_players$player_name,
        top_players$team,
        model_name = model_name,
        save_plot = TRUE
      )
    }
  }

  if (verbose) cat("SUCCESS Visualizations created\n")

  # Combine results across models with team best probabilities
  if (length(available_models) > 1) {
    if (verbose) cat("Creating model comparison analysis...\n")

    combined_results <- compare_model_simulations_enhanced(
      analysis_results,
      enhanced_analysis_results,
      top_n = 20
    )

    # Create comparison visualizations
    create_model_comparison_plots(analysis_results, save_plot = TRUE)
  } else {
    # Single model case - add team best probability
    if (verbose) cat("Creating single model results with team best probabilities...\n")

    primary_model <- available_models[1]
    team_best_data <- enhanced_analysis_results[[primary_model]]$team_best_analysis

    combined_results <- analysis_results[[primary_model]] %>%
      dplyr::left_join(
        team_best_data %>% dplyr::select(player_name, team, team_best_probability),
        by = c("player_name", "team")
      ) %>%
      dplyr::mutate(
        avg_win_probability = win_probability,
        avg_expected_votes = mean_votes,
        models_count = 1,
        team_best_probability = ifelse(is.na(team_best_probability), 0, team_best_probability)
      )
  }

  # Save comprehensive results
  if (verbose) cat("Saving results...\n")

  simulation_summary <- list(
    simulation_config = list(
      target_season = target_season,
      n_simulations = length(simulation_results[[available_models[1]]]),
      models_used = available_models,
      creation_date = Sys.time()
    ),
    raw_simulations = simulation_results,
    analysis_results = analysis_results,
    enhanced_analysis_results = enhanced_analysis_results,
    combined_results = combined_results
  )

  saveRDS(simulation_summary, sprintf("results/brownlow_simulation_results_%d.rds", target_season))

  # Save CSV results for easy viewing
  readr::write_csv(combined_results, sprintf("results/brownlow_win_probabilities_%d.csv", target_season))

  # Save enhanced analysis results as separate CSV files
  if (verbose) cat("Saving enhanced analysis results...\n")
  for (model_name in available_models) {
    model_enhanced <- enhanced_analysis_results[[model_name]]

    # Top N probabilities
    readr::write_csv(model_enhanced$top_n_analysis,
                     sprintf("results/top_n_probabilities_%s_%d.csv", model_name, target_season))

    # Team best players
    readr::write_csv(model_enhanced$team_best_analysis,
                     sprintf("results/team_best_players_%s_%d.csv", model_name, target_season))

    # Vote thresholds
    readr::write_csv(model_enhanced$vote_threshold_analysis,
                     sprintf("results/vote_thresholds_%s_%d.csv", model_name, target_season))

    # Team dominance
    readr::write_csv(model_enhanced$team_dominance_analysis,
                     sprintf("results/team_dominance_%s_%d.csv", model_name, target_season))

    # Winning margins
    readr::write_csv(model_enhanced$margin_analysis$margin_probabilities,
                     sprintf("results/winning_margins_%s_%d.csv", model_name, target_season))

    readr::write_csv(model_enhanced$margin_analysis$summary_statistics,
                     sprintf("results/margin_summary_%s_%d.csv", model_name, target_season))
  }

  # Create summary report for top candidates
  top_candidates <- combined_results %>%
    dplyr::arrange(desc(avg_win_probability)) %>%
    dplyr::slice_head(n = 20)

  readr::write_csv(top_candidates, sprintf("results/brownlow_top_candidates_summary_%d.csv", target_season))

  # Export dashboard data for interactive use
  if (length(analysis_results) > 0) {
    export_dashboard_data(analysis_results, sprintf("results/brownlow_dashboard_data_%d.json", target_season), top_n = 50)
  }

  # Print summary
  if (verbose) {
    cat(sprintf("\n=== SIMULATION SUMMARY - SEASON %d ===\n", target_season))
    cat(sprintf("Models analyzed: %s\n", paste(available_models, collapse = ", ")))
    cat(sprintf("Total simulations: %d per model\n", length(simulation_results[[available_models[1]]])))
    cat(sprintf("Top candidate: %s (%s) - %.1f%% win probability",
                top_candidates$player_name[1], top_candidates$team[1], top_candidates$avg_win_probability[1]))

    if ("team_best_probability" %in% names(top_candidates)) {
      cat(sprintf(", %.1f%% team best probability\n", top_candidates$team_best_probability[1]))
    } else {
      cat("\n")
    }

    cat("\nFiles created:\n")
    cat(sprintf("- results/brownlow_simulation_results_%d.rds - Complete simulation data\n", target_season))
    cat(sprintf("- results/brownlow_win_probabilities_%d.csv - All player probabilities with team best probability\n", target_season))
    cat(sprintf("- results/brownlow_top_candidates_summary_%d.csv - Top 20 candidates with team rankings\n", target_season))
    cat(sprintf("- results/brownlow_dashboard_data_%d.json - Interactive dashboard data\n", target_season))
    cat("- plots/simulation/ - Model visualization charts\n")
    cat("\nEnhanced analysis files (per model):\n")
    for (model in available_models) {
      cat(sprintf("- results/top_n_probabilities_%s_%d.csv - Flexible top N finish probabilities\n", model, target_season))
      cat(sprintf("- results/team_best_players_%s_%d.csv - Team's best player probabilities\n", model, target_season))
      cat(sprintf("- results/vote_thresholds_%s_%d.csv - Vote threshold exceed probabilities\n", model, target_season))
      cat(sprintf("- results/team_dominance_%s_%d.csv - Team vote dominance probabilities\n", model, target_season))
      cat(sprintf("- results/winning_margins_%s_%d.csv - Winning margin distributions\n", model, target_season))
      cat(sprintf("- results/margin_summary_%s_%d.csv - Margin summary statistics\n", model, target_season))
    }
  }

  return(list(
    analysis_results = analysis_results,
    enhanced_analysis_results = enhanced_analysis_results,
    combined_results = combined_results,
    top_candidates = top_candidates
  ))
}

#' Create Model Comparison Plots
#'
#' Creates comparative visualization plots across multiple models
#'
#' @param analysis_results List of analysis results from different models
#' @param save_plot Logical whether to save plots to files
#' @param top_n Integer number of top players to include
#'
#' @export
create_model_comparison_plots <- function(analysis_results, save_plot = TRUE, top_n = 15) {

  if (length(analysis_results) < 2) {
    return(invisible(NULL))
  }

  model_names <- names(analysis_results)

  # Combine top players from all models
  combined_data <- dplyr::bind_rows(lapply(model_names, function(model) {
    analysis_results[[model]] %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::mutate(model = model)
  }))

  # Win probability comparison
  win_prob_plot <- combined_data %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(paste(player_name, team, sep = " - "), win_probability),
               y = win_probability, fill = model)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Brownlow Win Probability Comparison Across Models",
      x = "Player",
      y = "Win Probability (%)",
      fill = "Model"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")

  if (save_plot) {
    ggplot2::ggsave("plots/simulation/win_probability_comparison.png", win_prob_plot,
           width = 12, height = 8, dpi = 300, bg = "white")
    cat("Model comparison plot saved to: plots/simulation/win_probability_comparison.png\n")
  }

  # Expected votes comparison
  votes_plot <- combined_data %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(paste(player_name, team, sep = " - "), mean_votes),
               y = mean_votes, fill = model)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Expected Votes Comparison Across Models",
      x = "Player",
      y = "Expected Votes",
      fill = "Model"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")

  if (save_plot) {
    ggplot2::ggsave("plots/simulation/expected_votes_comparison.png", votes_plot,
           width = 12, height = 8, dpi = 300, bg = "white")
    cat("Model comparison plot saved to: plots/simulation/expected_votes_comparison.png\n")
  }

  return(list(win_prob_plot = win_prob_plot, votes_plot = votes_plot))
}

#' Create Round-by-Round Voting Table
#'
#' Creates a traditional Brownlow prediction table showing player rankings and
#' expected votes for each round, similar to newspaper predictions
#'
#' @param sim_data Data frame with probability columns for each player/match
#' @param prob_prefix Character prefix for probability columns (e.g., "ensemble_prob_")
#' @param team_filter Character team name to filter results (optional)
#' @param use_expected_values Logical whether to use expected values rather than sampling
#' @param min_votes_threshold Numeric minimum expected total votes to include player
#'
#' @return List with round_table (data frame) and summary statistics
#' @export
create_detailed_round_by_round_table <- function(sim_data, prob_prefix = "ensemble_prob_",
                                      team_filter = NULL, use_expected_values = TRUE,
                                      min_votes_threshold = 0.1) {

  # Check that required columns exist
  prob_cols <- paste0(prob_prefix, 0:3)
  missing_cols <- setdiff(prob_cols, names(sim_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing probability columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Filter by team if specified
  if (!is.null(team_filter)) {
    sim_data <- sim_data %>% dplyr::filter(team == team_filter)
    if (nrow(sim_data) == 0) {
      stop(sprintf("No data found for team: %s", team_filter))
    }
  }

  # Calculate expected votes per match for each player
  match_expectations <- sim_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      expected_votes = (0 * .data[[prob_cols[1]]] +
                       1 * .data[[prob_cols[2]]] +
                       2 * .data[[prob_cols[3]]] +
                       3 * .data[[prob_cols[4]]])
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_name, team, match_round, expected_votes)

  # Create round-by-round matrix
  round_matrix <- match_expectations %>%
    tidyr::pivot_wider(
      names_from = match_round,
      values_from = expected_votes,
      names_prefix = "R",
      values_fill = 0
    )

  # Calculate season totals and statistics
  round_cols <- grep("^R[0-9]+$", names(round_matrix), value = TRUE)

  player_totals <- round_matrix %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_votes = sum(dplyr::c_across(dplyr::all_of(round_cols)), na.rm = TRUE),
      matches_played = sum(!is.na(dplyr::c_across(dplyr::all_of(round_cols))) &
                          dplyr::c_across(dplyr::all_of(round_cols)) > 0),
      votes_per_match = ifelse(matches_played > 0, total_votes / matches_played, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_votes >= min_votes_threshold) %>%
    dplyr::arrange(desc(total_votes), desc(votes_per_match))

  # Calculate basic win probabilities based on expected votes
  # This is a simplified calculation for the table display
  total_votes_all <- sum(player_totals$total_votes)
  player_totals <- player_totals %>%
    dplyr::mutate(
      win_probability = pmax(0, (total_votes / max(total_votes)) * 100),
      rank = dplyr::row_number()
    )

  # Prepare final table structure
  final_table <- player_totals %>%
    dplyr::select(
      rank, player_name, team, matches_played, total_votes, win_probability,
      dplyr::all_of(round_cols)
    ) %>%
    dplyr::rename(
      Rank = rank,
      Player = player_name,
      Team = team,
      M = matches_played,
      Votes = total_votes,
      `Win%` = win_probability
    )

  # Summary statistics
  summary_stats <- list(
    total_players = nrow(final_table),
    total_votes_distributed = sum(final_table$Votes),
    team_filter = team_filter,
    min_threshold = min_votes_threshold,
    top_player = if (nrow(final_table) > 0) {
      list(
        name = final_table$Player[1],
        team = final_table$Team[1],
        votes = final_table$Votes[1],
        win_prob = final_table$`Win%`[1]
      )
    } else NULL,
    rounds_with_data = length(round_cols)
  )

  return(list(
    round_table = final_table,
    summary = summary_stats,
    prob_prefix = prob_prefix
  ))
}

#' Display Round-by-Round Table
#'
#' Formats and displays a round-by-round table in the console with traditional
#' Brownlow prediction table formatting
#'
#' @param round_table_result Result from create_round_by_round_table()
#' @param max_players Integer maximum number of players to display
#' @param max_rounds Integer maximum number of rounds to display
#' @param format_threshold Numeric threshold below which to show "•" instead of numbers
#'
#' @export
display_round_by_round_table <- function(round_table_result, max_players = 100,
                                       max_rounds = 24, format_threshold = 0.1) {

  table_data <- round_table_result$round_table
  summary <- round_table_result$summary

  if (nrow(table_data) == 0) {
    cat("No players found meeting the criteria.\n")
    return(invisible(NULL))
  }

  # Limit display
  display_data <- table_data %>% dplyr::slice_head(n = max_players)

  # Get round columns and limit them
  round_cols <- grep("^R[0-9]+$", names(display_data), value = TRUE)
  round_numbers <- as.numeric(stringr::str_extract(round_cols, "[0-9]+"))
  round_cols <- round_cols[order(round_numbers)]
  round_cols <- round_cols[1:min(length(round_cols), max_rounds)]

  # Format the display data
  display_formatted <- display_data %>%
    dplyr::select(Rank, Player, Team, M, Votes, `Win%`, dplyr::all_of(round_cols)) %>%
    dplyr::mutate(
      Votes = round(Votes, 1),
      `Win%` = sprintf("%.1f", `Win%`)
    )

  # Format round columns - replace small values with "•"
  for (col in round_cols) {
    display_formatted[[col]] <- ifelse(
      display_formatted[[col]] < format_threshold,
      "•",
      sprintf("%.1f", display_formatted[[col]])
    )
  }

  # Print header
  cat(sprintf("\n=== BROWNLOW MEDAL PREDICTION TABLE ===\n"))
  if (!is.null(summary$team_filter)) {
    cat(sprintf("Team Filter: %s\n", summary$team_filter))
  }
  cat(sprintf("Showing top %d players (of %d total)\n",
              min(nrow(display_formatted), max_players), summary$total_players))
  cat(sprintf("Total votes distributed: %.1f\n", summary$total_votes_distributed))

  # Print table
  cat(sprintf("\n"))
  print(display_formatted, row.names = FALSE)

  # Print summary
  if (!is.null(summary$top_player)) {
    cat(sprintf("\nTop candidate: %s (%s) - %.1f votes, %.1f%% win probability\n",
                summary$top_player$name, summary$top_player$team,
                summary$top_player$votes, summary$top_player$win_prob))
  }

  cat(sprintf("Note: '•' indicates expected votes < %.1f\n", format_threshold))

  return(invisible(display_formatted))
}

#' Save Round-by-Round Table
#'
#' Saves round-by-round table to CSV file with proper formatting
#'
#' @param round_table_result Result from create_round_by_round_table()
#' @param filename Character filename for output (optional, will auto-generate if NULL)
#' @param target_season Integer season year for filename
#' @param include_raw_values Logical whether to include unformatted values
#'
#' @export
save_round_by_round_table <- function(round_table_result, filename = NULL,
                                    target_season = 2025, include_raw_values = TRUE) {

  table_data <- round_table_result$round_table
  summary <- round_table_result$summary

  if (nrow(table_data) == 0) {
    cat("No data to save.\n")
    return(invisible(NULL))
  }

  # Generate filename if not provided
  if (is.null(filename)) {
    team_suffix <- if (!is.null(summary$team_filter)) {
      paste0("_", gsub(" ", "_", summary$team_filter))
    } else ""

    filename <- sprintf("results/round_by_round_table_%s_%d%s.csv",
                       round_table_result$prob_prefix, target_season, team_suffix)
  }

  # Ensure results directory exists
  dir.create("results", showWarnings = FALSE, recursive = TRUE)

  # Prepare data for saving
  save_data <- table_data

  if (!include_raw_values) {
    # Format for display (with "•" symbols)
    round_cols <- grep("^R[0-9]+$", names(save_data), value = TRUE)
    for (col in round_cols) {
      save_data[[col]] <- ifelse(save_data[[col]] < 0.1, "•",
                                sprintf("%.1f", save_data[[col]]))
    }
    save_data$Votes <- round(save_data$Votes, 1)
    save_data$`Win%` <- sprintf("%.1f", save_data$`Win%`)
  }

  # Save to CSV
  readr::write_csv(save_data, filename)

  # Create summary file
  summary_filename <- stringr::str_replace(filename, "\\.csv$", "_summary.txt")
  summary_text <- sprintf(
    "Round-by-Round Brownlow Table Summary

Generated: %s
Model: %s
Season: %d
Team Filter: %s
Players Included: %d
Total Votes: %.1f
Min Threshold: %.2f

Top 3 Candidates:
%s

Rounds with Data: %d",
    Sys.time(),
    round_table_result$prob_prefix,
    target_season,
    ifelse(is.null(summary$team_filter), "All Teams", summary$team_filter),
    summary$total_players,
    summary$total_votes_distributed,
    summary$min_threshold,
    paste(
      sprintf("%d. %s (%s) - %.1f votes, %.1f%% win prob",
              1:min(3, nrow(table_data)),
              table_data$Player[1:min(3, nrow(table_data))],
              table_data$Team[1:min(3, nrow(table_data))],
              table_data$Votes[1:min(3, nrow(table_data))],
              table_data$`Win%`[1:min(3, nrow(table_data))]
      ),
      collapse = "\n"
    ),
    summary$rounds_with_data
  )

  writeLines(summary_text, summary_filename)

  cat(sprintf("Round-by-round table saved to: %s\n", filename))
  cat(sprintf("Summary saved to: %s\n", summary_filename))

  return(invisible(list(
    table_file = filename,
    summary_file = summary_filename
  )))
}

#' Analyze Teammate Vote Correlations for Betting Opportunities
#'
#' Identifies negatively correlated teammate pairs for Brownlow betting strategies.
#' Leverages the mathematical constraint that when one player gets 3 votes,
#' their teammate typically doesn't, creating systematic betting opportunities.
#'
#' @param simulation_results List of simulation results by model
#' @param model_name Character name of model to analyze (default: "ensemble")
#' @param min_expected_votes Numeric minimum expected votes to include player (default: 3)
#' @param min_correlation_strength Numeric minimum correlation strength to report (default: 0.25)
#' @param confidence_level Numeric confidence level for statistical tests (default: 0.95)
#' @param market_projections Optional data frame with market vote projections
#' @param verbose Logical whether to show progress messages
#'
#' @return List with correlation analysis, betting opportunities, and visualizations
#' @export
analyze_teammate_vote_correlations <- function(simulation_results,
                                             model_name = "ensemble",
                                             min_expected_votes = 3,
                                             min_correlation_strength = 0.25,
                                             confidence_level = 0.95,
                                             market_projections = NULL,
                                             verbose = TRUE) {

  if (verbose) {
    cat("=== TEAMMATE VOTE CORRELATION ANALYSIS ===\n")
  }

  # Validate inputs
  if (!model_name %in% names(simulation_results)) {
    stop(sprintf("Model '%s' not found in simulation results. Available: %s",
                 model_name, paste(names(simulation_results), collapse = ", ")))
  }

  model_results <- simulation_results[[model_name]]
  n_sims <- length(model_results)

  if (n_sims == 0) {
    stop("No simulation results found for the specified model")
  }

  if (verbose) {
    cat(sprintf("Analyzing %d simulations for model: %s\n", n_sims, model_name))
  }

  # Extract player information and create vote matrix
  if (verbose) cat("Extracting vote matrices by team...\n")

  # Get all unique players and teams
  all_players <- model_results[[1]] %>%
    dplyr::select(player_name, team) %>%
    dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

  # Create vote matrix: rows = players, columns = simulations
  vote_matrix <- matrix(0, nrow = nrow(all_players), ncol = n_sims)
  rownames(vote_matrix) <- all_players$player_key

  # Fill vote matrix
  for (i in 1:n_sims) {
    sim_result <- model_results[[i]] %>%
      dplyr::mutate(player_key = paste(player_name, team, sep = "_"))

    for (j in 1:nrow(sim_result)) {
      player_key <- sim_result$player_key[j]
      if (player_key %in% rownames(vote_matrix)) {
        vote_matrix[player_key, i] <- sim_result$total_votes[j]
      }
    }
  }

  # Calculate expected votes and filter players
  expected_votes <- rowMeans(vote_matrix)
  qualified_players <- names(expected_votes)[expected_votes >= min_expected_votes]

  if (verbose) {
    cat(sprintf("Filtered to %d players with >= %.1f expected votes\n",
                length(qualified_players), min_expected_votes))
  }

  # Group players by team and calculate correlations
  if (verbose) cat("Calculating within-team correlations...\n")

  team_correlations <- list()
  correlation_pairs <- data.frame()

  # Get unique teams
  teams <- unique(all_players$team)

  for (team in teams) {
    # Get team players who meet the criteria
    team_players <- all_players %>%
      dplyr::filter(team == !!team) %>%
      dplyr::pull(player_key)

    team_qualified <- intersect(team_players, qualified_players)

    if (length(team_qualified) >= 2) {
      # Extract team vote matrix
      team_vote_matrix <- vote_matrix[team_qualified, , drop = FALSE]

      # Calculate correlation matrix
      if (nrow(team_vote_matrix) >= 2) {
        team_cor_matrix <- cor(t(team_vote_matrix))
        team_correlations[[team]] <- team_cor_matrix

        # Extract pairwise correlations
        for (i in 1:(nrow(team_cor_matrix) - 1)) {
          for (j in (i + 1):nrow(team_cor_matrix)) {
            player1_key <- rownames(team_cor_matrix)[i]
            player2_key <- rownames(team_cor_matrix)[j]
            correlation <- team_cor_matrix[i, j]

            # Extract player names from keys
            player1_name <- stringr::str_extract(player1_key, "^[^_]+")
            player2_name <- stringr::str_extract(player2_key, "^[^_]+")

            correlation_pairs <- rbind(correlation_pairs, data.frame(
              team = team,
              player1_name = player1_name,
              player2_name = player2_name,
              player1_key = player1_key,
              player2_key = player2_key,
              correlation = correlation,
              player1_expected_votes = expected_votes[player1_key],
              player2_expected_votes = expected_votes[player2_key],
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }

  if (nrow(correlation_pairs) == 0) {
    stop("No qualifying teammate pairs found for correlation analysis")
  }

  # Filter to meaningful correlations and add significance testing
  if (verbose) cat("Calculating statistical significance...\n")

  correlation_pairs <- correlation_pairs %>%
    dplyr::filter(abs(correlation) >= min_correlation_strength) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Calculate correlation confidence interval
      correlation_test = list({
        x <- vote_matrix[player1_key, ]
        y <- vote_matrix[player2_key, ]
        cor.test(x, y, conf.level = confidence_level)
      }),
      correlation_p_value = correlation_test$p.value,
      correlation_ci_lower = correlation_test$conf.int[1],
      correlation_ci_upper = correlation_test$conf.int[2],
      correlation_significant = correlation_p_value < (1 - confidence_level)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-correlation_test) %>%
    dplyr::arrange(correlation)  # Most negative first

  # Calculate betting scenario probabilities
  if (verbose) cat("Calculating betting scenario probabilities...\n")

  betting_scenarios <- correlation_pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Calculate scenario probabilities
      scenario_probs = list({
        p1_votes <- vote_matrix[player1_key, ]
        p2_votes <- vote_matrix[player2_key, ]
        p1_expected <- player1_expected_votes
        p2_expected <- player2_expected_votes

        # Calculate key betting scenarios using expected votes
        p1_under_p2_over <- mean(p1_votes < p1_expected & p2_votes > p2_expected)
        p1_over_p2_under <- mean(p1_votes > p1_expected & p2_votes < p2_expected)
        both_under <- mean(p1_votes < p1_expected & p2_votes < p2_expected)
        both_over <- mean(p1_votes > p1_expected & p2_votes > p2_expected)

        # Calculate mean-based betting scenarios (using expected votes as thresholds)
        p1_over_mean_p2_over_mean <- mean(p1_votes > p1_expected & p2_votes > p2_expected)
        p1_over_mean_p2_under_mean <- mean(p1_votes > p1_expected & p2_votes < p2_expected)
        p1_under_mean_p2_over_mean <- mean(p1_votes < p1_expected & p2_votes > p2_expected)
        p1_under_mean_p2_under_mean <- mean(p1_votes < p1_expected & p2_votes < p2_expected)

        list(
          p1_under_p2_over = p1_under_p2_over,
          p1_over_p2_under = p1_over_p2_under,
          both_under = both_under,
          both_over = both_over,
          p1_over_mean_p2_over_mean = p1_over_mean_p2_over_mean,
          p1_over_mean_p2_under_mean = p1_over_mean_p2_under_mean,
          p1_under_mean_p2_over_mean = p1_under_mean_p2_over_mean,
          p1_under_mean_p2_under_mean = p1_under_mean_p2_under_mean,
          total_scenarios = p1_under_p2_over + p1_over_p2_under + both_under + both_over
        )
      }),

      # Extract scenario probabilities
      prob_p1_under_p2_over = scenario_probs$p1_under_p2_over,
      prob_p1_over_p2_under = scenario_probs$p1_over_p2_under,
      prob_both_under = scenario_probs$both_under,
      prob_both_over = scenario_probs$both_over,

      # Extract mean-based scenario probabilities (using expected votes as thresholds)
      prob_over_over_mean = scenario_probs$p1_over_mean_p2_over_mean,
      prob_over_under_mean = scenario_probs$p1_over_mean_p2_under_mean,
      prob_under_over_mean = scenario_probs$p1_under_mean_p2_over_mean,
      prob_under_under_mean = scenario_probs$p1_under_mean_p2_under_mean,

      # Calculate betting opportunity metrics
      best_scenario_prob = pmax(prob_p1_under_p2_over, prob_p1_over_p2_under),
      scenario_edge = best_scenario_prob - 0.5,  # Edge over random chance

      # Identify which scenario is better
      best_scenario = ifelse(prob_p1_under_p2_over > prob_p1_over_p2_under,
                            sprintf("%s under, %s over", player1_name, player2_name),
                            sprintf("%s over, %s under", player1_name, player2_name))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-scenario_probs)

  # Add market comparison if provided
  if (!is.null(market_projections)) {
    if (verbose) cat("Comparing with market projections...\n")

    # Join with market projections
    betting_scenarios <- betting_scenarios %>%
      dplyr::left_join(
        market_projections %>%
          dplyr::select(player_name, market_projection) %>%
          dplyr::rename(player1_market = market_projection),
        by = c("player1_name" = "player_name")
      ) %>%
      dplyr::left_join(
        market_projections %>%
          dplyr::select(player_name, market_projection) %>%
          dplyr::rename(player2_market = market_projection),
        by = c("player2_name" = "player_name")
      ) %>%
      dplyr::mutate(
        player1_model_edge = player1_expected_votes - ifelse(is.na(player1_market), player1_expected_votes, player1_market),
        player2_model_edge = player2_expected_votes - ifelse(is.na(player2_market), player2_expected_votes, player2_market)
      )
  }

  # Rank opportunities
  top_opportunities <- betting_scenarios %>%
    dplyr::filter(correlation_significant) %>%
    dplyr::arrange(desc(best_scenario_prob), correlation) %>%
    dplyr::mutate(
      opportunity_rank = dplyr::row_number(),
      correlation_strength = case_when(
        abs(correlation) >= 0.7 ~ "Very Strong",
        abs(correlation) >= 0.5 ~ "Strong",
        abs(correlation) >= 0.3 ~ "Moderate",
        TRUE ~ "Weak"
      )
    )

  # Summary statistics
  summary_stats <- list(
    total_teams_analyzed = length(teams),
    total_correlation_pairs = nrow(correlation_pairs),
    significant_correlations = sum(correlation_pairs$correlation_significant),
    negative_correlations = sum(correlation_pairs$correlation < 0 & correlation_pairs$correlation_significant),
    positive_correlations = sum(correlation_pairs$correlation > 0 & correlation_pairs$correlation_significant),
    strongest_negative_correlation = min(correlation_pairs$correlation[correlation_pairs$correlation_significant]),
    strongest_positive_correlation = max(correlation_pairs$correlation[correlation_pairs$correlation_significant]),
    best_betting_opportunity = if(nrow(top_opportunities) > 0) top_opportunities[1, ] else NULL
  )

  # Create results summary
  if (verbose) {
    cat("\n=== CORRELATION ANALYSIS RESULTS ===\n")
    cat(sprintf("Teams analyzed: %d\n", summary_stats$total_teams_analyzed))
    cat(sprintf("Significant correlations found: %d/%d\n",
                summary_stats$significant_correlations, summary_stats$total_correlation_pairs))
    cat(sprintf("Negative correlations: %d\n", summary_stats$negative_correlations))
    cat(sprintf("Positive correlations: %d\n", summary_stats$positive_correlations))

    if (!is.null(summary_stats$best_betting_opportunity)) {
      best_opp <- summary_stats$best_betting_opportunity
      cat(sprintf("\nTop Betting Opportunity:\n"))
      cat(sprintf("  %s vs %s (%s)\n", best_opp$player1_name, best_opp$player2_name, best_opp$team))
      cat(sprintf("  Correlation: %.3f (95%% CI: %.3f to %.3f)\n",
                  best_opp$correlation, best_opp$correlation_ci_lower, best_opp$correlation_ci_upper))
      cat(sprintf("  Best scenario: %s (%.1f%% probability)\n",
                  best_opp$best_scenario, best_opp$best_scenario_prob * 100))
      cat(sprintf("  Expected votes: %.1f vs %.1f\n",
                  best_opp$player1_expected_votes, best_opp$player2_expected_votes))
    }
  }

  # Return comprehensive results
  return(list(
    correlation_pairs = correlation_pairs,
    betting_scenarios = betting_scenarios,
    top_opportunities = top_opportunities,
    team_correlations = team_correlations,
    summary_stats = summary_stats,
    analysis_config = list(
      model_name = model_name,
      min_expected_votes = min_expected_votes,
      min_correlation_strength = min_correlation_strength,
      confidence_level = confidence_level,
      n_simulations = n_sims
    )
  ))
}

#' Save Teammate Correlation Analysis Results
#'
#' Saves correlation analysis results to CSV files and creates visualizations
#'
#' @param correlation_analysis Result from analyze_teammate_vote_correlations()
#' @param target_season Integer season year for filename
#' @param output_dir Character output directory (default: "results")
#' @param create_plots Logical whether to create visualization plots
#'
#' @export
save_correlation_analysis <- function(correlation_analysis, target_season = 2025,
                                    output_dir = "results", create_plots = TRUE) {

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  if (create_plots) {
    dir.create("plots/correlations", showWarnings = FALSE, recursive = TRUE)
  }

  model_name <- correlation_analysis$analysis_config$model_name

  # Save main results
  if (nrow(correlation_analysis$top_opportunities) > 0) {
    readr::write_csv(
      correlation_analysis$top_opportunities,
      file.path(output_dir, sprintf("teammate_correlations_%s_%d.csv", model_name, target_season))
    )
    cat(sprintf("Top opportunities saved to: %s/teammate_correlations_%s_%d.csv\n",
                output_dir, model_name, target_season))
  }

  # Save all correlation pairs
  if (nrow(correlation_analysis$correlation_pairs) > 0) {
    readr::write_csv(
      correlation_analysis$correlation_pairs,
      file.path(output_dir, sprintf("all_correlations_%s_%d.csv", model_name, target_season))
    )
  }

  # Save betting scenarios
  if (nrow(correlation_analysis$betting_scenarios) > 0) {
    readr::write_csv(
      correlation_analysis$betting_scenarios,
      file.path(output_dir, sprintf("betting_scenarios_%s_%d.csv", model_name, target_season))
    )
  }

  # Create summary report
  summary_file <- file.path(output_dir, sprintf("correlation_summary_%s_%d.txt", model_name, target_season))
  summary_text <- sprintf(
    "Teammate Vote Correlation Analysis Summary

Generated: %s
Model: %s
Season: %d
Simulations: %d

Analysis Configuration:
- Minimum Expected Votes: %.1f
- Minimum Correlation Strength: %.2f
- Confidence Level: %.0f%%

Results Summary:
- Teams Analyzed: %d
- Total Correlation Pairs: %d
- Significant Correlations: %d
- Negative Correlations: %d
- Positive Correlations: %d
- Strongest Negative Correlation: %.3f
- Strongest Positive Correlation: %.3f

Top 3 Betting Opportunities:
%s",
    Sys.time(),
    model_name,
    target_season,
    correlation_analysis$analysis_config$n_simulations,
    correlation_analysis$analysis_config$min_expected_votes,
    correlation_analysis$analysis_config$min_correlation_strength,
    correlation_analysis$analysis_config$confidence_level * 100,
    correlation_analysis$summary_stats$total_teams_analyzed,
    correlation_analysis$summary_stats$total_correlation_pairs,
    correlation_analysis$summary_stats$significant_correlations,
    correlation_analysis$summary_stats$negative_correlations,
    correlation_analysis$summary_stats$positive_correlations,
    correlation_analysis$summary_stats$strongest_negative_correlation,
    correlation_analysis$summary_stats$strongest_positive_correlation,
    if (nrow(correlation_analysis$top_opportunities) > 0) {
      paste(
        sprintf("%d. %s vs %s (%s) - Correlation: %.3f, Best Scenario: %s (%.1f%%)",
                1:min(3, nrow(correlation_analysis$top_opportunities)),
                correlation_analysis$top_opportunities$player1_name[1:min(3, nrow(correlation_analysis$top_opportunities))],
                correlation_analysis$top_opportunities$player2_name[1:min(3, nrow(correlation_analysis$top_opportunities))],
                correlation_analysis$top_opportunities$team[1:min(3, nrow(correlation_analysis$top_opportunities))],
                correlation_analysis$top_opportunities$correlation[1:min(3, nrow(correlation_analysis$top_opportunities))],
                correlation_analysis$top_opportunities$best_scenario[1:min(3, nrow(correlation_analysis$top_opportunities))],
                correlation_analysis$top_opportunities$best_scenario_prob[1:min(3, nrow(correlation_analysis$top_opportunities))] * 100
        ),
        collapse = "\n"
      )
    } else "No significant opportunities found"
  )

  writeLines(summary_text, summary_file)
  cat(sprintf("Summary report saved to: %s\n", summary_file))

  # Create visualizations if requested
  if (create_plots && nrow(correlation_analysis$top_opportunities) > 0) {
    create_simulation_correlation_plots(correlation_analysis, target_season, model_name)
  }

  return(invisible(list(
    summary_file = summary_file,
    main_results = file.path(output_dir, sprintf("teammate_correlations_%s_%d.csv", model_name, target_season))
  )))
}

#' Create Correlation Analysis Plots
#'
#' Creates visualization plots for correlation analysis results
#'
#' @param correlation_analysis Result from analyze_teammate_vote_correlations()
#' @param target_season Integer season year
#' @param model_name Character model name
#'
#' @export
create_simulation_correlation_plots <- function(correlation_analysis, target_season, model_name) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 package required for plotting")
    return(invisible(NULL))
  }

  top_opps <- correlation_analysis$top_opportunities %>%
    dplyr::slice_head(n = 15)  # Top 15 for readability

  if (nrow(top_opps) == 0) return(invisible(NULL))

  # 1. Correlation strength plot
  corr_plot <- top_opps %>%
    dplyr::mutate(
      pair_label = sprintf("%s vs %s\n(%s)", player1_name, player2_name, team),
      pair_label = factor(pair_label, levels = rev(pair_label))  # Reverse for ggplot
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = pair_label, y = correlation, fill = correlation_strength)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = correlation_ci_lower, ymax = correlation_ci_upper),
                          width = 0.2, alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Teammate Vote Correlations - Top Betting Opportunities",
      subtitle = sprintf("Model: %s | Season: %d | Error bars show 95%% confidence intervals", model_name, target_season),
      x = "Player Pairs",
      y = "Correlation Coefficient",
      fill = "Correlation Strength"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
      axis.text.y = ggplot2::element_text(size = 9)
    ) +
    ggplot2::scale_fill_manual(values = c("Very Strong" = "#d73027", "Strong" = "#f46d43",
                                         "Moderate" = "#fdae61", "Weak" = "#fee08b"))

  ggplot2::ggsave("plots/correlations/teammate_correlations.png", corr_plot,
         width = 12, height = 8, dpi = 300, bg = "white")

  # 2. Betting scenario probabilities plot
  scenario_plot <- top_opps %>%
    dplyr::mutate(
      pair_label = sprintf("%s vs %s", player1_name, player2_name),
      pair_label = factor(pair_label, levels = rev(pair_label))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = pair_label, y = best_scenario_prob * 100)) +
    ggplot2::geom_col(ggplot2::aes(fill = team), alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Betting Scenario Probabilities",
      subtitle = sprintf("Probability of best under/over scenario | Red line = 50%% (random chance)"),
      x = "Player Pairs",
      y = "Best Scenario Probability (%)",
      fill = "Team"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11),
      axis.text.y = ggplot2::element_text(size = 9),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 6))

  ggplot2::ggsave("plots/correlations/betting_probabilities.png", scenario_plot,
         width = 12, height = 8, dpi = 300, bg = "white")

  # 3. Expected votes scatter plot with correlation
  scatter_data <- correlation_analysis$top_opportunities %>%
    dplyr::slice_head(n = 10)

  if (nrow(scatter_data) > 0) {
    scatter_plot <- scatter_data %>%
      ggplot2::ggplot(ggplot2::aes(x = player1_expected_votes, y = player2_expected_votes)) +
      ggplot2::geom_point(ggplot2::aes(color = correlation, size = abs(correlation)), alpha = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%s vs %s", player1_name, player2_name)),
                        hjust = 0, vjust = 0, size = 3, check_overlap = TRUE) +
      ggplot2::labs(
        title = "Expected Votes vs Correlation Strength",
        subtitle = "Top 10 teammate pairs by betting opportunity",
        x = "Player 1 Expected Votes",
        y = "Player 2 Expected Votes",
        color = "Correlation",
        size = "Correlation Strength"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11)
      ) +
      ggplot2::scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

    ggplot2::ggsave("plots/correlations/expected_votes_scatter.png", scatter_plot,
           width = 10, height = 8, dpi = 300, bg = "white")
  }

  cat("Correlation plots saved to: plots/correlations/\n")
  cat("- teammate_correlations.png - Correlation coefficients with confidence intervals\n")
  cat("- betting_probabilities.png - Betting scenario probabilities\n")
  cat("- expected_votes_scatter.png - Expected votes vs correlation strength\n")

  return(invisible(NULL))
}

#' Print Detailed Correlation Scenarios
#'
#' Prints detailed betting scenario probabilities for top correlations,
#' including over/over, over/under, under/over, and under/under scenarios
#'
#' @param correlation_analysis Result from analyze_teammate_vote_correlations()
#' @param top_n Integer number of top correlations to display (default: 5)
#' @param use_enhanced_metrics Logical whether to use enhanced mean-based metrics if available,
#'   otherwise falls back to expected vote scenarios (default: FALSE for backward compatibility)
#'
#' @export
print_correlation_scenarios <- function(correlation_analysis, top_n = 5, use_enhanced_metrics = FALSE) {

  if (nrow(correlation_analysis$top_opportunities) == 0) {
    cat("No correlation opportunities found.\n")
    return(invisible(NULL))
  }

  top_corrs <- correlation_analysis$top_opportunities %>%
    dplyr::arrange(desc(abs(correlation))) %>%
    dplyr::slice_head(n = top_n)

  cat("=== DETAILED BETTING SCENARIOS ===\n")
  cat(sprintf("Top %d teammate correlations with scenario probabilities:\n\n", min(top_n, nrow(top_corrs))))

  for (i in 1:nrow(top_corrs)) {
    corr <- top_corrs[i, ]

    cat(sprintf("%d. %s vs %s (%s)\n",
                i, corr$player1_name, corr$player2_name, corr$team))
    cat(sprintf("   Correlation: %.3f (%.3f to %.3f)\n",
                corr$correlation, corr$correlation_ci_lower, corr$correlation_ci_upper))

    cat(sprintf("   Expected votes: %.1f vs %.1f\n",
                corr$player1_expected_votes, corr$player2_expected_votes))

    # Check if enhanced metrics are available and requested
    enhanced_cols <- c("prob_over_over_mean", "prob_over_under_mean",
                      "prob_under_over_mean", "prob_under_under_mean")
    has_enhanced <- all(enhanced_cols %in% names(corr))

    if (use_enhanced_metrics && has_enhanced) {
      cat("   \n")
      cat("   Enhanced mean-based scenarios:\n")
      cat(sprintf("   Over/Over:    %.1f%%\n", corr$prob_over_over_mean * 100))
      cat(sprintf("   Over/Under:   %.1f%%\n", corr$prob_over_under_mean * 100))
      cat(sprintf("   Under/Over:   %.1f%%\n", corr$prob_under_over_mean * 100))
      cat(sprintf("   Under/Under:  %.1f%%\n", corr$prob_under_under_mean * 100))

      # Highlight best scenarios
      scenarios <- c(corr$prob_over_over_mean, corr$prob_over_under_mean,
                    corr$prob_under_over_mean, corr$prob_under_under_mean)
      scenario_names <- c("Over/Over", "Over/Under", "Under/Over", "Under/Under")
      best_idx <- which.max(scenarios)

      cat(sprintf("   Best scenario: %s (%.1f%% probability)\n",
                  scenario_names[best_idx], scenarios[best_idx] * 100))

    } else {
      # Use standard expected vote scenarios (default behavior)
      cat("   \n")
      cat("   Expected vote scenarios:\n")
      cat(sprintf("   Over/Over:    %.1f%%\n", corr$prob_both_over * 100))
      cat(sprintf("   Over/Under:   %.1f%%\n", corr$prob_p1_over_p2_under * 100))
      cat(sprintf("   Under/Over:   %.1f%%\n", corr$prob_p1_under_p2_over * 100))
      cat(sprintf("   Under/Under:  %.1f%%\n", corr$prob_both_under * 100))

      # Show best scenario for standard metrics too
      scenarios <- c(corr$prob_both_over, corr$prob_p1_over_p2_under,
                    corr$prob_p1_under_p2_over, corr$prob_both_under)
      scenario_names <- c("Over/Over", "Over/Under", "Under/Over", "Under/Under")
      best_idx <- which.max(scenarios)

      cat(sprintf("   Best scenario: %s (%.1f%% probability)\n",
                  scenario_names[best_idx], scenarios[best_idx] * 100))
    }

    cat("\n")
  }

  # Summary statistics
  if (use_enhanced_metrics && has_enhanced && "prob_over_under_mean" %in% names(top_corrs)) {
    cat("=== SUMMARY STATISTICS (Enhanced Metrics) ===\n")

    # Calculate average probabilities across all top correlations
    avg_over_over <- mean(top_corrs$prob_over_over_mean, na.rm = TRUE)
    avg_over_under <- mean(top_corrs$prob_over_under_mean, na.rm = TRUE)
    avg_under_over <- mean(top_corrs$prob_under_over_mean, na.rm = TRUE)
    avg_under_under <- mean(top_corrs$prob_under_under_mean, na.rm = TRUE)

    cat(sprintf("Average scenario probabilities across top %d pairs:\n", nrow(top_corrs)))
    cat(sprintf("Over/Over:    %.1f%%\n", avg_over_over * 100))
    cat(sprintf("Over/Under:   %.1f%%\n", avg_over_under * 100))
    cat(sprintf("Under/Over:   %.1f%%\n", avg_under_over * 100))
    cat(sprintf("Under/Under:  %.1f%%\n", avg_under_under * 100))

    # Identify most profitable scenario type
    avg_scenarios <- c(avg_over_over, avg_over_under, avg_under_over, avg_under_under)
    scenario_names <- c("Over/Over", "Over/Under", "Under/Over", "Under/Under")
    best_avg_idx <- which.max(avg_scenarios)

    cat(sprintf("\nMost common profitable scenario: %s (%.1f%% average)\n",
                scenario_names[best_avg_idx], avg_scenarios[best_avg_idx] * 100))
  } else {
    cat("=== SUMMARY STATISTICS ===\n")

    # Use standard metrics for summary
    avg_over_over <- mean(top_corrs$prob_both_over, na.rm = TRUE)
    avg_over_under <- mean(top_corrs$prob_p1_over_p2_under, na.rm = TRUE)
    avg_under_over <- mean(top_corrs$prob_p1_under_p2_over, na.rm = TRUE)
    avg_under_under <- mean(top_corrs$prob_both_under, na.rm = TRUE)

    cat(sprintf("Average scenario probabilities across top %d pairs:\n", nrow(top_corrs)))
    cat(sprintf("Over/Over:    %.1f%%\n", avg_over_over * 100))
    cat(sprintf("Over/Under:   %.1f%%\n", avg_over_under * 100))
    cat(sprintf("Under/Over:   %.1f%%\n", avg_under_over * 100))
    cat(sprintf("Under/Under:  %.1f%%\n", avg_under_under * 100))

    # Identify most profitable scenario type
    avg_scenarios <- c(avg_over_over, avg_over_under, avg_under_over, avg_under_under)
    scenario_names <- c("Over/Over", "Over/Under", "Under/Over", "Under/Under")
    best_avg_idx <- which.max(avg_scenarios)

    cat(sprintf("\nMost common profitable scenario: %s (%.1f%% average)\n",
                scenario_names[best_avg_idx], avg_scenarios[best_avg_idx] * 100))
  }

  # Show correlation analysis (same for both metrics)
  avg_correlation <- mean(abs(top_corrs$correlation), na.rm = TRUE)
  cat(sprintf("Average correlation strength: %.3f\n", avg_correlation))

  if (avg_correlation > 0.5) {
    cat("Strong negative correlations detected - good for systematic betting strategies.\n")
  } else if (avg_correlation > 0.3) {
    cat("Moderate correlations detected - suitable for selective betting opportunities.\n")
  } else {
    cat("Weak correlations detected - proceed with caution for betting strategies.\n")
  }

  return(invisible(top_corrs))
}

#' Generate Round-by-Round Expected Votes Table
#'
#' Creates a pivot table showing expected votes by round for top candidates
#'
#' @param sim_data Simulation data with probability columns
#' @param results_table Results table with top candidates
#' @param model_name Model name for probability column prefix
#' @param top_n Number of top candidates to include (default: 15)
#'
#' @return Data frame with round-by-round expected votes, or NULL if no round data
#' @export
create_top_players_round_table <- function(sim_data, results_table, model_name, top_n = 15) {

  if (is.null(sim_data) || nrow(sim_data) == 0) return(NULL)

  # Find round column
  round_col <- case_when(
    "match_round" %in% names(sim_data) ~ "match_round",
    "round" %in% names(sim_data) ~ "round",
    "Round" %in% names(sim_data) ~ "Round",
    TRUE ~ NA_character_
  )

  if (is.na(round_col)) return(NULL)

  # Get top players
  top_players <- results_table %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(player_name, team)

  # Check probability columns exist
  prob_cols <- paste0(model_name, "_prob_", 0:3)
  if (!all(prob_cols %in% names(sim_data))) return(NULL)

  # Calculate expected votes by round
  round_table <- sim_data %>%
    dplyr::semi_join(top_players, by = c("player_name", "team")) %>%
    dplyr::mutate(
      expected_votes = !!rlang::sym(prob_cols[2]) * 1 +
                      !!rlang::sym(prob_cols[3]) * 2 +
                      !!rlang::sym(prob_cols[4]) * 3
    ) %>%
    dplyr::select(player_name, team, round = !!rlang::sym(round_col), expected_votes) %>%
    # Convert round values to standardized format
    dplyr::mutate(
      round = dplyr::case_when(
        stringr::str_detect(tolower(round), "opening") ~ "0",  # Opening Round -> 0
        TRUE ~ as.character(round)
      )
    ) %>%
    # Aggregate duplicates by summing expected votes per player-round combination
    dplyr::group_by(player_name, team, round) %>%
    dplyr::summarise(expected_votes = sum(expected_votes, na.rm = TRUE), .groups = "drop") %>%
    # Format round numbers with zero-padding for proper column ordering
    dplyr::mutate(round = sprintf("R%02d", as.numeric(round))) %>%
    tidyr::pivot_wider(
      names_from = round,
      values_from = expected_votes
    ) %>%
    dplyr::mutate(
      # Convert any remaining NAs to 0, ensure numeric columns, and round to 2 decimal places
      dplyr::across(dplyr::starts_with("R"), ~ round(ifelse(is.na(.x), 0, as.numeric(.x)), 2)),
      Total = round(rowSums(dplyr::select(., dplyr::starts_with("R")), na.rm = TRUE), 2)
    ) %>%
    # Reorder columns to ensure proper R01, R02, R03... sequence
    {
      round_cols <- names(.)[stringr::str_detect(names(.), "^R\\d+$")]
      round_cols_sorted <- round_cols[order(as.numeric(stringr::str_extract(round_cols, "\\d+")))]
      dplyr::select(., player_name, team, dplyr::all_of(round_cols_sorted), Total)
    } %>%
    dplyr::arrange(dplyr::desc(Total))

  return(round_table)
}
