#' Data Exploration Utilities
#'
#' This module provides comprehensive utility functions for AFL Brownlow Medal data exploration
#' and analysis, extracting complex logic from the main exploration script.

#' Run Comprehensive Data Exploration
#'
#' Performs complete exploratory data analysis including quality assessment,
#' voting patterns, correlations, team analysis, and temporal patterns.
#'
#' @param brownlow_data Enhanced datathon data from enhance_datathon_data()
#' @param save_plots Logical, whether to save plots (default: TRUE)
#' @param verbose Logical, whether to show progress messages (default: TRUE)
#'
#' @return List containing all exploration results
#' @export
run_comprehensive_exploration <- function(brownlow_data, save_plots = TRUE, verbose = TRUE) {
  if (verbose) cat("Running comprehensive data exploration...\n")

  exploration_results <- list()

  # Data Quality Assessment
  if (verbose) cat("- Assessing data quality...")
  if (nrow(brownlow_data$stats) > 0) {
    quality_assessment <- assess_data_quality(brownlow_data$stats)
    exploration_results$quality_assessment <- quality_assessment

    if (save_plots && !is.null(quality_assessment$missing_summary)) {
      missing_plot <- create_missing_values_plot(quality_assessment$missing_summary)
      if (!is.null(missing_plot)) {
        save_exploration_plot(missing_plot, "plots/missing_values.png")
      }
    }
  }
  if (verbose) cat(" ✓\n")

  # Voting Patterns Analysis
  if (verbose) cat("- Analyzing voting patterns...")
  if (nrow(brownlow_data$votes) > 0) {
    voting_analysis <- analyze_voting_distribution(brownlow_data$votes)
    exploration_results$voting_analysis <- voting_analysis

    if (save_plots && !is.null(voting_analysis$vote_distribution)) {
      votes_plot <- create_vote_distribution_plot(voting_analysis$vote_distribution)
      if (!is.null(votes_plot)) {
        save_exploration_plot(votes_plot, "plots/brownlow_votes_distribution.png")
      }
    }

    # Top vote getters
    exploration_results$top_players <- identify_top_vote_getters(brownlow_data$votes)
  }
  if (verbose) cat(" ✓\n")

  # Performance Analysis
  if (verbose) cat("- Analyzing performance distributions...")
  if ("afl_fantasy_score" %in% names(brownlow_data$stats)) {
    performance_analysis <- analyze_performance_distribution(
      brownlow_data$stats, "afl_fantasy_score", "AFL Fantasy Score"
    )
    exploration_results$performance_analysis <- performance_analysis

    if (save_plots && !is.null(performance_analysis$performance_distribution)) {
      fantasy_plot <- create_performance_distribution_plot(
        performance_analysis$performance_distribution,
        "AFL Fantasy Points Distribution", "AFL Fantasy Score"
      )
      if (!is.null(fantasy_plot)) {
        save_exploration_plot(fantasy_plot, "plots/fantasy_distribution.png")
      }
    }
  }
  if (verbose) cat(" ✓\n")

  # Statistical Relationships
  if (verbose) cat("- Computing statistical relationships...")
  if (nrow(brownlow_data$stats) > 0) {
    correlation_analysis <- analyze_vote_stats_correlations(brownlow_data$stats)
    exploration_results$correlation_analysis <- correlation_analysis

    if (save_plots && !is.null(correlation_analysis$correlation_matrix)) {
      # Correlation heatmap
      correlation_heatmap <- create_correlation_heatmap(correlation_analysis$correlations)
      if (!is.null(correlation_heatmap)) {
        save_exploration_plot(correlation_heatmap, "plots/correlation_heatmap.png", width = 8, height = 10)
      }

      # Key scatter plots
      create_key_scatter_plots(brownlow_data$stats, save_plots = save_plots)
    }
  }
  if (verbose) cat(" ✓\n")

  # Team Analysis
  if (verbose) cat("- Analyzing team patterns...")
  if (nrow(brownlow_data$votes) > 0) {
    team_analysis <- analyze_team_voting_patterns(brownlow_data$votes)
    exploration_results$team_analysis <- team_analysis

    if (save_plots && !is.null(team_analysis$team_summary)) {
      team_plot <- create_team_voting_plot(team_analysis$team_summary)
      if (!is.null(team_plot)) {
        save_exploration_plot(team_plot, "plots/team_voting_totals.png")
      }
    }
  }
  if (verbose) cat(" ✓\n")

  # Player Performance Patterns
  if (verbose) cat("- Analyzing player efficiency...")
  if (nrow(brownlow_data$stats) > 0) {
    player_efficiency <- calculate_player_efficiency(brownlow_data$stats, min_games = 20, min_votes = 5)
    exploration_results$player_efficiency <- player_efficiency

    if (save_plots && !"error" %in% names(player_efficiency)) {
      efficiency_plot <- create_player_efficiency_plot(player_efficiency)
      if (!is.null(efficiency_plot)) {
        save_exploration_plot(efficiency_plot, "plots/player_efficiency.png")
      }
    }
  }
  if (verbose) cat(" ✓\n")

  # Temporal Patterns
  if (verbose) cat("- Analyzing temporal patterns...")
  if (nrow(brownlow_data$votes) > 0) {
    temporal_analysis <- analyze_temporal_voting_patterns(brownlow_data$votes)
    exploration_results$temporal_analysis <- temporal_analysis

    if (save_plots && !is.null(temporal_analysis$round_patterns)) {
      temporal_plot <- create_temporal_voting_plot(temporal_analysis$round_patterns)
      if (!is.null(temporal_plot)) {
        save_exploration_plot(temporal_plot, "plots/votes_by_round.png")
      }
    }
  }
  if (verbose) cat(" ✓\n")

  return(exploration_results)
}

#' Create Key Scatter Plots
#'
#' Creates scatter plots for key statistical relationships with Brownlow votes.
#'
#' @param stats_data Data frame containing player statistics
#' @param save_plots Logical, whether to save plots
#'
#' @return NULL (plots are saved if save_plots = TRUE)
#' @export
create_key_scatter_plots <- function(stats_data, save_plots = TRUE) {
  key_relationships <- c("goals", "disposals", "tackles", "contested_possessions")
  available_relationships <- intersect(key_relationships, names(stats_data))

  for (stat in available_relationships) {
    scatter_plot <- create_scatter_plot(stats_data, stat, "brownlow_votes")
    if (!is.null(scatter_plot) && save_plots) {
      filename <- paste0("plots/", stat, "_vs_votes.png")
      save_exploration_plot(scatter_plot, filename)
    }
  }
}

#' Generate Exploration Summary
#'
#' Compiles comprehensive exploration results into a structured summary.
#'
#' @param brownlow_data Enhanced datathon data
#' @param exploration_results Results from run_comprehensive_exploration()
#'
#' @return List containing organized exploration summary
#' @export
generate_exploration_summary <- function(brownlow_data, exploration_results) {
  summary <- list(
    data_summary = brownlow_data$metadata,
    quality_assessment = exploration_results$quality_assessment,
    voting_analysis = exploration_results$voting_analysis,
    top_players = exploration_results$top_players,
    correlation_analysis = exploration_results$correlation_analysis,
    team_analysis = exploration_results$team_analysis,
    player_efficiency = exploration_results$player_efficiency,
    temporal_analysis = exploration_results$temporal_analysis,
    performance_analysis = exploration_results$performance_analysis,
    enhanced_features = if (!is.null(brownlow_data$metadata$enhancements)) brownlow_data$metadata$enhancements else NULL,
    exploration_date = Sys.time()
  )

  return(summary)
}

#' Print Key Findings
#'
#' Extracts and prints key findings from exploration results.
#'
#' @param exploration_summary Summary from generate_exploration_summary()
#' @param verbose Logical, whether to print findings
#'
#' @return Character vector of key findings
#' @export
print_key_findings <- function(exploration_summary, verbose = TRUE) {
  findings <- character()

  # Vote rate
  if (!is.null(exploration_summary$voting_analysis$summary_stats)) {
    vote_rate <- exploration_summary$voting_analysis$summary_stats$vote_rate
    finding <- sprintf("%.1f%% of player performances result in votes", vote_rate)
    findings <- c(findings, finding)
    if (verbose) cat("-", finding, "\n")
  }

  # Top correlation
  if (!is.null(exploration_summary$correlation_analysis$correlations) &&
      nrow(exploration_summary$correlation_analysis$correlations) > 0) {
    top_corr <- exploration_summary$correlation_analysis$correlations[1, ]
    finding <- sprintf("Strongest correlation with votes: %s (r = %.3f)",
                      top_corr$statistic, top_corr$correlation)
    findings <- c(findings, finding)
    if (verbose) cat("-", finding, "\n")
  }

  # Enhanced features
  if (!is.null(exploration_summary$enhanced_features)) {
    finding <- paste("Enhanced with:", paste(names(exploration_summary$enhanced_features), collapse = ", "))
    findings <- c(findings, finding)
    if (verbose) cat("-", finding, "\n")
  }

  # Outputs
  findings <- c(findings, "Plots saved to plots/ directory")
  findings <- c(findings, "Summary data saved to data/exploration_summary.rds")
  if (verbose) {
    cat("- Plots saved to plots/ directory\n")
    cat("- Summary data saved to data/exploration_summary.rds\n")
  }

  return(findings)
}

#' Save Exploration Results
#'
#' Saves exploration summary to file and prints key findings.
#'
#' @param exploration_summary Summary from generate_exploration_summary()
#' @param file_path Path to save summary (default: "data/exploration_summary.rds")
#' @param verbose Logical, whether to show progress and findings
#'
#' @return Character vector of key findings
#' @export
save_exploration_results <- function(exploration_summary,
                                   file_path = "data/exploration_summary.rds",
                                   verbose = TRUE) {
  if (verbose) cat("Saving exploration results...\n")

  saveRDS(exploration_summary, file_path)

  if (verbose) cat("Key findings:\n")
  findings <- print_key_findings(exploration_summary, verbose = verbose)

  return(findings)
}