#' Visualization Functions for Brownlow Medal Simulation Results
#'
#' This file contains functions to create comprehensive visualizations of Monte Carlo
#' simulation results for Brownlow Medal predictions.

#' Create Win Probability Bar Chart
#'
#' Creates a horizontal bar chart showing win probabilities for top players
#'
#' @param player_stats DataFrame with player statistics including win_probability
#' @param top_n Integer number of top players to show (default: 20)
#' @param model_name Character name of the model for title
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_win_probabilities <- function(player_stats, top_n = 20, model_name = "Model", 
                                  save_plot = FALSE, filename = NULL) {
  
  # Prepare data
  plot_data <- player_stats %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      player_label = paste0(player_name, " (", team, ")"),
      player_label = factor(player_label, levels = rev(player_label))  # For proper ordering
    )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = player_label, y = win_probability)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", win_probability)), 
                      hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Brownlow Medal Win Probabilities - %s Model", model_name),
      subtitle = sprintf("Top %d candidates based on %s simulation", 
                        nrow(plot_data), format(10000, big.mark = ",")),
      x = "Player",
      y = "Win Probability (%)",
      caption = "Based on Monte Carlo simulation of season using probability predictions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- sprintf("plots/simulation/win_probabilities_%s.png", 
                         tolower(stringr::str_replace_all(model_name, " ", "_")))
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 12, height = 8, dpi = 300)
    cat(sprintf("Win probability plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Expected Votes Distribution Plot
#'
#' Creates a scatter plot showing expected votes vs win probability with confidence intervals
#'
#' @param player_stats DataFrame with player statistics
#' @param top_n Integer number of top players to show
#' @param model_name Character name of the model
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_expected_votes_distribution <- function(player_stats, top_n = 30, model_name = "Model",
                                           save_plot = FALSE, filename = NULL) {
  
  # Prepare data
  plot_data <- player_stats %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      player_label = paste0(stringr::str_trunc(player_name, 12), " (", team, ")"),
      # Add confidence interval for expected votes if available
      votes_ci_lower = pmax(0, mean_votes - 1.96 * sd_votes / sqrt(1000)),  # Approximate CI
      votes_ci_upper = mean_votes + 1.96 * sd_votes / sqrt(1000)
    )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = mean_votes, y = win_probability)) +
    # Add confidence intervals for expected votes
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = votes_ci_lower, xmax = votes_ci_upper), 
                           alpha = 0.3, height = 0.5) +
    # Add points
    ggplot2::geom_point(ggplot2::aes(size = top3_probability), color = "steelblue", alpha = 0.8) +
    # Add labels for top candidates
    ggrepel::geom_text_repel(
      data = plot_data %>% dplyr::slice_head(n = 10),
      ggplot2::aes(label = player_label),
      size = 3, max.overlaps = 15
    ) +
    ggplot2::labs(
      title = sprintf("Expected Votes vs Win Probability - %s Model", model_name),
      subtitle = sprintf("Top %d candidates with 95%% confidence intervals", nrow(plot_data)),
      x = "Expected Season Votes",
      y = "Win Probability (%)",
      size = "Top 3 Prob (%)",
      caption = "Point size represents probability of finishing in top 3"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- sprintf("plots/simulation/expected_votes_%s.png", 
                         tolower(stringr::str_replace_all(model_name, " ", "_")))
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 12, height = 9, dpi = 300)
    cat(sprintf("Expected votes plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Position Distribution Plot
#'
#' Creates a plot showing the distribution of expected finishing positions
#'
#' @param player_stats DataFrame with player statistics
#' @param top_n Integer number of top players to show
#' @param model_name Character name of the model
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_position_distribution <- function(player_stats, top_n = 25, model_name = "Model",
                                      save_plot = FALSE, filename = NULL) {
  
  # Prepare data
  plot_data <- player_stats %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      player_label = paste0(player_name, " (", team, ")"),
      player_label = factor(player_label, levels = rev(player_label))
    )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = player_label, y = expected_position)) +
    ggplot2::geom_point(ggplot2::aes(color = win_probability, size = win_probability), alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Expected Finishing Positions - %s Model", model_name),
      subtitle = sprintf("Top %d candidates (lower position = better)", nrow(plot_data)),
      x = "Player",
      y = "Expected Finishing Position",
      color = "Win Prob (%)",
      size = "Win Prob (%)",
      caption = "Position 1 = Winner, lower numbers indicate higher expected finish"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_gradient(low = "lightblue", high = "darkblue") +
    ggplot2::scale_size_continuous(range = c(2, 6)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::guides(color = ggplot2::guide_colorbar(title.position = "top"),
                   size = ggplot2::guide_legend(title.position = "top"))
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- sprintf("plots/simulation/positions_%s.png", 
                         tolower(stringr::str_replace_all(model_name, " ", "_")))
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 10, height = 10, dpi = 300)
    cat(sprintf("Position distribution plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Model Comparison Plot
#'
#' Creates a plot comparing win probabilities across different models
#'
#' @param comparison_data DataFrame from compare_model_simulations()
#' @param top_n Integer number of top players to show
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_model_comparison <- function(comparison_data, top_n = 15, save_plot = FALSE, filename = NULL) {
  
  # Prepare data for plotting
  plot_data <- comparison_data %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      player_label = paste0(stringr::str_trunc(player_name, 15), " (", team, ")")
    )
  
  # Get win probability columns
  win_prob_cols <- grep("win_probability_", names(plot_data), value = TRUE)
  model_names <- stringr::str_replace(win_prob_cols, "win_probability_", "")
  
  if (length(win_prob_cols) < 2) {
    stop("Need at least 2 models for comparison")
  }
  
  # Reshape data for plotting
  plot_long <- plot_data %>%
    dplyr::select(player_label, all_of(win_prob_cols)) %>%
    tidyr::pivot_longer(
      cols = all_of(win_prob_cols),
      names_to = "model",
      values_to = "win_probability",
      names_prefix = "win_probability_"
    ) %>%
    dplyr::mutate(
      player_label = factor(player_label, levels = rev(unique(plot_data$player_label))),
      model = factor(model, levels = model_names)
    )
  
  # Create plot
  p <- ggplot2::ggplot(plot_long, ggplot2::aes(x = player_label, y = win_probability, fill = model)) +
    ggplot2::geom_col(position = "dodge", width = 0.7, alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Brownlow Medal Win Probabilities - Model Comparison",
      subtitle = sprintf("Top %d candidates across %d models", 
                        nrow(plot_data), length(model_names)),
      x = "Player",
      y = "Win Probability (%)",
      fill = "Model",
      caption = "Comparison of win probabilities from different simulation models"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- "plots/simulation/model_comparison.png"
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 12, height = 10, dpi = 300)
    cat(sprintf("Model comparison plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Top Finish Probabilities Plot
#'
#' Creates a stacked bar chart showing probabilities of top 3, 5, 10 finishes
#'
#' @param player_stats DataFrame with player statistics
#' @param top_n Integer number of top players to show
#' @param model_name Character name of the model
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_top_finish_probabilities <- function(player_stats, top_n = 20, model_name = "Model",
                                         save_plot = FALSE, filename = NULL) {
  
  # Prepare data
  plot_data <- player_stats %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(player_name, team, win_probability, top3_probability, 
                 top5_probability, top10_probability) %>%
    dplyr::mutate(
      player_label = paste0(player_name, " (", team, ")"),
      # Calculate exclusive probabilities
      prob_win = win_probability,
      prob_top3_not_win = pmax(0, top3_probability - win_probability),
      prob_top5_not_top3 = pmax(0, top5_probability - top3_probability),
      prob_top10_not_top5 = pmax(0, top10_probability - top5_probability),
      prob_not_top10 = pmax(0, 100 - top10_probability),
      player_label = factor(player_label, levels = rev(player_label))
    ) %>%
    tidyr::pivot_longer(
      cols = c(prob_win, prob_top3_not_win, prob_top5_not_top3, 
               prob_top10_not_top5, prob_not_top10),
      names_to = "finish_category",
      values_to = "probability"
    ) %>%
    dplyr::mutate(
      finish_category = factor(
        finish_category,
        levels = c("prob_not_top10", "prob_top10_not_top5", "prob_top5_not_top3", 
                   "prob_top3_not_win", "prob_win"),
        labels = c("Not Top 10", "Top 6-10", "Top 4-5", "Top 2-3", "Winner")
      )
    )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = player_label, y = probability, fill = finish_category)) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Probability of Top Finishes - %s Model", model_name),
      subtitle = sprintf("Distribution of finishing positions for top %d candidates", top_n),
      x = "Player",
      y = "Probability (%)",
      fill = "Finish Category",
      caption = "Stacked probabilities showing likelihood of different finishing positions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 9),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Not Top 10" = "#E0E0E0", "Top 6-10" = "#BFBFBF", 
                 "Top 4-5" = "#80B3FF", "Top 2-3" = "#4A90E2", "Winner" = "#1A5490")
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)))
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- sprintf("plots/simulation/top_finishes_%s.png", 
                         tolower(stringr::str_replace_all(model_name, " ", "_")))
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 12, height = 10, dpi = 300)
    cat(sprintf("Top finish probabilities plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Vote Distribution Histogram
#'
#' Creates histograms showing the distribution of votes for top candidates
#'
#' @param simulation_results List of raw simulation results
#' @param player_names Vector of player names to plot
#' @param team_names Vector of corresponding team names
#' @param model_name Character name of the model
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return ggplot object
#' @export
plot_vote_distributions <- function(simulation_results, player_names, team_names = NULL,
                                   model_name = "Model", save_plot = FALSE, filename = NULL) {
  
  if (is.null(team_names)) {
    team_names <- rep("", length(player_names))
  }
  
  n_sims <- length(simulation_results)
  
  # Extract vote distributions for specified players
  vote_data <- data.frame()
  
  for (i in 1:length(player_names)) {
    player_name <- player_names[i]
    team_name <- team_names[i]
    
    player_votes <- numeric(n_sims)
    
    for (sim_idx in 1:n_sims) {
      sim_result <- simulation_results[[sim_idx]]
      
      if (team_name == "") {
        player_row <- which(sim_result$player_name == player_name)
      } else {
        player_row <- which(sim_result$player_name == player_name & sim_result$team == team_name)
      }
      
      if (length(player_row) > 0) {
        player_votes[sim_idx] <- sim_result$total_votes[player_row[1]]
      }
    }
    
    vote_data <- rbind(vote_data, data.frame(
      player = if(team_name == "") player_name else paste0(player_name, " (", team_name, ")"),
      votes = player_votes,
      stringsAsFactors = FALSE
    ))
  }
  
  # Create plot
  p <- ggplot2::ggplot(vote_data, ggplot2::aes(x = votes)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue", color = "white") +
    ggplot2::facet_wrap(~ player, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title = sprintf("Vote Distribution Across Simulations - %s Model", model_name),
      subtitle = sprintf("Distribution of total season votes across %s simulations", 
                        format(n_sims, big.mark = ",")),
      x = "Total Season Votes",
      y = "Frequency",
      caption = "Each histogram shows possible vote outcomes for the player"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 10, face = "bold")
    )
  
  # Save plot if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- sprintf("plots/simulation/vote_distributions_%s.png", 
                         tolower(stringr::str_replace_all(model_name, " ", "_")))
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(filename, p, width = 14, height = 10, dpi = 300)
    cat(sprintf("Vote distribution plot saved to: %s\n", filename))
  }
  
  return(p)
}

#' Create Comprehensive Simulation Dashboard
#'
#' Creates a multi-panel dashboard with key simulation visualizations
#'
#' @param analysis_results List of analysis results from different models
#' @param top_n Integer number of top players to show
#' @param save_plot Logical whether to save the plot
#' @param filename Character filename for saved plot
#'
#' @return Combined plot object
#' @export
create_simulation_dashboard <- function(analysis_results, top_n = 15, 
                                       save_plot = FALSE, filename = NULL) {
  
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra package required for dashboard creation")
  }
  
  plots <- list()
  
  # Create individual plots for each model
  for (model_name in names(analysis_results)) {
    model_results <- analysis_results[[model_name]]
    
    # Win probabilities (smaller version for dashboard)
    p1 <- plot_win_probabilities(model_results, top_n = top_n, model_name = model_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_text(size = 8)
      )
    
    # Expected votes vs win probability
    p2 <- plot_expected_votes_distribution(model_results, top_n = top_n, model_name = model_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_text(size = 8),
        legend.position = "none"
      )
    
    plots[[paste0(model_name, "_prob")]] <- p1
    plots[[paste0(model_name, "_votes")]] <- p2
  }
  
  # Create comparison plot if multiple models
  if (length(analysis_results) > 1) {
    comparison_data <- compare_model_simulations(analysis_results, top_n = top_n)
    p_comp <- plot_model_comparison(comparison_data, top_n = top_n) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_text(size = 8)
      )
    plots[["comparison"]] <- p_comp
  }
  
  # Arrange plots
  if (length(analysis_results) == 1) {
    # Single model: side by side
    combined_plot <- gridExtra::grid.arrange(
      plots[[1]], plots[[2]],
      ncol = 2,
      top = grid::textGrob("Brownlow Medal Simulation Dashboard", 
                          gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )
  } else if (length(analysis_results) == 2) {
    # Two models: 2x2 grid plus comparison
    combined_plot <- gridExtra::grid.arrange(
      plots[[1]], plots[[2]],
      plots[[3]], plots[[4]],
      plots[["comparison"]],
      ncol = 2, nrow = 3,
      heights = c(1, 1, 1),
      top = grid::textGrob("Brownlow Medal Simulation Dashboard", 
                          gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )
  } else {
    # Multiple models: arrange dynamically
    n_models <- length(analysis_results)
    
    # Filter out any NULL plots to avoid zero dimension errors
    valid_plots <- plots[!sapply(plots, is.null)]
    
    if (length(valid_plots) > 0) {
      tryCatch({
        combined_plot <- gridExtra::grid.arrange(
          grobs = valid_plots,
          ncol = min(3, length(valid_plots)),
          top = grid::textGrob("Brownlow Medal Simulation Dashboard", 
                              gp = grid::gpar(fontsize = 16, fontface = "bold"))
        )
      }, error = function(e) {
        cat("Error creating dashboard grid arrangement:", e$message, "\n")
        cat("Falling back to simple plot list\n")
        # Return first plot as fallback
        combined_plot <- valid_plots[[1]]
      })
    } else {
      stop("No valid plots available for dashboard")
    }
  }
  
  # Save dashboard if requested
  if (save_plot) {
    if (is.null(filename)) {
      filename <- "plots/simulation/brownlow_simulation_dashboard.png"
    }
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    
    # Save with appropriate dimensions
    width <- if (length(analysis_results) == 1) 16 else 20
    height <- if (length(analysis_results) <= 2) 12 else 16
    
    ggplot2::ggsave(filename, combined_plot, width = width, height = height, dpi = 300)
    cat(sprintf("Simulation dashboard saved to: %s\n", filename))
  }
  
  return(combined_plot)
}