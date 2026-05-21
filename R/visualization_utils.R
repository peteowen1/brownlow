# Visualization Utilities
# Functions for creating common plots and visualizations for AFL data exploration


#' Create Vote Distribution Plot
#'
#' Creates a bar plot showing Brownlow vote distribution
#'
#' @param vote_distribution DataFrame with vote distribution from analyze_voting_distribution
#' @param title Character. Plot title
#' @return ggplot object
#' @export
create_vote_distribution_plot <- function(vote_distribution, title = "Brownlow Votes Distribution") {
  
  if (is.null(vote_distribution) || nrow(vote_distribution) == 0) {
    return(NULL)
  }
  
  # Ensure vote_distribution has the right column name
  vote_col <- names(vote_distribution)[1]
  
  ggplot2::ggplot(vote_distribution, ggplot2::aes(x = factor(.data[[vote_col]]), y = percentage)) +
    ggplot2::geom_col(fill = "darkred", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
    ggplot2::labs(
      title = title,
      x = "Brownlow Votes", 
      y = "Percentage (%)",
      subtitle = paste("Total observations:", sum(vote_distribution$n))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Performance Distribution Plot
#'
#' Creates a bar plot showing performance level distribution
#'
#' @param performance_distribution DataFrame with performance distribution
#' @param title Character. Plot title
#' @param performance_name Character. Name of performance metric
#' @return ggplot object
#' @export
create_performance_distribution_plot <- function(performance_distribution, 
                                               title = NULL, 
                                               performance_name = "Performance") {
  
  if (is.null(performance_distribution) || nrow(performance_distribution) == 0) {
    return(NULL)
  }
  
  if (is.null(title)) {
    title <- paste(performance_name, "Distribution")
  }
  
  ggplot2::ggplot(performance_distribution, ggplot2::aes(x = reorder(performance_level, -percentage), y = percentage)) +
    ggplot2::geom_col(fill = "darkgreen", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
    ggplot2::labs(
      title = title,
      x = "Performance Level", 
      y = "Percentage (%)",
      subtitle = paste("Total performances:", sum(performance_distribution$n))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Player Efficiency Plot
#'
#' Creates a scatter plot showing player voting efficiency
#'
#' @param efficiency_data DataFrame with player efficiency metrics
#' @param x_var Character. X-axis variable (default: "total_disposals")
#' @param y_var Character. Y-axis variable (default: "total_votes") 
#' @param title Character. Plot title
#' @return ggplot object
#' @export
create_player_efficiency_plot <- function(efficiency_data, 
                                        x_var = "total_disposals", 
                                        y_var = "total_votes",
                                        title = "Player Voting Efficiency") {
  
  if (is.null(efficiency_data) || nrow(efficiency_data) == 0) {
    return(NULL)
  }
  
  if (!all(c(x_var, y_var) %in% names(efficiency_data))) {
    return(NULL)
  }
  
  # Calculate correlation for subtitle
  correlation <- cor(efficiency_data[[x_var]], efficiency_data[[y_var]], use = "complete.obs")
  subtitle <- paste("Total Votes vs Total Disposals | r =", round(correlation, 3))
  
  ggplot2::ggplot(efficiency_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(alpha = 0.7, color = "steelblue", size = 2) +
    ggplot2::geom_smooth(method = "lm", color = "red", se = TRUE) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = stringr::str_to_title(gsub("_", " ", x_var)),
      y = stringr::str_to_title(gsub("_", " ", y_var))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Temporal Voting Plot
#'
#' Creates line plot showing voting patterns over time (by round)
#'
#' @param temporal_data DataFrame with temporal voting data
#' @param x_var Character. X-axis variable (default: "round_num")
#' @param y_var Character. Y-axis variable (default: "total_votes")
#' @param title Character. Plot title
#' @return ggplot object
#' @export
create_temporal_voting_plot <- function(temporal_data, 
                                      x_var = "round_num", 
                                      y_var = "total_votes",
                                      title = "Voting Patterns Over Time") {
  
  if (is.null(temporal_data) || nrow(temporal_data) == 0) {
    return(NULL)
  }
  
  if (!all(c(x_var, y_var) %in% names(temporal_data))) {
    return(NULL)
  }
  
  ggplot2::ggplot(temporal_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_line(color = "darkgreen", linewidth = 1.2) +
    ggplot2::geom_point(color = "darkgreen", size = 2) +
    ggplot2::labs(
      title = title,
      x = stringr::str_to_title(gsub("_", " ", x_var)),
      y = stringr::str_to_title(gsub("_", " ", y_var))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Team Voting Comparison Plot
#'
#' Creates horizontal bar plot comparing team voting totals
#'
#' @param team_data DataFrame with team voting data
#' @param top_n Integer. Number of top teams to show (default: 10)
#' @param title Character. Plot title
#' @return ggplot object
#' @export
create_team_voting_plot <- function(team_data, top_n = 10, title = "Team Voting Totals") {
  
  if (is.null(team_data) || nrow(team_data) == 0) {
    return(NULL)
  }
  
  # Take top N teams and ensure team column exists
  if (!"team" %in% names(team_data)) {
    return(NULL)
  }
  
  plot_data <- team_data %>%
    dplyr::slice_head(n = top_n)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(team, total_votes), y = total_votes)) +
    ggplot2::geom_col(fill = "navy", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = total_votes), hjust = -0.1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = "Team", 
      y = "Total Brownlow Votes",
      subtitle = paste("Top", top_n, "teams by total votes")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Multi-Panel Summary Plot
#'
#' Creates a multi-panel plot combining key visualizations
#'
#' @param vote_dist DataFrame with vote distribution
#' @param performance_dist DataFrame with performance distribution  
#' @param efficiency_data DataFrame with efficiency data
#' @param temporal_data DataFrame with temporal data
#' @return ggplot object with multiple panels
#' @export
create_summary_plot <- function(vote_dist = NULL, performance_dist = NULL, 
                              efficiency_data = NULL, temporal_data = NULL) {
  
  plots <- list()
  
  # Vote distribution plot
  if (!is.null(vote_dist)) {
    plots$votes <- create_vote_distribution_plot(vote_dist, "Vote Distribution") +
      ggplot2::theme(text = ggplot2::element_text(size = 8))
  }
  
  # Performance distribution plot
  if (!is.null(performance_dist)) {
    plots$performance <- create_performance_distribution_plot(
      performance_dist, "Performance Distribution"
    ) +
      ggplot2::theme(text = ggplot2::element_text(size = 8), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  # Efficiency scatter plot
  if (!is.null(efficiency_data)) {
    plots$efficiency <- create_player_efficiency_plot(efficiency_data, title = "Player Efficiency") +
      ggplot2::theme(text = ggplot2::element_text(size = 8))
  }
  
  # Temporal plot
  if (!is.null(temporal_data)) {
    plots$temporal <- create_temporal_voting_plot(temporal_data, title = "Votes by Round") +
      ggplot2::theme(text = ggplot2::element_text(size = 8))
  }
  
  # If we have plots, combine them
  if (length(plots) > 0) {
    # Use patchwork if available, otherwise return first plot
    if (requireNamespace("patchwork", quietly = TRUE)) {
      combined_plot <- Reduce(`+`, plots) + 
        patchwork::plot_layout(ncol = 2) +
        patchwork::plot_annotation(title = "AFL Brownlow Medal Data Summary")
      return(combined_plot)
    } else {
      return(plots[[1]])
    }
  }
  
  return(NULL)
}

#' Save Plot with Standard Settings
#'
#' Saves a ggplot with consistent settings and directory structure
#'
#' @param plot ggplot object
#' @param filename Character. Filename (including path)
#' @param width Numeric. Plot width (default: 10)
#' @param height Numeric. Plot height (default: 6)
#' @param dpi Numeric. Resolution (default: 300)
#' @return Character. Path to saved file
#' @export
save_exploration_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  
  if (is.null(plot)) {
    return(NULL)
  }
  
  # Ensure directory exists
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  
  # Save plot
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  return(filename)
}

#' Create Correlation Heatmap
#'
#' Creates a ggplot heatmap for correlation matrices
#'
#' @param correlation_data DataFrame with correlations (statistic, correlation, significance)
#' @param title Character. Plot title
#' @param top_n Integer. Number of top correlations to show (default: 15)
#' @return ggplot object
#' @export
create_correlation_heatmap <- function(correlation_data, title = "Feature Correlations with Votes", top_n = 15) {
  
  if (is.null(correlation_data) || nrow(correlation_data) == 0) {
    return(NULL)
  }
  
  # Take top correlations and prepare for heatmap
  plot_data <- correlation_data %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      statistic = stringr::str_to_title(gsub("_", " ", statistic)),
      correlation_text = paste0(round(correlation, 3), 
                               dplyr::if_else(!is.na(significance_level) & significance_level != "", 
                                      significance_level, "")),
      fill_color = dplyr::case_when(
        correlation > 0 ~ "Positive",
        correlation < 0 ~ "Negative",
        TRUE ~ "None"
      )
    )
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = 1, y = reorder(statistic, abs(correlation)), fill = correlation)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = correlation_text), color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", 
      midpoint = 0, name = "Correlation"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "*** p < 0.001, ** p < 0.01, * p < 0.05",
      x = NULL, y = "Statistic"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
      legend.position = "right"
    )
}

