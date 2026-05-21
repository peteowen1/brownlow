# Correlation and Statistical Analysis Utilities
# Functions for analyzing statistical relationships and correlations


#' Analyze Vote-Statistics Correlations
#'
#' Analyzes correlations between player statistics and Brownlow votes
#'
#' @param stats DataFrame with player statistics and brownlow_votes
#' @param stat_cols Vector of statistic column names to analyze (optional)
#' @param target_col Character. Target variable column name (default: "brownlow_votes")
#' @return List with correlation analysis results
#' @export
analyze_vote_stats_correlations <- function(stats, stat_cols = NULL, target_col = "brownlow_votes") {
  
  if (!target_col %in% names(stats)) {
    return(list(
      error = paste("Target column", target_col, "not found in data"),
      available_columns = names(stats)
    ))
  }
  
  # Default AFL statistics if not specified
  if (is.null(stat_cols)) {
    possible_stats <- c("kicks", "handballs", "disposals", "marks", "goals", "behinds",
                       "tackles", "contested_possessions", "uncontested_possessions", 
                       "clearances", "inside_fifties", "rebounds", "intercepts",
                       "afl_fantasy_score", "supercoach_score")
    stat_cols <- intersect(possible_stats, names(stats))
  }
  
  # Filter to valid statistical columns
  available_stats <- intersect(stat_cols, names(stats))
  
  if (length(available_stats) == 0) {
    return(list(
      error = "No valid statistic columns found",
      requested_stats = stat_cols,
      available_columns = names(stats)
    ))
  }
  
  # Filter data to games where players actually played
  analysis_data <- stats %>%
    dplyr::filter(!is.na(.data[[target_col]]))
  
  if (nrow(analysis_data) == 0) {
    return(list(
      error = paste("No valid data for target variable", target_col)
    ))
  }
  
  # Calculate correlation matrix
  cor_columns <- c(target_col, available_stats)
  cor_data <- analysis_data %>%
    dplyr::select(dplyr::all_of(cor_columns)) %>%
    dplyr::select_if(is.numeric) %>%
    stats::cor(use = "complete.obs")
  
  # Extract correlations with target variable
  target_correlations <- cor_data[, target_col, drop = FALSE] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("statistic") %>%
    dplyr::filter(statistic != target_col) %>%
    dplyr::rename(correlation = !!target_col) %>%
    dplyr::mutate(
      abs_correlation = abs(correlation),
      strength = dplyr::case_when(
        abs_correlation >= 0.7 ~ "Very Strong",
        abs_correlation >= 0.5 ~ "Strong",
        abs_correlation >= 0.3 ~ "Moderate",
        abs_correlation >= 0.1 ~ "Weak",
        TRUE ~ "Very Weak"
      ),
      direction = ifelse(correlation > 0, "Positive", "Negative")
    ) %>%
    dplyr::arrange(dplyr::desc(abs_correlation))
  
  # Significance testing for correlations
  significance_tests <- analysis_data %>%
    dplyr::select(dplyr::all_of(cor_columns)) %>%
    dplyr::select_if(is.numeric) %>%
    {
      results <- tibble::tibble()
      target_data <- .[[target_col]]

      for (stat in available_stats) {
        if (stat %in% names(.)) {
          stat_data <- .[[stat]]
          # Remove NA pairs
          complete_pairs <- !is.na(target_data) & !is.na(stat_data)

          if (sum(complete_pairs) > 3) {  # Need at least 4 observations
            cor_test <- stats::cor.test(target_data[complete_pairs], stat_data[complete_pairs])
            results <- dplyr::bind_rows(results, tibble::tibble(
              statistic = stat,
              correlation = cor_test$estimate,
              p_value = cor_test$p.value,
              confidence_low = cor_test$conf.int[1],
              confidence_high = cor_test$conf.int[2],
              n_observations = sum(complete_pairs)
            ))
          }
        }
      }
      results
    }
  
  # Join significance results with correlations
  enhanced_correlations <- target_correlations %>%
    dplyr::left_join(significance_tests, by = c("statistic", "correlation")) %>%
    dplyr::mutate(
      significant = p_value < 0.05,
      significance_level = dplyr::case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    dplyr::arrange(dplyr::desc(abs_correlation))
  
  return(list(
    correlations = enhanced_correlations,
    correlation_matrix = cor_data,
    summary_stats = list(
      total_statistics = length(available_stats),
      significant_correlations = sum(enhanced_correlations$significant, na.rm = TRUE),
      strong_correlations = sum(enhanced_correlations$abs_correlation >= 0.5, na.rm = TRUE),
      observations_analyzed = nrow(analysis_data)
    ),
    target_variable = target_col,
    analysis_date = Sys.time()
  ))
}

#' Create Correlation Matrix Plot
#'
#' Creates a correlation matrix heatmap plot
#'
#' @param correlation_matrix Matrix from analyze_vote_stats_correlations
#' @param title Character. Plot title
#' @param method Character. corrplot method (default: "color")
#' @param output_file Character. Output file path (optional)
#' @return Saves plot to file, returns file path
#' @export
create_correlation_matrix_plot <- function(correlation_matrix, title = "Correlation Matrix", 
                                          method = "color", output_file = NULL) {
  
  if (is.null(correlation_matrix) || !is.matrix(correlation_matrix)) {
    return(NULL)
  }
  
  # Create file path if not provided
  if (is.null(output_file)) {
    output_file <- "plots/correlation_matrix.png"
  }
  
  # Ensure plots directory exists
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  grDevices::png(output_file, width = 1000, height = 1000, res = 120)

  corrplot::corrplot(correlation_matrix,
           method = method,
           type = "upper",
           order = "hclust",
           tl.cex = 0.8,
           tl.col = "black",
           tl.srt = 45,
           title = title,
           mar = c(0,0,2,0))

  grDevices::dev.off()
  
  return(output_file)
}

#' Create Scatter Plots for Key Relationships
#'
#' Creates scatter plots showing relationships between statistics and target variable
#'
#' @param stats DataFrame with statistics
#' @param x_var Character. X-axis variable
#' @param y_var Character. Y-axis variable (default: "brownlow_votes")
#' @param title Character. Plot title (optional)
#' @return ggplot object
#' @export
create_scatter_plot <- function(stats, x_var, y_var = "brownlow_votes", title = NULL) {
  
  if (!all(c(x_var, y_var) %in% names(stats))) {
    return(NULL)
  }
  
  if (is.null(title)) {
    title <- paste(stringr::str_to_title(gsub("_", " ", y_var)), "vs", stringr::str_to_title(gsub("_", " ", x_var)))
  }
  
  # Filter to valid data
  plot_data <- stats %>%
    dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Calculate correlation for subtitle
  correlation <- stats::cor(plot_data[[x_var]], plot_data[[y_var]], use = "complete.obs")
  subtitle <- paste("Correlation:", round(correlation, 3))
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_jitter(alpha = 0.5, width = if(is.integer(plot_data[[x_var]])) 0.1 else 0) +
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


#' Generate Correlation Summary Report
#'
#' Generates a text summary of correlation analysis results
#'
#' @param correlation_analysis List from analyze_vote_stats_correlations
#' @return Character vector with report lines
#' @export
generate_correlation_report <- function(correlation_analysis) {
  
  if (!is.null(correlation_analysis$error)) {
    return(c(
      "CORRELATION ANALYSIS FAILED",
      paste("Error:", correlation_analysis$error)
    ))
  }
  
  correlations <- correlation_analysis$correlations
  summary_stats <- correlation_analysis$summary_stats
  
  report <- c(
    "STATISTICAL CORRELATIONS REPORT",
    "=" %>% rep(50) %>% paste(collapse = ""),
    "",
    paste("Target variable:", correlation_analysis$target_variable),
    paste("Statistics analyzed:", summary_stats$total_statistics),
    paste("Observations:", summary_stats$observations_analyzed),
    paste("Significant correlations:", summary_stats$significant_correlations),
    paste("Strong correlations (|r| >= 0.5):", summary_stats$strong_correlations),
    ""
  )
  
  # Top correlations
  if (nrow(correlations) > 0) {
    top_correlations <- head(correlations, 10)
    
    report <- c(report,
      "TOP 10 CORRELATIONS:",
      sprintf("%-20s %8s %8s %12s", "Statistic", "r", "p-value", "Significance"),
      sprintf("%-20s %8s %8s %12s", "---------", "-", "-------", "------------")
    )
    
    for (i in 1:nrow(top_correlations)) {
      row <- top_correlations[i, ]
      report <- c(report,
        sprintf("%-20s %8.3f %8.3f %12s", 
                substr(row$statistic, 1, 20), 
                row$correlation,
                row$p_value,
                row$significance_level)
      )
    }
    
    report <- c(report, "")
  }
  
  # Correlation strength summary
  strength_summary <- correlations %>%
    dplyr::count(strength) %>%
    dplyr::arrange(dplyr::desc(n))
  
  if (nrow(strength_summary) > 0) {
    report <- c(report,
      "CORRELATION STRENGTH DISTRIBUTION:",
      paste("  ", strength_summary$strength, ":", strength_summary$n, "correlations")
    )
  }
  
  report <- c(report,
    "",
    paste("Analysis completed at:", correlation_analysis$analysis_date)
  )
  
  return(report)
}