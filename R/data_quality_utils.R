# Data Quality Assessment Utilities
# Functions for assessing data quality, missing values, and data completeness


#' Assess Data Quality
#'
#' Comprehensive data quality assessment for AFL statistics
#'
#' @param stats DataFrame with player statistics
#' @param key_vars Vector of key variables to assess (optional)
#' @return List with quality assessment results
#' @export
assess_data_quality <- function(stats, key_vars = NULL) {
  
  if (is.null(key_vars)) {
    # Default key AFL statistics
    possible_vars <- c("kicks", "handballs", "disposals", "marks", "goals", "behinds", 
                       "tackles", "contested_possessions", "uncontested_possessions", 
                       "supercoach_score", "afl_fantasy_score", "brownlow_votes")
    key_vars <- intersect(possible_vars, names(stats))
  }
  
  if (length(key_vars) == 0) {
    return(list(
      error = "No recognized key variables found in data",
      available_columns = names(stats)
    ))
  }
  
  # Missing values analysis
  missing_summary <- stats %>%
    dplyr::select(dplyr::all_of(key_vars)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(is.na(.x)))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "Missing_Count") %>%
    dplyr::mutate(
      Missing_Percent = Missing_Count / nrow(stats) * 100,
      Total_Records = nrow(stats)
    ) %>%
    dplyr::arrange(dplyr::desc(Missing_Percent))
  
  # Data completeness by season
  completeness_by_season <- NULL
  if ("season" %in% names(stats)) {
    completeness_by_season <- stats %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        total_records = dplyr::n(),
        dplyr::across(dplyr::all_of(key_vars), ~sum(!is.na(.x)), .names = "complete_{.col}"),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("complete_"),
        names_to = "variable",
        names_prefix = "complete_",
        values_to = "complete_count"
      ) %>%
      dplyr::mutate(completeness_rate = complete_count / total_records * 100)
  }
  
  # Data type validation
  data_types <- sapply(stats[key_vars], class)
  numeric_vars <- names(data_types)[sapply(data_types, function(x) any(c("numeric", "integer") %in% x))]
  
  # Range validation for numeric variables
  range_summary <- NULL
  if (length(numeric_vars) > 0) {
    range_summary <- stats %>%
      dplyr::select(dplyr::all_of(numeric_vars)) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list(
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE)
      ), .names = "{.col}_{.fn}")) %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = c("variable", "statistic"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(names_from = statistic, values_from = value)
  }
  
  # Outlier detection for key variables
  outliers_summary <- NULL
  if (length(numeric_vars) > 0) {
    outliers_summary <- stats %>%
      dplyr::select(dplyr::all_of(numeric_vars)) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~{
        Q1 <- stats::quantile(.x, 0.25, na.rm = TRUE)
        Q3 <- stats::quantile(.x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        sum(.x < lower_bound | .x > upper_bound, na.rm = TRUE)
      }, .names = "outliers_{.col}")) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "variable", names_prefix = "outliers_", values_to = "outlier_count") %>%
      dplyr::mutate(outlier_percent = outlier_count / nrow(stats) * 100)
  }
  
  return(list(
    missing_summary = missing_summary,
    completeness_by_season = completeness_by_season,
    range_summary = range_summary,
    outliers_summary = outliers_summary,
    key_vars_found = key_vars,
    total_records = nrow(stats),
    assessment_date = Sys.time()
  ))
}

#' Create Missing Values Visualization
#'
#' Creates visualization of missing values pattern
#'
#' @param missing_summary DataFrame from assess_data_quality
#' @param title Character. Plot title
#' @return ggplot object
#' @export
create_missing_values_plot <- function(missing_summary, title = "Missing Values by Variable") {
  
  if (is.null(missing_summary) || nrow(missing_summary) == 0) {
    return(NULL)
  }
  
  ggplot2::ggplot(missing_summary, ggplot2::aes(x = stats::reorder(Variable, Missing_Percent), y = Missing_Percent)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Missing_Percent, 1), "%")),
              hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      x = "Variable",
      y = "Missing Percentage (%)",
      subtitle = paste("Total records:", unique(missing_summary$Total_Records))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}

#' Create Data Completeness Heatmap
#'
#' Creates heatmap showing data completeness by season and variable
#'
#' @param completeness_data DataFrame from assess_data_quality
#' @return ggplot object or NULL if no season data
#' @export
create_completeness_heatmap <- function(completeness_data) {
  
  if (is.null(completeness_data) || nrow(completeness_data) == 0) {
    return(NULL)
  }
  
  ggplot2::ggplot(completeness_data, ggplot2::aes(x = season, y = variable, fill = completeness_rate)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(completeness_rate, 0), "%")),
              color = "white", size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "red", mid = "yellow", high = "green",
      midpoint = 50, name = "Complete %"
    ) +
    ggplot2::labs(
      title = "Data Completeness by Season and Variable",
      x = "Season", y = "Variable"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Generate Data Quality Report
#'
#' Generates comprehensive data quality report
#'
#' @param quality_assessment List from assess_data_quality
#' @return Character vector with report lines
#' @export
generate_quality_report <- function(quality_assessment) {
  
  if (!is.null(quality_assessment$error)) {
    return(c(
      "DATA QUALITY ASSESSMENT FAILED",
      paste("Error:", quality_assessment$error),
      paste("Available columns:", paste(head(quality_assessment$available_columns, 10), collapse = ", "))
    ))
  }
  
  report <- c(
    "DATA QUALITY ASSESSMENT REPORT",
    "=" %>% rep(50) %>% paste(collapse = ""),
    "",
    paste("Total records analyzed:", quality_assessment$total_records),
    paste("Key variables found:", length(quality_assessment$key_vars_found)),
    paste("Variables:", paste(quality_assessment$key_vars_found, collapse = ", ")),
    ""
  )
  
  # Missing values summary
  if (!is.null(quality_assessment$missing_summary)) {
    missing_data <- quality_assessment$missing_summary
    high_missing <- missing_data %>% dplyr::filter(Missing_Percent > 10)
    
    report <- c(report,
      "MISSING VALUES ANALYSIS:",
      paste("- Variables with >10% missing:", nrow(high_missing)),
      if (nrow(high_missing) > 0) {
        c("- High missing variables:",
          paste("  ", high_missing$Variable, ":", round(high_missing$Missing_Percent, 1), "%"))
      } else {
        "- No variables with high missing rates"
      },
      ""
    )
  }
  
  # Outliers summary
  if (!is.null(quality_assessment$outliers_summary)) {
    outliers_data <- quality_assessment$outliers_summary
    high_outliers <- outliers_data %>% dplyr::filter(outlier_percent > 5)
    
    report <- c(report,
      "OUTLIERS ANALYSIS:",
      paste("- Variables with >5% outliers:", nrow(high_outliers)),
      if (nrow(high_outliers) > 0) {
        c("- High outlier variables:",
          paste("  ", high_outliers$variable, ":", round(high_outliers$outlier_percent, 1), "%"))
      } else {
        "- No variables with high outlier rates"
      },
      ""
    )
  }
  
  # Range validation
  if (!is.null(quality_assessment$range_summary)) {
    report <- c(report,
      "RANGE VALIDATION:",
      "- Variable ranges (min/max):"
    )
    
    ranges <- quality_assessment$range_summary %>%
      dplyr::mutate(range_text = paste0("  ", variable, ": ", round(min, 2), " to ", round(max, 2)))
    
    report <- c(report, ranges$range_text, "")
  }
  
  report <- c(report,
    paste("Assessment completed at:", quality_assessment$assessment_date)
  )
  
  return(report)
}

