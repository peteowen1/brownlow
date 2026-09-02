# Feature Selection and Analysis Utilities
#
# This module provides utility functions for feature correlation analysis,
# selection, and multicollinearity removal in the Brownlow Medal prediction pipeline.

#' Analyze Feature Correlations
#'
#' Computes correlations between features and target variable, creates visualizations,
#' and returns organized correlation results.
#'
#' @param features_data Data frame containing features and target
#' @param target_col Character string naming the target column (default: "brownlow_votes")
#' @param exclude_cols Character vector of columns to exclude from analysis
#' @param save_plots Logical, whether to save correlation plots (default: TRUE)
#' @param verbose Logical, whether to show progress messages (default: TRUE)
#'
#' @return List containing correlation analysis results
#' @export
analyze_feature_correlations <- function(features_data,
                                        target_col = "brownlow_votes",
                                        exclude_cols = c("date", "player_name", "team", "season", "match_round", "match_id",
                                                        "match_home_team", "match_away_team", "venue_name", "source"),
                                        save_plots = TRUE,
                                        verbose = TRUE) {
  if (verbose) cat("Analyzing feature correlations...")

  # Identify numeric features (exclude target column from exclusions for cleaning)
  all_numeric_cols <- features_data %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  # Features for correlation analysis (exclude specified columns but keep target)
  numeric_features <- setdiff(all_numeric_cols, exclude_cols)

  # Ensure target column is included in numeric features for analysis
  if (target_col %in% all_numeric_cols && !target_col %in% numeric_features) {
    numeric_features <- c(numeric_features, target_col)
  }

  # Clean features (handle infinite/NaN values) for ALL numeric columns
  features_clean <- features_data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_numeric_cols), ~replace(.x, is.infinite(.x) | is.nan(.x), NA))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_numeric_cols), ~tidyr::replace_na(.x, 0)))

  # Remove zero variance features (but preserve target column)
  zero_var_features <- features_clean %>%
    dplyr::select(dplyr::all_of(setdiff(numeric_features, target_col))) %>%
    dplyr::summarise(dplyr::across(everything(), ~var(.x, na.rm = TRUE))) %>%
    tidyr::pivot_longer(everything(), names_to = "feature", values_to = "variance") %>%
    dplyr::filter(variance == 0 | is.na(variance)) %>%
    dplyr::pull(feature)

  if (length(zero_var_features) > 0) {
    # Remove zero variance features but keep target column
    numeric_features <- setdiff(numeric_features, zero_var_features)
    if (verbose) cat(sprintf(" removed %d zero-variance features", length(zero_var_features)))
  }

  # Compute correlations with target
  feature_correlations <- features_clean %>%
    dplyr::select(dplyr::all_of(c(target_col, numeric_features))) %>%
    cor(use = "complete.obs") %>%
    as.data.frame() %>%
    dplyr::select(dplyr::all_of(target_col)) %>%
    tibble::rownames_to_column(var = "features") %>%
    dplyr::filter(features != target_col) %>%
    dplyr::mutate(abs_correlation = abs(.data[[target_col]])) %>%
    dplyr::arrange(desc(abs_correlation))

  if (verbose) cat(sprintf(" top correlation: %.3f", feature_correlations$abs_correlation[1]))

  # Create correlation visualization
  if (save_plots) {
    create_feature_correlation_plots(features_clean, feature_correlations, target_col, numeric_features)
  }

  if (verbose) cat(" ✓\n")

  return(list(
    correlations = feature_correlations,
    numeric_features = numeric_features,
    zero_var_features = zero_var_features,
    cleaned_data = features_clean
  ))
}

#' Create Correlation Plots
#'
#' Creates and saves correlation visualization plots.
#'
#' @param features_clean Cleaned feature data
#' @param feature_correlations Correlation analysis results
#' @param target_col Target column name
#' @param numeric_features Vector of numeric feature names
#'
#' @return NULL (plots are saved)
#' @export
create_feature_correlation_plots <- function(features_clean, feature_correlations, target_col, numeric_features) {
  # Top correlations bar plot
  top_corr_plot <- head(feature_correlations, 15) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(features, abs_correlation), y = .data[[target_col]])) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[[target_col]] > 0), alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top 15 Feature Correlations with Brownlow Votes",
                  x = "Features", y = "Correlation with Votes",
                  fill = "Positive Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  ggplot2::ggsave("plots/features/feature_correlations.png", top_corr_plot,
                  width = 12, height = 8)

  # Correlation matrix for top features
  top_features <- head(feature_correlations, 30)$features
  selected_features <- c(target_col, intersect(top_features, numeric_features)[1:min(20, length(top_features))])

  if (length(selected_features) > 5) {
    cor_matrix <- features_clean %>%
      dplyr::select(dplyr::all_of(selected_features)) %>%
      cor(use = "complete.obs")

    # Save correlation plot
    png("plots/features/top_features_correlation.png", width = 1200, height = 1200)
    corrplot::corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
                      tl.cex = 0.7, tl.col = "black", tl.srt = 45,
                      title = "Correlation Matrix - Top Features")
    dev.off()
  }

  # Feature distribution by vote level
  create_feature_distribution_plot(features_clean, feature_correlations, target_col)
}

#' Create Feature Distribution Plot
#'
#' Creates boxplots showing feature distributions by vote level.
#'
#' @param features_clean Cleaned feature data
#' @param feature_correlations Correlation analysis results
#' @param target_col Target column name
#'
#' @return NULL (plot is saved)
#' @export
create_feature_distribution_plot <- function(features_clean, feature_correlations, target_col) {
  feature_by_votes <- features_clean %>%
    dplyr::select(dplyr::all_of(c(target_col, head(feature_correlations, 8)$features))) %>%
    dplyr::mutate(vote_level = dplyr::case_when(
      .data[[target_col]] == 0 ~ "No Votes",
      .data[[target_col]] == 1 ~ "1 Vote",
      .data[[target_col]] == 2 ~ "2 Votes",
      .data[[target_col]] == 3 ~ "3 Votes"
    )) %>%
    tidyr::pivot_longer(
      cols = -c(dplyr::all_of(target_col), vote_level),
      names_to = "feature",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(vote_level))

  feature_boxplot <- ggplot2::ggplot(feature_by_votes, ggplot2::aes(x = vote_level, y = value, fill = vote_level)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::facet_wrap(~feature, scales = "free_y", ncol = 2) +
    ggplot2::labs(title = "Feature Distributions by Brownlow Vote Level",
                  x = "Vote Level", y = "Feature Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "none")

  ggplot2::ggsave("plots/features/feature_distributions_by_votes.png", feature_boxplot,
                  width = 14, height = 10)
}

#' Select Final Features
#'
#' Performs feature selection by filtering correlations and removing multicollinearity.
#'
#' @param correlation_results Results from analyze_feature_correlations()
#' @param correlation_threshold Minimum correlation threshold (default: 0.1)
#' @param multicollinearity_threshold Maximum correlation between features (default: 0.9)
#' @param verbose Logical, whether to show progress messages (default: TRUE)
#'
#' @return Character vector of selected feature names
#' @export
select_final_features <- function(correlation_results,
                                 correlation_threshold = 0.05,
                                 multicollinearity_threshold = 0.95,
                                 verbose = TRUE) {
  if (verbose) cat("Selecting features...")

  feature_correlations <- correlation_results$correlations
  features_clean <- correlation_results$cleaned_data

  # High correlation features
  high_corr_features <- feature_correlations %>%
    dplyr::filter(abs_correlation > correlation_threshold) %>%
    dplyr::pull(features)

  # Remove highly correlated features to reduce multicollinearity
  if (length(high_corr_features) > 1) {
    high_corr_matrix <- features_clean %>%
      dplyr::select(dplyr::all_of(high_corr_features)) %>%
      cor(use = "complete.obs")

    # Find pairs with correlation > threshold
    high_corr_pairs <- which(abs(high_corr_matrix) > multicollinearity_threshold &
                            upper.tri(high_corr_matrix, diag = FALSE),
                            arr.ind = TRUE)

    if (nrow(high_corr_pairs) > 0) {
      features_to_remove <- c()

      for (i in 1:nrow(high_corr_pairs)) {
        feat1 <- rownames(high_corr_matrix)[high_corr_pairs[i, 1]]
        feat2 <- colnames(high_corr_matrix)[high_corr_pairs[i, 2]]

        corr1 <- feature_correlations$abs_correlation[feature_correlations$features == feat1]
        corr2 <- feature_correlations$abs_correlation[feature_correlations$features == feat2]

        if (length(corr1) > 0 && length(corr2) > 0) {
          if (corr1 < corr2) {
            features_to_remove <- c(features_to_remove, feat1)
          } else {
            features_to_remove <- c(features_to_remove, feat2)
          }
        }
      }

      features_to_remove <- unique(features_to_remove)
      if (length(features_to_remove) > 0) {
        high_corr_features <- setdiff(high_corr_features, features_to_remove)
        if (verbose) cat(sprintf(" removed %d collinear features", length(features_to_remove)))
      }
    }
  }

  final_features <- high_corr_features
  if (verbose) cat(sprintf(" final set: %d features\n", length(final_features)))

  return(final_features)
}

#' Create Feature Summary and Documentation
#'
#' Creates comprehensive feature summary including statistics and categorization.
#'
#' @param final_features Character vector of selected features
#' @param modeling_data Modeling dataset
#' @param feature_correlations Correlation analysis results
#' @param save_docs Logical, whether to save documentation CSV (default: TRUE)
#'
#' @return List containing feature summary and categories
#' @export
create_feature_summary <- function(final_features, modeling_data, feature_correlations, save_docs = TRUE) {
  # Feature summary statistics
  feature_summary <- tibble::tibble(
    features = final_features,
    correlation_with_votes = feature_correlations$brownlow_votes[match(final_features, feature_correlations$features)],
    mean_value = sapply(final_features, function(f) mean(modeling_data[[f]], na.rm = TRUE)),
    sd_value = sapply(final_features, function(f) sd(modeling_data[[f]], na.rm = TRUE)),
    min_value = sapply(final_features, function(f) min(modeling_data[[f]], na.rm = TRUE)),
    max_value = sapply(final_features, function(f) max(modeling_data[[f]], na.rm = TRUE))
  ) %>%
    dplyr::arrange(desc(abs(correlation_with_votes)))

  # Feature categories
  feature_categories <- tibble::tibble(
    features = final_features
  ) %>%
    dplyr::mutate(
      category = dplyr::case_when(
        stringr::str_detect(features, "roll") ~ "Rolling Statistics",
        stringr::str_detect(features, "career|history") ~ "Historical Performance",
        stringr::str_detect(features, "share|percentage") ~ "Team Context",
        stringr::str_detect(features, "efficiency|ratio") ~ "Efficiency Metrics",
        stringr::str_detect(features, "impact|score") ~ "Impact Metrics",
        stringr::str_detect(features, "home|winner|advantage") ~ "Match Context",
        stringr::str_detect(features, "tackles|contested|clearances") ~ "Contested Play",
        stringr::str_detect(features, "goals|marks|kicks") ~ "Offensive Stats",
        TRUE ~ "Basic Statistics"
      )
    )

  # Save documentation
  if (save_docs) {
    feature_docs <- feature_summary %>%
      dplyr::left_join(feature_categories, by = "features") %>%
      dplyr::select(features, category, correlation_with_votes, mean_value, sd_value) %>%
      dplyr::arrange(category, desc(abs(correlation_with_votes)))

    readr::write_csv(feature_docs, "data/processed/feature_documentation.csv")
  }

  return(list(
    summary = feature_summary,
    categories = feature_categories
  ))
}

#' Create Prediction Templates
#'
#' Creates templates for future seasons by extending latest season data.
#'
#' @param all_seasons_data Combined training and test data
#' @param final_features Character vector of selected features
#' @param target_seasons Vector of target seasons (default: 2015:2025)
#' @param verbose Logical, whether to show progress messages (default: TRUE)
#'
#' @return Extended dataset with future season templates
#' @export
create_prediction_templates <- function(all_seasons_data, final_features,
                                      target_seasons = 2015:2025, verbose = TRUE) {
  if (verbose) cat("Creating prediction templates...")

  available_seasons <- sort(unique(all_seasons_data$season))
  missing_seasons <- setdiff(target_seasons, available_seasons)

  if (length(missing_seasons) > 0) {
    # Create template data for missing seasons
    latest_season_data <- all_seasons_data %>%
      dplyr::filter(season == max(available_seasons)) %>%
      dplyr::slice_sample(n = min(2000, nrow(.)))

    future_data_list <- list()
    for (missing_season in missing_seasons) {
      future_template <- latest_season_data %>%
        dplyr::mutate(
          season = missing_season,
          date = as.Date(sprintf("%d-03-15", missing_season)) + sample(0:200, n(), replace = TRUE),
          match_round = sample(1:23, n(), replace = TRUE),
          brownlow_votes = NA_real_
        )
      future_data_list[[as.character(missing_season)]] <- future_template
    }

    all_seasons_data <- dplyr::bind_rows(all_seasons_data, future_data_list) %>%
      dplyr::arrange(season, date, player_name)

    if (verbose) cat(sprintf(" added templates for %d future seasons", length(missing_seasons)))
  }

  # Create final predictions template
  total_predictions_template <- all_seasons_data %>%
    dplyr::select(
      season, player_name, team, date, match_round, dplyr::any_of("match_id"), brownlow_votes,
      dplyr::all_of(final_features)
    )

  if (verbose) cat(sprintf(" %d observations across %d seasons\n",
                          nrow(total_predictions_template),
                          length(unique(total_predictions_template$season))))

  return(total_predictions_template)
}
