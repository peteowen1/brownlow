#' Feature Engineering for Brownlow Medal Prediction
#'
#' This file contains functions for creating features from raw AFL statistics
#' that are relevant for predicting Brownlow Medal votes.


#' Create All Features Pipeline
#'
#' Main function that creates all features for Brownlow prediction
#'
#' @param brownlow_data List containing votes, stats, results, and other data
#' @param include_rolling Logical. Include rolling features (default: TRUE)
#' @param rolling_windows Vector of rolling window sizes (default: c(5, 10) for efficiency)
#'
#' @return DataFrame with all features for modeling
#' @export
create_all_features <- function(brownlow_data,
                                include_rolling = TRUE,
                                rolling_windows = c(5, 10)) {

  stats <- brownlow_data$stats
  results <- brownlow_data$results
  votes <- brownlow_data$votes

  # Create comprehensive performance features
  message("Creating player performance features...")
  stats_df <- create_comprehensive_performance_features(stats, results)

  # Create performance-based historical features (no brownlow_votes dependencies)
  message("Creating performance-based historical features...")
  stats_df <- create_comprehensive_performance_historical_features(stats_df)

  # Create team context features
  message("Creating team context features...")
  team_features <- create_comprehensive_team_features(
    stats_df = stats_df,
    results = results,
    include_umpires = FALSE,  # Use optimized default - no individual umpire names
    top_umpires = 0          # Use optimized default - no individual umpire features
  )
  stats_df <- team_features$data

  # Create weather features if weather data available
  weather_cols <- names(stats_df)[stringr::str_detect(names(stats_df), "weather|temp|wind|rain")]
  if (length(weather_cols) > 0) {
    message("Creating weather features...")
    stats_df <- create_comprehensive_weather_features(stats_df)
  }

  # Create contextual features (venue, time, opposition, match importance)
  message("Creating contextual features...")
  stats_df <- create_comprehensive_contextual_features(stats_df)

  # Create rolling performance features
  if (include_rolling) {
    message("Creating rolling performance features...")
    # VERY AGGRESSIVE: Only the most important rolling features
    # Based on feature importance: recent_consistency_disposals was important (gain: 0.00575)
    # Focus on only disposals with 5-game window for consistency metric
    key_metrics <- c("disposals")  # Only disposals - highest importance rolling feature
    reduced_windows <- c(5)        # Only 5-game window
    stats_df <- create_rolling_performance_features(stats_df, key_metrics, reduced_windows)

    # Manually add only the most important rolling features based on importance data
    stats_df <- stats_df %>%
      dplyr::group_by(player_name) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        # Only keep rolling consistency for disposals (was high importance: 0.00575)
        recent_consistency_disposals = disposals_roll5_sd,
        # Add recent vs career for top metrics (these were high importance)
        recent_vs_career_disposals = ifelse(!is.na(disposals_roll5_mean),
                                          disposals_roll5_mean - dplyr::cummean(dplyr::lag(disposals, default = 0)),
                                          0),
        recent_vs_career_impact = ifelse(!is.na(impact_score) & dplyr::row_number() > 5,
                                       zoo::rollmean(dplyr::lag(impact_score, default = 0), k = 5, fill = 0, align = "right") -
                                       dplyr::cummean(dplyr::lag(impact_score, default = 0)),
                                       0)
      ) %>%
      dplyr::ungroup() %>%
      # Remove all the automatically generated rolling features except the ones we want
      dplyr::select(-dplyr::matches("_roll\\d+_|_above_roll|_momentum"))
  }

  # Add GAM player coefficients as features if GAM model exists
  message("Checking for GAM player coefficients...")
  gam_model_path <- "models/gam_production_model.rds"
  if (file.exists(gam_model_path)) {
    message("Loading GAM model and extracting player coefficients...")

    tryCatch({
      # Load GAM production model
      gam_model <- readRDS(gam_model_path)

      # Extract player random effects
      player_coeffs <- extract_random_effects(gam_model, add_group_N = TRUE, group_var = "player_name")

      if (nrow(player_coeffs) > 0) {
        # Create player coefficient features
        player_coeff_features <- player_coeffs %>%
          dplyr::select(
            player_name = group,
            player_gam_coeff = value,
            player_gam_coeff_se = se,
            player_gam_coeff_n_obs = n
          ) %>%
          dplyr::mutate(
            # Create percentile ranking of coefficients
            player_gam_coeff_rank = dplyr::percent_rank(player_gam_coeff),
            # Create z-score normalized version
            player_gam_coeff_scaled = scale(player_gam_coeff)[,1],
            # Handle missing SE values
            player_gam_coeff_se = ifelse(is.na(player_gam_coeff_se), median(player_gam_coeff_se, na.rm = TRUE), player_gam_coeff_se),
            # Create coefficient confidence measure (inverse of SE)
            player_gam_coeff_confidence = 1 / (1 + player_gam_coeff_se)
          )

        # Join with main dataset
        stats_df <- stats_df %>%
          dplyr::left_join(player_coeff_features, by = "player_name")

        # Handle missing players (new players not in GAM training data)
        mean_coeff <- mean(player_coeff_features$player_gam_coeff, na.rm = TRUE)
        median_se <- median(player_coeff_features$player_gam_coeff_se, na.rm = TRUE)

        stats_df <- stats_df %>%
          dplyr::mutate(
            # Fill missing coefficients with population mean (neutral prior)
            player_gam_coeff = ifelse(is.na(player_gam_coeff), mean_coeff, player_gam_coeff),
            player_gam_coeff_se = ifelse(is.na(player_gam_coeff_se), median_se, player_gam_coeff_se),
            player_gam_coeff_n_obs = ifelse(is.na(player_gam_coeff_n_obs), 0, player_gam_coeff_n_obs),
            # Recalculate derived features for missing players
            player_gam_coeff_rank = ifelse(is.na(player_gam_coeff_rank), 0.5, player_gam_coeff_rank),
            player_gam_coeff_scaled = ifelse(is.na(player_gam_coeff_scaled), 0, player_gam_coeff_scaled),
            player_gam_coeff_confidence = ifelse(is.na(player_gam_coeff_confidence),
                                               1 / (1 + median_se), player_gam_coeff_confidence)
          )

        message(sprintf("✓ Added 6 GAM coefficient features from %d players", nrow(player_coeffs)))
      } else {
        message("⚠ No player coefficients extracted from GAM model")
      }

    }, error = function(e) {
      message(sprintf("⚠ Failed to load GAM coefficients: %s", e$message))
    })
  } else {
    message("⚠ GAM model not found - run 04_gam_model.R first to enable GAM coefficient features")
  }

  # Target variable (votes) will be added separately for modeling
  # Feature engineering should be independent of target variable to prevent leakage

  # Validate and clean features
  validation_result <- validate_performance_features(stats_df)
  stats_df <- validation_result$cleaned_data

  # Ensure all column names are snake_case
  stats_df <- stats_df %>%
    janitor::clean_names()

  # Optional feature count limiting
  n_features <- ncol(stats_df)
  message(sprintf("Feature engineering complete. Dataset has %d rows and %d columns.",
                  nrow(stats_df), n_features))

  # Feature optimization summary
  message("AGGRESSIVE Optimizations applied:")
  message("- Individual umpire names: DISABLED (40+ features removed)")
  message("- Streak indicators: REMOVED (4 low-importance features)")
  message("- Weather extremes: REMOVED (2 low-importance features)")
  message("- Rolling features: DRASTICALLY REDUCED to only 3 essential features")
  message("  (recent_consistency_disposals, recent_vs_career_disposals, recent_vs_career_impact)")
  message("- Special occasions: REMOVED most (good_friday, anzac_day, special_occasion, etc.)")
  message("- Time features: REMOVED most (weekend_game, early_rounds, late_rounds, etc.)")
  message("- Match context: REMOVED most low-value features (must_win, dead_rubber, etc.)")
  if (file.exists("models/gam_production_model.rds")) {
    message("+ GAM player coefficients: ADDED 6 new features for player-specific priors")
  }

  # If we have too many features, suggest further optimization
  if (n_features > 150) {
    message(sprintf("Note: %d features created. Consider feature selection for optimal model performance.", n_features))
  } else if (n_features <= 120) {
    message(sprintf("Good: %d features should work well with XGBoost.", n_features))
  }

  return(stats_df)
}
