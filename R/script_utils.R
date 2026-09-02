# Script Utilities for Brownlow Medal Analysis
#
# This file contains utility functions to simplify the main analysis scripts (01-07)
# by handling all the complex data loading, validation, and error handling internally.

#' Load Features for Prediction
#'
#' Smart function that loads the best available feature data for a given season,
#' handling all the complexity of checking multiple data sources internally.
#'
#' @param target_season Integer season to load features for
#' @param verbose Logical whether to show detailed progress (default: TRUE)
#'
#' @return List with features data and metadata
#' @export
load_features_for_prediction <- function(target_season, verbose = TRUE) {
  if (verbose) cat(sprintf("Loading features for %d season...\n", target_season))
  
  # Try enhanced modeling data first (best option - includes GAM coefficients)
  enhanced_file <- "data/processed/enhanced_modeling_data.rds"
  if (file.exists(enhanced_file)) {
    modeling_data <- readRDS(enhanced_file)

    # Check all data splits for target season
    if (target_season %in% modeling_data$holdout$season) {
      features <- modeling_data$holdout %>% dplyr::filter(season == target_season)
      source <- "enhanced_holdout"
    } else if (target_season %in% modeling_data$test$season) {
      features <- modeling_data$test %>% dplyr::filter(season == target_season)
      source <- "enhanced_test"
    } else if (target_season %in% modeling_data$train$season) {
      features <- modeling_data$train %>% dplyr::filter(season == target_season)
      source <- "enhanced_train"
    } else {
      features <- NULL
      source <- NULL
    }
    
    if (!is.null(features)) {
      if (verbose) cat(sprintf("SUCCESS Found %d observations in %s\n", nrow(features), source))
      return(list(
        features = features,
        feature_cols = modeling_data$feature_cols,
        source = source
      ))
    }
  }
  
  # Try full features dataset (second option)
  full_features_file <- "data/processed/full_features_dataset.rds"
  if (file.exists(full_features_file)) {
    full_features_data <- readRDS(full_features_file)
    if (target_season %in% full_features_data$season) {
      features <- full_features_data %>% dplyr::filter(season == target_season)
      if (verbose) cat(sprintf("SUCCESS Found %d observations in full features dataset\n", nrow(features)))

      # Get feature columns from metadata or data
      feature_cols <- load_feature_columns()

      return(list(
        features = features,
        feature_cols = feature_cols,
        source = "full_features"
      ))
    }
  }

  # Try enhanced dataset (third option)
  enhanced_file <- "data/processed/enhanced_dataset.rds"
  if (file.exists(enhanced_file)) {
    enhanced_data <- readRDS(enhanced_file)
    if (target_season %in% enhanced_data$season) {
      features <- enhanced_data %>% dplyr::filter(season == target_season)
      if (verbose) cat(sprintf("SUCCESS Found %d observations in enhanced dataset\n", nrow(features)))

      # Get feature columns from metadata or data
      feature_cols <- load_feature_columns()

      return(list(
        features = features,
        feature_cols = feature_cols,
        source = "enhanced"
      ))
    }
  }
  
  # If no pre-computed features found, try to create features for future season
  if (verbose) cat(sprintf("WARNING  No pre-computed features found for %d\n", target_season))
  
  # Check if this is a future season - try to create features on demand
  tryCatch({
    if (verbose) cat(sprintf("Attempting to create features for %d season...\n", target_season))
    
    # Load season data with future season support
    season_data <- load_season_data(target_season, "future", verbose = FALSE)
    
    # Load historical data for feature engineering context
    historical_data <- tryCatch({
      enhance_datathon_data(
        file_path = "data/brownlow_datathon_dataset.csv",
        add_umpires = TRUE,
        add_weather = TRUE,
        progress = FALSE
      )
    }, error = function(e) {
      load_datathon_data("data/brownlow_datathon_dataset.csv")
    })
    
    # Create features for the target season
    features <- create_all_features(
      brownlow_data = list(
        stats = season_data,
        votes = historical_data$stats %>% dplyr::filter(season < target_season),
        metadata = list(seasons = target_season)
      ),
      include_rolling = TRUE,
      rolling_windows = c(3, 5, 10)
    )
    
    # Get feature columns
    feature_cols <- load_feature_columns()
    
    if (verbose) cat(sprintf("SUCCESS Created %d observations with features for %d\n", nrow(features), target_season))
    
    return(list(
      features = features,
      feature_cols = feature_cols,
      source = "generated_on_demand"
    ))
    
  }, error = function(e) {
    if (verbose) cat(sprintf("ERROR Failed to create features: %s\n", e$message))
    return(NULL)
  })
}

#' Load Feature Columns
#'
#' Get the list of feature columns to use for modeling
#'
#' @return Character vector of feature column names
load_feature_columns <- function() {
  # Try feature metadata first
  metadata_file <- "data/processed/feature_metadata.rds"
  if (file.exists(metadata_file)) {
    metadata <- readRDS(metadata_file)
    return(metadata$final_features)
  }
  
  # Fallback to default features
  return(c(
    "coaches_votes", "supercoach_score", "afl_fantasy_score", "expected_votes_history",
    "disp_vote_correlation", "match_margin", "impact_score", "avg_votes_per_disposal",
    "career_max_votes", "score_involvement", "goals_vote_correlation", "disposals",
    "player_career_games", "tackles_vote_correlation", "metres_gained", "kicks",
    "handballs", "marks", "tackles", "inside_fifties", "goal_accuracy", "rating_points"
  ))
}

#' Load Trained Models
#'
#' Load trained models with error handling. Prefers production models trained on
#' complete dataset over evaluation models.
#'
#' @param verbose Logical whether to show progress
#'
#' @return List with models and metadata
#' @export
load_trained_models <- function(verbose = TRUE) {
  
  # Try production models first (trained on complete dataset)
  production_xgb <- "models/xgboost_production_model.rds"
  production_gam <- "models/gam_production_model.rds"
  
  # Fallback to evaluation models
  evaluation_xgb <- "models/xgboost_final_model.rds"
  evaluation_gam <- "models/gam_final_model.rds"
  
  # Load XGBoost model
  if (file.exists(production_xgb)) {
    xgb_model <- readRDS(production_xgb)
    xgb_source <- "production"
  } else if (file.exists(evaluation_xgb)) {
    xgb_model <- readRDS(evaluation_xgb)
    xgb_source <- "evaluation"
  } else {
    stop("No XGBoost model found. Run 03_xgboost_model.R first.")
  }
  
  # Load GAM model
  if (file.exists(production_gam)) {
    gam_model <- readRDS(production_gam)
    gam_source <- "production"
  } else if (file.exists(evaluation_gam)) {
    gam_model <- readRDS(evaluation_gam)
    gam_source <- "evaluation"
  } else {
    stop("No GAM model found. Run 04_gam_model.R first.")
  }
  
  # Load ensemble weights from results
  ensemble_weights <- NULL
  if (file.exists("results/ensemble_results.rds")) {
    ensemble_results <- readRDS("results/ensemble_results.rds")
    ensemble_weights <- ensemble_results$model_weights
  }
  
  if (verbose) {
    cat(sprintf("SUCCESS Models loaded: XGB (%s), GAM (%s)", xgb_source, gam_source))
    if (!is.null(ensemble_weights)) {
      cat(sprintf(". Ensemble: %.0f%% XGB, %.0f%% GAM\n", ensemble_weights[1]*100, ensemble_weights[2]*100))
    } else {
      cat(". No ensemble weights found\n")
    }
  }
  
  # Create ensemble model object if weights are available
  ensemble_model <- NULL
  if (!is.null(ensemble_weights)) {
    ensemble_model <- list(
      weights = ensemble_weights,
      xgboost_model = xgb_model,
      gam_model = gam_model,
      ensemble_results = ensemble_results
    )
  }

  return(list(
    xgboost = xgb_model,
    gam = gam_model,
    ensemble = ensemble_model,
    model_sources = list(xgb = xgb_source, gam = gam_source)
  ))
}

#' Load Season Data
#'
#' Load data for a specific season with fallback options
#'
#' @param target_season Integer season to load
#' @param prediction_type Character "future" or "historical"
#' @param verbose Logical whether to show progress
#'
#' @return Data frame with season data
#' @export
load_season_data <- function(target_season, prediction_type = "historical", verbose = TRUE) {
  
  # Load complete dataset
  season_data <- tryCatch({
    enhance_datathon_data(
      file_path = "data/brownlow_datathon_dataset.csv",
      add_umpires = TRUE,
      add_weather = TRUE,
      progress = FALSE
    )
  }, error = function(e) {
    if (verbose) cat("Fallback to basic dataset...\n")
    load_datathon_data("data/brownlow_datathon_dataset.csv")
  })
  
  # Check available seasons
  available_seasons <- sort(unique(season_data$stats$season))
  
  # Filter for target season
  target_data <- season_data$stats %>% dplyr::filter(season == target_season)
  
  if (nrow(target_data) == 0) {
    if (prediction_type == "future" && target_season > max(available_seasons)) {
      # Create template data for future seasons
      latest_season <- max(season_data$stats$season)
      target_data <- season_data$stats %>%
        dplyr::filter(season == latest_season) %>%
        dplyr::slice_sample(n = min(2000, nrow(.))) %>%
        dplyr::mutate(
          season = target_season,
          date = as.Date(sprintf("%d-03-15", target_season)) + sample(0:200, n(), replace = TRUE),
          match_round = sample(1:23, n(), replace = TRUE)
        )
      if (verbose) cat(sprintf("SUCCESS %d template created\n", nrow(target_data)))
    } else {
      stop(sprintf("Season %d not found. Available: %s", target_season, paste(available_seasons, collapse = ", ")))
    }
  } else {
    if (verbose) cat(sprintf("SUCCESS %d found\n", nrow(target_data)))
  }
  
  return(target_data)
}

#' Generate Model Predictions
#'
#' Generate predictions from trained models with proper error handling
#'
#' @param prediction_data Data frame with features
#' @param trained_models List with trained models
#' @param feature_cols Character vector of feature column names
#' @param verbose Logical whether to show progress
#'
#' @return List with predictions
#' @export
generate_model_predictions <- function(prediction_data, trained_models, feature_cols, verbose = TRUE) {
  
  n_obs <- nrow(prediction_data)
  results <- list()
  
  # XGBoost predictions
  if (is.null(trained_models$XGBoost)) {
    stop("XGBoost model is NULL. Please retrain the model using 03_xgboost_model.R")
  }
  
  tryCatch({
    xgb_raw <- predict(trained_models$XGBoost, as.matrix(prediction_data[, feature_cols]))
    results$xgboost_probs <- process_xgboost_predictions(xgb_raw, n_obs)
    if (verbose) cat("SUCCESS XGB ")
  }, error = function(e) {
    stop(sprintf("XGBoost prediction failed: %s\nThis usually means the model has an invalid handle or incompatible features.\nPlease retrain using: source('03_xgboost_model.R')", e$message))
  })
  
  # GAM predictions  
  if (is.null(trained_models$GAM)) {
    stop("GAM model is NULL. Please retrain the model using 04_gam_model.R")
  }
  
  tryCatch({
    gam_data <- prepare_gam_prediction_data(prediction_data)
    gam_raw <- predict(trained_models$GAM, newdata = gam_data, type = "response")
    results$gam_probs <- process_gam_predictions(gam_raw, n_obs)
    if (verbose) cat("GAM ")
  }, error = function(e) {
    stop(sprintf("GAM prediction failed: %s\nThis usually means missing factor levels or incompatible data structure.\nPlease retrain using: source('04_gam_model.R')", e$message))
  })
  
  # Create ensemble
  if (!is.null(trained_models$ensemble_weights)) {
    weights <- trained_models$ensemble_weights
    results$ensemble_probs <- weights[1] * results$xgboost_probs + weights[2] * results$gam_probs
  } else {
    results$ensemble_probs <- 0.5 * results$xgboost_probs + 0.5 * results$gam_probs
  }
  
  # Normalize probabilities
  results$xgboost_probs <- normalize_probabilities(results$xgboost_probs)
  results$gam_probs <- normalize_probabilities(results$gam_probs)  
  results$ensemble_probs <- normalize_probabilities(results$ensemble_probs)
  
  return(results)
}

# Helper functions (internal)
process_xgboost_predictions <- function(raw_pred, n_obs) {
  if (is.matrix(raw_pred)) {
    return(raw_pred)
  } else if (length(raw_pred) == n_obs * 4) {
    return(as_prob_matrix(raw_pred, n_obs))
  } else {
    # Convert regression to probabilities
    expected_votes <- pmax(0, pmin(3, raw_pred))
    return(t(sapply(expected_votes, function(ev) {
      if (ev <= 0.5) c(1-ev/0.5, ev/0.5, 0, 0)
      else if (ev <= 1.5) c(0, 1-(ev-0.5), (ev-0.5), 0)
      else if (ev <= 2.5) c(0, 0, 1-(ev-1.5), (ev-1.5))
      else c(0, 0, 0, 1)
    })))
  }
}

process_gam_predictions <- function(raw_pred, n_obs) {
  if (is.matrix(raw_pred)) {
    if (ncol(raw_pred) == 4) {
      return(raw_pred)
    } else {
      # Convert cumulative to individual probabilities
      return(t(apply(raw_pred, 1, function(cum_probs) {
        c(cum_probs[1], diff(c(cum_probs, 1)))
      })))
    }
  } else {
    # Convert regression to probabilities
    expected_votes <- pmax(0, pmin(3, raw_pred))
    return(t(sapply(expected_votes, function(ev) {
      if (ev <= 0.5) c(1-ev/0.5, ev/0.5, 0, 0)
      else if (ev <= 1.5) c(0, 1-(ev-0.5), (ev-0.5), 0)
      else if (ev <= 2.5) c(0, 0, 1-(ev-1.5), (ev-1.5))
      else c(0, 0, 0, 1)
    })))
  }
}

prepare_gam_prediction_data <- function(prediction_data) {
  gam_data <- prediction_data
  
  # Create factor variables GAM expects
  if ("coaches_votes" %in% names(gam_data)) {
    gam_data$coaches_votes_factor <- as.factor(gam_data$coaches_votes)
  }
  
  if ("career_max_votes" %in% names(gam_data)) {
    gam_data$career_max_votes_factor <- as.factor(gam_data$career_max_votes)
  }
  
  if ("goal_accuracy" %in% names(gam_data)) {
    gam_data$goal_accuracy_factor <- as.factor(dplyr::case_when(
      gam_data$goal_accuracy == 0 ~ "No_Goals",
      gam_data$goal_accuracy > 0 & gam_data$goal_accuracy <= 1/3 ~ "Poor_33%",
      gam_data$goal_accuracy > 1/3 & gam_data$goal_accuracy <= 0.5 ~ "Below_Avg_50%",
      gam_data$goal_accuracy > 0.5 & gam_data$goal_accuracy <= 2/3 ~ "Above_Avg_67%",
      gam_data$goal_accuracy > 2/3 & gam_data$goal_accuracy <= 1 ~ "Excellent_100%",
      TRUE ~ "Other"
    ))
  }
  
  if ("team" %in% names(gam_data)) {
    gam_data$team <- as.factor(gam_data$team)
  }
  
  if ("player_name" %in% names(gam_data)) {
    gam_data$player_name <- forcats::fct_lump_n(as.factor(gam_data$player_name), 
                                               n = 300, other_level = "Other_Player")
  }
  
  return(gam_data)
}

normalize_probabilities <- function(prob_matrix) {
  t(apply(prob_matrix, 1, function(probs) probs / sum(probs)))
}

#' Apply Enhanced Prediction Constraints
#'
#' Applies AFL voting constraints to probability matrices from multiple models using
#' enhanced analytical normalization. Ensures both individual and match-level constraints.
#'
#' @param prob_matrices List with probability matrices
#' @param prediction_data Data frame with match information
#' @param verbose Logical whether to show progress
#' @param return_full_diagnostics Logical whether to return detailed validation results
#'
#' @return List with constrained predictions (and diagnostics if requested)
#' @export
apply_prediction_constraints <- function(prob_matrices, prediction_data, verbose = TRUE,
                                       return_full_diagnostics = FALSE) {
  if (verbose) cat("=== APPLYING ENHANCED AFL VOTING CONSTRAINTS ===\n")

  # Create match IDs
  match_ids <- if ("match_id" %in% names(prediction_data)) {
    prediction_data$match_id
  } else {
    paste(prediction_data$date, prediction_data$match_round, sep = "_")
  }

  if (verbose) {
    cat(sprintf("Processing %d players across %d matches\n",
                length(match_ids), length(unique(match_ids))))
  }

  results <- list()
  all_diagnostics <- list()

  # Apply enhanced constraints to each model
  for (model_name in names(prob_matrices)) {
    if (verbose) {
      cat(sprintf("\nNormalizing %s probabilities...\n", stringr::str_to_title(model_name)))
    }

    # Use enhanced normalization with dual constraints
    norm_result <- normalize_probability_matrix(
      prob_matrix = prob_matrices[[model_name]],
      match_ids = match_ids,
      max_votes_per_match = 6,
      verbose = verbose
    )

    # Extract expected votes for backward compatibility
    results[[model_name]] <- norm_result$expected_votes
    all_diagnostics[[model_name]] <- norm_result$diagnostics

    if (verbose) {
      diagnostics <- norm_result$diagnostics
      cat(sprintf("✓ %s Results:\n", stringr::str_to_title(model_name)))
      cat(sprintf("  Total votes: %.3f (target: %.1f)\n",
                  sum(results[[model_name]]),
                  length(unique(match_ids)) * 6))
      cat(sprintf("  Vote range: %.3f - %.3f\n",
                  min(results[[model_name]]),
                  max(results[[model_name]])))
      cat(sprintf("  Constraints satisfied: %s\n",
                  ifelse(diagnostics$constraint_violations$all_constraints_satisfied, "✅ YES", "⚠️  NO")))

      if (!diagnostics$constraint_violations$all_constraints_satisfied) {
        cat(sprintf("  Violations: %d row, %d match, %d vote limit\n",
                    diagnostics$constraint_violations$row_violations_count,
                    diagnostics$constraint_violations$match_violations_p1 + diagnostics$constraint_violations$match_violations_p2 + diagnostics$constraint_violations$match_violations_p3,
                    diagnostics$constraint_violations$vote_limit_violations))
      }
    }
  }

  # Overall summary
  if (verbose) {
    cat(sprintf("\n=== CONSTRAINT APPLICATION COMPLETE ===\n"))
    total_votes_all_models <- sum(sapply(results, sum))
    expected_total <- length(prob_matrices) * length(unique(match_ids)) * 6
    cat(sprintf("Total votes across all models: %.1f (expected: %.1f)\n",
                total_votes_all_models, expected_total))

    all_satisfied <- all(sapply(all_diagnostics, function(d) d$constraint_violations$all_constraints_satisfied))
    cat(sprintf("All constraints satisfied: %s\n",
                ifelse(all_satisfied, "✅ YES", "⚠️  SOME VIOLATIONS")))
  }

  # Return results with optional diagnostics
  if (return_full_diagnostics) {
    return(list(
      predictions = results,
      diagnostics = all_diagnostics,
      summary = list(
        total_players = length(match_ids),
        total_matches = length(unique(match_ids)),
        models_processed = names(prob_matrices),
        all_constraints_satisfied = all(sapply(all_diagnostics, function(d) d$constraint_violations$all_constraints_satisfied))
      )
    ))
  } else {
    # Backward compatibility - return just the predictions
    return(results)
  }
}

#' Create Total Predictions Table
#'
#' Generate comprehensive predictions table for all seasons using pre-built template from 02.
#' Uses production models if available, otherwise falls back to evaluation models.
#'
#' @param xgb_model Trained XGBoost model (optional, will load if NULL)
#' @param gam_model Trained GAM model (optional, will load if NULL)
#' @param ensemble_weights Numeric vector of model weights (optional, will load if NULL)
#' @param feature_cols Character vector of feature column names
#' @param verbose Logical whether to show progress
#'
#' @return Data frame with comprehensive predictions for all seasons
#' @export
create_total_predictions_table <- function(xgb_model = NULL, gam_model = NULL, ensemble_weights = NULL, 
                                         feature_cols, verbose = TRUE) {
  
  if (verbose) cat("Creating total predictions table (2015-2025)...\n")
  
  # Load models if not provided
  if (is.null(xgb_model) || is.null(gam_model) || is.null(ensemble_weights)) {
    if (verbose) cat("Loading models...")
    trained_models <- load_trained_models(verbose = FALSE)
    if (is.null(xgb_model)) xgb_model <- trained_models$XGBoost
    if (is.null(gam_model)) gam_model <- trained_models$GAM
    if (is.null(ensemble_weights)) ensemble_weights <- trained_models$ensemble_weights
    if (verbose) cat(" SUCCESS\n")
  }
  
  # Load pre-built template from 02_feature_engineering.R
  if (!file.exists("data/processed/total_predictions_template.rds")) {
    stop("Total predictions template not found. Please run 02_feature_engineering.R first.")
  }
  
  total_template <- readRDS("data/processed/total_predictions_template.rds")
  available_seasons <- sort(unique(total_template$season))
  
  if (verbose) cat(sprintf("Template loaded: %d observations across %s seasons\n", 
                          nrow(total_template), paste(range(available_seasons), collapse = "-")))
  
  # Prepare prediction data
  prediction_data <- total_template %>%
    dplyr::filter(!is.na(season)) %>%
    dplyr::select(dplyr::all_of(c("season", "player_name", "team", "date", 
                                 "match_round", "match_id", "brownlow_votes", feature_cols)))
  
  # Generate XGBoost predictions
  if (verbose) cat("Generating XGBoost predictions...")
  prediction_matrix <- as.matrix(prediction_data[, feature_cols])
  xgb_raw <- predict(xgb_model, prediction_matrix)
  xgb_probs <- process_xgboost_predictions(xgb_raw, nrow(prediction_data))
  if (verbose) cat(" SUCCESS\n")
  
  # Generate GAM predictions
  if (verbose) cat("Generating GAM predictions...")
  gam_data <- prepare_gam_prediction_data(prediction_data)
  gam_raw <- predict(gam_model, newdata = gam_data, type = "response")
  gam_probs <- process_gam_predictions(gam_raw, nrow(prediction_data))
  if (verbose) cat(" SUCCESS\n")
  
  # Create ensemble probabilities
  ensemble_probs <- ensemble_weights[1] * xgb_probs + ensemble_weights[2] * gam_probs
  
  # Normalize all probabilities
  xgb_probs <- normalize_probabilities(xgb_probs)
  gam_probs <- normalize_probabilities(gam_probs)
  ensemble_probs <- normalize_probabilities(ensemble_probs)
  
  # Create final results table
  total_predictions <- prediction_data %>%
    dplyr::select(season, player_name, team, date, match_round, match_id, brownlow_votes) %>%
    dplyr::mutate(
      xgb_prob_0 = xgb_probs[, 1],
      xgb_prob_1 = xgb_probs[, 2], 
      xgb_prob_2 = xgb_probs[, 3],
      xgb_prob_3 = xgb_probs[, 4],
      gam_prob_0 = gam_probs[, 1],
      gam_prob_1 = gam_probs[, 2],
      gam_prob_2 = gam_probs[, 3], 
      gam_prob_3 = gam_probs[, 4],
      ensemble_prob_0 = ensemble_probs[, 1],
      ensemble_prob_1 = ensemble_probs[, 2],
      ensemble_prob_2 = ensemble_probs[, 3],
      ensemble_prob_3 = ensemble_probs[, 4],
      xgb_expected_votes = 0 * xgb_probs[, 1] + 1 * xgb_probs[, 2] + 2 * xgb_probs[, 3] + 3 * xgb_probs[, 4],
      gam_expected_votes = 0 * gam_probs[, 1] + 1 * gam_probs[, 2] + 2 * gam_probs[, 3] + 3 * gam_probs[, 4],
      ensemble_expected_votes = 0 * ensemble_probs[, 1] + 1 * ensemble_probs[, 2] + 2 * ensemble_probs[, 3] + 3 * ensemble_probs[, 4]
    )
  
  if (verbose) {
    seasons_with_predictions <- length(unique(total_predictions$season))
    cat(sprintf("SUCCESS Total: %d observations across %d seasons\n", 
                nrow(total_predictions), seasons_with_predictions))
  }
  
  return(total_predictions)
}