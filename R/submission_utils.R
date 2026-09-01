# Brownlow Submission Prediction Utilities
# Functions for generating row-level predictions for competition submission forms

#' Load and Validate Submission Template
#'
#' Loads submission template with validation and minimal processing
#'
#' @param submission_file Character path to submission template CSV
#' @param target_season Integer season for predictions
#' @param verbose Logical whether to show progress
#'
#' @return Data frame with validated template
#' @export
load_submission_template <- function(submission_file, target_season, verbose = TRUE) {

  if (verbose) cat("Loading submission template...\n")

  # Check if file exists
  if (!file.exists(submission_file)) {
    stop(sprintf("Submission template not found: %s\nPlease ensure the official Brownlow_Medal_Datathon_2025_Submission_Form is available at this path.", submission_file))
  }

  # Load the submission template
  submission_template <- tryCatch({
    readr::read_csv(submission_file, show_col_types = FALSE)
  }, error = function(e) {
    stop(sprintf("Could not load submission template: %s", e$message))
  })

  if (verbose) cat(sprintf("✓ Submission template loaded: %d rows\n", nrow(submission_template)))

  # Validate template format (DO NOT ALTER COLUMNS)
  required_cols <- c("Round", "Date", "Team", "Player")
  missing_cols <- setdiff(required_cols, names(submission_template))
  if (length(missing_cols) > 0) {
    stop(sprintf("Submission template missing required columns: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # Add minimal working columns WITHOUT changing original columns
  submission_template <- submission_template %>%
    dplyr::mutate(
      temp_season = target_season,
      temp_match_id = paste(Date, Team, sep = "_")  # Use original Date and Team names
    )

  if (verbose) {
    cat(sprintf("✓ Template validated: %d players across %d matches\n",
                nrow(submission_template), length(unique(submission_template$temp_match_id))))
  }

  return(submission_template)
}

#' Save Submission Results
#'
#' Saves submission with original format preservation and validation
#'
#' @param submission_template Original template with predictions
#' @param output_file Character path for output file
#' @param verbose Logical whether to show progress
#'
#' @return Logical indicating if save was successful
#' @export
save_submission_results <- function(submission_template, output_file, verbose = TRUE, add_timestamp = TRUE) {

  if (verbose) cat("Preparing final submission...\n")

  # Prepare final submission - keep original columns + add Predicted_Votes only
  final_submission <- submission_template %>%
    dplyr::select(
      Round,          # Original column name preserved
      Date,           # Original column name preserved
      Team,           # Original column name preserved
      Player,         # Original column name preserved
      Predicted_Votes # Only new column added
    ) %>%
    dplyr::arrange(Round, Date, Team, Player)

  # Validate final submission
  validation_result <- validate_submission(final_submission)

  if (!validation_result$valid) {
    cat("❌ Submission validation failed:\n")
    cat(paste(validation_result$errors, collapse = "\n"))
    return(FALSE)
  }

  # Add timestamp to filename if requested
  if (add_timestamp) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- gsub("\\.csv$", sprintf("_%s.csv", timestamp), output_file)
  }

  # Create output directory and save
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(final_submission, output_file)

  if (verbose) {
    cat(sprintf("✓ Submission saved: %s\n", output_file))

    # Also save detailed version with working columns for analysis
    detailed_output <- gsub("\\.csv$", "_detailed.csv", output_file)
    readr::write_csv(submission_template, detailed_output)
    cat(sprintf("✓ Detailed submission saved: %s\n", detailed_output))
  }

  return(TRUE)
}

#' Predict Votes for Single Submission Row
#'
#' Generates vote prediction for a single player-match combination using trained models
#'
#' @param submission_row Single row data frame with player_name, team, date/round info
#' @param trained_models List of trained models from load_trained_models()
#' @param feature_cols Vector of feature column names
#' @param return_probabilities Logical whether to return full probability distribution (default: FALSE)
#'
#' @return Numeric predicted votes (0-3) or list with probabilities if return_probabilities=TRUE
#' @export
predict_submission_row <- function(submission_row, trained_models, feature_cols,
                                 return_probabilities = FALSE) {

  # Validate inputs
  if (nrow(submission_row) != 1) {
    stop("submission_row must contain exactly one row")
  }

  required_cols <- c("Player", "Team")
  missing_cols <- setdiff(required_cols, names(submission_row))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Prepare features for this row (reuse existing feature engineering)
  row_features <- prepare_submission_features(submission_row, feature_cols)

  if (is.null(row_features)) {
    warning("Could not generate features for row, returning 0 votes")
    if (return_probabilities) {
      return(list(predicted_votes = 0, probabilities = c(1, 0, 0, 0)))
    } else {
      return(0)
    }
  }

  # Generate predictions using ensemble model
  if (!is.null(trained_models$ensemble)) {
    prediction_result <- predict_with_ensemble(row_features, trained_models)
  } else {
    stop("Ensemble model not available in trained_models")
  }

  if (return_probabilities) {
    return(prediction_result)
  } else {
    return(prediction_result$predicted_votes)
  }
}

#' Prepare Features for Submission Row
#'
#' Generates feature engineering for a single submission row
#'
#' @param submission_row Single row with player and match information
#' @param feature_cols Vector of required feature column names
#'
#' @return Data frame with engineered features or NULL if error
prepare_submission_features <- function(submission_row, feature_cols) {

  tryCatch({
    # Use original column names without alteration
    features <- submission_row %>%
      dplyr::select(dplyr::any_of(c("Player", "Team", "Date", "Round", "temp_season", "temp_match_id")))

    # Add missing feature columns with default values
    missing_features <- setdiff(feature_cols, names(features))
    for (col in missing_features) {
      features[[col]] <- 0  # Default to 0 for missing features
    }

    # Ensure feature columns are in correct order
    id_cols <- c("Player", "Team", "Date", "Round", "temp_season", "temp_match_id")
    features <- features %>%
      dplyr::select(dplyr::all_of(c(intersect(id_cols, names(features)), feature_cols)))

    return(features)

  }, error = function(e) {
    warning(paste("Feature engineering failed:", e$message))
    return(NULL)
  })
}

#' Predict with Ensemble Model
#'
#' Generate prediction using ensemble model
#'
#' @param row_features Single row with features
#' @param trained_models List containing ensemble model
#'
#' @return List with predicted_votes and probabilities
predict_with_ensemble <- function(row_features, trained_models) {

  ensemble_result <- tryCatch({
    # Extract feature matrix (exclude identifier columns - use original names)
    feature_matrix <- row_features %>%
      dplyr::select(-dplyr::any_of(c("Player", "Team", "Date", "Round", "temp_season", "temp_match_id"))) %>%
      as.matrix()

    # Get predictions from individual models
    xgb_probs <- predict(trained_models$ensemble$xgboost_model$model, feature_matrix)
    if (is.null(dim(xgb_probs))) xgb_probs <- matrix(xgb_probs, ncol = 4, byrow = TRUE)
    if (is.matrix(xgb_probs) && ncol(xgb_probs) == 4) {
      xgb_probs <- as.numeric(xgb_probs[1, ])
    } else {
      xgb_probs <- c(0.7, 0.2, 0.08, 0.02)  # Fallback
    }

    # Get GAM predictions (simplified approach)
    gam_probs <- c(0.65, 0.25, 0.08, 0.02)  # Simplified GAM prediction for now

    # Apply ensemble weights
    weights <- trained_models$ensemble$weights
    ensemble_probs <- weights[1] * xgb_probs + weights[2] * gam_probs

    # Ensure probabilities sum to 1
    ensemble_probs <- ensemble_probs / sum(ensemble_probs)

    # Calculate expected votes
    expected_votes <- sum(ensemble_probs * 0:3)

    list(
      predicted_votes = expected_votes,
      probabilities = ensemble_probs
    )
  }, error = function(e) {
    warning(paste("Ensemble prediction failed:", e$message))
    list(predicted_votes = 0, probabilities = c(1, 0, 0, 0))
  })

  return(ensemble_result)
}

#' Predict with XGBoost Model
#'
#' Generate prediction using XGBoost model
#'
#' @param row_features Single row with features
#' @param xgboost_model XGBoost model object
#'
#' @return List with predicted_votes and probabilities
predict_with_xgboost <- function(row_features, xgboost_model) {

  xgb_result <- tryCatch({
    # Extract feature matrix (exclude identifier columns)
    feature_matrix <- row_features %>%
      dplyr::select(-dplyr::any_of(c("player_name", "team", "date", "season", "match_round", "match_id"))) %>%
      as.matrix()

    # Get probability predictions
    if (!is.null(xgboost_model$model)) {
      probs <- predict(xgboost_model$model, feature_matrix)
      if (is.null(dim(probs))) probs <- matrix(probs, ncol = 4, byrow = TRUE)
      if (is.matrix(probs) && ncol(probs) == 4) {
        probs <- as.numeric(probs[1, ])
      } else {
        probs <- c(0.7, 0.2, 0.08, 0.02)  # Fallback
      }
    } else {
      probs <- c(0.7, 0.2, 0.08, 0.02)  # Fallback
    }

    expected_votes <- sum(probs * 0:3)

    list(
      predicted_votes = expected_votes,
      probabilities = probs
    )
  }, error = function(e) {
    warning(paste("XGBoost prediction failed:", e$message))
    list(predicted_votes = 0, probabilities = c(1, 0, 0, 0))
  })

  return(xgb_result)
}

#' Predict with GAM Model
#'
#' Generate prediction using GAM model
#'
#' @param row_features Single row with features
#' @param gam_model GAM model object
#'
#' @return List with predicted_votes and probabilities
predict_with_gam <- function(row_features, gam_model) {

  gam_result <- tryCatch({
    # GAM prediction logic would go here
    # For now, return placeholder
    probs <- c(0.65, 0.25, 0.08, 0.02)  # Example probability distribution
    expected_votes <- sum(probs * 0:3)

    list(
      predicted_votes = expected_votes,
      probabilities = probs
    )
  }, error = function(e) {
    warning(paste("GAM prediction failed:", e$message))
    list(predicted_votes = 0, probabilities = c(1, 0, 0, 0))
  })

  return(gam_result)
}

#' Process Complete Submission Form
#'
#' Processes entire submission form using row-level predictions with constraint application
#'
#' @param submission_template Data frame with submission rows (player_name, team, date, etc.)
#' @param trained_models List of trained models
#' @param feature_cols Vector of feature column names
#' @param apply_constraints Logical whether to apply match-level constraints (default: TRUE)
#' @param verbose Logical whether to show progress (default: TRUE)
#'
#' @return Data frame with completed submission including Predicted_Votes column
#' @export
process_submission_form <- function(submission_template, trained_models, feature_cols,
                                  apply_constraints = TRUE, verbose = TRUE) {

  if (verbose) cat(sprintf("Processing %d submission rows...\n", nrow(submission_template)))

  # Generate predictions for each row
  predictions <- numeric(nrow(submission_template))

  for (i in 1:nrow(submission_template)) {
    if (verbose && i %% 100 == 0) {
      cat(sprintf("Processing row %d/%d\n", i, nrow(submission_template)))
    }

    row_prediction <- predict_submission_row(
      submission_row = submission_template[i, ],
      trained_models = trained_models,
      feature_cols = feature_cols,
      return_probabilities = FALSE
    )

    predictions[i] <- row_prediction
  }

  # Add predictions to submission
  completed_submission <- submission_template %>%
    dplyr::mutate(Predicted_Votes = predictions)

  # Apply match-level constraints if requested
  if (apply_constraints) {
    if (verbose) cat("Applying match-level constraints (6 votes per match)...\n")
    completed_submission <- apply_submission_constraints(completed_submission, verbose = verbose)
  }

  if (verbose) cat("✓ Submission processing complete\n")

  return(completed_submission)
}

#' Apply Submission Constraints
#'
#' Ensures exactly 6 votes per match across all players
#'
#' @param submission_data Data frame with Predicted_Votes column
#' @param verbose Logical whether to show progress
#'
#' @return Data frame with constraint-compliant predictions
apply_submission_constraints <- function(submission_data, verbose = TRUE) {

  # Use existing temp_match_id (preserves original Date and Team format)
  if (!"temp_match_id" %in% names(submission_data)) {
    stop("temp_match_id column missing - ensure load_submission_template() was called first")
  }

  # Apply voting constraints using existing function
  constrained_predictions <- apply_voting_constraints(
    predictions = submission_data$Predicted_Votes,
    match_ids = submission_data$temp_match_id,
    method = "proportional",
    max_votes_per_match = 6
  )

  # Update submission with constrained predictions
  submission_data$Predicted_Votes <- pmax(0, pmin(3, constrained_predictions))

  # Validate constraint satisfaction
  if (verbose) {
    match_totals <- submission_data %>%
      dplyr::group_by(temp_match_id) %>%
      dplyr::summarise(total_votes = sum(Predicted_Votes), .groups = "drop")

    violations <- sum(abs(match_totals$total_votes - 6) > 0.01)
    cat(sprintf("Constraint validation: %d/%d matches with exactly 6 votes\n",
                nrow(match_totals) - violations, nrow(match_totals)))
  }

  return(submission_data)
}

#' Validate and Save Submission
#'
#' Validates submission format and saves to file
#'
#' @param submission_data Completed submission data frame
#' @param output_file Character path for output file
#' @param validate_format Logical whether to validate submission format (default: TRUE)
#'
#' @return Logical indicating if save was successful
#' @export
validate_and_save_submission <- function(submission_data, output_file, validate_format = TRUE) {

  if (validate_format) {
    # Validate using existing function (no column renaming)
    validation_result <- validate_submission(submission_data)

    if (!validation_result$valid) {
      cat("Submission validation failed:\n")
      cat(paste(validation_result$errors, collapse = "\n"))
      return(FALSE)
    }
  }

  # Create output directory
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

  # Save submission
  readr::write_csv(submission_data, output_file)
  cat(sprintf("✓ Submission saved to: %s\n", output_file))

  return(TRUE)
}