#' Standardized Model Results Structure
#'
#' This file defines the standard structure that all model scripts (03, 04, 05) should use
#' for saving results, ensuring consistency across XGBoost, GAM, and Ensemble outputs.

#' Create Standardized Model Results
#'
#' Creates a standardized results object that all model scripts should use
#'
#' @param model_name Character name of the model ("xgboost", "gam", "ensemble")
#' @param model_object The trained model object (optional, can be large)
#' @param train_pred Vector of training predictions
#' @param test_pred Vector of test predictions
#' @param val_pred Vector of validation predictions (optional)
#' @param train_pred_probs Matrix of training prediction probabilities
#' @param test_pred_probs Matrix of test prediction probabilities
#' @param val_pred_probs Matrix of validation prediction probabilities (optional)
#' @param constrained_pred Vector of constrained predictions (optional)
#' @param feature_cols Character vector of feature column names
#' @param train_metrics List of training performance metrics
#' @param test_metrics List of test performance metrics
#' @param val_metrics List of validation performance metrics (optional)
#' @param hyperparameters List of model hyperparameters (optional)
#' @param additional_info Named list of any model-specific additional information
#'
#' @return Standardized results list
#' @export
create_standardized_results <- function(
    model_name,
    model_object = NULL,
    train_pred = NULL,
    test_pred = NULL,
    val_pred = NULL,
    train_pred_probs = NULL,
    test_pred_probs = NULL,
    val_pred_probs = NULL,
    constrained_pred = NULL,
    feature_cols = NULL,
    train_metrics = NULL,
    test_metrics = NULL,
    val_metrics = NULL,
    hyperparameters = NULL,
    additional_info = list()
) {

  # Core structure - always present
  results <- list(
    # Metadata
    model_name = model_name,
    model_type = if (!is.null(train_pred_probs) || !is.null(test_pred_probs)) "multinomial" else "regression",
    creation_date = Sys.time(),
    feature_cols = feature_cols,

    # Predictions - point estimates
    predictions = list(
      train_pred = train_pred,
      test_pred = test_pred,
      val_pred = val_pred,
      constrained_pred = constrained_pred
    ),

    # Predictions - probability matrices (for multinomial)
    probabilities = list(
      train_pred_probs = train_pred_probs,
      test_pred_probs = test_pred_probs,
      val_pred_probs = val_pred_probs
    ),

    # Performance metrics
    metrics = list(
      train_metrics = train_metrics,
      test_metrics = test_metrics,
      val_metrics = val_metrics
    ),

    # Model configuration
    config = list(
      hyperparameters = hyperparameters,
      model_object = model_object  # Optional, can be NULL to save space
    ),

    # Additional model-specific information
    additional = additional_info
  )

  # Add class for method dispatch
  class(results) <- c("brownlow_model_results", "list")

  return(results)
}

#' Save Standardized Model Results
#'
#' Saves model results using the standard file naming convention
#'
#' @param results Standardized results object from create_standardized_results()
#' @param save_model Logical whether to include the model object (can be large)
#'
#' @export
save_standardized_results <- function(results, save_model = FALSE) {

  if (!save_model && !is.null(results$config$model_object)) {
    # Create version without model object to save space
    results_light <- results
    results_light$config$model_object <- NULL
    results_light$additional$model_object_saved <- FALSE
  } else {
    results_light <- results
    results_light$additional$model_object_saved <- TRUE
  }

  # Save to standard location
  filename <- sprintf("results/%s_results.rds", results$model_name)
  saveRDS(results_light, filename)

  cat(sprintf("✓ Saved %s results to %s\n", results$model_name, filename))

  return(invisible(filename))
}

#' Load Standardized Model Results
#'
#' Loads model results and validates structure
#'
#' @param model_name Character name of model to load
#'
#' @return Standardized results object
#' @export
load_standardized_results <- function(model_name) {

  filename <- sprintf("results/%s_results.rds", model_name)

  if (!file.exists(filename)) {
    stop(sprintf("Results file not found: %s", filename))
  }

  results <- readRDS(filename)

  # Validate structure
  if (!inherits(results, "brownlow_model_results")) {
    warning(sprintf("Results for %s may not be in standardized format", model_name))
  }

  return(results)
}

#' Extract Predictions from Standardized Results
#'
#' Helper function to extract predictions in the format expected by simulation
#'
#' @param results Standardized results object
#' @param data_split Character which data split to extract ("train", "test", "val")
#'
#' @return List with predictions and probabilities
#' @export
extract_predictions_for_simulation <- function(results, data_split = "test") {

  pred_key <- paste0(data_split, "_pred")
  prob_key <- paste0(data_split, "_pred_probs")

  list(
    predictions = results$predictions[[pred_key]],
    probabilities = results$probabilities[[prob_key]],
    feature_cols = results$feature_cols,
    model_name = results$model_name
  )
}

#' Print Method for Standardized Results
#'
#' @param x Standardized results object
#' @param ... Additional arguments
#'
#' @export
print.brownlow_model_results <- function(x, ...) {

  cat(sprintf("Brownlow Model Results: %s\n", toupper(x$model_name)))
  cat(sprintf("Type: %s\n", x$model_type))
  cat(sprintf("Created: %s\n", x$creation_date))
  cat(sprintf("Features: %d\n", length(x$feature_cols)))

  # Show available data
  pred_available <- sapply(x$predictions, function(p) !is.null(p))
  prob_available <- sapply(x$probabilities, function(p) !is.null(p))

  cat("\nAvailable Predictions:\n")
  for (split in c("train", "test", "val")) {
    pred_name <- paste0(split, "_pred")
    prob_name <- paste0(split, "_pred_probs")
    if (pred_available[pred_name]) {
      n_obs <- length(x$predictions[[pred_name]])
      prob_dims <- if (prob_available[prob_name]) sprintf(" + probs(%dx%d)",
                                                         nrow(x$probabilities[[prob_name]]),
                                                         ncol(x$probabilities[[prob_name]])) else ""
      cat(sprintf("  %s: %d observations%s\n", split, n_obs, prob_dims))
    }
  }

  # Show metrics
  metrics_available <- sapply(x$metrics, function(m) !is.null(m))
  if (any(metrics_available)) {
    cat("\nMetrics Available:\n")
    for (split in names(x$metrics)[metrics_available]) {
      cat(sprintf("  %s\n", split))
    }
  }

  invisible(x)
}