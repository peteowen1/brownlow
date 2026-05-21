#' Probability Metrics and Evaluation Functions
#'
#' This module provides functions for evaluating probability predictions
#' in multinomial classification problems, particularly for Brownlow Medal prediction.

#' Calculate Multinomial Log-Loss
#'
#' Computes the multinomial log-loss for probability predictions across multiple classes.
#' This is the primary metric for evaluating probability quality in multinomial problems.
#'
#' @param actual Vector of class labels (0, 1, 2, 3)
#' @param predicted_probs Matrix with probabilities for each class (n_obs x n_classes)
#'
#' @return Numeric value representing the log-loss (lower is better)
#' @export
multinomial_log_loss <- function(actual, predicted_probs) {
  # Ensure predicted_probs is a matrix
  if (!is.matrix(predicted_probs)) {
    predicted_probs <- as.matrix(predicted_probs)
  }

  # Clip probabilities to avoid log(0)
  predicted_probs <- pmax(predicted_probs, 1e-15)
  predicted_probs <- pmin(predicted_probs, 1 - 1e-15)

  # Convert actual to one-hot encoding
  n_classes <- ncol(predicted_probs)
  one_hot <- matrix(0, nrow = length(actual), ncol = n_classes)
  for (i in seq_along(actual)) {
    one_hot[i, actual[i] + 1] <- 1  # +1 because classes are 0-indexed but matrix is 1-indexed
  }

  # Calculate log loss
  log_loss <- -mean(rowSums(one_hot * log(predicted_probs)))
  return(log_loss)
}

#' Calculate Brier Score
#'
#' Computes the Brier score for multinomial probability predictions.
#' The Brier score measures the squared difference between predicted probabilities and actual outcomes.
#'
#' @param actual Vector of class labels (0, 1, 2, 3)
#' @param predicted_probs Matrix with probabilities for each class (n_obs x n_classes)
#'
#' @return Numeric value representing the Brier score (lower is better)
#' @export
brier_score <- function(actual, predicted_probs) {
  # Ensure predicted_probs is a matrix
  if (!is.matrix(predicted_probs)) {
    predicted_probs <- as.matrix(predicted_probs)
  }

  # Convert actual to one-hot encoding
  n_classes <- ncol(predicted_probs)
  one_hot <- matrix(0, nrow = length(actual), ncol = n_classes)
  for (i in seq_along(actual)) {
    one_hot[i, actual[i] + 1] <- 1  # +1 because classes are 0-indexed but matrix is 1-indexed
  }

  # Calculate Brier score (mean squared difference between predicted probabilities and one-hot actual)
  brier <- mean(rowSums((predicted_probs - one_hot)^2))
  return(brier)
}

#' Safely Extract Metric from Results
#'
#' Helper function to safely extract a metric from model results, returning NA if not available.
#'
#' @param metrics List or object containing metrics
#' @param metric_name Character string naming the metric to extract
#'
#' @return The metric value or NA if not available
#' @export
safe_extract <- function(metrics, metric_name) {
  if (is.null(metrics) || is.null(metrics[[metric_name]])) {
    return(NA)
  }
  return(metrics[[metric_name]])
}

#' Calculate Expected Calibration Error (ECE)
#'
#' Computes the Expected Calibration Error for probability predictions.
#' ECE measures how well predicted probabilities match actual outcomes.
#'
#' @param actual Vector of class labels
#' @param predicted_probs Matrix of predicted probabilities
#' @param n_bins Number of bins for calibration (default: 10)
#'
#' @return Numeric value representing ECE (lower is better)
#' @export
calculate_ece <- function(actual, predicted_probs, n_bins = 10) {
  if (!is.matrix(predicted_probs)) {
    predicted_probs <- as.matrix(predicted_probs)
  }

  # Get class predictions and confidence (max probability)
  class_pred <- apply(predicted_probs, 1, which.max) - 1
  confidence <- apply(predicted_probs, 1, max)

  # Create bins
  bin_boundaries <- seq(0, 1, length.out = n_bins + 1)
  bin_lowers <- bin_boundaries[-length(bin_boundaries)]
  bin_uppers <- bin_boundaries[-1]

  ece <- 0
  total_samples <- length(actual)

  for (i in 1:n_bins) {
    # Find samples in this bin
    in_bin <- (confidence > bin_lowers[i]) & (confidence <= bin_uppers[i])
    prop_in_bin <- sum(in_bin) / total_samples

    if (sum(in_bin) > 0) {
      # Calculate accuracy and confidence for this bin
      accuracy_in_bin <- mean(actual[in_bin] == class_pred[in_bin])
      avg_confidence_in_bin <- mean(confidence[in_bin])

      # Add to ECE
      ece <- ece + abs(avg_confidence_in_bin - accuracy_in_bin) * prop_in_bin
    }
  }

  return(ece)
}

#' Comprehensive Probability Evaluation
#'
#' Evaluates probability predictions using multiple metrics including log-loss, Brier score, and ECE.
#'
#' @param actual Vector of class labels
#' @param predicted_probs Matrix of predicted probabilities
#' @param class_names Optional vector of class names
#'
#' @return List containing all probability evaluation metrics
#' @export
evaluate_probability_quality <- function(actual, predicted_probs, class_names = NULL) {
  # Calculate core metrics
  log_loss <- multinomial_log_loss(actual, predicted_probs)
  brier <- brier_score(actual, predicted_probs)
  ece <- calculate_ece(actual, predicted_probs)

  # Class predictions for additional metrics
  class_pred <- apply(predicted_probs, 1, which.max) - 1
  accuracy <- mean(actual == class_pred)

  # Per-class metrics
  if (is.null(class_names)) {
    class_names <- paste0("Class_", 0:(ncol(predicted_probs) - 1))
  }

  per_class_brier <- numeric(ncol(predicted_probs))
  for (i in 1:ncol(predicted_probs)) {
    class_actual <- as.numeric(actual == (i - 1))
    class_prob <- predicted_probs[, i]
    per_class_brier[i] <- mean((class_prob - class_actual)^2)
  }
  names(per_class_brier) <- class_names

  return(list(
    log_loss = log_loss,
    brier_score = brier,
    ece = ece,
    accuracy = accuracy,
    per_class_brier = per_class_brier,
    n_samples = length(actual),
    n_classes = ncol(predicted_probs)
  ))
}