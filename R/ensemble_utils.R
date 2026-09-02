#' Ensemble Prediction and Optimization Utilities
#'
#' This module provides utility functions for optimizing ensemble weights,
#' combining model predictions, and generating final ensemble predictions.

#' Optimize Ensemble Weights
#'
#' Finds optimal weights for ensemble combination using cross-validation
#' and multinomial log-loss optimization.
#'
#' @param model_results List containing results from individual models
#' @param optimization_metric Metric to optimize ("log_loss", "brier", "accuracy")
#' @param weight_constraints List with min/max weight constraints
#' @param verbose Logical, whether to show progress messages
#'
#' @section Do not trust this function's output on production predictions:
#' It fits the blend on whatever predictions `model_results` carries. In this
#' pipeline stage `05` retrains the production model on 2015-2024 and then
#' writes probabilities for every row, so the 2023-24 rows this function
#' selects on are **in-sample for XGBoost**. The contamination is asymmetric —
#' a several-hundred-round booster memorises its training rows while a shrunk
#' GAM does not — so selection runs monotonically to the XGBoost cap:
#'
#' | model alone | 2023-24 (in-sample) | 2025 (out-of-sample) |
#' |---|---|---|
#' | XGBoost | 0.33565 | 0.35771 |
#' | GAM | 0.36177 | 0.35805 |
#'
#' Selecting on honest walk-forward predictions instead gives `w_xgb = 0.40`
#' against this function's 1.00, worth 0.00055 RMSE on 2025. See
#' POSTMORTEM-2025.md Sec 5j and `analysis/ensemble_weight_walkforward.R`.
#' `06_ensemble_predictions.R` therefore ignores this result by default.
#'
#' Two further cautions. The `min`/`max` constraint is **load-bearing** while
#' the input stays contaminated — it is the only thing stopping the search
#' reaching pure XGBoost — so do not relax it as an "arbitrary limit". And
#' `optimization_metric = "log_loss"` is not what the competition scores; it
#' scores RMSE (Sec 5k).
#'
#' @return List containing optimal weights and performance metrics
#' @export
optimize_ensemble_weights <- function(model_results,
                                    optimization_metric = "log_loss",
                                    weight_constraints = list(min = 0.1, max = 0.9),
                                    verbose = TRUE) {
  if (verbose) cat("Optimizing ensemble weights...")

  # Loud rather than silent: this returned a weight that shipped for a year
  # before anyone asked what rows it was fitted on.
  warning("optimize_ensemble_weights() fits the blend on whatever predictions it is ",
          "given. If those came from a model trained on the same rows, the result is ",
          "contaminated and will favour the higher-capacity model. Prefer the ",
          "walk-forward weight (POSTMORTEM Sec 5j).", call. = FALSE)

  # Extract test predictions and actual values for optimization
  models_with_probs <- names(model_results)[sapply(model_results, function(x) {
    # Check standardized structure first
    if (!is.null(x$probabilities$test_pred_probs)) return(TRUE)
    # Fallback to old structure
    if (!is.null(x$test_pred_probs)) return(TRUE)
    return(FALSE)
  })]

  if (length(models_with_probs) < 2) {
    stop("Need at least 2 models with probability predictions for ensemble")
  }

  # Get actual values from test data (use first available model)
  actual_values <- NULL
  for (model_name in models_with_probs) {
    if (verbose) cat(sprintf("\nTrying to find actual values in model: %s\n", model_name))

    # Try to get from test data in additional (correct standardized structure)
    if (!is.null(model_results[[model_name]]$additional)) {
      if (verbose) cat("  Found additional section\n")
      if (!is.null(model_results[[model_name]]$additional$test_data)) {
        if (verbose) cat("  Found test_data in additional\n")
        test_data <- model_results[[model_name]]$additional$test_data
        if ("brownlow_votes" %in% names(test_data)) {
          actual_values <- test_data$brownlow_votes
          if (verbose) cat(sprintf("  ✓ Found brownlow_votes in test_data (%d values)\n", length(actual_values)))
          break
        } else {
          if (verbose) cat("  No brownlow_votes column in test_data\n")
        }
      } else {
        if (verbose) cat("  No test_data in additional\n")
      }
    } else {
      if (verbose) cat("  No additional section\n")
    }

    # Try old structure test_data
    if (!is.null(model_results[[model_name]]$test_data)) {
      if (verbose) cat("  Found direct test_data\n")
      test_data <- model_results[[model_name]]$test_data
      if ("brownlow_votes" %in% names(test_data)) {
        actual_values <- test_data$brownlow_votes
        if (verbose) cat(sprintf("  ✓ Found brownlow_votes in direct test_data (%d values)\n", length(actual_values)))
        break
      } else {
        if (verbose) cat("  No brownlow_votes column in direct test_data\n")
      }
    } else {
      if (verbose) cat("  No direct test_data\n")
    }

    # Try metrics structure
    if (!is.null(model_results[[model_name]]$metrics$test_metrics$actual)) {
      actual_values <- model_results[[model_name]]$metrics$test_metrics$actual
      if (verbose) cat(sprintf("  ✓ Found actual in metrics (%d values)\n", length(actual_values)))
      break
    }

    # Fallback to old metrics structure
    if (!is.null(model_results[[model_name]]$test_metrics$actual)) {
      actual_values <- model_results[[model_name]]$test_metrics$actual
      if (verbose) cat(sprintf("  ✓ Found actual in old metrics (%d values)\n", length(actual_values)))
      break
    }

    if (verbose) cat(sprintf("  Available top-level fields in %s: %s\n", model_name, paste(names(model_results[[model_name]]), collapse = ", ")))
  }

  if (is.null(actual_values)) {
    stop("Could not find actual values for optimization. Please run final_debug.R to see the model structure.")
  }

  # Objective function for weight optimization
  objective_function <- function(weights) {
    # Ensure weights sum to 1
    weights <- weights / sum(weights)

    # Combine probability matrices
    ensemble_probs <- combine_probability_matrices(model_results, weights, models_with_probs)

    # Calculate optimization metric
    if (optimization_metric == "log_loss") {
      return(multinomial_log_loss(actual_values, ensemble_probs))
    } else if (optimization_metric == "brier") {
      return(brier_score(actual_values, ensemble_probs))
    } else if (optimization_metric == "accuracy") {
      pred_classes <- apply(ensemble_probs, 1, which.max) - 1
      return(-mean(actual_values == pred_classes))  # Negative for minimization
    }
  }

  # Grid search for optimal weights
  best_weights <- find_optimal_weights(
    objective_function = objective_function,
    n_models = length(models_with_probs),
    weight_constraints = weight_constraints,
    verbose = verbose
  )

  # Calculate final metrics with optimal weights
  final_ensemble_probs <- combine_probability_matrices(model_results, best_weights, models_with_probs)
  final_metrics <- evaluate_probability_quality(actual_values, final_ensemble_probs)

  if (verbose) cat(" ✓\n")

  # Create weight summary
  weight_summary <- setNames(best_weights, models_with_probs)

  return(list(
    weights = weight_summary,
    ensemble_probs = final_ensemble_probs,
    metrics = final_metrics,
    optimization_metric = optimization_metric,
    models_used = models_with_probs
  ))
}

#' Combine Probability Matrices
#'
#' Combines probability matrices from multiple models using specified weights.
#'
#' @param model_results List containing model results
#' @param weights Numeric vector of weights (must sum to 1)
#' @param model_names Character vector of model names to combine
#'
#' @return Combined probability matrix
#' @export
combine_probability_matrices <- function(model_results, weights, model_names) {
  # Normalize weights
  weights <- weights / sum(weights)

  # Initialize ensemble probabilities
  ensemble_probs <- NULL

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    weight <- weights[i]

    # Get probabilities from standardized structure
    model_probs <- model_results[[model_name]]$probabilities$test_pred_probs
    if (is.null(model_probs)) {
      # Fallback to old structure
      model_probs <- model_results[[model_name]]$test_pred_probs
    }

    if (is.null(ensemble_probs)) {
      ensemble_probs <- weight * model_probs
    } else {
      ensemble_probs <- ensemble_probs + weight * model_probs
    }
  }

  return(ensemble_probs)
}

#' Find Optimal Weights
#'
#' Uses grid search to find optimal ensemble weights.
#'
#' @param objective_function Function to optimize
#' @param n_models Number of models in ensemble
#' @param weight_constraints List with min/max constraints
#' @param grid_size Grid resolution for search (default: 20)
#' @param verbose Logical, whether to show progress
#'
#' @return Optimal weight vector
#' @export
find_optimal_weights <- function(objective_function,
                                n_models,
                                weight_constraints = list(min = 0.1, max = 0.9),
                                grid_size = 20,
                                verbose = TRUE) {
  if (n_models == 2) {
    # For 2 models, search over single parameter
    weight_range <- seq(weight_constraints$min, weight_constraints$max, length.out = grid_size)
    best_score <- Inf
    best_weights <- c(0.5, 0.5)

    for (w1 in weight_range) {
      w2 <- 1 - w1
      if (w2 >= weight_constraints$min && w2 <= weight_constraints$max) {
        weights <- c(w1, w2)
        score <- objective_function(weights)

        if (score < best_score) {
          best_score <- score
          best_weights <- weights
        }
      }
    }

    return(best_weights)

  } else {
    # For 3+ models, use random search with constraints
    n_iterations <- grid_size^2
    best_score <- Inf
    best_weights <- rep(1/n_models, n_models)

    for (i in 1:n_iterations) {
      # Generate random weights with constraints
      weights <- runif(n_models, weight_constraints$min, weight_constraints$max)
      weights <- weights / sum(weights)

      score <- objective_function(weights)

      if (score < best_score) {
        best_score <- score
        best_weights <- weights
      }
    }

    return(best_weights)
  }
}

#' Generate Ensemble Predictions
#'
#' Creates ensemble predictions using optimized weights and individual model predictions.
#'
#' @param model_results List containing individual model results
#' @param ensemble_weights Optimized ensemble weights
#' @param prediction_data Data to make predictions on
#' @param verbose Logical, whether to show progress messages
#'
#' @return List containing ensemble predictions and metadata
#' @export
generate_ensemble_predictions <- function(model_results,
                                        ensemble_weights,
                                        prediction_data = NULL,
                                        verbose = TRUE) {
  if (verbose) cat("Generating ensemble predictions...")

  # Get available models
  available_models <- names(ensemble_weights$weights)

  # Combine test probabilities
  ensemble_test_probs <- combine_probability_matrices(
    model_results,
    ensemble_weights$weights,
    available_models
  )

  # Generate class predictions from probabilities
  ensemble_test_pred <- apply(ensemble_test_probs, 1, which.max) - 1

  # Combine training probabilities if available
  ensemble_train_probs <- NULL
  ensemble_train_pred <- NULL

  train_probs_available <- all(sapply(available_models, function(m) {
    # Check standardized structure first
    if (!is.null(model_results[[m]]$probabilities$train_pred_probs)) return(TRUE)
    # Fallback to old structure
    if (!is.null(model_results[[m]]$train_pred_probs)) return(TRUE)
    return(FALSE)
  }))

  if (train_probs_available) {
    ensemble_train_probs <- combine_probability_matrices_train(model_results, ensemble_weights$weights, available_models)
    ensemble_train_pred <- apply(ensemble_train_probs, 1, which.max) - 1
  }

  if (verbose) cat(" ✓\n")

  return(list(
    test_pred = ensemble_test_pred,
    test_pred_probs = ensemble_test_probs,
    train_pred = ensemble_train_pred,
    train_pred_probs = ensemble_train_probs,
    weights = ensemble_weights$weights,
    models_used = available_models,
    ensemble_metrics = ensemble_weights$metrics
  ))
}

#' Combine Training Probability Matrices
#'
#' Helper function to combine training probability matrices.
#'
#' @param model_results List containing model results
#' @param weights Numeric vector of weights
#' @param model_names Character vector of model names
#'
#' @return Combined training probability matrix
#' @export
combine_probability_matrices_train <- function(model_results, weights, model_names) {
  weights <- weights / sum(weights)
  ensemble_probs <- NULL

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    weight <- weights[i]

    # Get probabilities from standardized structure
    model_probs <- model_results[[model_name]]$probabilities$train_pred_probs
    if (is.null(model_probs)) {
      # Fallback to old structure
      model_probs <- model_results[[model_name]]$train_pred_probs
    }

    if (is.null(ensemble_probs)) {
      ensemble_probs <- weight * model_probs
    } else {
      ensemble_probs <- ensemble_probs + weight * model_probs
    }
  }

  return(ensemble_probs)
}

#' Create Ensemble Performance Summary
#'
#' Creates a comprehensive summary of ensemble performance compared to individual models.
#'
#' @param model_results List containing individual model results
#' @param ensemble_results Ensemble prediction results
#' @param actual_values Actual values for evaluation
#' @param save_summary Logical, whether to save summary to file
#'
#' @return Data frame with performance comparison
#' @export
create_ensemble_performance_summary <- function(model_results,
                                              ensemble_results,
                                              actual_values,
                                              save_summary = TRUE) {
  # Individual model performance
  model_performance <- data.frame()

  for (model_name in names(model_results)) {
    # Check standardized structure first
    test_metrics <- model_results[[model_name]]$metrics$test_metrics
    if (is.null(test_metrics)) {
      # Fallback to old structure
      test_metrics <- model_results[[model_name]]$test_metrics
    }

    if (!is.null(test_metrics)) {
      perf_row <- data.frame(
        model = model_name,
        log_loss = safe_extract(test_metrics, "log_loss"),
        accuracy = safe_extract(test_metrics, "overall_accuracy"),
        brier_score = safe_extract(test_metrics, "brier_score"),
        weight = ifelse(model_name %in% names(ensemble_results$weights),
                       ensemble_results$weights[[model_name]], 0),
        stringsAsFactors = FALSE
      )
      model_performance <- rbind(model_performance, perf_row)
    }
  }

  # Ensemble performance
  ensemble_perf <- data.frame(
    model = "Ensemble",
    log_loss = ensemble_results$ensemble_metrics$log_loss,
    accuracy = ensemble_results$ensemble_metrics$accuracy,
    brier_score = ensemble_results$ensemble_metrics$brier_score,
    weight = 1.0,
    stringsAsFactors = FALSE
  )

  performance_summary <- rbind(model_performance, ensemble_perf) %>%
    dplyr::arrange(log_loss)

  if (save_summary) {
    readr::write_csv(performance_summary, "results/ensemble_performance_summary.csv")
  }

  return(performance_summary)
}

#' Load ensemble models for optimization
#'
#' @param model_dir Directory containing model results (default: "results")
#' @param verbose Logical indicating whether to print progress messages
#' @return List of model results ready for ensemble optimization
#' @export
load_ensemble_models <- function(model_dir = "results", verbose = TRUE) {

  if (verbose) cat("Loading models...")

  # Check if models exist
  required_files <- c(
    file.path(model_dir, "xgboost_results.rds"),
    file.path(model_dir, "gam_results.rds")
  )

  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    cat("Missing required files:\n")
    cat(paste(missing_files, collapse = "\n"))
    stop("Please run the individual model scripts first.")
  }

  # Load results
  xgboost_results <- readRDS(file.path(model_dir, "xgboost_results.rds"))
  gam_results <- readRDS(file.path(model_dir, "gam_results.rds"))

  model_results <- list(
    xgboost = xgboost_results,
    gam = gam_results
  )

  # Verify probability predictions are available (check standardized structure)
  available_models <- names(model_results)[sapply(model_results, function(x) {
    # Check for standardized structure first
    if (!is.null(x$probabilities$test_pred_probs)) {
      return(TRUE)
    }
    # Fallback to old structure
    if (!is.null(x$test_pred_probs)) {
      return(TRUE)
    }
    return(FALSE)
  })]

  if (length(available_models) < 2) {
    stop("Need at least 2 models with probability predictions for ensemble")
  }

  if (verbose) {
    cat(sprintf(" ✓ %s models ready for ensemble\n", paste(available_models, collapse = ", ")))
  }

  return(model_results)
}

#' Create ensemble submission template
#'
#' @param model_results List of model results
#' @param ensemble_predictions Ensemble prediction results
#' @param constrained_predictions Constrained prediction vector
#' @param test_data Optional test data frame (if NULL, will try to get from model_results)
#' @param output_file Path to save submission file
#' @param verbose Logical indicating whether to print progress messages
#' @return Data frame with submission template
#' @export
create_ensemble_submission <- function(model_results, ensemble_predictions, constrained_predictions,
                                     test_data = NULL, output_file = "submissions/ensemble_predictions.csv", verbose = TRUE) {

  if (verbose) cat("Creating submission template...")

  # Use provided test_data, or fall back to getting it from model results
  if (is.null(test_data)) {
    test_data <- model_results[[names(model_results)[1]]]$additional$test_data
    if (is.null(test_data)) {
      # Fallback to old structure
      test_data <- model_results[[names(model_results)[1]]]$test_data
    }
  }

  # Generate submission format
  if ("date" %in% names(test_data)) {
    submission_template <- test_data %>%
      dplyr::select(player_name, team, date, dplyr::any_of("match_id")) %>%
      dplyr::mutate(
        predicted_votes = constrained_predictions,
        ensemble_prob_0 = ensemble_predictions$test_pred_probs[, 1],
        ensemble_prob_1 = ensemble_predictions$test_pred_probs[, 2],
        ensemble_prob_2 = ensemble_predictions$test_pred_probs[, 3],
        ensemble_prob_3 = ensemble_predictions$test_pred_probs[, 4]
      ) %>%
      dplyr::arrange(date, team, player_name)
  } else {
    # Fallback if date not available
    submission_template <- test_data %>%
      dplyr::select(player_name, team, dplyr::any_of(c("match_id", "season", "match_round"))) %>%
      dplyr::mutate(predicted_votes = constrained_predictions) %>%
      dplyr::arrange(player_name)
  }

  # Validate submission
  validation_result <- validate_submission(submission_template)

  if (validation_result$valid) {
    # Create output directory
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(submission_template, output_file)
    if (verbose) cat(" ✓ Submission saved\n")
  } else {
    if (verbose) {
      cat("Submission template validation errors:\n")
      cat(paste(validation_result$errors, collapse = "\n"))
    }
  }

  return(submission_template)
}

#' Save ensemble results to files
#'
#' @param ensemble_weights List with optimized weights and metrics
#' @param ensemble_predictions List with ensemble predictions
#' @param constrained_ensemble Constrained prediction results
#' @param performance_summary Performance comparison data frame
#' @param submission_template Competition submission template (optional)
#' @param model_results Original model results
#' @param save_dir Directory to save results (default: "results")
#' @param verbose Logical indicating whether to print progress messages
#' @return List with saved file paths and summary statistics
#' @export
save_ensemble_results <- function(ensemble_weights, ensemble_predictions, constrained_ensemble,
                                performance_summary, model_results, submission_template = NULL,
                                save_dir = "results", verbose = TRUE) {

  if (verbose) cat("Saving ensemble results...")

  # Create save directory
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

  # Get actual values for improvement calculation
  first_model <- model_results[[names(model_results)[1]]]

  # Get test data from standardized structure
  test_data <- first_model$additional$test_data
  if (is.null(test_data)) {
    # Fallback to old structure
    test_data <- first_model$test_data
  }

  # Get target column name
  target_col <- first_model$target_col
  if (is.null(target_col)) {
    target_col <- "brownlow_votes"  # Default fallback
  }

  actual_values <- test_data[[target_col]]

  # Calculate constraint improvement
  constraint_improvement <- mean(constrained_ensemble$constrained_predictions == actual_values) -
                           mean(ensemble_predictions$test_pred == actual_values)

  # Create comprehensive ensemble results
  ensemble_results_final <- list(
    model_weights = ensemble_weights$weights,
    ensemble_metrics = ensemble_weights$metrics,
    performance_summary = performance_summary,
    test_predictions = ensemble_predictions$test_pred,
    test_probabilities = ensemble_predictions$test_pred_probs,
    constrained_predictions = constrained_ensemble$constrained_predictions,
    submission_template = submission_template,
    optimization_details = list(
      metric_used = "log_loss",
      models_combined = names(model_results),
      constraint_improvement = constraint_improvement
    ),
    creation_date = Sys.time()
  )

  # Save ensemble results
  ensemble_file <- file.path(save_dir, "ensemble_results.rds")
  saveRDS(ensemble_results_final, ensemble_file)

  # Save performance comparison
  performance_file <- file.path(save_dir, "ensemble_performance_comparison.csv")
  readr::write_csv(performance_summary, performance_file)

  # Calculate improvement metrics
  best_individual_loss <- min(performance_summary$log_loss[performance_summary$model != "Ensemble"], na.rm = TRUE)
  ensemble_improvement <- best_individual_loss - ensemble_weights$metrics$log_loss

  if (verbose) {
    cat(sprintf(" ✓ Ensemble log-loss: %.3f, Best individual: %.3f, Improvement: %.4f\n",
               ensemble_weights$metrics$log_loss,
               best_individual_loss,
               ensemble_improvement))
  }

  return(list(
    ensemble_file = ensemble_file,
    performance_file = performance_file,
    ensemble_results = ensemble_results_final,
    improvement = ensemble_improvement
  ))
}