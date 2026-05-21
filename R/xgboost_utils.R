# XGBoost Utility Functions for Brownlow Medal Prediction
# Functions for XGBoost-specific analysis, probability analysis, and diagnostics

#' Analyze XGBoost Multinomial Probabilities
#'
#' Performs comprehensive probability analysis for multinomial XGBoost models
#'
#' @param pred_matrix Matrix of prediction probabilities (n_obs x n_classes)
#' @param actual Vector of actual class labels
#' @param class_names Vector of class names (default: c("0", "1", "2", "3"))
#' @return List containing probability analysis results
#' @export
analyze_xgboost_probabilities <- function(pred_matrix, actual, class_names = c("0", "1", "2", "3")) {
  
  # Ensure pred_matrix has proper column names
  if (is.null(colnames(pred_matrix))) {
    colnames(pred_matrix) <- paste0("Prob_", class_names, "_votes")
  }
  
  n_classes <- ncol(pred_matrix)
  
  # Calculate Brier Score for each class
  actual_one_hot <- model.matrix(~ factor(actual, levels = 0:(n_classes-1)) - 1)
  colnames(actual_one_hot) <- paste0("Actual_", class_names, "_votes")
  
  brier_scores <- colMeans((pred_matrix - actual_one_hot)^2)
  overall_brier <- mean(brier_scores)
  
  # Get class predictions
  pred_class <- apply(pred_matrix, 1, which.max) - 1
  
  # Create calibration data
  calibration_data <- data.frame(
    actual = actual,
    pred_matrix
  ) %>%
    dplyr::mutate(
      max_prob = apply(pred_matrix, 1, max),
      pred_class = pred_class,
      correct = actual == pred_class
    )
  
  # Calculate Expected Calibration Error (ECE)
  calibration_bins <- calibration_data %>%
    dplyr::mutate(
      prob_bin = cut(max_prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
    ) %>%
    dplyr::group_by(prob_bin) %>%
    dplyr::summarise(
      n = n(),
      mean_confidence = mean(max_prob),
      accuracy = mean(correct),
      .groups = "drop"
    ) %>%
    dplyr::filter(n >= 10)  # Only bins with sufficient data
  
  ece <- if (nrow(calibration_bins) > 0) {
    calibration_bins %>%
      dplyr::mutate(weight = n / sum(n)) %>%
      dplyr::summarise(ece = sum(weight * abs(mean_confidence - accuracy))) %>%
      dplyr::pull(ece)
  } else {
    NA
  }
  
  # Log-likelihood (if possible)
  log_loss <- tryCatch({
    -mean(log(pmax(pred_matrix[cbind(seq_along(actual), actual + 1)], 1e-15)))
  }, error = function(e) NA)
  
  return(list(
    brier_scores = brier_scores,
    overall_brier = overall_brier,
    calibration_data = calibration_data,
    calibration_bins = calibration_bins,
    ece = ece,
    log_loss = log_loss,
    pred_class = pred_class
  ))
}

#' Create Calibration and Probability Plots
#'
#' Generates probability distribution and calibration reliability plots
#'
#' @param prob_analysis List from analyze_xgboost_probabilities()
#' @param save_plots Logical. Whether to save plots to files
#' @param plot_dir Directory to save plots (default: "plots/xgboost/")
#' @return List of ggplot objects
#' @export
create_calibration_plots <- function(prob_analysis, save_plots = TRUE, plot_dir = "plots/xgboost/") {
  
  if (save_plots) {
    dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  plots <- list()
  
  # Probability distribution plot
  prob_cols <- grep("Prob_.*_votes", names(prob_analysis$calibration_data), value = TRUE)
  
  if (length(prob_cols) > 0) {
    plots$prob_distribution <- prob_analysis$calibration_data %>%
      dplyr::select(actual, dplyr::all_of(prob_cols)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(prob_cols),
                   names_to = "Vote_Type",
                   values_to = "Probability") %>%
      dplyr::mutate(
        Vote_Type = stringr::str_extract(Vote_Type, "\\d+"),
        Vote_Type = paste0(Vote_Type, " votes"),
        Actual_Match = (as.numeric(stringr::str_extract(Vote_Type, "\\d+")) == actual)
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = Probability, fill = Actual_Match)) +
      ggplot2::geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      ggplot2::facet_wrap(~Vote_Type) +
      ggplot2::labs(title = "Predicted Probability Distributions by Vote Type",
           subtitle = "Blue = Correct predictions, Red = Incorrect predictions",
           x = "Predicted Probability", y = "Count",
           fill = "Correct Prediction") +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"))
    
    if (save_plots) {
      ggplot2::ggsave(file.path(plot_dir, "probability_distributions.png"), plots$prob_distribution,
             width = 12, height = 8)
    }
  }
  
  # Calibration reliability diagram
  if (nrow(prob_analysis$calibration_bins) > 0) {
    plots$calibration <- prob_analysis$calibration_bins %>%
      ggplot2::ggplot(ggplot2::aes(x = mean_confidence, y = accuracy)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.7, color = "steelblue") +
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "coral") +
      ggplot2::labs(title = "Probability Calibration Reliability Diagram",
           subtitle = "Points closer to diagonal line indicate better calibration",
           x = "Mean Predicted Probability", y = "Observed Accuracy",
           size = "Count") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_minimal()
    
    if (save_plots) {
      ggplot2::ggsave(file.path(plot_dir, "calibration_plot.png"), plots$calibration,
             width = 8, height = 6)
    }
  }
  
  return(plots)
}

#' Evaluate Voting Constraints for XGBoost
#'
#' Comprehensive evaluation of voting constraint application
#'
#' @param test_data Test dataset
#' @param predictions Vector of predictions
#' @param actual Vector of actual values
#' @param match_id_cols Vector of column names to create match IDs
#' @param is_multinomial Logical. Whether model is multinomial
#' @return List with constraint evaluation results
#' @export
evaluate_voting_constraints <- function(test_data, predictions, actual, 
                                       match_id_cols = c("date", "team"), 
                                       is_multinomial = FALSE) {
  
  # Create match IDs
  if (length(match_id_cols) == 1) {
    match_ids <- test_data[[match_id_cols[1]]]
  } else {
    match_ids <- do.call(paste, c(test_data[match_id_cols], sep = "_"))
  }
  
  # Apply constraints
  if (is_multinomial) {
    # For multinomial, predictions are already constrained to valid vote values
    constrained_pred <- predictions
    cat("Multinomial predictions - using class predictions as constraints\n")
  } else {
    constrained_pred <- apply_voting_constraints(
      predictions = predictions,
      match_ids = match_ids,
      method = "proportional"
    )
  }
  
  # Evaluate constrained predictions
  if (is_multinomial) {
    constrained_metrics <- evaluate_model(
      actual = actual,
      predicted = constrained_pred,
      model_type = "multinomial"
    )
  } else {
    constrained_metrics <- evaluate_model(
      actual = actual,
      predicted = constrained_pred,
      model_type = "regression"
    )
  }
  
  # Verify constraint violations
  constraint_check <- test_data %>%
    dplyr::select(dplyr::all_of(match_id_cols[1]), team) %>%
    dplyr::mutate(Constrained_Votes = constrained_pred) %>%
    dplyr::group_by(.data[[match_id_cols[1]]]) %>%
    dplyr::summarise(
      Total_Votes = sum(Constrained_Votes),
      N_Teams = dplyr::n_distinct(team),
      Expected_Votes = (N_Teams / 2) * 6,
      Constraint_Error = abs(Total_Votes - Expected_Votes),
      .groups = "drop"
    )
  
  max_constraint_error <- max(constraint_check$Constraint_Error, na.rm = TRUE)
  
  return(list(
    constrained_predictions = constrained_pred,
    constrained_metrics = constrained_metrics,
    constraint_check = constraint_check,
    max_constraint_error = max_constraint_error
  ))
}

#' Analyze XGBoost Predictions by Player/Team
#'
#' Detailed analysis of prediction accuracy by player, team, and other factors
#'
#' @param test_data Test dataset
#' @param predictions Vector of predictions
#' @param actual Vector of actual values
#' @param is_multinomial Logical. Whether model is multinomial
#' @return List with prediction analysis results
#' @export
analyze_xgboost_predictions <- function(test_data, predictions, actual, is_multinomial = FALSE) {
  
  # Create results dataframe
  analysis_cols <- intersect(c("date", "season", "match_round", "team", "player_name"), names(test_data))
  
  test_results <- test_data %>%
    dplyr::select(dplyr::all_of(analysis_cols), dplyr::all_of(names(test_data)[names(test_data) %in% c("brownlow_votes")])) %>%
    dplyr::mutate(
      Actual_Votes = actual,
      Predicted_Votes = predictions,
      Prediction_Error = abs(Actual_Votes - Predicted_Votes),
      Correct_Prediction = if (is_multinomial) {
        Actual_Votes == Predicted_Votes
      } else {
        abs(Actual_Votes - round(Predicted_Votes)) < 0.5
      }
    )
  
  # Best and worst predictions
  prediction_cols <- c("player_name", "team", "Actual_Votes", "Predicted_Votes", "Prediction_Error")
  if ("date" %in% names(test_results)) {
    prediction_cols <- c("player_name", "team", "date", "Actual_Votes", "Predicted_Votes", "Prediction_Error")
  }
  
  best_predictions <- test_results %>%
    dplyr::arrange(Prediction_Error) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::select(dplyr::all_of(intersect(prediction_cols, names(test_results))))
  
  worst_predictions <- test_results %>%
    dplyr::arrange(dplyr::desc(Prediction_Error)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::select(dplyr::all_of(intersect(prediction_cols, names(test_results))))
  
  # Player-level accuracy (if player_name available)
  player_accuracy <- NULL
  if ("player_name" %in% names(test_results)) {
    player_accuracy <- test_results %>%
      dplyr::group_by(player_name) %>%
      dplyr::summarise(
        Games = dplyr::n(),
        Mean_Error = mean(Prediction_Error),
        Accuracy = mean(Correct_Prediction),
        Total_Actual_Votes = sum(Actual_Votes),
        Total_Predicted_Votes = sum(Predicted_Votes),
        .groups = "drop"
      ) %>%
      dplyr::filter(Games >= 5) %>%
      dplyr::arrange(Mean_Error)
  }
  
  # Team-level accuracy (if team available)
  team_accuracy <- NULL
  if ("team" %in% names(test_results)) {
    team_accuracy <- test_results %>%
      dplyr::group_by(team) %>%
      dplyr::summarise(
        Games = dplyr::n(),
        Mean_Error = mean(Prediction_Error),
        Accuracy = mean(Correct_Prediction),
        Total_Actual_Votes = sum(Actual_Votes),
        Total_Predicted_Votes = sum(Predicted_Votes),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Mean_Error)
  }
  
  return(list(
    test_results = test_results,
    best_predictions = best_predictions,
    worst_predictions = worst_predictions,
    player_accuracy = player_accuracy,
    team_accuracy = team_accuracy
  ))
}

#' Create XGBoost Performance Plots
#'
#' Creates performance visualization plots for XGBoost models
#'
#' @param test_metrics Metrics from evaluate_model()
#' @param is_multinomial Logical. Whether model is multinomial
#' @param save_plots Logical. Whether to save plots
#' @param plot_dir Directory to save plots
#' @return List of ggplot objects
#' @export
create_xgboost_performance_plots <- function(test_metrics, is_multinomial = FALSE, 
                                            save_plots = TRUE, plot_dir = "plots/xgboost/") {
  
  if (save_plots) {
    dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  plots <- list()
  
  # Performance by vote level
  if (!is.null(test_metrics$by_votes)) {
    if (is_multinomial) {
      # For multinomial, plot accuracy
      plots$performance_by_votes <- test_metrics$by_votes %>%
        ggplot2::ggplot(ggplot2::aes(x = actual_votes, y = accuracy)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = round(accuracy, 3)), vjust = -0.5) +
        ggplot2::labs(title = "XGBoost Performance by Vote Level (Multinomial)",
             x = "Actual Votes", y = "Accuracy") +
        ggplot2::theme_minimal()
    } else {
      # For regression, plot RMSE
      plots$performance_by_votes <- test_metrics$by_votes %>%
        ggplot2::ggplot(ggplot2::aes(x = actual_votes, y = rmse)) +
        ggplot2::geom_col(fill = "coral", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = round(rmse, 3)), vjust = -0.5) +
        ggplot2::labs(title = "XGBoost Performance by Vote Level (Regression)",
             x = "Actual Votes", y = "RMSE") +
        ggplot2::theme_minimal()
    }
    
    if (save_plots) {
      ggplot2::ggsave(file.path(plot_dir, "performance_by_votes.png"), plots$performance_by_votes,
             width = 8, height = 6)
    }
  }
  
  return(plots)
}



#' Prepare XGBoost Predictions
#'
#' Handles XGBoost prediction generation and probability matrix reshaping.
#' Can also package existing predictions for ensemble modeling.
#'
#' @param final_model Trained XGBoost model (optional if providing pre-computed predictions)
#' @param train_data Training data (optional if providing pre-computed predictions)
#' @param test_data Test data (optional if providing pre-computed predictions)
#' @param feature_cols Feature column names
#' @param target_col Target column name (optional if providing pre-computed predictions)
#' @param train_pred Pre-computed training predictions (alternative to generating from model)
#' @param test_pred Pre-computed test predictions (alternative to generating from model)
#' @param train_pred_probs Pre-computed training prediction probabilities
#' @param test_pred_probs Pre-computed test prediction probabilities
#' @param constrained_pred Constrained test predictions
#' @param is_multinomial Whether model is multinomial (for packaging mode)
#' @param verbose Logical, whether to show progress messages
#'
#' @return List containing prediction matrices, class predictions, and metadata
#' @export
prepare_xgboost_predictions <- function(final_model = NULL, train_data = NULL, test_data = NULL,
                                       feature_cols = NULL, target_col = NULL,
                                       train_pred = NULL, test_pred = NULL,
                                       train_pred_probs = NULL, test_pred_probs = NULL,
                                       constrained_pred = NULL, is_multinomial = FALSE,
                                       verbose = TRUE) {
  # Mode 1: Generate predictions from model
  if (!is.null(final_model) && !is.null(train_data) && !is.null(test_data)) {
    if (verbose) cat("Generating predictions from model...")

    # Training predictions
    dtrain_matrix <- xgboost::xgb.DMatrix(
      data = as.matrix(train_data[, feature_cols]),
      label = train_data[[target_col]]
    )
    train_pred <- predict(final_model, dtrain_matrix)

    # Test predictions
    dtest_matrix <- xgboost::xgb.DMatrix(
      data = as.matrix(test_data[, feature_cols]),
      label = test_data[[target_col]]
    )
    test_pred <- predict(final_model, dtest_matrix)

    # Reshape probability matrices
    train_pred_matrix <- matrix(train_pred, nrow = nrow(train_data), byrow = TRUE)
    test_pred_matrix <- matrix(test_pred, nrow = nrow(test_data), byrow = TRUE)
    colnames(train_pred_matrix) <- paste0("Prob_", 0:3, "_votes")
    colnames(test_pred_matrix) <- paste0("Prob_", 0:3, "_votes")

    # Generate class predictions
    train_pred_class <- apply(train_pred_matrix, 1, which.max) - 1
    test_pred_class <- apply(test_pred_matrix, 1, which.max) - 1

    if (verbose) cat(sprintf(" %d train, %d test\n", nrow(train_pred_matrix), nrow(test_pred_matrix)))

    return(list(
      train_pred_matrix = train_pred_matrix,
      test_pred_matrix = test_pred_matrix,
      train_pred_class = train_pred_class,
      test_pred_class = test_pred_class
    ))
  }

  # Mode 2: Package existing predictions
  else if (!is.null(train_pred) && !is.null(test_pred)) {
    if (verbose) cat("Packaging existing predictions...")

    predictions <- list(
      train_pred = train_pred,
      test_pred = test_pred,
      feature_cols = feature_cols,
      model_type = if (is_multinomial) "multinomial" else "regression"
    )

    # Add probability matrices for multinomial
    if (is_multinomial && !is.null(train_pred_probs)) {
      predictions$train_pred_probs <- train_pred_probs
    }

    if (is_multinomial && !is.null(test_pred_probs)) {
      predictions$test_pred_probs <- test_pred_probs
    }

    # Add constrained predictions
    if (!is.null(constrained_pred)) {
      predictions$constrained_test_pred <- constrained_pred
    }

    if (verbose) cat(" ✓\n")
    return(predictions)
  }

  else {
    stop("Must provide either: (final_model, train_data, test_data) OR (train_pred, test_pred)")
  }
}

#' Evaluate XGBoost Performance
#'
#' Comprehensive performance evaluation for XGBoost models including
#' probability analysis, feature importance, and diagnostics.
#'
#' @param predictions Results from prepare_xgboost_predictions()
#' @param train_data Training data
#' @param test_data Test data
#' @param final_model Trained XGBoost model
#' @param feature_cols Feature column names
#' @param target_col Target column name
#' @param save_plots Logical, whether to save diagnostic plots
#' @param verbose Logical, whether to show progress messages
#'
#' @return List containing all evaluation results
#' @export
evaluate_xgboost_performance <- function(predictions, train_data, test_data, final_model,
                                        feature_cols, target_col, save_plots = TRUE, verbose = TRUE) {
  if (verbose) cat("Evaluating performance...")

  # Basic metrics
  train_metrics <- evaluate_model(
    actual = train_data[[target_col]],
    predicted = predictions$train_pred_matrix
  )

  test_metrics <- evaluate_model(
    actual = test_data[[target_col]],
    predicted = predictions$test_pred_matrix
  )

  if (verbose) {
    cat(sprintf(" train log-loss: %.3f, test log-loss: %.3f\n",
                ifelse(is.na(train_metrics$log_loss), 999, train_metrics$log_loss),
                ifelse(is.na(test_metrics$log_loss), 999, test_metrics$log_loss)))
  }

  # Probability analysis
  if (verbose) cat("Analyzing probabilities...")
  prob_analysis <- analyze_xgboost_probabilities(
    pred_matrix = predictions$test_pred_matrix,
    actual = test_data[[target_col]],
    class_names = c("0", "1", "2", "3")
  )

  if (save_plots) {
    calibration_plots <- create_calibration_plots(prob_analysis, save_plots = TRUE)
  }

  if (verbose) {
    cat(sprintf(" brier: %.3f, ECE: %.3f\n",
                prob_analysis$overall_brier,
                ifelse(is.na(prob_analysis$ece), 0, prob_analysis$ece)))
  }

  # Feature importance
  if (verbose) cat("Analyzing feature importance...")
  importance <- analyze_feature_importance(
    model = final_model,
    feature_names = feature_cols,
    model_type = "xgboost",
    top_n = 25
  )

  if (save_plots) {
    importance_plot <- ggplot2::ggplot(head(importance, 20),
                                      ggplot2::aes(x = reorder(Feature, Importance), y = Importance)) +
      ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "XGBoost Feature Importance (Top 20)",
                    subtitle = "Based on information gain",
                    x = "Feature", y = "Importance") +
      ggplot2::theme_minimal()

    ggplot2::ggsave("plots/xgboost/feature_importance.png", importance_plot,
                    width = 12, height = 8)
  }

  if (verbose) cat(sprintf(" top: %s\n", importance$Feature[1]))

  # Model diagnostics
  if (verbose) cat("Creating diagnostics...")
  diagnostics <- model_diagnostics(
    actual = test_data[[target_col]],
    predicted = predictions$test_pred_matrix,
    model_type = "multinomial"
  )

  if (save_plots) {
    ggplot2::ggsave("plots/xgboost/predicted_vs_actual.png", diagnostics$pred_vs_actual_plot,
                    width = 8, height = 6)

    if (!is.null(diagnostics$calibration_plot)) {
      ggplot2::ggsave("plots/xgboost/model_calibration.png", diagnostics$calibration_plot,
                      width = 8, height = 6)
    }

    performance_plots <- create_xgboost_performance_plots(
      test_metrics = test_metrics,
      is_multinomial = TRUE,
      save_plots = TRUE
    )
  }

  if (verbose) cat(" ✓\n")

  return(list(
    train_metrics = train_metrics,
    test_metrics = test_metrics,
    prob_analysis = prob_analysis,
    importance = importance,
    diagnostics = diagnostics
  ))
}

#' Train Production XGBoost Model
#'
#' Trains final production model on complete dataset for deployment.
#'
#' @param train_data Training data
#' @param test_data Test data
#' @param feature_cols Feature column names
#' @param target_col Target column name
#' @param best_params Optimal hyperparameters
#' @param best_iteration Optimal number of rounds
#' @param verbose Logical, whether to show progress messages
#'
#' @return Trained production model
#' @export
train_production_xgboost <- function(train_data, test_data, feature_cols, target_col,
                                    best_params, best_iteration, verbose = TRUE) {
  if (verbose) cat("Retraining production model on complete dataset...")

  complete_dataset <- dplyr::bind_rows(train_data, test_data) %>%
    dplyr::filter(season != 2025)  # Exclude any incomplete future seasons

  if (verbose) cat(sprintf(" using %d observations from all seasons\n", nrow(complete_dataset)))

  # Train production model with same optimal hyperparameters
  production_model <- train_final_xgboost(
    train_data = complete_dataset,
    feature_cols = feature_cols,
    target_col = target_col,
    params = best_params,
    nrounds = best_iteration
  )

  # Save production model (overwrites evaluation model)
  saveRDS(production_model, "models/xgboost_production_model.rds")

  if (verbose) cat(" ✓ Production model saved with complete dataset\n")

  return(production_model)
}