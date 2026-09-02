# Model Utilities for Brownlow Medal Prediction
#
# This file contains utility functions for model training, validation,
# evaluation, and hyperparameter tuning.

#' Prepare Data for Modeling
#'
#' Prepares the feature dataset for machine learning by handling missing values,
#' encoding categorical variables, and splitting into train/validation sets
#'
#' @param features DataFrame with all features
#' @param target_col Character. Name of target column (default: "brownlow_votes")
#' @param test_seasons Vector. seasons to use for testing (default: last 2 seasons)
#' @param holdout_seasons Vector. seasons to exclude completely as holdout (default: c(2025))
#' @param remove_zero_votes Logical. Remove observations with 0 votes from training (default: FALSE)
#'
#' @return List containing train and test datasets
#' @export
prepare_modeling_data <- function(features,
                                  target_col = "brownlow_votes",
                                  test_seasons = NULL,
                                  holdout_seasons = c(2025),
                                  remove_zero_votes = FALSE) {

  # Remove rows with missing target
  modeling_data <- features %>%
    dplyr::filter(!is.na(.data[[target_col]]))

  # Exclude holdout seasons completely from modeling
  available_data <- modeling_data %>%
    dplyr::filter(!season %in% holdout_seasons)

  # Store holdout data separately (not used in training/validation/testing)
  holdout_data <- modeling_data %>%
    dplyr::filter(season %in% holdout_seasons)

  # Determine test seasons if not specified
  if (is.null(test_seasons)) {
    all_seasons <- sort(unique(available_data$season))
    # Use 2023 and 2024 as test seasons
    test_seasons <- tail(all_seasons, 2)
  }

  # Split into train and test from available data only
  train_data <- available_data %>%
    dplyr::filter(!season %in% test_seasons)

  test_data <- available_data %>%
    dplyr::filter(season %in% test_seasons)

  # Optionally remove zero votes from training (to focus on vote-getting performance)
  if (remove_zero_votes) {
    train_data <- train_data %>%
      dplyr::filter(.data[[target_col]] > 0)
  }

  # Identify numeric and categorical columns
  numeric_cols <- names(train_data)[sapply(train_data, is.numeric)]
  categorical_cols <- names(train_data)[sapply(train_data, function(x) is.character(x) | is.factor(x))]

  # Remove identifier columns from features
  exclude_cols <- c("date", "player_name", "match_id", target_col, "season")
  feature_cols <- setdiff(numeric_cols, exclude_cols)

  message(sprintf("Training data: %d observations (seasons 2015-2022)", nrow(train_data)))
  message(sprintf("Test data: %d observations (seasons 2023-2024)", nrow(test_data)))
  if (nrow(holdout_data) > 0) {
    message(sprintf("Holdout data: %d observations (seasons: %s)",
                    nrow(holdout_data), paste(holdout_seasons, collapse = ", ")))
  }
  message(sprintf("Features: %d numeric columns", length(feature_cols)))

  return(list(
    train = train_data,
    test = test_data,
    holdout = holdout_data,
    feature_cols = feature_cols,
    target_col = target_col,
    test_seasons = test_seasons,
    holdout_seasons = holdout_seasons
  ))
}

#' Prepare Full Training Data (All Available Seasons)
#'
#' Prepares the complete dataset for final model training using all available
#' historical data (2015-2024) after hyperparameter optimization is complete.
#' This follows ML best practice: evaluate on holdout -> optimize -> retrain on full data.
#'
#' @param features DataFrame with all features
#' @param target_col Character. Name of target column (default: "brownlow_votes")
#' @param holdout_seasons Vector. seasons to exclude completely as holdout (default: c(2025))
#' @param remove_zero_votes Logical. Remove observations with 0 votes from training (default: FALSE)
#'
#' @return List containing full training data and metadata
#' @export
prepare_full_training_data <- function(features,
                                      target_col = "brownlow_votes",
                                      test_seasons = NULL,
                                      holdout_seasons = c(2025),
                                      remove_zero_votes = FALSE) {
  # test_seasons was referenced in the body but never declared as a parameter,
  # so this either errored or silently picked up a global of the same name.
  # Unused today, which is the only reason it never bit.

  # Remove rows with missing target
  modeling_data <- features %>%
    dplyr::filter(!is.na(.data[[target_col]]))

  # Create test data from specified test seasons
  test_data <- if (!is.null(test_seasons)) {
    modeling_data %>%
      dplyr::filter(season %in% test_seasons & !season %in% holdout_seasons)
  } else {
    data.frame()
  }

  # Use remaining data for training (excluding test and holdout seasons)
  full_training_data <- modeling_data %>%
    dplyr::filter(!season %in% c(test_seasons, holdout_seasons))

  # Store holdout data separately (for future predictions)
  holdout_data <- modeling_data %>%
    dplyr::filter(season %in% holdout_seasons)

  # Optionally remove zero votes from training (to focus on vote-getting performance)
  if (remove_zero_votes) {
    full_training_data <- full_training_data %>%
      dplyr::filter(.data[[target_col]] > 0)
  }

  # Identify numeric and categorical columns
  numeric_cols <- names(full_training_data)[sapply(full_training_data, is.numeric)]
  categorical_cols <- names(full_training_data)[sapply(full_training_data, function(x) is.character(x) | is.factor(x))]

  # Remove identifier columns from features
  exclude_cols <- c("date", "player_name", "match_id", target_col, "season")
  feature_cols <- setdiff(numeric_cols, exclude_cols)

  # Get season range for training data
  training_seasons <- sort(unique(full_training_data$season))

  message(sprintf("Full training data: %d observations across %d seasons (%d-%d)",
                  nrow(full_training_data),
                  length(training_seasons),
                  min(training_seasons),
                  max(training_seasons)))
  if (nrow(holdout_data) > 0) {
    message(sprintf("Holdout data: %d observations (seasons: %s)",
                    nrow(holdout_data), paste(holdout_seasons, collapse = ", ")))
  }
  message(sprintf("Features: %d numeric columns", length(feature_cols)))

  return(list(
    train = full_training_data,
    test = test_data,
    holdout = holdout_data,
    feature_cols = feature_cols,
    target_col = target_col,
    training_seasons = training_seasons,
    test_seasons = test_seasons,
    holdout_seasons = holdout_seasons,
    full_training_mode = TRUE
  ))
}

#' Cross-Validation for Brownlow Models
#'
#' Performs time-aware cross-validation respecting the temporal nature of AFL seasons
#'
#' @param data DataFrame with features and target
#' @param target_col Character. Name of target column
#' @param feature_cols Vector. Names of feature columns
#' @param n_folds Integer. Number of CV folds (default: 5)
#' @param min_train_seasons Integer. Minimum seasons for training (default: 3)
#'
#' @return List of train/validation indices for each fold
#' @export
create_time_aware_cv <- function(data,
                                 target_col = "brownlow_votes",
                                 feature_cols,
                                 n_folds = 5,
                                 min_train_seasons = 3) {

  seasons <- sort(unique(data$season))
  n_seasons <- length(seasons)

  if (n_seasons < (min_train_seasons + 1)) {
    stop("Not enough seasons for time-aware CV")
  }

  cv_folds <- list()

  # Create expanding window CV
  for (i in 1:n_folds) {
    # Training seasons: from start to (end - n_folds + i)
    train_end_idx <- n_seasons - n_folds + i
    train_seasons <- seasons[1:max(min_train_seasons, train_end_idx)]

    # Validation season: next season after training
    val_season <- seasons[train_end_idx + 1]

    if (train_end_idx + 1 <= n_seasons) {
      train_indices <- which(data$season %in% train_seasons)
      val_indices <- which(data$season == val_season)

      cv_folds[[i]] <- list(
        train = train_indices,
        validation = val_indices,
        train_seasons = train_seasons,
        val_season = val_season
      )
    }
  }

  return(cv_folds)
}

#' Calculate MLogloss for Regression Models
#'
#' Converts regression predictions to probabilities and calculates multinomial log-loss
#'
#' @param actual Vector of actual votes (0-3)
#' @param predicted Vector of predicted votes (continuous)
#'
#' @return Multinomial log-loss value
#' @export
calculate_regression_mlogloss <- function(actual, predicted) {
  tryCatch({
    # Convert regression predictions to probabilities for each class (0, 1, 2, 3)
    # Use a simple softmax-like approach based on distance to each class

    n_obs <- length(predicted)
    prob_matrix <- matrix(0, nrow = n_obs, ncol = 4)  # 4 classes: 0, 1, 2, 3

    for (i in 1:n_obs) {
      pred_val <- pmax(0, pmin(3, predicted[i]))  # Constrain to 0-3

      # Calculate probabilities based on distance to each class
      distances <- abs(pred_val - c(0, 1, 2, 3))

      # Convert distances to probabilities using exponential decay
      # Smaller distances = higher probabilities
      temp <- 2.0  # Temperature parameter
      weights <- exp(-distances / temp)
      probs <- weights / sum(weights)

      prob_matrix[i, ] <- probs
    }

    # Calculate multinomial log-loss
    mlogloss <- -mean(log(pmax(prob_matrix[cbind(seq_along(actual), actual + 1)], 1e-15)))

    return(mlogloss)

  }, error = function(e) {
    warning(sprintf("Could not calculate mlogloss for regression model: %s", e$message))
    return(NA)
  })
}

#' Evaluate Model Performance
#'
#' Calculates performance metrics for Brownlow prediction models with primary focus
#' on probability quality (log-loss) rather than classification accuracy
#'
#' @param actual Vector of actual votes
#' @param predicted Vector of predicted votes or probability matrix for multinomial
#' @param by_votes Logical. Calculate metrics by vote level (default: TRUE)
#' @param model_type Character. "regression" or "multinomial" (default: "multinomial")
#' @param return_class Logical. Return class predictions instead of probabilities (default: FALSE)
#'
#' @return List of performance metrics with log_loss as primary metric for multinomial
#' @export
evaluate_model <- function(actual, predicted, by_votes = TRUE, model_type = "multinomial", return_class = FALSE) {

  # Initialize predicted_class variable for later use
  predicted_class <- NULL

  if (model_type == "multinomial") {
    # For multinomial classification
    if (is.matrix(predicted)) {
      # Convert probability matrix to class predictions
      predicted_class <- apply(predicted, 1, which.max) - 1  # Convert to 0-3 scale
    } else {
      predicted_class <- round(pmax(0, pmin(3, predicted)))
    }

    # Classification metrics
    actual_factor <- factor(actual, levels = 0:3)
    predicted_factor <- factor(predicted_class, levels = 0:3)

    # Overall accuracy
    exact_accuracy <- mean(actual == predicted_class, na.rm = TRUE)

    # Confusion matrix
    conf_matrix <- caret::confusionMatrix(predicted_factor, actual_factor)

    # Performance for vote-getting (0 vs >0)
    vote_getting_actual <- as.numeric(actual > 0)
    vote_getting_predicted <- as.numeric(predicted_class > 0)
    vote_getting_accuracy <- mean(vote_getting_actual == vote_getting_predicted, na.rm = TRUE)

    # Log-loss if we have probabilities
    log_loss <- NA
    if (is.matrix(predicted)) {
      log_loss <- -mean(log(pmax(predicted[cbind(seq_along(actual), actual + 1)], 1e-15)))
    }

    # PRIORITY ORDER: Probability metrics first, then accuracy metrics
    metrics <- list(
      log_loss = log_loss,  # PRIMARY METRIC: Multinomial log-loss
      mlogloss = log_loss,  # Alias for log_loss
      exact_accuracy = exact_accuracy,
      vote_getting_accuracy = vote_getting_accuracy,
      confusion_matrix = conf_matrix,
      overall_accuracy = conf_matrix$overall['Accuracy'],
      balanced_accuracy = conf_matrix$overall['Balanced Accuracy'],
      kappa = conf_matrix$overall['Kappa']
    )

  } else {
    # Original regression metrics
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    mae <- mean(abs(actual - predicted), na.rm = TRUE)

    # Correlation
    correlation <- cor(actual, predicted, use = "complete.obs")

    # For classification perspective (0 vs 1 vs 2 vs 3 votes)
    actual_class <- factor(actual, levels = 0:3)
    predicted_class <- factor(round(pmax(0, pmin(3, predicted))), levels = 0:3)

    # Accuracy for exact vote prediction
    exact_accuracy <- mean(actual == round(predicted), na.rm = TRUE)

    # Performance for vote-getting (0 vs >0)
    vote_getting_actual <- as.numeric(actual > 0)
    vote_getting_predicted <- as.numeric(predicted > 0.5)
    vote_getting_accuracy <- mean(vote_getting_actual == vote_getting_predicted, na.rm = TRUE)

    # Calculate mlogloss for regression by converting to probabilities
    mlogloss <- calculate_regression_mlogloss(actual, predicted)

    metrics <- list(
      rmse = rmse,
      mae = mae,
      correlation = correlation,
      exact_accuracy = exact_accuracy,
      vote_getting_accuracy = vote_getting_accuracy,
      mlogloss = mlogloss  # Add mlogloss for regression models too
    )
  }

  # Detailed metrics by vote level
  if (by_votes) {
    if (model_type == "multinomial") {
      # For multinomial, use the class predictions already computed above
      vote_level_metrics <- data.frame(
        actual = actual,
        predicted_class = predicted_class
      ) %>%
        dplyr::mutate(
          actual_votes = factor(actual, levels = 0:3),
          predicted_votes = factor(predicted_class, levels = 0:3)
        ) %>%
        dplyr::group_by(actual_votes) %>%
        dplyr::summarise(
          count = n(),
          accuracy = mean(actual == predicted_class, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # For regression, use original logic
      vote_level_metrics <- data.frame(
        actual = actual,
        predicted = predicted
      ) %>%
        dplyr::mutate(
          actual_votes = factor(actual, levels = 0:3),
          predicted_rounded = pmax(0, pmin(3, round(predicted)))
        ) %>%
        dplyr::group_by(actual_votes) %>%
        dplyr::summarise(
          count = n(),
          mean_predicted = mean(predicted, na.rm = TRUE),
          rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
          mae = mean(abs(actual - predicted), na.rm = TRUE),
          .groups = "drop"
        )
    }

    metrics$by_votes <- vote_level_metrics
  }

  return(metrics)
}

#' Feature Importance Analysis
#'
#' Analyzes feature importance from trained models
#'
#' @param model Trained model object
#' @param feature_names Vector of feature names
#' @param model_type Character. Type of model ("xgboost", "ranger", etc.)
#' @param top_n Integer. Number of top features to return (default: 20)
#'
#' @return DataFrame of feature importance
#' @export
analyze_feature_importance <- function(model, feature_names, model_type = "xgboost", top_n = 20) {

  if (model_type == "xgboost") {
    importance <- xgboost::xgb.importance(
      feature_names = feature_names,
      model = model
    ) %>%
      as.data.frame() %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::mutate(
        Feature = Feature,
        Importance = Gain,
        Type = "XGBoost Gain"
      )

  } else if (model_type == "ranger") {
    importance <- data.frame(
      Feature = names(model$variable.importance),
      Importance = as.numeric(model$variable.importance)
    ) %>%
      dplyr::arrange(desc(Importance)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::mutate(Type = "Random Forest")

  } else if (model_type == "gam") {
    # For GAM models, extract significance of terms
    gam_summary <- summary(model)
    importance <- data.frame(
      Feature = rownames(gam_summary$p.table),
      Importance = abs(gam_summary$p.table[, "t value"]),
      P_Value = gam_summary$p.table[, "Pr(>|t|)"]
    ) %>%
      dplyr::arrange(desc(Importance)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::mutate(Type = "GAM t-value")

  } else {
    stop("Unsupported model type for feature importance")
  }

  return(importance)
}

#' Hyperparameter Tuning for XGBoost
#'
#' Performs hyperparameter tuning for XGBoost models using cross-validation
#' Optimizes for multinomial log-loss (lower is better) for multinomial models
#'
#' @param train_data DataFrame with training data
#' @param feature_cols Vector of feature column names
#' @param target_col Character. Target column name
#' @param cv_folds List of CV fold indices
#' @param param_grid List. Parameter grid for tuning
#' @param n_iter Integer. Number of random search iterations (default: 50)
#' @param model_type Character. "regression" or "multinomial" (default: "multinomial")
#' @param n_cores Integer. Number of cores for parallel processing (default: NULL)
#'
#' @return List with best parameters optimized for log-loss (multinomial) or RMSE (regression)
#' @export
tune_xgboost_hyperparameters <- function(train_data,
                                          feature_cols,
                                          target_col,
                                          cv_folds,
                                          param_grid = NULL,
                                          n_iter = 50,
                                          model_type = "multinomial",
                                          n_cores = NULL) {

  # Set up parallel processing with future
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores()/2) %>% round()  # Use half of available cores
  }
  cat(sprintf("Using %d cores for parallel processing with future...\n", n_cores))

  # Set up future plan for parallel processing
  future::plan(future::multisession, workers = n_cores)

  # Default parameter grid if not provided
  if (is.null(param_grid)) {
    if (model_type == "multinomial") {
      param_grid <- list(
        max_depth = c(3, 4, 5, 6),
        eta = c(0.025, 0.05, 0.1, 0.2),
        subsample = c(0.4, 0.6, 0.8),
        colsample_bytree = c(0.4, 0.6, 0.8),
        min_child_weight = c(1, 3, 5),
        gamma = c(0, 0.1, 0.2),
        lambda = c(0.1, 1, 2),
        alpha = c(0, 0.1, 0.5)
      )
    } else {
      param_grid <- list(
        max_depth = c(3, 4, 5, 6, 8),
        eta = c(0.01, 0.05, 0.1, 0.2, 0.3),
        subsample = c(0.7, 0.8, 0.9, 1.0),
        colsample_bytree = c(0.7, 0.8, 0.9, 1.0),
        min_child_weight = c(1, 3, 5, 7),
        gamma = c(0, 0.1, 0.2, 0.5),
        lambda = c(0.1, 1, 2, 5),
        alpha = c(0, 0.1, 0.5, 1)
      )
    }
  }

  # Generate random parameter combinations
  set.seed(42)
  param_combinations <- expand.grid(param_grid) %>%
    dplyr::sample_n(min(n_iter, nrow(.)))

  cat(sprintf("Testing %d parameter combinations in parallel...\n", nrow(param_combinations)))

  # Function to evaluate single parameter combination using xgb.cv
  evaluate_params <- function(i) {
    params <- as.list(param_combinations[i, ])

    # Set objective based on model type
    if (model_type == "multinomial") {
      params$objective <- "multi:softprob"
      params$eval_metric <- "mlogloss"
      params$num_class <- 4  # 0, 1, 2, 3 votes
    } else {
      params$objective <- "reg:squarederror"
      params$eval_metric <- "rmse"
    }

    # Prepare full training data for xgb.cv
    dtrain_full <- xgboost::xgb.DMatrix(
      data = as.matrix(train_data[, feature_cols]),
      label = train_data[[target_col]]
    )

    # Use xgb.cv to find optimal number of rounds and get CV score
    cv_result <- xgboost::xgb.cv(
      params = params,
      data = dtrain_full,
      # folds = cv_folds,
      nrounds = 500,  # Maximum rounds to test
      nfold = 5,  # Use 5-fold CV within xgb.cv
      stratified = (model_type == "multinomial"),
      early_stopping_rounds = 50,
      verbose = 0,  # Reduce verbosity for parallel execution
      showsd = FALSE,
      print_every_n = 10
    )

    # Get best iteration and score
    best_iteration <- cv_result$best_iteration
    if (is.null(best_iteration)) best_iteration <- cv_result$niter

    # Extract best CV score
    if (model_type == "multinomial") {
      best_score <- min(cv_result$evaluation_log$test_mlogloss_mean, na.rm = TRUE)
    } else {
      best_score <- min(cv_result$evaluation_log$test_rmse_mean, na.rm = TRUE)
    }

    return(list(
      params = params,
      cv_score_mean = best_score,
      cv_score_sd = 0,  # xgb.cv doesn't provide SD across parameter sets
      fold_scores = rep(best_score, 5),  # Placeholder
      best_iteration = best_iteration
    ))
  }

  # Run parallel tuning with progress tracking
  start_time <- Sys.time()
  total_combinations <- nrow(param_combinations)

  cat(sprintf("Starting parallel tuning of %d combinations using %d cores...\n",
             total_combinations, n_cores))

  # Use future_map with progress tracking
  cv_results <- furrr::future_map(
    1:total_combinations,
    evaluate_params,
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )

  # Reset future plan to sequential
  future::plan(future::sequential)

  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("\nParallel tuning completed in %.1f minutes\n", as.numeric(elapsed_time)))

  # Find best parameters
  cv_scores <- sapply(cv_results, function(x) x$cv_score_mean)

  if (model_type == "multinomial") {
    best_idx <- which.min(cv_scores)  # Lower log-loss is better
    metric_name <- "Log-Loss"
  } else {
    best_idx <- which.min(cv_scores)  # Lower RMSE is better
    metric_name <- "RMSE"
  }

  best_params <- cv_results[[best_idx]]$params
  best_score <- cv_results[[best_idx]]$cv_score_mean
  best_iteration <- cv_results[[best_idx]]$best_iteration

  cat(sprintf("\nBest CV %s: %.4f\n", metric_name, best_score))
  cat(sprintf("Best iteration: %d rounds\n", best_iteration))

  return(list(
    best_params = best_params,
    best_cv_score = best_score,
    best_iteration = best_iteration,
    all_results = cv_results,
    model_type = model_type
  ))
}

#' Train Final XGBoost Model
#'
#' Trains the final XGBoost model with best parameters and optimal number of rounds.
#' The nrounds parameter should come from hyperparameter tuning results to ensure
#' consistency with the tuning phase.
#'
#' @param train_data DataFrame with training data
#' @param feature_cols Vector of feature column names
#' @param target_col Character. Target column name
#' @param params List. Model parameters from hyperparameter tuning
#' @param nrounds Integer. Number of boosting rounds from hyperparameter tuning (required)
#'
#' @return Trained XGBoost model
#' @export
train_final_xgboost <- function(train_data, feature_cols, target_col, params, nrounds) {

  # Validate required parameters
  if (missing(nrounds) || is.null(nrounds) || !is.numeric(nrounds) || nrounds <= 0) {
    stop("nrounds parameter is required and must be a positive integer from hyperparameter tuning results")
  }

  # Prepare data
  dtrain <- xgboost::xgb.DMatrix(
    data = as.matrix(train_data[, feature_cols]),
    label = train_data[[target_col]]
  )

  # Set default objective and eval metric if not in params
  if (!"objective" %in% names(params)) {
    # Default to regression if not specified
    params$objective <- "reg:squarederror"
  }
  if (!"eval_metric" %in% names(params)) {
    if (params$objective == "multi:softprob") {
      params$eval_metric <- "mlogloss"
    } else {
      params$eval_metric <- "rmse"
    }
  }

  cat(sprintf("Training final XGBoost model with %d rounds (from hyperparameter tuning)...\n", nrounds))

  # Train final model with optimal rounds from tuning
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 1
  )

  # Booster objects are ALTREP-backed from xgboost 3.0 and reject arbitrary
  # element assignment. The round count is recoverable via
  # xgboost::xgb.get.num.boosted.rounds(model), so nothing is lost here.

  return(model)
}

#' Model Diagnostics
#'
#' Creates diagnostic plots and statistics for model evaluation
#'
#' @param actual Vector of actual values
#' @param predicted Vector of predicted values or probability matrix for multinomial
#' @param feature_data DataFrame with features (optional)
#' @param model Trained model (optional)
#' @param model_type Character. "regression", "multinomial", or "classification"
#'
#' @return List of diagnostic information
#' @export
model_diagnostics <- function(actual, predicted, feature_data = NULL, model = NULL, model_type = "regression") {

  diagnostics <- list()

  # Handle different prediction types
  if (is.matrix(predicted)) {
    # For multinomial predictions, convert to class predictions
    predicted_values <- apply(predicted, 1, which.max) - 1
    model_type <- "multinomial"
  } else {
    predicted_values <- predicted
  }

  # Basic residual analysis (for regression/classification)
  if (model_type %in% c("regression", "classification")) {
    residuals <- actual - predicted_values

    diagnostics$residual_stats <- list(
      mean_residual = mean(residuals, na.rm = TRUE),
      sd_residual = sd(residuals, na.rm = TRUE),
      skewness = if (requireNamespace("moments", quietly = TRUE)) {
        moments::skewness(residuals)
      } else {
        NA
      },
      kurtosis = if (requireNamespace("moments", quietly = TRUE)) {
        moments::kurtosis(residuals)
      } else {
        NA
      }
    )

    # Residual distribution
    diagnostics$residual_plot <- ggplot2::ggplot(
      data = data.frame(Residuals = residuals),
      ggplot2::aes(x = Residuals)
    ) +
      ggplot2::geom_histogram(bins = 30, alpha = 0.7, color = "black") +
      ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      ggplot2::labs(title = "Residual Distribution", x = "Residuals", y = "Count") +
      ggplot2::theme_minimal()
  }

  # Prediction vs actual plot
  diagnostics$pred_vs_actual_plot <- ggplot2::ggplot(
    data = data.frame(Actual = actual, Predicted = predicted_values),
    ggplot2::aes(x = Actual, y = Predicted)
  ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggplot2::labs(title = paste("Predicted vs Actual Votes -", stringr::str_to_title(model_type)),
         x = "Actual Votes", y = "Predicted Votes") +
    ggplot2::theme_minimal()

  # Confusion matrix for classification/multinomial
  if (model_type %in% c("classification", "multinomial")) {
    confusion_matrix <- table(Predicted = predicted_values, Actual = actual)
    diagnostics$confusion_matrix <- confusion_matrix

    # Accuracy by class
    class_accuracy <- diag(confusion_matrix) / rowSums(confusion_matrix)
    diagnostics$class_accuracy <- class_accuracy
  }

  # Probability calibration plot for multinomial
  if (model_type == "multinomial" && is.matrix(predicted)) {
    max_probs <- apply(predicted, 1, max)
    correct_predictions <- (predicted_values == actual)

    # Create probability bins
    prob_bins <- cut(max_probs, breaks = seq(0, 1, 0.1), include.lowest = TRUE)

    calibration_data <- data.frame(
      max_prob = max_probs,
      correct = correct_predictions,
      prob_bin = prob_bins
    ) %>%
      dplyr::group_by(prob_bin) %>%
      dplyr::summarise(
        n = n(),
        mean_confidence = mean(max_prob),
        accuracy = mean(correct),
        .groups = "drop"
      ) %>%
      dplyr::filter(n >= 5)

    if (nrow(calibration_data) > 0) {
      diagnostics$calibration_plot <- ggplot2::ggplot(calibration_data, ggplot2::aes(x = mean_confidence, y = accuracy)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
        ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.7, color = "steelblue") +
        ggplot2::labs(title = "Probability Calibration",
             x = "Mean Predicted Probability", y = "Observed Accuracy",
             size = "Count") +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_minimal()
    }
  }

  return(diagnostics)
}


#' Train GAM Model
#'
#' Trains a GAM model for Brownlow prediction with automatic formula generation
#'
#' @param train_data DataFrame with training data
#' @param feature_cols Vector of feature column names
#' @param target_col Character. Target column name
#' @param smooth_vars Vector of variables to apply smoothing to
#' @param categorical_vars Vector of categorical variables
#' @param family Character. GAM family ("gaussian", "poisson", "quasipoisson")
#' @param k Integer. Basis dimension for smooth terms (default: 10)
#' @param method Character. Smoothing parameter estimation method (default: "REML")
#'
#' @return Trained GAM model
#' @export
train_gam_model <- function(train_data,
                           feature_cols,
                           target_col,
                           smooth_vars = NULL,
                           categorical_vars = NULL,
                           family = "gaussian",
                           k = 10,
                           method = "REML") {

  # Auto-detect smooth and categorical variables if not specified
  if (is.null(smooth_vars)) {
    numeric_vars <- feature_cols[sapply(train_data[feature_cols], is.numeric)]
    smooth_vars <- numeric_vars[!numeric_vars %in% categorical_vars]
  }

  if (is.null(categorical_vars)) {
    categorical_vars <- feature_cols[sapply(train_data[feature_cols], function(x) is.character(x) | is.factor(x))]
  }

  # Build formula components
  smooth_terms <- if (length(smooth_vars) > 0) {
    paste0("s(", smooth_vars, ", k = ", k, ")", collapse = " + ")
  } else {
    ""
  }

  categorical_terms <- if (length(categorical_vars) > 0) {
    paste(categorical_vars, collapse = " + ")
  } else {
    ""
  }

  # Combine terms
  formula_parts <- c(smooth_terms, categorical_terms)
  formula_parts <- formula_parts[formula_parts != ""]

  if (length(formula_parts) == 0) {
    stop("No features specified for GAM model")
  }

  formula_str <- paste(target_col, "~", paste(formula_parts, collapse = " + "))
  model_formula <- as.formula(formula_str)

  cat("GAM Formula:", deparse(model_formula), "\n")

  # Train GAM model
  gam_model <- mgcv::gam(
    formula = model_formula,
    data = train_data,
    family = family,
    method = method
  )

  return(gam_model)
}


#' Predict with GAM Model
#'
#' Makes predictions with GAM model and handles potential issues
#'
#' @param gam_model Trained GAM model
#' @param newdata DataFrame with new data for prediction
#' @param type Character. Type of prediction ("response" or "link")
#' @param apply_constraints Logical. Apply 0-3 vote constraints
#'
#' @return Vector of predictions
#' @export
predict_gam <- function(gam_model, newdata, type = "response", apply_constraints = TRUE) {

  # Make predictions
  predictions <- predict(gam_model, newdata = newdata, type = type)

  # Apply constraints if requested
  if (apply_constraints && type == "response") {
    predictions <- pmax(0, pmin(3, predictions))
  }

  return(predictions)
}


# Null-or operator (internal utility)
# @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
