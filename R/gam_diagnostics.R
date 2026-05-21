# GAM Model Diagnostics and Validation Functions
# Functions for evaluating GAM model performance and assumptions

#' Check GAM Model Convergence
#'
#' @param gam_model A fitted GAM model object
#' @return List with convergence information
check_gam_convergence <- function(gam_model) {
  list(
    converged = gam_model$converged,
    iterations = gam_model$iter,
    outer_iterations = gam_model$outer.info$iter,
    gradient_norm = gam_model$outer.info$grad,
    conv_tol = gam_model$outer.info$conv.tol
  )
}


#' Plot GAM Smooth Terms
#'
#' @param gam_model A fitted GAM model object
#' @param select Vector of smooth term indices to plot (NULL for all)
#' @param pages Number of plots per page
#' @return Plots of smooth terms
plot_gam_smooths <- function(gam_model, select = NULL, pages = 4) {
  # Save only safe graphical parameters
  old_mar <- par("mar")
  old_mfrow <- par("mfrow")

  # Restore on function exit
  on.exit({
    par(mar = old_mar, mfrow = old_mfrow)
  })

  # Try progressively smaller margins until one works
  margin_attempts <- list(
    c(4, 4, 2, 1),    # Standard margins
    c(3, 3, 1, 1),    # Smaller margins
    c(2, 2, 1, 0.5),  # Even smaller
    c(1.5, 1.5, 0.5, 0.2)  # Minimal margins
  )

  page_attempts <- c(pages, 2, 1)  # Try fewer pages if needed

  success <- FALSE

  for (page_count in page_attempts) {
    for (margins in margin_attempts) {
      tryCatch({
        par(mar = margins)
        plot(gam_model, select = select, pages = page_count, shade = TRUE, scale = 0)
        success <- TRUE
        if (page_count != pages || !identical(margins, c(4, 4, 2, 1))) {
          cat(sprintf("Successfully plotted with %d pages and margins: %s\n",
                     page_count, paste(margins, collapse = ", ")))
        }
        break
      }, error = function(e) {
        # Continue to next attempt
      })
    }
    if (success) break
  }

  if (!success) {
    # Last resort: try using dev.new() to create a larger plot window
    tryCatch({
      # Reset to minimal margins
      par(mar = c(1, 1, 0.5, 0.2))
      # Try a simple plot without pages parameter
      plot(gam_model, select = select, shade = TRUE, scale = 0)
      cat("Plotted using fallback method without pages parameter\n")
      success <- TRUE
    }, error = function(e) {
      cat("Unable to create smooth term plots due to margin constraints.\n")
      cat("Try running: windows(width=12, height=8) or dev.new() before plotting\n")
      cat("Error:", e$message, "\n")
    })
  }

  invisible(success)
}

#' Calculate GAM Model Metrics
#'
#' @param gam_model A fitted GAM model object
#' @param test_data Optional test dataset for external validation
#' @return List of model performance metrics
calculate_gam_metrics <- function(gam_model, test_data = NULL) {
  # Get summary once to avoid multiple expensive calls
  model_summary <- summary(gam_model)

  # Training metrics
  train_metrics <- list(
    deviance_explained = model_summary$dev.expl,
    r_squared = model_summary$r.sq,
    aic = AIC(gam_model),
    bic = BIC(gam_model),
    gcv_score = gam_model$gcv.ubre,
    edf = sum(gam_model$edf),
    n_obs = nobs(gam_model)
  )

  # Test metrics if test data provided
  if (!is.null(test_data)) {
    test_pred <- predict(gam_model, newdata = test_data, type = "response")
    test_actual <- test_data$brownlow_votes

    test_metrics <- list(
      rmse = sqrt(mean((test_pred - test_actual)^2)),
      mae = mean(abs(test_pred - test_actual)),
      cor = cor(test_pred, test_actual)
    )

    # Add cached summary to test metrics
    test_metrics$cached_summary <- model_summary
    return(c(train_metrics, test_metrics))
  }

  # Add cached summary to train metrics
  train_metrics$cached_summary <- model_summary
  return(train_metrics)
}


#' Validate GAM Predictions
#'
#' @param gam_model A fitted GAM model object
#' @param validation_data Validation dataset
#' @param actual_col Name of actual values column
#' @return List with validation metrics and plots
validate_gam_predictions <- function(gam_model, validation_data, actual_col = "brownlow_votes") {
  # Generate predictions
  raw_predictions <- predict(gam_model, newdata = validation_data, type = "response")
  actual <- validation_data[[actual_col]]

  # Handle OCAT models (probability matrix) vs regular GAM (vector)
  if (is.matrix(raw_predictions)) {
    cat("OCAT model detected - converting probability matrix to expected votes\n")
    # Convert from 1,2,3,4 scale to 0,1,2,3 scale
    predictions <- apply(raw_predictions, 1, function(probs) {
      sum(c(0, 1, 2, 3) * probs)
    })
    # Also convert actual values from 1,2,3,4 scale to 0,1,2,3 scale
    actual <- actual
  } else {
    predictions <- as.numeric(raw_predictions)
  }

  # Calculate metrics
  rmse <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
  mae <- mean(abs(predictions - actual), na.rm = TRUE)
  correlation <- cor(predictions, actual, use = "complete.obs")

  cat(sprintf("Validation metrics: RMSE=%.4f, MAE=%.4f, Cor=%.4f\n", rmse, mae, correlation))

  # Create validation plot
  plot_data <- data.frame(actual = actual, predicted = predictions)
  val_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = actual, y = predicted)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
    ggplot2::labs(title = "Actual vs Predicted Values",
         x = "Actual Brownlow Votes",
         y = "Predicted Brownlow Votes",
         subtitle = paste("RMSE:", round(rmse, 3), "| MAE:", round(mae, 3), "| Cor:", round(correlation, 3))) +
    ggplot2::theme_minimal()

  # Residuals plot
  residuals <- actual - predictions
  resid_plot <- ggplot2::ggplot(data.frame(predicted = predictions, residuals = residuals),
                       ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue") +
    ggplot2::labs(title = "Residuals vs Predicted",
         x = "Predicted Values",
         y = "Residuals") +
    ggplot2::theme_minimal()

  return(list(
    metrics = list(rmse = rmse, mae = mae, correlation = correlation),
    validation_plot = val_plot,
    residuals_plot = resid_plot
  ))
}


#' Create GAM Feature Importance Plot
#'
#' @param gam_model A fitted GAM model object
#' @param top_n Number of top features to display
#' @return Feature importance plot
plot_gam_importance <- function(gam_model, top_n = 20) {
  # Extract smooth terms and their effective degrees of freedom
  smooth_terms <- gam_model$smooth
  edf_values <- gam_model$edf

  # Get parametric terms (exclude smooth terms and intercept)
  param_coefs <- coef(gam_model)
  param_terms <- names(param_coefs)
  param_terms <- param_terms[!grepl("\\)", param_terms)]  # Remove smooth terms

  # Ensure we have valid parametric terms
  param_terms <- param_terms[param_terms %in% names(param_coefs)]
  param_importance <- abs(param_coefs[param_terms])

  # Get smooth term labels - ensure they match edf_values length
  if (length(smooth_terms) > 0) {
    smooth_labels <- sapply(smooth_terms, function(x) x$label)
    # Ensure lengths match - trim if necessary
    min_length <- min(length(smooth_labels), length(edf_values))
    smooth_labels <- smooth_labels[1:min_length]
    smooth_edf <- edf_values[1:min_length]
  } else {
    smooth_labels <- character(0)
    smooth_edf <- numeric(0)
  }

  # Create importance data frame
  importance_df <- data.frame(
    term = c(param_terms, smooth_labels),
    importance = c(param_importance, smooth_edf),
    type = c(rep("Parametric", length(param_terms)), rep("Smooth", length(smooth_labels)))
  )

  # Select top features
  importance_df <- importance_df[order(-importance_df$importance), ]
  importance_df <- head(importance_df, top_n)
  importance_df$term <- factor(importance_df$term, levels = rev(importance_df$term))

  # Create plot
  ggplot2::ggplot(importance_df, ggplot2::aes(x = term, y = importance, fill = type)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "GAM Feature Importance",
         subtitle = "Based on |coefficient| for parametric terms and EDF for smooth terms",
         x = "Features",
         y = "Importance",
         fill = "Term Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
