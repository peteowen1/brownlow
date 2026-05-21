# 03 - GAM Model for Brownlow Medal Prediction ####
# This script implements a Generalized Additive Model (GAM) using mgcv with OCAT family
# for probability prediction with smooth terms and random effects for players, teams, and umpires.

# Setup ####

## Load required libraries ----
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(gratia)
library(stringr)
library(forcats)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create("plots/gam", showWarnings = FALSE, recursive = TRUE)
dir.create("models", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# Load Processed Data ####

if (!file.exists("data/processed/modeling_data.rds")) {
  stop("Processed data not found. Please run 02_base_feature_engineering.R first.")
}

cat("Loading processed data...")
prepared_data <- readRDS("data/processed/modeling_data.rds")
feature_metadata <- readRDS("data/processed/feature_metadata.rds")

cat(sprintf(" %d features, train: %d, test: %d\n",
            length(prepared_data$feature_cols),
            nrow(prepared_data$train),
            nrow(prepared_data$test)))

# Prepare Data for GAM ####

cat("Preparing GAM data...")
all_training_data <- prepared_data$train

# Prepare training data with proper factor handling
gam_data_train <- prepare_gam_training_data(
  all_training_data = all_training_data,
  target_col = prepared_data$target_col,
  feature_cols = prepared_data$feature_cols,
  prepared_data = prepared_data
)

# Prepare test data with matching factor levels
gam_data_test <- prepare_gam_test_data(
  test_data = prepared_data$test,
  gam_data_train = gam_data_train,
  target_col = prepared_data$target_col,
  feature_cols = prepared_data$feature_cols,
  prepared_data = prepared_data
)

available_features <- prepared_data$feature_cols

# Dataset alignment check
cat(sprintf("Dataset alignment check:\n"))
cat(sprintf("  GAM train: %d obs vs XGBoost train: %d obs\n",
            nrow(gam_data_train), nrow(all_training_data)))
cat(sprintf("  GAM test: %d obs vs XGBoost test: %d obs\n",
            nrow(gam_data_test), nrow(prepared_data$test)))

# Train GAM Model ####

# Define available features for model specification
feature_formula <- as.formula(paste0("votes ~ ",
  "s(coaches_votes, k=10, bs='ts') +",
  "s(supercoach_score, k=10, bs='ts') +",
  "s(afl_fantasy_score, k=10, bs='ts') +",
  "s(impact_score, k=10, bs='ts') +",
  "s(player_impact_share, k=10, bs='ts') +",
  "s(player_disposal_share, k=10, bs='ts') +",
  "s(rating_points, k=10, bs='ts') +",
  "s(career_avg_brownlow_votes_prior_seasons, k=10, bs='ts') +",
  "s(career_avg_coaches_votes, k=10, bs='ts') +",
  "s(career_avg_rating_points, k=10, bs='ts') +",
  "s(career_avg_supercoach_score, k=10, bs='ts') +",
  "s(career_games_with_goals, k=10, bs='ts') +",
  "s(recent_avg_impact, k=10, bs='ts') +",
  "s(score_involvement, k=10, bs='ts') +",
  "s(disposals, k=10, bs='ts') +",
  "s(goals, k=4, bs='ts') +",
  "s(player_goal_share, k=4, bs='ts') +",
  "s(metres_gained, k=8, bs='ts') +",
  "s(tackles, k=8, bs='ts') +",
  "s(marks, k=6, bs='ts') +",
  "s(kicks, k=6, bs='ts') +",
  "s(handballs, k=6, bs='ts') +",
  "s(inside_fifties, k=5, bs='ts') +",
  "s(total_score, k=10, bs='ts') +",
  "s(match_margin, k=10, bs='ts') +",
  "s(team_out_scored_opposition, bs='re') +",
  "s(team, bs='re') +",
  "s(player_name, bs='re')"
))

best_gam_model <- train_gam_with_validation(
  gam_data_train = gam_data_train,
  feature_formula = feature_formula,
  verbose = TRUE
)

if (is.null(best_gam_model)) {
  stop("GAM model training failed")
}

# Model Evaluation ####

cat("Generating predictions...")

# Validate features and clean test data
model_vars <- c('supercoach_score', 'afl_fantasy_score', 'impact_score', 'disposals',
               'metres_gained', 'player_career_games', 'match_margin', 'rating_points',
               'marks', 'kicks', 'handballs', 'tackles', 'inside_fifties',
               'score_involvement', 'coaches_votes')

gam_data_test_clean <- validate_gam_features(
  gam_data_test = gam_data_test,
  model_vars = model_vars,
  verbose = TRUE
)

# Validate that we have test data after cleaning
if (nrow(gam_data_test_clean) == 0) {
  stop("No valid test data remaining after cleaning")
}

# Generate probability predictions (OCAT always returns probability matrix)
predictions <- predict(best_gam_model, newdata = gam_data_test_clean, type = "response")

# Validate predictions matrix before proceeding
if (is.null(predictions) || !is.matrix(predictions) || nrow(predictions) == 0) {
  stop("GAM model failed to generate valid predictions matrix")
}

# Validate that predictions matrix has correct number of columns (4 for votes 0,1,2,3)
if (ncol(predictions) != 4) {
  stop(sprintf("GAM predictions matrix has %d columns, expected 4 (for votes 0,1,2,3)",
               ncol(predictions)))
}

colnames(predictions) <- paste0("Prob_", 0:3, "_votes")

cat(sprintf(" %d predictions\n", nrow(predictions)))

# Enhanced probability normalization for GAM predictions
cat("Applying enhanced probability normalization...")

# Create match IDs for normalization
match_ids <- if ("match_id" %in% names(gam_data_test_clean)) {
  gam_data_test_clean$match_id
} else {
  paste(gam_data_test_clean$season, gam_data_test_clean$match_round, sep = "_R")
}

# Validate match IDs length matches predictions
if (length(match_ids) != nrow(predictions)) {
  stop(sprintf("Match IDs length (%d) doesn't match predictions rows (%d)",
               length(match_ids), nrow(predictions)))
}

# Apply enhanced normalization to GAM probabilities
gam_normalization_result <- normalize_probability_matrix(
  prob_matrix = predictions,
  match_ids = match_ids,
  verbose = TRUE
)

# Update predictions with normalized probabilities
predictions_normalized <- gam_normalization_result$normalized_probabilities
colnames(predictions_normalized) <- paste0("Prob_", 0:3, "_votes_norm")

# Use normalized probabilities for class predictions
test_pred_gam <- apply(predictions_normalized, 1, which.max) - 1

# Convert actual values to 0,1,2,3 scale
test_actual <- gam_data_test_clean[[prepared_data$target_col]]

# Log normalization results
if (gam_normalization_result$diagnostics$constraint_violations$all_constraints_satisfied) {
  cat(" ✅ constraints satisfied\n")
} else {
  cat(sprintf(" ⚠️ %d constraint violations\n",
              gam_normalization_result$diagnostics$row_violations_count +
              gam_normalization_result$diagnostics$vote_total_violations))
}

# Evaluate performance using normalized probability matrix
cat("Evaluating performance...")
test_metrics_gam <- evaluate_model(
  actual = test_actual,
  predicted = predictions_normalized
)

cat(sprintf(" log-loss: %.3f, accuracy: %.3f\n",
            ifelse(is.na(test_metrics_gam$log_loss), 999, test_metrics_gam$log_loss),
            test_metrics_gam$overall_accuracy))

# Feature Importance and Diagnostics ####

cat("Analyzing feature importance...")
importance_plot_gam <- plot_gam_importance(best_gam_model, top_n = 6)
ggsave("plots/gam/gam_feature_importance.png", importance_plot_gam,
       width = 12, height = 8)
cat(" ✓\n")

cat("Creating diagnostics...")
convergence_info <- check_gam_convergence(best_gam_model)

validation_results <- validate_gam_predictions(
  gam_model = best_gam_model,
  validation_data = gam_data_test_clean,
  actual_col = prepared_data$target_col
)

ggsave("plots/gam/predicted_vs_actual.png", validation_results$validation_plot,
       width = 8, height = 6)
ggsave("plots/gam/residuals_plot.png", validation_results$residuals_plot,
       width = 8, height = 6)

cat(sprintf(" RMSE: %.3f\n", validation_results$metrics$rmse))

# Smooth Term Visualization ####

cat("Plotting smooth terms...")
# plot_gam_smooths(best_gam_model, select = NULL, pages = 4)
cat(" ✓\n")

# Final Model Metrics ####

cat("Computing final metrics...")
final_metrics <- calculate_gam_metrics(best_gam_model, gam_data_test_clean)
cat(sprintf(" deviance: %.1f%%, AIC: %.0f\n",
            final_metrics$deviance_explained * 100,
            final_metrics$aic))

# Save Results ####

cat("Saving results...")
# Save the GAM model
saveRDS(best_gam_model, "models/gam_final_model.rds")

# Create standardized results with enhanced probability information
gam_results <- create_standardized_results(
  model_name = "gam",
  model_object = best_gam_model,
  train_pred = NULL,  # GAM script doesn't generate train predictions
  test_pred = test_pred_gam,
  val_pred = NULL,    # GAM script doesn't generate val predictions
  train_pred_probs = NULL,
  test_pred_probs = predictions_normalized,  # Use normalized probabilities
  val_pred_probs = NULL,
  feature_cols = available_features,
  train_metrics = NULL,
  test_metrics = test_metrics_gam,
  val_metrics = NULL,
  hyperparameters = list(model_name = "team_player_re"),
  additional_info = list(
    final_metrics = final_metrics,
    validation_results = validation_results,
    model_summary = final_metrics$cached_summary,  # Use cached summary from calculate_gam_metrics
    test_data = gam_data_test_clean,  # Keep for compatibility
    # Enhanced probability information
    original_probabilities = predictions,  # Store original for comparison
    normalized_probabilities = predictions_normalized,
    normalization_diagnostics = gam_normalization_result$diagnostics,
    expected_votes = gam_normalization_result$expected_votes
  )
)

# Save using standardized function
save_standardized_results(gam_results, save_model = TRUE)

cat(sprintf(" ✓ Log-loss: %.3f, Deviance: %.1f%%, Test accuracy: %.1f%%\n",
            ifelse(is.na(test_metrics_gam$log_loss), 999, test_metrics_gam$log_loss),
            final_metrics$deviance_explained * 100,
            test_metrics_gam$overall_accuracy * 100))

# Retrain Production Model ####

cat("Retraining production GAM model on complete dataset...")

# Combine all available data (training + test)
complete_gam_data <- bind_rows(gam_data_train, gam_data_test_clean) %>%
  dplyr::filter(season != 2025)  # Exclude any incomplete future seasons

cat(sprintf(" using %d observations from all seasons\n", nrow(complete_gam_data)))

# Retrain GAM model with same specification on complete dataset
production_gam_model <- train_gam_with_validation(
  gam_data_train = complete_gam_data,
  feature_formula = feature_formula,
  verbose = FALSE
)

# Save production model (overwrites evaluation model)
saveRDS(production_gam_model, "models/gam_production_model.rds")

cat(" ✓ Production GAM model saved with complete dataset\n")

# Save GAM Probability DataFrame ####
cat("\n=== SAVING GAM PROBABILITIES ===\n")

# Use ALL the data we already have (including 2025!)
all_data <- bind_rows(prepared_data$train, prepared_data$test, prepared_data$holdout)

# Prepare GAM data using the same function as training
gam_all_data <- prepare_gam_training_data(
  all_training_data = all_data,
  target_col = prepared_data$target_col,
  feature_cols = prepared_data$feature_cols,
  prepared_data = prepared_data
)

# Generate GAM probabilities for all seasons
gam_probs <- predict(production_gam_model, newdata = gam_all_data, type = "response")

# Convert to probability matrix (simplified approach)
if (is.vector(gam_probs)) {
  # Simple fallback: create probability distributions based on predictions
  gam_prob_matrix <- matrix(0, nrow = length(gam_probs), ncol = 4)

  # Vectorized approach instead of for loop
  pred_vals <- pmax(0, pmin(3, round(gam_probs)))

  # Create one-hot encoding
  gam_prob_matrix[cbind(1:length(gam_probs), pred_vals + 1)] <- 1
} else {
  gam_prob_matrix <- gam_probs
}

# Apply probability normalization to ensure 6 votes per match
cat("Normalizing GAM probabilities...\n")
gam_norm_result <- normalize_probability_matrix(
  prob_matrix = gam_prob_matrix,
  match_ids = all_data$match_id,
  max_votes_per_match = 6,
  verbose = TRUE
)

# Create GAM probability dataframe with normalized probabilities
gam_prob_df <- all_data %>%
  select(player_name, team, date, match_id, season, brownlow_votes) %>%
  bind_cols(
    as.data.frame(gam_norm_result$normalized_probabilities) %>%
      setNames(c("gam_prob_0", "gam_prob_1", "gam_prob_2", "gam_prob_3"))
  ) %>%
  mutate(
    gam_expected_votes = gam_prob_0 * 0 + gam_prob_1 * 1 + gam_prob_2 * 2 + gam_prob_3 * 3
  )

# Save GAM probabilities
saveRDS(gam_prob_df, "results/gam_probabilities.rds")
write_csv(gam_prob_df, "results/gam_probabilities.csv")

cat(sprintf("✓ GAM probabilities saved: %d rows\n", nrow(gam_prob_df)))
