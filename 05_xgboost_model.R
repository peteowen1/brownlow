# 05 - XGBoost Multinomial Model for Brownlow Medal Prediction ####
# This script implements a multinomial XGBoost model with hyperparameter tuning
# for predicting AFL Brownlow Medal vote probabilities across classes (0, 1, 2, 3).

# Setup ####

## Load required libraries ----
library(xgboost)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(DALEX)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

# Seeds averaged for the shipped probabilities. Five captures most of the
# available variance reduction — ten buys only a further 0.00002 — at half the
# fits. Raise it if compute is free; the returns diminish rather than vanish.
PRODUCTION_SEEDS <- 1:5

## Configuration parameters ----
rerun_tuning <- FALSE  # Set to TRUE to force hyperparameter tuning rerun

## Create necessary directories ----
dir.create("models", showWarnings = FALSE)
dir.create("plots/xgboost", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE)

# Load Processed Data ####

if (!file.exists("data/processed/enhanced_modeling_data.rds")) {
  stop("Enhanced features not found. Please run 04_enhanced_feature_engineering.R first.")
}

cat("Loading enhanced features...")
prepared_data <- readRDS("data/processed/enhanced_modeling_data.rds")
feature_metadata <- readRDS("data/processed/enhanced_feature_metadata.rds")

cat(sprintf(" %d features, train: %d, test: %d\n",
            length(prepared_data$feature_cols),
            nrow(prepared_data$train),
            nrow(prepared_data$test)))

# Setup Cross-Validation ####

cat("Setting up cross-validation...")

# prepare_modeling_data() already holds the target season out of $train, so this
# is just the train seasons. Used for tuning and for the evaluation model only.
all_training_data <- prepared_data$train
stopifnot(!any(prepared_data$holdout_seasons %in% all_training_data$season))

cv_folds <- create_time_aware_cv(
  data = all_training_data,
  target_col = prepared_data$target_col,
  feature_cols = prepared_data$feature_cols,
  n_folds = 5,
  min_train_seasons = 3
)

cat(sprintf(" %d folds from seasons %s\n",
            length(cv_folds),
            paste(range(all_training_data$season), collapse = "-")))

# Hyperparameter Tuning ####

tuning_file <- "models/xgboost_tuning_results.rds"

if (file.exists(tuning_file) && !rerun_tuning) {
  cat("Loading cached tuning results...")
  tuning_results <- readRDS(tuning_file)
  cat(" ✓\n")
} else {
  cat("Hyperparameter tuning...")
  param_grid <- list(
    max_depth = c(3, 4, 5, 6),
    eta = c(0.01,0.025, 0.05, 0.1, 0.2),
    subsample = c(0.4, 0.6, 0.8),
    colsample_bytree = c(0.4, 0.6, 0.8),
    min_child_weight = c(1, 3, 5),
    gamma = c(0, 0.1, 0.2),
    lambda = c(0.1, 1, 2),
    alpha = c(0, 0.1, 0.5)
  )

  tuning_results <- tune_xgboost_hyperparameters(
    train_data = all_training_data,
    feature_cols = prepared_data$feature_cols,
    target_col = prepared_data$target_col,
    cv_folds = cv_folds,
    param_grid = param_grid,
    n_iter = 50,
    model_type = "multinomial"
  )

  saveRDS(tuning_results, tuning_file)
  cat(" ✓\n")
}

cat(sprintf("Best CV log-loss: %.3f, optimal rounds: %d\n",
            tuning_results$best_cv_score, tuning_results$best_iteration))

# Train Final Model ####

tictoc::tic()
cat("Training final model...")

final_model <- train_final_xgboost(
  train_data = all_training_data,
  feature_cols = prepared_data$feature_cols,
  target_col = prepared_data$target_col,
  params = tuning_results$best_params,
  nrounds = tuning_results$best_iteration
)
saveRDS(final_model, "models/xgboost_final_model.rds")
cat(" ✓\n")

tictoc::toc()
# Model Evaluation ####

predictions <- prepare_xgboost_predictions(
  final_model = final_model,
  train_data = all_training_data,
  test_data = prepared_data$test,
  feature_cols = prepared_data$feature_cols,
  target_col = prepared_data$target_col,
  verbose = TRUE
)

evaluation_results <- evaluate_xgboost_performance(
  predictions = predictions,
  train_data = all_training_data,
  test_data = prepared_data$test,
  final_model = final_model,
  feature_cols = prepared_data$feature_cols,
  target_col = prepared_data$target_col,
  save_plots = TRUE,
  verbose = TRUE
)

# Prediction Analysis and Constraints ####

cat("Analyzing predictions...")
prediction_analysis <- analyze_xgboost_predictions(
  test_data = prepared_data$test,
  predictions = predictions$test_pred_class,
  actual = prepared_data$test[[prepared_data$target_col]],
  is_multinomial = TRUE
)

cat(sprintf(" %d correct class predictions, %d incorrect\n",
            sum(predictions$test_pred_class == prepared_data$test[[prepared_data$target_col]]),
            sum(predictions$test_pred_class != prepared_data$test[[prepared_data$target_col]])))

# Enhanced probability normalization for XGBoost predictions
cat("Applying enhanced probability normalization...")

# Create match IDs for normalization
match_ids <- if ("match_id" %in% names(prepared_data$test)) {
  prepared_data$test$match_id
} else {
  paste(prepared_data$test$season, prepared_data$test$home_team, prepared_data$test$away_team, sep = "_")
}

# Apply enhanced normalization to XGBoost probabilities
xgb_normalization_result <- normalize_probability_matrix(
  prob_matrix = predictions$test_pred_matrix,
  match_ids = match_ids,
  verbose = TRUE
)

# Update predictions with normalized probabilities
predictions$test_pred_matrix_norm <- xgb_normalization_result$normalized_probabilities
colnames(predictions$test_pred_matrix_norm) <- paste0("Prob_", 0:3, "_votes_norm")

# Generate class predictions from normalized probabilities
predictions$test_pred_class_norm <- apply(predictions$test_pred_matrix_norm, 1, which.max) - 1

# Log normalization results
if (xgb_normalization_result$diagnostics$constraint_violations$all_constraints_satisfied) {
  cat(" ✅ constraints satisfied\n")
} else {
  cat(sprintf(" ⚠️ constraint violations detected\n"))
}

# Evaluate performance using normalized probabilities
cat("Evaluating normalized predictions...")
test_metrics_norm <- evaluate_model(
  actual = prepared_data$test[[prepared_data$target_col]],
  predicted = predictions$test_pred_matrix_norm
)

cat(sprintf(" normalized accuracy: %.3f -> %.3f\n",
            evaluation_results$test_metrics$overall_accuracy,
            test_metrics_norm$overall_accuracy))

# Save Results ####

cat("Saving results...")

# Create standardized results with enhanced probability information
xgboost_results <- create_standardized_results(
  model_name = "xgboost",
  model_object = final_model,
  train_pred = predictions$train_pred_class,
  test_pred = predictions$test_pred_class_norm,  # Use normalized class predictions
  train_pred_probs = predictions$train_pred_matrix,
  test_pred_probs = predictions$test_pred_matrix_norm,  # Use normalized probabilities
  feature_cols = prepared_data$feature_cols,
  train_metrics = evaluation_results$train_metrics,
  test_metrics = test_metrics_norm,  # Use normalized test metrics
  hyperparameters = tuning_results$best_params,
  additional_info = list(
    cv_score = tuning_results$best_cv_score,
    feature_importance = evaluation_results$importance,
    prediction_analysis = prediction_analysis,
    prob_analysis = evaluation_results$prob_analysis,
    # Enhanced probability information
    original_probabilities = predictions$test_pred_matrix,  # Store original for comparison
    normalized_probabilities = predictions$test_pred_matrix_norm,
    normalization_diagnostics = xgb_normalization_result$diagnostics,
    expected_votes = xgb_normalization_result$expected_votes,
    original_test_metrics = evaluation_results$test_metrics  # Keep original for comparison
  )
)

# Save using standardized function
save_standardized_results(xgboost_results, save_model = TRUE)

cat(sprintf(" ✓ Normalized Log-loss: %.3f, Test accuracy: %.1f%% -> %.1f%%, Top feature: %s\n",
            ifelse(is.na(test_metrics_norm$log_loss), 999, test_metrics_norm$log_loss),
            evaluation_results$test_metrics$overall_accuracy * 100,
            test_metrics_norm$overall_accuracy * 100,
            evaluation_results$importance$Feature[1]))

# Retrain Production Model ####

# final_model above saw only the train seasons, so its test-season metrics are
# honest. It must NOT be the model that ships: it has never seen the two most
# recent completed seasons. production_model is refit on train + test and is the
# only model used for the probabilities below.
production_model <- train_production_xgboost(
  train_data = prepared_data$train,
  test_data = prepared_data$test,
  feature_cols = prepared_data$feature_cols,
  target_col = prepared_data$target_col,
  best_params = tuning_results$best_params,
  best_iteration = tuning_results$best_iteration,
  exclude_seasons = prepared_data$holdout_seasons,
  verbose = TRUE
)

imp <- xgboost::xgb.importance(model = production_model)
imp %>% tibble::as_tibble() %>% dplyr::arrange(dplyr::desc(Gain)) %>% print(n = 50)

# Save XGBoost Probability DataFrame ####
cat("\n=== SAVING XGBOOST PROBABILITIES ===\n")

all_data <- bind_rows(prepared_data$train, prepared_data$test, prepared_data$holdout)

# Generate XGBoost probabilities for all seasons from the PRODUCTION model.
# xgboost >= 2.1 returns the multi-class matrix already shaped; the old
# reshape = TRUE argument has been removed.
# Averaged over seeds rather than taken from the single production_model fit.
# Across 10 seeds the 2025 holdout RMSE ranged 0.35464-0.35664 -- a spread of
# 0.00200, larger than every improvement this project has banked combined. A
# single set.seed(42) draw made the submission a lottery. Averaging removes it
# and beats the expected single seed by 0.00028. See R/seed_averaging.R and
# POSTMORTEM Sec 5p.
#
# production_model is still trained above and still ships: it is the object
# used for variable importance, and it is the first of the averaged fits.
cat(sprintf("Averaging XGBoost probabilities over %d seeds...\n", length(PRODUCTION_SEEDS)))
seed_avg <- train_seed_averaged_xgboost(
  train_data   = dplyr::bind_rows(prepared_data$train, prepared_data$test),
  newdata      = all_data,
  feature_cols = prepared_data$feature_cols,
  target_col   = prepared_data$target_col,
  params       = tuning_results$best_params,
  nrounds      = tuning_results$best_iteration,
  seeds        = PRODUCTION_SEEDS,
  verbose      = TRUE
)
xgb_probs <- seed_avg$prob_matrix

# Apply probability normalization to ensure 6 votes per match
cat("Normalizing XGBoost probabilities...\n")
xgb_norm_result <- normalize_probability_matrix(
  prob_matrix = xgb_probs,
  match_ids = all_data$match_id,
  max_votes_per_match = 6,
  verbose = TRUE
)

# Create XGBoost probability dataframe with normalized probabilities
xgboost_prob_df <- all_data %>%
  select(player_name, team, date, match_id, season, brownlow_votes) %>%
  bind_cols(
    as.data.frame(xgb_norm_result$normalized_probabilities) %>%
      setNames(c("xgboost_prob_0", "xgboost_prob_1", "xgboost_prob_2", "xgboost_prob_3"))
  ) %>%
  mutate(
    xgboost_expected_votes = xgboost_prob_0 * 0 + xgboost_prob_1 * 1 + xgboost_prob_2 * 2 + xgboost_prob_3 * 3
  )

# Stamp how these probabilities were produced. Without this, 06's staleness
# check compares hyperparameters and round count but has no way to know the
# PROCEDURE changed -- it would silently accept a weight derived against a
# single-fit model now that production averages over PRODUCTION_SEEDS.
attr(xgboost_prob_df, "seed_provenance") <- list(
  n_seeds = length(PRODUCTION_SEEDS),
  seeds   = PRODUCTION_SEEDS
)

# Save XGBoost probabilities
saveRDS(xgboost_prob_df, "results/xgboost_probabilities.rds")
write_csv(xgboost_prob_df, "results/xgboost_probabilities.csv")

cat(sprintf("✓ XGBoost probabilities saved: %d rows\n", nrow(xgboost_prob_df)))

