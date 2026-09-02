# 06 - Ensemble Predictions for Brownlow Medal Competition ####
# This script combines XGBoost and GAM probability models, optimizes ensemble weights
# based on multinomial log-loss, and generates final predictions for competition submission.

# Setup ####

## Load required libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create("plots/ensemble", showWarnings = FALSE, recursive = TRUE)

# Load Models and Results ####

model_results <- load_ensemble_models(verbose = TRUE)

# Optimize Ensemble Weights ####

ensemble_weights <- optimize_ensemble_weights(
  model_results = model_results,
  optimization_metric = "log_loss",
  weight_constraints = list(min = 0.1, max = 0.9),
  verbose = TRUE
)

# Generate Ensemble Predictions ####

ensemble_predictions <- generate_ensemble_predictions(
  model_results = model_results,
  ensemble_weights = ensemble_weights,
  verbose = TRUE
)

# Evaluate Ensemble Performance ####

performance_summary <- create_ensemble_performance_summary(
  model_results = model_results,
  ensemble_results = ensemble_predictions,
  actual_values = model_results[[names(model_results)[1]]]$test_data[[model_results[[names(model_results)[1]]]$target_col]],
  save_summary = TRUE
)

# Enhanced Ensemble Normalization ####

cat("\n=== APPLYING ENHANCED ENSEMBLE NORMALIZATION ===\n")

# Load test data with proper match IDs
prepared_data <- readRDS("data/processed/modeling_data.rds")
test_data <- prepared_data$test
match_ids <- test_data$match_id

# Apply enhanced ensemble probability normalization
if (!is.null(ensemble_predictions$test_pred_probs)) {
  cat("Normalizing ensemble probabilities with dual constraints...\n")

  # Use the enhanced normalization for ensemble probabilities
  ensemble_norm_result <- normalize_probability_matrix(
    prob_matrix = ensemble_predictions$test_pred_probs,
    match_ids = match_ids,
    max_votes_per_match = 6,
    verbose = TRUE
  )

  # Update ensemble predictions with normalized results
  constrained_predictions <- ensemble_norm_result$expected_votes
  normalized_probabilities <- ensemble_norm_result$normalized_probabilities

  # Enhanced constraint validation
  if (ensemble_norm_result$diagnostics$constraint_violations$all_constraints_satisfied) {
    cat("✅ All ensemble probability constraints satisfied\n")
  } else {
    cat("⚠️  Ensemble constraint violations detected:\n")
    cat(sprintf("  Row violations: %d\n", ensemble_norm_result$diagnostics$constraint_violations$row_violations_count))
    cat(sprintf("  Vote total violations: %d\n", ensemble_norm_result$diagnostics$constraint_violations$vote_total_violations))
  }

  constrained_ensemble <- list(
    constrained_predictions = constrained_predictions,
    normalized_probabilities = normalized_probabilities,
    normalization_diagnostics = ensemble_norm_result$diagnostics,
    expected_votes_by_player = ensemble_norm_result$expected_votes
  )

} else {
  # Fallback to legacy method if probabilities not available
  cat("No ensemble probabilities found, using legacy constraint method...\n")
  constrained_predictions <- apply_voting_constraints(
    predictions = ensemble_predictions$test_pred,
    match_ids = match_ids,
    method = "proportional",
    max_votes_per_match = 6
  )

  constrained_ensemble <- list(
    constrained_predictions = constrained_predictions
  )
}

# Create Ensemble Probability DataFrame ####

cat("\n=== CREATING ENSEMBLE PROBABILITIES ===\n")

# Load pre-saved XGBoost and GAM probabilities
xgboost_prob_df <- readRDS("results/xgboost_probabilities.rds")
gam_prob_df <- readRDS("results/gam_probabilities.rds")

cat(sprintf("✓ XGBoost probabilities loaded: %d rows\n", nrow(xgboost_prob_df)))
cat(sprintf("✓ GAM probabilities loaded: %d rows\n", nrow(gam_prob_df)))

# Combine using ensemble weights
#
# optimize_ensemble_weights() above fits the blend on the models' stored
# 2023-24 probabilities -- but stage 05 produces those from a model trained on
# 2015-2024, so it is selecting on rows the booster memorised. XGBoost scores
# 0.3357 there against 0.3577 out of sample while the GAM barely moves, so the
# search runs monotonically to the 0.9 cap and only the cap stops it going to
# 1.0. See POSTMORTEM-2025.md Sec 5j.
#
# WALKFORWARD_XGB_WEIGHT is chosen instead on genuinely out-of-sample
# predictions (2023 scored by models trained on < 2023, 2024 on < 2024) by
# analysis/ensemble_weight_walkforward.R. Measured on the 2025 holdout, weight
# never fitted on it: 0.35647 against 0.35702 for the in-sample-selected
# weight that used to ship -- worth 0.00055.
#
# Set USE_WALKFORWARD_WEIGHT <- FALSE to fall back to the old behaviour.
USE_WALKFORWARD_WEIGHT <- TRUE
WALKFORWARD_XGB_WEIGHT <- 0.40

ensemble_weights_values <- ensemble_weights$weights
if (USE_WALKFORWARD_WEIGHT) {
  xgb_weight <- WALKFORWARD_XGB_WEIGHT
  gam_weight <- 1 - WALKFORWARD_XGB_WEIGHT
  cat(sprintf("Ensemble weights (walk-forward, Sec 5j): XGBoost=%.3f, GAM=%.3f  [in-sample search wanted %.3f]\n",
              xgb_weight, gam_weight, ensemble_weights_values[1]))
} else {
  xgb_weight <- ensemble_weights_values[1]
  gam_weight <- ensemble_weights_values[2]
  cat(sprintf("Ensemble weights (in-sample search, LEAKY -- see Sec 5j): XGBoost=%.3f, GAM=%.3f\n",
              xgb_weight, gam_weight))
}

# Create ensemble probabilities by weighted combination.
#
# JOIN, do not bind by position. These two frames are built by separate scripts
# (03 and 05) from separately prepared data; that they happen to share a row
# order today is luck, not a guarantee. A reordering in either would silently
# pair one player's XGBoost probabilities with another's GAM probabilities, and
# nothing downstream would notice — the result still normalises to 6 votes per
# match and passes every constraint check.
combined <- xgboost_prob_df %>%
  select(player_name, team, date, match_id, season, brownlow_votes,
         dplyr::starts_with("xgboost_prob_")) %>%
  inner_join(
    gam_prob_df %>% select(player_name, team, date, match_id,
                           dplyr::starts_with("gam_prob_")),
    by = c("player_name", "team", "date", "match_id")
  )

if (nrow(combined) != nrow(xgboost_prob_df)) {
  stop(sprintf(paste0("Ensemble join changed the row count: %d -> %d. ",
                      "Either the two model outputs disagree on their rows, or ",
                      "(player, team, date, match_id) is not unique."),
               nrow(xgboost_prob_df), nrow(combined)))
}

ensemble_prob_df <- combined %>%
  mutate(
    ensemble_prob_0 = xgb_weight * xgboost_prob_0 + gam_weight * gam_prob_0,
    ensemble_prob_1 = xgb_weight * xgboost_prob_1 + gam_weight * gam_prob_1,
    ensemble_prob_2 = xgb_weight * xgboost_prob_2 + gam_weight * gam_prob_2,
    ensemble_prob_3 = xgb_weight * xgboost_prob_3 + gam_weight * gam_prob_3,
    ensemble_expected_votes = ensemble_prob_0 * 0 + ensemble_prob_1 * 1 + ensemble_prob_2 * 2 + ensemble_prob_3 * 3
  )

# Save ensemble probabilities
saveRDS(ensemble_prob_df, "results/ensemble_probabilities.rds")
write_csv(ensemble_prob_df, "results/ensemble_probabilities.csv")

cat(sprintf("✓ Ensemble probabilities saved: %d rows\n", nrow(ensemble_prob_df)))
cat("Columns: player_name, team, date, match_id, season, brownlow_votes, ensemble_prob_0-3, ensemble_expected_votes\n")

# Save Ensemble Results ####

ensemble_results_final <- save_ensemble_results(
  ensemble_weights = ensemble_weights,
  ensemble_predictions = ensemble_predictions,
  constrained_ensemble = constrained_ensemble,
  performance_summary = performance_summary,
  model_results = model_results,
  verbose = TRUE
)