# 07 - AFL Season Predictions for Brownlow Medal ####
# This script generates predictions for any AFL season using trained models
# and creates a comprehensive player vote expectation table.

# Setup ####

## Configuration parameters ----
target_season <- 2025         # Season to generate predictions for
prediction_type <- ifelse(target_season < 2025, 'historical', "future")    # "future" for upcoming seasons, "historical" for past seasons

## Load required libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(forcats)
library(xgboost)
library(mgcv)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create(sprintf("predictions/%d", target_season), showWarnings = FALSE, recursive = TRUE)
dir.create(sprintf("plots/%d", target_season), showWarnings = FALSE, recursive = TRUE)

# Load Trained Models ####

cat("Loading models...")
trained_models_raw <- load_trained_models(verbose = TRUE)

# Fix naming mismatch for generate_model_predictions
trained_models_data <- list(
  XGBoost = trained_models_raw$xgboost,  # Uppercase for compatibility
  GAM = trained_models_raw$gam,
  ensemble = trained_models_raw$ensemble
)

# Load Target Season Data ####
# Note: Skipping target_season_data loading since features_data contains everything needed

# cat(sprintf("Loading %d data...", target_season))
# target_season_data <- load_season_data(target_season, prediction_type, verbose = TRUE)

# Load Features for Prediction ####

cat("Loading features...")
features_data <- load_features_for_prediction(target_season, verbose = TRUE)

if (is.null(features_data)) {
  stop(sprintf("Could not load features for %d season. Please run feature engineering first.", target_season))
}

# Prepare clean prediction dataset
prediction_data <- features_data$features %>%
  select(
    # Identifiers
    date, season, match_round, team, player_name,
    # Match identifier (if available)
    any_of("match_id"),
    # Features
    all_of(intersect(features_data$feature_cols, names(.)))
  ) %>%
  # Handle missing values
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) #%>% dplyr::filter(complete.cases(.))

cat(sprintf("✓ Prediction dataset prepared: %d observations, %d features\n",
            nrow(prediction_data), length(features_data$feature_cols)))

# Generate Model Predictions ####

cat("Generating predictions...")
pred_start <- Sys.time()

# Generate predictions using utility function
predictions <- generate_model_predictions(
  prediction_data = prediction_data,
  trained_models = trained_models_data,
  feature_cols = features_data$feature_cols,
  verbose = TRUE
)

# Apply Enhanced AFL voting constraints with comprehensive validation
cat("\n=== APPLYING ENHANCED CONSTRAINT VALIDATION ===\n")

constrained_results <- apply_prediction_constraints(
  prob_matrices = list(
    xgboost_probs = predictions$xgboost_probs,
    gam_probs = predictions$gam_probs,
    ensemble_probs = predictions$ensemble_probs
  ),
  prediction_data = prediction_data,
  verbose = TRUE,
  return_full_diagnostics = TRUE  # Get detailed validation
)

# Extract predictions for backward compatibility
constrained_predictions <- constrained_results$predictions

# Enhanced constraint validation reporting
cat("\n=== SEASON PREDICTION CONSTRAINT VALIDATION ===\n")
cat(sprintf("Total players processed: %d\n", constrained_results$summary$total_players))
cat(sprintf("Total matches: %d\n", constrained_results$summary$total_matches))
cat(sprintf("Models processed: %s\n", paste(constrained_results$summary$models_processed, collapse = ", ")))

# Check and report constraint satisfaction for each model
for (model_name in names(constrained_results$diagnostics)) {
  diagnostics <- constrained_results$diagnostics[[model_name]]
  cat(sprintf("\n%s Model Validation:\n", stringr::str_to_title(model_name)))
  cat(sprintf("  All constraints satisfied: %s\n",
              ifelse(diagnostics$constraint_violations$all_constraints_satisfied, "✅ YES", "⚠️  NO")))

  if (!diagnostics$constraint_violations$all_constraints_satisfied) {
    cat(sprintf("  Row constraint violations: %d (max error: %.2e)\n",
                diagnostics$row_violations_count, diagnostics$row_violations_max))
    cat(sprintf("  Match constraint violations: P1=%d, P2=%d, P3=%d\n",
                diagnostics$match_violations_p1, diagnostics$match_violations_p2, diagnostics$match_violations_p3))
    cat(sprintf("  Vote total violations: %d (max error: %.2e)\n",
                diagnostics$vote_total_violations, diagnostics$vote_total_max_error))
    cat(sprintf("  Vote limit violations: %d (max votes: %.3f)\n",
                diagnostics$vote_limit_violations, diagnostics$vote_limit_max))
  }
}

# Overall system validation
if (constrained_results$summary$all_constraints_satisfied) {
  cat("\n✅ ALL SEASON PREDICTION CONSTRAINTS SUCCESSFULLY SATISFIED\n")
  cat("Ready for competition submission and simulation analysis.\n")
} else {
  cat("\n⚠️  CONSTRAINT VIOLATIONS DETECTED\n")
  cat("Review model outputs before proceeding to submission.\n")
}

pred_time <- difftime(Sys.time(), pred_start, units = "secs")
cat(sprintf("✓ All predictions generated in %.1f seconds\n", as.numeric(pred_time)))

# Create Season Predictions Summary ####

cat("Creating summary...")

# Combine all predictions
season_predictions <- prediction_data %>%
  select(date, any_of("match_id"), season, match_round, team, player_name) %>%
  mutate(
    xgboost_pred = constrained_predictions$xgboost_probs,
    gam_pred = constrained_predictions$gam_probs,
    ensemble_pred = constrained_predictions$ensemble_probs,
    prediction_date = Sys.Date()
  )

# Player Total Expected Votes ####

cat("Calculating expectations...")

# Calculate total expected votes by player
player_vote_expectations <- season_predictions %>%
  group_by(player_name, team) %>%
  summarise(
    games_predicted = n(),
    xgboost_total = sum(xgboost_pred, na.rm = TRUE),
    gam_total = sum(gam_pred, na.rm = TRUE),
    ensemble_total = sum(ensemble_pred, na.rm = TRUE),
    xgboost_avg = mean(xgboost_pred, na.rm = TRUE),
    gam_avg = mean(gam_pred, na.rm = TRUE),
    ensemble_avg = mean(ensemble_pred, na.rm = TRUE),
    max_single_game = max(ensemble_pred, na.rm = TRUE),
    vote_getting_games = sum(ensemble_pred > 0),
    vote_getting_rate = vote_getting_games / games_predicted,
    .groups = "drop"
  )

# Load actual Brownlow votes for comparison (if available)
cat("Loading actual votes for comparison...")
actual_votes <- tryCatch({
  # Use already loaded features data to extract actual votes
  if ("brownlow_votes" %in% names(features_data$features)) {
    features_data$features %>%
      dplyr::filter(season == target_season) %>%
      group_by(player_name, team) %>%
      summarise(
        actual_total_votes = sum(brownlow_votes, na.rm = TRUE),
        actual_games_with_votes = sum(brownlow_votes > 0, na.rm = TRUE),
        actual_max_single_game = max(brownlow_votes, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Fallback to basic dataset only if brownlow_votes not in features
    basic_data <- load_datathon_data("data/brownlow_datathon_dataset.csv")
    basic_data %>%
      dplyr::filter(season == target_season) %>%
      group_by(player_name, team) %>%
      summarise(
        actual_total_votes = sum(brownlow_votes, na.rm = TRUE),
        actual_games_with_votes = sum(brownlow_votes > 0, na.rm = TRUE),
        actual_max_single_game = max(brownlow_votes, na.rm = TRUE),
        .groups = "drop"
      )
  }
}, error = function(e) {
  cat(" no actual data available\n")
  return(NULL)
})

# Join actual votes with predictions
if (!is.null(actual_votes) && nrow(actual_votes) > 0) {
  player_vote_expectations <- player_vote_expectations %>%
    left_join(actual_votes, by = c("player_name", "team")) %>%
    mutate(
      # Calculate prediction errors (only for players with actual votes)
      ensemble_error = ifelse(!is.na(actual_total_votes),
                             (ensemble_total - actual_total_votes), NA),
      xgboost_error = ifelse(!is.na(actual_total_votes),
                            (xgboost_total - actual_total_votes), NA),
      gam_error = ifelse(!is.na(actual_total_votes),
                        (gam_total - actual_total_votes), NA),
      # Prediction accuracy indicators
      prediction_quality = case_when(
        is.na(actual_total_votes) ~ "No actual data",
        abs(ensemble_error) <= 2 ~ "Excellent (±2)",
        abs(ensemble_error) <= 5 ~ "Good (±5)",
        abs(ensemble_error) <= 10 ~ "Fair (±10)",
        TRUE ~ "Poor (>10)"
      )
    )

  actual_data_available <- sum(!is.na(player_vote_expectations$actual_total_votes))
  cat(sprintf(" ✓ %d players with actual votes\n", actual_data_available))
} else {
  # Add NA columns for consistency
  player_vote_expectations <- player_vote_expectations %>%
    mutate(
      actual_total_votes = NA_real_,
      actual_games_with_votes = NA_integer_,
      actual_max_single_game = NA_real_,
      ensemble_error = NA_real_,
      xgboost_error = NA_real_,
      gam_error = NA_real_,
      prediction_quality = "No actual data"
    )
  cat(" ✓ (no actual data for comparison)\n")
}

# Final ranking and medal probability
player_vote_expectations <- player_vote_expectations %>%
  arrange(desc(ensemble_total)) %>%
  mutate(
    rank = row_number(),
    medal_probability = case_when(
      rank == 1 ~ "High",
      rank <= 3 ~ "Medium",
      rank <= 10 ~ "Low",
      TRUE ~ "Very Low"
    )
  )

cat(sprintf("✓ Vote expectations calculated for %d players\n", nrow(player_vote_expectations)))

# Top players Analysis ####

top_players <- head(player_vote_expectations, 50)

View(player_vote_expectations)

# Display top players with actual vs predicted comparison
cat("\nTop 10 Players - Predicted vs Actual Votes:\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")

top_10_display <- head(player_vote_expectations, 10) %>%
  select(rank, player_name, team, ensemble_total, actual_total_votes,
         ensemble_error, prediction_quality, medal_probability)

if (any(!is.na(top_10_display$actual_total_votes))) {
  # If we have actual data, show comparison
  print(top_10_display %>%
    mutate(
      ensemble_total = round(ensemble_total, 1),
      actual_total_votes = ifelse(is.na(actual_total_votes), "TBD", as.character(actual_total_votes)),
      ensemble_error = ifelse(is.na(ensemble_error), "TBD", paste0("±", round(ensemble_error, 1)))
    ) %>%
    select(rank, player_name, team, ensemble_total, actual_total_votes,
           ensemble_error, prediction_quality) %>%
    rename(
      Rank = rank,
      Player = player_name,
      Team = team,
      Predicted = ensemble_total,
      Actual = actual_total_votes,
      Error = ensemble_error,
      Quality = prediction_quality
    ))
} else {
  # If no actual data, show predictions only
  print(top_10_display %>%
    select(rank, player_name, team, ensemble_total, medal_probability) %>%
    mutate(ensemble_total = round(ensemble_total, 1)) %>%
    rename(
      Rank = rank,
      Player = player_name,
      Team = team,
      Predicted_Votes = ensemble_total,
      Medal_Probability = medal_probability
    ))
}

# Team Analysis ####

team_expectations <- player_vote_expectations %>%
  group_by(team) %>%
  summarise(
    players = n(),
    Total_Expected_Votes = sum(ensemble_total, na.rm = TRUE),
    Avg_Votes_Per_Player = mean(ensemble_total, na.rm = TRUE),
    Top_Player_Votes = max(ensemble_total, na.rm = TRUE),
    medal_contenders = sum(rank <= 10),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Expected_Votes))

# Visualization ####

cat("Creating plots...")

# Top 15 players chart with actual vs predicted (if available)
top_15_data <- head(player_vote_expectations, 15)

if (any(!is.na(top_15_data$actual_total_votes))) {
  # Create comparison plot when actual data is available
  top_players_plot <- top_15_data %>%
    ggplot(aes(x = reorder(player_name, ensemble_total))) +
    geom_col(aes(y = ensemble_total, fill = "Predicted"), alpha = 0.7, width = 0.7) +
    geom_point(aes(y = actual_total_votes, color = "Actual"), size = 3, na.rm = TRUE) +
    geom_text(aes(y = ensemble_total, label = round(ensemble_total, 1)),
              hjust = -0.1, size = 2.5, color = "darkblue") +
    geom_text(aes(y = actual_total_votes, label = ifelse(!is.na(actual_total_votes),
                                                        as.character(actual_total_votes), "")),
              hjust = -0.3, size = 2.5, color = "darkred", na.rm = TRUE) +
    coord_flip() +
    scale_fill_manual(values = c("Predicted" = "#87CEEB")) +
    scale_color_manual(values = c("Actual" = "#FF6B6B")) +
    labs(title = sprintf("Top 15 Players - Predicted vs Actual Votes (%d)", target_season),
         subtitle = paste("Predictions from ensemble model, actual results comparison"),
         x = "Player", y = "Brownlow Votes",
         fill = "Vote Type", color = "Vote Type") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          legend.title = element_blank())
} else {
  # Original prediction-only plot
  top_players_plot <- top_15_data %>%
    ggplot(aes(x = reorder(player_name, ensemble_total), y = ensemble_total)) +
    geom_col(aes(fill = medal_probability), alpha = 0.8) +
    geom_text(aes(label = round(ensemble_total, 1)), hjust = -0.1, size = 3) +
    coord_flip() +
    scale_fill_manual(values = c("High" = "#FFD700", "Medium" = "#FFA500",
                                "Low" = "#87CEEB", "Very Low" = "#D3D3D3")) +
    labs(title = sprintf("Top 15 Players - Expected Brownlow Votes (%d)", target_season),
         subtitle = paste("Based on ensemble model predictions as of", Sys.Date()),
         x = "Player", y = "Expected Total Votes",
         fill = "Medal Probability") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10))
}

ggsave(sprintf("plots/%d/top_players_expected_votes.png", target_season), top_players_plot,
       width = 12, height = 8)

# Vote distribution by team
team_votes_plot <- team_expectations %>%
  head(15) %>%
  ggplot(aes(x = reorder(team, Total_Expected_Votes), y = Total_Expected_Votes)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(Total_Expected_Votes, 0)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = sprintf("Team Expected Votes - %d Season", target_season),
       subtitle = "Total expected Brownlow votes by team",
       x = "Team", y = "Total Expected Votes") +
  theme_minimal()

ggsave(sprintf("plots/%d/team_expected_votes.png", target_season), team_votes_plot,
       width = 10, height = 8)

# Player performance distribution
perf_dist_plot <- player_vote_expectations %>%
  dplyr::filter(games_predicted >= 10) %>%  # Filter to regular players
  ggplot(aes(x = ensemble_avg, y = vote_getting_rate)) +
  geom_point(aes(size = ensemble_total, color = medal_probability), alpha = 0.7) +
  scale_size_continuous(range = c(1, 8)) +
  scale_color_manual(values = c("High" = "#FFD700", "Medium" = "#FFA500",
                               "Low" = "#87CEEB", "Very Low" = "#D3D3D3")) +
  labs(title = sprintf("Player Performance Profile - %d Season", target_season),
       subtitle = "Average votes per game vs. vote-getting consistency",
       x = "Average Votes Per Game", y = "Vote-Getting Rate",
       size = "Total Expected Votes", color = "Medal Probability") +
  theme_minimal()

ggsave(sprintf("plots/%d/player_performance_profile.png", target_season), perf_dist_plot,
       width = 12, height = 8)

# Prediction accuracy analysis (if actual data available)
if (any(!is.na(player_vote_expectations$actual_total_votes))) {
  cat("Creating prediction accuracy analysis...")

  # Prediction accuracy scatter plot
  accuracy_plot <- player_vote_expectations %>%
    dplyr::filter(!is.na(actual_total_votes)) %>%
    ggplot(aes(x = ensemble_total, y = actual_total_votes)) +
    geom_point(aes(color = prediction_quality), size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
    geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
    scale_color_manual(values = c(
      "Excellent (±2)" = "#00AA00",
      "Good (±5)" = "#FFAA00",
      "Fair (±10)" = "#FF6600",
      "Poor (>10)" = "#AA0000"
    )) +
    labs(title = sprintf("Prediction Accuracy - %d Season", target_season),
         subtitle = "Predicted vs Actual Brownlow Votes (Perfect prediction = diagonal line)",
         x = "Predicted Total Votes", y = "Actual Total Votes",
         color = "Prediction Quality") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10))

  ggsave(sprintf("plots/%d/prediction_accuracy.png", target_season), accuracy_plot,
         width = 10, height = 8)

  # Summary statistics
  accuracy_stats <- player_vote_expectations %>%
    dplyr::filter(!is.na(actual_total_votes)) %>%
    summarise(
      players_with_data = n(),
      mean_error = mean(ensemble_error, na.rm = TRUE),
      median_error = median(ensemble_error, na.rm = TRUE),
      rmse = sqrt(mean(ensemble_error^2, na.rm = TRUE)),
      correlation = cor(ensemble_total, actual_total_votes, use = "complete.obs"),
      excellent_predictions = sum(prediction_quality == "Excellent (±2)", na.rm = TRUE),
      good_predictions = sum(prediction_quality %in% c("Excellent (±2)", "Good (±5)"), na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("\nPrediction Accuracy Summary (%d):\n", target_season))
  cat(sprintf("Players analyzed: %d\n", accuracy_stats$players_with_data))
  cat(sprintf("Mean error: %.1f votes\n", accuracy_stats$mean_error))
  cat(sprintf("RMSE: %.1f votes\n", accuracy_stats$rmse))
  cat(sprintf("Correlation: %.3f\n", accuracy_stats$correlation))
  cat(sprintf("Excellent predictions (±2): %d (%.1f%%)\n",
              accuracy_stats$excellent_predictions,
              accuracy_stats$excellent_predictions / accuracy_stats$players_with_data * 100))
  cat(sprintf("Good+ predictions (±5): %d (%.1f%%)\n",
              accuracy_stats$good_predictions,
              accuracy_stats$good_predictions / accuracy_stats$players_with_data * 100))
}

# Save Results ####

cat("Saving results...")

# Save detailed predictions
saveRDS(season_predictions, sprintf("predictions/%d/detailed_predictions.rds", target_season))
write_csv(season_predictions, sprintf("predictions/%d/detailed_predictions.csv", target_season))

# Save probability matrices for simulation (script 07)
prediction_probabilities <- list(
  xgboost_probs = predictions$xgboost_probs,
  gam_probs = predictions$gam_probs,
  ensemble_probs = predictions$ensemble_probs
)
saveRDS(prediction_probabilities, sprintf("predictions/%d/prediction_probabilities.rds", target_season))

# Save player expectations
saveRDS(player_vote_expectations, sprintf("predictions/%d/player_vote_expectations.rds", target_season))
write_csv(player_vote_expectations, sprintf("predictions/%d/player_vote_expectations.csv", target_season))

# Save team analysis
write_csv(team_expectations, sprintf("predictions/%d/team_expectations.csv", target_season))

# Create summary report
summary_report <- list(
  prediction_date = Sys.Date(),
  total_players = nrow(player_vote_expectations),
  total_predictions = nrow(season_predictions),
  ensemble_weights = ensemble_weights,
  top_10_players = head(player_vote_expectations, 10),
  medal_favorite = player_vote_expectations$player_name[1],
  predicted_winner_votes = player_vote_expectations$ensemble_total[1],
  team_analysis = team_expectations
)

saveRDS(summary_report, sprintf("predictions/%d/prediction_summary.rds", target_season))

# Final Summary ####

total_time <- difftime(Sys.time(), pred_start, units = "mins")
cat(sprintf("✓ %d predictions complete (%.1f mins). Top 5: %s\n",
            target_season, as.numeric(total_time),
            paste(head(player_vote_expectations$player_name, 5), collapse = ", ")))
