# Compare Submission Predictions vs Actual 2025 Brownlow Results
# This script loads both datasets, joins them, and performs comprehensive comparison analysis

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(lubridate)
devtools::load_all()

cat("=== BROWNLOW 2025 PREDICTION COMPARISON ===\n")

# Load datasets ####
cat("Loading datasets...\n")

## Load submission data ----
submission_data <- read_csv("submissions/brownlow_submission_2025.csv", show_col_types = FALSE)
cat(sprintf("✓ Submission data: %d predictions\n", nrow(submission_data)))

## Load actual results ----
results_data <- read_csv("data/brownlow-results-2025.csv", show_col_types = FALSE)
cat(sprintf("✓ Results data: %d player records\n", nrow(results_data)))

# Examine data structure ####
cat("\n=== DATA STRUCTURE EXAMINATION ===\n")

cat("Submission columns:", paste(names(submission_data), collapse = ", "), "\n")
cat("Results columns:", paste(names(results_data), collapse = ", "), "\n")

cat("\nSubmission unique rounds:", paste(sort(unique(submission_data$match_round)), collapse = ", "), "\n")
cat("Results unique rounds:", paste(sort(unique(results_data$rd)), collapse = ", "), "\n")

cat("\nSubmission unique teams:", paste(sort(unique(submission_data$match_home_team)), collapse = ", "), "\n")
cat("Results unique teams:", paste(sort(unique(results_data$team)), collapse = ", "), "\n")

# Data preparation ####
cat("\n=== DATA PREPARATION ===\n")

## Clean and standardize submission data ----
submission_clean <- submission_data %>%
  mutate(
    # Standardize team names
    home_team_std = standardize_team_name(match_home_team),
    away_team_std = standardize_team_name(match_away_team),
    player_team_std = standardize_team_name(player_team),

    # Create full player name
    full_player_name = paste(player_first_name, player_last_name),

    # Standardize player name for joining (to match results format)
    player_name_std = case_when(
      full_player_name == "Callum Brown" ~ "Callum M. Brown",
      full_player_name == "Nat Fyfe" ~ "Nathan Fyfe",
      full_player_name == "Tom Cole" ~ "Thomas Cole",
      full_player_name == "Tom Lynch" ~ "Tom J. Lynch",
      TRUE ~ full_player_name
    ),

    # Clean round (convert to numeric)
    round_num = as.numeric(match_round),

    # Parse date
    match_date_parsed = dmy(match_date)
  )

## Clean and standardize results data ----
results_clean <- results_data %>%
  mutate(
    # Standardize team names
    team_std = case_when(
      team == "HAW" ~ "Hawthorn",
      team == "SYD" ~ "Sydney",
      team == "COLL" ~ "Collingwood",
      team == "MELB" ~ "Melbourne",
      team == "GEEL" ~ "Geelong",
      team == "WB" ~ "Western Bulldogs",
      team == "BL" ~ "Brisbane",
      team == "FRE" ~ "Fremantle",
      team == "PORT" ~ "Port Adelaide",
      team == "RICH" ~ "Richmond",
      team == "CARL" ~ "Carlton",
      team == "ESS" ~ "Essendon",
      team == "GWS" ~ "Greater Western Sydney",
      team == "WCE" ~ "West Coast",
      team == "ADEL" ~ "Adelaide",
      team == "NMFC" ~ "North Melbourne",
      team == "STK" ~ "St Kilda",
      team == "GCFC" ~ "Gold Coast",
      TRUE ~ team
    ),

    # Clean round
    round_num = ifelse(matchId == '20250142410',0,as.numeric(rd)),

    # Split player name
    first_name = str_extract(playerName, "^\\S+"),
    last_name = str_extract(playerName, "\\S+$")
  )

cat("✓ Data cleaning completed\n")

# Create joining strategy ####
cat("\n=== CREATING JOIN LOGIC ===\n")

## Strategy 1: Join by round + team + player name ----
cat("Attempting Strategy 1: Round + Team + Player Name...\n")

joined_data <- submission_clean %>%
  left_join(
    results_clean,
    by = c("round_num" = "round_num",
           "player_team_std" = "team_std",
           "player_name_std" = "playerName")
  )

joined_data_v2 <- results_clean %>%
  left_join(
    submission_clean,
    by = c("round_num" = "round_num",
           "team_std" = "player_team_std",
           "playerName" = "player_name_std")
  )

joined_data %>% dplyr::filter(is.na(Actual)) %>% View()
joined_data_v2 %>% dplyr::filter(is.na(brownlow_votes)) %>% View()

# Check join success
join_success_rate <- mean(!is.na(joined_data$Actual))
cat(sprintf("Join success rate: %.1f%% (%d/%d matches)\n",
            join_success_rate * 100,
            sum(!is.na(joined_data$Actual)),
            nrow(joined_data)))

if (join_success_rate < 0.8) {
  cat("⚠️ Low join rate, trying alternative strategies...\n")

  ## Strategy 2: Try player_id matching ----
  if ("player_id" %in% names(submission_data) && "playerId" %in% names(results_data)) {
    cat("Trying Strategy 2: Player ID matching...\n")

    joined_data_alt <- submission_clean %>%
      left_join(
        results_clean,
        by = c("player_id" = "playerId")
      ) %>%
      dplyr::filter(!is.na(Actual))

    alt_success_rate <- nrow(joined_data_alt) / nrow(submission_clean)
    cat(sprintf("Alt join success rate: %.1f%%\n", alt_success_rate * 100))

    if (alt_success_rate > join_success_rate) {
      joined_data <- joined_data_alt
      join_success_rate <- alt_success_rate
      cat("✓ Using player ID matching\n")
    }
  }
}

# dplyr::filter to successful matches for analysis
analysis_data <- joined_data %>%
  dplyr::filter(!is.na(Actual)) %>%
  mutate(
    prediction_error = brownlow_votes - Actual,
    absolute_error = abs(prediction_error),
    squared_error = prediction_error^2
  )

cat(sprintf("✓ Analysis dataset: %d matched predictions\n", nrow(analysis_data)))

# Performance Analysis ####
cat("\n=== PERFORMANCE ANALYSIS ===\n")

## Overall metrics ----
overall_metrics <- analysis_data %>%
  summarise(
    n_predictions = n(),
    correlation = cor(brownlow_votes, Actual),
    rmse = sqrt(mean(squared_error)),
    mae = mean(absolute_error),
    mean_prediction = mean(brownlow_votes),
    mean_actual = mean(Actual),
    total_votes_predicted = sum(brownlow_votes),
    total_votes_actual = sum(Actual)
  )

cat("OVERALL PERFORMANCE:\n")
cat(sprintf("  Correlation: %.3f\n", overall_metrics$correlation))
cat(sprintf("  RMSE: %.3f\n", overall_metrics$rmse))
cat(sprintf("  MAE: %.3f\n", overall_metrics$mae))
cat(sprintf("  Mean Prediction: %.3f\n", overall_metrics$mean_prediction))
cat(sprintf("  Mean Actual: %.3f\n", overall_metrics$mean_actual))
cat(sprintf("  Total Votes Predicted: %.1f\n", overall_metrics$total_votes_predicted))
cat(sprintf("  Total Votes Actual: %.0f\n", overall_metrics$total_votes_actual))

## Compare with other models in results dataset ----
cat("\n=== COMPARISON WITH OTHER MODELS ===\n")

# Get model columns (excluding metadata)
model_cols <- setdiff(names(results_clean), c("matchId", "rd", "team", "playerId", "playerName", "Rnk", "Actual",
                                             "team_std", "round_num", "first_name", "last_name"))

cat("Available models for comparison:", paste(model_cols, collapse = ", "), "\n")

# Calculate metrics for each model
model_comparison <- results_clean %>%
  dplyr::filter(!is.na(Actual)) %>%
  summarise(
    across(all_of(model_cols),
           ~ list(
             correlation = cor(.x, Actual, use = "complete.obs"),
             rmse = sqrt(mean((.x - Actual)^2, na.rm = TRUE)),
             mae = mean(abs(.x - Actual), na.rm = TRUE)
           ),
           .names = "{.col}_metrics")
  ) %>%
  pivot_longer(everything(), names_to = "model_metric", values_to = "value") %>%
  separate(model_metric, into = c("model", "metric"), sep = "_metrics") %>%
  unnest_wider(value) %>%
  arrange(desc(correlation))

# Add our model to comparison
our_model_metrics <- data.frame(
  model = "our_submission",
  correlation = overall_metrics$correlation,
  rmse = overall_metrics$rmse,
  mae = overall_metrics$mae
)

model_comparison_with_ours <- bind_rows(model_comparison, our_model_metrics) %>%
  arrange(desc(correlation))

cat("\nMODEL RANKINGS (by correlation):\n")
print(model_comparison_with_ours, n = 15)

# Match-by-match analysis ####
cat("\n=== MATCH-BY-MATCH ANALYSIS ===\n")

match_performance <- analysis_data %>%
  group_by(round_num, home_team_std, away_team_std) %>%
  summarise(
    n_predictions = n(),
    correlation = cor(brownlow_votes, Actual),
    rmse = sqrt(mean(squared_error)),
    total_votes_predicted = sum(brownlow_votes),
    total_votes_actual = sum(Actual),
    .groups = "drop"
  ) %>%
  arrange(desc(correlation))

cat("TOP 5 BEST PREDICTED MATCHES:\n")
print(head(match_performance, 5))

cat("\nTOP 5 WORST PREDICTED MATCHES:\n")
print(tail(match_performance, 5))

# Player-level analysis ####
cat("\n=== PLAYER-LEVEL ANALYSIS ===\n")

player_analysis <- analysis_data %>%
  group_by(full_player_name, player_team_std) %>%
  summarise(
    n_games = n(),
    avg_prediction = mean(brownlow_votes),
    avg_actual = mean(Actual),
    correlation = if(n() > 1) cor(brownlow_votes, Actual) else NA_real_,
    rmse = sqrt(mean(squared_error)),
    total_predicted = sum(brownlow_votes),
    total_actual = sum(Actual),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_games >= 3) %>%  # Players with at least 3 games
  arrange(desc(total_actual))

cat("TOP 10 VOTE WINNERS (Actual vs Predicted):\n")
print(head(player_analysis, 10))

# Save results ####
cat("\n=== SAVING RESULTS ===\n")

# Create output directory
dir.create("analysis/submission_comparison", showWarnings = FALSE, recursive = TRUE)

# Save detailed comparison
write_csv(analysis_data, "analysis/submission_comparison/detailed_predictions_vs_actual.csv")

# Save summary metrics
write_csv(model_comparison_with_ours, "analysis/submission_comparison/model_performance_comparison.csv")
write_csv(match_performance, "analysis/submission_comparison/match_by_match_performance.csv")
write_csv(player_analysis, "analysis/submission_comparison/player_level_analysis.csv")

# Create summary report
summary_report <- list(
  overall_metrics = overall_metrics,
  join_success_rate = join_success_rate,
  model_ranking = which(model_comparison_with_ours$model == "our_submission"),
  n_models_compared = nrow(model_comparison_with_ours),
  best_match = match_performance[1, ],
  worst_match = match_performance[nrow(match_performance), ],
  top_predicted_player = player_analysis[1, ]
)

saveRDS(summary_report, "analysis/submission_comparison/summary_report.rds")

cat("✓ Results saved to analysis/submission_comparison/\n")

# Create visualization ####
cat("\n=== CREATING VISUALIZATIONS ===\n")

if (nrow(analysis_data) > 0) {

  # Prediction vs Actual scatter plot
  p1 <- ggplot(analysis_data, aes(x = Actual, y = brownlow_votes)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "lm", color = "green", se = FALSE) +
    labs(
      title = "Submission Predictions vs Actual Brownlow Votes (2025)",
      subtitle = sprintf("Correlation: %.3f, RMSE: %.3f",
                        overall_metrics$correlation, overall_metrics$rmse),
      x = "Actual Votes",
      y = "Predicted Votes"
    ) +
    theme_minimal()

  ggsave("analysis/submission_comparison/predictions_vs_actual.png", p1, width = 10, height = 8)

  # Model comparison plot
  p2 <- model_comparison_with_ours %>%
    mutate(is_our_model = model == "our_submission") %>%
    ggplot(aes(x = reorder(model, correlation), y = correlation, fill = is_our_model)) +
    geom_col() +
    scale_fill_manual(values = c("grey70", "red"), guide = "none") +
    coord_flip() +
    labs(
      title = "Model Performance Comparison (2025 Brownlow)",
      subtitle = "Correlation with Actual Votes",
      x = "Model",
      y = "Correlation"
    ) +
    theme_minimal()

  ggsave("analysis/submission_comparison/model_comparison.png", p2, width = 12, height = 8)

  cat("✓ Visualizations saved\n")
}

# Final summary ####
cat("\n=== FINAL SUMMARY ===\n")
cat(sprintf("📊 Successfully analyzed %d predictions\n", nrow(analysis_data)))
cat(sprintf("🎯 Overall correlation: %.3f\n", overall_metrics$correlation))
cat(sprintf("📏 RMSE: %.3f\n", overall_metrics$rmse))
cat(sprintf("🏆 Model ranking: %d out of %d models\n",
            which(model_comparison_with_ours$model == "our_submission"),
            nrow(model_comparison_with_ours)))

if (overall_metrics$correlation > 0.5) {
  cat("✅ Strong predictive performance!\n")
} else if (overall_metrics$correlation > 0.3) {
  cat("✔️ Moderate predictive performance\n")
} else {
  cat("⚠️ Room for improvement in predictions\n")
}

cat("\n📁 All results saved to: analysis/submission_comparison/\n")
