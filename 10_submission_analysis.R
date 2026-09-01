# 09 - Brownlow Medal Submission Analysis ####
# This script analyzes our 2025 Brownlow Medal submission predictions against actual results
# and compares our model performance with other prediction systems

# Setup ####

## Load required libraries ----
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create("analysis/submission_comparison", showWarnings = FALSE, recursive = TRUE)


# Load Data ####

## Configuration ----
# Set submission file to analyze (NULL = use latest, or specify filename)
submission_file <- NULL  # Change to specific filename if desired, e.g., "brownlow_submission_2025_20240924_143022.csv"

## Load submission data ----
if (is.null(submission_file)) {
  # Find latest submission file
  submission_files <- list.files("submissions", pattern = "^brownlow_submission_2025_.*\\.csv$", full.names = TRUE)
  if (length(submission_files) == 0) {
    stop("No submission files found in submissions/ directory")
  }
  submission_file_path <- submission_files[which.max(file.mtime(submission_files))]
  cat(sprintf("Using latest submission file: %s\n", basename(submission_file_path)))
} else {
  submission_file_path <- file.path("submissions", submission_file)
  if (!file.exists(submission_file_path)) {
    stop(sprintf("Specified submission file not found: %s", submission_file_path))
  }
  cat(sprintf("Using specified submission file: %s\n", submission_file))
}

submission_data <- read_csv(submission_file_path, show_col_types = FALSE)

## Season-specific fixture patches ----
# Rescheduled matches whose round label differs between the results feed and the
# submission form. Set to NA when there is no such fixture in the target season.
RESCHEDULED_2025_MATCH_ID <- "20250142410"   # Gold Coast v Essendon, replayed 27 Aug

## Load actual results ----
results_data <- read_csv("data/brownlow-results-2025.csv", show_col_types = FALSE)


# Data Preparation ####

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

    # One 2025 fixture is labelled differently by the two sources: Gold Coast v
    # Essendon was the Opening Round game postponed by Cyclone Alfred and replayed
    # on 27 August. The results feed files it as Round 0, the submission form as
    # Round 24. Without this it collides with the genuine Opening Round and
    # produces 41 duplicate join keys.
    #
    # This is a 2025-specific patch and WILL be wrong for 2026. The join
    # assertion below is what actually protects against that — it fails loudly
    # rather than silently mismatching players.
    round_num = ifelse(matchId == RESCHEDULED_2025_MATCH_ID, 0, as.numeric(rd)),

    # Split player name
    first_name = str_extract(playerName, "^\\S+"),
    last_name = str_extract(playerName, "\\S+$")
  )

# Join Submission with Actual Results ####

joined_data <- submission_clean %>%
  left_join(
    results_clean,
    by = c("round_num" = "round_num",
           "player_team_std" = "team_std",
           "player_name_std" = "playerName")
  )

# A duplicate key MULTIPLIES rows rather than leaving them unmatched, so an
# NA check cannot see it. This is exactly what the round-label clash above
# produces if the patch is stale.
if (nrow(joined_data) != nrow(submission_clean)) {
  dupes <- results_clean %>%
    dplyr::count(round_num, team_std, playerName) %>%
    dplyr::filter(n > 1)
  stop(sprintf(paste0("Join changed the row count: %d -> %d. ",
                      "%d duplicate (round, team, player) keys in the results feed - ",
                      "most likely a rescheduled fixture labelled differently by the ",
                      "two sources. Check RESCHEDULED_2025_MATCH_ID."),
               nrow(submission_clean), nrow(joined_data), nrow(dupes)))
}

# Check join success
join_success_rate <- mean(!is.na(joined_data$Actual))

# Handle unmatched players (they received 0 votes)
if (join_success_rate > 0.99) {
  joined_data <- joined_data %>%
    mutate(Actual = ifelse(is.na(Actual), 0, Actual))
  join_success_rate <- 1.0
}

# Filter to successful matches for analysis
analysis_data <- joined_data %>%
  dplyr::filter(!is.na(Actual)) %>%
  mutate(
    prediction_error = brownlow_votes - Actual,
    absolute_error = abs(prediction_error),
    squared_error = prediction_error^2
  )


# Performance Analysis ####


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
cat(sprintf("  Correlation: %.3f, RMSE: %.3f, MAE: %.3f\n",
            overall_metrics$correlation, overall_metrics$rmse, overall_metrics$mae))
cat(sprintf("  Total Votes - Predicted: %.1f, Actual: %.0f\n",
            overall_metrics$total_votes_predicted, overall_metrics$total_votes_actual))

## Compare with other models in results dataset ----

# Get model columns (excluding metadata)
model_cols <- setdiff(names(results_clean), c("matchId", "rd", "team", "playerId", "playerName", "Rnk", "Actual",
                                             "team_std", "round_num", "first_name", "last_name"))


# Calculate metrics for each model
model_comparison <- data.frame()
for(model_name in model_cols) {
  if(model_name %in% names(results_clean)) {
    model_data <- results_clean %>% dplyr::filter(!is.na(Actual) & !is.na(.data[[model_name]]))
    if(nrow(model_data) > 0) {
      model_comparison <- bind_rows(model_comparison, data.frame(
        model = model_name,
        correlation = cor(model_data[[model_name]], model_data$Actual, use = "complete.obs"),
        rmse = sqrt(mean((model_data[[model_name]] - model_data$Actual)^2, na.rm = TRUE)),
        mae = mean(abs(model_data[[model_name]] - model_data$Actual), na.rm = TRUE)
      ))
    }
  }
}
model_comparison <- model_comparison %>% arrange(desc(correlation))

# Add our model to comparison
our_model_metrics <- data.frame(
  model = "our_submission",
  correlation = overall_metrics$correlation,
  rmse = overall_metrics$rmse,
  mae = overall_metrics$mae
)

model_comparison_with_ours <- bind_rows(model_comparison, our_model_metrics) %>%
  arrange((rmse))

cat("\nMODEL RANKINGS (by correlation):\n")
if(nrow(model_comparison_with_ours) > 0) {
  print(as.data.frame(model_comparison_with_ours), row.names = FALSE)
} else {
  cat("No model comparisons available\n")
}

# Match-by-match analysis ####

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
  arrange((rmse))

cat("\nTOP 5 BEST PREDICTED MATCHES:\n")
print(head(match_performance, 5))

cat("TOP 5 WORST PREDICTED MATCHES:\n")
print(tail(match_performance, 5))

# Player-level analysis ####

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

cat("\nTOP 10 VOTE WINNERS (Actual vs Predicted):\n")
print(head(player_analysis, 10))

# Save Results ####

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


# Create Visualizations ####

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

}

# Final Summary ####
cat("\n=== FINAL SUMMARY ===\n")
cat(sprintf("Analyzed %d predictions | Correlation: %.3f | RMSE: %.3f\n",
            nrow(analysis_data), overall_metrics$correlation, overall_metrics$rmse))
cat(sprintf("Model ranking: %d out of %d models\n",
            which(model_comparison_with_ours$model == "our_submission"),
            nrow(model_comparison_with_ours)))

if (overall_metrics$correlation > 0.5) {
  cat("Strong predictive performance\n")
} else if (overall_metrics$correlation > 0.3) {
  cat("Moderate predictive performance\n")
} else {
  cat("Room for improvement needed\n")
}

cat("Results saved to: analysis/submission_comparison/\n")
