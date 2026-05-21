# 08 - Brownlow Medal Submission Predictions ####
# Join existing ensemble predictions to official competition form

# Competition Rules:
# - Use official Brownlow_Medal_Datathon_2025_Submission_Form.csv AS-IS
# - Add ONLY "Predicted_Votes" column (0-3, decimals allowed)
# - Ensure exactly 6 votes per match across all players
# - Submit by 2:59pm AEST Monday 22nd September 2025

# Setup ####
library(tidyverse)
devtools::load_all()

# Load Data ####

## Official competition submission form ----
submission_file <- "data/Brownlow_Medal_Datathon_2025_Submission_Form.csv"
submission_form <- read_csv(submission_file, show_col_types = FALSE)

## Check submission form structure ----
print("Submission form columns:")
print(names(submission_form))
print(paste("Rows:", nrow(submission_form)))

## Ensemble probabilities from 05_ensemble_predictions.R ----
ensemble_probabilities <- read_rds("results/ensemble_probabilities.rds")

# Join Predictions ####

## Check what columns we have ----
print("Ensemble probabilities columns:")
print(names(ensemble_probabilities))
print(paste("Rows:", nrow(ensemble_probabilities)))

## Debug the ensemble data structure ----
print("First few rows of ensemble data:")
print(head(ensemble_probabilities))
print("Season column unique values:")
print(unique(ensemble_probabilities$season))

## Prepare ensemble data for joining (using base R filtering) ----
ensemble_2025_raw <- ensemble_probabilities[ensemble_probabilities$season == 2025, ]
print(paste("2025 data rows:", nrow(ensemble_2025_raw)))

ensemble_2025 <- ensemble_2025_raw %>%
  mutate(
    match_date = format(as.Date(date), "%d/%m/%Y"),  # Convert to Date first, then format
    full_player_name = player_name  # Keep original player name for joining
  ) %>%
  select(full_player_name, team, match_date, ensemble_expected_votes)

## Prepare submission form for joining ----
submission_prepared <- submission_form %>%
  mutate(
    full_player_name = paste(player_first_name, player_last_name)  # Combine names
  )

## Join to official submission form ----
submission_with_predictions <- submission_prepared %>%
  left_join(
    ensemble_2025,
    by = c("full_player_name" = "full_player_name",
           "player_team" = "team",
           "match_date" = "match_date")
  ) %>%
  select(-full_player_name)  # Remove helper column

# Apply Constraints ####

## Ensure exactly 6 votes per match with proper 0-3 clamping ----
submission_final <- submission_with_predictions %>%
  group_by(match_id) %>%  # Group by match_id instead!
  mutate(
    # First clamp individual predictions to 0-3 range
    clamped_votes = pmax(0, pmin(3, ensemble_expected_votes)),

    # Then scale to ensure exactly 6 votes per match
    match_total = sum(clamped_votes, na.rm = TRUE),
    scaled_votes = if_else(match_total > 0,
                          clamped_votes * 6 / match_total,
                          0),

    # Finally clamp again to ensure no player exceeds 3 votes
    brownlow_votes = pmax(0, pmin(3, scaled_votes))
  ) %>%
  ungroup() %>%
  select(-ensemble_expected_votes, -clamped_votes, -match_total, -scaled_votes)  # Remove helper columns

# Save Results ####

## Create output directory and save ----
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- sprintf("submissions/brownlow_submission_2025_%s.csv", timestamp)
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write_csv(submission_final, output_file)

## Summary ----
total_votes <- sum(submission_final$brownlow_votes, na.rm = TRUE)
n_predictions <- nrow(submission_final)
print(paste("✅ Submission complete:", n_predictions, "predictions,", round(total_votes, 1), "total votes"))

