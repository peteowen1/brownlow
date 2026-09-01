# 09 - Brownlow Medal Submission Predictions ####
# Join existing ensemble predictions to the official competition form.

# Competition rules:
# - Use the official submission form AS-IS; add only the predicted votes column
# - Individual predictions between 0 and 3 inclusive, decimals allowed
# - Predictions across all players in a match must total exactly 6
# - 2026 deadline: 2:59pm AEST Monday 21 September 2026

# Setup ####
library(tidyverse)
devtools::load_all()

## Configuration ----
# Set TARGET_SEASON each year; the form path and output filename follow from it.
TARGET_SEASON <- 2025
submission_file <- sprintf("data/Brownlow_Medal_Datathon_%d_Submission_Form.csv", TARGET_SEASON)

# Load Data ####

## Official competition submission form ----
if (!file.exists(submission_file)) {
  stop(sprintf("Submission form not found: %s\nSet TARGET_SEASON at the top of this script.",
               submission_file))
}
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
ensemble_2025_raw <- ensemble_probabilities[ensemble_probabilities$season == TARGET_SEASON, ]
print(paste(TARGET_SEASON, "data rows:", nrow(ensemble_2025_raw)))
stopifnot("no rows for TARGET_SEASON in ensemble_probabilities" = nrow(ensemble_2025_raw) > 0)

ensemble_2025 <- ensemble_2025_raw %>%
  mutate(
    match_date = format(as.Date(date), "%d/%m/%Y"),  # Convert to Date first, then format
    full_player_name = player_name,  # Keep original player name for joining
    team_key = standardize_team_name(team)
  ) %>%
  select(full_player_name, team_key, match_date,
         ensemble_prob_0, ensemble_prob_1, ensemble_prob_2, ensemble_prob_3)

## Prepare submission form for joining ----
# Canonicalise club names on BOTH sides. The raw datathon feed has shipped the
# current season under different club names to the history (Brisbane Lions vs
# Brisbane, Greater Western Sydney vs GWS), and the competition form is a
# separate file again — an unmatched club here silently produces zero votes.
submission_prepared <- submission_form %>%
  mutate(
    full_player_name = paste(player_first_name, player_last_name),
    team_key = standardize_team_name(player_team)
  )

## Join to official submission form ----
submission_with_predictions <- submission_prepared %>%
  left_join(
    ensemble_2025,
    by = c("full_player_name", "team_key", "match_date")
  )

n_unmatched <- sum(is.na(submission_with_predictions$ensemble_prob_3))
if (n_unmatched > 0) {
  stop(sprintf(paste0("%d of %d submission rows found no prediction. ",
                      "Check player-name and club-name spelling against the form.\n%s"),
               n_unmatched, nrow(submission_with_predictions),
               paste(utils::capture.output(print(utils::head(
                 submission_with_predictions[is.na(submission_with_predictions$ensemble_prob_3),
                                             c("match_date", "player_team", "full_player_name")], 10))),
                 collapse = "\n")))
}

submission_with_predictions <- submission_with_predictions %>%
  select(-full_player_name, -team_key)

# Apply Constraints ####

## Enforce the vote structure by iterative proportional fitting ----
# In every match exactly one player takes 3 votes, one takes 2 and one takes 1,
# so across the field P(3), P(2) and P(1) each sum to 1 and each player's four
# probabilities sum to 1. Satisfying all four makes the 6-vote total exact.
#
# The previous method rescaled expected votes to sum to 6, which enforces one of
# those four constraints. Measured over 5 seeds on the 2025 holdout, IPF beat it
# by 0.00176 RMSE against a seed noise floor of 0.00042 — see
# analysis/harness/constraint_method_comparison.csv.
prob_matrix <- as.matrix(submission_with_predictions[, paste0("ensemble_prob_", 0:3)])

submission_final <- submission_with_predictions %>%
  mutate(brownlow_votes = normalize_votes_ipf(prob_matrix, match_id)) %>%
  select(-dplyr::starts_with("ensemble_prob_"))

## Validate before writing ----
match_totals <- tapply(submission_final$brownlow_votes, submission_final$match_id, sum)
stopifnot(
  "every match must total exactly 6 votes" = max(abs(match_totals - 6)) < 1e-6,
  "no player may fall outside 0-3 votes"   = all(submission_final$brownlow_votes >= 0 &
                                                 submission_final$brownlow_votes <= 3),
  "no missing predictions"                  = !anyNA(submission_final$brownlow_votes)
)
cat(sprintf("Constraints OK: %d matches, worst total error %.2e\n",
            length(match_totals), max(abs(match_totals - 6))))

# Save Results ####

## Create output directory and save ----
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- sprintf("submissions/brownlow_submission_%d_%s.csv", TARGET_SEASON, timestamp)
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write_csv(submission_final, output_file)

## Summary ----
total_votes <- sum(submission_final$brownlow_votes, na.rm = TRUE)
n_predictions <- nrow(submission_final)
print(paste("✅ Submission complete:", n_predictions, "predictions,", round(total_votes, 1), "total votes"))

