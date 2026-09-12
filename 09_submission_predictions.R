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
TARGET_SEASON <- 2026
MODEL_NAME <- "InTheGame"
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
ensemble_target_raw <- ensemble_probabilities[ensemble_probabilities$season == TARGET_SEASON, ]
print(paste(TARGET_SEASON, "data rows:", nrow(ensemble_target_raw)))
stopifnot("no rows for TARGET_SEASON in ensemble_probabilities" = nrow(ensemble_target_raw) > 0)

ensemble_target <- ensemble_target_raw %>%
  mutate(
    full_player_name = player_name,  # Keep original player name for joining
    team_key = standardize_team_name(team)
  ) %>%
  select(full_player_name, team_key, match_id,
         ensemble_prob_0, ensemble_prob_1, ensemble_prob_2, ensemble_prob_3)

## Recover match_id for the submission form ----
# The 2026 form dropped match_date entirely, so match_id can't be built
# straight from the form. It still carries player_id, and the raw datathon
# feed carries player_id and match_date together, so (season, match_round,
# player_id) recovers match_date.
#
# match_id here must be the pipeline's own synthetic key --
# paste(date, standardize_team_name(home), standardize_team_name(away)) --
# NOT the raw feed's numeric match_id column. R/performance_features.R
# overwrites the numeric one with this synthetic one during feature
# engineering (comment there: "Create proper match_id using date +
# home_team + away_team"), and that's what ensemble_probabilities is keyed
# on. Verified empirically: all 207 candidate keys built this way match
# ensemble_probabilities$match_id for TARGET_SEASON exactly, 0 unmatched
# either direction -- the numeric raw-feed match_id is a different ID space
# entirely and would silently join nothing.
raw_feed <- read_csv("data/brownlow_datathon_dataset.csv", show_col_types = FALSE) %>%
  dplyr::filter(season == TARGET_SEASON) %>%
  dplyr::select(season, match_round, player_id, match_date) %>%
  dplyr::distinct()

dup_keys <- raw_feed %>% dplyr::count(season, match_round, player_id) %>% dplyr::filter(n > 1)
if (nrow(dup_keys) > 0) {
  stop(sprintf("%d (season, match_round, player_id) keys map to more than one match_date in the raw feed.",
               nrow(dup_keys)))
}

submission_prepared <- submission_form %>%
  mutate(
    full_player_name = paste(player_first_name, player_last_name),
    team_key = standardize_team_name(player_team)
  ) %>%
  left_join(raw_feed, by = c("season", "match_round", "player_id"))

n_no_date <- sum(is.na(submission_prepared$match_date))
if (n_no_date > 0) {
  stop(sprintf(paste0("%d of %d submission rows found no match_date via (season, match_round, player_id). ",
                      "Check the raw feed covers TARGET_SEASON and player_id is populated on both sides."),
               n_no_date, nrow(submission_prepared)))
}

submission_prepared <- submission_prepared %>%
  mutate(
    match_id = paste(as.Date(match_date, format = "%d/%m/%Y"),
                      standardize_team_name(match_home_team),
                      standardize_team_name(match_away_team),
                      sep = "_")
  ) %>%
  dplyr::select(-match_date)

## Join to official submission form ----
# Canonicalise club names on BOTH sides. The raw datathon feed has shipped the
# current season under different club names to the history (Brisbane Lions vs
# Brisbane, Greater Western Sydney vs GWS), and the competition form is a
# separate file again — an unmatched club here silently produces zero votes.
# (team_key was already built above via standardize_team_name().)
submission_with_predictions <- submission_prepared %>%
  left_join(
    ensemble_target,
    by = c("full_player_name", "team_key", "match_id")
  )

# A duplicate key in ensemble_target would MULTIPLY a player's row rather than
# leave it unmatched, so the NA check below cannot see it. IPF then forces the
# match to total 6 across the inflated field, and every constraint check still
# passes while the vote mass is redistributed among a player who now appears
# twice. This is the same duplicate-key hazard that a Round 0 / Round 24
# labelling clash produced in the 2025 results join.
if (nrow(submission_with_predictions) != nrow(submission_prepared)) {
  stop(sprintf(paste0("Join changed the row count: %d -> %d. ",
                      "ensemble_probabilities has duplicate (player, team, match_id) keys."),
               nrow(submission_prepared), nrow(submission_with_predictions)))
}

n_unmatched <- sum(is.na(submission_with_predictions$ensemble_prob_3))
if (n_unmatched > 0) {
  stop(sprintf(paste0("%d of %d submission rows found no prediction. ",
                      "Check player-name and club-name spelling against the form.\n%s"),
               n_unmatched, nrow(submission_with_predictions),
               paste(utils::capture.output(print(utils::head(
                 submission_with_predictions[is.na(submission_with_predictions$ensemble_prob_3),
                                             c("match_id", "player_team", "full_player_name")], 10))),
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
  mutate(brownlow_votes_prediction = normalize_votes_ipf(prob_matrix, match_id)) %>%
  select(-dplyr::starts_with("ensemble_prob_"))

## Validate before writing ----
# Check the shape FIRST. Every constraint below passes vacuously on an empty
# frame: tapply over no rows gives a zero-length result, max(abs(x - 6)) is
# then -Inf which is duly < 1e-6, all() of nothing is TRUE, and anyNA() of
# nothing is FALSE. An empty submission would sail through and be written out
# with "Constraints OK" printed above it.
stopifnot(
  "submission lost or gained rows" = nrow(submission_final) == nrow(submission_form),
  "submission is empty"            = nrow(submission_final) > 0,
  "match_id must never be missing" = !anyNA(submission_final$match_id)
)

match_totals <- tapply(submission_final$brownlow_votes_prediction, submission_final$match_id, sum)
stopifnot(
  "no matches found"                       = length(match_totals) > 0,
  "every match must total exactly 6 votes" = max(abs(match_totals - 6)) < 1e-6,
  "no player may fall outside 0-3 votes"   = all(submission_final$brownlow_votes_prediction >= 0 &
                                                 submission_final$brownlow_votes_prediction <= 3),
  "no missing predictions"                  = !anyNA(submission_final$brownlow_votes_prediction)
)
cat(sprintf("Constraints OK: %d matches, worst total error %.2e\n",
            length(match_totals), max(abs(match_totals - 6))))

# Save Results ####

## Create output directory and save ----
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_file <- sprintf("submissions/brownlow_submission_%d_%s.csv", TARGET_SEASON, timestamp)
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write_csv(submission_final, output_file)

## Upload-ready file ----
# Must match the template AS-IS (T&Cs: "must not modify the template except
# to add predicted vote counts") -- exact original columns and order, model
# name substituted into the organiser's naming convention.
upload_file <- sprintf("submissions/Brownlow_Medal_Datathon_%d_Submission_Form_V2_%s.csv",
                        TARGET_SEASON, MODEL_NAME)
write_csv(submission_final %>% select(all_of(names(submission_form))), upload_file)

## Summary ----
total_votes <- sum(submission_final$brownlow_votes_prediction, na.rm = TRUE)
n_predictions <- nrow(submission_final)
print(paste("✅ Submission complete:", n_predictions, "predictions,", round(total_votes, 1), "total votes"))
print(paste("Upload file:", upload_file))

