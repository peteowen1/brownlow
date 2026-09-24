# Test Lagged Career Averages Implementation
# This script tests the new calculate_lagged_career_averages function

library(dplyr)
library(lubridate)
devtools::load_all()

cat("Testing lagged career averages implementation...\n")

# Create a small test dataset
test_data <- tibble::tibble(
  player_name = rep(c("Player_A", "Player_B"), each = 10),
  date = rep(seq(as.Date("2020-01-01"), by = "weeks", length.out = 10), 2),
  season = c(rep(2020, 5), rep(2021, 5), rep(2020, 5), rep(2021, 5)),

  # Game-by-game metrics
  disposals = c(20, 25, 30, 22, 28, 24, 26, 29, 23, 27,  # Player A
                18, 22, 26, 20, 24, 21, 23, 25, 19, 22), # Player B
  goals = c(2, 1, 3, 0, 2, 1, 2, 1, 0, 1,  # Player A
            1, 0, 2, 1, 1, 0, 1, 2, 1, 0), # Player B
  impact_score = c(80, 75, 90, 70, 85, 78, 82, 88, 72, 80,  # Player A
                   65, 68, 78, 62, 70, 66, 69, 75, 60, 67), # Player B

  # Season-level metric
  brownlow_votes = c(1, 0, 3, 0, 2, 0, 1, 2, 0, 1,  # Player A
                     0, 1, 2, 0, 1, 1, 0, 1, 0, 0)   # Player B
)

cat("Original test data structure:\n")
print(head(test_data, 8))

# Test the function
cat("\nTesting calculate_lagged_career_averages...\n")
result <- calculate_lagged_career_averages(test_data)

# Check the results
career_avg_cols <- names(result)[stringr::str_starts(names(result), "career_avg_")]
cat("\nCreated career average columns:\n")
print(career_avg_cols)

# Validate game-by-game averages (disposals example)
cat("\nValidating disposals career averages for Player_A:\n")
player_a_data <- result %>%
  dplyr::filter(player_name == "Player_A") %>%
  dplyr::select(date, season, disposals, career_avg_disposals, career_games_so_far)

print(player_a_data)

# Test data leakage prevention - first few games should have 0 career average
cat("\nValidating no data leakage (first game should have 0 career average):\n")
first_games <- result %>%
  dplyr::group_by(player_name) %>%
  dplyr::slice(1) %>%
  dplyr::select(player_name, career_avg_disposals, career_avg_goals)

print(first_games)

# Test Brownlow votes season-level logic
if ("career_avg_brownlow_votes_prior_seasons" %in% names(result)) {
  cat("\nValidating Brownlow votes career averages:\n")
  brownlow_test <- result %>%
    dplyr::select(player_name, season, brownlow_votes, career_avg_brownlow_votes_prior_seasons) %>%
    dplyr::distinct() %>%
    dplyr::arrange(player_name, season)

  print(brownlow_test)

  # Check that 2020 seasons have NA (no prior seasons)
  first_seasons <- brownlow_test %>%
    dplyr::filter(season == 2020)

  if (all(is.na(first_seasons$career_avg_brownlow_votes_prior_seasons))) {
    cat("✓ PASS: First seasons correctly have NA for career Brownlow averages\n")
  } else {
    cat("✗ FAIL: First seasons should have NA for career Brownlow averages\n")
  }
}

cat("\nTest completed!\n")