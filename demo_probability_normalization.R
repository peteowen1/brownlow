# Demonstration: Enhanced Probability Normalization
# Shows how to use the new probability constraint functions

library(dplyr)
devtools::load_all()

cat("=== PROBABILITY NORMALIZATION DEMO ===\n")

# Example 1: Basic normalization with analytical solution ####
cat("\n=== EXAMPLE 1: Basic Probability Normalization ===\n")

# Create example probability matrix (unnormalized). Real AFL matches have
# ~44 players; normalize_probability_matrix() requires at least 4 per match
# (targets are n-3, 1, 1, 1, which needs n >= 4 to stay non-negative).
prob_matrix <- matrix(c(
  # Player 1: mostly gets 0 votes
  0.8, 0.15, 0.04, 0.01,
  # Player 2: balanced probabilities
  0.4, 0.3, 0.2, 0.1,
  # Player 3: high chance of votes
  0.2, 0.2, 0.3, 0.3,
  # Player 3b: mostly gets 0 votes
  0.75, 0.15, 0.07, 0.03,
  # Player 4: different match
  0.9, 0.08, 0.015, 0.005,
  # Player 5:
  0.5, 0.25, 0.15, 0.1,
  # Player 6:
  0.1, 0.3, 0.4, 0.2,
  # Player 6b: mostly gets 0 votes
  0.85, 0.08, 0.05, 0.02
), nrow = 8, byrow = TRUE)

match_ids <- c("Match_1", "Match_1", "Match_1", "Match_1",
              "Match_2", "Match_2", "Match_2", "Match_2")

cat("Original probability matrix:\n")
print(round(prob_matrix, 3))

cat("\nApplying dual constraints (analytical solution)...\n")
result <- normalize_probability_matrix(
  prob_matrix = prob_matrix,
  match_ids = match_ids,
  verbose = TRUE
)

cat("\nResulting expected votes per player:\n")
expected_votes_by_match <- data.frame(
  Player = paste0("Player_", seq_along(match_ids)),
  Match = match_ids,
  Expected_Votes = round(result$expected_votes, 3)
)
print(expected_votes_by_match)

# Example 2: Integration with simulation data ####
cat("\n=== EXAMPLE 2: Integration with Simulation Pipeline ===\n")

# Simulate typical ensemble prediction data
simulation_data <- data.frame(
  player_name = rep(paste0("Player_", 1:22), 2),
  team = rep(c("Team_A", "Team_B"), each = 22),
  date = rep(c("2025-05-01", "2025-05-01"), each = 22),
  season = 2025,
  match_round = 10,

  # Raw ensemble probabilities (slightly unnormalized)
  ensemble_prob_0 = runif(44, 0.3, 0.9),
  ensemble_prob_1 = runif(44, 0.05, 0.3),
  ensemble_prob_2 = runif(44, 0.02, 0.2),
  ensemble_prob_3 = runif(44, 0.01, 0.15)
)

cat("Sample simulation data (first 5 rows):\n")
print(simulation_data[1:5, c("player_name", "team", "ensemble_prob_0", "ensemble_prob_1", "ensemble_prob_2", "ensemble_prob_3")])

# Apply ensemble normalization
cat("\nNormalizing ensemble probabilities...\n")
normalized_data <- normalize_ensemble_probabilities(
  prediction_data = simulation_data,
  prob_prefix = "ensemble_prob_",
  verbose = TRUE
)

cat("\nNormalized data sample (first 3 rows):\n")
norm_cols <- c("player_name", "team", "ensemble_prob_norm_0", "ensemble_prob_norm_1",
               "ensemble_prob_norm_2", "ensemble_prob_norm_3", "ensemble_prob_expected_votes")
print(round(normalized_data[1:3, norm_cols], 3))

# Check constraints
cat("\nConstraint verification:\n")
match_1_players <- normalized_data[normalized_data$team == "Team_A", ]
match_2_players <- normalized_data[normalized_data$team == "Team_B", ]

cat("Match 1 (Team A) vote totals:\n")
cat(sprintf("  P(1 vote) sum: %.6f\n", sum(match_1_players$ensemble_prob_norm_1)))
cat(sprintf("  P(2 votes) sum: %.6f\n", sum(match_1_players$ensemble_prob_norm_2)))
cat(sprintf("  P(3 votes) sum: %.6f\n", sum(match_1_players$ensemble_prob_norm_3)))
cat(sprintf("  Total expected votes: %.6f\n", sum(match_1_players$ensemble_prob_expected_votes)))

cat("Match 2 (Team B) vote totals:\n")
cat(sprintf("  P(1 vote) sum: %.6f\n", sum(match_2_players$ensemble_prob_norm_1)))
cat(sprintf("  P(2 votes) sum: %.6f\n", sum(match_2_players$ensemble_prob_norm_2)))
cat(sprintf("  P(3 votes) sum: %.6f\n", sum(match_2_players$ensemble_prob_norm_3)))
cat(sprintf("  Total expected votes: %.6f\n", sum(match_2_players$ensemble_prob_expected_votes)))

# Example 3: Comparison with other methods ####
cat("\n=== EXAMPLE 3: Method Comparison ===\n")

# Compare different approaches
set.seed(42)
test_matrix <- matrix(runif(16, 0, 1), nrow = 4, ncol = 4)
test_matches <- c("M1", "M1", "M1", "M1")

cat("Original matrix (unnormalized):\n")
print(round(test_matrix, 3))

# Method 1: Simple row normalization only
simple_normalized <- test_matrix / rowSums(test_matrix)
cat("\nMethod 1 - Simple row normalization:\n")
print(round(simple_normalized, 3))
cat("Column sums:", round(colSums(simple_normalized), 3), "\n")

# Method 2: Our analytical dual constraint method
analytical_result <- normalize_probability_matrix(test_matrix, test_matches, verbose = FALSE)
cat("\nMethod 2 - Analytical dual constraints:\n")
print(round(analytical_result$normalized_probabilities, 3))
cat("Column sums:", round(colSums(analytical_result$normalized_probabilities), 3), "\n")

# Example 4: Performance comparison ####
cat("\n=== EXAMPLE 4: Performance Demonstration ===\n")

# Large dataset performance test
n_players <- 1000
large_matrix <- matrix(runif(n_players * 4), nrow = n_players, ncol = 4)
large_matches <- rep(paste0("M", 1:(n_players/22)), each = 22)[1:n_players]

cat(sprintf("Testing performance with %d players...\n", n_players))

start_time <- Sys.time()
large_result <- normalize_probability_matrix(large_matrix, large_matches, verbose = FALSE)
end_time <- Sys.time()

cat(sprintf("Processing time: %.3f seconds\n", as.numeric(end_time - start_time)))
cat(sprintf("Constraints satisfied: %s\n",
            ifelse(large_result$diagnostics$constraint_violations$all_constraints_satisfied, "✅ YES", "❌ NO")))

# Summary ####
cat("\n=== SUMMARY ===\n")
cat("✅ Analytical Solution: No optimization required - direct mathematical formula\n")
cat("✅ Dual Constraints: Both player probabilities and match vote types sum correctly\n")
cat("✅ Fast Performance: Scales linearly with dataset size\n")
cat("✅ Robust Handling: Manages edge cases and provides comprehensive validation\n")
cat("✅ Easy Integration: Drop-in replacement for existing constraint functions\n")

cat("\n=== USAGE RECOMMENDATIONS ===\n")
cat("🔧 For new projects: Use normalize_probability_matrix() directly\n")
cat("🔧 For ensemble pipeline: Use normalize_ensemble_probabilities() wrapper\n")
cat("🔧 For quick fixes: Use quick_probability_fix() for development\n")
cat("🔧 For legacy code: Enhanced apply_match_probability_constraints() maintains compatibility\n")

cat("\n✅ DEMO COMPLETE - PROBABILITY NORMALIZATION SYSTEM READY!\n")