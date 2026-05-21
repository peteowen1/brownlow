# Demonstration: Teammate Vote Correlation Analysis for Betting
# Shows how to identify negatively correlated teammate pairs for betting opportunities
# NOTE: This demo reads from 07_brownlow_simulation.R outputs but uses "demo_" prefixed variables

library(dplyr)
devtools::load_all()

# Load simulation results and run correlation analysis ####
demo_simulation_data <- readRDS("results/brownlow_simulation_results_2025.rds")
demo_results <- demo_simulation_data$raw_simulations

# Run correlation analysis
demo_correlations <- analyze_teammate_vote_correlations(
  simulation_results = demo_results,
  model_name = "ensemble",
  min_expected_votes = 5,
  min_correlation_strength = 0.15,
  verbose = TRUE
)

# Display top opportunities with detailed scenarios
print_correlation_scenarios(demo_correlations, top_n = 20)

# Save results and create plots
# NOTE: Output files will be the same as main analysis since they use model_name + target_season
save_correlation_analysis(demo_correlations, target_season = 2025, create_plots = TRUE)
