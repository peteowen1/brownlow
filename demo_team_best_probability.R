# Demo: Team Best Probability in Simulation Results
# Shows how the enhanced simulation results table includes team best probabilities
# NOTE: This demo reads from 07_brownlow_simulation.R outputs but uses "demo_" prefixed variables

library(dplyr)
devtools::load_all()

cat("=== TEAM BEST PROBABILITY DEMO ===\n")

# Overview ####
cat("\n=== NEW FEATURE: TEAM BEST PROBABILITY ===\n")
cat("✅ Added: team_best_probability column to simulation results table\n")
cat("✅ Shows: Probability each player will poll the most votes on their team\n")
cat("✅ Useful for: Identifying team leaders and captaincy chances\n")
cat("✅ Format: Percentage (0-100%) indicating team leadership probability\n")

# Example interpretation ####
cat("\n=== EXAMPLE INTERPRETATION ===\n")
cat("Player with 85% team_best_probability:\n")
cat("  → In 85% of simulations, this player gets more votes than any teammate\n")
cat("  → Strong team leader, likely captain or star midfielder\n")
cat("  → High confidence for team-based betting markets\n")

cat("\nPlayer with 15% team_best_probability:\n")
cat("  → Only leads team in 15% of simulations\n")
cat("  → Secondary option on a strong team\n")
cat("  → May still have decent overall win probability if team is weak\n")

# Simulation with team best probabilities ####
if (file.exists("results/brownlow_simulation_results_2025.rds")) {
  cat("\n=== LOADING EXAMPLE SIMULATION RESULTS ===\n")

  # Load existing simulation results
  demo_sim_data <- readRDS("results/brownlow_simulation_results_2025.rds")

  if (!is.null(demo_sim_data$combined_results) &&
      "team_best_probability" %in% names(demo_sim_data$combined_results)) {

    cat("✅ Found simulation results with team best probabilities!\n")

    demo_results_table <- demo_sim_data$combined_results

    # Show overall top candidates
    cat("\n=== TOP 10 OVERALL CANDIDATES ===\n")
    top_overall <- demo_results_table %>%
      dplyr::arrange(desc(avg_win_probability)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::select(player_name, team, avg_win_probability, team_best_probability, avg_expected_votes) %>%
      dplyr::mutate(
        avg_win_probability = round(avg_win_probability, 1),
        team_best_probability = round(team_best_probability, 1),
        avg_expected_votes = round(avg_expected_votes, 1)
      )

    print(top_overall)

    # Show team leaders (highest team_best_probability)
    cat("\n=== TOP 10 TEAM LEADERS (Highest Team Best Probability) ===\n")
    team_leaders <- demo_results_table %>%
      dplyr::arrange(desc(team_best_probability)) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::select(player_name, team, team_best_probability, avg_win_probability, avg_expected_votes) %>%
      dplyr::mutate(
        team_best_probability = round(team_best_probability, 1),
        avg_win_probability = round(avg_win_probability, 1),
        avg_expected_votes = round(avg_expected_votes, 1)
      )

    print(team_leaders)

    # Team-by-team analysis
    cat("\n=== TEAM-BY-TEAM LEADERS ===\n")
    team_analysis <- demo_results_table %>%
      dplyr::group_by(team) %>%
      dplyr::slice_max(team_best_probability, n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(team_best_probability)) %>%
      dplyr::select(player_name, team, team_best_probability, avg_win_probability) %>%
      dplyr::mutate(
        team_best_probability = round(team_best_probability, 1),
        avg_win_probability = round(avg_win_probability, 1)
      )

    print(team_analysis)

    # Interesting insights
    cat("\n=== INTERESTING INSIGHTS ===\n")

    # Players with high overall probability but low team best probability (strong teams)
    strong_team_players <- demo_results_table %>%
      dplyr::filter(avg_win_probability > 2.0, team_best_probability < 70) %>%
      dplyr::arrange(desc(avg_win_probability)) %>%
      dplyr::slice_head(n = 5)

    if (nrow(strong_team_players) > 0) {
      cat("🔥 Strong team players (high win prob, but teammates also strong):\n")
      for (i in 1:nrow(strong_team_players)) {
        cat(sprintf("  %s (%s): %.1f%% win prob, %.1f%% team best prob\n",
                    strong_team_players$player_name[i],
                    strong_team_players$team[i],
                    strong_team_players$avg_win_probability[i],
                    strong_team_players$team_best_probability[i]))
      }
    }

    # Players with very high team leadership (>90%)
    dominant_leaders <- demo_results_table %>%
      dplyr::filter(team_best_probability > 90) %>%
      dplyr::arrange(desc(team_best_probability))

    if (nrow(dominant_leaders) > 0) {
      cat("\n⭐ Dominant team leaders (>90% team best probability):\n")
      for (i in 1:min(5, nrow(dominant_leaders))) {
        cat(sprintf("  %s (%s): %.1f%% team best prob, %.1f%% win prob\n",
                    dominant_leaders$player_name[i],
                    dominant_leaders$team[i],
                    dominant_leaders$team_best_probability[i],
                    dominant_leaders$avg_win_probability[i]))
      }
    }

  } else {
    cat("❌ No team best probabilities found in existing results\n")
    cat("Run 07_brownlow_simulation.R to generate results with team best probabilities\n")
  }

} else {
  cat("\n❌ No simulation results found\n")
  cat("Run 07_brownlow_simulation.R to generate simulation results first\n")
}

# Usage in analysis ####
cat("\n=== USAGE IN ANALYSIS ===\n")
cat("📊 Team captaincy analysis:\n")
cat("   • Players with >80% team_best_probability are clear team leaders\n")
cat("   • Multiple players with 40-60% suggest balanced teams\n")

cat("\n📊 Betting and fantasy insights:\n")
cat("   • High team_best_probability + High win_probability = Premium pick\n")
cat("   • High team_best_probability + Low win_probability = Good value on weak team\n")
cat("   • Low team_best_probability + High win_probability = Strong team with multiple stars\n")

cat("\n📊 Strategic insights:\n")
cat("   • Teams with dominant leaders (>90%) have clear focal points\n")
cat("   • Teams with split leadership suggest depth or uncertainty\n")
cat("   • Compare with actual team captains for validation\n")

# Technical details ####
cat("\n=== TECHNICAL IMPLEMENTATION ===\n")
cat("🔧 Calculation method:\n")
cat("   • Run Monte Carlo simulations (e.g., 2000 iterations)\n")
cat("   • For each simulation, identify highest vote-getter per team\n")
cat("   • Count how often each player leads their team\n")
cat("   • Convert to percentage: (times_led_team / total_simulations) × 100\n")

cat("\n🔧 Output locations:\n")
cat("   • Main results: results/brownlow_win_probabilities_YYYY.csv\n")
cat("   • Top candidates: results/brownlow_top_candidates_summary_YYYY.csv\n")
cat("   • Detailed team analysis: results/team_best_players_MODEL_YYYY.csv\n")

cat("\n🔧 Column description:\n")
cat("   • team_best_probability: Percentage chance of leading team in Brownlow votes\n")
cat("   • Range: 0-100% (higher = more likely to be team's best)\n")
cat("   • Interpretation: Probability of being the team's highest vote-getter\n")

cat("\n✅ TEAM BEST PROBABILITY FEATURE READY FOR USE!\n")
cat("This provides valuable insights into team leadership and relative player strength.\n")