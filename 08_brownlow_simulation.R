# 08 - Brownlow Medal Season Simulation
# Monte Carlo simulation of Brownlow Medal seasons using model predictions

# Setup
library(tidyverse)
library(furrr)
library(parallel)
devtools::load_all()

set.seed(42)
theme_set(theme_minimal())

# Configuration
target_season <- 2026
n_simulations <- 2000
models_to_run <- c("ensemble")  # Options: "xgb", "gam", "ensemble"
n_cores <- round(detectCores() / 2)

# Load and prepare data
plan(multisession, workers = n_cores)
sim_data <- load_simulation_data(target_season, verbose = FALSE)

# Validate and normalize probabilities
prepare_model_data <- function(model_name, data) {
  if (is.null(data)) return(NULL)

  prob_cols <- grep("_prob_[0-3]$", names(data), value = TRUE)
  if (length(prob_cols) != 4) return(NULL)

  prob_matrix <- as.matrix(data[, prob_cols])
  validation <- validate_probability_constraints(prob_matrix, data$match_id, verbose = FALSE)

  if (!validation$all_constraints_satisfied) {
    norm_result <- normalize_probability_matrix(prob_matrix, data$match_id, verbose = FALSE)
    data[, prob_cols] <- norm_result$normalized_probabilities
  }

  return(data)
}

sim_data <- map(sim_data, ~ prepare_model_data(names(sim_data)[which(sim_data == .x)], .x))
available_models <- names(sim_data)[!sapply(sim_data, is.null)]
models_to_run <- intersect(models_to_run, available_models)

devtools::install()

# Run simulations
run_model_simulation <- function(model_name, data, n_sims) {
  prob_prefix <- paste0(model_name, "_prob_")
  future_map(
    1:n_sims,
    ~simulate_single_season(sim_data = data, prob_prefix = prob_prefix, simulation_id = .x, method = "expected_values"),
    .options = furrr_options(seed = TRUE, packages = c("brownlow", "dplyr")),
    .progress = TRUE
  )
}

simulation_results <- map(
  set_names(models_to_run),
  ~run_model_simulation(.x, sim_data[[.x]], n_simulations)
)

# Analyze results
final_results <- analyze_and_save_results(simulation_results, target_season, verbose = FALSE)
plan(sequential)

# Summary
cat(sprintf("Simulated %d runs for %s\n", n_simulations, paste(models_to_run, collapse = ", ")))
if (nrow(final_results$top_candidates) > 0) {
  winner <- final_results$top_candidates[1, ]
  cat(sprintf("Top candidate: %s (%s) - %.1f%% win probability\n",
              winner$player_name, winner$team, winner$avg_win_probability))
}

results_table <- final_results$combined_results %>% arrange(-mean_votes)

View(results_table)

# Round-by-round expected votes table
round_table <- create_top_players_round_table(sim_data[[models_to_run[1]]], results_table, models_to_run[1])
if (!is.null(round_table)) View(round_table)

View(round_table)

#
sim_data$ensemble %>%
  group_by(match_id) %>%
  summarise(sum(ensemble_prob_0),sum(ensemble_prob_1),sum(ensemble_prob_2),sum(ensemble_prob_3))
