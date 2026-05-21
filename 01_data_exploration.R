# 01 - Brownlow Medal Data Exploration ####
# This script performs exploratory data analysis on AFL Brownlow Medal data
# to understand voting patterns, player performance relationships, and identify
# key features for prediction modeling.

# Setup ####

## Load required libraries ----
library(dplyr)
library(fitzRoy)
library(ggplot2)
library(tidyr)
library(corrplot)
library(VIM)
library(stringr)
library(lubridate)
library(tibble)
library(janitor)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create("plots", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/cache", showWarnings = FALSE, recursive = TRUE)

# Data Collection and Enhancement ####

cat("Loading enhanced datathon dataset...")
start_time <- Sys.time()

brownlow_data <- enhance_datathon_data(
  file_path = "data/brownlow_datathon_dataset.csv",
  add_umpires = TRUE,
  add_weather = TRUE,
  progress = TRUE,
  force_reload = TRUE
)

data_collection_time <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf(" ✓ %d records from %s seasons in %.1f seconds\n",
            nrow(brownlow_data$stats),
            paste(range(brownlow_data$metadata$seasons), collapse = "-"),
            as.numeric(data_collection_time)))

# Save data checkpoint
saveRDS(brownlow_data, "data/cache/checkpoint_01_data_loaded.rds")

# Comprehensive Exploration ####

cat("Running comprehensive exploration...")
exploration_results <- run_comprehensive_exploration(
  brownlow_data = brownlow_data,
  save_plots = TRUE,
  verbose = FALSE
)

# Generate and Save Summary ####

exploration_summary <- generate_exploration_summary(brownlow_data, exploration_results)
key_findings <- save_exploration_results(exploration_summary, verbose = TRUE)

# Final Summary ####

total_time <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\n🎉 Data exploration complete! Total runtime: %.1f minutes\n", as.numeric(total_time)))