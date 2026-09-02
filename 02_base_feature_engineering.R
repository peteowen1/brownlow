# 02 - Base Feature Engineering for Brownlow Medal Prediction ####
# This script creates BASE features from raw AFL statistics (no GAM coefficients)
# Used by both GAM (03_gam_model.R) and enhanced feature engineering (04_enhanced_feature_engineering.R)

# Setup ####

## Load required libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(lubridate)
library(stringr)
library(tibble)
library(caret)
library(fitzRoy)

## Load custom functions ----
devtools::load_all()

## Set up environment ----
set.seed(42)
theme_set(theme_minimal())

## Create necessary directories ----
dir.create("plots/features", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Load Data ####

cat("Loading enhanced dataset...")
brownlow_data <- enhance_datathon_data(
  file_path = "data/brownlow_datathon_dataset.csv",
  add_umpires = TRUE,
  add_weather = TRUE,
  progress = FALSE
)

cat(sprintf(" %d records from %s seasons\n",
            nrow(brownlow_data$stats),
            paste(range(brownlow_data$metadata$seasons), collapse = "-")))

# Create Base Features ####

cat("Creating features...")
features_data <- create_all_features(
  brownlow_data = brownlow_data,
  include_rolling = TRUE,  # Enable rolling features with optimized defaults
  rolling_windows = c(5, 10)  # Reduced from c(3, 5, 10) for efficiency
)

cat(sprintf(" %d observations, %d columns\n", nrow(features_data), ncol(features_data)))

# Express the composites the model leans on relative to the same match, so a
# league-wide shift in scoring does not move what the model reads. Needed for
# 2026: AFL Fantasy scores hitouts, and the centre bounce is gone, so ruck
# scores drop mechanically. See POSTMORTEM Sec 5o and R/match_relative_features.R.
cat("Adding match-relative composites...")
features_data <- add_match_relative_features(features_data)
assert_match_relative_clean(features_data)
cat(sprintf(" %d columns\n", ncol(features_data)))

# Feature Analysis and Selection ####

correlation_results <- analyze_feature_correlations(
  features_data = features_data,
  target_col = "brownlow_votes",
  save_plots = TRUE,
  verbose = TRUE
)

final_features <- select_final_features(
  correlation_results = correlation_results,
  correlation_threshold = 0,
  multicollinearity_threshold = 0.99,
  verbose = TRUE
)

# Create Modeling Datasets ####

cat("Preparing modeling data...")
modeling_data <- correlation_results$cleaned_data %>%
  select(
    date, season, match_round, team, player_name, any_of("match_id"),
    brownlow_votes,
    all_of(final_features)
  ) %>%
  mutate(across(all_of(final_features), as.numeric)) %>%
  dplyr::filter(!is.na(brownlow_votes))

# Create train/test split
prepared_data <- prepare_modeling_data(
  features = modeling_data,
  target_col = "brownlow_votes",
  test_seasons = c(2023, 2024)
)

cat(sprintf(" train: %d, test: %d\n",
            nrow(prepared_data$train),
            nrow(prepared_data$test)))

# Create Complete Predictions Dataset ####

cat("Creating prediction templates...")
all_seasons_data <- bind_rows(prepared_data$train, prepared_data$test) %>%
  arrange(season, date, player_name)

total_predictions_template <- create_prediction_templates(
  all_seasons_data = all_seasons_data,
  final_features = final_features,
  target_seasons = 2015:2025,
  verbose = TRUE
)

# Feature Summary and Documentation ####

feature_docs <- create_feature_summary(
  final_features = final_features,
  modeling_data = modeling_data,
  feature_correlations = correlation_results$correlations,
  save_docs = TRUE
)

# Save Processed Data ####

cat("Saving processed data...")

# Save modeling datasets
saveRDS(prepared_data, "data/processed/modeling_data.rds")
saveRDS(modeling_data, "data/processed/full_features_dataset.rds")
saveRDS(total_predictions_template, "data/processed/total_predictions_template.rds")

# Save feature metadata
feature_metadata <- list(
  final_features = final_features,
  feature_summary = feature_docs$summary,
  feature_categories = feature_docs$categories,
  correlation_analysis = correlation_results$correlations,
  data_splits = list(
    train_seasons = setdiff(unique(prepared_data$train$season), prepared_data$test_seasons),
    test_seasons = prepared_data$test_seasons,
    train_size = nrow(prepared_data$train),
    test_size = nrow(prepared_data$test)
  ),
  processing_date = Sys.time()
)

saveRDS(feature_metadata, "data/processed/feature_metadata.rds")

cat(sprintf(" ✓ %d features, top correlation: %.3f\n",
            length(final_features),
            feature_docs$summary$correlation_with_votes[1]))
