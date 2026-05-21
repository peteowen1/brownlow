# 04 - Enhanced Feature Engineering with GAM Coefficients ####

library(dplyr)
devtools::load_all()

# Load data
prepared_data <- readRDS("data/processed/modeling_data.rds")
gam_model <- readRDS("models/gam_production_model.rds")

# Extract GAM player coefficients
player_coefs <- mixedup::extract_random_effects(gam_model, add_group_N = TRUE) %>%
  dplyr::filter(group_var == "player_name") %>%
  dplyr::select(group, player_coef = value)

# Add coefficients to datasets
enhanced_train <- prepared_data$train %>%
  dplyr::left_join(player_coefs, by = c("player_name" = "group"))

enhanced_test <- prepared_data$test %>%
  dplyr::left_join(player_coefs, by = c("player_name" = "group"))

enhanced_holdout <- prepared_data$holdout %>%
  dplyr::left_join(player_coefs, by = c("player_name" = "group"))

# Save enhanced data
enhanced_prepared_data <- list(
  train = enhanced_train,
  test = enhanced_test,
  holdout = enhanced_holdout,
  feature_cols = c(prepared_data$feature_cols, "player_coef"),
  target_col = prepared_data$target_col,
  training_seasons = prepared_data$training_seasons,
  test_seasons = prepared_data$test_seasons,
  holdout_seasons = prepared_data$holdout_seasons,
  full_training_mode = prepared_data$full_training_mode
)

saveRDS(enhanced_prepared_data, "data/processed/enhanced_modeling_data.rds")
