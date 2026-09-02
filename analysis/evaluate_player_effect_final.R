# Can the residual bias feature replace the GAM entirely? ----------------------
#
# At full tuned depth player_bias (0.36003) and the GAM's player_coef (0.36019)
# are indistinguishable under scale-to-6. If that holds under IPF, and if the two
# are redundant rather than additive, then the whole mgcv/bam stage can go: one
# hour of fitting and a 48 MB model replaced by a group_by and a shrinkage
# constant read off the GAM once.
#
# The shrinkage constant k = 5.81 is the fitted smoothing parameter of the
# s(player_name, bs="re") term, which for a random effect IS the variance ratio
# sigma^2_resid / sigma^2_player.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5
seeds <- seq_len(N_SEEDS)
K <- 5.81

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- enhanced$target_col

with_coef    <- enhanced$feature_cols
without_coef <- setdiff(with_coef, "player_coef")

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(player_coef = replace_na(player_coef, 0))   # unseen player => zero effect

# --- expanding-window residuals at full depth --------------------------------
cat("building residuals (base model sees only strictly prior seasons)\n")
seasons <- sort(unique(all_data$season))
residuals <- lapply(seasons[-1], function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, without_coef]), label = tr[[target]]),
                 nrounds = tuning$best_iteration, verbose = 0)
  pm <- predict(m, as.matrix(te[, without_coef]))
  if (is.null(dim(pm))) pm <- matrix(pm, ncol = 4, byrow = TRUE)
  cat(sprintf("  %d\n", S))
  tibble(player_name = te$player_name, season = S,
         resid = te[[target]] - as.numeric(pm %*% c(0, 1, 2, 3)))
}) %>% bind_rows()

bias_tbl <- residuals %>%
  group_by(player_name, season) %>%
  summarise(season_resid = sum(resid), games = n(), .groups = "drop") %>%
  arrange(player_name, season) %>%
  group_by(player_name) %>%
  mutate(prior_resid = cumsum(season_resid) - season_resid,
         prior_games = cumsum(games) - games) %>%
  ungroup() %>%
  mutate(player_bias = prior_resid / (prior_games + K)) %>%
  select(player_name, season, player_bias, player_prior_games = prior_games)

saveRDS(bias_tbl, "data/processed/player_bias.rds")
cat(sprintf("player_bias built for %d player-seasons -> data/processed/player_bias.rds\n\n",
            nrow(bias_tbl)))

augmented <- all_data %>%
  left_join(bias_tbl, by = c("player_name", "season")) %>%
  mutate(player_bias = replace_na(player_bias, 0),
         player_prior_games = replace_na(player_prior_games, 0))

train   <- augmented %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(augmented %>%
                                   dplyr::filter(season %in% enhanced$holdout_seasons))

run <- function(feats, label) {
  cat(sprintf("  %s\n", label))
  evaluate_seeds(xgb_fit_predict(feats, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, label, seeds = seeds, normalizer = "ipf")
}

bias_cols <- c("player_bias", "player_prior_games")
results <- bind_rows(
  run(without_coef,                    "neither"),
  run(with_coef,                       "player_coef only (needs the GAM)"),
  run(c(without_coef, bias_cols),      "player_bias only (no GAM)"),
  run(c(with_coef, bias_cols),         "both")
)

comparison <- compare_configs(results, baseline_label = "neither")
write_csv(comparison, "analysis/harness/player_effect_final.csv")
cat("\nwritten: analysis/harness/player_effect_final.csv\n")
cat("\nIf 'player_bias only' matches 'player_coef only', 03_gam_model.R can be retired.\n")
