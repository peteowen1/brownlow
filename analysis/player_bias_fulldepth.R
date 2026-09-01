# Full-depth retest of the residual player-bias feature ------------------------
#
# First pass used a 150-round base model to build the expanding-window residuals
# and lost to the GAM's player_coef by ~2 sd. An underfit base leaves real signal
# in its residuals, so "player bias" was partly just base-model error. This
# repeats it at the tuned depth so the idea gets a fair hearing.
#
# The shrinkage constant k comes from the GAM: for a bs="re" term mgcv's
# smoothing parameter is the variance ratio sigma^2_resid / sigma^2_player, which
# is exactly the k in  bias = sum(prior residuals) / (prior games + k).
# The fitted 2025 production model gives sp = 5.81.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5
GAM_IMPLIED_K <- 5.81

prepared <- readRDS("data/processed/modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- prepared$feature_cols
target   <- prepared$target_col

all_data <- bind_rows(prepared$train, prepared$test, prepared$holdout) %>%
  arrange(season, date)
holdout <- attach_holdout_actuals(prepared$holdout)

# --- expanding-window residuals, at full tuned depth --------------------------
# For each season S the base model sees only seasons < S, so a residual for
# season S never depends on season S's own votes.
cat(sprintf("building residuals at %d rounds (was 150 in the first pass)\n",
            tuning$best_iteration))

seasons <- sort(unique(all_data$season))
residuals <- lapply(seasons[-1], function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  set.seed(1)
  model <- xgb.train(tuning$best_params,
                     xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                     nrounds = tuning$best_iteration, verbose = 0)
  pm <- predict(model, as.matrix(te[, feats]))
  if (is.null(dim(pm))) pm <- matrix(pm, ncol = 4, byrow = TRUE)
  cat(sprintf("  season %d: %d rows\n", S, nrow(te)))
  tibble(player_name = te$player_name, season = S,
         resid = te[[target]] - as.numeric(pm %*% c(0, 1, 2, 3)))
}) %>% bind_rows()

# --- shrink each player's cumulative prior residual toward zero ---------------
make_bias <- function(k) {
  residuals %>%
    group_by(player_name, season) %>%
    summarise(season_resid = sum(resid), games = n(), .groups = "drop") %>%
    arrange(player_name, season) %>%
    group_by(player_name) %>%
    mutate(prior_resid = cumsum(season_resid) - season_resid,
           prior_games = cumsum(games) - games) %>%
    ungroup() %>%
    mutate(player_bias = prior_resid / (prior_games + k)) %>%
    select(player_name, season, player_bias, player_prior_games = prior_games)
}

evaluate_with <- function(bias_tbl, label) {
  augmented <- all_data %>%
    left_join(bias_tbl, by = c("player_name", "season")) %>%
    mutate(player_bias = replace_na(player_bias, 0),
           player_prior_games = replace_na(player_prior_games, 0))
  tr <- augmented %>% dplyr::filter(!season %in% prepared$holdout_seasons)
  ho <- augmented %>% dplyr::filter(season %in% prepared$holdout_seasons) %>%
    mutate(Actual = holdout$Actual[match(
      paste(player_name, date), paste(holdout$player_name, holdout$date))])
  stopifnot(!anyNA(ho$Actual), sum(ho$Actual) == 1242)
  use <- c(feats, "player_bias", "player_prior_games")
  evaluate_seeds(xgb_fit_predict(use, target, tuning$best_params, tuning$best_iteration),
                 tr, ho, label, seeds = seq_len(N_SEEDS), normalizer = "scale")
}

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, tuning$best_params, tuning$best_iteration),
                 all_data %>% dplyr::filter(!season %in% prepared$holdout_seasons),
                 holdout, "no player effect", seeds = seq_len(N_SEEDS)),
  lapply(c(GAM_IMPLIED_K, 20, 50), function(k)
    evaluate_with(make_bias(k), sprintf("player_bias k=%.2f", k))) %>% bind_rows()
)

comparison <- compare_configs(results, baseline_label = "no player effect")
dir.create("analysis/harness", showWarnings = FALSE, recursive = TRUE)
write_csv(comparison, "analysis/harness/player_bias_fulldepth.csv")
cat("\nwritten: analysis/harness/player_bias_fulldepth.csv\n")
cat("\nCompare against player_coef once 03 and 04 have regenerated it.\n")
