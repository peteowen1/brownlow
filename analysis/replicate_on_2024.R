# Out-of-sample replication on 2024 ------------------------------------------
#
# Everything adopted or rejected so far was measured on the 2025 holdout, and
# two candidates had their parameters CHOSEN on that same holdout: Plackett-Luce
# beta (5 values tried, 0.8 reported) and the PL/IPF blend weight (4 tried, 0.25
# reported), plus player_bias k (3 tried, 5.81 reported). Selecting and scoring
# on one season biases those margins upward, which matters when the margins in
# question are ~0.0001.
#
# This refits on 2015-2023, scores on 2024, and holds every parameter FIXED at
# the value chosen on 2025. Nothing here is tuned. A gain that replicates is
# real; one that vanishes was selection noise.
#
# It also re-tests IPF itself, which is the largest change adopted this session
# and deserves the same scrutiny as the ideas being rejected.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS   <- 5
HOLDOUT   <- 2024
PL_BETA   <- 0.8    # chosen on 2025, fixed here
PL_WEIGHT <- 0.25   # chosen on 2025, fixed here
BIAS_K    <- 5.81   # GAM-implied, fixed here

prepared <- readRDS("data/processed/modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- prepared$feature_cols
target   <- prepared$target_col

# 2024 votes are real in this dataset, so no external results file is needed.
all_data <- bind_rows(prepared$train, prepared$test, prepared$holdout) %>%
  dplyr::filter(season <= HOLDOUT) %>%
  arrange(season, date)

train   <- all_data %>% dplyr::filter(season < HOLDOUT)
holdout <- all_data %>% dplyr::filter(season == HOLDOUT) %>%
  mutate(Actual = .data[[target]])

stopifnot(nrow(holdout) > 0, sum(holdout$Actual) == 6 * n_distinct(holdout$match_id))
cat(sprintf("train %d rows (%d-%d) | holdout %d rows (%d) | %d matches | %d votes\n\n",
            nrow(train), min(train$season), max(train$season),
            nrow(holdout), HOLDOUT, n_distinct(holdout$match_id), sum(holdout$Actual)))

rmse <- function(v) sqrt(mean((v - holdout$Actual)^2))

# --- constraint methods share one fit per seed -------------------------------
cat("fitting base model per seed\n")
base_fit <- xgb_fit_predict(feats, target, tuning$best_params, tuning$best_iteration)
base_probs <- lapply(seq_len(N_SEEDS), function(s) base_fit(train, holdout, s))

score <- function(f, label) {
  vals <- vapply(base_probs, function(pm) rmse(f(pm)), numeric(1))
  tibble(label = label, normalizer = NA_character_, rmse = mean(vals), sd = sd(vals),
         min = min(vals), max = max(vals), n_seeds = length(vals))
}

pl_votes <- function(pm) pl_expected_votes(log(pmax(pm[, 4], 1e-12)),
                                           holdout$match_id, beta = PL_BETA)

results <- bind_rows(
  score(function(pm) normalize_votes_scale(pm, holdout$match_id), "scale-to-6 (old method)"),
  score(function(pm) normalize_votes_ipf(pm, holdout$match_id),   "IPF (adopted)"),
  score(function(pm) PL_WEIGHT * pl_votes(pm) +
                     (1 - PL_WEIGHT) * normalize_votes_ipf(pm, holdout$match_id),
        sprintf("%.2f PL + %.2f IPF (rejected)", PL_WEIGHT, 1 - PL_WEIGHT)),
  score(function(pm) pl_votes(pm), "Plackett-Luce alone (rejected)")
)

# --- player_bias needs its own feature set, so its own fits ------------------
cat("\nbuilding expanding-window residuals for player_bias\n")
seasons <- sort(unique(all_data$season))
residuals <- lapply(seasons[-1], function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = tuning$best_iteration, verbose = 0)
  pm <- predict(m, as.matrix(te[, feats]))
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
  mutate(player_bias = prior_resid / (prior_games + BIAS_K)) %>%
  select(player_name, season, player_bias, player_prior_games = prior_games)

aug <- all_data %>%
  left_join(bias_tbl, by = c("player_name", "season")) %>%
  mutate(player_bias = replace_na(player_bias, 0),
         player_prior_games = replace_na(player_prior_games, 0))

train_b   <- aug %>% dplyr::filter(season < HOLDOUT)
holdout_b <- aug %>% dplyr::filter(season == HOLDOUT) %>% mutate(Actual = .data[[target]])

results <- bind_rows(results,
  evaluate_seeds(xgb_fit_predict(c(feats, "player_bias", "player_prior_games"),
                                 target, tuning$best_params, tuning$best_iteration),
                 train_b, holdout_b, sprintf("player_bias k=%.2f (rejected)", BIAS_K),
                 seeds = seq_len(N_SEEDS), normalizer = "ipf")
)

saveRDS(results, "analysis/harness/replication_2024_raw.rds")
comparison <- compare_configs(results, baseline_label = "IPF (adopted)")
write_csv(comparison, "analysis/harness/replication_2024.csv")
cat("\nwritten: analysis/harness/replication_2024.csv\n")
cat("\nParameters were fixed at their 2025-chosen values; nothing was tuned here.\n")
