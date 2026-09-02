# Do season-to-date form and reputation features help? ------------------------
#
# Two families, tested separately because they carry different risks:
#
#   OBSERVED  cumulative coaches votes, SuperCoach, fantasy, disposals etc. over
#             strictly prior rounds. No model involved, so no circularity.
#
#   PREDICTED cumulative model-predicted votes so far. Circular by nature - the
#             feature needs a model and the model uses the feature - so it is
#             built two-stage: a simple stage-1 model that has never seen the
#             row's own target produces the votes, those are accumulated, and
#             the stage-2 model consumes the accumulation. Stage 1 is fitted
#             walk-forward (train on seasons < S, predict S) so no row is ever
#             scored by a model that saw its target.
#
# Stage 1 is deliberately shallow. It only has to answer "who is polling well so
# far", not be optimal, and a less overfitted stage 1 gives a cleaner signal.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  mutate(player_coef = replace_na(player_coef, 0)) %>%
  arrange(season, date)

# ---- stage 1: walk-forward predicted votes ---------------------------------
all_data <- all_data %>% mutate(row_key = seq_len(n()))

# Stage 1's round count is found by walk-forward early stopping rather than
# guessed, and cached, so an unchanged setup pays for the search once.
stage1_fit <- optimal_nrounds(
  train_data   = all_data %>% dplyr::filter(!season %in% enhanced$holdout_seasons),
  feature_cols = feats, target_col = target, params = tuning$best_params,
  max_rounds   = 1500, early_stopping_rounds = 50
)
STAGE1_ROUNDS <- stage1_fit$nrounds

cat(sprintf("stage 1: walk-forward predictions at %d rounds\n", STAGE1_ROUNDS))
seasons <- sort(unique(all_data$season))

pred <- lapply(seasons, function(S) {
  te <- all_data %>% dplyr::filter(season == S)
  tr <- all_data %>% dplyr::filter(season < S)
  if (nrow(tr) == 0) return(tibble(row_key = te$row_key, pred_votes = 0))
  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = STAGE1_ROUNDS, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))
  cat(sprintf("  %d\n", S))
  tibble(row_key = te$row_key, pred_votes = as.numeric(pm %*% c(0, 1, 2, 3)))
}) %>% bind_rows()

all_data <- all_data %>% left_join(pred, by = "row_key")

# ---- build both feature families -------------------------------------------
OBSERVED <- intersect(c("coaches_votes", "supercoach_score", "afl_fantasy_score",
                        "rating_points", "disposals", "contested_possessions",
                        "goals", "score_involvements", "impact_score"),
                      names(all_data))

aug <- all_data %>%
  add_season_to_date_features(metrics = c(OBSERVED, "pred_votes"), prefix = "szn",
                              rank_metrics = c("coaches_votes", "supercoach_score",
                                               "pred_votes"))
assert_season_to_date_clean(aug, prefix = "szn")

obs_cols  <- grep("^szn_(coaches_votes|supercoach_score|afl_fantasy_score|rating_points|disposals|contested_possessions|goals|score_involvements|impact_score)_", names(aug), value = TRUE)
pred_cols <- grep("^szn_pred_votes_", names(aug), value = TRUE)
cat(sprintf("\nobserved features: %d | predicted-vote features: %d\n",
            length(obs_cols), length(pred_cols)))

# ---- evaluate ---------------------------------------------------------------
train   <- aug %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(aug %>% dplyr::filter(season %in% enhanced$holdout_seasons))

run <- function(f, label) {
  cat(sprintf("  %s\n", label))
  evaluate_seeds(xgb_fit_predict(f, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, label, seeds = seq_len(N_SEEDS), normalizer = "ipf")
}

results <- bind_rows(
  run(feats,                                    "baseline (no season-to-date)"),
  run(c(feats, obs_cols),                       "+ observed season-to-date"),
  run(c(feats, pred_cols),                      "+ predicted votes so far"),
  run(c(feats, obs_cols, pred_cols, "szn_games_prior"), "+ both")
)

comparison <- compare_configs(results, baseline_label = "baseline (no season-to-date)")
saveRDS(results, "analysis/harness/season_form_raw.rds")
write_csv(comparison, "analysis/harness/season_form.csv")

# ---- did it help the case that motivated it? --------------------------------
cat("\n=== Matt Rowell, round 7 2025 ===\n")
r <- aug %>% dplyr::filter(player_name == "Matt Rowell", season == 2025, match_round == "7")
if (nrow(r) == 1) {
  cat(sprintf("  games played earlier that season      %d\n", r$szn_games_prior))
  cat(sprintf("  coaches votes accumulated to date     %.0f  (rank %d in the competition)\n",
              r$szn_coaches_votes_sum, r$szn_coaches_votes_rank))
  cat(sprintf("  predicted votes accumulated to date   %.2f  (rank %d)\n",
              r$szn_pred_votes_sum, r$szn_pred_votes_rank))
  cat("  -> if he was already prominent, these are the columns that say so.\n")
}
