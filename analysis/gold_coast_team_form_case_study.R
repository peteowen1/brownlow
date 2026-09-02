# Gold Coast case study: does team_form_vs_expectation help where it should? ---
#
# analysis/team_form_vs_expectation.R found no aggregate improvement on the
# full 2025 holdout (delta -0.00014 vs a 0.00092 noise threshold). But
# POSTMORTEM Sec 3 identifies the miss it targets as concentrated, not
# diffuse: 6 of 16 worst 2025 misses were Gold Coast (Rowell x3-4, Miller,
# Flanders), driven by Gold Coast making finals for the first time -- a
# club-level step change the model's career/prior-season features can't see.
# A whole-holdout RMSE average over ~600 players can hide a real, useful
# effect on the ~25 Gold Coast rows that matter. This isolates it, the same
# way analysis/rowell_live_vs_datathon.R isolated one player rather than
# trusting an aggregate.
#
# Single seed, like rowell_live_vs_datathon.R and diagnose_freeze_artifact.R
# -- this is mechanism diagnosis, not a claim requiring a seed sweep.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col
params   <- tuning$best_params

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  mutate(row_id = dplyr::row_number()) %>%
  add_team_form_vs_expectation()
assert_team_form_clean(all_data)
new_cols <- c("team_prior_season_win_rate", "team_current_season_win_rate_so_far",
             "team_form_vs_expectation")

n_train <- nrow(enhanced$train)
n_test  <- nrow(enhanced$test)
train   <- all_data %>% dplyr::filter(row_id <= n_train)
test    <- all_data %>% dplyr::filter(row_id > n_train, row_id <= n_train + n_test)
holdout <- all_data %>% dplyr::filter(row_id > n_train + n_test)
holdout <- attach_holdout_actuals(holdout)
train_all <- bind_rows(train, test)

cat("\n=== Gold Coast's team_form_vs_expectation trajectory through 2025 ===\n")
gc_form <- holdout %>%
  dplyr::filter(standardize_team_name(team) == standardize_team_name("Gold Coast")) %>%
  dplyr::distinct(match_round, team_prior_season_win_rate, team_current_season_win_rate_so_far,
                  team_form_vs_expectation) %>%
  dplyr::arrange(suppressWarnings(as.numeric(match_round)))
print(as.data.frame(gc_form), row.names = FALSE, digits = 3)

cat("\n=== rounds via early stopping (cached from the earlier run if unchanged) ===\n")
rounds <- optimal_nrounds(train_all, feats, target, params,
                          max_rounds = 400, early_stopping_rounds = 30, verbose = TRUE)$nrounds

dtr_base <- xgboost::xgb.DMatrix(as.matrix(train_all[, feats]), label = train_all[[target]])
dtr_new  <- xgboost::xgb.DMatrix(as.matrix(train_all[, c(feats, new_cols)]), label = train_all[[target]])

set.seed(1)
m_base <- xgboost::xgb.train(params, dtr_base, nrounds = rounds, verbose = 0)
set.seed(1)
m_new  <- xgboost::xgb.train(params, dtr_new, nrounds = rounds, verbose = 0)

pm_base <- as_prob_matrix(predict(m_base, as.matrix(holdout[, feats])), nrow(holdout))
pm_new  <- as_prob_matrix(predict(m_new,  as.matrix(holdout[, c(feats, new_cols)])), nrow(holdout))
holdout$pred_base <- as.numeric(pm_base %*% c(0, 1, 2, 3))
holdout$pred_new  <- as.numeric(pm_new  %*% c(0, 1, 2, 3))

cat("\n=== every Gold Coast row with actual votes > 0, 2025 ===\n")
gc_votes <- holdout %>%
  dplyr::filter(standardize_team_name(team) == standardize_team_name("Gold Coast"), Actual > 0) %>%
  dplyr::transmute(round = match_round, player = player_name, actual = Actual,
                   pred_base = round(pred_base, 3), pred_new = round(pred_new, 3),
                   base_err = round(actual - pred_base, 3), new_err = round(actual - pred_new, 3),
                   team_form_gap = round(team_form_vs_expectation, 3)) %>%
  dplyr::arrange(suppressWarnings(as.numeric(round)))
print(as.data.frame(gc_votes), row.names = FALSE)

cat("\n=== the specific misses named in POSTMORTEM Sec 3 ===\n")
named <- gc_votes %>% dplyr::filter(player %in% c("Matt Rowell", "Sam Flanders", "Touk Miller"))
print(as.data.frame(named), row.names = FALSE)

cat("\n=== all Gold Coast rows: base MAE vs new MAE (votes > 0 only) ===\n")
cat(sprintf("  base MAE: %.4f\n", mean(abs(gc_votes$base_err))))
cat(sprintf("  new  MAE: %.4f\n", mean(abs(gc_votes$new_err))))

write_csv(gc_votes, "analysis/harness/gold_coast_team_form_case_study.csv")
cat("\nwritten: analysis/harness/gold_coast_team_form_case_study.csv\n")
