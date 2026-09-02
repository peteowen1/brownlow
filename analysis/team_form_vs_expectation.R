# Team form vs pre-season expectation: does it move the datathon model? --------
#
# Pete, 2026-09-02: of the three external-data ideas raised (odds, pre-season
# expectation, media buzz), this is the one he wants now. No external source
# needed — team_prior_season_win_rate and team_current_season_win_rate_so_far
# (R/team_form_features.R) are derived purely from team_score/opposition_score,
# already in every row. Targets the Gold Coast 2025 miss (POSTMORTEM Sec 3).
#
# Same harness discipline as every other feature test this session: 5-seed
# comparison against the pooled noise floor, not a single run.

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
cat(sprintf("\nsummary of new columns (n=%d rows):\n", nrow(all_data)))
print(summary(all_data[, new_cols]))

n_train <- nrow(enhanced$train)
n_test  <- nrow(enhanced$test)
train   <- all_data %>% dplyr::filter(row_id <= n_train)
test    <- all_data %>% dplyr::filter(row_id > n_train, row_id <= n_train + n_test)
holdout <- all_data %>% dplyr::filter(row_id > n_train + n_test)
stopifnot(nrow(train) == n_train, nrow(test) == n_test, nrow(holdout) == nrow(enhanced$holdout))

holdout <- attach_holdout_actuals(holdout)
stopifnot(!anyNA(holdout$match_id))

train_all <- bind_rows(train, test)

# One shared round count for both configs -- 3 extra numeric columns on top of
# 258 (~1.2%) is nowhere near the 259->268 (~3.5%) in-season case that needed
# its own honest re-tune (R/honest_tuning.R); reusing rounds keeps the
# comparison fair without paying for a second early-stopping search.
cat("\n=== rounds via early stopping ===\n")
rounds <- optimal_nrounds(train_all, feats, target, params,
                          max_rounds = 400, early_stopping_rounds = 30, verbose = TRUE)$nrounds
cat(sprintf("rounds: %d\n", rounds))

fp_base <- xgb_fit_predict(feats, target, params, rounds)
fp_new  <- xgb_fit_predict(c(feats, new_cols), target, params, rounds)

results <- bind_rows(
  evaluate_seeds(fp_base, train_all, holdout, "baseline (no team form)", seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(fp_new,  train_all, holdout, "+ team form vs expectation", seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = "baseline (no team form)")

write_csv(results, "analysis/harness/team_form_vs_expectation.csv")
cat("\nwritten: analysis/harness/team_form_vs_expectation.csv\n")
