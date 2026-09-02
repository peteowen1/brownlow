# Match-relative versions of the composites the model leans on ------------------
#
# Two findings meet here.
#
# 1. Sec 5n: gains will not come from rare profiles, because a feature helping
#    ~1% of rows cannot move an RMSE over 9,522. They must come from something
#    touching MANY rows slightly. Every row has a supercoach score.
#
# 2. Shift exposure (Sec 5l). 92.5% of model gain sits in features my audit
#    called "absolute", but that overstates fragility: coaches_votes (35.9%) is
#    a fixed 5-4-3-2-1 per-match allocation, so it is rank-like and immune to a
#    league-wide shift. The genuinely exposed block is supercoach_score (13.5%)
#    + afl_fantasy_score (5.4%) -- absolute scores from FIXED-WEIGHT formulas.
#    AFL Fantasy scores hitouts at +1, so removing the centre bounce
#    mechanically lowers ruck scores in 2026 without any player changing.
#
# A model trained on 2015-2025 levels reads a 2026 score of 105 as the same
# thing it always was. Expressing the same quantity against the other players
# in the same match removes that dependence: if the whole league shifts, the
# rank does not.
#
# This is insurance AND a possible gain, and the harness decides which. Adding
# rank-within-match versions of features the model already leans on is not new
# information -- it is the same information in a form that survives a
# distribution shift.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col
params   <- tuning$best_params

BASE <- c("supercoach_score", "afl_fantasy_score", "impact_score", "disposals")

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  mutate(row_id = dplyr::row_number()) %>%
  group_by(match_id) %>%
  mutate(dplyr::across(dplyr::all_of(BASE),
                       list(rank = ~dplyr::min_rank(-.x) / dplyr::n(),
                            vsmax = ~.x / pmax(max(.x, na.rm = TRUE), 1e-9)),
                       .names = "{.col}_{.fn}")) %>%
  ungroup()

new_cols <- as.vector(outer(BASE, c("rank", "vsmax"), paste, sep = "_"))
stopifnot(all(new_cols %in% names(all_data)))
cat(sprintf("added %d match-relative features\n", length(new_cols)))
print(summary(all_data[, new_cols[1:4]]))

# Sanity: the rank of the match's best supercoach score must be the minimum.
chk <- all_data %>% group_by(match_id) %>%
  summarise(ok = which.max(supercoach_score) == which.min(supercoach_score_rank), .groups = "drop")
cat(sprintf("rank orientation correct in %d of %d matches\n", sum(chk$ok), nrow(chk)))
stopifnot(all(chk$ok))

n_train <- nrow(enhanced$train); n_test <- nrow(enhanced$test)
train   <- all_data %>% dplyr::filter(row_id <= n_train)
test    <- all_data %>% dplyr::filter(row_id > n_train, row_id <= n_train + n_test)
holdout <- all_data %>% dplyr::filter(row_id > n_train + n_test)
stopifnot(nrow(train) == n_train, nrow(test) == n_test, nrow(holdout) == nrow(enhanced$holdout))

holdout <- attach_holdout_actuals(holdout)
train_all <- bind_rows(train, test)

rounds <- optimal_nrounds(train_all, feats, target, params,
                          max_rounds = 2000, metric = "rmse_ipf", verbose = TRUE)$nrounds

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, params, rounds),
                 train_all, holdout, "baseline", seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(c(feats, new_cols), target, params, rounds),
                 train_all, holdout, "+ match-relative composites", seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = "baseline")

write_csv(results, "analysis/harness/match_relative_features_test.csv")
cat("\nwritten: analysis/harness/match_relative_features_test.csv\n")
