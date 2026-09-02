# Does impact-per-possession help? ---------------------------------------------
#
# The one hypothesis the 2026 research strengthened rather than weakened. See
# R/efficiency_features.R for why the gap is real and why explicit ratios can
# help a tree model.
#
# Prints the Rowell / low-disposal-high-impact diagnostic BEFORE the harness
# result, so the mechanism can be inspected whichever way the number lands. If
# these features do not separate the players Sec 3 says we miss, a null is
# unsurprising and the diagnostic says why — which is more useful than the
# delta alone.

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
  add_efficiency_features()

new_cols <- c("impact_per_disposal", "score_inv_per_disposal", "metres_per_disposal",
              "defensive_acts_per_disposal", "impact_per_disposal_vs_match",
              "impact_per_disposal_rank")
cat("\n=== distribution of the new features ===\n")
print(summary(all_data[, new_cols]))

# ---- does the ratio actually separate vote-getters on low volume? ------------
scored <- all_data %>% dplyr::filter(season < 2025)
low_vol <- scored %>% dplyr::filter(disposals <= quantile(disposals, 0.4, na.rm = TRUE))
cat(sprintf("\n=== low-disposal games only (bottom 40%%, n=%d) ===\n", nrow(low_vol)))
print(as.data.frame(low_vol %>%
  group_by(polled = .data[[target]] > 0) %>%
  summarise(n = n(),
            impact_per_disposal = round(mean(impact_per_disposal), 3),
            score_inv_per_disp  = round(mean(score_inv_per_disposal), 3),
            vs_match            = round(mean(impact_per_disposal_vs_match), 3),
            .groups = "drop")), row.names = FALSE)

n_train <- nrow(enhanced$train); n_test <- nrow(enhanced$test)
train   <- all_data %>% dplyr::filter(row_id <= n_train)
test    <- all_data %>% dplyr::filter(row_id > n_train, row_id <= n_train + n_test)
holdout <- all_data %>% dplyr::filter(row_id > n_train + n_test)
stopifnot(nrow(train) == n_train, nrow(test) == n_test, nrow(holdout) == nrow(enhanced$holdout))

holdout <- attach_holdout_actuals(holdout)
train_all <- bind_rows(train, test)

cat("\n=== rounds on the scored metric ===\n")
rounds <- optimal_nrounds(train_all, feats, target, params,
                          max_rounds = 2000, metric = "rmse_ipf", verbose = TRUE)$nrounds

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, params, rounds),
                 train_all, holdout, "baseline", seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(c(feats, new_cols), target, params, rounds),
                 train_all, holdout, "+ impact per possession", seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = "baseline")

write_csv(results, "analysis/harness/efficiency_features_test.csv")
cat("\nwritten: analysis/harness/efficiency_features_test.csv\n")
