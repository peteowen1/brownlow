# How much did the calculate_career_performance_summary() leak move the numbers? -
#
# Eight features computed as a whole-career, un-lagged mean/max/sum/sd per
# player, joined onto every row for that player including their debut game
# (verified non-zero on 85-100% of debuts, where a correctly lagged feature
# must be exactly zero — no prior game exists). All eight are currently in
# feature_cols; the bug predates this session.
#
#   career_total_games, career_avg_tackles, career_avg_marks,
#   career_max_disposals, career_max_goals, career_games_with_goals,
#   career_games_high_disposals, career_consistency_disposals
#
# This is a straight in/out comparison on the 2025 holdout, 5 seeds, IPF, at the
# already-tuned round count — not a full walk-forward rebuild. That answers
# "how much did this move the number we've been quoting" quickly; a proper fix
# requires rebuilding calculate_career_performance_summary() to lag correctly
# and regenerating every downstream artefact, which is a separate, larger job.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

LEAKY <- c("career_total_games", "career_avg_tackles", "career_avg_marks",
          "career_max_disposals", "career_max_goals", "career_games_with_goals",
          "career_games_high_disposals", "career_consistency_disposals")

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- enhanced$target_col
stopifnot("expected all 8 leaky columns present" = all(LEAKY %in% enhanced$feature_cols))

feats_with_leak    <- enhanced$feature_cols
feats_without_leak <- setdiff(enhanced$feature_cols, LEAKY)
cat(sprintf("features: %d with the leaky 8, %d without\n\n",
            length(feats_with_leak), length(feats_without_leak)))

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  mutate(player_coef = replace_na(player_coef, 0))
train   <- all_data %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(all_data %>%
                                    dplyr::filter(season %in% enhanced$holdout_seasons))

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats_with_leak, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, "with 8 leaky career-summary features",
                 seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(feats_without_leak, target, tuning$best_params, tuning$best_iteration),
                 train, holdout, "without them",
                 seeds = 1:5, normalizer = "ipf")
)

compare_configs(results, baseline_label = "with 8 leaky career-summary features")
write_csv(results, "analysis/harness/career_summary_leak_size.csv")
cat("\nwritten: analysis/harness/career_summary_leak_size.csv\n")
