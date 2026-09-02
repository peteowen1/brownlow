# Season totals: the metric that matters for betting --------------------------
#
# The datathon scores RMSE over 9,522 player-matches, 8,901 of which are players
# who polled nothing. The eventual Brownlow winner is about 22 of those rows, so
# even a large improvement on him is invisible in the aggregate: fixing a
# 13-vote season underestimate entirely moves total RMSE by well under the
# 0.0008 seed-noise floor.
#
# Betting lives somewhere else. What matters is whether the predicted SEASON
# TOTAL for the contenders is right, and whether the winner comes out on top.
# Season-to-date form features were rejected on RMSE; this asks whether that
# rejection also holds on the objective they were actually designed for.
#
# 2025 truth: Matt Rowell won with 39. We predicted 25.9.

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
  arrange(season, date) %>%
  mutate(row_key = seq_len(n()))

# ---- stage 1 + season-to-date features (same construction as the RMSE test) --
stage1 <- optimal_nrounds(all_data %>% dplyr::filter(!season %in% enhanced$holdout_seasons),
                          feats, target, tuning$best_params,
                          max_rounds = 1500, early_stopping_rounds = 50)

pred <- lapply(sort(unique(all_data$season)), function(S) {
  te <- all_data %>% dplyr::filter(season == S)
  tr <- all_data %>% dplyr::filter(season < S)
  if (nrow(tr) == 0) return(tibble(row_key = te$row_key, pred_votes = 0))
  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = stage1$nrounds, verbose = 0)
  tibble(row_key = te$row_key,
         pred_votes = as.numeric(as_prob_matrix(predict(m, as.matrix(te[, feats])),
                                                nrow(te)) %*% c(0, 1, 2, 3)))
}) %>% bind_rows()

OBSERVED <- intersect(c("coaches_votes", "supercoach_score", "afl_fantasy_score",
                        "rating_points", "disposals", "contested_possessions",
                        "goals", "score_involvements", "impact_score"), names(all_data))

aug <- all_data %>%
  left_join(pred, by = "row_key") %>%
  add_season_to_date_features(metrics = c(OBSERVED, "pred_votes"), prefix = "szn",
                              rank_metrics = c("coaches_votes", "supercoach_score", "pred_votes"))
assert_season_to_date_clean(aug, prefix = "szn")

szn_cols <- c(grep("^szn_", names(aug), value = TRUE))

train   <- aug %>% dplyr::filter(!season %in% enhanced$holdout_seasons)
holdout <- attach_holdout_actuals(aug %>% dplyr::filter(season %in% enhanced$holdout_seasons))

# ---- season totals per player ----------------------------------------------
actual_totals <- holdout %>%
  group_by(player_name, team) %>%
  summarise(actual_total = sum(Actual), .groups = "drop") %>%
  arrange(desc(actual_total))

evaluate_totals <- function(f, label) {
  cat(sprintf("  %s\n", label))
  preds <- lapply(seq_len(N_SEEDS), function(s) {
    set.seed(s)
    m <- xgb.train(tuning$best_params,
                   xgb.DMatrix(as.matrix(train[, f]), label = train[[target]]),
                   nrounds = tuning$best_iteration, verbose = 0)
    pm <- as_prob_matrix(predict(m, as.matrix(holdout[, f])), nrow(holdout))
    normalize_votes_ipf(pm, holdout$match_id)
  })
  votes <- Reduce(`+`, preds) / N_SEEDS

  totals <- holdout %>%
    mutate(pred = votes) %>%
    group_by(player_name, team) %>%
    summarise(pred_total = sum(pred), .groups = "drop") %>%
    left_join(actual_totals, by = c("player_name", "team")) %>%
    mutate(actual_total = replace_na(actual_total, 0))

  top20 <- totals %>% arrange(desc(actual_total)) %>% head(20)
  winner <- totals %>% dplyr::filter(player_name == "Matt Rowell")
  pred_leader <- totals %>% arrange(desc(pred_total)) %>% slice(1)

  tibble(
    label            = label,
    mae_top20        = mean(abs(top20$pred_total - top20$actual_total)),
    winner_predicted = winner$pred_total,
    winner_shortfall = winner$actual_total - winner$pred_total,
    winner_rank      = which(order(-totals$pred_total) == which(totals$player_name == "Matt Rowell")),
    predicted_leader = pred_leader$player_name,
    spearman_top50   = cor(head(arrange(totals, desc(actual_total)), 50)$pred_total,
                           head(arrange(totals, desc(actual_total)), 50)$actual_total,
                           method = "spearman")
  )
}

cat("evaluating season totals\n")
results <- bind_rows(
  evaluate_totals(feats,                "baseline"),
  evaluate_totals(c(feats, szn_cols),   "+ season-to-date form")
)

cat("\n=== season-total accuracy on 2025 (Rowell won with 39) ===\n")
print(as.data.frame(results), digits = 4, row.names = FALSE)

write_csv(results, "analysis/harness/season_totals.csv")
cat("\nwritten: analysis/harness/season_totals.csv\n")
