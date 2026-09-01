# Does overperformance during the count persist? -------------------------------
#
# Pete's idea, 2026-09-01. During the Brownlow count rounds are read out in order.
# After round r you know every player's ACTUAL votes for rounds 1..r. Compare
# that to what the model predicted for those same rounds: a player running hot is
# revealing something the pre-count model did not know. Use it to update the
# forecast for the rounds not yet read, and re-simulate.
#
# This cannot help the datathon submission at all — that is locked before a
# single vote is read. It is purely for betting in-play.
#
# THE CRUX, and the only thing this script measures: does overperformance
# persist? If a player over-polls relative to the model across rounds 1..r, is he
# more likely to over-poll across the rest? If it is noise, the correct update
# weight is zero and the idea dies for the cost of one regression.
#
#   over_front = actual(1..r)   - predicted(1..r)
#   resid_back = actual(r+1..R) - predicted(r+1..R)
#   slope of resid_back ~ over_front IS the update weight
#
# A slope near zero means overperformance is noise. A positive slope means it
# carries, and its size says how much to move the remaining-round forecast.
#
# Predictions are walk-forward — train on seasons < S, predict S — so no fold
# ever sees its own target. player_coef is excluded: it is a GAM random effect
# fitted once over 2015-2024, constant for a player's whole career, and would
# leak later seasons into every earlier fold.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- setdiff(enhanced$feature_cols, "player_coef")
target   <- enhanced$target_col

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(round_idx = as.integer(factor(paste(season, date))))

# Round index within season, so "after round r" is well defined even with the
# Opening Round label and rescheduled fixtures.
all_data <- all_data %>%
  group_by(season) %>%
  mutate(round_idx = dense_rank(date)) %>%
  ungroup()

# 2025 has no votes in the modelling data; attach the published ones.
actual_2025 <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                        show_col_types = FALSE) %>%
  transmute(date = as.Date(match_date, format = "%d/%m/%Y"),
            team = player_team_std, player_name = full_player_name, actual_votes = Actual)

cat("=== walk-forward predictions ===\n")
seasons <- sort(unique(all_data$season))
preds <- lapply(seasons, function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  if (nrow(tr) == 0) return(NULL)
  rounds <- optimal_nrounds(tr, feats, target, tuning$best_params,
                            max_rounds = 1500, early_stopping_rounds = 50,
                            verbose = FALSE)$nrounds
  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = rounds, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))
  cat(sprintf("  %d\n", S))
  te %>% mutate(pred_votes = normalize_votes_ipf(pm, te$match_id))
}) %>% bind_rows()

# Attach the truth
preds <- preds %>%
  mutate(team_std = standardize_team_name(team)) %>%
  left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
            by = c("date", "team_std" = "team", "player_name")) %>%
  mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
  dplyr::filter(!is.na(actual))

cat(sprintf("\n%d player-matches across %d seasons\n\n", nrow(preds), n_distinct(preds$season)))

# ---- the test ---------------------------------------------------------------
# For each season and each cutoff, split the season and regress the back-half
# residual on the front-half overperformance. Restricted to players with a real
# presence in the season, because the question is about contenders.
cutoffs <- c(4, 6, 8, 10, 12, 14, 16, 18)

results <- lapply(seasons, function(S) {
  d <- preds %>% dplyr::filter(season == S)
  if (nrow(d) == 0) return(NULL)
  max_r <- max(d$round_idx)

  lapply(cutoffs[cutoffs < max_r - 2], function(r) {
    front <- d %>% dplyr::filter(round_idx <= r) %>%
      group_by(player_name) %>%
      summarise(obs = sum(actual), pred = sum(pred_votes), games = n(), .groups = "drop") %>%
      mutate(over_front = obs - pred)
    back <- d %>% dplyr::filter(round_idx > r) %>%
      group_by(player_name) %>%
      summarise(obs_b = sum(actual), pred_b = sum(pred_votes), games_b = n(), .groups = "drop") %>%
      mutate(resid_back = obs_b - pred_b)

    j <- inner_join(front, back, by = "player_name") %>%
      dplyr::filter(games >= 3, games_b >= 3)
    if (nrow(j) < 50) return(NULL)

    fit <- lm(resid_back ~ over_front, data = j)
    tibble(season = S, cutoff = r, n = nrow(j),
           slope = coef(fit)[2],
           se    = summary(fit)$coefficients[2, 2],
           t     = summary(fit)$coefficients[2, 3],
           r2    = summary(fit)$r.squared)
  }) %>% bind_rows()
}) %>% bind_rows()

cat("=== slope of back-half residual on front-half overperformance ===\n")
cat("  slope 0  => overperformance is noise, do not update\n")
cat("  slope >0 => it persists; the slope is the update weight\n\n")
by_cut <- results %>% group_by(cutoff) %>%
  summarise(n_seasons = n(), mean_slope = mean(slope),
            se_of_mean = sd(slope)/sqrt(n()),
            t_of_mean = mean(slope)/(sd(slope)/sqrt(n())), .groups = "drop")
print(as.data.frame(by_cut), row.names = FALSE, digits = 3)

cat("\n=== pooled across all seasons and cutoffs ===\n")
cat(sprintf("  observations : %d season-cutoff pairs\n", nrow(results)))
cat(sprintf("  mean slope   : %+.4f\n", mean(results$slope)))
cat(sprintf("  se           : %.4f\n", sd(results$slope)/sqrt(nrow(results))))
cat(sprintf("  t            : %+.2f\n", mean(results$slope)/(sd(results$slope)/sqrt(nrow(results)))))
cat(sprintf("  mean R-sq    : %.4f\n", mean(results$r2)))
cat("\n  |t| under about 2 means the update weight is indistinguishable from zero\n")
cat("  and in-count momentum updating should not be built.\n")

write_csv(results, "analysis/harness/overperformance_persistence.csv")
cat("\nwritten: analysis/harness/overperformance_persistence.csv\n")
