# Live vs datathon expected votes: Matt Rowell 2025 -----------------------------
#
# Pete's direct check, 2026-09-02: for a specific player, does the live
# (in-season) model's expected season total actually move toward the truth as
# real results come in, even though the AGGREGATE weekly-model backtest
# (§5/§5a) came out net negative?
#
#   datathon sum = sum over the season of the PRE-SEASON model's per-game
#                  expected votes — what actually shipped, no in-season info.
#                  For Rowell 2025 this is already verified: 26.20 (from the
#                  corrected submission file, POSTMORTEM §5i).
#
#   live sum      = sum over the season of, for EACH round, what the stage-2
#                  model predicts for THAT round using every prior round's
#                  REAL result. Not a single frozen cutoff (§5/§5a's design)
#                  — each row scored with its own true cumulative in-season
#                  state, which is exactly what add_in_season_vote_features()
#                  computes naturally (no freezing needed) and exactly the
#                  "oracle" arm from analysis/diagnose_freeze_artifact.R,
#                  read here as the object of interest rather than a control.
#
# Reuses the SAME honestly-tuned hyperparameters cached from §5a
# (models/stage2_honest_tuning.rds) rather than re-running the search.
# Shown for the top 15 predicted vote-getters, not just Rowell alone, so this
# is a fair look rather than one cherry-picked case.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- setdiff(enhanced$feature_cols, "player_coef")
target   <- enhanced$target_col

stage2_tune <- readRDS("models/stage2_honest_tuning.rds")
stage2_params <- stage2_tune$best_params
cat(sprintf("using cached honest-tuned params (mean fold mlogloss %.5f)\n", stage2_tune$best_score))

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(round_num = suppressWarnings(dplyr::if_else(match_round == "Opening Round", 0,
                                                      as.numeric(match_round))))

actual_2025 <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                        show_col_types = FALSE) %>%
  transmute(date = as.Date(match_date, format = "%d/%m/%Y"),
            team = player_team_std, player_name = full_player_name, actual_votes = Actual)

# ---- stage 1: walk-forward for every season, 2017-2025 in one loop ----------
# 2017-2024's fits exist to give STAGE-2's TRAINING rows an honest pred_votes:
# each row needs a prediction from a model that never saw that row's own
# season, or the "overperformance" feature it feeds is contaminated by a model
# that already memorised the answer. 2025's fit is just the ordinary "one
# model trained on everything before it" case — no different in kind, so it
# belongs in the same loop rather than a special-cased reuse of the shipped
# production model, which turned out to be incompatible anyway: that artefact
# has 258 features (it includes player_coef, the whole-career constant that
# leaks across season folds, per POSTMORTEM Sec 5g) against the 257 used here.
seasons_train <- 2017:2025
cat("=== stage 1: walk-forward, 2017-2025 ===\n")
season_data <- lapply(seasons_train, function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)
  rounds <- optimal_nrounds(tr, feats, target, tuning$best_params,
                            max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
  set.seed(1)
  m <- xgb.train(tuning$best_params, xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = rounds, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))
  te <- te %>% mutate(team_std = standardize_team_name(team), pred_votes = as.numeric(pm %*% c(0, 1, 2, 3)))
  cat(sprintf("  %d  (%d rounds)\n", S, rounds))
  te
}) %>% bind_rows()

season_data <- season_data %>%
  left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
            by = c("date", "team_std" = "team", "player_name")) %>%
  mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
  dplyr::filter(!is.na(actual))

season_data_is <- add_in_season_vote_features(season_data, pred_col = "pred_votes")
assert_in_season_clean(season_data_is)
inseason_cols <- grep("^inseason_", names(season_data_is), value = TRUE)
stage2_feats  <- c(feats, inseason_cols)

cat("\n=== training stage 2 on 2017-2024, honestly-tuned params ===\n")
train2 <- season_data_is %>% dplyr::filter(season < 2025)
rounds2 <- optimal_nrounds(train2, stage2_feats, target, stage2_params,
                           max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
set.seed(1)
m2 <- xgb.train(stage2_params, xgb.DMatrix(as.matrix(train2[, stage2_feats]), label = train2[[target]]),
                nrounds = rounds2, verbose = 0)
cat(sprintf("stage-2 rounds: %d\n", rounds2))

# ---- score every 2025 row with its OWN true prior-round state --------------
te25 <- season_data_is %>% dplyr::filter(season == 2025)
live_pm <- as_prob_matrix(predict(m2, as.matrix(te25[, stage2_feats])), nrow(te25))
te25$live_pred_votes <- as.numeric(live_pm %*% c(0, 1, 2, 3))

comparison <- te25 %>%
  group_by(player_name, team) %>%
  summarise(actual_total    = sum(actual),
           datathon_total  = sum(pred_votes),
           live_total      = sum(live_pred_votes),
           games           = n(), .groups = "drop") %>%
  arrange(desc(actual_total))

cat("\n=== top 15 by actual votes: datathon (pre-season) vs live (round-by-round) ===\n")
print(as.data.frame(comparison %>% head(15) %>%
        mutate(datathon_err = actual_total - datathon_total,
              live_err     = actual_total - live_total,
              improved     = abs(live_err) < abs(datathon_err))),
      row.names = FALSE, digits = 4)

cat("\n=== Matt Rowell specifically ===\n")
r <- comparison %>% dplyr::filter(player_name == "Matt Rowell")
cat(sprintf("  actual              : %.0f\n", r$actual_total))
cat(sprintf("  datathon prediction : %.2f  (error %+.2f)\n", r$datathon_total, r$actual_total - r$datathon_total))
cat(sprintf("  live prediction     : %.2f  (error %+.2f)\n", r$live_total, r$actual_total - r$live_total))

cat("\n=== overall: does live beat datathon on the top 15? (mean absolute error) ===\n")
top15 <- comparison %>% head(15)
cat(sprintf("  datathon MAE: %.3f\n", mean(abs(top15$actual_total - top15$datathon_total))))
cat(sprintf("  live MAE    : %.3f\n", mean(abs(top15$actual_total - top15$live_total))))
cat(sprintf("  improved in %d of 15\n", sum(abs(top15$actual_total - top15$live_total) <
                                            abs(top15$actual_total - top15$datathon_total))))

write_csv(comparison, "analysis/harness/rowell_live_vs_datathon.csv")
cat("\nwritten: analysis/harness/rowell_live_vs_datathon.csv\n")

# ---- round-by-round trajectory for Rowell: WHEN (if ever) did live react? ---
cat("\n=== Matt Rowell, round by round: datathon (fixed pre-season) vs live ===\n")
rowell_by_round <- te25 %>%
  dplyr::filter(player_name == "Matt Rowell") %>%
  arrange(round_num) %>%
  transmute(round = round_num, actual, datathon_pred = pred_votes, live_pred = live_pred_votes,
           inseason_games_so_far, inseason_actual_votes_so_far, inseason_pred_votes_so_far,
           inseason_overperformance)
print(as.data.frame(rowell_by_round), row.names = FALSE, digits = 4)
write_csv(rowell_by_round, "analysis/harness/rowell_round_by_round.csv")
cat("\nwritten: analysis/harness/rowell_round_by_round.csv\n")
