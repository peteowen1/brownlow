# Is the in-season model's failure caused by FREEZING, or is the signal itself
# not usable? -------------------------------------------------------------------
#
# analysis/in_season_model_value.R found the stage-2 model significantly WORSE
# than the pre-season baseline (0 of 7 seasons improved, t=-4.02, p=0.007) —
# a stronger negative than the hand-tuned tilt it was meant to beat.
#
# Before concluding the underlying phenomenon isn't usable — which would
# contradict the persistence regression (real, t=4.78, survives a confound
# check) — check a specific implementation flaw: back-half rows had their
# in-season features FROZEN at their round-14 state for every remaining match.
# A round-20 fixture paired with round-14 in-season state is a covariate
# combination the model never saw during training, where every row's in-season
# features matched its own actual round. A tree model asked to score an
# out-of-distribution combination can look arbitrarily bad without that saying
# anything about whether the underlying signal is real.
#
# This isolates it with a pointwise RMSE check (cheap — no simulation needed)
# on ONE season (2023, the largest measured degradation), comparing the SAME
# trained stage-2 model against:
#
#   frozen   in-season features held at their round-14 value (what was tested)
#   natural  in-season features computed the honest way for every row — each
#            back-half row's own true cumulative history through its own prior
#            rounds. This is NOT usable for live betting (round 20's own
#            "actual votes so far" naturally includes rounds 15-19, which
#            haven't been counted yet in a single-decision-point design) — it
#            is an oracle upper bound purely to isolate the freezing artifact.
#
# If frozen is much worse than natural, freezing is the problem and a
# round-by-round (path-dependent) redesign is worth trying. If both are
# similarly bad, the smaller stage-2 training set or the extra features are
# the real problem, not the freezing mechanism.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

S       <- 2023
CUTOFF  <- 14

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- setdiff(enhanced$feature_cols, "player_coef")
target   <- enhanced$target_col

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(round_num = suppressWarnings(dplyr::if_else(match_round == "Opening Round", 0,
                                                      as.numeric(match_round))))

actual_2025 <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                        show_col_types = FALSE) %>%
  transmute(date = as.Date(match_date, format = "%d/%m/%Y"),
            team = player_team_std, player_name = full_player_name, actual_votes = Actual)

seasons_all <- sort(unique(all_data$season))
seasons_all <- seasons_all[seasons_all >= 2017]

cat("=== rebuilding stage-1 predictions (needed for in-season features) ===\n")
season_data <- lapply(seasons_all, function(Sx) {
  tr <- all_data %>% dplyr::filter(season < Sx)
  te <- all_data %>% dplyr::filter(season == Sx)
  rounds <- optimal_nrounds(tr, feats, target, tuning$best_params,
                            max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
  set.seed(1)
  m <- xgb.train(tuning$best_params, xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = rounds, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))
  te <- te %>%
    mutate(team_std = standardize_team_name(team), pred_votes = as.numeric(pm %*% c(0, 1, 2, 3))) %>%
    left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
              by = c("date", "team_std" = "team", "player_name")) %>%
    mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
    dplyr::filter(!is.na(actual))
  cat(sprintf("  %d\n", Sx))
  te
}) %>% bind_rows()

season_data_is <- add_in_season_vote_features(season_data, pred_col = "pred_votes")
inseason_cols  <- grep("^inseason_", names(season_data_is), value = TRUE)
stage2_feats   <- c(feats, inseason_cols)

cat(sprintf("\n=== training stage-2 on seasons < %d ===\n", S))
train2  <- season_data_is %>% dplyr::filter(season < S)
rounds2 <- optimal_nrounds(train2, stage2_feats, target, tuning$best_params,
                           max_rounds = 1500, early_stopping_rounds = 50, verbose = FALSE)$nrounds
set.seed(1)
m2 <- xgb.train(tuning$best_params, xgb.DMatrix(as.matrix(train2[, stage2_feats]), label = train2[[target]]),
                nrounds = rounds2, verbose = 0)
cat(sprintf("stage-2 rounds: %d\n", rounds2))

te   <- season_data_is %>% dplyr::filter(season == S)
back <- te %>% dplyr::filter(round_num > CUTOFF)
front <- te %>% dplyr::filter(round_num <= CUTOFF)

# --- frozen: round-14 state repeated for every remaining match ---------------
frozen <- front %>% group_by(player_name, team) %>%
  summarise(dplyr::across(dplyr::all_of(inseason_cols), ~dplyr::last(.x)), .groups = "drop")
back_frozen <- back %>% select(-dplyr::all_of(inseason_cols)) %>%
  left_join(frozen, by = c("player_name", "team")) %>%
  mutate(dplyr::across(dplyr::all_of(inseason_cols), ~tidyr::replace_na(.x, 0)))

# --- natural: each row's own true in-season state (oracle, not live-usable) --
back_natural <- back  # season_data_is already computed these naturally per row

ev <- function(pm) as.numeric(as_prob_matrix(pm, nrow(back)) %*% c(0, 1, 2, 3))
pred_frozen  <- ev(predict(m2, as.matrix(back_frozen[, stage2_feats])))
pred_natural <- ev(predict(m2, as.matrix(back_natural[, stage2_feats])))
pred_base    <- back$pred_votes  # stage-1, no in-season info at all

rmse <- function(p) sqrt(mean((p - back$actual)^2))
cat(sprintf("\n=== pointwise RMSE on %d back-half rows, season %d ===\n", nrow(back), S))
cat(sprintf("  baseline (no in-season info)      RMSE %.4f\n", rmse(pred_base)))
cat(sprintf("  stage-2, FROZEN at round %d        RMSE %.4f  <- what was tested\n", CUTOFF, rmse(pred_frozen)))
cat(sprintf("  stage-2, NATURAL per-row (oracle)  RMSE %.4f  <- isolates the freezing artifact\n", rmse(pred_natural)))

cat("\n=== does the frozen prediction's error grow with distance from the freeze point? ===\n")
back_frozen$sq_err <- (pred_frozen - back$actual)^2
back_frozen$rounds_since_freeze <- back_frozen$round_num - CUTOFF
by_dist <- back_frozen %>% group_by(rounds_since_freeze) %>%
  summarise(mean_sq_err = mean(sq_err), n = n(), .groups = "drop") %>% arrange(rounds_since_freeze)
print(as.data.frame(by_dist), row.names = FALSE, digits = 4)
cat("\n  If error rises with distance from the freeze point, that confirms staleness\n")
cat("  (covariate shift), not a fundamentally bad signal.\n")
