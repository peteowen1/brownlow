# Walk-forward simulation backtest --------------------------------------------
#
# The saved 2023/2024 simulations are not evaluable: the production model trains
# on 2015-2024, so those seasons are in-sample. Only 2025 was genuinely held out,
# leaving ONE honest evaluation point and one winner — not enough to say anything
# about calibration.
#
# This rebuilds them properly. For each season S: train on seasons < S, predict
# S, simulate, and score what the model said about the players who actually
# finished 1st through 10th.
#
# Scoring only the winner gives 9 points across 9 seasons. Scoring the top 20 —
# roughly the best player at each of the 18 clubs — gives 180. Weights of
# 1/position mean the winner still dominates at 1.0 while twentieth contributes
# 0.05, which matches how much each actually matters for betting.
#
# Feasible only because of the simulator rewrite: the original does a full-frame
# dplyr filter per match per simulation (2.778 s per season sim, 15.4 hours for
# this backtest). prepare_season_sim() hoists the split out and runs in 0.028 s.
#
# TWO LIMITATIONS, stated rather than buried.
#
# First, player_coef is excluded — see the note where feats is defined. It is a
# GAM random effect fitted once over 2015-2024, so it is constant for a player's
# entire career and cannot be given to a fold that should only see earlier
# seasons. The shipped model uses it legitimately; this backtest cannot.
#
# Second, this backtests XGBoost alone, not the
# XGBoost+GAM ensemble, because a GAM fit is ~1 hour and nine of them is not
# worth it here. The current ensemble is 82% XGBoost, so it is a close proxy but
# not the shipped configuration.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

N_SIMS  <- 5000
MAX_POS <- 20     # roughly the best player at each of the 18 clubs

# 5,000 rather than 2,000 because the tail now matters. At 2,000 sims the
# smallest resolvable probability is 1/2000 = 0.0005, and a player who genuinely
# finishes 20th about once in 3,000 seasons would read as an impossible zero.
# 5,000 halves that floor, and the rewritten simulator makes it affordable:
# 0.031 s per season simulation against the original 2.778 s.
FIRST   <- 2017   # needs enough prior seasons to train on

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
target   <- enhanced$target_col

# DROP player_coef. It comes from models/gam_production_model.rds, which is fitted
# once on 2015-2024 — so it takes exactly ONE value for a player's whole career
# (verified: constant for 100% of players, against 0% for every lagged feature).
# Handing that to a fold nominally trained on 2015-2016 tells it how players
# polled through 2024, including the season it is predicting. A first pass with it
# in had models trained on two seasons calling the winner at 55% and 87%.
#
# The shipped pipeline is NOT affected: there the GAM fits on 2015-2024 and the
# coefficient is applied to 2025 rows it never saw. Only reusing one fitted
# coefficient across walk-forward folds is illegitimate.
#
# Cost: the backtest is mildly pessimistic against the shipped model, which
# legitimately gains ~0.001 RMSE from this feature.
feats <- setdiff(enhanced$feature_cols, "player_coef")
cat(sprintf("features: %d (player_coef dropped - leaks future seasons into every fold)
", length(feats)))

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date)
stopifnot("player_coef must not reach the model" = !"player_coef" %in% feats)

register <- load_ineligible_players()

# ---- actual finishing order --------------------------------------------------
# Derived from the votes themselves rather than from memory. Ineligible players
# are removed BEFORE ranking, so they never occupy a position they could not
# hold — Heeney polled equal 4th in 2024 and could not win.
#
# 2025's votes are not in the modelling dataset (it was the prediction season),
# so they are read from the published per-match results and summed — derived from
# data like every other season rather than hardcoded from a news article.
actual_2025 <- readr::read_csv(
    "analysis/submission_comparison/detailed_predictions_vs_actual.csv",
    show_col_types = FALSE) %>%
  group_by(player_name = full_player_name) %>%
  summarise(votes = sum(Actual), .groups = "drop") %>%
  mutate(season = 2025) %>%
  select(season, player_name, votes)

stopifnot("2025 votes should total 6 per match across 207 matches" =
            sum(actual_2025$votes) == 1242)

finish_order <- all_data %>%
  dplyr::filter(season < 2025) %>%
  group_by(season, player_name) %>%
  summarise(votes = sum(brownlow_votes), .groups = "drop") %>%
  bind_rows(actual_2025) %>%
  anti_join(register %>% dplyr::filter(reason != "none-checked") %>%
              transmute(season, player_name),
            by = c("season", "player_name")) %>%
  group_by(season) %>%
  arrange(desc(votes), .by_group = TRUE) %>%
  slice_head(n = MAX_POS) %>%
  mutate(position = row_number()) %>%
  ungroup()

winners <- finish_order %>%
  dplyr::filter(position == 1) %>%
  transmute(season, winner = player_name, winning_votes = votes)

cat("=== actual winners, derived from vote data + eligibility register ===\n")
print(as.data.frame(winners %>% dplyr::filter(season >= FIRST)), row.names = FALSE)

# ---- walk forward ------------------------------------------------------------
seasons <- sort(unique(all_data$season))
seasons <- seasons[seasons >= FIRST]

cat("\n=== walking forward ===\n")
results <- lapply(seasons, function(S) {
  tr <- all_data %>% dplyr::filter(season < S)
  te <- all_data %>% dplyr::filter(season == S)

  rounds <- optimal_nrounds(tr, feats, target, tuning$best_params,
                            max_rounds = 1500, early_stopping_rounds = 50,
                            verbose = FALSE)$nrounds

  set.seed(1)
  m <- xgb.train(tuning$best_params,
                 xgb.DMatrix(as.matrix(tr[, feats]), label = tr[[target]]),
                 nrounds = rounds, verbose = 0)
  pm <- as_prob_matrix(predict(m, as.matrix(te[, feats])), nrow(te))

  te$sim_prob_0 <- pm[, 1]; te$sim_prob_1 <- pm[, 2]
  te$sim_prob_2 <- pm[, 3]; te$sim_prob_3 <- pm[, 4]

  prep   <- prepare_season_sim(te, prob_prefix = "sim_prob_")
  totals <- simulate_seasons_fast(prep, n_sims = N_SIMS, seed = S)

  wp <- win_probabilities_from_totals(totals)
  pp <- position_probabilities_from_totals(totals, max_pos = MAX_POS)

  if (any(register$season == S)) {
    wp <- tryCatch(apply_eligibility(wp, S, register), error = function(e) wp)
  }
  wp <- wp %>% arrange(desc(win_probability))

  w   <- winners %>% dplyr::filter(season == S)
  hit <- wp %>% dplyr::filter(player_name == w$winner)
  ord <- finish_order %>% dplyr::filter(season == S) %>% arrange(position) %>% pull(player_name)
  wll <- weighted_position_logloss(pp, ord)

  # Persist the raw output. Every metric so far has required a refit to compute,
  # which is why the conditional top-20 question could not be answered from the
  # previous run. Saving these makes any future metric a re-read.
  saveRDS(list(season = S, position_probs = pp, win_probs = wp,
               finish_order = ord, n_players = nrow(pp)),
          sprintf("analysis/harness/backtest_raw_%d.rds", S))

  cat(sprintf("  %d  train %d-%d (%d rounds) | winner %s %s | wLL %.3f\n",
              S, min(tr$season), max(tr$season), rounds, w$winner,
              if (nrow(hit)) sprintf("%.1f%% rank %d", hit$win_probability[1],
                                     which(wp$player_name == w$winner)[1]) else "MISSING",
              wll$weighted_logloss))

  tibble(season             = S,
         winner             = w$winner,
         winning_votes      = w$winning_votes,
         winner_prob        = if (nrow(hit)) hit$win_probability[1] else 0,
         winner_rank        = if (nrow(hit)) which(wp$player_name == w$winner)[1] else NA_integer_,
         winner_mean_votes  = if (nrow(hit)) hit$mean_votes[1] else NA_real_,
         favourite          = wp$player_name[1],
         favourite_prob     = wp$win_probability[1],
         favourite_won      = wp$player_name[1] == w$winner,
         n_with_any_prob    = sum(wp$win_probability > 0),
         weighted_logloss   = wll$weighted_logloss,
         mean_logloss_top10 = wll$mean_logloss,
         p_actual_1st       = wll$probabilities[["pos1"]],
         p_actual_2nd       = wll$probabilities[["pos2"]],
         p_actual_3rd       = wll$probabilities[["pos3"]])
})

out <- bind_rows(results)

cat("\n=== per season ===\n")
print(as.data.frame(out %>% select(season, winner, winner_prob, winner_rank,
                                   favourite, favourite_prob, favourite_won,
                                   weighted_logloss)),
      row.names = FALSE, digits = 4)

cat("\n=== calibration: does the favourite win as often as we claim? ===\n")
cat(sprintf("  seasons evaluated (all held out) : %d\n", nrow(out)))
cat(sprintf("  favourite actually won           : %d of %d (%.0f%%)\n",
            sum(out$favourite_won), nrow(out), 100 * mean(out$favourite_won)))
cat(sprintf("  mean probability we gave them    : %.1f%%\n", mean(out$favourite_prob)))
cat("   -> if calibrated these two agree; a gap is over- or under-confidence\n")

cat("\n=== weighted log-loss over the actual top 20, weights 1/position ===\n")
cat(sprintf("  evaluation points          : %d seasons x %d positions = %d\n",
            nrow(out), MAX_POS, nrow(out) * MAX_POS))
cat(sprintf("  weighted log-loss (mean)   : %.4f\n", mean(out$weighted_logloss)))
cat(sprintf("  unweighted top-10 log-loss : %.4f\n", mean(out$mean_logloss_top10)))
cat(sprintf("  mean P(actual 1st)         : %.3f\n", mean(out$p_actual_1st)))
cat(sprintf("  mean P(actual 2nd)         : %.3f\n", mean(out$p_actual_2nd)))
cat(sprintf("  mean P(actual 3rd)         : %.3f\n", mean(out$p_actual_3rd)))
cat(sprintf("  winner in our top 3        : %d of %d\n",
            sum(out$winner_rank <= 3, na.rm = TRUE), nrow(out)))
cat(sprintf("  winner's votes vs our mean : %+.1f average\n",
            mean(out$winning_votes - out$winner_mean_votes, na.rm = TRUE)))
cat("\n  Reference: spreading probability evenly over ~20 real contenders\n")
cat("  scores about -log(1/20) = 3.00. Lower is better.\n")

write_csv(out, "analysis/harness/simulation_backtest.csv")
cat("\nwritten: analysis/harness/simulation_backtest.csv\n")
