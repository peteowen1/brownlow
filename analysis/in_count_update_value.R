# How much does the in-count momentum update improve weighted top-20 log-loss? -
#
# analysis/overperformance_persistence.R confirmed the SIGNAL exists (pooled
# t = 4.78, survives a confound check for static per-player bias). This measures
# what it is actually WORTH in the sim's own metric.
#
# Design: at a chosen cutoff round r, front-half votes are already known in a
# real count — this is not a prediction, it is a fact. So both arms of the
# comparison use the IDENTICAL real front-half actual votes; only the back-half
# simulation differs. That isolates exactly the marginal value of the momentum
# update, holding everything else fixed.
#
#   baseline arm: season total = front-half ACTUAL + back-half simulated from
#                 the model's raw (unadjusted) probabilities
#   adjusted arm: season total = front-half ACTUAL + back-half simulated from
#                 probabilities TILTED toward each player's observed
#                 overperformance, scaled by the persistence slope measured at
#                 this cutoff
#
# The weight applied to raw overperformance is the UNADJUSTED persistence slope
# (analysis/harness/overperformance_persistence.csv), not the bias-adjusted one.
# In a real count you only observe raw overperformance; the bias-adjusted
# regression was a diagnostic to rule out a confound, not an operational weight.
#
# CUTOFF = 14 is chosen because it is where the persistence slope stabilises
# (see the per-cutoff table) and it represents a realistic in-play moment: most
# of the season's evidence has accumulated but a meaningful stretch remains to
# bet on. Not swept across cutoffs here to keep the run tractable; the shape of
# the answer should be similar at 12/16/18 given the slope table.

library(dplyr)
library(readr)
library(tidyr)
library(xgboost)
devtools::load_all()

CUTOFF   <- 14
N_SIMS   <- 5000
FIRST    <- 2017
MAX_POS  <- 20

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- setdiff(enhanced$feature_cols, "player_coef")
target   <- enhanced$target_col

slopes <- read_csv("analysis/harness/overperformance_persistence.csv", show_col_types = FALSE)
WEIGHT <- slopes %>% dplyr::filter(cutoff == CUTOFF) %>% pull(slope) %>%
  { stopifnot("no cached slope for this cutoff" = length(.) > 0); mean(.) }
cat(sprintf("cutoff %d -> pooled-season measured slope, using mean across seasons at this cutoff\n", CUTOFF))
cat(sprintf("update weight = %.4f\n\n", WEIGHT))

all_data <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout) %>%
  arrange(season, date) %>%
  mutate(round_num = suppressWarnings(dplyr::if_else(match_round == "Opening Round", 0,
                                                      as.numeric(match_round))))
stopifnot("round_num must parse for every row" = !anyNA(all_data$round_num))

actual_2025 <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                        show_col_types = FALSE) %>%
  transmute(date = as.Date(match_date, format = "%d/%m/%Y"),
            team = player_team_std, player_name = full_player_name, actual_votes = Actual)

seasons <- sort(unique(all_data$season))
seasons <- seasons[seasons >= FIRST]

cat("=== walking forward: model + full probability matrix per season ===\n")
season_data <- lapply(seasons, function(S) {
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

  te <- te %>%
    mutate(team_std = standardize_team_name(team),
           raw_prob_0 = pm[, 1], raw_prob_1 = pm[, 2],
           raw_prob_2 = pm[, 3], raw_prob_3 = pm[, 4])

  te <- te %>%
    left_join(actual_2025 %>% mutate(team = standardize_team_name(team)),
              by = c("date", "team_std" = "team", "player_name")) %>%
    mutate(actual = if_else(season == 2025, actual_votes, brownlow_votes)) %>%
    dplyr::filter(!is.na(actual))

  cat(sprintf("  %d  (%d rounds, %d rows)\n", S, rounds, nrow(te)))
  te
})
names(season_data) <- as.character(seasons)

# ---- per-season comparison ---------------------------------------------------
score_season <- function(te, S) {
  max_r <- max(te$round_num)
  if (max_r < CUTOFF + 3) {
    cat(sprintf("  %d: skipped, season too short (max round %d)\n", S, max_r))
    return(NULL)
  }

  front <- te %>% dplyr::filter(round_num <= CUTOFF)
  back  <- te %>% dplyr::filter(round_num > CUTOFF)

  front_actual <- front %>% group_by(player_name, team) %>%
    summarise(front_actual = sum(actual),
             front_pred   = sum(raw_prob_1 + 2*raw_prob_2 + 3*raw_prob_3),
             .groups = "drop") %>%
    mutate(over_front = front_actual - front_pred)

  back_games <- back %>% count(player_name, team, name = "back_games")

  boost <- front_actual %>%
    inner_join(back_games, by = c("player_name", "team")) %>%
    mutate(target_shift_per_game = pmax(-1.5, pmin(1.5, WEIGHT * over_front / back_games)))

  back_ev <- as.numeric(as.matrix(back[, c("raw_prob_1","raw_prob_2","raw_prob_3")]) %*% c(1,2,3) +
                        back$raw_prob_0 * 0)
  back <- back %>% mutate(raw_ev = back_ev) %>%
    left_join(boost %>% select(player_name, team, target_shift_per_game),
              by = c("player_name", "team")) %>%
    mutate(target_shift_per_game = tidyr::replace_na(target_shift_per_game, 0),
           target_ev = pmax(0.001, pmin(2.999, raw_ev + target_shift_per_game)))

  raw_pm <- as.matrix(back[, c("raw_prob_0","raw_prob_1","raw_prob_2","raw_prob_3")])
  adj_pm <- tilt_to_expected_value(raw_pm, back$target_ev)

  back_raw <- back %>% mutate(sim_prob_0 = raw_pm[,1], sim_prob_1 = raw_pm[,2],
                              sim_prob_2 = raw_pm[,3], sim_prob_3 = raw_pm[,4])
  back_adj <- back %>% mutate(sim_prob_0 = adj_pm[,1], sim_prob_1 = adj_pm[,2],
                              sim_prob_2 = adj_pm[,3], sim_prob_3 = adj_pm[,4])

  # Season total = real known front-half votes (same in both arms) + simulated
  # back-half votes. Players can appear in only one half (debuted late, or
  # missed the rest of the season through injury), so this is a union of keys
  # with each side defaulting to 0 rather than an inner join.
  front_by_key <- setNames(front_actual$front_actual,
                           paste(front_actual$player_name, front_actual$team, sep = "_"))

  totals_from <- function(back_frame) {
    prep <- prepare_season_sim(back_frame, prob_prefix = "sim_prob_")
    sim  <- simulate_seasons_fast(prep, n_sims = N_SIMS, seed = S)

    all_keys <- union(rownames(sim), names(front_by_key))
    front_vec <- front_by_key[all_keys]
    front_vec[is.na(front_vec)] <- 0

    # `sim[all_keys, ]` errors on any name not already in sim (R does not pad
    # with NA rows for character indexing), so index by position via match().
    idx <- match(all_keys, rownames(sim))
    sim_full <- sim[idx, , drop = FALSE]
    sim_full[is.na(sim_full)] <- 0  # keys present only in front_by_key
    rownames(sim_full) <- all_keys

    sim_full + front_vec  # recycles front_vec down each column, i.e. every sim
  }

  raw_dir <- readRDS(sprintf("analysis/harness/backtest_raw_%d.rds", S))
  ord <- raw_dir$finish_order

  eval_arm <- function(totals, label) {
    pp  <- position_probabilities_from_totals(totals, max_pos = MAX_POS)
    wll <- weighted_position_logloss(pp, ord)
    cat(sprintf("    %-9s weighted LL %.4f\n", label, wll$weighted_logloss))
    wll$weighted_logloss
  }

  cat(sprintf("  %d  (front rounds<=%d actual, back rounds>%d simulated x%d)\n", S, CUTOFF, CUTOFF, N_SIMS))
  base_ll <- eval_arm(totals_from(back_raw), "baseline")
  adj_ll  <- eval_arm(totals_from(back_adj), "adjusted")

  tibble(season = S, baseline_wll = base_ll, adjusted_wll = adj_ll,
         improvement = base_ll - adj_ll,
         mean_target_shift = mean(boost$target_shift_per_game),
         n_players_boosted = sum(abs(boost$target_shift_per_game) > 0.01))
}

results <- lapply(seasons, function(S) score_season(season_data[[as.character(S)]], S)) %>% bind_rows()

cat("\n=== per season ===\n")
print(as.data.frame(results), row.names = FALSE, digits = 4)

cat("\n=== summary ===\n")
cat(sprintf("  seasons scored              : %d\n", nrow(results)))
cat(sprintf("  mean weighted LL, baseline  : %.4f\n", mean(results$baseline_wll)))
cat(sprintf("  mean weighted LL, adjusted  : %.4f\n", mean(results$adjusted_wll)))
cat(sprintf("  mean improvement            : %+.4f\n", mean(results$improvement)))
cat(sprintf("  improved in                 : %d of %d seasons\n", sum(results$improvement > 0), nrow(results)))
cat(sprintf("  paired t-test on improvement: t = %.2f, p = %.3f\n",
            t.test(results$improvement)$statistic, t.test(results$improvement)$p.value))

write_csv(results, "analysis/harness/in_count_update_value.csv")
cat("\nwritten: analysis/harness/in_count_update_value.csv\n")
