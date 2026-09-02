# Ensemble weight selected on walk-forward predictions -------------------------
#
# POSTMORTEM Sec 5j: optimize_ensemble_weights() picks the blend on 2023-24
# probabilities that stage 05 produced from a model trained on 2015-2024 --
# i.e. on rows the booster memorised. XGBoost scores 0.3357 there but 0.3577 out
# of sample; the GAM barely moves. So selection is monotone toward pure XGBoost
# and only the 0.9 cap prevents it.
#
# This regenerates BOTH components' 2023 and 2024 predictions walk-forward
# (season S predicted by models trained only on seasons < S), selects the weight
# on those, and then applies it once to 2025. Both arms must be out-of-sample:
# making only XGBoost honest would flip the bias toward the GAM instead.
#
# KNOWN LIMITATION, stated rather than hidden: the walk-forward XGBoost excludes
# player_coef, because that feature is a GAM random effect fitted across all
# seasons and leaks backwards into any earlier fold (Sec 5g). Production
# XGBoost includes it. So the selection-stage booster is slightly weaker than
# the one that ships, which biases the chosen weight mildly TOWARD the GAM. The
# alternative -- keeping a feature we know leaks -- would bias it toward
# XGBoost by an unknown and probably larger amount.

library(dplyr)
library(readr)
library(mgcv)
library(xgboost)
devtools::load_all()

SEASONS <- c(2023, 2024)

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
prepared <- readRDS("data/processed/modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")

xgb_feats <- setdiff(enhanced$feature_cols, "player_coef")
target    <- enhanced$target_col

enh_all <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout)
prep_all <- bind_rows(prepared$train, prepared$test, prepared$holdout)

# Same list 03_gam_model.R passes to validate_gam_features().
GAM_MODEL_VARS <- c('supercoach_score', 'afl_fantasy_score', 'impact_score', 'disposals',
                    'metres_gained', 'player_career_games', 'match_margin', 'rating_points',
                    'marks', 'kicks', 'handballs', 'tackles', 'inside_fifties',
                    'score_involvement', 'coaches_votes')

gam_formula <- as.formula(paste0("votes ~ ",
  "s(coaches_votes, k=10, bs='ts') +",
  "s(supercoach_score, k=10, bs='ts') +",
  "s(afl_fantasy_score, k=10, bs='ts') +",
  "s(impact_score, k=10, bs='ts') +",
  "s(player_impact_share, k=10, bs='ts') +",
  "s(player_disposal_share, k=10, bs='ts') +",
  "s(rating_points, k=10, bs='ts') +",
  "s(career_avg_brownlow_votes_prior_seasons, k=10, bs='ts') +",
  "s(career_avg_coaches_votes, k=10, bs='ts') +",
  "s(career_avg_rating_points, k=10, bs='ts') +",
  "s(career_avg_supercoach_score, k=10, bs='ts') +",
  "s(career_games_with_goals, k=10, bs='ts') +",
  "s(recent_avg_impact, k=10, bs='ts') +",
  "s(score_involvement, k=10, bs='ts') +",
  "s(disposals, k=10, bs='ts') +",
  "s(goals, k=4, bs='ts') +",
  "s(player_goal_share, k=4, bs='ts') +",
  "s(metres_gained, k=8, bs='ts') +",
  "s(tackles, k=8, bs='ts') +",
  "s(marks, k=6, bs='ts') +",
  "s(kicks, k=6, bs='ts') +",
  "s(handballs, k=6, bs='ts') +",
  "s(inside_fifties, k=5, bs='ts') +",
  "s(total_score, k=10, bs='ts') +",
  "s(match_margin, k=10, bs='ts') +",
  "s(team_out_scored_opposition, bs='re') +",
  "s(team, bs='re') +",
  "s(player_name, bs='re')"
))

walk_forward_season <- function(S) {
  cat(sprintf("\n===== season %d =====\n", S))

  # ---- XGBoost, trained on < S -----------------------------------------------
  tr <- enh_all %>% dplyr::filter(season < S)
  te <- enh_all %>% dplyr::filter(season == S)
  rounds <- optimal_nrounds(tr, xgb_feats, target, tuning$best_params,
                            max_rounds = 600, early_stopping_rounds = 40, verbose = TRUE)$nrounds
  set.seed(1)
  m <- xgboost::xgb.train(tuning$best_params,
                          xgboost::xgb.DMatrix(as.matrix(tr[, xgb_feats]), label = tr[[target]]),
                          nrounds = rounds, verbose = 0)
  Px <- as_prob_matrix(predict(m, as.matrix(te[, xgb_feats])), nrow(te))
  cat(sprintf("  xgboost: trained on %d rows (<%d), %d rounds\n", nrow(tr), S, rounds))

  xgb_out <- te %>%
    dplyr::transmute(player_name, team, date, match_id, season, actual = .data[[target]],
                     x0 = Px[,1], x1 = Px[,2], x2 = Px[,3], x3 = Px[,4])

  # ---- GAM, trained on < S ---------------------------------------------------
  gtr_raw <- prep_all %>% dplyr::filter(season < S)
  gte_raw <- prep_all %>% dplyr::filter(season == S)

  gtr <- prepare_gam_training_data(all_training_data = gtr_raw,
                                   target_col = prepared$target_col,
                                   feature_cols = prepared$feature_cols,
                                   prepared_data = prepared)
  gte <- prepare_gam_test_data(test_data = gte_raw, gam_data_train = gtr,
                               target_col = prepared$target_col,
                               feature_cols = prepared$feature_cols,
                               prepared_data = prepared)
  gm <- train_gam_with_validation(gam_data_train = gtr, feature_formula = gam_formula, verbose = FALSE)
  if (is.null(gm)) stop(sprintf("GAM failed for season %d", S))

  gte_clean <- validate_gam_features(gam_data_test = gte, model_vars = GAM_MODEL_VARS, verbose = FALSE)
  Pg <- predict(gm, newdata = gte_clean, type = "response")
  stopifnot(ncol(Pg) == 4)
  cat(sprintf("  gam    : trained on %d rows (<%d), scored %d of %d rows\n",
              nrow(gtr), S, nrow(gte_clean), nrow(gte)))

  gam_out <- gte_clean %>%
    dplyr::transmute(player_name = as.character(player_name), team = as.character(team),
                     match_id, g0 = Pg[,1], g1 = Pg[,2], g2 = Pg[,3], g3 = Pg[,4])

  xgb_out %>%
    dplyr::mutate(player_name = as.character(player_name), team = as.character(team)) %>%
    dplyr::inner_join(gam_out, by = c("player_name", "team", "match_id"))
}

wf <- lapply(SEASONS, walk_forward_season) %>% bind_rows()
cat(sprintf("\nwalk-forward selection rows: %d across %d matches\n",
            nrow(wf), dplyr::n_distinct(wf$match_id)))
stopifnot(nrow(wf) > 0)

Xs <- as.matrix(wf[, c("x0","x1","x2","x3")])
Gs <- as.matrix(wf[, c("g0","g1","g2","g3")])

rmse_at <- function(w, X, G, ids, actual) {
  sqrt(mean((normalize_votes_ipf(w * X + (1 - w) * G, ids) - actual)^2))
}

grid <- seq(0, 1, by = 0.05)
sel <- vapply(grid, function(w) rmse_at(w, Xs, Gs, wf$match_id, wf$actual), numeric(1))
sel_tbl <- tibble(w_xgb = grid, walkforward_rmse = sel)
cat("\n=== weight curve on WALK-FORWARD 2023-24 predictions ===\n")
print(as.data.frame(sel_tbl), row.names = FALSE, digits = 6)
w_star <- grid[which.min(sel)]
cat(sprintf("\n  walk-forward-selected w_xgb = %.2f\n", w_star))

# ---- apply once to 2025 -------------------------------------------------------
xgbp <- readRDS("results/xgboost_probabilities.rds")
gamp <- readRDS("results/gam_probabilities.rds")
hold <- attach_holdout_actuals(enhanced$holdout) %>%
  dplyr::select(player_name, team, date, match_id, Actual)

h <- xgbp %>%
  dplyr::inner_join(gamp %>% dplyr::select(player_name, team, date, match_id,
                                           dplyr::starts_with("gam_prob_")),
                    by = c("player_name","team","date","match_id")) %>%
  dplyr::filter(season == 2025) %>%
  dplyr::inner_join(hold, by = c("player_name","team","date","match_id"))
stopifnot(nrow(h) == nrow(enhanced$holdout))

Xh <- as.matrix(h[, paste0("xgboost_prob_", 0:3)])
Gh <- as.matrix(h[, paste0("gam_prob_", 0:3)])

cat("\n=== 2025 holdout RMSE ===\n")
for (lbl_w in list(c("walk-forward selected", w_star), c("ships today", 0.858),
                   c("in-sample selected", 1.00))) {
  w <- as.numeric(lbl_w[2])
  cat(sprintf("  %-22s w=%.2f : %.5f\n", lbl_w[1], w, rmse_at(w, Xh, Gh, h$match_id, h$Actual)))
}

write_csv(sel_tbl, "analysis/harness/ensemble_weight_walkforward.csv")
cat("\nwritten: analysis/harness/ensemble_weight_walkforward.csv\n")
