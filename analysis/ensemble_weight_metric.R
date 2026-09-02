# Selection metric mismatch in the ensemble weight ------------------------------
#
# analysis/ensemble_weight_grid.R found the two metrics disagree sharply on
# 2023-2024 selection / 2025 evaluation:
#
#   log-loss on raw component probabilities  -> w_xgb = 1.00 (drop the GAM)
#   RMSE after IPF on the 2025 holdout       -> minimised near w_xgb = 0.55
#
# optimize_ensemble_weights() optimises log-loss, so the pipeline is tuned on a
# metric that disagrees with the one the datathon scores. But the 0.55 above was
# read off the holdout and is therefore not claimable.
#
# This runs the honest version: choose the weight on 2023-2024 using RMSE after
# IPF -- the same objective we are scored on -- and then evaluate that choice,
# once, on 2025. If the RMSE-selected weight lands near the holdout optimum, the
# gain is real and capturable. If it lands somewhere else, the 0.55 was holdout
# noise and the current setup is fine.

library(dplyr)
library(readr)
devtools::load_all()

xgb <- readRDS("results/xgboost_probabilities.rds")
gam <- readRDS("results/gam_probabilities.rds")

both <- xgb %>%
  dplyr::inner_join(gam %>% dplyr::select(player_name, team, date, match_id,
                                          dplyr::starts_with("gam_prob_")),
                    by = c("player_name", "team", "date", "match_id"))
stopifnot(nrow(both) == nrow(xgb))

mlogloss <- function(P, y) -mean(log(pmax(P[cbind(seq_along(y), y + 1L)], 1e-15)))

# ---- selection set: 2023-2024, real votes ------------------------------------
sel <- both %>% dplyr::filter(season %in% c(2023, 2024))
Xs <- as.matrix(sel[, paste0("xgboost_prob_", 0:3)])
Gs <- as.matrix(sel[, paste0("gam_prob_", 0:3)])

rmse_at <- function(w, X, G, ids, actual) {
  votes <- normalize_votes_ipf(w * X + (1 - w) * G, ids)
  sqrt(mean((votes - actual)^2))
}

grid <- seq(0, 1, by = 0.05)
cat(sprintf("selecting on 2023-2024 (%d rows, %d matches) by RMSE after IPF\n",
            nrow(sel), dplyr::n_distinct(sel$match_id)))
sel_rmse <- vapply(grid, function(w) rmse_at(w, Xs, Gs, sel$match_id, sel$brownlow_votes), numeric(1))
sel_mll  <- vapply(grid, function(w) mlogloss(w * Xs + (1 - w) * Gs, sel$brownlow_votes), numeric(1))

sel_tbl <- tibble(w_xgb = grid, sel_rmse = sel_rmse, sel_mlogloss = sel_mll)
print(as.data.frame(sel_tbl), row.names = FALSE, digits = 6)

w_rmse <- grid[which.min(sel_rmse)]
w_mll  <- grid[which.min(sel_mll)]
cat(sprintf("\n  RMSE-selected     w = %.2f\n", w_rmse))
cat(sprintf("  logloss-selected  w = %.2f   <- what the pipeline currently does\n", w_mll))

# ---- evaluate both, once, on 2025 --------------------------------------------
enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
hold <- attach_holdout_actuals(enhanced$holdout) %>%
  dplyr::select(player_name, team, date, match_id, Actual)
h <- both %>% dplyr::filter(season == 2025) %>%
  dplyr::inner_join(hold, by = c("player_name", "team", "date", "match_id"))
stopifnot(nrow(h) == nrow(enhanced$holdout))

Xh <- as.matrix(h[, paste0("xgboost_prob_", 0:3)])
Gh <- as.matrix(h[, paste0("gam_prob_", 0:3)])

r_rmse_sel <- rmse_at(w_rmse, Xh, Gh, h$match_id, h$Actual)
r_mll_sel  <- rmse_at(w_mll,  Xh, Gh, h$match_id, h$Actual)
r_shipped  <- rmse_at(0.858,  Xh, Gh, h$match_id, h$Actual)

cat("\n=== 2025 holdout RMSE, weights chosen on 2023-2024 only ===\n")
cat(sprintf("  logloss-selected w=%.2f  : %.5f   <- current pipeline objective\n", w_mll, r_mll_sel))
cat(sprintf("  shipped          w=0.858 : %.5f\n", r_shipped))
cat(sprintf("  RMSE-selected    w=%.2f  : %.5f   (delta vs logloss %+.5f)\n",
            w_rmse, r_rmse_sel, r_rmse_sel - r_mll_sel))

write_csv(sel_tbl, "analysis/harness/ensemble_weight_metric.csv")
cat("\nwritten: analysis/harness/ensemble_weight_metric.csv\n")
