# Is the ensemble weight grid costing us? --------------------------------------
#
# find_optimal_weights() searches seq(0.1, 0.9, length.out = 20) for the
# two-model case. Steps are 0.0421 apart and the 19th point is
# 0.1 + 18*0.8/19 = 0.85789, which is the 0.858 the pipeline keeps reporting.
# Two things were never checked:
#
#   1. GRID RESOLUTION. We are not measuring the optimal weight, we are
#      measuring the nearest of 20 allowed values. That also explains why the
#      weight did not move when the honest hyperparameters changed the
#      underlying predictions -- a sub-0.02 shift lands on the same grid point.
#
#   2. THE 0.9 CAP. Harmless while the scrambled-matrix bug (Sec 4) pinned
#      XGBoost at the MINIMUM 0.1. Now XGBoost sits one grid step below the
#      ceiling with the GAM adding 0.0002 log-loss, so the cap may be binding
#      and nobody has measured it.
#
# Weight selection uses 2023-2024 (the test seasons) and evaluation uses the
# real 2025 holdout, so the weight is never chosen on the data it is scored on.
# The full holdout curve is printed as a DIAGNOSTIC -- reading the argmin off
# that curve and calling it our weight would be selecting on the test set.

library(dplyr)
library(readr)
devtools::load_all()

xgb <- readRDS("results/xgboost_probabilities.rds")
gam <- readRDS("results/gam_probabilities.rds")
stopifnot(nrow(xgb) == nrow(gam))

both <- xgb %>%
  dplyr::inner_join(gam %>% dplyr::select(player_name, team, date, match_id,
                                          dplyr::starts_with("gam_prob_")),
                    by = c("player_name", "team", "date", "match_id"))
stopifnot(nrow(both) == nrow(xgb))

X <- as.matrix(both[, paste0("xgboost_prob_", 0:3)])
G <- as.matrix(both[, paste0("gam_prob_", 0:3)])

# ---- multinomial log-loss on rows with known votes ---------------------------
mlogloss <- function(P, y) {
  p <- pmax(P[cbind(seq_along(y), y + 1L)], 1e-15)
  -mean(log(p))
}

sel <- both$season %in% c(2023, 2024)
y_sel <- both$brownlow_votes[sel]

grid <- seq(0, 1, by = 0.001)
sel_scores <- vapply(grid, function(w) mlogloss(w * X[sel, ] + (1 - w) * G[sel, ], y_sel), numeric(1))
w_fine <- grid[which.min(sel_scores)]

# What the shipped coarse+capped search would pick on the SAME selection data.
coarse <- seq(0.1, 0.9, length.out = 20)
coarse_scores <- vapply(coarse, function(w) mlogloss(w * X[sel, ] + (1 - w) * G[sel, ], y_sel), numeric(1))
w_coarse <- coarse[which.min(coarse_scores)]

cat(sprintf("\nselection on 2023-2024 (%d rows):\n", sum(sel)))
cat(sprintf("  coarse grid, capped 0.1-0.9 : w_xgb = %.4f  (mlogloss %.5f)\n", w_coarse, min(coarse_scores)))
cat(sprintf("  fine grid,   uncapped 0-1   : w_xgb = %.4f  (mlogloss %.5f)\n", w_fine, min(sel_scores)))
cat(sprintf("  cap binding? %s\n", if (w_fine > 0.899) "YES - fine grid wants more XGBoost than the cap allows" else "no"))

# ---- evaluate both choices on the real 2025 holdout --------------------------
enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
hold <- attach_holdout_actuals(enhanced$holdout) %>%
  dplyr::select(player_name, team, date, match_id, Actual)

h <- both %>%
  dplyr::filter(season == 2025) %>%
  dplyr::inner_join(hold, by = c("player_name", "team", "date", "match_id"))
cat(sprintf("\n2025 holdout rows matched: %d (of %d)\n", nrow(h), sum(both$season == 2025)))
stopifnot(nrow(h) > 0, !anyNA(h$match_id))

Xh <- as.matrix(h[, paste0("xgboost_prob_", 0:3)])
Gh <- as.matrix(h[, paste0("gam_prob_", 0:3)])

score_w <- function(w) {
  P <- w * Xh + (1 - w) * Gh
  votes <- normalize_votes_ipf(P, h$match_id)
  c(rmse = sqrt(mean((votes - h$Actual)^2)), mll = mlogloss(P, h$brownlow_votes))
}

s_coarse <- score_w(w_coarse)
s_fine   <- score_w(w_fine)
cat("\n=== 2025 holdout, weight chosen on 2023-2024 ===\n")
cat(sprintf("  shipped (coarse, capped) w=%.4f : RMSE %.5f\n", w_coarse, s_coarse["rmse"]))
cat(sprintf("  fine grid, uncapped      w=%.4f : RMSE %.5f  (delta %+.5f)\n",
            w_fine, s_fine["rmse"], s_fine["rmse"] - s_coarse["rmse"]))

cat("\n=== DIAGNOSTIC ONLY: holdout RMSE across the whole weight range ===\n")
cat("    (the argmin here is NOT selectable -- it is chosen on the test set)\n")
diag_grid <- seq(0, 1, by = 0.05)
diag_tbl <- tibble(w_xgb = diag_grid,
                   rmse = vapply(diag_grid, function(w) score_w(w)["rmse"], numeric(1)))
print(as.data.frame(diag_tbl), row.names = FALSE, digits = 6)
cat(sprintf("\n  holdout-optimal w would have been %.2f (RMSE %.5f)\n",
            diag_tbl$w_xgb[which.min(diag_tbl$rmse)], min(diag_tbl$rmse)))
cat(sprintf("  pure XGBoost (w=1.00) RMSE %.5f | pure GAM (w=0.00) RMSE %.5f\n",
            score_w(1)["rmse"], score_w(0)["rmse"]))

write_csv(diag_tbl, "analysis/harness/ensemble_weight_grid.csv")
cat("\nwritten: analysis/harness/ensemble_weight_grid.csv\n")
