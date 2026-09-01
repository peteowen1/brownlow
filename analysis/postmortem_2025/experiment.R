suppressMessages({library(dplyr); library(readr); library(xgboost)})
set.seed(42)
setwd("C:/dev/brownlow")

p  <- readRDS("data/processed/enhanced_modeling_data.rds")
fc <- p$feature_cols; tc <- p$target_col
tun <- readRDS("models/xgboost_tuning_results.rds")

# ---- attach 2025 actuals to holdout -------------------------------------
det <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv", show_col_types=FALSE) %>%
  transmute(date_j = as.Date(match_date, format="%d/%m/%Y"),
            team_j = player_team_std, player_j = full_player_name,
            Actual, ours_2025 = brownlow_votes)

hold <- p$holdout %>%
  mutate(date_j = as.Date(date),
         team_j = ifelse(team == "Brisbane Lions", "Brisbane", team),
         player_j = player_name)

hold <- hold %>% left_join(det, by = c("date_j","team_j","player_j"))
stopifnot(nrow(hold) == 9522)
cat(sprintf("JOIN: %d/%d matched, total actual votes %d (expect 1242)\n",
            sum(!is.na(hold$Actual)), nrow(hold), sum(hold$Actual, na.rm=TRUE)))
hold <- hold %>% filter(!is.na(Actual))
stopifnot(sum(hold$Actual) == 1242)

# ---- helpers -------------------------------------------------------------
rmse <- function(p,a) sqrt(mean((p-a)^2))

# scale expected votes to exactly 6 per match, clamped 0-3 (same as submission)
to_votes <- function(pm, mid){
  ev <- pm %*% c(0,1,2,3)
  d <- data.frame(mid=mid, ev=as.numeric(ev)) %>% group_by(mid) %>%
    mutate(v = pmax(0, pmin(3, ev)), v = if (sum(v) > 0) v * 6 / sum(v) else v, v = pmax(0, pmin(3, v))) %>% ungroup()
  d$v
}

fit_predict <- function(train_df, newdata, feats, label){
  dtr <- xgb.DMatrix(as.matrix(train_df[, feats]), label = train_df[[tc]])
  m <- xgb.train(params = tun$best_params, data = dtr, nrounds = tun$best_iteration, verbose = 0)
  pm <- predict(m, as.matrix(newdata[, feats]))
  if (is.null(dim(pm))) pm <- matrix(pm, ncol = 4, byrow = TRUE)
  v <- to_votes(pm, newdata$match_id)
  cat(sprintf("  %-46s RMSE %.5f\n", label, rmse(v, newdata$Actual)))
  list(model=m, votes=v, pm=pm)
}

cat("\n=== REFERENCE ===\n")
cat(sprintf("  %-46s RMSE %.5f\n", "actual 2025 submission (xgb+gam ensemble)", rmse(hold$ours_2025, hold$Actual)))
cat(sprintf("  %-46s RMSE %.5f\n", "qp (2025 winner)", 0.34977))

cat("\n=== EXPERIMENT 1: training window ===\n")
A <- fit_predict(p$train, hold, fc, "A: train 2015-2022 (what was shipped)")
B <- fit_predict(bind_rows(p$train, p$test), hold, fc, "B: train 2015-2024 (all available)")

cat("\n=== EXPERIMENT 2: add within-match rank features ===\n")
rank_src <- intersect(c("afl_fantasy_score","supercoach_score","rating_points","coaches_votes",
                        "disposals","contested_possessions","goals","score_involvements",
                        "clearances","effective_disposals","metres_gained","impact_score",
                        "midfield_score","tackles","marks"), names(p$train))
cat("  rank sources:", paste(rank_src, collapse=", "), "\n")

add_ranks <- function(d){
  g <- d %>% group_by(match_id)
  for (v in rank_src) {
    g <- g %>% mutate(
      !!paste0("mrank_", v) := rank(-.data[[v]], na.last="keep", ties.method="average"),
      !!paste0("mpct_",  v) := (dplyr::n() - rank(.data[[v]], na.last="keep", ties.method="average")) / dplyr::n(),
      !!paste0("mgap_",  v) := .data[[v]] - max(.data[[v]], na.rm=TRUE)
    )
  }
  ungroup(g)
}
rank_cols <- as.vector(outer(c("mrank_","mpct_","mgap_"), rank_src, paste0))
tr2 <- add_ranks(bind_rows(p$train, p$test)); ho2 <- add_ranks(hold)
fc2 <- c(fc, rank_cols)
C <- fit_predict(tr2, ho2, fc2, "C: train 2015-2024 + within-match ranks")

cat("\n=== EXPERIMENT 3: blend B/C with shipped ensemble ===\n")
for (w in seq(0,1,0.25)) cat(sprintf("  %.2f*C + %.2f*shipped   RMSE %.5f\n", w, 1-w, rmse(w*C$votes + (1-w)*hold$ours_2025, hold$Actual)))

saveRDS(list(hold=hold, A=A$votes, B=B$votes, C=C$votes, rank_cols=rank_cols),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-brownlow/415b1684-42c6-4814-8680-c9e78a09b68b/scratchpad/exp_out.rds")
cat("\ndone\n")
