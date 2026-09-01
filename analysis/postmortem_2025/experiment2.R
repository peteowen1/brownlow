suppressMessages({library(dplyr); library(readr); library(xgboost)})
set.seed(42); setwd("C:/dev/brownlow")
p <- readRDS("data/processed/enhanced_modeling_data.rds"); fc <- p$feature_cols; tc <- p$target_col
tun <- readRDS("models/xgboost_tuning_results.rds")
rmse <- function(a,b) sqrt(mean((a-b)^2))

det <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv", show_col_types=FALSE) %>%
  transmute(date_j=as.Date(match_date, format="%d/%m/%Y"), team_j=player_team_std,
            player_j=full_player_name, Actual, ours_2025=brownlow_votes)
hold <- p$holdout %>% mutate(date_j=as.Date(date), team_j=ifelse(team=="Brisbane Lions","Brisbane",team),
                             player_j=player_name) %>%
  left_join(det, by=c("date_j","team_j","player_j"))
stopifnot(nrow(hold)==9522, sum(hold$Actual)==1242)

rank_src <- intersect(c("afl_fantasy_score","supercoach_score","rating_points","coaches_votes","disposals",
  "contested_possessions","goals","score_involvements","clearances","effective_disposals","metres_gained",
  "impact_score","midfield_score","tackles","marks"), names(p$train))
add_ranks <- function(d){ g <- d %>% group_by(match_id)
  for (v in rank_src) g <- g %>% mutate(
    !!paste0("mrank_",v) := rank(-.data[[v]], na.last="keep", ties.method="average"),
    !!paste0("mpct_", v) := (dplyr::n()-rank(.data[[v]], na.last="keep", ties.method="average"))/dplyr::n(),
    !!paste0("mgap_", v) := .data[[v]] - max(.data[[v]], na.rm=TRUE))
  ungroup(g) }
rank_cols <- as.vector(outer(c("mrank_","mpct_","mgap_"), rank_src, paste0))
tr <- add_ranks(bind_rows(p$train, p$test)); ho <- add_ranks(hold); fc2 <- c(fc, rank_cols)

pred_mat <- function(m, nd, feats){ pm <- predict(m, as.matrix(nd[,feats]))
  if (is.null(dim(pm))) pm <- matrix(pm, ncol=4, byrow=TRUE); pm }

# --- normalisation A: current approach (scale expected votes to 6) -----------
norm_scale <- function(pm, mid){
  ev <- as.numeric(pm %*% c(0,1,2,3))
  unlist(lapply(split(ev, mid), function(v){ v <- pmax(0,pmin(3,v))
    if (sum(v)>0) v <- v*6/sum(v); pmax(0,pmin(3,v)) }))[order(order(mid))]
}
# --- normalisation B: IPF / Sinkhorn, per match ------------------------------
# rows sum to 1 ; column targets = (n-3, 1, 1, 1)  => total expected votes = 6 exactly
norm_ipf <- function(pm, mid, iters = 200){
  out <- numeric(nrow(pm)); idx <- split(seq_len(nrow(pm)), mid)
  for (ii in idx){
    M <- pmax(pm[ii,,drop=FALSE], 1e-12); n <- nrow(M); tgt <- c(n-3,1,1,1)
    for (k in seq_len(iters)){
      M <- M / rowSums(M)                       # each player's 4 probs sum to 1
      cs <- colSums(M); M <- sweep(M, 2, tgt/cs, "*")   # each vote class allocated once
    }
    M <- M / rowSums(M)
    out[ii] <- as.numeric(M %*% c(0,1,2,3))
  }
  out
}

cat("=== reference ===\n")
cat(sprintf("  shipped 2025 submission   RMSE %.5f\n", rmse(hold$ours_2025, hold$Actual)))
cat(sprintf("  qp (2025 winner)          RMSE %.5f\n\n", 0.34977))

cat("=== multinomial xgb, 2015-2024 + ranks ===\n")
dtr <- xgb.DMatrix(as.matrix(tr[,fc2]), label = tr[[tc]])
mC <- xgb.train(tun$best_params, dtr, nrounds = tun$best_iteration, verbose = 0)
pmC <- pred_mat(mC, ho, fc2)
vC_scale <- norm_scale(pmC, ho$match_id); vC_ipf <- norm_ipf(pmC, ho$match_id)
cat(sprintf("  raw expected votes, no constraint   RMSE %.5f\n", rmse(as.numeric(pmC %*% c(0,1,2,3)), ho$Actual)))
cat(sprintf("  scale-to-6 (current method)         RMSE %.5f\n", rmse(vC_scale, ho$Actual)))
cat(sprintf("  IPF / Sinkhorn (3 class constraints) RMSE %.5f\n", rmse(vC_ipf, ho$Actual)))

cat("\n=== direct RMSE regression on votes (2015-2024 + ranks) ===\n")
pr <- list(objective="reg:squarederror", eval_metric="rmse", max_depth=tun$best_params$max_depth,
           eta=tun$best_params$eta, subsample=tun$best_params$subsample,
           colsample_bytree=tun$best_params$colsample_bytree,
           min_child_weight=tun$best_params$min_child_weight, gamma=tun$best_params$gamma,
           lambda=tun$best_params$lambda, alpha=tun$best_params$alpha)
mR <- xgb.train(pr, dtr, nrounds = tun$best_iteration, verbose = 0)
evR <- predict(mR, as.matrix(ho[,fc2]))
vR <- unlist(lapply(split(evR, ho$match_id), function(v){v<-pmax(0,pmin(3,v)); if(sum(v)>0) v<-v*6/sum(v); pmax(0,pmin(3,v))}))[order(order(ho$match_id))]
cat(sprintf("  reg:squarederror, scale-to-6        RMSE %.5f\n", rmse(vR, ho$Actual)))

cat("\n=== blends ===\n")
best <- c(); for (w in seq(0,1,0.1)){ r <- rmse(w*vC_ipf + (1-w)*hold$ours_2025, ho$Actual)
  cat(sprintf("  %.1f*IPF + %.1f*shipped              RMSE %.5f\n", w, 1-w, r)) }
cat("\n")
for (w in seq(0,1,0.25)){ r <- rmse(w*vC_ipf + (1-w)*vR, ho$Actual)
  cat(sprintf("  %.2f*IPF + %.2f*regression           RMSE %.5f\n", w, 1-w, r)) }
three <- function(a,b,cc) rmse(a*vC_ipf + b*vR + cc*hold$ours_2025, ho$Actual)
cat(sprintf("\n  1/3 each (IPF, reg, shipped)        RMSE %.5f\n", three(1/3,1/3,1/3)))
saveRDS(list(ho=ho, vC_ipf=vC_ipf, vC_scale=vC_scale, vR=vR, pmC=pmC),
        "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-brownlow/415b1684-42c6-4814-8680-c9e78a09b68b/scratchpad/exp2_out.rds")
cat("\ndone\n")
