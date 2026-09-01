suppressMessages({library(dplyr); library(readr); library(xgboost)})
set.seed(42); setwd("C:/dev/brownlow")
devtools::load_all(quiet = TRUE)

p   <- readRDS("data/processed/enhanced_modeling_data.rds")
tun <- readRDS("models/xgboost_tuning_results.rds")
rmse <- function(a,b) sqrt(mean((a-b)^2))

cat("=== splits ===\n")
cat(" train  :", paste(sort(unique(p$train$season)), collapse=","), "\n")
cat(" test   :", paste(sort(unique(p$test$season)), collapse=","), "\n")
cat(" holdout:", paste(p$holdout_seasons, collapse=","), "\n\n")

# the assertion now in 05_xgboost_model.R
stopifnot(!any(p$holdout_seasons %in% p$train$season))
cat("assertion: holdout season absent from train ... OK\n\n")

cat("=== train_production_xgboost() with the fix ===\n")
prod <- train_production_xgboost(
  train_data     = p$train,
  test_data      = p$test,
  feature_cols   = p$feature_cols,
  target_col     = p$target_col,
  best_params    = tun$best_params,
  best_iteration = tun$best_iteration,
  exclude_seasons = p$holdout_seasons,
  verbose = TRUE
)

# ---- score it on 2025, exactly as the submission pipeline would --------------
det <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv", show_col_types=FALSE) %>%
  transmute(date_j=as.Date(match_date, format="%d/%m/%Y"), team_j=player_team_std,
            player_j=full_player_name, Actual, ours_2025=brownlow_votes)
ho <- p$holdout %>% mutate(date_j=as.Date(date),
       team_j=ifelse(team=="Brisbane Lions","Brisbane",team), player_j=player_name) %>%
  left_join(det, by=c("date_j","team_j","player_j"))
stopifnot(nrow(ho)==9522, sum(ho$Actual)==1242)

score <- function(m, label){
  pm <- predict(m, as.matrix(ho[, p$feature_cols]))
  if (is.null(dim(pm))) pm <- matrix(pm, ncol=4, byrow=TRUE)
  stopifnot(nrow(pm)==nrow(ho), ncol(pm)==4)
  ev <- as.numeric(pm %*% c(0,1,2,3))
  v <- unlist(lapply(split(ev, ho$match_id), function(x){
        x <- pmax(0,pmin(3,x)); if (sum(x)>0) x <- x*6/sum(x); pmax(0,pmin(3,x))
      }))[order(order(ho$match_id))]
  cat(sprintf("  %-42s RMSE %.5f\n", label, rmse(v, ho$Actual)))
}

cat("\n=== 2025 holdout RMSE ===\n")
# The 2025 model artefacts cannot be deserialised under xgboost 3.2.1.1, so the
# 2015-2022 baseline below is the freshly-refit equivalent from experiment.R.
cat(sprintf("  %-42s RMSE %.5f
", "OLD: refit on 2015-2022 (was shipped)", 0.36680))
score(prod,                                      "NEW: production model (2015-2024)")
cat(sprintf("  %-42s RMSE %.5f\n", "shipped 2025 ensemble (xgb+gam)", rmse(ho$ours_2025, ho$Actual)))
cat(sprintf("  %-42s RMSE %.5f\n", "qp (2025 winner)", 0.34977))
cat("\ndone\n")
