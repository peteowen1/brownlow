# Harness comparison: how should probabilities become a submission? ------------
#
# Compares the three ways of turning per-player vote probabilities into the
# 6-votes-per-match submission the competition scores:
#
#   scale-to-6  the current method. Rescale expected votes so the match totals 6.
#               Enforces one of the four constraints that actually hold.
#   IPF         Sinkhorn on all four: each player's probabilities sum to 1, and
#               across the match P(3), P(2) and P(1) each sum to 1.
#   Plackett-Luce  model the umpires' actual process - draw a best, then a
#               second, then a third from the field. Constraints are structural.
#
# Every configuration is scored over several seeds, because xgboost's row and
# column subsampling makes a single run vary by ~0.001 - which is a tenth of the
# entire 1st-to-4th spread in the 2025 datathon.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

N_SEEDS <- 5
seeds <- seq_len(N_SEEDS)

prepared <- readRDS("data/processed/modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")

holdout <- attach_holdout_actuals(prepared$holdout)
train   <- bind_rows(prepared$train, prepared$test) %>%
  dplyr::filter(!season %in% prepared$holdout_seasons)

cat(sprintf("train %d rows (seasons %s) | holdout %d rows | %d features | %d seeds\n\n",
            nrow(train), paste(range(train$season), collapse = "-"),
            nrow(holdout), length(prepared$feature_cols), N_SEEDS))

# Fit once per seed; every configuration below reuses these predictions so the
# comparison isolates the constraint method rather than re-rolling the model.
fit_predict <- xgb_fit_predict(prepared$feature_cols, prepared$target_col,
                               tuning$best_params, tuning$best_iteration)

cat("fitting base model, one per seed...\n")
base_probs <- lapply(seeds, function(s) {
  cat(sprintf("  seed %d\n", s))
  fit_predict(train, holdout, s)
})

rmse <- function(v) sqrt(mean((v - holdout$Actual)^2))

score_config <- function(votes_from_probs, label) {
  vals <- vapply(base_probs, function(pm) rmse(votes_from_probs(pm)), numeric(1))
  tibble(label = label, normalizer = NA_character_,
         rmse = mean(vals), sd = sd(vals), min = min(vals), max = max(vals),
         n_seeds = length(vals))
}

# Plackett-Luce worth comes from the model's own P(3 votes): the probability it
# assigns to this player being the best on the ground. beta sharpens or flattens
# that before the sequential draw.
pl_votes <- function(pm, beta) {
  pl_expected_votes(log(pmax(pm[, 4], 1e-12)), holdout$match_id, beta = beta)
}

results <- bind_rows(
  score_config(function(pm) normalize_votes_scale(pm, holdout$match_id), "scale-to-6 (current)"),
  score_config(function(pm) normalize_votes_ipf(pm, holdout$match_id),   "IPF (4 constraints)"),
  lapply(c(0.6, 0.8, 1.0, 1.2, 1.5), function(b)
    score_config(function(pm) pl_votes(pm, b), sprintf("Plackett-Luce beta=%.1f", b))) %>% bind_rows()
)

# Best PL setting, then blends of the two structurally-valid methods.
best_beta <- as.numeric(sub(".*beta=", "",
  results$label[results$label != "scale-to-6 (current)" &
                results$label != "IPF (4 constraints)"][
    which.min(results$rmse[grepl("Plackett", results$label)])]))
cat(sprintf("\nbest Plackett-Luce beta: %.1f\n", best_beta))

results <- bind_rows(results,
  lapply(c(0.25, 0.5, 0.75), function(w)
    score_config(function(pm) {
      w * pl_votes(pm, best_beta) + (1 - w) * normalize_votes_ipf(pm, holdout$match_id)
    }, sprintf("%.2f PL + %.2f IPF", w, 1 - w))) %>% bind_rows()
)

comparison <- compare_configs(results, baseline_label = "scale-to-6 (current)")

dir.create("analysis/harness", showWarnings = FALSE, recursive = TRUE)
write_csv(comparison, "analysis/harness/constraint_method_comparison.csv")
cat("\nwritten: analysis/harness/constraint_method_comparison.csv\n")
