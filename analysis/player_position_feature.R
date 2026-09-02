# Does the model know what position anyone plays? ------------------------------
#
# It does not. `player_position` is in the raw datathon dataset, 100% populated
# across all 96,810 rows (2015-2025, including the prediction season), 20
# values -- and it is not a model feature. The mechanism is mundane rather than
# deliberate: prepare_modeling_data() builds feature_cols from numeric columns
# only, so a character column was silently dropped. Nobody decided this.
#
# It should matter a great deal. Votes per game by starting position, on rows
# with real votes:
#
#   C 0.518 | RR 0.447 | R 0.386 | FF 0.187 | RK 0.142 | ... | FB 0.039 |
#   BPL 0.034 | SUB 0.002
#
# A 15x spread between the centre and a back pocket. POSTMORTEM Sec 3 lists
# "non-midfielders" as the second-biggest miss pattern and recommends building
# ruck/defender features -- but every stat it names (hitouts_to_advantage,
# contested_marks, intercepts, spoils, one_percenters, ruck_contests) is
# ALREADY a feature. The gap was never the stats. The model simply cannot tell
# a centre from a full back.
#
# Encoded as one-hot dummies rather than a target encoding: 20 columns is
# cheap, and target encoding on a vote-related quantity would need time-aware
# folds to avoid leaking, which is exactly the class of bug this codebase keeps
# finding (Sec 5g, 5i, 5j).
#
# NOT leakage: the starting position is the team sheet, known before the first
# bounce. SUB is likewise a named role at selection, not an in-game outcome.

library(dplyr)
library(readr)
library(xgboost)
devtools::load_all()

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
tuning   <- readRDS("models/xgboost_tuning_results.rds")
feats    <- enhanced$feature_cols
target   <- enhanced$target_col
params   <- tuning$best_params

# NOT joinable on match_id: the enhanced data builds its own string key
# ("2015-04-02_Carlton_Richmond") while the raw file keeps the AFL numeric id
# (14581) -- zero overlap. And match_date here is d/m/Y, so a default as.Date()
# silently returns NA for every row. Both traps cost a run each; the key is
# (player_id, date), which is unique across all 96,810 raw rows.
raw <- read_csv("data/brownlow_datathon_dataset.csv", show_col_types = FALSE) %>%
  dplyr::transmute(player_id = as.numeric(player_id),
                   date = as.Date(match_date, format = "%d/%m/%Y"),
                   player_position) %>%
  dplyr::distinct()
stopifnot(!anyNA(raw$player_position), !anyNA(raw$date))
# One position per player-match, or the join would fan out.
stopifnot(nrow(raw) == nrow(dplyr::distinct(raw, player_id, date)))

attach_position <- function(df, label) {
  n0 <- nrow(df)
  out <- df %>%
    dplyr::mutate(player_id = as.numeric(player_id), date = as.Date(date)) %>%
    dplyr::left_join(raw, by = c("player_id", "date"))
  if (nrow(out) != n0) stop(sprintf("%s: join changed rows %d -> %d", label, n0, nrow(out)))
  if (anyNA(out$player_position)) {
    stop(sprintf("%s: %d rows got no position", label, sum(is.na(out$player_position))))
  }
  out
}

train   <- attach_position(enhanced$train, "train")
test    <- attach_position(enhanced$test, "test")
holdout <- attach_position(enhanced$holdout, "holdout")

# One-hot on the union of levels so every split has identical columns.
levs <- sort(unique(c(train$player_position, test$player_position, holdout$player_position)))
pos_cols <- paste0("pos_", janitor::make_clean_names(levs))
cat(sprintf("%d position levels -> %d dummies\n", length(levs), length(pos_cols)))

add_dummies <- function(df) {
  for (i in seq_along(levs)) df[[pos_cols[i]]] <- as.numeric(df$player_position == levs[i])
  df
}
train <- add_dummies(train); test <- add_dummies(test); holdout <- add_dummies(holdout)

# Every row is exactly one position.
stopifnot(all(rowSums(as.matrix(holdout[, pos_cols])) == 1))

holdout <- attach_holdout_actuals(holdout)
train_all <- bind_rows(train, test)

cat("\n=== votes per game by position, training seasons ===\n")
print(as.data.frame(train_all %>% group_by(player_position) %>%
  summarise(n = n(), votes_per_game = round(mean(.data[[target]]), 4), .groups = "drop") %>%
  arrange(desc(votes_per_game))), row.names = FALSE)

cat("\n=== rounds via early stopping ===\n")
rounds <- optimal_nrounds(train_all, feats, target, params,
                          max_rounds = 600, early_stopping_rounds = 40, verbose = TRUE)$nrounds

results <- bind_rows(
  evaluate_seeds(xgb_fit_predict(feats, target, params, rounds),
                 train_all, holdout, "baseline (no position)", seeds = 1:5, normalizer = "ipf"),
  evaluate_seeds(xgb_fit_predict(c(feats, pos_cols), target, params, rounds),
                 train_all, holdout, "+ player_position one-hot", seeds = 1:5, normalizer = "ipf")
)
compare_configs(results, baseline_label = "baseline (no position)")

write_csv(results, "analysis/harness/player_position_feature.csv")
cat("\nwritten: analysis/harness/player_position_feature.csv\n")
