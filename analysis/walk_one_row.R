# Walk one row end to end -----------------------------------------------------
#
# Aggregate metrics can be true and reassuring while the specification
# underneath is broken. Three bugs this session produced valid-looking output:
# a model trained on the wrong seasons, a scrambled probability matrix with
# perfect column means, and a stopifnot block that passed on an empty frame.
# None showed up in an RMSE.
#
# So before fitting anything else, take ONE concrete player-match, state the
# inputs and the target in plain words, print the actual row the model consumes,
# and print the upstream records that built each column.
#
# Default subject: Matt Rowell, Round 7 2025 - the single worst miss of the 2025
# submission. Predicted 0.03 expected votes; polled 3.

library(dplyr)
library(readr)
devtools::load_all()

PLAYER <- "Matt Rowell"
SEASON <- 2025
ROUND  <- "7"

enhanced <- readRDS("data/processed/enhanced_modeling_data.rds")
all_rows <- bind_rows(enhanced$train, enhanced$test, enhanced$holdout)

row <- all_rows %>%
  dplyr::filter(player_name == PLAYER, season == SEASON, match_round == ROUND)
stopifnot("subject row not found" = nrow(row) == 1)

cat(sprintf("=== SUBJECT: %s, %s round %s ===\n", PLAYER, SEASON, ROUND))
cat(sprintf("match: %s   date: %s   team: %s\n\n", row$match_id, row$date, row$team))

cat("--- what the model is asked ---\n")
cat("Given this player's performance in this match and their history before it,\n")
cat("what is the probability they receive 0, 1, 2 or 3 Brownlow votes?\n\n")

# ---- 1. the raw performance the umpires actually saw ------------------------
box <- c("disposals", "kicks", "handballs", "marks", "tackles", "goals", "behinds",
         "clearances", "contested_possessions", "score_involvements", "metres_gained",
         "coaches_votes", "supercoach_score", "afl_fantasy_score", "rating_points",
         "time_on_ground_percentage")
box <- intersect(box, names(row))
cat("--- 1. this game's box score ---\n")
for (b in box) cat(sprintf("  %-26s %s\n", b, format(row[[b]], digits = 4)))

# ---- 2. how that ranked inside the match ------------------------------------
match_rows <- all_rows %>% dplyr::filter(match_id == row$match_id)
cat(sprintf("\n--- 2. rank within the match (%d players) ---\n", nrow(match_rows)))
for (b in intersect(c("coaches_votes","supercoach_score","afl_fantasy_score",
                      "disposals","goals","contested_possessions"), box)) {
  r <- rank(-match_rows[[b]], ties.method = "min")[match_rows$player_name == PLAYER]
  cat(sprintf("  %-26s rank %2d of %d   (best in match: %s)\n",
              b, r, nrow(match_rows), format(max(match_rows[[b]], na.rm = TRUE), digits = 4)))
}

# ---- 3. the history features, and the records that built them ---------------
cat("\n--- 3. history features the model sees ---\n")
hist <- intersect(c("career_games_so_far", "career_avg_brownlow_votes_prior_seasons",
                    "career_avg_coaches_votes", "career_avg_disposals",
                    "recent_avg_impact", "player_coef"), names(row))
for (h in hist) cat(sprintf("  %-42s %s\n", h, format(row[[h]], digits = 4)))

cat("\n--- 3b. the upstream games those were computed from ---\n")
prior <- all_rows %>%
  dplyr::filter(player_name == PLAYER, date < row$date) %>%
  arrange(desc(date)) %>%
  select(any_of(c("season", "match_round", "team", "disposals", "coaches_votes",
                  "brownlow_votes"))) %>%
  head(8)
cat(sprintf("  %d prior games in the dataset; most recent 8:\n", nrow(
  all_rows %>% dplyr::filter(player_name == PLAYER, date < row$date))))
print(as.data.frame(prior), row.names = FALSE)

career <- all_rows %>%
  dplyr::filter(player_name == PLAYER, date < row$date) %>%
  summarise(games = n(), total_votes = sum(brownlow_votes),
            mean_disposals = mean(disposals, na.rm = TRUE),
            mean_coaches = mean(coaches_votes, na.rm = TRUE))
cat("\n  recomputed independently from those rows:\n")
cat(sprintf("    prior games %d | prior votes %s | mean disposals %.2f | mean coaches %.2f\n",
            career$games, career$total_votes, career$mean_disposals, career$mean_coaches))
cat("    -> compare against career_games_so_far and career_avg_* above.\n")
cat("       A mismatch means the feature is not what its name claims.\n")

# ---- 4. what each model said ------------------------------------------------
cat("\n--- 4. what the models predicted ---\n")
show_probs <- function(file, pfx, label) {
  if (!file.exists(file)) { cat(sprintf("  %-10s (not built yet)\n", label)); return(invisible()) }
  d <- readRDS(file) %>%
    dplyr::filter(player_name == PLAYER, match_id == row$match_id)
  if (nrow(d) != 1) { cat(sprintf("  %-10s %d rows matched - JOIN PROBLEM\n", label, nrow(d))); return(invisible()) }
  p <- as.numeric(d[, paste0(pfx, "prob_", 0:3)])
  cat(sprintf("  %-10s P(0)=%.4f P(1)=%.4f P(2)=%.4f P(3)=%.4f   E[votes]=%.3f\n",
              label, p[1], p[2], p[3], p[4], sum(p * 0:3)))
}
show_probs("results/xgboost_probabilities.rds",  "xgboost_",  "XGBoost")
show_probs("results/gam_probabilities.rds",      "gam_",      "GAM")
show_probs("results/ensemble_probabilities.rds", "ensemble_", "ensemble")

# ---- 5. the truth -----------------------------------------------------------
actual <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                   show_col_types = FALSE) %>%
  dplyr::filter(full_player_name == PLAYER,
                as.Date(match_date, format = "%d/%m/%Y") == as.Date(row$date))
cat("\n--- 5. what actually happened ---\n")
if (nrow(actual) == 1) {
  cat(sprintf("  actual votes: %d      2025 submission predicted: %.3f\n",
              actual$Actual, actual$brownlow_votes))
  cat(sprintf("  who else polled in this match:\n"))
  others <- read_csv("analysis/submission_comparison/detailed_predictions_vs_actual.csv",
                     show_col_types = FALSE) %>%
    # Filter on the match, not the date: several matches share a date.
    dplyr::filter(as.Date(match_date, format = "%d/%m/%Y") == as.Date(row$date),
                  player_team_std %in% standardize_team_name(match_rows$team),
                  Actual > 0) %>%
    select(full_player_name, player_team_std, Actual, predicted = brownlow_votes) %>%
    arrange(desc(Actual))
  print(as.data.frame(others), row.names = FALSE, digits = 3)
} else {
  cat("  could not resolve the actual result for this row\n")
}
