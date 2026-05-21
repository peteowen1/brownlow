# Global variable declarations to suppress R CMD check warnings

#' @importFrom grDevices dev.off png
#' @importFrom graphics par
#' @importFrom stats AIC BIC as.formula binom.test coef complete.cases cor cor.test
#' @importFrom stats filter lag median model.matrix nobs optim predict quantile 
#' @importFrom stats reorder sd
#' @importFrom utils head tail
NULL

# Define global variables used in dplyr/ggplot2/data.table chains
utils::globalVariables(c(
  # Player and match identifiers
  "player_name", "team", "season", "match_id", "match_date", "match_round",
  "player_id", "player_first_name", "player_last_name", "player_team",
  "player_position", "player_key", "player_label", "player_idx",
  
  # Team identifiers  
  "match_home_team", "match_away_team", "home_team", "away_team", 
  "match_winner", "winning_team", "home_away", "teams_in_round",
  
  # Score and results
  "home_score", "away_score", "home_final_score", "away_final_score",
  "team_score", "opposition_score", "opponent_score", "match_margin", 
  "margin", "result", "team_won", "is_winner", "is_home",
  
  # Statistical measures
  "disposals", "kicks", "handballs", "goals", "behinds", "marks", "tackles",
  "contested_possessions", "uncontested_possessions", "contested_marks",
  "clearances", "inside_fifties", "marks_inside_fifty", "goal_assists",
  "one_percenters", "spoils", "rebounds", "clangers",
  "afl_fantasy_score", "supercoach_score", "fantasy_score",
  "time_on_ground_percentage", "tog_percentage",
  
  # Derived metrics
  "impact_score", "efficiency_ratio", "player_disposal_share", 
  "player_goal_share", "player_tackle_share", "player_impact_share",
  "team_total_disposals", "team_total_goals", "team_total_tackles",
  "team_total_marks", "team_total_contested_poss", "team_total_clearances",
  "team_total_impact_score", "team_avg_fantasy_score",
  
  # Voting and predictions
  "brownlow_votes", "votes", "total_votes", "career_total_votes",
  "career_games_with_votes", "career_vote_rate", "career_avg_brownlow_votes_prior_seasons",
  "career_avg_coaches_votes", "career_avg_supercoach_score", "career_avg_afl_fantasy_score",
  "career_avg_player_disposal_share", "career_avg_impact_score", "career_avg_player_impact_share",
  "career_avg_opposition_score", "career_avg_team_out_scored_opposition", "career_avg_player_goal_share",
  "career_avg_goals", "career_avg_score_involvement", "career_avg_disposals",
  "career_avg_rating_points", "career_avg_metres_gained",
  "career_avg_votes_when_polling", "career_max_votes", "last_votes_date",
  "days_since_last_votes", "recent_votes_avg", "votes_in_last_3", "votes_in_last_5",
  "expected_votes", "expected_votes_history", "expected_votes_recent",
  "expected_votes_position", "expected_votes_combined", "historical_expectation_gap",
  
  # Position and role indicators
  "is_midfielder", "is_forward", "is_back", "games_as_mid", "votes_as_mid",
  "games_as_forward", "votes_as_forward", "games_as_back", "votes_as_back",
  
  # Probability columns
  "pvotes0", "pvotes1", "pvotes2", "pvotes3", "pvotes0_adj", "pvotes1_adj", 
  "pvotes2_adj", "pvotes3_adj", "match_sum_p0", "match_sum_p1", 
  "match_sum_p2", "match_sum_p3", "max_prob", "prob_bin",
  
  # Model evaluation metrics
  "predicted", "actual", "Predicted", "Actual", "Predicted_Votes", 
  "Actual_Votes", "Prediction_Error", "Correct_Prediction", "correct",
  "Mean_Error", "predicted_votes", "actual_votes", "Constrained_Votes",
  "raw_pred", "scaled_pred", "capped_pred", "match_total", "capped_total",
  
  # Team and match context
  "venue_name", "venue_clean", "close_game", "blowout", "umpire",
  "num_umpires", "other_umpires", "Date", "Round", "Venue", "Home.team", "Away.team",
  "Team", "Player", "Season",
  
  # Analysis variables
  "performance_level", "percentage", "n", "correlation", "abs_correlation",
  "statistic", "significance_level", "correlation_text", "strength",
  "Variable", "Missing_Percent", "Missing_Count", "completeness_rate",
  "variable", "value", "outlier_count", "outlier_percent", "total_records",
  "complete_count", "N_Teams", "Total_Votes", "Expected_Votes",
  
  # Simulation results
  "win_probability", "mean_votes", "median_votes", "sd_votes", "expected_position",
  "top3_probability", "top5_probability", "top10_probability", "q25_votes", "q75_votes",
  "votes_ci_lower", "votes_ci_upper", "prob_win", "prob_top3_not_win",
  "prob_top5_not_top3", "prob_top10_not_top5", "prob_not_top10",
  "finish_category", "probability", "avg_win_probability",
  
  # Plotting variables
  "Vote_Type", "Probability", "Actual_Match", "mean_confidence", "accuracy",
  "Residuals", "prob_bin", "Importance", "Feature", "Gain", "term", "importance",
  "type", "model", "Constrained_Votes",
  
  # Time and trends
  "month", "round_num", "games", "games_played", "games_with_votes",
  "team_games_played", "team_wins_so_far", "row_number", "team_form_trends",
  
  # Data manipulation helpers
  ".", ".data", "n_distinct", "everything", "any_of", "all_of", "starts_with",
  "where", "across", "desc", "weight", "sd", "median", "quantile"
))