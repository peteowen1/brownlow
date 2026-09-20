# Assemble the Brownlow Vote Tracker artifact page from the priced outputs.
# Inputs (all written by price_markets_2026.R / ou_correlated_multis.R):
#   results/markets/expected_votes_by_round_2026.csv, player_summary_2026.csv,
#   sportsbet_2026_kelly.csv, ou_multis_priced.csv
# Output: results/markets/brownlow-vote-tracker.html (publish with the Artifact
# tool to https://claude.ai/artifact/GdnuLLmYyM5G5zzwNJHJfT).
suppressMessages({ library(data.table); library(jsonlite) })
ev <- fread("results/markets/expected_votes_by_round_2026.csv")
su <- fread("results/markets/player_summary_2026.csv")
k  <- fread("results/markets/sportsbet_2026_kelly.csv")[edge > 0, .(market, sel, odds, p, edge, kelly)]
mu <- fread("results/markets/ou_multis_priced.csv")[edge > 0][1:40, .(market = "OU multi",
        sel = paste0(leg1, " + ", leg2, " (lift ", lift, ")"), odds = round(odds, 2), p = p_joint, edge, kelly)]
rounds <- sort(unique(ev$round))
w   <- dcast(ev, player_name + team ~ round, value.var = "xv")
opp <- dcast(ev, player_name + team ~ round, value.var = "opponent", fill = "")
su  <- su[match(paste(w$player_name, w$team), paste(player_name, team))]
players <- lapply(seq_len(nrow(w)), function(i) {
  xv <- unname(as.list(w[i, as.character(rounds), with = FALSE]))
  list(n = w$player_name[i], t = w$team[i], xv = xv, op = unname(as.list(opp[i, as.character(rounds), with = FALSE])),
       pos = su$position[i], mean = round(su$sim_mean[i], 1), sd = round(su$sim_sd[i], 1), win = su$win_pct[i],
       top3 = su$top3_pct[i], top10 = su$top10_pct[i], games = sum(!is.na(unlist(xv)))) })
stopifnot(nrow(k) > 0, length(players) == 668)
json <- toJSON(list(rounds = rounds, players = players, bets = rbind(k, mu), pulled_at = fread("results/markets/sportsbet_lines_latest.csv")$pulled_at[1]),
               auto_unbox = TRUE, na = "null", digits = 3)
head <- readLines("analysis/artifact/head.html", warn = FALSE); tail <- readLines("analysis/artifact/tail.html", warn = FALSE)
writeLines(c(head, json, tail), "results/markets/brownlow-vote-tracker.html")
cat(sprintf("artifact page: %d players, %d bet lines, %.0f KB -> results/markets/brownlow-vote-tracker.html\n",
            length(players), nrow(k) + nrow(mu), file.size("results/markets/brownlow-vote-tracker.html") / 1024))
