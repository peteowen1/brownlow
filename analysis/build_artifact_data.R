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
mu <- fread("results/markets/ou_multis_priced.csv")[edge > 0][, .(market = "OU multi",
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
lv <- fread("results/markets/live_state.csv")
g  <- fread("results/markets/live_grid.csv")
# Assert coverage, not presence. Every check below reads clean on a zero-row
# table -- anyNA() of nothing is FALSE and all() of nothing is TRUE -- so a
# live_count.R run that died partway would publish an empty live tab with all
# its assertions passing.
stopifnot("live_state.csv is empty" = nrow(lv) > 0,
          "live_grid.csv is empty" = nrow(g) > 0,
          "live_state.csv is missing players" = nrow(lv) == length(players))
gav <- dcast(g, pkey ~ round, value.var = "votes")
gxv <- dcast(g, pkey ~ round, value.var = "xv")
rc <- as.character(rounds)
mi <- match(lv$pkey, gav$pkey)
stopifnot(!anyNA(mi), identical(setdiff(rc, names(gav)), character(0)))
live <- lv[, .(n = player_name, t = team, read, gl = games_left, proj, sd = proj_sd,
               q10, q90, win = win_pct, dwin = win_delta, top3 = top3_pct, top5 = top5_pct,
               top10 = top10_pct, top20 = top20_pct,
               init = pre_mean, pre = pre_win_pct)]
live[, pi := match(paste(lv$player_name, lv$team), paste(w$player_name, w$team)) - 1L]
stopifnot(!anyNA(live$pi))
AV <- as.matrix(gav[mi, rc, with = FALSE]); XV <- as.matrix(gxv[mi, rc, with = FALSE])
live[, av  := lapply(seq_len(.N), function(i) unname(as.list(AV[i, ])))]
live[, gxv := lapply(seq_len(.N), function(i) unname(as.list(XV[i, ])))]
live_meta <- fromJSON("results/markets/live_meta.json")

# Every match as (round, player indices, P(3 votes)) so the page can run the
# Plackett-Luce count itself: the browser fetches the AFL feed, works out which
# matches are still unread, and re-simulates only those. That is what makes the
# page live rather than a snapshot of whenever it was last built.
ev[, pidx := match(paste(player_name, team), paste(w$player_name, w$team)) - 1L]
ev[, mkey := paste(round, pmin(team, opponent), pmax(team, opponent))]
stopifnot(!anyNA(ev$pidx))
ms <- ev[, .(r = round[1L], p = list(pidx), wt = list(pmax(p3, 1e-9))), by = mkey]
stopifnot(all(lengths(ms$p) >= 3))
matches <- lapply(seq_len(nrow(ms)), function(i) list(r = ms$r[i], p = ms$p[[i]], w = ms$wt[[i]]))
json <- toJSON(list(rounds = rounds, players = players, bets = rbind(k, mu),
                    live = live, live_meta = live_meta, matches = matches,
                    pulled_at = fread("results/markets/sportsbet_lines_latest.csv")$pulled_at[1]),
               auto_unbox = TRUE, na = "null", digits = 3, dataframe = "rows")
head <- readLines("analysis/artifact/head.html", warn = FALSE); tail <- readLines("analysis/artifact/tail.html", warn = FALSE)
writeLines(c(head, json, tail), "results/markets/brownlow-vote-tracker.html")
cat(sprintf("artifact page: %d players, %d bet lines, live %d/%d matches read, %.0f KB -> results/markets/brownlow-vote-tracker.html\n",
            length(players), nrow(k) + nrow(mu), live_meta$matches_read, live_meta$matches_total,
            file.size("results/markets/brownlow-vote-tracker.html") / 1024))
