# Live in-count re-simulation.
#
# During the count the AFL publishes votes round by round at
# api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S2026014 (the feed behind
# afl.com.au/brownlow-medal/live-tracker). Every match that has been read out is
# a FACT; only the unread matches are still random. So the live forecast is
#
#   season total = votes already read  +  simulation of the unread matches only
#
# which is both more accurate and cheaper than a full-season sim, and gets
# cheaper as the night goes on.
#
# The momentum tilt from analysis/in_count_update_value.R is deliberately NOT
# applied: measured over 2017-2025 it made the weighted top-20 log-loss WORSE in
# 6 of 9 seasons (mean -0.023). The persistence signal is real but too weak to
# pay for the variance it adds, so remaining matches use raw model probabilities.
#
# Run: powershell.exe -Command 'Rscript "analysis/live_count.R"'

suppressMessages({ library(data.table); library(jsonlite); devtools::load_all(quiet = TRUE) })
N_SIMS <- as.integer(Sys.getenv("LIVE_SIMS", "20000"))
SEASON <- "CD_S2026014"
dir.create("results/live", showWarnings = FALSE, recursive = TRUE)

# ---- live feed ---------------------------------------------------------------
tok <- httr::content(httr::POST("https://api.afl.com.au/cfs/afl/WMCTok"))$token
stopifnot("no AFL media token" = is.character(tok) && nzchar(tok))
feed_url <- sprintf("https://api.afl.com.au/cfs/afl/bfawards/leaderboard/season/%s", SEASON)
resp <- httr::GET(feed_url, httr::add_headers(`x-media-mis-token` = tok),
                  query = list(`_` = as.integer(Sys.time())))
httr::stop_for_status(resp)
feed <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
writeLines(toJSON(feed, auto_unbox = TRUE), sprintf("results/live/feed_%s.json", stamp))

lb <- feed$leaderboard
votes <- rbindlist(lapply(lb, function(p) {
  rr <- p$roundByRoundVotes
  if (!length(rr)) return(NULL)
  data.table(afl_name = paste(p$player$givenName, p$player$surname),
             afl_team = p$team$teamName,
             round    = vapply(rr, function(x) as.integer(x$roundNumber), 1L),
             votes    = vapply(rr, function(x) as.integer(x$votes), 1L))
}))
cat(sprintf("feed status %s | %d players on the board | %d votes read\n",
            feed$status, length(lb), sum(votes$votes)))

# ---- our 2026 rows -----------------------------------------------------------
ep  <- fread("results/ensemble_probabilities.csv")[season == 2026]
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team"))[season == 2026]
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[, .(player_name, team, date, match_round)],
            by = c("player_name","team","date"), all.x = TRUE)
stopifnot(nrow(ep) == 9522, !anyNA(ep$match_round))
ep[, key := paste(player_name, team, sep = "_")]

# ---- name and team resolution ------------------------------------------------
TEAM <- c("Adelaide Crows"="Adelaide","Brisbane Lions"="Brisbane","Carlton"="Carlton",
          "Collingwood"="Collingwood","Essendon"="Essendon","Fremantle"="Fremantle",
          "Geelong Cats"="Geelong","Gold Coast SUNS"="Gold Coast",
          "GWS GIANTS"="Greater Western Sydney","Hawthorn"="Hawthorn","Melbourne"="Melbourne",
          "North Melbourne"="North Melbourne","Port Adelaide"="Port Adelaide","Richmond"="Richmond",
          "St Kilda"="St Kilda","Sydney Swans"="Sydney","West Coast Eagles"="West Coast",
          "Western Bulldogs"="Western Bulldogs")
squash <- squash_name   # R/name_aliases.R
votes[, team := unname(TEAM[afl_team])]
if (anyNA(votes$team)) stop("unmapped AFL team: ", paste(unique(votes[is.na(team), afl_team]), collapse = ", "))

lookup <- unique(ep[, .(key, player_name, team, sq = squash(player_name))])
# The merge below is on (sq, team), so two different players whose names squash
# to the same string in the same team would each match the feed's single vote
# row and DOUBLE that vote rather than drop it. Nothing downstream would notice:
# the match would total 7 and only the 6-vote assertion would catch it, after
# the fact. Assert the key is unique instead.
stopifnot("two players share a squashed name within a team" =
            !anyDuplicated(lookup[, .(sq, team)]))
# to_our_name() squashes the feed's spelling and applies the shared alias table
# in one step (R/name_aliases.R). The feed uses full forms and middle initials
# the datathon data does not carry; team separates the players who still collide.
votes[, sq := to_our_name(afl_name)]
votes <- merge(votes, lookup[, .(sq, team, key)], by = c("sq","team"), all.x = TRUE)
lost <- votes[is.na(key)]
# STOP rather than carry on. A name the feed spells differently to the datathon
# data silently subtracts that player's votes from their total and leaves their
# match showing five, which is a published number that is wrong and looks
# authoritative -- exactly what happened to Harrison/Harry Petty on count night.
# Halting costs one refresh cycle and the page keeps serving its last correct
# state, which is strictly better than replacing it with a wrong one. The fix is
# always a one-line entry in R/name_aliases.R, so it clears inside a cycle.
if (nrow(lost)) {
  print(lost[, .(votes = sum(votes)), by = .(afl_name, afl_team)])
  stop(sprintf(paste0("%d vote(s) from %d unmatched player(s) would be dropped. ",
                      "Add the spelling(s) above to R/name_aliases.R and rerun."),
               sum(lost$votes), uniqueN(lost$afl_name)))
}

# ---- which matches have been read out ----------------------------------------
# A match is identified by (round, team). Any vote from it means it has been read.
played <- unique(ep[, .(match_id, match_round, team)])
vm <- merge(votes[!is.na(key)], played, by.x = c("round","team"), by.y = c("match_round","team"), all.x = TRUE)
stopifnot(!anyNA(vm$match_id))
done <- vm[, .(read_votes = sum(votes)), by = match_id]
# A match that does not total 6 has been read wrong, and there is no safe way to
# carry on with it. Treating it as settled removes it from the simulation, so
# the missing vote is never counted and never simulated. Putting it back into
# `future` instead is WORSE, not better: `obs` below sums every matched vote
# regardless of settled status, so the match would contribute its partial read
# AND a fresh six-vote simulation -- eleven votes from a six-vote game. With the
# unmatched-name stop above, the only remaining cause is a genuinely partial
# feed mid-round, which the next refresh resolves on its own.
odd <- done[read_votes != 6]
if (nrow(odd)) {
  print(odd)
  stop(sprintf("%d match(es) above do not total 6 votes; refusing to publish a partial read.",
               nrow(odd)))
}
settled <- done$match_id
rounds_done <- sort(unique(ep[match_id %in% settled, match_round]))
cat(sprintf("%d of %d matches read; rounds %s\n", length(settled), uniqueN(ep$match_id),
            paste(range(rounds_done), collapse = "-")))

obs <- vm[, .(read = sum(votes)), by = key]

# ---- simulate only the unread matches ----------------------------------------
future <- ep[!match_id %in% settled]
all_keys <- sort(unique(ep$key))
if (nrow(future) == 0) {
  sim <- matrix(0L, length(all_keys), N_SIMS, dimnames = list(all_keys, NULL))
} else {
  prep <- prepare_season_sim(future)
  t0 <- Sys.time()
  s <- simulate_counts_vectorised(prep, n_sims = N_SIMS, seed = as.integer(Sys.time()) %% 100000L)
  cat(sprintf("simulated %d counts over %d unread matches in %.1f sec\n", N_SIMS,
              uniqueN(future$match_id), as.numeric(Sys.time() - t0, units = "secs")))
  sim <- matrix(0L, length(all_keys), N_SIMS, dimnames = list(all_keys, NULL))
  sim[match(rownames(s), all_keys), ] <- s
}
read_vec <- rep(0L, length(all_keys))
read_vec[match(obs$key, all_keys)] <- obs$read
totals <- sim + read_vec

# ---- summaries ---------------------------------------------------------------
# Finishing positions from the k-th largest total in each count rather than a
# full rank(): rank() on every column is the slowest thing in the script once
# N_SIMS is large, and "rank <= k with ties.method = min" is exactly
# "total >= the k-th largest", which one partial sort per column gives.
nk <- nrow(totals)
kth <- apply(totals, 2, function(x) -sort(-x, partial = c(1, 3, 5, 10, 20))[c(1, 3, 5, 10, 20)])
at_least <- function(row) rowMeans(totals >= rep(kth[row, ], each = nk))
is_max <- totals == rep(kth[1, ], each = nk)
p_win <- rowMeans(is_max & rep(colSums(is_max) == 1, each = nk))
games_left <- ep[!match_id %in% settled, .N, by = key]
xv_left <- ep[!match_id %in% settled, .(xv_left = sum(ensemble_expected_votes)), by = key]

live <- data.table(
  pkey = all_keys,
  player_name = sub("_[^_]+$", "", all_keys),
  team = sub("^.*_", "", all_keys),
  read = read_vec,
  games_left = games_left$N[match(all_keys, games_left$key)],
  xv_left = xv_left$xv_left[match(all_keys, xv_left$key)],
  proj = rowMeans(totals),
  proj_sd = apply(totals, 1, sd),
  q10 = apply(totals, 1, quantile, 0.10),
  q90 = apply(totals, 1, quantile, 0.90),
  win_pct = round(100 * p_win, 2),
  top3_pct = round(100 * at_least(2), 1),
  top5_pct = round(100 * at_least(3), 1),
  top10_pct = round(100 * at_least(4), 1),
  top20_pct = round(100 * at_least(5), 1))
live[is.na(games_left), `:=`(games_left = 0L, xv_left = 0)]
live[, `:=`(proj = round(proj, 1), proj_sd = round(proj_sd, 1), xv_left = round(xv_left, 1))]

pre <- fread("results/markets/player_summary_2026.csv")
live <- merge(live, pre[, .(player_name, team, pre_win_pct = win_pct, pre_mean = round(sim_mean, 1))],
              by = c("player_name","team"), all.x = TRUE)
live[, win_delta := round(win_pct - fifelse(is.na(pre_win_pct), 0, pre_win_pct), 2)]
setorder(live, -win_pct, -proj)
fwrite(live, "results/markets/live_state.csv")

# Round-by-round grid: what the model expected for that match against what was
# actually read out. A settled match with no vote row is a genuine zero; an
# unsettled one is still unknown, so it stays NA and the page leaves it blank.
grid <- ep[, .(pkey = key, round = match_round, xv = round(ensemble_expected_votes, 2),
               settled = match_id %in% settled)]
grid <- merge(grid, vm[, .(pkey = key, round, votes)], by = c("pkey","round"), all.x = TRUE)
grid[settled & is.na(votes), votes := 0L]
fwrite(grid, "results/markets/live_grid.csv")

meta <- list(updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             status = feed$status, n_sims = N_SIMS,
             matches_read = length(settled), matches_total = uniqueN(ep$match_id),
             votes_read = sum(vm$votes), last_round = max(rounds_done),
             rounds_done = rounds_done, unmatched_votes = sum(lost$votes))
write_json(meta, "results/markets/live_meta.json", auto_unbox = TRUE)
saveRDS(totals, "results/markets/live_totals.rds")

cat("\n== live top 12 (win % from here) ==\n")
print(live[1:12, .(player_name, team, read, games_left, proj, win_pct, pre_win_pct, win_delta)])
cat("\nwrote results/markets/live_state.csv and live_meta.json\n")
