# Does the datathon file's vote history agree with the AFL's own record?
#
# `2025-03-29_Brisbane_Geelong` carries zero votes for all 46 of its players in
# our data, while the AFL feed says Zorko 3, McCluggage 2, Payne 1. That is a
# match scored as all-zeros in training data, which quietly rewards whichever
# model predicts least.
#
# One wrong entry in static reference data predicts siblings, so this audits
# EVERY season the feed covers rather than patching the one match that happened
# to surface. The feed is authoritative; the datathon file is a convenience copy.
#
# Output: analysis/harness/vote_audit_vs_feed.csv, one row per disagreement.
#
# Run: powershell.exe -Command 'Rscript "analysis/audit_votes_vs_feed.R"'

suppressMessages({ library(data.table); library(jsonlite); devtools::load_all(quiet = TRUE) })

TEAM <- c("Adelaide Crows"="Adelaide","Brisbane Lions"="Brisbane","Carlton"="Carlton",
          "Collingwood"="Collingwood","Essendon"="Essendon","Fremantle"="Fremantle",
          "Geelong Cats"="Geelong","Gold Coast SUNS"="Gold Coast",
          "GWS GIANTS"="Greater Western Sydney","Hawthorn"="Hawthorn","Melbourne"="Melbourne",
          "North Melbourne"="North Melbourne","Port Adelaide"="Port Adelaide","Richmond"="Richmond",
          "St Kilda"="St Kilda","Sydney Swans"="Sydney","West Coast Eagles"="West Coast",
          "Western Bulldogs"="Western Bulldogs")

raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("season","match_round","player_first_name","player_last_name",
                        "player_team","brownlow_votes"))
raw[, `:=`(sq = squash_name(paste(player_first_name, player_last_name)),
           team = sub(" Lions$", "", player_team),
           stored = as.numeric(brownlow_votes))]
seasons <- sort(unique(raw$season))
seasons <- seasons[seasons < 2026]          # 2026's column is zero-filled by design
cat(sprintf("auditing seasons %s against the AFL feed\n", paste(range(seasons), collapse = "-")))

tok <- httr::content(httr::POST("https://api.afl.com.au/cfs/afl/WMCTok"))$token
pull <- function(yr) {
  r <- httr::GET(sprintf("https://api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S%d014", yr),
                 httr::add_headers(`x-media-mis-token` = tok),
                 query = list(`_` = as.integer(Sys.time())))
  if (httr::status_code(r) != 200) return(NULL)
  f <- fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
  if (!length(f$leaderboard)) return(NULL)
  rbindlist(lapply(f$leaderboard, function(p) {
    rr <- p$roundByRoundVotes; if (!length(rr)) return(NULL)
    data.table(season = yr, afl_name = paste(p$player$givenName, p$player$surname),
               sq = to_our_name(paste(p$player$givenName, p$player$surname)),
               team = unname(TEAM[p$team$teamName]),
               match_round = vapply(rr, function(x) as.integer(x$roundNumber), 1L),
               feed = as.numeric(vapply(rr, function(x) as.integer(x$votes), 1L))) }))
}
fd <- rbindlist(lapply(seasons, pull))
stopifnot("no seasons came back from the feed" = nrow(fd) > 0)
got <- sort(unique(fd$season))
cat(sprintf("feed returned %d season(s): %s\n", length(got), paste(got, collapse = ", ")))
cat(sprintf("missing from the feed (not audited): %s\n",
            paste(setdiff(seasons, got), collapse = ", ")))

# Both sides are summed by squashed name below, so two different club-mates who
# squash alike would have their votes pooled and could cancel into "agreement".
# live_count.R and live_rmse.R guard their joins the same way.
raw[, player_name := paste(player_first_name, player_last_name)]
clash_ours <- raw[season %in% got, .(n = uniqueN(player_name)), by = .(season, sq, team)][n > 1]
clash_feed <- fd[, .(n = uniqueN(afl_name)), by = .(season, sq, team)][n > 1]
if (nrow(clash_ours) || nrow(clash_feed)) {
  print(rbind(clash_ours[, src := "datathon"], clash_feed[, src := "feed"]))
  stop("two different players share a squashed name within a club-season; the audit would pool their votes")
}
ours <- raw[season %in% got, .(stored = sum(stored)), by = .(season, match_round, sq, team)]
cmp <- merge(ours, fd[, .(feed = sum(feed)), by = .(season, match_round, sq, team)],
             by = c("season","match_round","sq","team"), all = TRUE)
cmp[is.na(stored), stored := 0][is.na(feed), feed := 0]
bad <- cmp[stored != feed][order(season, match_round, -feed)]

cat(sprintf("\n=== %d player-round disagreements across %d season(s) ===\n",
            nrow(bad), length(got)))
cat("stored = what the datathon file says; feed = what the AFL says. Any row here\n")
cat("is a training label that is wrong in one direction or the other.\n")
if (nrow(bad)) {
  print(bad[, .(season, round = match_round, player = sq, team, stored, feed)], nrow = 60)
  cat("\nby season:\n")
  print(bad[, .(rows = .N, votes_missing = sum(feed - stored)), by = season][order(season)])
}
# Per-season vote totals are the coarse check that catches a whole match going
# missing even when individual names fail to join.
tot <- merge(raw[season %in% got, .(stored = sum(stored)), by = season],
             fd[, .(feed = sum(feed)), by = season], by = "season")
cat("\n=== season vote totals, stored against the feed ===\n")
print(tot[, .(season, stored, feed, gap = feed - stored)])
fwrite(bad, "analysis/harness/vote_audit_vs_feed.csv")
cat("\nwritten: analysis/harness/vote_audit_vs_feed.csv\n")
