# Two data defects queued out of POSTMORTEM-2026.md Sec 5.
#
# 1. 207 of our 9,522 rows (2.2%) do not join to the community comparison sheet,
#    so they are dropped for EVERY entrant when the leaderboard is scored. The
#    cause is player-name spelling, not missing predictions. This lists exactly
#    which names differ so the alias table can be completed once rather than
#    discovered one at a time during a live count.
#
# 2. `2025-03-29_Brisbane_Geelong` carries zero votes in the datathon file. A
#    match scored as all-zeros quietly rewards whichever model predicts least,
#    and 2025 is training data, so this is a live contaminant for 2027. The AFL
#    feed is authoritative, so ask it what actually happened.
#
# Run: powershell.exe -Command 'Rscript "analysis/fix_name_joins.R"'

suppressMessages({ library(data.table); library(jsonlite) })
squash <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))

# ---- 1. which names fail to join, and what does the sheet call them? ---------
sh <- fread(sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv",
                    "1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw"), skip = "matchId")   # the header row, found by content: the sheet gained a summary row above it after 2026-09-21
ours <- fread("submissions/Brownlow_Medal_Datathon_2026_Submission_Form_V2_InTheGame.csv")
ABBR <- c("Adelaide"="ADEL","Brisbane"="BL","Carlton"="CARL","Collingwood"="COLL",
          "Essendon"="ESS","Fremantle"="FRE","Gold Coast"="GCFC","Geelong"="GEEL",
          "Greater Western Sydney"="GWS","Hawthorn"="HAW","Melbourne"="MELB",
          "North Melbourne"="NMFC","Port Adelaide"="PORT","Richmond"="RICH","St Kilda"="STK",
          "Sydney"="SYD","Western Bulldogs"="WB","West Coast"="WCE")
ours[, `:=`(sq = squash(paste(player_first_name, player_last_name)),
            tm = unname(ABBR[sub(" Lions$", "", player_team)]), rd = match_round)]
sh[, `:=`(sq = squash(playerName), tm = teamAbbreviation, rd = rd)]
stopifnot("unmapped team in our submission" = !anyNA(ours$tm),
          "row counts should match" = nrow(ours) == nrow(sh))

miss <- ours[!sh, on = .(rd, sq, tm)]
cat(sprintf("=== 1. our rows that do not join to the sheet: %d of %d (%.1f%%) ===\n",
            nrow(miss), nrow(ours), 100 * nrow(miss) / nrow(ours)))
# For each unmatched player, find what the sheet calls someone on the same team
# in the same round that WE do not have -- that is almost always the same person.
orphan <- sh[!ours, on = .(rd, sq, tm)]
byname <- miss[, .(rounds = .N, team = tm[1]), by = .(ours = paste(player_first_name, player_last_name), sq)]
cand <- orphan[, .(sheet_name = playerName[1], sheet_sq = sq[1]), by = .(rd, tm)]
sug <- merge(byname, orphan[, .(sq_sheet = sq, sheet_name = playerName, tm, rd)],
             by.x = "team", by.y = "tm", allow.cartesian = TRUE)
sug <- sug[, .(matched_sheet_name = sheet_name[which.max(
                  mapply(function(a, b) sum(strsplit(a, "")[[1]] %in% strsplit(b, "")[[1]]),
                         sq, sq_sheet))]), by = .(ours, team, rounds)]
cat("\nour spelling -> what the sheet calls them (best match within the same club):\n")
print(sug[order(-rounds)], nrow = 40)
fwrite(sug, "analysis/harness/name_join_mismatches.csv")

# ---- 2. what did the AFL actually award in that 2025 match? ------------------
cat("\n=== 2. the 2025 match with no votes in the datathon file ===\n")
tok <- httr::content(httr::POST("https://api.afl.com.au/cfs/afl/WMCTok"))$token
resp <- httr::GET("https://api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S2025014",
                  httr::add_headers(`x-media-mis-token` = tok),
                  query = list(`_` = as.integer(Sys.time())))
httr::stop_for_status(resp)
feed <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
v25 <- rbindlist(lapply(feed$leaderboard, function(p) {
  rr <- p$roundByRoundVotes; if (!length(rr)) return(NULL)
  data.table(player = paste(p$player$givenName, p$player$surname),
             team = p$team$teamName,
             round = vapply(rr, function(x) as.integer(x$roundNumber), 1L),
             votes = vapply(rr, function(x) as.integer(x$votes), 1L)) }))
cat(sprintf("2025 feed: status %s, %d votes across %d rounds\n",
            feed$status, sum(v25$votes), uniqueN(v25$round)))

ep <- fread("results/ensemble_probabilities.csv")[season == 2025]
gap <- ep[, .(stored = sum(brownlow_votes)), by = match_id][stored != 6]
cat(sprintf("matches in our 2025 data not carrying 6 votes: %d\n", nrow(gap)))
print(gap)
for (m in gap$match_id) {
  teams <- unique(ep[match_id == m, team])
  rr <- unique(ep[match_id == m, .(match_id)])
  r <- ep[match_id == m][1]
  cat(sprintf("\n%s -- teams %s\n", m, paste(teams, collapse = " v ")))
  hit <- v25[team %in% c(teams, paste(teams, "Lions"), "Brisbane Lions", "Geelong Cats") ]
  # the round number is not stored on our side for this table, so match on the
  # two clubs and show every round where both appear with votes
  cand <- hit[, .N, by = round][N >= 2]
  print(hit[round %in% cand$round][order(round, -votes)])
}
cat("\nwritten: analysis/harness/name_join_mismatches.csv\n")
