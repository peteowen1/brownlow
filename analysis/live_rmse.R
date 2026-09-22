# Score every model in the community prediction sheet on the votes read so far.
#
# The sheet (docs.google.com/spreadsheets/d/1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw,
# "2026 Brownlow Predictions", shared by Daniel Wishart) carries one column per
# entrant and an Actual column that is still all zeros, so it cannot score
# itself. The actuals come from the AFL live feed instead, joined on the AFL's
# own match and player ids, which the sheet carries in numeric form.
#
# RMSE is computed over EVERY player-match row belonging to a match that has
# been read out — including the thousands that polled nothing, which is what the
# datathon metric does. Rows from unread matches are excluded entirely rather
# than scored as zero.
#
# Run: powershell.exe -Command 'Rscript "analysis/live_rmse.R"'

suppressMessages({ library(data.table); library(jsonlite); devtools::load_all(quiet = TRUE) })
SHEET <- "1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw"
OURS  <- "submissions/Brownlow_Medal_Datathon_2026_Submission_Form_V2_InTheGame.csv"

# ---- the prediction sheet ----------------------------------------------------
url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv", SHEET)
sh <- fread(url, skip = 3)
stopifnot(nrow(sh) == 9522, all(c("matchId","playerId","playerName","rd","Actual") %in% names(sh)))
models <- setdiff(names(sh), c("matchId","rd","teamAbbreviation","playerId","playerName","gameRank","Actual"))
cat(sprintf("sheet: %d rows, %d models (%s)\n", nrow(sh), length(models), paste(models, collapse = ", ")))

# ---- our own entry, joined by round and name ---------------------------------
ours <- fread(OURS)
# Shared alias table (R/name_aliases.R). The sheet spells players in full
# ("Harrison Petty") where the datathon data uses short forms, which used to
# drop 207 rows -- 2.2% -- from EVERY entrant's score, not just ours.
squash <- squash_name
ABBR <- c("Adelaide"="ADEL","Brisbane Lions"="BL","Carlton"="CARL","Collingwood"="COLL",
          "Essendon"="ESS","Fremantle"="FRE","Gold Coast"="GCFC","Geelong"="GEEL",
          "Greater Western Sydney"="GWS","Hawthorn"="HAW","Melbourne"="MELB",
          "North Melbourne"="NMFC","Port Adelaide"="PORT","Richmond"="RICH","St Kilda"="STK",
          "Sydney"="SYD","Western Bulldogs"="WB","West Coast"="WCE")
ours[, `:=`(sq = squash(paste(player_first_name, player_last_name)), rd = match_round,
            tm = unname(ABBR[player_team]))]
stopifnot("unmapped team in our submission" = !anyNA(ours$tm))
sh[, `:=`(sq = to_our_name(playerName), tm = teamAbbreviation)]
# Team is in the key because two different Bailey Williamses play in the same
# rounds; on name and round alone they collide and both get dropped, which would
# score our entry on 144 fewer rows than everyone else's.
dup <- ours[, .N, by = .(rd, sq, tm)][N > 1]
stopifnot("still ambiguous on round, name and team" = nrow(dup) == 0)
sh <- merge(sh, ours[, .(rd, sq, tm, InTheGame = brownlow_votes_prediction)],
            by = c("rd","sq","tm"), all.x = TRUE)
cat(sprintf("our entry matched %d of %d sheet rows\n", sum(!is.na(sh$InTheGame)), nrow(sh)))
models <- c(models, "InTheGame")

# ---- actuals from the live feed ----------------------------------------------
tok <- httr::content(httr::POST("https://api.afl.com.au/cfs/afl/WMCTok"))$token
resp <- httr::GET("https://api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S2026014",
                  httr::add_headers(`x-media-mis-token` = tok),
                  query = list(`_` = as.integer(Sys.time())))
httr::stop_for_status(resp)
feed <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
polled <- rbindlist(lapply(feed$leaderboard, function(p) {
  rr <- p$roundByRoundVotes; if (!length(rr)) return(NULL)
  data.table(playerId = as.numeric(sub("^CD_I", "", p$player$playerId)),
             matchId  = vapply(rr, function(x) as.numeric(sub("^CD_M", "", x$matchId)), 1),
             votes    = vapply(rr, function(x) as.integer(x$votes), 1L)) }))
read_matches <- unique(polled$matchId)
stopifnot("feed match ids do not line up with the sheet" = all(read_matches %in% sh$matchId))

sh <- merge(sh, polled, by = c("matchId","playerId"), all.x = TRUE)
sh[, actual := fifelse(is.na(votes), 0, as.numeric(votes))]
scored <- sh[matchId %in% read_matches]
stopifnot("a read match does not total 6 votes" =
            all(scored[, .(v = sum(actual)), by = matchId]$v == 6))
cat(sprintf("\nscoring %d matches read out, %d player-match rows, %d votes\n",
            length(read_matches), nrow(scored), sum(scored$actual)))

# ---- RMSE --------------------------------------------------------------------
# Every model is scored on the SAME rows: any row an entrant left blank is
# dropped for everyone, so the ranking compares models rather than coverage.
have <- Reduce(`&`, lapply(models, function(m) !is.na(scored[[m]])))
miss <- vapply(models, function(m) sum(is.na(scored[[m]])), 1L)
if (any(miss > 0)) { cat("\nblank predictions (these rows are excluded for every model):\n")
  print(data.table(model = models[miss > 0], blank = miss[miss > 0])) }
common <- scored[have]
cat(sprintf("\ncommon scoring set: %d of %d rows, %d votes\n",
            nrow(common), nrow(scored), sum(common$actual)))

res <- rbindlist(lapply(models, function(m) {
  p <- common[[m]]
  data.table(model = m,
             rmse = sqrt(mean((p - common$actual)^2)),
             mae  = mean(abs(p - common$actual)),
             pred_votes = sum(p)) }))
setorder(res, rmse)
res[, rank := .I]
cat("\n=== RMSE on the matches read so far. Lower is better; this is the datathon's own metric,\n")
cat("    averaged over every player-match row in those matches, most of which polled nothing. ===\n")
print(res[, .(rank, model, rmse = round(rmse, 5), mae = round(mae, 5))], nrow = 40)
# analysis/harness/ is committed; results/ is gitignored, and this table is the
# leaderboard POSTMORTEM-2026.md Sec 1 quotes.
fwrite(res, "analysis/harness/live_rmse.csv")
cat("\nwritten: analysis/harness/live_rmse.csv\n")
