# Where did the 2026 model's error actually sit, and where did we lose to the
# best entrant in the community sheet?
#
# Two parts, deliberately separated:
#   A. our model against the real 2026 votes on ALL 9,522 player-match rows,
#      joined by our own keys so coverage is complete
#   B. our model against `jt` (best real entrant) on the rows the sheet shares,
#      scored in ONE pass on identical rows
#
# Everything is reported as a share of total SQUARED error, because that is what
# RMSE is made of: a pattern that is real but carries 1% of SSE cannot move the
# metric and is a correctness note, not an improvement.
#
# Run: powershell.exe -Command 'Rscript "analysis/postmortem_2026_misses.R"'

suppressMessages({ library(data.table); library(jsonlite) })

# ---- actual votes, straight from the concluded feed ---------------------------
tok <- httr::content(httr::POST("https://api.afl.com.au/cfs/afl/WMCTok"))$token
resp <- httr::GET("https://api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S2026014",
                  httr::add_headers(`x-media-mis-token` = tok),
                  query = list(`_` = as.integer(Sys.time())))
httr::stop_for_status(resp)
feed <- fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
stopifnot("count is not concluded" = feed$status == "CONCLUDED")
polled <- rbindlist(lapply(feed$leaderboard, function(p) {
  rr <- p$roundByRoundVotes; if (!length(rr)) return(NULL)
  data.table(afl_name = paste(p$player$givenName, p$player$surname),
             afl_team = p$team$teamName,
             round = vapply(rr, function(x) as.integer(x$roundNumber), 1L),
             votes = vapply(rr, function(x) as.integer(x$votes), 1L)) }))

# ---- our predictions ----------------------------------------------------------
ep  <- fread("results/ensemble_probabilities.csv")[season == 2026]
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team","player_position"))[season == 2026]
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[, .(player_name, team, date, match_round, pos = player_position)],
            by = c("player_name","team","date"), all.x = TRUE)
stopifnot(nrow(ep) == 9522, !anyNA(ep$match_round))

TEAM <- c("Adelaide Crows"="Adelaide","Brisbane Lions"="Brisbane","Carlton"="Carlton",
          "Collingwood"="Collingwood","Essendon"="Essendon","Fremantle"="Fremantle",
          "Geelong Cats"="Geelong","Gold Coast SUNS"="Gold Coast",
          "GWS GIANTS"="Greater Western Sydney","Hawthorn"="Hawthorn","Melbourne"="Melbourne",
          "North Melbourne"="North Melbourne","Port Adelaide"="Port Adelaide","Richmond"="Richmond",
          "St Kilda"="St Kilda","Sydney Swans"="Sydney","West Coast Eagles"="West Coast",
          "Western Bulldogs"="Western Bulldogs")
squash <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
ALIAS <- c(baileyjwilliams = "baileywilliams", harrisonpetty = "harrypetty",
           matthewcarroll = "mattcarroll")
polled[, `:=`(team = unname(TEAM[afl_team]), sq = squash(afl_name))]
polled[sq %in% names(ALIAS), sq := unname(ALIAS[sq])]
ep[, sq := squash(player_name)]
ep <- merge(ep, polled[, .(sq, team, round, votes)],
            by.x = c("sq","team","match_round"), by.y = c("sq","team","round"), all.x = TRUE)
ep[, actual := fifelse(is.na(votes), 0L, votes)]
ep[, pred := ensemble_expected_votes]
ep[, err := pred - actual]

# ---- anchor checks, written before looking at any of the output ---------------
cat("=== anchor checks ===\n")
cat(sprintf("  every match allocates 6 votes          : %s\n",
            all(ep[, sum(actual), by = match_id]$V1 == 6)))
cat(sprintf("  season total is 207 x 6 = 1242 votes   : %s (%d)\n",
            sum(ep$actual) == 1242, sum(ep$actual)))
cat(sprintf("  our predictions also sum to about 1242 : %.1f\n", sum(ep$pred)))
cat(sprintf("  Daicos polled 47, the winner           : %s\n",
            ep[player_name == "Nick Daicos", sum(actual)] == 47))
cat(sprintf("  no row lost its actual in the join     : %s\n", !anyNA(ep$actual)))
cat(sprintf("\ncoverage: %d rows, %d players, %d matches, rounds %d-%d\n",
            nrow(ep), uniqueN(ep$player_name), uniqueN(ep$match_id),
            min(ep$match_round), max(ep$match_round)))

sse <- sum(ep$err^2)
cat(sprintf("\noverall: RMSE %.5f, MAE %.5f, total squared error %.1f\n",
            sqrt(mean(ep$err^2)), mean(abs(ep$err)), sse))

# ---- A1. where the squared error sits, by what actually happened --------------
a1 <- ep[, .(rows = .N, mean_pred = mean(pred), mean_signed_err = mean(err),
             sse = sum(err^2)), by = .(actual)][order(actual)]
a1[, share_of_sse := round(100 * sse / sum(sse), 1)]
cat("\n=== A1. squared error split by what the player ACTUALLY polled ===\n")
cat("rows = player-matches; mean_pred = our average prediction on them;\n")
cat("mean_signed_err = predicted minus actual, so negative means we under-predicted;\n")
cat("share_of_sse = percentage of all squared error, which is what RMSE is built from.\n")
print(a1[, .(actual, rows, mean_pred = round(mean_pred, 3),
             mean_signed_err = round(mean_signed_err, 3), share_of_sse)])

# ---- A2. calibration: when we said X, what happened? --------------------------
ep[, bucket := cut(pred, breaks = c(-Inf, .01, .05, .1, .25, .5, 1, 1.5, 2, Inf),
                   labels = c("<0.01","0.01-0.05","0.05-0.10","0.10-0.25","0.25-0.50",
                              "0.50-1.00","1.00-1.50","1.50-2.00",">2.00"))]
a2 <- ep[, .(rows = .N, mean_pred = mean(pred), mean_actual = mean(actual),
             sse = sum(err^2)), by = bucket][order(bucket)]
a2[, `:=`(ratio = round(mean_pred / pmax(mean_actual, 1e-9), 2),
          share_of_sse = round(100 * sse / sum(sse), 1))]
cat("\n=== A2. calibration by predicted-vote bucket ===\n")
cat("mean_actual is what those rows really averaged. ratio = predicted / actual:\n")
cat("1.00 is perfect, above 1 means we over-predicted that band, below means under.\n")
print(a2[, .(bucket, rows, mean_pred = round(mean_pred, 3),
             mean_actual = round(mean_actual, 3), ratio, share_of_sse)])

# ---- A3. by round -------------------------------------------------------------
a3 <- ep[, .(rows = .N, rmse = sqrt(mean(err^2)), sse = sum(err^2)), by = match_round]
a3[, share_of_sse := round(100 * sse / sum(sse), 1)]
setorder(a3, -sse)
cat("\n=== A3. the five rounds we did worst on (by total squared error) ===\n")
cat("rmse is within that round only; lower is better.\n")
print(head(a3[, .(round = match_round, rows, rmse = round(rmse, 4), share_of_sse)], 5))
cat(sprintf("best round: R%d at RMSE %.4f | spread across rounds: %.4f to %.4f\n",
            a3[which.min(rmse), match_round], min(a3$rmse), min(a3$rmse), max(a3$rmse)))

# ---- A4. biggest player-level misses ------------------------------------------
a4 <- ep[, .(games = .N, pred = sum(pred), actual = sum(actual), sse = sum(err^2)),
         by = .(player_name, team)]
a4[, `:=`(diff = pred - actual, share_of_sse = round(100 * sse / sse_total, 2)),
   env = list(sse_total = sse)]
cat("\n=== A4. the ten players we most UNDER-predicted over the season ===\n")
cat("diff = our season total minus their real total; negative means we had them too low.\n")
print(head(a4[order(diff), .(player_name, team, games, pred = round(pred, 1), actual,
                             diff = round(diff, 1), share_of_sse)], 10))
cat("\n=== A4b. the ten players we most OVER-predicted ===\n")
print(head(a4[order(-diff), .(player_name, team, games, pred = round(pred, 1), actual,
                              diff = round(diff, 1), share_of_sse)], 10))
cat(sprintf("\ntop 20 players by squared error carry %.1f%% of all of it\n",
            100 * sum(head(a4[order(-sse)]$sse, 20)) / sse))

# ---- A5. by position ----------------------------------------------------------
GROUP <- c(RK="Ruck", C="Mid", R="Mid", RR="Mid", WL="Mid", WR="Mid",
           FF="Fwd", CHF="Fwd", FPL="Fwd", FPR="Fwd", HFFL="Fwd", HFFR="Fwd",
           FB="Back", CHB="Back", BPL="Back", BPR="Back", HBFL="Back", HBFR="Back",
           INT="Int", EMERG="Int")
ep[, grp := fifelse(is.na(GROUP[pos]), "Int", unname(GROUP[pos]))]
a5 <- ep[, .(rows = .N, pred = sum(pred), actual = sum(actual), sse = sum(err^2)), by = grp]
a5[, `:=`(diff = round(pred - actual, 1), share_of_sse = round(100 * sse / sum(sse), 1))]
cat("\n=== A5. by position group: are we systematically high or low anywhere? ===\n")
cat("diff = our predicted season votes minus real ones for that group; 0 would be perfect.\n")
print(a5[order(-sse), .(grp, rows, pred = round(pred, 0), actual, diff, share_of_sse)])

# ---- B. head-to-head with the best real entrant -------------------------------
sh <- fread(sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv",
                    "1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw"), skip = 3)
ABBR <- c("Adelaide"="ADEL","Brisbane"="BL","Carlton"="CARL","Collingwood"="COLL",
          "Essendon"="ESS","Fremantle"="FRE","Gold Coast"="GCFC","Geelong"="GEEL",
          "Greater Western Sydney"="GWS","Hawthorn"="HAW","Melbourne"="MELB",
          "North Melbourne"="NMFC","Port Adelaide"="PORT","Richmond"="RICH","St Kilda"="STK",
          "Sydney"="SYD","Western Bulldogs"="WB","West Coast"="WCE")
sh[, `:=`(sq = squash(playerName), tm = teamAbbreviation)]
ep[, tm := unname(ABBR[team])]
stopifnot("unmapped team" = !anyNA(ep$tm))
h2h <- merge(sh[, .(rd, sq, tm, jt, consensus)],
             ep[, .(rd = match_round, sq, tm, player_name, ours = pred, actual, grp)],
             by = c("rd","sq","tm"))
h2h <- h2h[!is.na(jt) & !is.na(ours)]
cat(sprintf("\n=== B. head-to-head with jt, the best real entrant ===\ncommon rows: %d of %d (%d votes)\n",
            nrow(h2h), nrow(ep), sum(h2h$actual)))
cat(sprintf("ours RMSE %.5f | jt RMSE %.5f | consensus RMSE %.5f\n",
            sqrt(mean((h2h$ours - h2h$actual)^2)), sqrt(mean((h2h$jt - h2h$actual)^2)),
            sqrt(mean((h2h$consensus - h2h$actual)^2))))
h2h[, `:=`(e_ours = (ours - actual)^2, e_jt = (jt - actual)^2)]
gap <- sum(h2h$e_ours) - sum(h2h$e_jt)
b1 <- h2h[, .(rows = .N, ours = sum(e_ours), jt = sum(e_jt)), by = actual][order(actual)]
b1[, `:=`(gap = ours - jt, share_of_gap = round(100 * (ours - jt) / gap, 1))]
cat("\n--- B1. where the gap to jt comes from, split by what was actually polled ---\n")
cat("gap = our squared error minus jt's on those rows; positive means jt beat us there.\n")
print(b1[, .(actual, rows, ours = round(ours, 1), jt = round(jt, 1),
             gap = round(gap, 1), share_of_gap)])
b2 <- h2h[, .(games = .N, ours = sum(e_ours), jt = sum(e_jt), actual = sum(actual)),
          by = player_name]
b2[, gap := ours - jt]
cat("\n--- B2. the ten players where jt beat us by the most ---\n")
print(head(b2[order(-gap), .(player_name, games, actual, ours = round(ours, 2),
                             jt = round(jt, 2), gap = round(gap, 2))], 10))
cat("\n--- B3. the ten players where we beat jt by the most ---\n")
print(head(b2[order(gap), .(player_name, games, actual, ours = round(ours, 2),
                            jt = round(jt, 2), gap = round(gap, 2))], 10))
cat(sprintf("\ntotal gap %.1f squared-error units; the ten worst players above carry %.1f%% of it\n",
            gap, 100 * sum(head(b2[order(-gap)]$gap, 10)) / gap))

fwrite(ep[, .(player_name, team, pos, grp, round = match_round, pred, actual, err)],
       "results/markets/postmortem_2026_rows.csv")
cat("\nwritten: results/markets/postmortem_2026_rows.csv\n")
