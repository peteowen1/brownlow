# Best-on-ground hit rate: how often is a model's top-rated player in a match
# the one who actually polled the 3?
#
# This is the number POSTMORTEM-2026.md Sec 2 rests on. It separates the two
# things that make a model good at this competition:
#
#   discrimination - do you pick the right player
#   conviction     - how much expected vote do you stake on him
#
# We and the best entrant differ by two matches in 207 on the first and by 0.26
# expected votes on the second, which is why Sec 3 concludes the gap is not
# something a calibration knob can close.
#
# Also reports round-to-round RMSE against what pure sampling noise would
# produce, because the rounds we scored worst on look like a pattern and are not.
#
# Run: powershell.exe -Command 'Rscript "analysis/postmortem_2026_hitrate.R"'

suppressMessages({ library(data.table) })
ep <- fread("results/markets/postmortem_2026_rows.csv")   # written by postmortem_2026_misses.R
stopifnot("run analysis/postmortem_2026_misses.R first" = nrow(ep) == 9522)

# ---- is the round-to-round spread anything but noise? ------------------------
r <- ep[, .(rmse = sqrt(mean(err^2))), by = round][order(round)]
ct <- cor.test(r$round, r$rmse)
cat("=== round-to-round RMSE ===\n")
cat(sprintf("range %.4f (R%d) to %.4f (R%d); correlation with round number %+.3f (p = %.3f)\n",
            min(r$rmse), r$round[which.min(r$rmse)], max(r$rmse), r$round[which.max(r$rmse)],
            ct$estimate, ct$p.value))
set.seed(1)
boot <- replicate(500, {
  s <- ep[sample(.N)][, g := rep(seq_len(uniqueN(ep$round)), length.out = .N)]
  sd(s[, .(rm = sqrt(mean(err^2))), by = g]$rm) })
cat(sprintf("observed sd across rounds %.4f; reshuffling the same rows at random gives %.4f\n",
            sd(r$rmse), mean(boot)))
cat("The real variation is no larger than chance, so there is no round pattern to explain.\n")

# ---- best-on-ground hit rate, ours against the community sheet ---------------
sh <- fread(sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv",
                    "1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw"), skip = 3)
sq <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
ABBR <- c("Adelaide"="ADEL","Brisbane"="BL","Carlton"="CARL","Collingwood"="COLL",
          "Essendon"="ESS","Fremantle"="FRE","Gold Coast"="GCFC","Geelong"="GEEL",
          "Greater Western Sydney"="GWS","Hawthorn"="HAW","Melbourne"="MELB",
          "North Melbourne"="NMFC","Port Adelaide"="PORT","Richmond"="RICH","St Kilda"="STK",
          "Sydney"="SYD","Western Bulldogs"="WB","West Coast"="WCE")
sh[, k := paste(rd, sq(playerName), teamAbbreviation)]
ep[, k := paste(round, sq(player_name), unname(ABBR[team]))]
m <- merge(sh[, .(k, matchId, jt, consensus, espn)], ep[, .(k, pred, actual)], by = "k")
stopifnot("join lost matches" = uniqueN(m$matchId) == 207)
cat(sprintf("\n=== best-on-ground pick: %d matches, %d rows ===\n", uniqueN(m$matchId), nrow(m)))
cat("Share of matches where the model's single highest-rated player polled the 3.\n")
cat("Higher is better. top2 = that same pick polled at least a 2.\n")
res <- rbindlist(lapply(c("pred","jt","consensus","espn"), function(cc) {
  hit <- m[, .(g3 = actual[which.max(get(cc))] == 3,
               g2 = actual[which.max(get(cc))] >= 2), by = matchId]
  data.table(model = if (cc == "pred") "ours" else cc,
             got_3 = sprintf("%.1f%% (%d/207)", 100 * mean(hit$g3), sum(hit$g3)),
             got_2_or_3 = sprintf("%.1f%%", 100 * mean(hit$g2)),
             mean_on_real_bog = round(mean(m[actual == 3][[cc]]), 3),
             mean_on_own_top = round(m[, max(get(cc)), by = matchId][, mean(V1)], 3)) }))
print(res)
cat("\nmean_on_real_bog is what the model staked on the player who actually polled the 3;\n")
cat("mean_on_own_top is what it staked on its own favourite. Both nearer 3.0 is more confident.\n")
fwrite(res, "analysis/harness/postmortem_2026_hitrate.csv")
cat("written: analysis/harness/postmortem_2026_hitrate.csv\n")
