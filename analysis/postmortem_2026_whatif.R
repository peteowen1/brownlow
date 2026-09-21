# What would the 2026 submission have scored under the two fixes the miss
# analysis pointed at? (analysis/postmortem_2026_misses.R)
#
#   Arm S  sharpen  - concentrate each match's 3/2/1 probability on the model's
#                     own favourite. We picked the best-on-ground as often as
#                     the best entrant did (72.0% vs 72.9%) but staked 2.04
#                     expected votes on him against their 2.30, and 68% of the
#                     RMSE gap sat on 3-vote rows.
#   Arm P  position - damp defenders. We gave backs 161 season votes against a
#                     real 100, and took 58 off the midfielders.
#   Arm SP both, to see whether they are the same effect wearing two hats.
#
# The transform acts on the already-IPF-normalised probability matrix: raise the
# 1/2/3 columns to a power, then re-run IPF so each match still carries exactly
# one 3, one 2 and one 1. At beta = 1 that is the identity, which is asserted
# rather than assumed.
#
# Two readings are reported and they are NOT the same claim:
#   - the 2026 sweep, which is hindsight: the best beta is chosen knowing the
#     answer, so it is a ceiling, not an achievable gain
#   - beta fitted on 2015-2025 and APPLIED to 2026, which is the honest number,
#     with the caveat that those seasons are largely in-sample for the model and
#     an in-sample fit looks over-confident, biasing the fitted beta DOWN. That
#     bias runs against the hypothesis, so a positive result there is real.
#
# Run: powershell.exe -Command 'Rscript "analysis/postmortem_2026_whatif.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })

ep <- fread("results/ensemble_probabilities.csv")
pm <- fread("results/markets/postmortem_2026_rows.csv")   # 2026 actuals from the AFL feed
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team","player_position"))
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[, .(player_name, team, date, match_round, pos = player_position)],
            by = c("player_name","team","date"), all.x = TRUE)
stopifnot(!anyNA(ep$pos))

# 2026's brownlow_votes column is zero-filled by construction, so take the real
# ones from the feed-derived file instead.
ep <- merge(ep, pm[, .(player_name, team, match_round = round, feed_actual = actual)],
            by = c("player_name","team","match_round"), all.x = TRUE)
ep[, actual := fifelse(season == 2026, as.numeric(feed_actual), as.numeric(brownlow_votes))]

GROUP <- c(RK="Ruck", C="Mid", R="Mid", RR="Mid", WL="Mid", WR="Mid",
           FF="Fwd", CHF="Fwd", FPL="Fwd", FPR="Fwd", HFFL="Fwd", HFFR="Fwd",
           FB="Back", CHB="Back", BPL="Back", BPR="Back", HBFL="Back", HBFR="Back",
           INT="Int", EMERG="Int")
ep[, grp := fifelse(is.na(GROUP[pos]), "Int", unname(GROUP[pos]))]

# ---- coverage, before any number is reported ---------------------------------
cov <- ep[, .(rows = .N, matches = uniqueN(match_id), votes = sum(actual),
              ok = sum(actual) == 6 * uniqueN(match_id)), by = season][order(season)]
cat("=== coverage: every season must carry exactly 6 votes per match ===\n")
print(cov)
# 2025 comes up 6 votes short: one match has no votes recorded in the datathon
# file. Drop that match rather than the season, and say which it was — a match
# scored as all-zeros would quietly reward whichever arm predicts least.
bad <- ep[, .(v = sum(actual)), by = .(season, match_id)][v != 6]
if (nrow(bad)) { cat("\nmatches excluded for not carrying 6 votes:\n"); print(bad) }
ep <- ep[season >= 2015 & !match_id %in% bad$match_id]
stopifnot("still not 6 votes a match after the exclusion" =
            all(ep[, .(v = sum(actual)), by = match_id]$v == 6))

# ---- the transform -----------------------------------------------------------
PC <- paste0("ensemble_prob_", 0:3)
sharpen <- function(d, beta = 1, back_factor = 1) {
  P <- as.matrix(d[, ..PC])
  w <- rep(1, nrow(d))
  if (back_factor != 1) w <- fifelse(d$grp == "Back", back_factor, 1)
  P[, 2:4] <- (P[, 2:4] ^ beta) * w
  P[, 2:4] <- pmax(P[, 2:4], 1e-12)
  out <- normalize_probability_matrix(P, d$match_id)$normalized_probabilities
  as.numeric(out[, 2] + 2 * out[, 3] + 3 * out[, 4])
}
rmse <- function(p, a) sqrt(mean((p - a)^2))

# ---- limiting case: beta = 1 and back_factor = 1 must reproduce the baseline --
base26 <- ep[season == 2026]
id26 <- sharpen(base26, beta = 1, back_factor = 1)
cat(sprintf("\nlimiting case, beta = 1: max deviation from the shipped prediction = %.2e\n",
            max(abs(id26 - base26$ensemble_expected_votes))))
stopifnot("beta = 1 is not the identity, so the transform changes more than one thing" =
            max(abs(id26 - base26$ensemble_expected_votes)) < 1e-6)
stopifnot("transform broke the 6-votes-per-match constraint" =
            all(abs(base26[, .(v = sum(id26[.I])), by = match_id]$v - 6) < 1e-6))
cat(sprintf("baseline 2026 RMSE: %.5f\n", rmse(base26$ensemble_expected_votes, base26$actual)))

# ---- arm S: sweep the sharpening power ---------------------------------------
BETAS <- seq(0.8, 2.2, by = 0.1)
sw <- rbindlist(lapply(BETAS, function(b) {
  ep[, .(rmse = rmse(sharpen(.SD, beta = b), actual)), by = season,
     .SDcols = c(PC, "match_id", "grp")][, beta := b] }))
w26 <- sw[season == 2026][order(rmse)]
prior <- sw[season < 2026, .(rmse = mean(rmse)), by = beta][order(rmse)]
cat("\n=== arm S: sharpening power. RMSE on 2026, lower is better ===\n")
print(dcast(sw[season %in% c(2023, 2024, 2025, 2026)], beta ~ season,
            value.var = "rmse")[, lapply(.SD, function(x) round(x, 5))])
cat(sprintf("\n2026 sweep best (hindsight ceiling): beta %.1f -> RMSE %.5f\n",
            w26$beta[1], w26$rmse[1]))
BETA_FIT <- prior$beta[1]
cat(sprintf("beta chosen on 2015-2025 only (honest): %.1f -> 2026 RMSE %.5f\n",
            BETA_FIT, sw[season == 2026 & beta == BETA_FIT, rmse]))

# ---- arm P: damp the defenders -----------------------------------------------
FACTORS <- seq(0.4, 1.2, by = 0.1)
pw <- rbindlist(lapply(FACTORS, function(f) {
  ep[, .(rmse = rmse(sharpen(.SD, back_factor = f), actual)), by = season,
     .SDcols = c(PC, "match_id", "grp")][, factor := f] }))
p26 <- pw[season == 2026][order(rmse)]
pprior <- pw[season < 2026, .(rmse = mean(rmse)), by = factor][order(rmse)]
FAC_FIT <- pprior$factor[1]
cat("\n=== arm P: multiplier on defenders' vote probability ===\n")
print(p26[order(factor), .(factor, rmse_2026 = round(rmse, 5))])
cat(sprintf("2026 sweep best (hindsight): factor %.1f -> RMSE %.5f\n", p26$factor[1], p26$rmse[1]))
cat(sprintf("factor chosen on 2015-2025 only: %.1f -> 2026 RMSE %.5f\n",
            FAC_FIT, pw[season == 2026 & factor == FAC_FIT, rmse]))

# ---- arm SP: both, at the prior-season-fitted settings ------------------------
both <- ep[season == 2026, .(rmse = rmse(sharpen(.SD, beta = BETA_FIT, back_factor = FAC_FIT),
                                         actual)), .SDcols = c(PC, "match_id", "grp")]$rmse
cat(sprintf("\n=== arm SP: both fixes at the prior-season settings -> 2026 RMSE %.5f ===\n", both))

# ---- what it would have meant on the leaderboard ------------------------------
cat("\n=== where each arm would have finished against the community sheet ===\n")
board <- fread("results/markets/live_rmse.csv")[, .(model, rmse)]
arms <- data.table(model = c("ours as submitted",
                             sprintf("ours + sharpen (beta %.1f, fitted pre-2026)", BETA_FIT),
                             sprintf("ours + defender damp (factor %.1f, fitted pre-2026)", FAC_FIT),
                             "ours + both (fitted pre-2026)",
                             sprintf("ours + sharpen (beta %.1f, HINDSIGHT)", w26$beta[1])),
                   rmse = c(rmse(base26$ensemble_expected_votes, base26$actual),
                            sw[season == 2026 & beta == BETA_FIT, rmse],
                            pw[season == 2026 & factor == FAC_FIT, rmse],
                            both, w26$rmse[1]))
cat("NOTE: our rows here are scored on all 9,522 2026 rows, the sheet's on the 9,094 it shares,\n")
cat("so the placings are indicative. Lower RMSE is better.\n")
comb <- rbind(board[!model %in% c("InTheGame")], arms)[order(rmse)][, rank := .I]
print(comb[rank <= 12 | grepl("ours", model), .(rank, model, rmse = round(rmse, 5))], nrow = 30)
fwrite(sw, "results/markets/whatif_sharpen_sweep.csv")
cat("\nwritten: results/markets/whatif_sharpen_sweep.csv\n")
