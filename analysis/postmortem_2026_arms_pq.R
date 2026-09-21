# Arm P (damp the defenders) and the cheap screen for the stale-player-effect
# theory. Companion to analysis/postmortem_2026_whatif.R, which did arm S.
#
# Scoped to 2023-2026 rather than 2015-2026: the earlier seasons have a
# different match count and, more to the point, the machine is short of memory
# and the recent seasons are the ones whose optimum transfers.
#
# The screen follows the "cheap in-sample check before the expensive refit"
# rule. The stale-player-effect story (Clayton Oliver 10.5 predicted against 23
# actual, Murphy Reid 7.7 against 15) is only ACTIONABLE if something knowable
# BEFORE the season predicts the residual. Two candidates that are:
#   age          - young players improve on their career prior
#   changed_club - the player effect is fitted on their form at a different club
# If neither carries signal, the story is real but worthless and the GAM refit
# gets skipped.
#
# Run: powershell.exe -Command 'Rscript "analysis/postmortem_2026_arms_pq.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
PC <- paste0("ensemble_prob_", 0:3)

ep <- fread("results/ensemble_probabilities.csv",
            select = c("player_name","team","date","match_id","season","brownlow_votes", PC,
                       "ensemble_expected_votes"))[season >= 2023]
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","player_first_name","player_last_name",
                        "player_team","player_position","player_age"))
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[season >= 2023, .(player_name, team, date, pos = player_position,
                                      age = player_age)],
            by = c("player_name","team","date"), all.x = TRUE)
pm <- fread("results/markets/postmortem_2026_rows.csv",
            select = c("player_name","team","round","actual"))
ep <- merge(ep, raw[season == 2026, .(player_name, team, date)][, .SD], by = c("player_name","team","date"),
            all.x = TRUE, suffixes = c("", ".y"))
# 2026's stored votes are zero-filled, so take the feed actuals via player+team+round
rd <- unique(raw[season == 2026, .(player_name, team, date)])
ep[, actual := as.numeric(brownlow_votes)]
mm <- fread("results/markets/postmortem_2026_rows.csv",
            select = c("player_name","team","round","actual"))
key26 <- unique(raw[season == 2026, .(player_name, team, date)])
r26 <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name","player_last_name","player_team"))[season == 2026]
r26[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
r26 <- merge(r26[, .(player_name, team, date, round = match_round)], mm,
             by = c("player_name","team","round"))
ep <- merge(ep, r26[, .(player_name, team, date, feed = actual)],
            by = c("player_name","team","date"), all.x = TRUE)
ep[season == 2026, actual := feed]
stopifnot("2026 actuals did not attach" = !anyNA(ep[season == 2026, actual]))

GROUP <- c(RK="Ruck", C="Mid", R="Mid", RR="Mid", WL="Mid", WR="Mid",
           FF="Fwd", CHF="Fwd", FPL="Fwd", FPR="Fwd", HFFL="Fwd", HFFR="Fwd",
           FB="Back", CHB="Back", BPL="Back", BPR="Back", HBFL="Back", HBFR="Back",
           INT="Int", EMERG="Int")
ep[, grp := fifelse(is.na(GROUP[pos]), "Int", unname(GROUP[pos]))]
bad <- ep[, .(v = sum(actual)), by = match_id][v != 6]
ep <- ep[!match_id %in% bad$match_id]
cat(sprintf("=== coverage: %d rows, seasons %s, %d matches (excluded %d not carrying 6 votes) ===\n",
            nrow(ep), paste(sort(unique(ep$season)), collapse = "/"), uniqueN(ep$match_id), nrow(bad)))

sharpen <- function(d, beta = 1, back_factor = 1) {
  P <- as.matrix(d[, ..PC])
  w <- if (back_factor == 1) 1 else fifelse(d$grp == "Back", back_factor, 1)
  P[, 2:4] <- pmax((P[, 2:4] ^ beta) * w, 1e-12)
  out <- normalize_probability_matrix(P, d$match_id)$normalized_probabilities
  as.numeric(out[, 2] + 2 * out[, 3] + 3 * out[, 4])
}
rmse <- function(p, a) sqrt(mean((p - a)^2))
base <- ep[season == 2026]
cat(sprintf("baseline 2026 RMSE: %.5f\n", rmse(base$ensemble_expected_votes, base$actual)))

# ---- arm P -------------------------------------------------------------------
FACTORS <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1)
pw <- rbindlist(lapply(FACTORS, function(f) {
  ep[, .(rmse = rmse(sharpen(.SD, back_factor = f), actual)), by = season,
     .SDcols = c(PC, "match_id", "grp")][, factor := f] }))
cat("\n=== arm P: multiplier on defenders' 1/2/3-vote probability, then re-IPF ===\n")
cat("RMSE by season, lower is better; factor 1.0 is the model as shipped.\n")
print(dcast(pw, factor ~ season, value.var = "rmse")[, lapply(.SD, function(x) round(x, 5))])
fit_f <- pw[season < 2026, .(rmse = mean(rmse)), by = factor][order(rmse)]$factor[1]
best_f <- pw[season == 2026][order(rmse)]$factor[1]
cat(sprintf("\nfactor fitted on 2023-2025: %.1f -> 2026 RMSE %.5f (baseline %.5f)\n",
            fit_f, pw[season == 2026 & factor == fit_f, rmse],
            pw[season == 2026 & factor == 1, rmse]))
cat(sprintf("factor best on 2026 (hindsight): %.1f -> RMSE %.5f\n",
            best_f, pw[season == 2026][order(rmse)]$rmse[1]))

# ---- arm SP: both together, at settings fitted on prior seasons ---------------
BETA_FIT <- 1.3   # from the 2023-2025 optimum in postmortem_2026_whatif.R
cat(sprintf("\n=== arm SP: sharpen beta %.1f + defender factor %.1f ===\n", BETA_FIT, fit_f))
cat(sprintf("2026 RMSE %.5f\n",
            rmse(sharpen(base[, .SD, .SDcols = c(PC,"match_id","grp")], BETA_FIT, fit_f), base$actual)))

# ---- the cheap screen for the stale-player-effect theory ---------------------
p26 <- ep[season == 2026]
prior <- ep[season == 2025, .(prior_team = team[1]), by = player_name]
p26 <- merge(p26, prior, by = "player_name", all.x = TRUE)
p26[, `:=`(resid = actual - ensemble_expected_votes,
           changed_club = !is.na(prior_team) & prior_team != team,
           newcomer = is.na(prior_team))]
cat("\n=== screen 1: residual by age band ===\n")
cat("resid = actual minus predicted, averaged per player-match. Positive means the\n")
cat("model had them TOO LOW. n is player-matches. A stale career prior would show up\n")
cat("as a positive residual for the young and a negative one for the old.\n")
p26[, band := cut(age, c(0, 22, 25, 28, 31, 99),
                  labels = c("<=22","23-25","26-28","29-31","32+"))]
print(p26[, .(n = .N, mean_pred = round(mean(ensemble_expected_votes), 4),
              mean_actual = round(mean(actual), 4),
              mean_resid = round(mean(resid), 4)), by = band][order(band)])
cat("\n=== screen 2: residual by whether the player changed club since 2025 ===\n")
print(p26[, .(n = .N, mean_pred = round(mean(ensemble_expected_votes), 4),
              mean_actual = round(mean(actual), 4),
              mean_resid = round(mean(resid), 4)),
          by = .(status = fifelse(newcomer, "new to the data",
                          fifelse(changed_club, "changed club", "same club")))])

cat("\n=== screen 3: does either candidate add anything over the prediction itself? ===\n")
cat("adj-R2 of actual ~ prediction, then with each candidate added; partial correlation\n")
cat("is the candidate's own correlation with the target once the prediction is held fixed.\n")
m0 <- lm(actual ~ ensemble_expected_votes, data = p26)
scr <- rbindlist(lapply(c("age","changed_club","newcomer"), function(v) {
  m1 <- lm(as.formula(paste("actual ~ ensemble_expected_votes +", v)), data = p26)
  pc <- cor(residuals(m0), residuals(lm(as.formula(paste(v, "~ ensemble_expected_votes")), data = p26)))
  data.table(candidate = v, adj_r2_base = summary(m0)$adj.r.squared,
             adj_r2_with = summary(m1)$adj.r.squared, partial_cor = pc) }))
scr[, `:=`(delta_adj_r2 = adj_r2_with - adj_r2_base)]
print(scr[, .(candidate, adj_r2_base = round(adj_r2_base, 5),
              adj_r2_with = round(adj_r2_with, 5), delta_adj_r2 = round(delta_adj_r2, 6),
              partial_cor = round(partial_cor, 4))])
cat(sprintf("\nfor scale: an effect of spread s removes about s^2/(2*sigma^2) of RMSE, sigma = %.4f\n",
            rmse(base$ensemble_expected_votes, base$actual)))
fwrite(pw, "results/markets/whatif_position_sweep.csv")
cat("written: results/markets/whatif_position_sweep.csv\n")
