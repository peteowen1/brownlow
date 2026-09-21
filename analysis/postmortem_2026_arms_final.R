# The arm values quoted in POSTMORTEM-2026.md Sec 3's summary table.
#
# The full sweeps live in analysis/postmortem_2026_whatif.R and take about nine
# minutes. This reads their committed output for the parameters those sweeps
# chose, then scores each arm on 2026 in a few seconds, so the summary table's
# figures -- including the combined "+0.00094", which the sweeps print but did
# not previously persist -- have a file behind them.
#
# Every parameter here is READ from the prior-season fit, never chosen on 2026.
# The hindsight optimum is reported alongside only so the two can be compared.
#
# Run: powershell.exe -Command 'Rscript "analysis/postmortem_2026_arms_final.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
PC <- paste0("ensemble_prob_", 0:3)

sw <- fread("analysis/harness/whatif_sharpen_sweep.csv")
pw <- fread("analysis/harness/whatif_position_sweep.csv")
stopifnot("run analysis/postmortem_2026_whatif.R first" = nrow(sw) > 0 && nrow(pw) > 0,
          "sweeps do not cover the prior seasons" =
            uniqueN(sw[season < 2026, season]) >= 10 && uniqueN(pw[season < 2026, season]) >= 10)

BETA_FIT <- sw[season < 2026, .(m = mean(rmse)), by = beta][order(m)]$beta[1]
FAC_FIT  <- pw[season < 2026, .(m = mean(rmse)), by = factor][order(m)]$factor[1]
BETA_HIND <- sw[season == 2026][order(rmse)]$beta[1]
cat(sprintf("fitted on 2015-2025: beta %.1f, defender factor %.1f | hindsight beta %.1f\n",
            BETA_FIT, FAC_FIT, BETA_HIND))

ep <- fread("results/ensemble_probabilities.csv")[season == 2026]
pm <- fread("results/markets/postmortem_2026_rows.csv",
            select = c("player_name","team","round","grp","actual"))
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team"))[season == 2026]
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[, .(player_name, team, date, round = match_round)],
            by = c("player_name","team","date"), all.x = TRUE)
ep <- merge(ep, pm, by = c("player_name","team","round"), all.x = TRUE)
stopifnot("2026 actuals did not attach" = !anyNA(ep$actual), nrow(ep) == 9522,
          "season must carry 1242 votes" = sum(ep$actual) == 1242)

sharpen <- function(d, beta = 1, back_factor = 1) {
  P <- as.matrix(d[, ..PC])
  w <- if (back_factor == 1) 1 else fifelse(d$grp == "Back", back_factor, 1)
  P[, 2:4] <- pmax((P[, 2:4] ^ beta) * w, 1e-12)
  out <- normalize_probability_matrix(P, d$match_id)$normalized_probabilities
  as.numeric(out[, 2] + 2 * out[, 3] + 3 * out[, 4])
}
rmse <- function(p) sqrt(mean((p - ep$actual)^2))
base <- rmse(ep$ensemble_expected_votes)

arms <- rbindlist(list(
  data.table(arm = "as submitted", fitted_on = "-", rmse = base),
  data.table(arm = sprintf("sharpen beta %.1f", BETA_FIT), fitted_on = "2015-2025",
             rmse = rmse(sharpen(ep, beta = BETA_FIT))),
  data.table(arm = sprintf("defender factor %.1f", FAC_FIT), fitted_on = "2015-2025",
             rmse = rmse(sharpen(ep, back_factor = FAC_FIT))),
  data.table(arm = sprintf("both (beta %.1f, factor %.1f)", BETA_FIT, FAC_FIT),
             fitted_on = "2015-2025", rmse = rmse(sharpen(ep, BETA_FIT, FAC_FIT))),
  data.table(arm = sprintf("sharpen beta %.1f", BETA_HIND), fitted_on = "2026 (hindsight)",
             rmse = rmse(sharpen(ep, beta = BETA_HIND)))))
arms[, gain := round(base - rmse, 5)][, rmse := round(rmse, 5)]

cat("\n=== 2026 RMSE by arm. Lower RMSE is better; gain is the improvement on the\n")
cat("    submitted model, and the 0.00055 decidability bar is what it must clear. ===\n")
print(arms)
fwrite(arms, "analysis/harness/whatif_arms_2026.csv")
cat("\nwritten: analysis/harness/whatif_arms_2026.csv\n")
