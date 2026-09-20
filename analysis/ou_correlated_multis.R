# Player votes over/under lines: singles, pairwise correlation of simulated
# season totals, and the two-leg multis whose joint probability beats the
# product of the singles (positively correlated same-direction legs, or
# negatively correlated opposite-direction legs).
# Reads the sims cached by analysis/price_markets_2026.R and the lines pulled by analysis/sportsbet_pull.R.
suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
L <- fread("results/markets/sportsbet_lines_latest.csv")[grepl(" Votes O/U$", event)]
ou <- L[, .(player = sub(" Votes O/U$", "", event), side = selection, line = handicap, odds)]
V <- readRDS("results/markets/votes_sims_2026_5000.rds")
ep <- fread("results/ensemble_probabilities.csv")[season == 2026]
ep[, date := as.IDate(date)]; setkey(ep, player_name, team, date)   # same order merge() left it in price_markets_2026.R
prep <- prepare_season_sim(ep); keys <- unique(prep$player_key)
R <- uniqueN(ep$match_id) ; R <- 25L   # rounds 0-24
cell_pid <- rep(seq_along(keys), each = R)
totals <- rowsum(V, cell_pid, reorder = TRUE); pname <- sub("_[^_]+$", "", keys)
pl <- unique(ou$player); T <- totals[match(pl, pname), ]; rownames(T) <- pl
stopifnot(!anyNA(T[, 1]))

# singles
ou[, p := mapply(function(n, s, l) if (s == "Over") mean(T[n, ] > l) else mean(T[n, ] < l), player, side, line)]
ou[, `:=`(edge = round(p * odds - 1, 3), kelly = round(pmax((p * odds - 1) / (odds - 1), 0), 4), p = round(p, 4))]
cat("== O/U singles (sim mean in brackets) ==\n")
ou[, sim_mean := round(rowMeans(T)[player], 1)]
print(ou[order(-edge)], nrow = 50)
fwrite(ou, "results/markets/ou_singles_priced.csv")

# correlation of season totals
C <- cor(t(T)); fwrite(as.data.table(round(C, 3), keep.rownames = "player"), "results/markets/ou_player_correlation.csv")
pairs <- as.data.table(as.table(C)); setnames(pairs, c("a", "b", "cor"))
pairs <- pairs[a != b & as.character(a) < as.character(b)][order(-abs(cor))]; pairs[, cor := round(cor, 3)]
cat("\n== strongest correlations of simulated season totals (n = 5000 sims) ==\n"); print(head(pairs, 15))

# two-leg multis: every (leg, leg) combination across different players
legs <- ou[, .(player, side, line, odds, p)]
ind <- function(n, s, l) if (s == "Over") T[n, ] > l else T[n, ] < l
M <- list()
for (i in seq_len(nrow(legs))) for (j in seq_len(nrow(legs))) if (legs$player[i] < legs$player[j]) {
  a <- ind(legs$player[i], legs$side[i], legs$line[i]); b <- ind(legs$player[j], legs$side[j], legs$line[j])
  pj <- mean(a & b); pi <- legs$p[i] * legs$p[j]; odds <- legs$odds[i] * legs$odds[j]
  M[[length(M) + 1]] <- data.table(leg1 = paste(legs$player[i], legs$side[i], legs$line[i]), leg2 = paste(legs$player[j], legs$side[j], legs$line[j]),
    p_joint = pj, p_indep = pi, lift = pj / pi, odds = odds, edge = pj * odds - 1, kelly = pmax((pj * odds - 1) / (odds - 1), 0))
}
M <- rbindlist(M)[order(-edge)]
M[, c("p_joint","p_indep","lift","edge","kelly") := lapply(.SD, round, 3), .SDcols = c("p_joint","p_indep","lift","edge","kelly")]
fwrite(M, "results/markets/ou_multis_priced.csv")
cat("\n== top 2-leg multis by edge (odds 3.50; lift = joint p / independent p, >1 means the legs help each other) ==\n")
print(head(M, 20))
cat("\n== biggest lifts (correlation working hardest) ==\n"); print(head(M[order(-lift)], 10))
