# Does the vectorised sampler draw from the same distribution as the loop it
# replaced?
#
# simulate_counts_vectorised() claims to be an exact vectorised form of
# simulate_match_votes_pl(): instead of three sequential sample.int() draws it
# gives player i an exponential clock E_i/w_i and takes the three that fire
# first. The equivalence is a standard result, but "standard result" is not
# evidence that THIS implementation got it right -- an off-by-one in the worth
# vector or a transposed matrix would still produce a plausible-looking answer.
#
# So draw one real match many times under both and compare P(3), P(2) and P(1)
# per player. They should agree to Monte Carlo noise: at n = 20,000 the standard
# error of a single proportion is at most 0.0035, so a difference of two
# independent estimates has se <= 0.005 and the largest of ~130 comparisons
# sitting near 2-3 se is expected rather than alarming.
#
# Run: powershell.exe -Command 'Rscript "analysis/validate_vectorised_sampler.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
N <- as.integer(Sys.getenv("VALIDATE_SIMS", "20000"))
TOL <- 0.02   # far above Monte Carlo noise; catches a real implementation bug

ep <- fread("results/ensemble_probabilities.csv")[season == 2026]
one <- ep[match_id == ep$match_id[1]]
prep <- prepare_season_sim(one)
cat(sprintf("match %s: %d players, %d draws per sampler\n",
            one$match_id[1], nrow(one), N))

set.seed(11)
old <- replicate(N, simulate_match_votes_pl(prep$P))
new <- simulate_counts_vectorised(prep, n_sims = N, seed = 12)

tally <- function(v, keys, lbl) {
  data.table(pk = rep(keys, N), v = as.integer(v))[
    , setNames(.(mean(v == 3), mean(v == 2), mean(v == 1)),
               paste0(c("p3_","p2_","p1_"), lbl)), by = pk]
}
cmp <- merge(tally(old, prep$player_key, "old"), tally(new, rownames(new), "new"), by = "pk")
setorder(cmp, -p3_old)
cat("\n=== P(3 / 2 / 1 votes) per player, sequential sampler vs vectorised ===\n")
cat("These should agree to Monte Carlo noise. Columns are probabilities, not counts.\n")
print(head(cmp, 8), digits = 3)

d3 <- max(abs(cmp$p3_old - cmp$p3_new)); d2 <- max(abs(cmp$p2_old - cmp$p2_new))
d1 <- max(abs(cmp$p1_old - cmp$p1_new))
cat(sprintf("\nmax absolute difference: P(3) %.4f | P(2) %.4f | P(1) %.4f  (tolerance %.2f)\n",
            d3, d2, d1, TOL))

# Every draw must allocate exactly 3 + 2 + 1 votes, under both samplers.
stopifnot("sequential sampler did not allocate 6 votes" = all(colSums(old) == 6),
          "vectorised sampler did not allocate 6 votes" = all(colSums(new) == 6),
          "samplers disagree by more than Monte Carlo noise" = max(d3, d2, d1) < TOL)
cat("PASS: the two samplers agree within tolerance and both allocate 6 votes a match.\n")
