# Head-to-head timing: the per-simulation loop against the vectorised sampler.
#
# R/fast_simulation.R claims simulate_counts_vectorised() is roughly two orders
# of magnitude faster than simulate_seasons_fast(). That claim was originally
# written from an interactive timing that was never saved, so it was not
# reproducible from repo state. This script measures both on the SAME season in
# ONE pass and writes the result, so the number in the docstring has something
# behind it.
#
# Both arms simulate the full 2026 season (207 matches, 9,522 rows) with the
# Plackett-Luce sampler. Only the loop structure differs.
#
# Run: powershell.exe -Command 'Rscript "analysis/benchmark_vectorised_sampler.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
N <- as.integer(Sys.getenv("BENCH_SIMS", "200"))

ep <- fread("results/ensemble_probabilities.csv")[season == 2026]
prep <- prepare_season_sim(ep)
cat(sprintf("2026: %d rows, %d matches, %d simulations per arm\n",
            nrow(ep), length(prep$match_idx), N))

t0 <- Sys.time(); old <- simulate_seasons_fast(prep, n_sims = N, seed = 1, sampler = "pl")
t_old <- as.numeric(Sys.time() - t0, units = "secs")
t0 <- Sys.time(); new <- simulate_counts_vectorised(prep, n_sims = N, seed = 1)
t_new <- as.numeric(Sys.time() - t0, units = "secs")

# Both must allocate exactly 6 votes per match in every simulation, or the
# comparison is between two different things rather than two implementations.
stopifnot("loop arm lost votes" = all(colSums(old) == 6 * length(prep$match_idx)),
          "vectorised arm lost votes" = all(colSums(new) == 6 * length(prep$match_idx)))

res <- data.table(
  n_sims = N, matches = length(prep$match_idx), rows = nrow(ep),
  loop_secs = round(t_old, 2), vectorised_secs = round(t_new, 2),
  speedup = round(t_old / t_new, 1),
  loop_secs_per_5000 = round(t_old / N * 5000, 1),
  vectorised_secs_per_5000 = round(t_new / N * 5000, 1),
  measured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
cat("\n=== timing, same season and sampler, lower is better ===\n")
print(res)
cat(sprintf("\nmean season total agrees between arms (sanity, not a timing check): %.2f vs %.2f\n",
            mean(rowMeans(old)), mean(rowMeans(new))))
fwrite(res, "analysis/harness/vectorised_sampler_timing.csv")
cat("written: analysis/harness/vectorised_sampler_timing.csv\n")
