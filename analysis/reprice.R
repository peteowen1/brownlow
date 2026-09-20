# One command: pull live Sportsbet odds, price every line against the cached
# 2026 sims, rebuild the O/U multis and the artifact page.
#   powershell.exe -Command 'Rscript "analysis/reprice.R"'
# Then publish results/markets/brownlow-vote-tracker.html with the Artifact tool
# (same URL: https://claude.ai/artifact/GdnuLLmYyM5G5zzwNJHJfT).
# ~1 min with the sim cache present; ~4 min if results/markets/votes_sims_2026_5000.rds
# has to be rebuilt.
for (s in c("analysis/sportsbet_pull.R", "analysis/price_markets_2026.R",
            "analysis/ou_correlated_multis.R", "analysis/build_artifact_data.R")) {
  cat("\n==== ", s, " ====\n", sep = ""); t0 <- Sys.time()
  source(s, local = new.env())
  cat(sprintf("---- %s done in %.0fs\n", s, as.numeric(Sys.time() - t0, units = "secs")))
}
