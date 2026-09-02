# Metrics over the saved backtest output ---------------------------------------
#
# Reads analysis/harness/backtest_raw_*.rds, so adding a metric costs a re-read
# rather than nine model fits and 45,000 simulations.
#
# The question this settles: a log-loss number means nothing without a baseline,
# and the baseline encodes what you think the model was asked to do.
#
#   uniform over the whole field   the model had to find the contenders itself
#   uniform over a known top 20    someone handed it the shortlist for free
#   coaches-votes ranking          a real, model-free competitor
#
# and separately, the CONDITIONAL question: given the 20 players who actually
# finished top 20, how well does the model order them? That renormalises the
# position probabilities within those 20, which is the only version directly
# comparable to -log(1/20) = 3.00.

library(dplyr)
library(readr)
devtools::load_all()

files <- list.files("analysis/harness", pattern = "^backtest_raw_\\d+\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No saved backtest output. Run analysis/simulation_backtest.R first.")
raw <- lapply(files, readRDS)
cat(sprintf("loaded %d seasons: %s\n\n", length(raw),
            paste(vapply(raw, function(r) r$season, numeric(1)), collapse = ", ")))

FLOOR <- 1e-4

# For one season, the -log(p) at each finishing position, both unconditional and
# renormalised within the players who actually finished top N.
season_curve <- function(r) {
  pp  <- r$position_probs
  ord <- r$finish_order
  k   <- min(length(ord), sum(grepl("^p_pos", names(pp))))

  rows_top <- match(ord, pp$player_name)

  vapply(seq_len(k), function(i) {
    col  <- paste0("p_pos", i)
    all_p <- pp[[col]]
    who   <- rows_top[i]

    uncond <- if (is.na(who)) FLOOR else max(all_p[who], FLOOR)

    # renormalise within the players who actually finished top k
    mass <- sum(all_p[rows_top], na.rm = TRUE)
    cond <- if (is.na(who) || mass <= 0) FLOOR else max(all_p[who] / mass, FLOOR)

    c(unconditional = -log(uncond), conditional = -log(cond),
      p_uncond = uncond, p_cond = cond, mass_in_top = mass)
  }, numeric(5)) %>% t() %>% as.data.frame() %>%
    mutate(position = seq_len(k), season = r$season)
}

curves <- bind_rows(lapply(raw, season_curve))
n_pos  <- max(curves$position)

by_pos <- curves %>%
  group_by(position) %>%
  summarise(logloss_uncond = mean(unconditional),
            logloss_cond   = mean(conditional),
            mean_p_uncond  = mean(p_uncond),
            mean_mass_in_top = mean(mass_in_top), .groups = "drop")

cat("=== log-loss by finishing position, averaged over seasons ===\n")
cat("  unconditional : model must pick the player out of the whole field\n")
cat("  conditional   : renormalised within the players who actually finished top 20\n\n")
print(as.data.frame(by_pos), row.names = FALSE, digits = 3)

w <- 1 / seq_len(n_pos)
cat("\n=== summary ===\n")
cat(sprintf("  unconditional, unweighted : %.3f\n", mean(by_pos$logloss_uncond)))
cat(sprintf("  unconditional, weighted   : %.3f\n",
            sum(w * by_pos$logloss_uncond) / sum(w)))
cat(sprintf("  CONDITIONAL,  unweighted  : %.3f\n", mean(by_pos$logloss_cond)))
cat(sprintf("  CONDITIONAL,  weighted    : %.3f\n",
            sum(w * by_pos$logloss_cond) / sum(w)))

field <- vapply(raw, function(r) nrow(r$position_probs), numeric(1))
cat("\n=== baselines ===\n")
cat(sprintf("  uniform over the whole field (%.0f players avg) : %.3f\n",
            mean(field), -log(1 / mean(field))))
cat(sprintf("  uniform over a known shortlist of %d           : %.3f\n", n_pos, -log(1 / n_pos)))
cat("\n  The conditional number above is the one comparable to the shortlist\n")
cat("  baseline: both are told which players finished top 20, and only the\n")
cat("  ordering is being scored.\n")

cat(sprintf("\n=== how much probability mass sits inside the actual top %d? ===\n", n_pos))
cat(sprintf("  position 1  : %.1f%%\n", 100 * by_pos$mean_mass_in_top[1]))
cat(sprintf("  position 10 : %.1f%%\n", 100 * by_pos$mean_mass_in_top[10]))
cat(sprintf("  position %d : %.1f%%\n", n_pos, 100 * by_pos$mean_mass_in_top[n_pos]))
cat("  Low mass means the model expected someone else entirely at that position.\n")

write_csv(by_pos, "analysis/harness/backtest_position_curve.csv")
cat("\nwritten: analysis/harness/backtest_position_curve.csv\n")
