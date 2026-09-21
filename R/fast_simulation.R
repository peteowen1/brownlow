# Fast season simulation -------------------------------------------------------
#
# simulate_complete_season() calls `sim_data %>% filter(match_id == match)` inside
# its per-match loop, so every simulation rescans the whole 9,522-row frame 207
# times. Measured: 2.778 s per season simulation, which makes a 2,000-sim
# ten-season backtest a 15.4-hour job and is why the pipeline settles for 2,000
# sims rather than enough to resolve a longshot.
#
# Hoisting the split out of the loop and indexing a plain matrix instead gives
# 0.028 s per season — 99x — and turns the same backtest into ten minutes. That
# also makes 20,000+ simulations affordable, which matters: at 2,000 sims
# anything below about 0.05% probability rounds to zero, so genuine outsiders
# cannot be priced at all.

#' Prepare a Season for Repeated Simulation
#'
#' Does the per-season work once — extracting the probability matrix and
#' splitting row indices by match — so the simulation loop only samples.
#'
#' @param sim_data Data frame with `match_id`, `player_name`, `team` and four
#'   probability columns
#' @param prob_prefix Prefix of the probability columns
#'
#' @return List used by `simulate_seasons_fast()`
#' @export
prepare_season_sim <- function(sim_data, prob_prefix = "ensemble_prob_") {
  prob_cols <- paste0(prob_prefix, 0:3)
  missing <- setdiff(prob_cols, names(sim_data))
  if (length(missing) > 0) {
    stop(sprintf("Missing probability columns: %s", paste(missing, collapse = ", ")))
  }
  if (anyNA(sim_data$match_id)) stop("match_id must never be missing.")

  P <- as.matrix(sim_data[, prob_cols])
  P <- pmax(P, 0)
  P <- P / rowSums(P)

  list(
    P          = P,
    match_idx  = split(seq_len(nrow(sim_data)), sim_data$match_id),
    players    = sim_data$player_name,
    teams      = sim_data$team,
    player_key = paste(sim_data$player_name, sim_data$team, sep = "_")
  )
}

#' Simulate a Season Many Times
#'
#' @param prepared Output of `prepare_season_sim()`
#' @param n_sims Number of season simulations
#' @param seed Base RNG seed
#' @param sampler "legacy" for the existing marginal-sum allocation, or "pl" for
#'   the sequential Plackett-Luce draw. Kept switchable so the two can be scored
#'   against the same backtest rather than swapped on argument.
#' @param beta Sharpness for the Plackett-Luce worth vector; ignored for legacy
#'
#' @return Matrix of season vote totals, one row per unique player, one column
#'   per simulation
#' @export
simulate_seasons_fast <- function(prepared, n_sims = 2000, seed = 1,
                                  sampler = c("legacy", "pl"), beta = 1) {
  sampler <- match.arg(sampler)
  set.seed(seed)
  keys <- unique(prepared$player_key)
  key_id <- match(prepared$player_key, keys)

  totals <- matrix(0, nrow = length(keys), ncol = n_sims)
  P <- prepared$P

  for (s in seq_len(n_sims)) {
    votes <- numeric(nrow(P))
    for (ii in prepared$match_idx) {
      votes[ii] <- if (sampler == "pl") {
        simulate_match_votes_pl(P[ii, , drop = FALSE], beta = beta)
      } else {
        simulate_match_votes(P[ii, , drop = FALSE], method = "expected_values")
      }
    }
    totals[, s] <- rowsum(votes, key_id, reorder = FALSE)[order(unique(key_id)), ]
  }

  rownames(totals) <- keys
  totals
}

#' Simulate Every Count at Once, One Match at a Time
#'
#' `simulate_seasons_fast()` loops over simulations and, inside that, over
#' matches — so a 5,000-count season is 5,000 x 207 calls to
#' `simulate_match_votes_pl()`, each doing three `sample.int()` draws.
#'
#' The loop over simulations is unnecessary. Plackett-Luce has an exact
#' vectorised form: give player i an exponential clock E_i / w_i and the order
#' in which the clocks fire is a Plackett-Luce ordering of the field. So one
#' match's 5,000 counts are one `rexp()` matrix and three `max.col()` calls, and
#' the whole season becomes a loop over matches only — about 600 vectorised
#' operations instead of a million scalar ones.
#'
#' Measured head-to-head on the 2026 season by
#' `analysis/benchmark_vectorised_sampler.R`, written to
#' `analysis/harness/vectorised_sampler_timing.csv`: **37x** — 7.6 s against
#' 0.2 s for 200 counts, which scales to roughly 190 s against 5 s for the
#' 5,000 a live re-simulation needs. An earlier draft of this comment claimed
#' ~100x from an interactive timing that was never saved; it was wrong by a
#' factor of three, which is why the benchmark is now committed.
#'
#' `analysis/validate_vectorised_sampler.R` checks the two samplers agree on
#' P(3), P(2) and P(1) per player over 20,000 draws of a real match.
#'
#' @param prepared Output of `prepare_season_sim()`
#' @param n_sims Number of season simulations
#' @param seed RNG seed
#' @param beta Sharpness applied to the Plackett-Luce worth vector
#'
#' @return Integer matrix of season vote totals, one row per unique player
#'   (named), one column per simulation
#' @export
simulate_counts_vectorised <- function(prepared, n_sims = 5000, seed = 1, beta = 1) {
  set.seed(seed)
  keys <- unique(prepared$player_key)
  gid <- match(prepared$player_key, keys)
  worth <- pmax(prepared$P[, 4], 1e-12)^beta

  nk <- length(keys)
  totals <- matrix(0L, nk, n_sims, dimnames = list(keys, NULL))
  col_base <- (seq_len(n_sims) - 1L) * nk
  rows <- seq_len(n_sims)

  for (ii in prepared$match_idx) {
    n <- length(ii)
    if (n < 3) stop(sprintf("Match has %d players; need at least 3 to allocate 3-2-1.", n))
    clock <- matrix(stats::rexp(n_sims * n), n_sims, n) / rep(worth[ii], each = n_sims)
    g <- gid[ii]
    for (v in c(3L, 2L, 1L)) {
      pick <- max.col(-clock, ties.method = "first")
      clock[cbind(rows, pick)] <- Inf
      at <- col_base + g[pick]
      totals[at] <- totals[at] + v
    }
  }
  totals
}

#' Draw One Match's 3-2-1 Votes by Sequential Plackett-Luce
#'
#' The existing `simulate_match_votes()` allocates the 3 votes by sampling from
#' P(3 votes), then allocates the 2 by sampling from `P(2) + P(3)` among the
#' remaining players, then the 1 from `P(1) + P(2) + P(3)`. Those sums are a
#' heuristic, not a conditional distribution, and they mis-price the favourite:
#' measured on a real match, the favourite's P(2) comes out 0.135 against an
#' input marginal of 0.172, a 21% shortfall, while the field gains 5%. Small per
#' match, but it compounds across 22 rounds and runs against the favourite.
#'
#' Plackett-Luce is the correct generative model for this. Each player has a
#' worth; the best is drawn with probability proportional to worth, the second is
#' drawn the same way from the remaining field, and so on. Worth comes from the
#' model's own P(3 votes) — its estimate of who was best on the ground.
#'
#' Note this is the opposite conclusion to Plackett-Luce as a *predictor*, where
#' it lost because it discarded the multinomial's separately fitted P(1) and P(2)
#' heads. As a *generative* model it is exactly right, because here we genuinely
#' are drawing three placings from a field.
#'
#' @param match_probs n x 4 matrix of P(0), P(1), P(2), P(3) for one match
#' @param beta Sharpness applied to the worth vector
#'
#' @return Numeric vector of votes, one per player, summing to 6
#' @export
simulate_match_votes_pl <- function(match_probs, beta = 1) {
  n <- nrow(match_probs)
  if (n < 3) stop(sprintf("Match has %d players; need at least 3 to allocate 3-2-1.", n))

  worth <- pmax(match_probs[, 4], 1e-12)^beta
  votes <- numeric(n)
  available <- seq_len(n)

  for (v in c(3, 2, 1)) {
    w <- worth[available]
    if (sum(w) <= 0) w <- rep(1, length(w))
    pick <- available[sample.int(length(available), 1, prob = w)]
    votes[pick] <- v
    available <- setdiff(available, pick)
  }
  votes
}

#' Finishing-Position Probabilities from Simulated Season Totals
#'
#' Evaluating only the winner gives one data point per season. Extending to the
#' top `max_pos` finishers gives `max_pos` per season, which at nine seasons is
#' the difference between 9 evaluation points and 90.
#'
#' Ties use `ties.method = "min"`, matching how the AFL reports an equal fourth:
#' two players tied on votes both hold that position and the next is skipped. So
#' the column sums are not exactly 1 — that is a property of the sport, not an
#' error — and the same convention is applied to the actual results so prediction
#' and outcome are defined identically.
#'
#' @param totals Output of `simulate_seasons_fast()`
#' @param max_pos Highest finishing position to track
#'
#' @return Data frame with `player_name`, `team` and `p_pos1` … `p_pos<max_pos>`
#' @export
position_probabilities_from_totals <- function(totals, max_pos = 10) {
  n_sims <- ncol(totals)
  counts <- matrix(0, nrow = nrow(totals), ncol = max_pos)

  for (s in seq_len(n_sims)) {
    r <- rank(-totals[, s], ties.method = "min")
    in_range <- which(r <= max_pos)
    for (i in in_range) counts[i, r[i]] <- counts[i, r[i]] + 1
  }

  parts <- strsplit(rownames(totals), "_", fixed = TRUE)
  out <- data.frame(
    player_name = vapply(parts, function(p) paste(p[-length(p)], collapse = "_"), character(1)),
    team        = vapply(parts, function(p) p[length(p)], character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  probs <- as.data.frame(counts / n_sims)
  names(probs) <- paste0("p_pos", seq_len(max_pos))
  cbind(out, probs)
}

#' Weighted Log-Loss Over the Top Finishing Positions
#'
#' Scores how much probability the model put on the players who actually
#' finished 1st, 2nd, … nth, weighting each position by `1 / position` so the
#' winner dominates and tenth barely registers.
#'
#' @param pos_probs Output of `position_probabilities_from_totals()`
#' @param actual_order Character vector of player names in actual finishing
#'   order, longest first
#' @param floor Probability floor, so a missed player costs a finite amount
#'
#' @return List with the weighted log-loss, the per-position detail, and the
#'   unweighted mean for comparison
#' @export
weighted_position_logloss <- function(pos_probs, actual_order, floor = 1e-4) {
  k <- min(length(actual_order), sum(grepl("^p_pos", names(pos_probs))))
  detail <- vapply(seq_len(k), function(i) {
    row <- match(actual_order[i], pos_probs$player_name)
    if (is.na(row)) floor else max(pos_probs[[paste0("p_pos", i)]][row], floor)
  }, numeric(1))

  w <- 1 / seq_len(k)
  list(
    weighted_logloss = sum(w * -log(detail)) / sum(w),
    mean_logloss     = mean(-log(detail)),
    probabilities    = stats::setNames(detail, paste0("pos", seq_len(k))),
    weights          = w
  )
}

#' Win Probabilities from Simulated Season Totals
#'
#' Ties split the win credit, which is right for a "wins outright" market and
#' wrong for "wins or shares" — the Brownlow can be shared.
#'
#' @param totals Output of `simulate_seasons_fast()`
#'
#' @return Data frame with `player_name`, `team`, `win_probability`,
#'   `mean_votes` and quantiles
#' @export
win_probabilities_from_totals <- function(totals) {
  n_sims <- ncol(totals)
  wins <- numeric(nrow(totals))

  for (s in seq_len(n_sims)) {
    v <- totals[, s]
    leaders <- which(v == max(v))
    wins[leaders] <- wins[leaders] + 1 / length(leaders)
  }

  parts <- strsplit(rownames(totals), "_", fixed = TRUE)
  data.frame(
    player_name     = vapply(parts, function(p) paste(p[-length(p)], collapse = "_"), character(1)),
    team            = vapply(parts, function(p) p[length(p)], character(1)),
    win_probability = 100 * wins / n_sims,
    mean_votes      = rowMeans(totals),
    q10_votes       = apply(totals, 1, stats::quantile, 0.10),
    q90_votes       = apply(totals, 1, stats::quantile, 0.90),
    max_votes       = apply(totals, 1, max),
    stringsAsFactors = FALSE,
    row.names        = NULL
  )
}
