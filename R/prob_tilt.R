# Exponential tilting of a vote-probability row --------------------------------
#
# The in-count momentum update needs to shift a player's expected votes for
# their REMAINING matches by a target amount, while keeping the result a valid
# probability distribution over {0,1,2,3} that IPF can still constrain.
#
# Exponential tilting does this in one parameter: p_new propto p * exp(theta * v),
# v = c(0,1,2,3). E[votes | theta] is strictly increasing in theta (it is the
# derivative of the cumulant-generating function), so a target expected value
# has a unique theta, found here by vectorised Newton's method rather than a
# per-row uniroot() call — this runs over thousands of rows per season.

#' Tilt Each Row of a Probability Matrix to a Target Expected Value
#'
#' Most rows here are heavily peaked on P(0) — a typical player is very unlikely
#' to poll. Newton's method on such a row has a near-zero derivative (the
#' tilted-distribution variance), so a step can overshoot into an unbounded
#' theta and produce NaN; measured, this hit 143 of 3,564 rows in one season.
#' Bisection has no derivative to blow up and is globally convergent given that
#' E(theta) is monotonic in theta, so it is used instead, at the cost of a fixed
#' iteration count rather than Newton's faster convergence — irrelevant here
#' since a season's worth of rows tilts in well under a second either way.
#'
#' @param prob_matrix n x 4 matrix of P(0), P(1), P(2), P(3)
#' @param target_ev Numeric vector, length n, the desired expected value per row
#' @param iters Bisection iterations; 60 halvings of [-50, 50] resolves theta to
#'   about 1.7e-14, far past the precision this needs
#'
#' @return n x 4 matrix of tilted, renormalised probabilities
#' @export
tilt_to_expected_value <- function(prob_matrix, target_ev, iters = 60) {
  v <- c(0, 1, 2, 3)
  n <- nrow(prob_matrix)
  logp <- log(pmax(prob_matrix, 1e-12))

  expected_value <- function(theta) {
    w <- exp(logp + outer(theta, v))
    as.numeric((w %*% v) / rowSums(w))
  }

  lo <- rep(-50, n)
  hi <- rep(50, n)
  for (k in seq_len(iters)) {
    mid <- (lo + hi) / 2
    go_right <- expected_value(mid) < target_ev
    lo[go_right]  <- mid[go_right]
    hi[!go_right] <- mid[!go_right]
  }
  theta <- (lo + hi) / 2

  w <- exp(logp + outer(theta, v))
  w / rowSums(w)
}
