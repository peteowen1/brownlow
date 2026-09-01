# Plackett-Luce vote allocation ------------------------------------------------
#
# The current pipeline models each player row independently as a 4-class
# multinomial over {0,1,2,3} votes, then rescales after the fact so the match
# totals 6. That is not how votes are produced. In each match the umpires pick a
# best, then a second, then a third, from the 44-46 players on the park. Whether
# a player polls depends entirely on who else played.
#
# Plackett-Luce is the standard model for exactly that: each player has a worth
# v_i, the best is drawn with probability v_i / sum(v), the second is drawn the
# same way from the remaining field, and so on. Expected votes follow directly:
#
#   E[votes_i] = 3 * P(i is 1st) + 2 * P(i is 2nd) + 1 * P(i is 3rd)
#
# and the constraints hold by construction rather than by post-processing: the
# three placings each sum to 1 across the match, so the total is always exactly 6.

#' Plackett-Luce Placing Probabilities Within One Match
#'
#' Exact by default (`top_k = Inf`, all players ranked). Setting `top_k` to a
#' finite value truncates the field to the k highest-worth players, which is
#' faster but loses tail mass that must then be renormalised away. Cost is
#' O(k^2 * n) with k = min(top_k, n); a full 2,154-match season takes a few
#' seconds exactly, so truncation is rarely worth the approximation.
#'
#' @param v Numeric vector of non-negative worths, one per player
#' @param top_k Players ranked into the exact calculation
#'
#' @return n x 3 matrix of P(1st), P(2nd), P(3rd)
#' @export
pl_placing_probs <- function(v, top_k = Inf) {
  n <- length(v)
  v <- pmax(as.numeric(v), 1e-12)
  S <- sum(v)

  p1 <- v / S
  p2 <- numeric(n)
  p3 <- numeric(n)

  lead <- order(v, decreasing = TRUE)[seq_len(min(top_k, n))]

  # P(i is 2nd) = sum over first-placed j of  P(j 1st) * v_i / (S - v_j)
  for (j in lead) {
    p2 <- p2 + p1[j] * v / (S - v[j])
    p2[j] <- p2[j] - p1[j] * v[j] / (S - v[j])   # j cannot also be 2nd
  }

  # P(i is 3rd) = sum over ordered pairs (j 1st, k 2nd) of the sequential draw
  for (j in lead) {
    Sj <- S - v[j]
    for (k in lead) {
      if (k == j) next
      pjk <- p1[j] * v[k] / Sj
      Sjk <- Sj - v[k]
      if (Sjk <= 0) next
      p3 <- p3 + pjk * v / Sjk
      p3[j] <- p3[j] - pjk * v[j] / Sjk
      p3[k] <- p3[k] - pjk * v[k] / Sjk
    }
  }

  cbind(p1, pmax(p2, 0), pmax(p3, 0))
}

#' Convert Model Scores to a Plackett-Luce Vote Submission
#'
#' Reparameterises any per-player score into the sequential-choice form above.
#' `beta` sets how sharply worth responds to the score and is the one tunable:
#' larger beta concentrates votes on the favourites.
#'
#' @param score Numeric per-player score, higher meaning more likely to poll
#' @param match_ids Match identifier per row
#' @param beta Scale on the score
#' @param top_k Players ranked into the exact calculation per match
#' @param renormalise Rescale placings to sum to exactly 1 per match, absorbing
#'   the tail truncation
#'
#' @return Numeric vector of expected votes, summing to 6 in every match
#' @export
pl_expected_votes <- function(score, match_ids, beta = 1, top_k = Inf, renormalise = TRUE) {
  out <- numeric(length(score))
  for (idx in split(seq_along(score), match_ids)) {
    s <- score[idx]
    v <- exp(beta * (s - max(s)))
    P <- pl_placing_probs(v, top_k = top_k)
    if (renormalise) {
      cs <- colSums(P)
      cs[cs <= 0] <- 1
      P <- sweep(P, 2, cs, "/")
    }
    out[idx] <- P %*% c(3, 2, 1)
  }
  out
}

#' Turn Plackett-Luce Expected Votes into a Harness-Compatible Probability Matrix
#'
#' `evaluate_seeds()` expects an n x 4 matrix. The placing probabilities map
#' straight onto it: P(3 votes) is P(1st), P(2 votes) is P(2nd), P(1 vote) is
#' P(3rd), and the remainder is P(0).
#'
#' @param score Numeric per-player score
#' @param match_ids Match identifier per row
#' @param beta Scale on the score
#' @param top_k Players ranked into the exact calculation per match
#'
#' @return n x 4 matrix of P(0), P(1), P(2), P(3)
#' @export
pl_prob_matrix <- function(score, match_ids, beta = 1, top_k = Inf) {
  out <- matrix(0, nrow = length(score), ncol = 4)
  for (idx in split(seq_along(score), match_ids)) {
    s <- score[idx]
    v <- exp(beta * (s - max(s)))
    P <- pl_placing_probs(v, top_k = top_k)
    cs <- colSums(P); cs[cs <= 0] <- 1
    P <- sweep(P, 2, cs, "/")
    out[idx, 4] <- P[, 1]   # 3 votes  <- 1st
    out[idx, 3] <- P[, 2]   # 2 votes  <- 2nd
    out[idx, 2] <- P[, 3]   # 1 vote   <- 3rd
    out[idx, 1] <- pmax(0, 1 - rowSums(P))
  }
  out
}
