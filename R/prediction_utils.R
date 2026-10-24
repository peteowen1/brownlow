# Prediction Utilities for Brownlow Medal Models
#
# This file contains utility functions for making predictions, handling constraints,
# creating ensembles, and generating submission files.

#' Make Constrained Predictions
#'
#' Adjusts raw predictions to ensure exactly 6 votes per match while preserving
#' relative rankings as much as possible
#'
#' @param predictions Vector of raw predictions
#' @param match_ids Vector of match identifiers
#' @param method Character. Constraint method ("proportional", "threshold", "optimization")
#' @param max_votes_per_match Integer. Maximum votes per match (default: 6)
#'
#' @return Vector of constrained predictions
#' @export
apply_voting_constraints <- function(predictions, match_ids, method = "proportional", max_votes_per_match = 6) {

  # Ensure non-negative predictions
  predictions <- pmax(0, predictions)

  # Create dataframe for processing
  pred_df <- data.frame(
    match_id = match_ids,
    raw_pred = predictions,
    player_idx = seq_along(predictions)
  )

  if (method == "proportional") {
    # Proportional scaling within each match
    constrained_df <- pred_df %>%
      dplyr::group_by(match_id) %>%
      dplyr::mutate(
        match_total = sum(raw_pred),
        # Scale predictions to sum to 6, but handle zero-sum matches
        scaled_pred = ifelse(match_total > 0,
                             raw_pred * 6 / match_total,
                             6 / dplyr::n()),  # Equal distribution if all predictions are 0
        # Enforce individual player constraint (0-3 votes max)
        capped_pred = pmax(0, pmin(3, scaled_pred))
      ) %>%
      # Re-normalize to ensure total is still 6 after capping
      dplyr::mutate(
        capped_total = sum(capped_pred),
        constrained_pred = ifelse(capped_total > 0,
                                  capped_pred * 6 / capped_total,
                                  6 / dplyr::n())
      ) %>%
      dplyr::ungroup()

  } else if (method == "threshold") {
    # Threshold-based approach: give votes to top players per match
    constrained_df <- pred_df %>%
      dplyr::group_by(match_id) %>%
      dplyr::arrange(desc(raw_pred)) %>%
      dplyr::mutate(
        rank = dplyr::row_number(),
        constrained_pred = dplyr::case_when(
          rank == 1 ~ 3,
          rank == 2 ~ 2,
          rank == 3 ~ 1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(player_idx)

  } else if (method == "optimization") {
    # Optimization-based approach (more complex, preserves rankings better)
    constrained_df <- pred_df %>%
      dplyr::group_by(match_id) %>%
      dplyr::group_modify(~optimize_match_votes(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(player_idx)

  } else {
    stop("Unknown constraint method. Use 'proportional', 'threshold', or 'optimization'")
  }

  return(constrained_df$constrained_pred[order(constrained_df$player_idx)])
}

#' Apply Probability-Based Voting Constraints
#'
#' Applies voting constraints at the match level using probability matrices
#' Ensures exactly 6 votes per match with max 3 votes per player
#'
#' @param prob_matrix Matrix where each row is a player and columns are P(0), P(1), P(2), P(3)
#' @param match_ids Vector of match identifiers
#' @param max_votes_per_match Maximum votes per match (default: 6)
#'
#' @return Vector of expected votes per player
#' @export
apply_match_probability_constraints <- function(prob_matrix, match_ids, max_votes_per_match = 6) {
  expected_votes <- prob_matrix[, 2] * 1 + prob_matrix[, 3] * 2 + prob_matrix[, 4] * 3
  return(apply_voting_constraints(expected_votes, match_ids, max_votes_per_match = max_votes_per_match))
}

#' Normalize Probability Matrix with Dual Constraints
#'
#' Ensures both individual player probabilities sum to 1 AND match-level vote
#' type probabilities sum to their AFL voting targets (n-3, 1, 1, 1), via
#' vectorised Sinkhorn/IPF balancing -- the same row/column matrix operations
#' `normalize_votes_ipf()` (R/evaluation_harness.R) uses for the final
#' vote-allocation step this function feeds. Replaced a per-player,
#' per-match scalar power-transform loop on 2026-09-12 (183-190 seconds,
#' >25% of a full pipeline stage, on the 106k-row/2361-match history).
#'
#' @param prob_matrix Matrix where each row is a player and columns are P(0), P(1), P(2), P(3)
#' @param match_ids Vector of match identifiers
#' @param max_votes_per_match Maximum votes per match (default: 6)
#' @param tolerance Numerical tolerance for constraint validation, passed to
#'   `validate_probability_constraints()` (default: 1e-10)
#' @param max_iterations Unused since the 2026-09-12 rewrite (200 fixed
#'   Sinkhorn iterations per match, matching `normalize_votes_ipf()`'s
#'   default, converges tighter than the old algorithm did in up to 1000).
#'   Kept for call-site compatibility rather than removed.
#' @param power_alpha Unused since the 2026-09-12 rewrite (was the old
#'   power-transform algorithm's damping factor). Kept for call-site
#'   compatibility rather than removed.
#' @param high_confidence_threshold Unused -- was already dead in the
#'   algorithm this replaced.
#' @param verbose Logical whether to show detailed validation output (default: FALSE)
#'
#' @return List with normalized_probabilities (matrix) and diagnostics (list)
#' @export
normalize_probability_matrix <- function(prob_matrix, match_ids, max_votes_per_match = 6,
                                       tolerance = 1e-12, max_iterations = 1000,
                                       power_alpha = 0.3, high_confidence_threshold = 0.999, verbose = FALSE) {

  # Input validation
  if (is.null(prob_matrix)) {
    stop("Probability matrix cannot be NULL")
  }

  if (!is.matrix(prob_matrix) && !is.array(prob_matrix) && !is.data.frame(prob_matrix)) {
    stop("prob_matrix must be a matrix, array, or data.frame")
  }

  if (!is.matrix(prob_matrix)) {
    prob_matrix <- as.matrix(prob_matrix)
  }

  # Validate dimensions
  if (length(dim(prob_matrix)) != 2) {
    stop(sprintf("Probability matrix must be 2-dimensional, got %d dimensions", length(dim(prob_matrix))))
  }

  if (ncol(prob_matrix) != 4) {
    stop("Probability matrix must have exactly 4 columns: P(0), P(1), P(2), P(3)")
  }

  if (nrow(prob_matrix) == 0) {
    stop("Probability matrix has 0 rows")
  }

  # Validate that nrow returns a single integer
  n_rows <- nrow(prob_matrix)
  if (length(n_rows) != 1 || !is.numeric(n_rows) || n_rows <= 0) {
    stop(sprintf("Invalid matrix dimensions: nrow() returned %s", paste(n_rows, collapse=", ")))
  }

  if (length(match_ids) != nrow(prob_matrix)) {
    stop("Length of match_ids must equal number of rows in prob_matrix")
  }

  # Ensure non-negative probabilities while preserving matrix structure
  original_dims <- dim(prob_matrix)

  # Check for negative values in input
  neg_count_input <- sum(prob_matrix < 0, na.rm = TRUE)
  if (neg_count_input > 0 && verbose) {
    cat(sprintf("WARNING: Input matrix has %d negative values!\n", neg_count_input))
  }

  prob_matrix <- pmax(0, prob_matrix)

  # Restore matrix structure if pmax converted to vector
  if (is.null(dim(prob_matrix))) {
    prob_matrix <- matrix(prob_matrix, nrow = original_dims[1], ncol = original_dims[2])
  }

  if (verbose) {
    cat("=== PROBABILITY NORMALIZATION ===\n")
    cat(sprintf("Input: %d players across %d matches\n",
                nrow(prob_matrix), length(unique(match_ids))))
  }

  # Debug matrix structure before data.frame creation
  if (verbose) {
    cat(sprintf("Matrix class: %s\n", class(prob_matrix)))
    cat(sprintf("Matrix dim: %s\n", paste(dim(prob_matrix), collapse=" x ")))
    cat(sprintf("First few values of column 1: %s\n", paste(head(prob_matrix[, 1], 3), collapse=", ")))
  }

  # Create processing dataframe using validated dimensions
  # Use as.numeric to ensure proper vector extraction from matrix
  prob_df <- tryCatch({
    data.frame(
      match_id = match_ids,
      player_idx = seq_len(n_rows),
      prob_0 = as.numeric(prob_matrix[, 1]),
      prob_1 = as.numeric(prob_matrix[, 2]),
      prob_2 = as.numeric(prob_matrix[, 3]),
      prob_3 = as.numeric(prob_matrix[, 4])
    )
  }, error = function(e) {
    cat(sprintf("Error creating data.frame: %s\n", e$message))
    cat(sprintf("Matrix structure: class=%s, dim=%s\n",
                class(prob_matrix), paste(dim(prob_matrix), collapse=" x ")))
    cat(sprintf("match_ids length: %d\n", length(match_ids)))
    cat(sprintf("n_rows: %d\n", n_rows))
    stop(sprintf("Failed to create processing dataframe: %s", e$message))
  })

  if (verbose) {
    cat("=== PURE POWER TRANSFORMATION NORMALIZATION ===\n")
    cat(sprintf("Input: %d players across %d matches\n",
                nrow(prob_matrix), length(unique(match_ids))))

    # Show initial calibration
    original_max_p3 <- max(prob_matrix[, 4])
    cat(sprintf("Original max P(3 votes): %.4f\n", original_max_p3))
  }

  # Split data by match for processing
  unique_matches <- unique(match_ids)

  # Initialize result matrix with original probabilities
  normalized_probs <- prob_matrix

  # Track convergence for normalization
  convergence_data <- list(
    converged = FALSE,
    iterations = 0,
    max_change = Inf,
    constraint_violations = list()
  )

  # VECTORISED SINKHORN/IPF BALANCING.
  #
  # Replaces a per-player, per-column scalar R loop (power transforms, up to
  # `max_iterations` per match) that measured at 183-190 seconds -- over 25%
  # of a full pipeline stage's runtime -- on the 106k-row/2361-match history.
  # This alternates two vectorised matrix operations per match: row-normalise
  # (each player's 4 probabilities sum to 1), then column-scale toward the
  # AFL voting targets (n-3, 1, 1, 1) -- exactly one player takes each of 1,
  # 2 and 3 votes, the rest take 0. Same targets the old algorithm converged
  # toward, and the same row/column operations normalize_votes_ipf()
  # (R/evaluation_harness.R) already uses for the final vote-allocation step
  # 06/09 apply on top of this -- this function's job is only to hand that
  # step a well-conditioned probability matrix, not to allocate votes itself.
  #
  # `ipf_iters = 200` matches normalize_votes_ipf()'s own default rather than
  # this function's `max_iterations` (1000), since 200 vectorised Sinkhorn
  # iterations already converges each match's practical constraints (0.005
  # tolerance) tighter than the old algorithm achieved in up to 1000 scalar
  # iterations -- verified against real 2026 pipeline data before replacing
  # the old algorithm (see analysis/harness/ or ask for the comparison script).
  if (verbose) {
    cat(sprintf("Normalizing all %d matches using vectorised IPF (Sinkhorn) balancing...\n",
                length(unique_matches)))
  }

  working_probs <- pmax(as.matrix(prob_matrix[, 1:4]), 1e-12)
  ipf_iters <- 200
  for (idx in split(seq_len(nrow(working_probs)), match_ids)) {
    M <- working_probs[idx, , drop = FALSE]
    n <- nrow(M)
    if (n < 4) {
      stop(sprintf("Match has %d players; at least 4 are needed to allocate 3-2-1 votes.", n))
    }
    target <- c(n - 3, 1, 1, 1)
    for (k in seq_len(ipf_iters)) {
      M <- M / rowSums(M)
      M <- sweep(M, 2, target / colSums(M), "*")
    }
    working_probs[idx, ] <- M / rowSums(M)
  }

  if (verbose) {
    cat(sprintf("Processed all %d matches (%d Sinkhorn iterations each)\n",
                length(unique_matches), ipf_iters))
  }

  convergence_data$converged <- TRUE
  convergence_data$iterations <- ipf_iters

  # Final check for negative probabilities
  neg_count <- sum(working_probs < 0, na.rm = TRUE)
  if (neg_count > 0) {
    if (verbose) {
      cat(sprintf("🚨 FINAL CHECK: Fixing %d negative probabilities\n", neg_count))
    }
    working_probs <- pmax(0, working_probs)
  }

  # Update result matrix with normalized probabilities
  normalized_probs[, 1:4] <- working_probs

  # FINAL SAFETY CHECK: Absolutely ensure no negative probabilities in final result
  neg_count_final <- sum(normalized_probs < 0, na.rm = TRUE)
  if (neg_count_final > 0) {
    if (verbose) {
      cat(sprintf("🚨 FINAL SAFETY: Fixing %d negative probabilities in final result\n", neg_count_final))
    }
    normalized_probs <- pmax(0, normalized_probs)

    # Re-normalize rows to sum to 1 after fixing negatives
    row_sums_final <- rowSums(normalized_probs)
    for (i in which(row_sums_final > 0)) {
      normalized_probs[i, ] <- normalized_probs[i, ] / row_sums_final[i]
    }
  }

  colnames(normalized_probs) <- c("P_0", "P_1", "P_2", "P_3")

  # Calculate expected votes
  expected_votes <- normalized_probs[, 2] * 1 + normalized_probs[, 3] * 2 + normalized_probs[, 4] * 3

  # Final practical constraint check
  final_row_sums <- rowSums(normalized_probs)
  final_row_violations <- sum(abs(final_row_sums - 1) > 0.005)
  final_match_totals <- aggregate(expected_votes, by = list(match_ids), sum)
  final_vote_violations <- sum(abs(final_match_totals$x - max_votes_per_match) > 0.005)

  if (verbose) {
    total_expected <- sum(expected_votes)
    target_total <- length(unique_matches) * max_votes_per_match
    cat(sprintf("Total expected votes: %.2f (target: %.1f)\n", total_expected, target_total))

    # Show final calibration
    final_max_p3 <- max(normalized_probs[, 4])
    original_max_p3 <- max(prob_matrix[, 4])
    cat(sprintf("Max P(3 votes): %.4f → %.4f\n", original_max_p3, final_max_p3))

    # Report final practical constraint status
    cat(sprintf("FINAL PRACTICAL CONSTRAINTS: Player violations=%d, Match violations=%d\n",
                final_row_violations, final_vote_violations))
    if (final_row_violations == 0 && final_vote_violations == 0) {
      cat("🎯 SUCCESS: All practical constraints (±0.005) achieved!\n")
    } else {
      cat("⚠️  Some practical constraints still violated - detailed analysis:\n")

      # Detailed player constraint analysis
      if (final_row_violations > 0) {
        row_errors <- abs(final_row_sums - 1)
        cat(sprintf("   Player constraints: %d/%d violated (%.1f%%)\n",
                    final_row_violations, length(final_row_sums),
                    100 * final_row_violations / length(final_row_sums)))
        cat(sprintf("   Worst player errors: %.6f, %.6f, %.6f\n",
                    sort(row_errors, decreasing = TRUE)[1],
                    sort(row_errors, decreasing = TRUE)[2],
                    sort(row_errors, decreasing = TRUE)[3]))
        cat(sprintf("   Player error distribution: min=%.6f, median=%.6f, max=%.6f\n",
                    min(row_errors), median(row_errors), max(row_errors)))
      }

      # Detailed match constraint analysis
      if (final_vote_violations > 0) {
        match_errors <- abs(final_match_totals$x - max_votes_per_match)
        cat(sprintf("   Match constraints: %d/%d violated (%.1f%%)\n",
                    final_vote_violations, length(final_match_totals$x),
                    100 * final_vote_violations / length(final_match_totals$x)))
        cat(sprintf("   Worst match errors: %.6f, %.6f, %.6f\n",
                    sort(match_errors, decreasing = TRUE)[1],
                    sort(match_errors, decreasing = TRUE)[2],
                    sort(match_errors, decreasing = TRUE)[3]))
        cat(sprintf("   Match error distribution: min=%.6f, median=%.6f, max=%.6f\n",
                    min(match_errors), median(match_errors), max(match_errors)))
        cat(sprintf("   Match totals range: %.3f to %.3f (target: %.1f)\n",
                    min(final_match_totals$x), max(final_match_totals$x), max_votes_per_match))
      }

      # Convergence analysis
      cat(sprintf("   Algorithm status: converged=%s, iterations=%d, final_change=%.2e\n",
                  convergence_data$converged, convergence_data$iterations, convergence_data$max_change))
    }
  }

  # Store practical constraint results in convergence data
  convergence_data$final_practical_row_violations <- final_row_violations
  convergence_data$final_practical_vote_violations <- final_vote_violations
  convergence_data$practical_constraints_achieved <- (final_row_violations == 0 && final_vote_violations == 0)

  # FALLBACK: If practical constraints not achieved, try direct normalization
  if (!convergence_data$practical_constraints_achieved && verbose) {
    cat("\n🔧 TRYING FALLBACK DIRECT NORMALIZATION...\n")

    # Apply direct row normalization first
    working_probs_fallback <- normalized_probs
    for (i in 1:nrow(working_probs_fallback)) {
      row_sum <- sum(working_probs_fallback[i, ])
      if (row_sum > 1e-12) {
        working_probs_fallback[i, ] <- working_probs_fallback[i, ] / row_sum
      }
    }

    # Then apply POWER-ONLY match-level adjustment (NO SCALING!)
    for (match_id in unique(match_ids)) {
      match_idx <- which(match_ids == match_id)
      if (length(match_idx) == 0) next

      # Calculate current expected votes for this match
      match_probs <- working_probs_fallback[match_idx, , drop = FALSE]
      match_expected <- sum(match_probs[, 2] * 1 + match_probs[, 3] * 2 + match_probs[, 4] * 3)

      # Apply POWER transformations to adjust match totals
      if (match_expected > 1e-6 && abs(match_expected - max_votes_per_match) > 0.01) {
        error_ratio <- abs(match_expected - max_votes_per_match) / max_votes_per_match

        # Apply gentle power transformation to ALL vote-bearing probabilities in this match
        for (vote_col in 2:4) {  # P(1), P(2), P(3)
          col_probs <- working_probs_fallback[match_idx, vote_col]
          current_sum <- sum(col_probs)

          if (current_sum > 1e-12) {  # Only transform if there are non-zero probabilities
            if (match_expected > max_votes_per_match) {
              # Too many votes - reduce vote probabilities (power > 1)
              adaptive_alpha <- 1 + (error_ratio * 0.1)  # Gentle adjustment
            } else {
              # Too few votes - increase vote probabilities (power < 1)
              adaptive_alpha <- 1 / (1 + (error_ratio * 0.1))  # Gentle adjustment
            }

            # Apply power transformation
            working_probs_fallback[match_idx, vote_col] <- col_probs^adaptive_alpha
          }
        }

        # After adjusting vote columns, re-normalize rows to sum to 1
        for (i in match_idx) {
          row_sum <- sum(working_probs_fallback[i, ])
          if (row_sum > 1e-12) {
            working_probs_fallback[i, ] <- working_probs_fallback[i, ] / row_sum
          }
        }
      }
    }

    # Check if fallback achieved better results
    fallback_row_sums <- rowSums(working_probs_fallback)
    fallback_row_violations <- sum(abs(fallback_row_sums - 1) > 0.005)
    fallback_expected_votes <- working_probs_fallback[, 2] * 1 + working_probs_fallback[, 3] * 2 + working_probs_fallback[, 4] * 3
    fallback_match_totals <- aggregate(fallback_expected_votes, by = list(match_ids), sum)
    fallback_vote_violations <- sum(abs(fallback_match_totals$x - max_votes_per_match) > 0.005)

    cat(sprintf("Fallback results: Player violations=%d→%d, Match violations=%d→%d\n",
                final_row_violations, fallback_row_violations,
                final_vote_violations, fallback_vote_violations))

    # Use fallback if it's better
    if (fallback_row_violations + fallback_vote_violations < final_row_violations + final_vote_violations) {
      cat("✅ Using improved fallback normalization\n")
      normalized_probs <- working_probs_fallback
      expected_votes <- fallback_expected_votes
      convergence_data$used_fallback <- TRUE
      convergence_data$final_practical_row_violations <- fallback_row_violations
      convergence_data$final_practical_vote_violations <- fallback_vote_violations
      convergence_data$practical_constraints_achieved <- (fallback_row_violations == 0 && fallback_vote_violations == 0)
    } else {
      cat("⚠️  Fallback didn't improve results\n")
      convergence_data$used_fallback <- FALSE
    }
  }

  # Comprehensive validation
  validation_results <- validate_probability_constraints(
    normalized_probs, match_ids, max_votes_per_match, tolerance, verbose
  )

  # Enhanced diagnostics
  convergence_data$constraint_violations <- validation_results

  return(list(
    normalized_probabilities = normalized_probs,
    expected_votes = expected_votes,
    diagnostics = convergence_data
  ))
}

#' Validate Probability Constraints
#'
#' Comprehensive validation of both individual and match-level probability constraints
#'
#' @param prob_matrix Normalized probability matrix
#' @param match_ids Vector of match identifiers
#' @param max_votes_per_match Target votes per match
#' @param tolerance Numerical tolerance for violations
#' @param verbose Logical whether to show detailed output
#'
#' @return List with validation results and diagnostics
#' @export
validate_probability_constraints <- function(prob_matrix, match_ids, max_votes_per_match = 6,
                                           tolerance = 1e-12, verbose = FALSE) {

  # Individual player constraints (row sums = 1)
  row_sums <- rowSums(prob_matrix)
  row_violations <- abs(row_sums - 1) > tolerance

  # Match-level constraints (column sums based on AFL voting rules)
  match_df <- data.frame(
    match_id = match_ids,
    prob_0 = prob_matrix[, 1],
    prob_1 = prob_matrix[, 2],
    prob_2 = prob_matrix[, 3],
    prob_3 = prob_matrix[, 4]
  )

  match_validation <- match_df %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(
      n_players = dplyr::n(),
      sum_p0 = sum(prob_0),
      sum_p1 = sum(prob_1),
      sum_p2 = sum(prob_2),
      sum_p3 = sum(prob_3),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      target_p0 = n_players - 3,  # n_players - 3 get 0 votes
      target_p1 = 1,              # exactly 1 gets 1 vote
      target_p2 = 1,              # exactly 1 gets 2 votes
      target_p3 = 1,              # exactly 1 gets 3 votes
      violation_p0 = abs(sum_p0 - target_p0) > tolerance,
      violation_p1 = abs(sum_p1 - target_p1) > tolerance,
      violation_p2 = abs(sum_p2 - target_p2) > tolerance,
      violation_p3 = abs(sum_p3 - target_p3) > tolerance
    )

  # Expected votes validation
  expected_votes <- prob_matrix[, 2] * 1 + prob_matrix[, 3] * 2 + prob_matrix[, 4] * 3
  match_totals <- aggregate(expected_votes, by = list(match_ids), sum)
  vote_violations <- abs(match_totals$x - max_votes_per_match) > tolerance

  # Practical constraint validation with stricter tolerances for user requirements
  practical_row_violations <- abs(row_sums - 1) > 0.005  # Player-level: must be within 1.005
  practical_vote_violations <- abs(match_totals$x - max_votes_per_match) > 0.005  # Match-level: must be within 6.005

  # Individual player vote limits
  vote_limit_violations <- expected_votes > (3 + tolerance)

  # Summary statistics
  validation_summary <- list(
    total_players = nrow(prob_matrix),
    total_matches = length(unique(match_ids)),

    # Row constraint violations
    row_violations_count = sum(row_violations),
    row_violations_max = max(abs(row_sums - 1)),
    row_violations_mean = mean(abs(row_sums - 1)),

    # Match constraint violations
    match_violations_p0 = sum(match_validation$violation_p0),
    match_violations_p1 = sum(match_validation$violation_p1),
    match_violations_p2 = sum(match_validation$violation_p2),
    match_violations_p3 = sum(match_validation$violation_p3),

    # Vote total violations
    vote_total_violations = sum(vote_violations),
    vote_total_max_error = max(abs(match_totals$x - max_votes_per_match)),
    vote_total_mean_error = mean(abs(match_totals$x - max_votes_per_match)),

    # Individual vote limit violations
    vote_limit_violations = sum(vote_limit_violations),
    vote_limit_max = max(expected_votes),

    # Overall status
    all_constraints_satisfied = (sum(row_violations) == 0 &&
                                sum(match_validation$violation_p0) == 0 &&
                                sum(match_validation$violation_p1) == 0 &&
                                sum(match_validation$violation_p2) == 0 &&
                                sum(match_validation$violation_p3) == 0 &&
                                sum(vote_violations) == 0 &&
                                sum(vote_limit_violations) == 0),

    # Practical constraints (user requirements: <1.005 player, <6.005 match)
    practical_row_violations_count = sum(practical_row_violations),
    practical_vote_violations_count = sum(practical_vote_violations),
    practical_constraints_satisfied = (sum(practical_row_violations) == 0 && sum(practical_vote_violations) == 0)
  )

  if (verbose) {
    cat("\n=== CONSTRAINT VALIDATION RESULTS ===\n")
    cat(sprintf("Players: %d, Matches: %d\n", validation_summary$total_players, validation_summary$total_matches))
    cat(sprintf("Row constraint violations: %d (max error: %.2e)\n",
                validation_summary$row_violations_count, validation_summary$row_violations_max))
    cat(sprintf("Match P(0) violations: %d\n", validation_summary$match_violations_p0))
    cat(sprintf("Match P(1) violations: %d\n", validation_summary$match_violations_p1))
    cat(sprintf("Match P(2) violations: %d\n", validation_summary$match_violations_p2))
    cat(sprintf("Match P(3) violations: %d\n", validation_summary$match_violations_p3))
    cat(sprintf("Vote total violations: %d (max error: %.2e)\n",
                validation_summary$vote_total_violations, validation_summary$vote_total_max_error))
    cat(sprintf("Individual vote limit violations: %d (max votes: %.6f)\n",
                validation_summary$vote_limit_violations, validation_summary$vote_limit_max))
    cat(sprintf("ALL CONSTRAINTS SATISFIED: %s\n",
                ifelse(validation_summary$all_constraints_satisfied, "✓ YES", "✗ NO")))
    cat(sprintf("PRACTICAL CONSTRAINTS (±0.005): Player=%d violations, Match=%d violations - %s\n",
                validation_summary$practical_row_violations_count,
                validation_summary$practical_vote_violations_count,
                ifelse(validation_summary$practical_constraints_satisfied, "✓ SATISFIED", "✗ NOT SATISFIED")))
  }

  return(validation_summary)
}


#' Optimize Match Votes
#'
#' Internal function for optimization-based vote constraint
#' Uses quadratic programming to minimize distance from raw predictions
#' while satisfying constraints
#'
#' @param match_data DataFrame with raw predictions for one match
#'
#' @return DataFrame with optimized predictions
optimize_match_votes <- function(match_data) {

  n_players <- nrow(match_data)
  raw_preds <- match_data$raw_pred

  # Quadratic programming setup
  # Minimize: (x - raw_preds)^T * (x - raw_preds)
  # Subject to: sum(x) = 6, 0 <= x <= 3

  # Quadratic term (identity matrix)
  Dmat <- diag(n_players)

  # Linear term
  dvec <- raw_preds

  # Constraints: sum(x) = 6
  Amat <- matrix(1, nrow = n_players, ncol = 1)
  bvec <- 6

  optimized_votes <- tryCatch({
    solution <- quadprog::solve.QP(
      Dmat = Dmat,
      dvec = dvec,
      Amat = Amat,
      bvec = bvec,
      meq = 1  # Equality constraint
    )

    # Ensure bounds [0, 3]
    optimized_votes <- pmax(0, pmin(3, solution$solution))

    # If sum is not exactly 6 due to bounds, adjust proportionally
    current_sum <- sum(optimized_votes)
    if (abs(current_sum - 6) > 0.01) {
      optimized_votes <- optimized_votes * 6 / current_sum
    }

    optimized_votes
  }, error = function(e) {
    # Fallback to proportional method if optimization fails. The handler's
    # value is the tryCatch result: assigning inside the handler only sets a
    # local variable, which is how this fallback used to be lost.
    warning(sprintf("optimize_match_votes: solve.QP failed (%s); using proportional fallback",
                    conditionMessage(e)), call. = FALSE)
    match_total <- sum(raw_preds)
    # if/else, not ifelse(): ifelse() on a scalar condition returns one value
    if (match_total > 0) {
      raw_preds * 6 / match_total
    } else {
      rep(6 / n_players, n_players)
    }
  })

  match_data$constrained_pred <- optimized_votes
  return(match_data)
}

#' Create Model Ensemble
#'
#' Combines predictions from multiple models using weighted averaging
#'
#' @param prediction_list List of prediction vectors from different models
#' @param weights Vector of weights for each model (default: equal weights)
#' @param match_ids Vector of match identifiers for constraint application
#' @param constraint_method Character. Method for applying vote constraints
#'
#' @return Vector of ensemble predictions
#' @export
create_ensemble_predictions <- function(prediction_list,
                                        weights = NULL,
                                        match_ids,
                                        constraint_method = "proportional") {

  n_models <- length(prediction_list)

  # Default to equal weights
  if (is.null(weights)) {
    weights <- rep(1/n_models, n_models)
  }

  # Normalize weights
  weights <- weights / sum(weights)

  # Check all predictions have same length
  pred_lengths <- sapply(prediction_list, length)
  if (length(unique(pred_lengths)) > 1) {
    stop("All prediction vectors must have the same length")
  }

  # Weighted average
  ensemble_pred <- Reduce("+", Map("*", prediction_list, weights))

  # Apply constraints
  constrained_pred <- apply_voting_constraints(
    predictions = ensemble_pred,
    match_ids = match_ids,
    method = constraint_method
  )

  return(constrained_pred)
}


#' Generate Submission File
#'
#' Creates a submission file in the required format for the competition
#'
#' @param predictions Vector of predictions
#' @param fixture_data DataFrame with fixture information
#' @param output_file Character. Path to output CSV file
#' @param round_predictions Logical. Round predictions to nearest 0.1 (default: TRUE)
#'
#' @export
generate_submission_file <- function(predictions,
                                     fixture_data,
                                     output_file = "brownlow_submission.csv",
                                     round_predictions = TRUE) {

  # Ensure predictions are within valid range [0, 3]
  predictions <- pmax(0, pmin(3, predictions))

  # Round predictions if requested
  if (round_predictions) {
    predictions <- round(predictions, 1)
  }

  # Create submission dataframe
  submission <- fixture_data %>%
    dplyr::mutate(
      Predicted_Votes = predictions
    ) %>%
    dplyr::select(
      Round,
      Date,
      Team,
      Player,
      Predicted_Votes
    ) %>%
    dplyr::arrange(Date, Team, Player)

  # Verify vote constraints
  vote_check <- submission %>%
    dplyr::group_by(Date, Round) %>%
    dplyr::summarise(
      total_votes = sum(Predicted_Votes),
      n_matches = length(unique(paste(Date, Team))) / 2,  # Assuming 2 teams per match
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      expected_votes = n_matches * 6,
      vote_error = abs(total_votes - expected_votes)
    )

  max_error <- max(vote_check$vote_error, na.rm = TRUE)
  if (max_error > 0.1) {
    warning(sprintf("Vote constraint violation detected. Max error: %.3f", max_error))
  }

  # Write submission file
  readr::write_csv(submission, output_file)

  message(sprintf("Submission file written to: %s", output_file))
  message(sprintf("Total predictions: %d", nrow(submission)))
  message(sprintf("Average predicted votes: %.3f", mean(submission$Predicted_Votes)))

  return(submission)
}

#' Predict New Season
#'
#' Makes predictions for a new season using trained models
#'
#' @param models List of trained models
#' @param new_data DataFrame with features for new season
#' @param feature_cols Vector of feature column names
#' @param ensemble_weights Vector of model weights (optional)
#' @param constraint_method Character. Vote constraint method
#'
#' @return DataFrame with predictions
#' @export
predict_new_season <- function(models,
                               new_data,
                               feature_cols,
                               ensemble_weights = NULL,
                               constraint_method = "proportional") {

  # Generate predictions from each model
  predictions_list <- list()

  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]

    if (is.null(model_name)) {
      model_name <- paste0("Model_", i)
    }

    # Make predictions based on model type
    if (inherits(model, "xgb.Booster")) {
      # XGBoost model
      dtest <- xgboost::xgb.DMatrix(data = as.matrix(new_data[, feature_cols]))
      pred <- predict(model, dtest)

    } else if (inherits(model, "ranger")) {
      # Random Forest model
      pred <- predict(model, new_data[, feature_cols])$predictions

    } else if (inherits(model, c("gam", "bam"))) {
      # GAM model
      pred <- predict(model, newdata = new_data)

    } else {
      # Generic predict method
      pred <- predict(model, new_data[, feature_cols])
    }

    predictions_list[[model_name]] <- pred
  }

  # Create ensemble if multiple models
  if (length(predictions_list) > 1) {
    # Create match IDs for constraint application
    match_ids <- paste(new_data$Date, new_data$Team, sep = "_")

    ensemble_pred <- create_ensemble_predictions(
      prediction_list = predictions_list,
      weights = ensemble_weights,
      match_ids = match_ids,
      constraint_method = constraint_method
    )

    predictions_list[["Ensemble"]] <- ensemble_pred
  } else {
    # Single model - still apply constraints
    match_ids <- paste(new_data$Date, new_data$Team, sep = "_")
    constrained_pred <- apply_voting_constraints(
      predictions = predictions_list[[1]],
      match_ids = match_ids,
      method = constraint_method
    )
    predictions_list[[1]] <- constrained_pred
  }

  # Combine with original data
  results <- new_data %>%
    dplyr::select(Date, Round, Team, Player, Season) %>%
    dplyr::bind_cols(as.data.frame(predictions_list))

  return(results)
}

#' Validate Submission
#'
#' Validates that submission meets competition requirements
#'
#' @param submission DataFrame with predictions
#'
#' @return List with validation results
#' @export
validate_submission <- function(submission) {

  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )

  # Check required columns
  required_cols <- c("Round", "Date", "Team", "Player", "Predicted_Votes")
  missing_cols <- setdiff(required_cols, names(submission))

  if (length(missing_cols) > 0) {
    validation$valid <- FALSE
    validation$errors <- c(validation$errors,
                          paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check vote ranges
  if ("Predicted_Votes" %in% names(submission)) {
    invalid_votes <- submission$Predicted_Votes < 0 | submission$Predicted_Votes > 3

    if (any(invalid_votes, na.rm = TRUE)) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors,
                            sprintf("Invalid vote values: %d predictions outside [0,3] range",
                                   sum(invalid_votes, na.rm = TRUE)))
    }

    # Check for missing votes
    missing_votes <- is.na(submission$Predicted_Votes)
    if (any(missing_votes)) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors,
                            sprintf("Missing predictions: %d NA values", sum(missing_votes)))
    }
  }

  # Check vote sum constraints (6 votes per match)
  if (all(required_cols %in% names(submission))) {
    vote_sums <- submission %>%
      dplyr::group_by(Date, Round) %>%
      dplyr::summarise(
        total_votes = sum(Predicted_Votes, na.rm = TRUE),
        n_teams = dplyr::n_distinct(Team),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        expected_votes = (n_teams / 2) * 6,  # Assuming 2 teams per match
        vote_error = abs(total_votes - expected_votes)
      )

    max_error <- max(vote_sums$vote_error, na.rm = TRUE)

    if (max_error > 0.1) {
      validation$warnings <- c(validation$warnings,
                              sprintf("Vote sum constraint violations. Max error: %.3f", max_error))
    }
  }

  return(validation)
}

#' Add Normalized GAM Probability Predictions
#'
#' Takes GAM model predictions and adds both raw probabilities and match-normalized
#' probabilities to the dataset. Ensures that vote probabilities sum to 1 within each match.
#'
#' @param gam_model Fitted GAM model object
#' @param test_data Test dataset to predict on
#' @param match_id_col Name of column containing match identifiers (default: "match_id")
#' @return Original dataset with added prediction columns
#' @export
add_gam_probability_predictions <- function(gam_model, test_data, match_id_col = "match_id") {

  cat("Adding GAM probability predictions...\n")

  # Generate raw predictions from GAM model
  raw_predictions <- predict(gam_model, newdata = test_data, type = "response")

  # Check if this is an OCAT model (returns probability matrix)
  if (is.matrix(raw_predictions)) {
    cat(sprintf("OCAT model detected - probability matrix: %d x %d\n",
                nrow(raw_predictions), ncol(raw_predictions)))

    # Raw probabilities for votes 0, 1, 2, 3
    test_data$pvotes0 <- raw_predictions[, 1]
    test_data$pvotes1 <- raw_predictions[, 2]
    test_data$pvotes2 <- raw_predictions[, 3]
    test_data$pvotes3 <- raw_predictions[, 4]

  } else {
    cat("Regular GAM model detected - creating probability approximations\n")
    # For regular GAM models, create rough probability estimates
    # This is a simplified approach - ideally you'd use the model's variance estimates
    predictions_capped <- pmax(0, pmin(3, raw_predictions))

    # Simple probability assignment based on predicted value
    test_data$pvotes0 <- ifelse(predictions_capped <= 0.5, 1 - predictions_capped, 0)
    test_data$pvotes1 <- ifelse(predictions_capped > 0.5 & predictions_capped <= 1.5,
                               1 - abs(predictions_capped - 1), 0)
    test_data$pvotes2 <- ifelse(predictions_capped > 1.5 & predictions_capped <= 2.5,
                               1 - abs(predictions_capped - 2), 0)
    test_data$pvotes3 <- ifelse(predictions_capped > 2.5, predictions_capped - 2, 0)

    # Normalize so they sum to 1
    prob_sum <- test_data$pvotes0 + test_data$pvotes1 + test_data$pvotes2 + test_data$pvotes3
    test_data$pvotes0 <- test_data$pvotes0 / prob_sum
    test_data$pvotes1 <- test_data$pvotes1 / prob_sum
    test_data$pvotes2 <- test_data$pvotes2 / prob_sum
    test_data$pvotes3 <- test_data$pvotes3 / prob_sum
  }

  cat("Normalizing probabilities within matches...\n")

  # Create match identifier if it doesn't exist or is NULL
  if (is.null(match_id_col) || !match_id_col %in% names(test_data)) {
    # Try different approaches to create match identifier
    if ("season" %in% names(test_data) && "round" %in% names(test_data) && "team" %in% names(test_data)) {
      # Create match_id from season + round (assuming 2 teams per match)
      test_data <- test_data %>%
        dplyr::arrange(season, round, team) %>%
        dplyr::group_by(season, round) %>%
        dplyr::mutate(
          teams_in_round = dplyr::n_distinct(team),
          match_id = paste0(season, "_R", round, "_M",
                           ceiling(dplyr::row_number() / (teams_in_round/ceiling(teams_in_round/2))))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-teams_in_round)
      match_id_col <- "match_id"
      cat("Created match_id from season + round columns\n")
    } else if (all(c("date", "team") %in% names(test_data))) {
      # Create match_id from date
      test_data <- test_data %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(match_id = dplyr::cur_group_id()) %>%
        dplyr::ungroup()
      match_id_col <- "match_id"
      cat("Created match_id from date column\n")
    } else if ("match_date" %in% names(test_data)) {
      # Use match_date as match identifier
      test_data <- test_data %>%
        dplyr::group_by(match_date) %>%
        dplyr::mutate(match_id = dplyr::cur_group_id()) %>%
        dplyr::ungroup()
      match_id_col <- "match_id"
      cat("Created match_id from match_date column\n")
    } else {
      cat("Available columns: ", paste(names(test_data), collapse=", "), "\n")
      stop("Cannot create match identifier - need one of: match_id, date, match_date, or (season + round + team)")
    }
  }

  # Normalize probabilities within each match so they sum to 1
  test_data <- test_data %>%
    dplyr::group_by(!!dplyr::sym(match_id_col)) %>%
    dplyr::mutate(
      # Calculate match-level probability sums
      match_sum_p0 = sum(pvotes0, na.rm = TRUE),
      match_sum_p1 = sum(pvotes1, na.rm = TRUE),
      match_sum_p2 = sum(pvotes2, na.rm = TRUE),
      match_sum_p3 = sum(pvotes3, na.rm = TRUE),

      # Create adjusted probabilities that sum to 1 within the match
      pvotes0_adj = dplyr::case_when(
        match_sum_p0 > 0 ~ pvotes0 / match_sum_p0,
        TRUE ~ 1 / dplyr::n()  # Equal probability if sum is 0
      ),
      pvotes1_adj = dplyr::case_when(
        match_sum_p1 > 0 ~ pvotes1 / match_sum_p1,
        TRUE ~ 0
      ),
      pvotes2_adj = dplyr::case_when(
        match_sum_p2 > 0 ~ pvotes2 / match_sum_p2,
        TRUE ~ 0
      ),
      pvotes3_adj = dplyr::case_when(
        match_sum_p3 > 0 ~ pvotes3 / match_sum_p3,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(-match_sum_p0, -match_sum_p1, -match_sum_p2, -match_sum_p3) %>%
    dplyr::ungroup()

  # Verify normalization
  prob_check <- test_data %>%
    dplyr::group_by(!!dplyr::sym(match_id_col)) %>%
    dplyr::summarise(
      sum_p0_adj = sum(pvotes0_adj, na.rm = TRUE),
      sum_p1_adj = sum(pvotes1_adj, na.rm = TRUE),
      sum_p2_adj = sum(pvotes2_adj, na.rm = TRUE),
      sum_p3_adj = sum(pvotes3_adj, na.rm = TRUE),
      .groups = "drop"
    )

  cat(sprintf("Probability normalization check (should be ~1.0):\n"))
  cat(sprintf("  P(0 votes) sums: %.3f +/- %.3f\n",
              mean(prob_check$sum_p0_adj), sd(prob_check$sum_p0_adj)))
  cat(sprintf("  P(1 votes) sums: %.3f +/- %.3f\n",
              mean(prob_check$sum_p1_adj), sd(prob_check$sum_p1_adj)))
  cat(sprintf("  P(2 votes) sums: %.3f +/- %.3f\n",
              mean(prob_check$sum_p2_adj), sd(prob_check$sum_p2_adj)))
  cat(sprintf("  P(3 votes) sums: %.3f +/- %.3f\n",
              mean(prob_check$sum_p3_adj), sd(prob_check$sum_p3_adj)))

  cat(sprintf("Added 8 probability columns to dataset (%d rows)\n", nrow(test_data)))
  return(test_data)
}

#' Normalize Ensemble Probability Matrices
#'
#' Convenience wrapper for normalizing probability matrices from ensemble predictions
#' Automatically handles the most common use case in the simulation pipeline
#'
#' @param prediction_data Data frame with player/match info and probability columns
#' @param prob_prefix Character prefix for probability columns (e.g., "ensemble_prob_")
#' @param match_id_col Character name of match ID column (default: "match_id")
#' @param create_match_id Logical whether to create match_id if missing (default: TRUE)
#' @param verbose Logical whether to show detailed output (default: FALSE)
#'
#' @return Original data frame with normalized probability columns added
#' @export

