# Impact per possession ---------------------------------------------------------
#
# POSTMORTEM Sec 3's third miss pattern is players who poll on modest disposal
# counts. The 2026 rule change makes this more pressing rather than less: the
# reform was triggered by the Rowell / Wanganeen-Milera controversy of
# September 2025, exactly this profile, and Rowell is also our single biggest
# 2025 miss.
#
# Audited the 267 existing features first, since the player_position idea died
# on redundancy (Sec 5m). There are per-minute rates (disposals_per_minute),
# team-share ratios (player_impact_share) and possession-mix ratios
# (contested_ratio, disposal_efficiency, clearance_efficiency) — but **nothing
# divides an impact measure by disposals**. The gap is real.
#
# Worth stating why explicit ratios can help a gradient-boosted tree at all,
# since it is not obvious: a tree splits on thresholds of single features, so
# it cannot represent x / y natively. Approximating a ratio means carving the
# joint (x, y) space with many axis-aligned splits, which is exactly the shape
# a GBM spends depth on and still only approximates. Supplying the ratio
# directly is one of the few feature-engineering moves that reliably buys
# something for this model class.
#
# NOT leakage: every input is a current-game box-score stat the model already
# uses (impact_score, score_involvements, metres_gained, disposals). This adds
# no new information about the game — it re-expresses information already
# present in a form the model cannot easily construct.

#' Add Impact-Per-Possession Features
#'
#' @description
#' Re-expresses existing current-game statistics as impact per disposal, plus a
#' match-relative version of the headline ratio. Votes are allocated within a
#' match, so how a player's efficiency compares to the other 45 players that
#' day is the quantity that matters, not its absolute level.
#'
#' Guards against division by zero with a floor of one disposal rather than an
#' `ifelse(..., 0)`: a player with no disposals and some impact should read as
#' high efficiency on tiny volume, not as zero.
#'
#' @param stats DataFrame with `impact_score`, `score_involvements`,
#'   `metres_gained`, `intercepts`, `spoils`, `one_percenters`, `disposals` and
#'   `match_id`
#' @return `stats` with the efficiency features added
#' @export
add_efficiency_features <- function(stats) {
  needed <- c("impact_score", "score_involvements", "metres_gained",
              "intercepts", "spoils", "one_percenters", "disposals", "match_id")
  missing <- setdiff(needed, names(stats))
  if (length(missing) > 0) {
    stop(sprintf("add_efficiency_features() needs: %s", paste(missing, collapse = ", ")))
  }

  out <- stats %>%
    dplyr::mutate(
      .denom = pmax(dplyr::coalesce(disposals, 0), 1),
      impact_per_disposal      = dplyr::coalesce(impact_score, 0) / .denom,
      score_inv_per_disposal   = dplyr::coalesce(score_involvements, 0) / .denom,
      metres_per_disposal      = dplyr::coalesce(metres_gained, 0) / .denom,
      defensive_acts_per_disposal =
        (dplyr::coalesce(intercepts, 0) + dplyr::coalesce(spoils, 0) +
           dplyr::coalesce(one_percenters, 0)) / .denom
    ) %>%
    # Votes are a within-match competition, so the comparison that matters is
    # against the other players in THIS match, not against the league.
    dplyr::group_by(match_id) %>%
    dplyr::mutate(
      impact_per_disposal_vs_match = impact_per_disposal - mean(impact_per_disposal, na.rm = TRUE),
      impact_per_disposal_rank     = dplyr::min_rank(-impact_per_disposal) / dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.denom)

  new_cols <- c("impact_per_disposal", "score_inv_per_disposal", "metres_per_disposal",
                "defensive_acts_per_disposal", "impact_per_disposal_vs_match",
                "impact_per_disposal_rank")
  if (any(vapply(out[new_cols], function(x) any(!is.finite(x)), logical(1)))) {
    stop("Efficiency features contain non-finite values; check the disposal floor.")
  }
  out
}
