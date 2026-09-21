# brownlow (development version)

This file starts at 0.2.0. Earlier versions are documented in the commit
history and in `POSTMORTEM-2025.md`.

# brownlow 0.2.11

* The post-mortem's leaderboard, sharpening-sweep and defender-sweep tables now
  write to `analysis/harness/` rather than the gitignored `results/`, so the
  numbers quoted in `POSTMORTEM-2026.md` have committed evidence behind them.

* POSTMORTEM-2026.md Sec 3b corrected. Swept over all twelve seasons rather than
  2023-2025, the defender multiplier has no stable optimum (per-season argmin
  scatters 0.4 to 1.2) and the pooled fit gains 0.00057 on 2026 — exactly on the
  decidability bar. The verdict moves from "worth nothing" to "real bias, no
  learnable correction", and Sec 5 now says not to adopt one.

* `analysis/postmortem_2026_whatif.R` persists its first sweep as soon as it
  completes instead of writing everything at the end, so an interrupted run no
  longer discards work it had already finished.

# brownlow 0.2.10

* Live in-count tracking. `analysis/live_count.R` pulls the AFL vote feed during
  the count, treats matches already read out as facts and re-simulates only the
  unread ones, driving the Brownlow Vote Tracker artifact via
  `analysis/live_refresh.ps1`.

* `simulate_counts_vectorised()` draws every simulation of a match at once using
  the exponential-race form of Plackett-Luce. Measured 37x faster than the
  per-simulation loop (`analysis/benchmark_vectorised_sampler.R`), and checked
  against the sequential sampler by `analysis/validate_vectorised_sampler.R`.

* `POSTMORTEM-2026.md`: we finished 7th of 23 at 0.29467 RMSE. Four theories for
  closing the gap were tested and are worth about 9% of it between them; the
  remainder is discrimination, not calibration.

* `analysis/adopt_rmse_rounds.R` appended to its provenance list rather than
  replacing by key, so after two runs the provenance reported 1,100 rounds while
  `best_iteration` held 1,000. Fixed, and the fix repairs an already-corrupted
  list as well as preventing new ones.

* `live_count.R` now stops rather than publishing a wrong number when a feed
  player name does not match our data, or when a match does not total 6 votes.

# brownlow 0.2.9

Bookmaker pricing off the 2026 simulation, no package code change.
`analysis/sportsbet_pull.R` reads every Brownlow market from Sportsbet's JSON
API; `analysis/price_markets_2026.R` prices each selection against a cached
per-round 5,000-season simulation and writes the Kelly table;
`analysis/ou_correlated_multis.R` prices player over/unders and two-leg
multis; `analysis/reprice.R` chains them with the artifact page build.
Runbook updated for count night.

# brownlow 0.2.8

Documentation only; no code change. `RUNBOOK.md`'s "Next session starts
here" block described the pre-2026-data state from 3 September -- replaced
with the actual current state (submission ready, training leak fixed,
normalizer speedup shipped, what's still queued), every number checked
against the artifact it came from rather than memory.

# brownlow 0.2.7

`normalize_probability_matrix()` -- used on the model's critical path in
03, 05, 06 and 08 -- replaced a per-player, per-column scalar R loop (up to
1000 power-transform iterations per match) with the same vectorised
Sinkhorn/IPF row/column balancing `normalize_votes_ipf()` already used for
the final vote-allocation step. Measured 183-190 seconds -- over 25% of a
pipeline stage's runtime -- down to ~24 seconds, and the old algorithm
never fully converged (82 match-level violations logged before this
change; the new one routinely hits exact 0.00e+00 error). Same function
name and return contract, so all 6 call sites got the speedup with no
edits needed there.

Verified this is a speed fix, not a quality change: full rebuild dropped
stage 05 13.7 -> 6.7 min and stage 03 11.7 -> 5.5 min; 2025 in-sample RMSE
moved 0.32057 -> 0.32002 (within noise); the submission leaderboard is
stable (Nick Daicos 42.17 -> 42.04 predicted votes).

Also: `R/enhanced_datathon_data.R` had two definitions of
`enhance_datathon_data()` -- the first was dead and broken (called
`load_datathon_data()` with an argument that function doesn't accept),
deleted. `prepare_full_training_data()`'s stale `holdout_seasons = c(2025)`
default (unused anywhere today) updated to `c(2026)`. Fixed two demo/test
scripts whose 3-player synthetic fixtures hit the (correctly ported)
`n >= 4` match-size guard -- real AFL matches have ~44 players, so the
pipeline itself was never affected.

Two rounds of independent Sonnet review; the first found no correctness
issue on the datathon-relevant path but caught a doc-generation regression
and the two demo/test script breaks, both fixed in a follow-up commit and
verified by direct script execution.

# brownlow 0.2.6

**A training data leak affected every 2026 prediction shipped in 0.2.5.**
`prepare_modeling_data()`'s `holdout_seasons` default (`R/model_utils.R`) is
a hardcoded `c(2025)`, never overridden by its one real caller. Combined
with a missing `brownlow_votes` being replaced with `0` during cleaning --
correct for a completed season, wrong for the current one -- 2026's entire
season (9,522 of 77,766 training rows, 12.2%) sat in `$train` with a
fabricated "0 votes" label for every player in every match, for the exact
season the model exists to predict.

Confirmed against the regenerated data directly: fixing this moved Nick
Daicos's predicted 2026 votes 29.75 -> 42.17 and win probability 73.6% ->
94.8% -- the systematic zero-vote drag from 12% fabricated rows, gone.

* `02_base_feature_engineering.R`: pass `holdout_seasons = c(2026)`
  explicitly, matching the existing explicit `test_seasons`. Update this
  every year alongside `test_seasons` and `TARGET_SEASON` -- see the new
  RUNBOOK.md rollover checklist item.
* `analysis/ensemble_weight_walkforward.R`: the "score against real holdout
  actuals" diagnostic assumed holdout == the season with known votes. Now
  explicitly checks the most recent season with real actuals (2025),
  relabeled IN-SAMPLE since 2025 is no longer excluded from training.
* `R/simulation_utils.R`: `load_simulation_data()` never branched on the
  holdout split at all, only train/test/val -- fixed the resulting
  misrouting for a genuinely future season, with a guard against a stale
  `predictions/{season}/` directory shadowing a resolved season's real
  votes.
* `07_season_predictions.R`: fixed an undefined-variable bug (dead code,
  never previously exercised for a real future season).
* `RUNBOOK.md`: added `holdout_seasons` to the year-rollover checklist.

Two rounds of independent Sonnet review; the second verified the fix's
mechanism, scope, and exact before/after numbers directly against the
regenerated data files, not just the code.

# brownlow 0.2.5

2025 holdout RMSE 0.35530 -> 0.35604 (walk-forward-selected blend). Within
one measured seed-noise sd (0.00060, POSTMORTEM Sec 5p) of the prior figure,
so not distinguishable from run-to-run noise -- not investigated further.

The 2026 Brownlow Datathon's submission form and the organiser's raw data
feed both changed schema this year, breaking the pipeline used to produce a
submission. This release fixes it and ships a working 2026 submission.

* `09_submission_predictions.R`: the 2026 form dropped `match_id`/`match_date`
  (only `player_id` remains). Recovers `match_date` via `(season,
  match_round, player_id)` against the raw feed, then rebuilds `match_id` as
  the pipeline's own synthetic `date_home_away` key (matching
  `R/performance_features.R`) rather than the raw feed's numeric one --
  verified empirically (exact 207/207 match) before shipping.
* Several `filter()`/`select()` calls fixed after discovering `stats::filter()`
  silently masks `dplyr::filter()` once `devtools::load_all()` attaches
  `mgcv`/`nlme` -- was producing "object not found" instead of filtering.
* `R/evaluation_harness.R`: `attach_holdout_actuals()` now canonicalises
  player names before joining, because the organiser's refresh renumbered
  `player_id` for every player and changed 15 players' names (e.g.
  `Robert Hansen Jr` -> `Robert Hansen`).
* `05_xgboost_model.R`: removed a redundant 7th XGBoost fit that existed
  only to source `xgb.importance()` -- reuses one of the 5 already-fitted
  seed-averaged models instead (~2 min/run saved, verified
  behaviour-preserving).
* Adds `wait_and_run.ps1`, a memory-gated wrapper for running pipeline steps
  one at a time when other sessions on this machine are contending for RAM.

# brownlow 0.2.4

Housekeeping only; no change to the model or its output (2025 holdout RMSE
stays 0.35530).

* Twelve one-off PowerShell runners consolidated into `run_chain.ps1`, which
  takes `-Steps` and `-LogName` and stops at the first failing step.
* `RUNBOOK.md` updated for the 5-seed averaged `05` and the stricter `06`
  staleness stop, and now opens with a next-session handover block.

# brownlow 0.2.3

2025 holdout RMSE 0.35546 -> 0.35530.

* Production model averaged over 5 seeds instead of a single `set.seed(42)`
  fit. The seed spread was measured at 0.00200 across 10 seeds -- larger than
  every improvement this project has banked. Averaging removes the lottery and
  beats the expected single seed.
* `06_ensemble_predictions.R`'s staleness check now also detects a change in
  the *prediction procedure* (e.g. single-seed vs seed-averaged), not just a
  change in hyperparameters or round count -- the gap seed averaging itself
  opened, closed the same day.
* Documented in `NEWS.md` because it matters for reproducibility: the 5-seed
  noise-floor estimates used throughout this project's history are themselves
  noisy. A 10-seed estimate gives sd 0.00060, roughly double the 0.00026 used
  before. See `POSTMORTEM-2025.md` Sec 5p.

# brownlow 0.2.2

2025 holdout RMSE 0.35556 -> 0.35546.

* New `add_match_relative_features()`, wired into `02_base_feature_engineering.R`.
  Expresses `supercoach_score`, `afl_fantasy_score`, `impact_score` and
  `disposals` as a rank and a share-of-maximum within each match. Votes are
  allocated within a match, and the two composites are absolute scores from
  fixed-weight formulas -- AFL Fantasy scores hitouts, and the 2026 removal of
  the centre bounce lowers ruck scores mechanically. A rank is invariant to a
  league-wide shift. **Adopted on that reasoning, not on the measurement**: the
  end-to-end gain was 0.00010, against 0.00041 in the isolated harness test.
* `assert_match_relative_clean()` verifies rank orientation on every column
  with a `_rank` counterpart. An inverted rank would reverse a feature's
  meaning while still producing plausible values in [0, 1].
* New `add_efficiency_features()` � impact per possession. **A documented null,
  deliberately not wired into the pipeline.** It separates its target
  population cleanly (players polling on low disposals show 5.896 impact per
  disposal against 4.077) but cannot move the score, because that population is
  ~1% of rows. Kept because a measured null with a verified mechanism is worth
  more than an untested idea.

# brownlow 0.2.1

Behaviour-preserving: the selected blend weight and the shipped predictions are
unchanged (verified at w = 0.55, 2025 holdout RMSE 0.35556).

* New `select_ensemble_weight()`, taking probability matrices, actuals and
  match ids rather than a `model_results` object, so whether a selection is
  out-of-sample is decided and visible at the call site. Defaults to
  `rmse_ipf`. Warns when the optimum lands on a grid edge, which usually
  indicates contaminated inputs rather than a genuine verdict.
* `optimize_ensemble_weights()` documented as superseded. Retained because
  `06_ensemble_predictions.R` still calls it to report what a contaminated
  search would have chosen, as a contrast to the honest weight.
* Fixed nine corrupted help pages. A file-level `#'` block with no object
  beneath it is adopted by roxygen into the next function, overwriting its
  title and prepending boilerplate to its description. Eight of the nine were
  pre-existing.

# brownlow 0.2.0

A minor rather than patch bump: this release changes what the shipped model
does, not just how it is built.

## Measurement fixes (2025 holdout RMSE 0.35836 -> 0.35556)

Three defects of the same family — a number computed on the wrong rows or
against the wrong metric. None involved adding a feature.

* **Ensemble weight was selected on in-sample predictions**
  (POSTMORTEM Sec 5j). Stage `05` retrains on 2015-2024 then writes
  probabilities for every row, so the 2023-24 rows used to fit the blend were
  in-sample for XGBoost. The contamination is asymmetric — a booster memorises,
  a shrunk GAM does not — so selection ran to the XGBoost cap. Now derived
  walk-forward by `analysis/ensemble_weight_walkforward.R` and persisted with
  provenance to `models/ensemble_weight_walkforward.rds`.
* **Hyperparameters were tuned under random k-fold CV** on time-series data
  (Sec 5g). Retuned on leak-free expanding-window folds.
* **Rounds were selected on log-loss while the competition scores RMSE**
  (Sec 5k). `optimal_nrounds()` and `tune_xgboost_honest()` gain
  `metric = c("mlogloss", "rmse_ipf")`. The two disagree 3x on rounds (400 vs
  ~1100) and not at all on hyperparameters.

## Behaviour changes

* `06_ensemble_predictions.R` reads the blend weight from a derived artifact
  and **stops** if it is stale or missing, rather than using a constant.
* It now persists the weight actually used, so `07`/`08` no longer rebuild
  their ensembles from the contaminated one.
* `optimal_nrounds()` returns `hit_ceiling` and warns, instead of printing an
  uncatchable note.
* `optimize_ensemble_weights()` warns that its selection is contaminated
  unless given out-of-sample predictions.

## New

* `RUNBOOK.md` — deadline-day procedure with measured timings, required
  console output, 2026 rule changes, and a symptom/cause/fix table.
* `R/team_form_features.R` — team form vs pre-season expectation. Measured as
  a null and deliberately **not** wired into the pipeline; kept because a
  measured null is worth more than an untested idea.

## Documentation

* `CLAUDE.md` corrected: the datathon scores RMSE, not multinomial calibration.
