# brownlow (development version)

This file starts at 0.2.0. Earlier versions are documented in the commit
history and in `POSTMORTEM-2025.md`.

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
