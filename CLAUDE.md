# CLAUDE.md

Brownlow Medal Prediction R Package — AFL Brownlow votes prediction using ML ensemble (XGBoost + GAM). Built for the Betfair Brownlow Datathon.

See `ARCHITECTURE.md` for function reference, design patterns, and competition details.

## Core Commands

```r
devtools::load_all()  # Load package functions (NEVER use source() or library() for R/ functions)
```

### Analysis Pipeline (Run in Order)
```r
source("01_data_exploration.R")           # EDA and voting patterns
source("02_base_feature_engineering.R")   # Base features (no GAM coefficients)
source("03_gam_model.R")                  # GAM with smooth terms + random effects
source("04_enhanced_feature_engineering.R") # Add GAM player coefficients
source("05_xgboost_model.R")             # XGBoost training (~1-2 hours)
source("06_ensemble_predictions.R")       # Optimal ensemble + final predictions
source("07_season_predictions.R")         # Season-level + competition submissions
source("08_brownlow_simulation.R")        # Monte Carlo winner simulations
source("09_submission_predictions.R")     # Produces the Datathon submission file
source("10_submission_analysis.R")        # Analyses the Datathon submission
```

## Key Rules

### Data
- **Always start from** `enhance_datathon_data()` — cleanest dataset with actual votes
- All columns must be **snake_case** — use `janitor::clean_names()`
- Never edit NAMESPACE directly — roxygen2 generates it

### Modeling Philosophy
**The datathon scores RMSE.** Verified 2026-09-02 against the 2025 and 2026
Terms & Conditions, which use identical wording: "average RMSE for each
individual prediction," lowest wins. The FAQ adds "you should use a regression
model, not a classification model."

This file previously said to optimise multinomial probability calibration with
priority `log-loss > ECE > Brier`. That was wrong about the competition. See
POSTMORTEM-2025.md Sec 5k.

**That gap is now closed, and measuring it settled the question** — see
POSTMORTEM-2026.md Sec 3d. `optimal_nrounds()` still *defaults* to
`validation_mlogloss`, but the production round count was re-selected on
`metric = "rmse_ipf"` (391 -> 1,000, adopted 2026-09-02 via
`analysis/adopt_rmse_rounds.R`), and `tune_xgboost_honest()` re-run under RMSE
chose **identical** hyperparameters. Both changes measured inside the 0.00055
decidability bar. Do not re-open this as an improvement idea: it is done, and it
was worth nothing.

What to do:
- **Report and compare on RMSE after IPF**, via `evaluate_seeds()`. Every
  number in POSTMORTEM-2025.md is already RMSE, so the measurement layer was
  never affected.
- A multinomial (`multi:softprob`) model is still the right *structure* —
  votes are 0/1/2/3 with exactly one 3, one 2 and one 1 per match, and IPF
  needs a probability matrix to enforce that. Predicting probabilities and
  taking the expectation is a regression on votes; that is not the same
  mistake as optimising classification accuracy.
- Calibration remains worth checking as a diagnostic. It is not the objective.

### Time-Aware Validation (CRITICAL)
- Train on historical seasons up to cutoff, validate on next season, test on most recent 2
- **Never** randomly shuffle time-series data or train on future data
- Perfect performance (AUC = 1.0) = data leakage

### Data Leakage Prevention
```r
# BAD: uses current-game target variable
games_since_last_vote = ifelse(brownlow_votes > 0, 0, ...)

# GOOD: uses only historical info via lag
games_since_last_vote = calculate_using_lag(dplyr::lag(brownlow_votes, default = 0))
```
All rolling/cumulative features must use `dplyr::lag(default = 0)` to exclude current observations.

### Analysis Scripts (01-08)
- **NO function definitions** — all functions belong in `R/` directory
- **NO complex logic blocks** — >5-10 lines of processing → make it a function
- **Target**: 50-100 lines per script, calling utility functions from `R/`
- Use `library()` calls only here (never in `R/` files — use `pkg::fn()` there)

### File Organization
- `R/` — Package functions (exported, use `pkg::fn()` syntax)
- `01_*` to `08_*` — Pipeline scripts (run in sequence)
- `data/processed/` — Cached intermediates
- `models/` — Trained model objects
- `results/` — Predictions and metrics
- `submissions/` — Competition files

## Simplicity Rules

- Write minimal code that works — 10 lines over 100
- Let code error clearly instead of adding fallbacks or graceful degradation
- Don't wrap everything in tryCatch with fallback strategies
- Use library functions directly (e.g., `mixedup::extract_ranef()`) instead of writing wrappers
- The user can handle error messages — explicit failures > hidden bugs

## Related Projects

AFL-adjacent. See `C:\dev\torpverse\` for the main AFL analytics ecosystem (torp package, TORP ratings, EP/WP models). Both use fitzRoy-sourced AFL data and share XGBoost+GAM modeling approaches. For full ecosystem: `C:\dev\ECOSYSTEM.md`
