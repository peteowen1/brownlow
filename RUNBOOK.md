# Deadline-day runbook — Brownlow Datathon 2026

**Deadline: 2:59pm AEST, Monday 21 September 2026.** Template released no later
than **8 September 2026**. Registration closes 17 September.

Written 2026-09-02, after a full clean rerun, so every timing below is measured
rather than estimated. The whole pipeline is ~15 minutes. The risk on the day is
not compute, it is doing the steps in the wrong order or trusting a stale
artifact.

---

## 0. What we are actually scored on

**RMSE per individual prediction.** Not log-loss, not calibration, not who wins
the medal. Verified against the 2025 and 2026 Terms & Conditions, which use
identical wording. See POSTMORTEM-2025.md Sec 5k — `CLAUDE.md` asserted the
opposite until 2026-09-02.

Submission is **every player in every match**: 46 players x 207 matches =
**9,522 rows**. Suspended players still accrue votes and must still be
predicted (T&Cs clause 12) — `data/brownlow_ineligible.csv` is about *medal
eligibility* for the simulation workstream, and must NOT be used to zero out
rows in the submission.

Unsubmitted rows default to 6/46. One submission per entrant; resubmitting is
allowed and the last file before the deadline is the one scored.

---

## 1. When the template lands (8 September)

1. Save it to `data/Brownlow_Medal_Datathon_2026_Submission_Form.csv`.
2. Open it and check the column names against the 2025 form, which were:
   `match_date, match_round, match_id, match_home_team, match_away_team,
   player_id, player_first_name, player_last_name, player_team, brownlow_votes`.
   If they changed, `09_submission_predictions.R`'s join needs updating.
3. Confirm the row count is 9,522. If 2026 squads are 23 per side with no
   substitute (see Sec 4), 46 per match still holds.
4. Set `TARGET_SEASON <- 2026` at the top of `09_submission_predictions.R`.
   The script `stop()`s with a clear message if the form is missing, so a
   forgotten flip fails loudly rather than silently submitting 2025.

---

## 2. The run, in order

Run from `C:\dev\brownlow`. Use PowerShell, not Git Bash — `arrow` segfaults
under Git Bash R, and inline `Rscript -e` has segfaulted on this machine.

`run_pipeline_02_06.ps1` does stages 02-06 in sequence and **stops at the first
failure** rather than letting later stages run on stale inputs.

| stage | what it does | measured time |
|---|---|---|
| `02_base_feature_engineering.R` | features from the raw CSV | 1.5 min |
| `03_gam_model.R` | GAM + player random effects | 7.8 min |
| `04_enhanced_feature_engineering.R` | adds `player_coef` | 0.3 min |
| `05_xgboost_model.R` | trains, writes probabilities | 6.2 min |
| `analysis/ensemble_weight_walkforward.R` | derives the blend weight | 9.6 min |
| `06_ensemble_predictions.R` | blends, writes the file `09` reads | 0.3 min |
| `09_submission_predictions.R` | builds the submission | seconds |

**`06` will refuse to run without the weight artifact.** It reads
`models/ensemble_weight_walkforward.rds`, which is gitignored and therefore
absent on a fresh clone — run `analysis/ensemble_weight_walkforward.R` first.
It fails with a message saying exactly that, so this cannot pass silently.

**Rerun the weight whenever the model changes.** The blend weight is fitted
against a specific booster: when the round count went 391 -> 1100, the optimal
weight moved 0.40 -> 0.55. Keeping the old weight across that change would
have mis-set the ensemble. `06` compares the artifact's recorded
hyperparameters against current tuning and warns on drift, but the warning is
a backstop, not permission to skip the step.

Then `10_submission_analysis.R` to eyeball it.

**Do not skip 02.** Everything downstream is built on its output, and the leak
fixes live in its assertions.

---

## 3. Checks that must pass — in the order you will see them

**Stage 02** prints three assertions. All three must appear:

```
Career performance check passed: 12 columns zero on all N debut games
Leakage check passed: 9 career averages are zero on all N debut games
Last-season check passed: 9 columns zero for all N rows of every player's debut season
```

Then confirm the split summary: train/test/holdout seasons, and that the
holdout IS the prediction season with votes zeroed.

**Stage 05** prints `Loading cached tuning results...` — this is the trap that
cost us on 2026-09-02. It silently reuses `models/xgboost_tuning_results.rds`
regardless of whether the feature set changed. Check the printed
`Best CV log-loss` and `optimal rounds` match what you expect. If features
changed, re-run `analysis/honest_retune_267.R` and `analysis/adopt_honest_tuning.R`
first, then rerun 05.

**Stage 06** must print the walk-forward weight, not the in-sample one:

```
Ensemble weights (walk-forward, Sec 5j): XGBoost=0.400, GAM=0.600  [in-sample search wanted 0.858]
```

If it prints `LEAKY`, `USE_WALKFORWARD_WEIGHT` got flipped to FALSE. The
in-sample search is contaminated and drives the weight to the cap — see
POSTMORTEM Sec 5j.

**Stage 09** — verify before submitting:

- row count equals the form's row count exactly (no join fan-out, no drops)
- no `NA` predictions anywhere
- total predicted votes ≈ `6 x number of matches` (1242 for 207 matches).
  IPF makes this near-exact; 2025 verified at 1242.0 against 1242 actual.
- every prediction in `[0, 3]`
- spot-check that the top handful of predicted vote-getters are plausible
  midfielders, not fringe players — a scrambled prediction matrix once made
  this look fine on aggregate while every row was wrong (Sec 4)

---

## 4. What is different about 2026

The 2026 season is the least like its training history of any season we have.
None of this is handled automatically; it is here so nobody is surprised.

1. **Umpires may now consult statistics before finalising votes** — first time
   ever. This changes the target itself, not just the inputs. Every training
   season reflects umpires voting from memory.
2. **Sub rule scrapped** — 23-player squads, 5-man bench, no `SUB` role. The
   `SUB` position value will not appear in 2026 data.
3. **Centre bounce scrapped** — ball-ups instead, ruckmen barred from crossing
   the circle early. Directly affects `hitouts`, `hitouts_to_advantage`,
   `ruck_contests`, `clearances`.
4. **Last-disposal out-of-bounds free kick**, shrug-in-tackle counts as prior
   opportunity, kick-in clock 12s to 8s.
5. Tasmania enters **2028**. 18 clubs in 2026, no change.

---

## 5. Traps that have actually bitten, with their symptoms

| symptom | cause | fix |
|---|---|---|
| `object '<column>' not found` in a script that looks correct | bare `filter()` resolving to `stats::filter`, because `devtools::load_all()` runs after `library(dplyr)` and shifts the search path | always `dplyr::filter()` in analysis scripts |
| a join silently matches 0 rows | `match_id` is a constructed string (`2015-04-02_Carlton_Richmond`) in the processed data but the AFL numeric id (`14581`) in the raw CSV — zero overlap | join raw on `(player_id, date)` |
| all dates parse to `NA` | raw `match_date` is `d/m/Y` | `as.Date(x, format = "%d/%m/%Y")` |
| model looks fine on aggregate but every row is wrong | reshaping an already-correct matrix scrambles rows while leaving column means intact | use `as_prob_matrix()` |
| `Rscript` dies with exit 139 and no message | wrong working directory, or inline `-e` | run from the repo root, use script files |
| a long run dies at the very end with a syntax error | the script was edited while running; Rscript re-reads the file as it goes | never edit a script mid-run; copy it first |
| stage 05 finishes suspiciously fast | it reused cached tuning | see Sec 3 |

---

## 6. If something is wrong and time is short

The artifacts from the last good run are the fallback. `data/processed/` and
`models/` are gitignored and regenerable, but a full rerun is only ~15 minutes,
so prefer rerunning cleanly over patching a broken artifact.

Back up before overwriting:

```powershell
Copy-Item -Recurse data\processed <somewhere>\pre_run_backup\
Copy-Item -Recurse models <somewhere>\pre_run_backup\
```

**Submit something rather than nothing.** Unsubmitted rows score as 6/46, which
is worse than any calibrated model we have. If the pipeline is broken at 2pm,
submit the last known-good `results/ensemble_probabilities.rds` through `09`.
