# Deadline-day runbook — Brownlow Datathon 2026

## AFTER THE COUNT — start here (22 September 2026)

**The count is over and the datathon is scored. Nick Daicos won with 47 votes**
from Bailey Smith 36 and Marcus Bontempelli 34. We placed **7th of 23** at
**0.29467 RMSE**. Everything below this section is pre-count and historical;
read it for reference material, not for state.

**Read `POSTMORTEM-2026.md` first** — it is the analysis of the result and §5 is
the 2027 queue. Headlines:

- We pick the best-on-ground as often as the best entrant (72.0% vs 72.9%) but
  stake 2.04 expected votes on him against their 2.30. The gap is
  **discrimination, not calibration**.
- Four theories for closing it were tested and are worth **+0.00094 combined**
  against a 0.0222 gap. Do not re-open them; §3 says why each is dead.
- The one large lever: our XGBoost and GAM have an error correlation of
  **0.975**, so blending them buys almost nothing. A second member of the same
  quality at correlation **0.73** would put us level with the best entrant,
  worth about **+0.020**. See §5.

**Live count tooling, should you want it next September:**
`analysis/live_count.R` pulls the AFL vote feed during the count and
re-simulates only the unread matches; `analysis/live_refresh.ps1` drives it and
rebuilds the artifact page. Feed details and its traps are in the memory note
`afl-live-awards-feed` — most importantly it **503s a real browser but serves
curl from any origin**, so the page cannot fetch it itself.

**Not backed up:** `results/live/` holds 37 JSON snapshots of the feed taken
through the count, the only replay of a live count we have. It is gitignored and
exists on this machine only. Worth a piggyback release before it is lost.

## 21 September (count night) — pre-count reference

- **Submission SENT 20 Sep** (Pete emailed the CSV to datathon@betfair.com.au; MS To-Do item closed). Nothing further to do for the datathon until results.

- **Sims for market analysis are on disk, keep them:**
  `results/markets/votes_sims_2026_5000.rds` — integer matrix, rows are
  (player × round) cells in the order `analysis/kelly_markets_2026.R` builds
  (`cell_pid` / `cell_rnd`), columns are 5,000 simulated seasons, seed 2026,
  Plackett-Luce sampler on the 2026 ensemble probabilities. Sibling CSVs:
  `sportsbet_2026_kelly.csv` (every priced line, 20 Sep odds),
  `expected_votes_by_round_2026.csv`, `player_summary_2026.csv`.
  Re-running the script reads the cache (20 s); delete the RDS to redraw.
- **Artifact:** https://claude.ai/artifact/GdnuLLmYyM5G5zzwNJHJfT (by-round
  xV table + value-bet tab). Republish from this repo's session or pass the URL.
- **Sportsbet live prices (VERIFIED 20 Sep, works from plain curl):**
  `https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{eventId}/Markets`
  (`/SportMarkets` is a 404) with a browser User-Agent and `Accept: application/json`;
  class listing `.../Sportsbook/Sports/Competitions/27508/Events` (Brownlow, 20 events).
  Competition ids: 6136 (medal, hub, exotics, leader-after, groups), 16767
  (3-vote games, one market per match), 28057 (team most votes), 24522 (poll
  N+ / to poll a vote), 27508 (player O/U). Raw pulls: `results/markets/sportsbet_raw/<stamp>/`.
- **ONE COMMAND to reprice everything and rebuild the page (~50 s):**
  `powershell.exe -Command 'Rscript "analysis/reprice.R"'` = `sportsbet_pull.R`
  (all 5 classes → `sportsbet_lines_latest.csv`, 2,113 selections on 20 Sep)
  → `price_markets_2026.R` (generic pricer keyed on event/market name; prints
  any unpriced lines, 0 on 20 Sep) → `ou_correlated_multis.R` →
  `build_artifact_data.R` (writes `results/markets/brownlow-vote-tracker.html`).
  Then publish that file with the Artifact tool passing
  `url = https://claude.ai/artifact/GdnuLLmYyM5G5zzwNJHJfT`. Page templates live
  in `analysis/artifact/head.html` + `tail.html`. Pattern is written up for the
  other verses in `C:\dev\ECOSYSTEM.md` §3.
- Kelly caveat carried over: Gawn / Jackson / Grundy lines dominate the edge
  list and are one correlated ruck opinion; ruck calibration 2019-25 checks out
  (actual/pred 0.93-1.11) so it is a real view, size it as one bet.

## Next session starts here — SUPERSEDED, see the post-count section at the top

**State as of 12 September 2026, before the count.** Kept for its detail on how
the submission was built; for current state read POSTMORTEM-2026.md.

**Original note follows.** The 2026 submission is built, correct, and
merged (`main` at v0.2.7, `dev` fully merged, tree clean). Registered as
**InTheGame** (Discord `random_effects153`). Everything below is either done,
queued, or explicitly left for Pete.

**Where it stands (verified, not assumed):**
- `submissions/Brownlow_Medal_Datathon_2026_Submission_Form_V2_InTheGame.csv`
  is the file to send: 9,522 rows, every one of 207 matches totals 6.0 votes,
  columns match the organiser's template exactly.
- Nick Daicos is the clear favourite: **42.04 predicted votes**, **94.2% win
  probability**, 99.9% top-5 (2,000-run simulation,
  `results/brownlow_dashboard_data_2026.json` — this is a fresh Monte Carlo
  draw each rebuild, so expect it to move a fraction of a point run to run;
  don't treat the decimal as more precise than that).
- Honest generalisation estimate is now the **walk-forward 2023-24 RMSE,
  0.397572** (`analysis/harness/ensemble_weight_walkforward.csv`), not a 2025
  holdout figure — 2025 is now correctly folded into training (see below), so
  there is no clean holdout season left except 2026 itself, which has no
  known votes to score against.
- An **email draft is already sitting in Outlook** (`pete.owen1@hotmail.co.uk`,
  subject "Brownlow Datathon 2026 submission - InTheGame", to
  `datathon@betfair.com.au`) — it needs the CSV attached and sending. A
  reminder is set in MS To-Do (`AFL` list) for 20 September so this doesn't
  get missed.

**What actually happened this session (2026-09-11/12), for context on why the
numbers moved from whatever an older session remembers:**
1. The 2026 template and raw feed both changed schema (dropped `match_id`/
   `match_date`, renumbered every `player_id`, changed 15 players' names,
   stripped punctuation from surnames). Fixed in `09_submission_predictions.R`
   and `R/evaluation_harness.R` — PR #8.
2. **Serious bug found and fixed**: `prepare_modeling_data()`'s
   `holdout_seasons` default was a stale `c(2025)`, so 2026's placeholder
   zero-vote rows (9,522 of them, 12.2% of training) were being trained on as
   if they were real "0 votes" outcomes. Fixed by passing `holdout_seasons =
   c(2026)` explicitly in `02_base_feature_engineering.R` — **update this
   every year**, it will recur exactly the same way in 2027 otherwise. PR #9.
   Removing the leak moved Daicos's predicted votes 29.75 -> 42.17 and win
   probability 73.6% -> 94.8%.
3. `normalize_probability_matrix()` rewritten from a scalar per-player loop
   (183-190 sec, never fully converged) to vectorised Sinkhorn/IPF matching
   `normalize_votes_ipf()` (~24 sec, exact convergence). Pure speed win,
   verified RMSE-neutral. PR #10.
4. `07_season_predictions.R` and `08_brownlow_simulation.R` had never been
   run for a genuinely in-progress future season before (only past seasons
   already resolved into test/val) — both had latent bugs (an undefined
   variable, and season-routing that didn't know about the holdout split)
   that only surfaced today. Fixed as part of PR #9.

**Queued, not started:**
- **Single-seed vs averaged asymmetry** (carried over from 3 September,
  still true): production averages 5 seeds; the walk-forward weight arms in
  `analysis/ensemble_weight_walkforward.R` are still single-seed. ~45 min fix,
  on a weight curve flat across 0.30-0.55, so genuinely second-order.
- Two pre-existing broken demo/test call sites, confirmed unrelated to
  anything this session touched: `test_probability_normalization.R`'s Test 4
  calls `apply_match_probability_constraints(..., return_full_results = TRUE)`,
  an argument that function doesn't accept (present since the initial
  commit); `demo_probability_normalization.R`'s Example 2 calls
  `normalize_ensemble_probabilities()`, which no longer exists anywhere in
  `R/`. Neither affects the pipeline. Not fixed — flagged and left.
- `prepare_full_training_data()` and `R/enhanced_datathon_data.R`'s dead
  first `enhance_datathon_data()` definition were both cleaned up (PR #10),
  no further action.

**Undecided, needs Pete:** the match-relative composites (§5o in
`POSTMORTEM-2025.md`) were adopted on reasoning, not evidence — end-to-end
gain was 0.00010 against the 0.00041 the harness suggested. Reverting is one
line in `02` plus a chain rerun. Left in on the 2026 shift-robustness
argument; 2026 data now exists to actually test this, but it hasn't been
re-measured since it landed.

**Deadline: 2:59pm AEST, Monday 21 September 2026** (Discord announcement
says 3pm — treat as the same cutoff, submit well before either). Registration
closed 17 September. Submission is by **email**, not a portal — see
`datathon-2026-registration` memory for the full mechanism.

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
5. **Set `holdout_seasons = c(<current year>)` at the `prepare_modeling_data()`
   call in `02_base_feature_engineering.R`**, alongside the existing explicit
   `test_seasons`. Its default is a stale `c(2025)` in `R/model_utils.R` and
   nothing enforces overriding it. Missing this does NOT fail loudly: a
   missing `brownlow_votes` gets replaced with 0 during cleaning
   (`R/datathon_data.R`), so the current season's placeholder zeros silently
   pass the NA-filter and land in `$train` as fabricated "0 votes" labels for
   every player in every match of the season being predicted -- 9,522 rows
   (12.2% of training) in the 2026-09-12 incident, discovered only because
   the model's own predictions looked suspiciously suppressed. Verify by
   checking `readRDS("data/processed/modeling_data.rds")$holdout$season`
   equals the current year, not the previous one, after running `02`.

---

## 2. The run, in order

Run from `C:\dev\brownlow`. Use PowerShell, not Git Bash — `arrow` segfaults
under Git Bash R, and inline `Rscript -e` has segfaulted on this machine.

`run_chain.ps1` runs any sequence of steps and **stops at the first failure**
rather than letting later stages run on stale inputs:

```powershell
pwsh -File run_chain.ps1 -Steps "02_base_feature_engineering.R","03_gam_model.R",`
  "04_enhanced_feature_engineering.R","05_xgboost_model.R",`
  "analysis/ensemble_weight_walkforward.R","06_ensemble_predictions.R" -LogName deadline
```

For a long run, launch it detached so it survives the caller:

```powershell
Start-Process pwsh -ArgumentList '-NoProfile','-NonInteractive','-File',`
  'C:\dev\brownlow\run_chain.ps1','-Steps','05_xgboost_model.R' -WindowStyle Hidden
```

| stage | what it does | measured time |
|---|---|---|
| `02_base_feature_engineering.R` | features from the raw CSV | 1.5 min |
| `03_gam_model.R` | GAM + player random effects | 7.8 min |
| `04_enhanced_feature_engineering.R` | adds `player_coef` | 0.3 min |
| `05_xgboost_model.R` | trains 5 seeds, averages, writes probabilities | 10-13 min |
| `analysis/ensemble_weight_walkforward.R` | derives the blend weight | 9.6 min |
| `06_ensemble_predictions.R` | blends, writes the file `09` reads | 0.3 min |
| `09_submission_predictions.R` | builds the submission | seconds |

**`06` will refuse to run without the weight artifact.** It reads
`models/ensemble_weight_walkforward.rds`, which is gitignored and therefore
absent on a fresh clone — run `analysis/ensemble_weight_walkforward.R` first.
It fails with a message saying exactly that, so this cannot pass silently.

**Rerun the weight whenever the model changes.** The blend weight is fitted
against a specific booster: when the round count went 391 -> 1100 the optimal
weight moved 0.40 -> 0.55, and adding features moved it again to 0.35. Keeping
an old weight across such a change mis-sets the ensemble.

`06` **stops** (not warns) if the weight artifact's recorded hyperparameters,
round count, or **prediction procedure** disagree with what is currently on
disk. That last one matters: seed averaging changed nothing in the
hyperparameters, so an earlier version of the check would have accepted a
stale weight silently. Treat the stop as instruction to rerun
`analysis/ensemble_weight_walkforward.R`, not as an obstacle to work around.

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
