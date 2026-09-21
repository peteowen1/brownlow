# 2026 Datathon Post-Mortem

Written the night of the count, 2026-09-21, while the numbers were fresh.
Companion to POSTMORTEM-2025.md; section numbering is independent.

Nick Daicos won with 47 votes from Bailey Smith 36 and Marcus Bontempelli 34.
All 207 matches, 1,242 votes.

---

## 1. How we did

Entered as **InTheGame**. Final RMSE **0.29467**, **7th of 23** in the community
comparison sheet (see `analysis/live_rmse.R` for the scorer, and the memory note
`brownlow-predictions-sheet` for the sheet's quirks).

| Rank | Model | RMSE | MAE |
|---|---|---|---|
| 1 | consensus *(mean of all entrants, not a competitor)* | 0.27195 | 0.07717 |
| 2 | jt | 0.27246 | 0.08143 |
| 3 | qp | 0.27688 | 0.08985 |
| 4 | wheelo | 0.28934 | 0.08631 |
| 5 | bg2 | 0.29112 | 0.06999 |
| 6 | bg1 | 0.29368 | 0.06964 |
| **7** | **InTheGame** | **0.29467** | 0.09290 |
| … | | | |
| 23 | afl | 0.38401 | 0.08676 |

Scored on the 9,094 rows every entrant filled in. Our MAE rank (19th of 23) is
far worse than our RMSE rank, which is the first clue to §2.

**The model called the winner.** Daicos was our favourite before a vote was read
(97.9%) and never fell below 98% during the count.

**`consensus` beat every individual entrant.** That is the single most useful
number in the table and it is discussed in §5.

---

## 2. Where the loss actually sits

Full detail in `analysis/postmortem_2026_misses.R`, which runs on all 9,522 rows
with actuals taken from the AFL live feed.

**We identify the best-on-ground as often as the best entrant does. We stake
less on him.**

| | Top pick polled the 3 | Mean prediction on the real 3-vote winner |
|---|---|---|
| ours | 72.0% (149/207) | 2.040 |
| jt | 72.9% (151/207) | 2.296 |
| consensus | 74.4% (154/207) | — |

Two matches out of 207 separate our discrimination from `jt`'s — that is noise.
But **68% of the entire RMSE gap to `jt` sits on the 207 rows that polled 3**
(82.1 of 120.7 squared-error units). On the 1-vote rows we beat him.

Calibration agrees, conditioning on our own prediction rather than on the
outcome (the direction that regression-to-the-mean cannot fake):

| Predicted band | Rows | Predicted ÷ actual | Share of squared error |
|---|---|---|---|
| 0.05–0.25 | 780 | 1.27–1.49 | 13.3% |
| 0.25–1.00 | 543 | 0.97–1.06 | 41.0% |
| 1.50+ | 293 | 0.90–0.92 | 23.4% |

We bleed probability off the genuine stars and spread it across the long tail.
That is also exactly why MAE ranks 19th while RMSE ranks 7th.

---

## 3. Four theories, all tested, all worth almost nothing

The decidability bar for this setup is **0.00055** RMSE (pooled seed sd 0.00028,
doubled — POSTMORTEM-2025 §5a and §5p). Anything below it is not a result.

| Theory | Verdict | Worth |
|---|---|---|
| Under-confidence — sharpen the favourite | Real | **+0.0021** ceiling; +0.0020 on a recent-season fit |
| Defender over-prediction | 2026-only artefact | 0 out of sample |
| Stale player effects | Direction right, magnitude null | ~0 — skip the GAM refit |
| Selection on mlogloss not RMSE | Already fixed pre-season, both axes | −0.0004, inside noise |

Against a **0.0222** gap to `jt`, all four together are worth about 0.002 —
**9%**.

### 3a. Sharpening — the only effect above the noise bar

`analysis/postmortem_2026_whatif.R`. Raise the 1/2/3 columns to a power, re-run
IPF so each match still carries exactly one 3, one 2 and one 1. At beta = 1 the
transform is the identity, asserted in the script to 1e-6 (measured 3.35e-09).

2026 RMSE: 0.29140 at beta 1.0 → **0.28931 at beta 1.4** (the hindsight
optimum). Beta fitted on 2015-2025 picks 2.0 and gives 0.29084. The three recent
seasons all optimise at 1.2–1.4, so a "last three seasons" rule would have
picked 1.3 and captured 0.00199 — but that rule was only noticed *after* seeing
it work, so it is a post-hoc selection, not a validated one.

Worth roughly one place on the leaderboard, not three.

### 3b. Defenders — a one-season artefact, not a model flaw

We gave backs 161 season votes against a real 100, and took 58 off the
midfielders. That looked like a systematic feature problem. It is not.

Defender multiplier, RMSE by season (`analysis/postmortem_2026_arms_pq.R`):

| Factor | 2023 | 2024 | 2025 | 2026 |
|---|---|---|---|---|
| 0.4 | 0.33924 | 0.33234 | 0.32157 | **0.28839** |
| 0.8 | **0.33806** | 0.33046 | 0.31939 | 0.29027 |
| 1.0 | 0.33813 | **0.33035** | 0.31913 | 0.29140 |
| 1.1 | 0.33824 | 0.33039 | **0.31909** | 0.29196 |

2026 wants heavy damping; **every prior season wants a factor of ~1.0**, and
2025 marginally wants defenders pushed *up*. Fitted honestly the rule picks 1.0
and delivers nothing.

*Loose end:* if 2023-25 were in-sample for the model, a good fit there could
mask a real bias. Evidence against — 2026 scores better (0.291) than all three
(0.319–0.338), which is backwards for in-sample rows.

### 3c. Player effects — real direction, null magnitude

The GAM player coefficient is a career prior, so players who changed club or
broke out should be under-predicted. They are: club-changers by +0.0213 votes a
game (n=540), under-22s by +0.0081, over-32s over-predicted by −0.0150.

Then size it. Against σ = 0.2914, an effect of spread 0.021 removes about
`s²/2σ² = 0.27%` of RMSE, on 5.7% of rows.

The cheap screen (POSTMORTEM-2025 discipline; torp's §9 rule) confirms it:

| Candidate | Δ adjusted R² | Partial correlation |
|---|---|---|
| age | 0.000217 | −0.0292 |
| changed club | 0.000089 | +0.0203 |
| new to the data | 0.000077 | +0.0192 |

torp's box-score feature program was harness-tested and confirmed null at a max
partial correlation of **0.077**. Ours are 0.019–0.029, well below a feature set
already proven worthless. **The GAM refit was correctly skipped.**

### 3d. mlogloss vs RMSE — already done, and worth nothing

POSTMORTEM-2025 §5k flagged this and it was **acted on before the season**:

Each row below is one logged run, cited separately, because they are **not** a
single before/after measurement and an earlier draft of this table wrongly
spliced three of them together:

| Run | What it compared | Result |
|---|---|---|
| `rmse_sel.log` | round count, 400 (mlogloss argmin) vs 1,200 (rmse_ipf argmin) | 0.35690 → 0.35648, **−0.00042** |
| `rmse_tune.log` | hyperparameters, mlogloss-tuned vs rmse_ipf-tuned, both held at 1,100 rounds | **identical parameters**, 0.35642 → 0.35642, **0** |
| `honest_retune.log` | a separate question (expanding-window vs random k-fold tuning); source of the 391 figure | 0.35741 → 0.35700, −0.00042 |

Both RMSE-selection results are inside the 0.00055 bar. The production round
count before any of this was **391**; the 2026 submission shipped with
`best_iteration = 1000`, adopted from a later run of `adopt_rmse_rounds.R` and
never itself scored against 391 — see §4, which is also where the two adoption
runs disagreeing came from.

The round optimum is properly bracketed, not capped: the grid is
`seq(100, 2500, by = 100)`, the RMSE argmin is 1200, the curve turns up after it
and is still rising at 2500. Everything from **800 to 1600 is inside the
decidability bar** (9 of 25 grid points), so the exact number does not matter —
the shipped 1000 is 0.00023 off the argmin.

Note the metrics disagree by a factor of three on round count (400 vs 1200) and
it is worth 0.0017 on the validation curve but only 0.0004 on the holdout.

---

## 4. Bug found: the provenance record contradicted the model

`analysis/adopt_rmse_rounds.R:52` appended to the provenance list with `c()`
instead of replacing by key. It ran twice on 2026-09-02, so the list carries
duplicate names:

| Run | `rounds_selected` | `rounds_score` |
|---|---|---|
| 19:56:43 | 1100 | 0.35820 |
| 23:28:53 | **1000** | 0.35898 |

`best_iteration` is 1000, from the second run, but `provenance$rounds_selected`
returns **1100** because `[[` takes the first match on a duplicated name. Nothing
reads it programmatically, so no published number is wrong — but a provenance
record that lies is the one thing provenance exists to prevent. Ironically the
same file had already fixed this exact defect once, for the BACKUP filename.

**Fixed 2026-09-21**: replace by key, plus a `stopifnot` on duplicate names. The
existing `models/xgboost_tuning_results.rds` is left as-is: it is the historical
record of what actually shipped, and `best_iteration = 1000` is authoritative.

*Unresolved:* the two runs produced different curves (run 2 scored 1000 at
0.35898 where run 1's curve has 0.35842). Different seed, or the feature set
moved 258 → 267 between them. Both inside noise, so it is a correctness
question, not an accuracy one.

---

## 5. What to do for 2027

**Stop looking for calibration fixes.** Four were tested and they total 9% of
the gap. The remaining 91% is **discrimination** — picking the right player more
often — and no post-hoc transform on the existing probability matrix can buy it.

The one empirical clue is in §1: **`consensus`, the plain average of 22
entrants, beat every individual model.** Our ensemble is two correlated members
(XGBoost + GAM on shared features). Widening it across genuinely different model
families is the only mechanism this dataset demonstrates adds discrimination.
That is a hypothesis and deserves a real walk-forward test, not adoption on
reasoning.

Smaller, concrete:

1. **Fix the name spellings.** 207 of our rows (2.2%) did not join to the
   comparison sheet — Harrison/Harry Petty, Matthew/Matt Carroll, the two Bailey
   Williamses. Costs nothing to fix and makes every cross-model comparison exact.
2. **Sharpening is worth adopting on principle**, the way RMSE round selection
   was: pick beta on the three most recent seasons, expect ~0.002, do not expect
   it to be decidable.
3. **`2025-03-29_Brisbane_Geelong` has no votes in the datathon file.** A match
   scored as all-zeros silently rewards whichever model predicts least. Chase it
   before it contaminates a 2027 fit.

---

## 6. Live count infrastructure, built the same night

`analysis/live_count.R` + `analysis/live_refresh.ps1` +
`analysis/build_artifact_data.R` drive the Brownlow Vote Tracker artifact.

- The AFL publishes live votes at
  `api.afl.com.au/cfs/afl/bfawards/leaderboard/season/CD_S2026014` (the feed
  behind afl.com.au/brownlow-medal/live-tracker), token from a POST to
  `/cfs/afl/WMCTok`. Match and player ids are the AFL's own, so they join
  exactly to the prediction sheet.
- **That endpoint answers curl from any origin but returns 503 to a real browser
  request**, so the artifact cannot fetch it itself. Votes are pushed into the
  page at build time instead, and the page reloads periodically.
- `simulate_counts_vectorised()` (R/fast_simulation.R) replaces the per-sim loop
  with the exponential-race form of Plackett-Luce — give player *i* a clock
  `E_i/w_i`, take the three that fire first. Same distribution:
  `analysis/validate_vectorised_sampler.R` compares both samplers over 20,000
  draws of a real match and they agree to within 0.010 on every per-player
  P(3)/P(2)/P(1), which is Monte Carlo noise at that sample size.
  `analysis/benchmark_vectorised_sampler.R` measures the speedup at **37x** —
  7.6 s against 0.2 s for 200 counts of the 2026 season, so roughly 190 s
  against 5 s for the 5,000 a live re-simulation needs. The artifact page runs
  the same trick in JavaScript, so it re-simulates continuously in the browser.
- Only the *unread* matches are simulated; matches already read out are facts.
- The in-count momentum tilt (`analysis/in_count_update_value.R`) is deliberately
  **not** applied: it made weighted top-20 log-loss worse in 6 of 9 seasons.
