# 2025 Datathon Post-Mortem and 2026 Plan

Written 2026-09-01. All numbers below are measured on the 9,522-row 2025 holdout
against the published actual votes, not estimated.

Reproduce with the scripts referenced in "Evidence" at the bottom.

---

## 1. How we did

**4th of 22 models. RMSE 0.36063.** Winner (`qp`) 0.34977. Gap 0.0109.

| Rank | Model | RMSE |
|-----:|-------|------|
| 1 | qp | 0.34977 |
| 2 | consensus | 0.35738 |
| 3 | wheelo | 0.36013 |
| **4** | **ours** | **0.36063** |
| 5 | brebbles | 0.36127 |
| 6 | jt | 0.36683 |
| … | | |
| 22 | hs | 0.45416 |

Reference points: predicting all zeros gives 0.55168; a flat 6/46 = 0.1304 for
every player gives 0.53604. So the whole competition is fought inside a
0.35–0.45 band, and 1st to 4th is 0.011 wide. Small absolute gains decide it.

The rival columns in `data/brownlow-results-2025.csv` have only 5 NAs each, all
on true-zero rows, so the leaderboard is a fair like-for-like comparison. The
ranking above holds whether NAs are dropped or treated as zero.

---

## 2. Where the loss actually sits

Sum of squared error by actual vote value:

| Actual votes | n | our SSE | % of our loss | our SSE − qp SSE |
|---|---:|---:|---:|---:|
| 0 | 8,901 | 425.3 | 34.3% | ~+24 |
| 1 | 207 | 109.5 | 8.8% | +9.2 |
| 2 | 207 | 282.7 | 22.8% | +15.5 |
| 3 | 207 | 420.8 | 34.0% | +24.7 |
| **total** | 9,522 | **1,238.3** | | **~+73** |

**Two thirds of our deficit to the winner is on the 621 rows that actually
polled.** We are not losing on the 8,901 quiet games; we are losing on knowing
who the three vote-getters were.

### Calibration is not the problem

The best possible global linear recalibration of our numbers is
`actual = 0.0055 + 0.958 × ours`, and applying it improves RMSE by **0.0004**.
Bin-by-bin the biases are small and unsigned. There is no free win in rescaling.

The problem is **discrimination**: on rows where the player actually got 3
votes, our mean prediction was 1.811 against qp's 1.869. On 2-vote rows, 1.135
against 1.172. qp simply puts more probability on the right players.

---

## 3. Biggest individual misses

Games where a player polled 3 and we gave them under 0.35 expected votes:

| Rd | Player | Team | ours | qp | consensus |
|---:|--------|------|-----:|---:|----------:|
| 1 | Jack Viney | Melbourne | 0.02 | 0.07 | 0.02 |
| 5 | Dayne Zorko | Brisbane | 0.02 | 0.03 | 0.01 |
| 7 | Matt Rowell | Gold Coast | 0.03 | 0.11 | 0.02 |
| 9 | Cam Rayner | Brisbane | 0.04 | 0.10 | 0.12 |
| 4 | Matt Rowell | Gold Coast | 0.06 | 0.25 | 0.02 |
| 1 | Chad Warner | Sydney | 0.06 | 0.11 | 0.09 |
| 6 | Reilly O'Brien | Adelaide | 0.07 | 0.09 | 0.03 |
| 14 | Patrick Cripps | Carlton | 0.07 | 0.08 | 0.04 |
| 9 | Matt Rowell | Gold Coast | 0.07 | 0.18 | 0.07 |
| 16 | Sam Flanders | Gold Coast | 0.11 | 0.07 | 0.05 |
| 10 | Touk Miller | Gold Coast | 0.11 | 0.20 | 0.08 |
| 10 | Liam Ryan | West Coast | 0.16 | 0.12 | 0.22 |
| 22 | Mark Blicavs | Geelong | 0.17 | 0.10 | 0.09 |
| 14 | Josh Weddle | Hawthorn | 0.19 | 0.52 | 0.67 |
| 19 | Josh Ward | Hawthorn | 0.26 | 0.40 | 0.25 |
| 15 | Dayne Zorko | Brisbane | 0.34 | 0.36 | 0.14 |

Three patterns, in order of how much they cost:

1. **Gold Coast.** Six of the sixteen worst misses are Gold Coast players
   (Rowell ×4, Miller, Flanders). Gold Coast made finals for the first time in
   2025. Our features lean heavily on career and prior-season history, so a
   club-level step change reads as noise for most of a season. Every rival
   missed these too, but qp missed them by less.

2. **Non-midfielders.** Rucks (Reilly O'Brien, Mark Blicavs, Luke Jackson),
   key defenders (James Sicily, Josh Weddle) and small forwards (Liam Ryan).
   These win votes on impact the disposal-count features cannot see.

3. **Star names on quiet stat lines.** Cripps twice, Viney, Zorko twice. The
   umpires reward reputation and contested work more than our features do.

The false positives are mostly forgivable — Shai Bolton 2.60 for 0 votes, Lachie
Neale 2.55 for 0 — and every rival made the same calls. That block of error is
irreducible.

---

## 4. Bugs found

Ordered by measured cost.

### 4.1 The submitted model was trained on 2015–2022 only — cost 0.0072 RMSE — FIXED 2026-09-01

`05_xgboost_model.R:237` builds `production_model` via
`train_production_xgboost()` and then **never uses it**. The saved probabilities
at `05_xgboost_model.R:258` come from `final_model`, which was trained on
`prepared_data$train` — and `prepare_modeling_data()` puts 2023 and 2024 in
`test`, not `train`.

So the 2025 submission's XGBoost half was produced by a model that had never
seen 2023 or 2024. The GAM half was **not** affected — `03_gam_model.R`
correctly uses `production_gam_model` for its saved probabilities. That is why
the shipped ensemble (0.36063) still beat XGBoost-alone-as-shipped (0.36680):
the GAM was carrying it.

Measured on the 2025 holdout, single XGBoost model, everything else held fixed:

| Training window | RMSE |
|---|---:|
| 2015–2022 (what shipped) | 0.36680 |
| 2015–2024 (verified after fix) | **0.35957** |

That one fix is worth **0.0072**, two thirds of the entire gap to the winner.
The fixed single XGBoost now beats the shipped two-model ensemble by 0.0011 and
would have placed 3rd in 2025.

**Fix applied 2026-09-01:**

- `05_xgboost_model.R` now generates the saved probabilities from
  `production_model` (train + test) rather than `final_model` (train only).
  `final_model` is kept for honest test-season evaluation, which is what it is
  for.
- `train_production_xgboost()` gained an `exclude_seasons` argument, replacing a
  hardcoded `filter(season != 2025)` that would have silently discarded 2025
  from next year's training set.
- `03_gam_model.R` had the same hardcoded `season != 2025`; now filtered on
  `prepared_data$holdout_seasons`.
- Added `stopifnot(!any(prepared_data$holdout_seasons %in% all_training_data$season))`
  in `05_xgboost_model.R`, the first of the three assertions from §7.

Verified end to end: production model trains on 87,288 rows from 2015–2024 and
scores 0.35957 on the 2025 holdout.

### 4.2 The 6-vote constraint is applied wrong — cost 0.0007 RMSE

There is only one constraint being enforced: total expected votes per match = 6,
applied by proportional rescaling (`09_submission_predictions.R`, and again
inside `normalize_probability_matrix()`).

But the real structure has **three** constraints per match, not one. Exactly one
player gets 3 votes, exactly one gets 2, exactly one gets 1. So across the 44–46
players in a match:

```
sum of P(3 votes) = 1
sum of P(2 votes) = 1
sum of P(1 vote)  = 1
and each player's four probabilities sum to 1
```

Enforcing all four with iterative proportional fitting (Sinkhorn) satisfies
the 6-vote total automatically and beats the rescale:

| Constraint method | RMSE |
|---|---:|
| none (raw expected votes) | 0.36135 |
| scale expected votes to 6 (current) | 0.36047 |
| IPF on all four constraints | 0.35879 |

### 4.3 `prepare_full_training_data()` uses an undefined variable

`R/model_utils.R:97` declares
`function(features, target_col, holdout_seasons, remove_zero_votes)` but line
107 reads `test_seasons`, which is not a parameter. It either errors or silently
picks up a global. The function is unused today, which is the only reason this
has not bitten.

### 4.4 The ensemble combines two data frames by row position

`06_ensemble_predictions.R` does:

```r
ensemble_prob_0 = xgb_weight * xgboost_prob_df$xgboost_prob_0 +
                  gam_weight * gam_prob_df$gam_prob_0
```

with no join. It works today only because both frames happen to be built from
the same `bind_rows(train, test, holdout)` in the same order. Any change to
either upstream script silently produces garbage that still passes every
constraint check. Join on `match_id` + `player_name`.

### 4.5 The pipeline does not run at all on the installed xgboost (3.2.1.1)

Three separate breakages, all found while applying the §4.1 fix:

1. **`reshape = TRUE` was removed from `predict()`.** Call sites:
   `05_xgboost_model.R` (fixed), `R/submission_utils.R:213`,
   `R/submission_utils.R:263` (**still to fix**). Current xgboost returns the
   multi-class matrix already shaped.
2. **`model$best_iteration <- nrounds` throws.** Booster objects are ALTREP-backed
   from xgboost 3.0 and reject element assignment:
   `ALTLIST classes must provide a Set_elt method`. This aborted
   `train_final_xgboost()` outright, so *no* model could be trained. Fixed by
   deleting the line — nothing read it, and the round count is available from
   `xgboost::xgb.get.num.boosted.rounds()`.
3. **Every model artefact in `models/` from 2025 is unloadable.**
   `readRDS("models/xgboost_final_model.rds")` then `predict()` gives
   `'xgb.Booster' object is corrupted or is from an incompatible XGBoost version`.
   The full pipeline has to be rerun from `02` regardless; budget for that.

### 4.8 `R/submission_utils.R` is a dead island containing a fake GAM

All 435 lines are unreachable from the pipeline. `09_submission_predictions.R`
joins `results/ensemble_probabilities.rds` straight onto the competition form and
never calls any of it. Internal references only:
`predict_submission_row` → `predict_with_ensemble` → nothing else;
`predict_with_xgboost`, `predict_with_gam`, `process_submission_form`,
`validate_and_save_submission` and `save_submission_results` have zero callers.

It is not merely dead, it is a trap. `predict_with_ensemble()` contains:

```r
gam_probs <- c(0.65, 0.25, 0.08, 0.02)  # Simplified GAM prediction for now
```

The GAM contribution is a **hardcoded constant**. Around it are four silent
fallbacks (`c(0.7, 0.2, 0.08, 0.02)` when the prediction is not a 4-column
matrix, `c(1, 0, 0, 0)` from the enclosing `tryCatch`). Anything built on this
would produce a submission that passes every constraint check and is badly
wrong. It also predicts one row at a time, so it is O(n) `predict()` calls.

The two `reshape = TRUE` sites here were fixed mechanically on 2026-09-01 so the
package loads cleanly, but **the file should be deleted, not repaired.** Left as
a separate decision rather than folded into the training-window fix, because it
is a 435-line removal and the priority was getting a clean pipeline run before
the template lands.

### 4.7 Latent: a dead one-hot fallback in the GAM probability path

`03_gam_model.R` has `if (is.vector(gam_probs))` which, when it fires, replaces
the GAM's probabilities with a **one-hot matrix of rounded predictions**. It does
not fire today because the family is `mgcv::ocat(R = 4)`, which returns an n × 4
matrix. But any change of family silently swaps calibrated probabilities for
hard 0/1s, and every downstream constraint check would still pass. Delete the
branch and let it error.

### 4.6 Hardcoded round fix mislabels a rescheduled match

`10_submission_analysis.R` contains
`round_num = ifelse(matchId == '20250142410', 0, as.numeric(rd))`. That forces
the Gold Coast v Essendon match of 27 Aug 2025 — the Opening Round fixture
postponed by Cyclone Alfred and replayed in Round 24 — into Round 0, where it
collides with the genuine Opening Round. Last year's analysis survived it because
both sides of that join used the same labelling, but any join against the
feature data on `(round, team, player)` produces 41 duplicate keys.
**Join on date, not round.**

---

## 5. What we measured, and what it is worth

All figures: single XGBoost multinomial, tuned hyperparameters unchanged,
scored on the 2025 holdout against actual votes.

| Configuration | RMSE | Δ |
|---|---:|---:|
| Replicating the shipped training window | 0.36680 | — |
| + train on 2015–2024 | 0.36016 | −0.0066 |
| + within-match rank features | 0.35947 | −0.0007 |
| + IPF constraint | 0.35879 | −0.0007 |
| **combined** | **0.35879** | **−0.0080** |
| *(shipped 2025 ensemble, for reference)* | *0.36063* | |
| *(2025 winner qp)* | *0.34977* | |

**0.35879 would have finished 2nd in 2025** — and it is one model, without the
GAM, so it is also far cheaper to run.

Two things that did **not** work, so we can stop wondering:

- **Direct RMSE regression** (`reg:squarederror` on the vote count) scored
  0.36180, worse than multinomial. Keep the multinomial objective; the
  expected value of the fitted distribution is the right RMSE-optimal point
  estimate.
- **Global recalibration** buys 0.0004. Not a lever.

Remaining gap to qp after all of the above: **0.0090**. That has to come from
better discrimination on vote-getters — section 3's three patterns.

---

## 5a. The measurement noise floor — read this before trusting any small number

XGBoost is configured with `subsample = 0.8` and `colsample_bytree = 0.6`. Those
draws consume the RNG, so two runs that differ only in what happened *before*
training give different models. Measured across seeds on the 2025 holdout:

**sd ≈ 0.0004, so run-to-run spread is ~0.001.**

That reframes everything in §5. Only the training-window fix (0.0072) is
comfortably above the floor. The IPF gain (0.0007), the within-match rank
features (0.0007) and `player_coef` (0.0008) were all single-seed measurements
at roughly 2× the sd — suggestive, not settled.

**Every comparison from here on must be multi-seed with a reported sd.** A single
run is a coin flip at this scale, and the difference between 1st and 4th in 2025
was only 0.011 — about ten noise units. Chasing noise is the most likely way to
spend the next three weeks and gain nothing.

## 5b. Player effects: what beat what (3 seeds each)

| Config | 2025 RMSE | sd |
|---|---:|---:|
| no player effect at all | 0.36106 | 0.00037 |
| **`player_coef` (GAM random effect)** | **0.35988** | 0.00052 |
| `player_bias` residual shrink, k = 5.81 | 0.36072 | 0.00004 |
| `player_bias` residual shrink, k = 20 | 0.36062 | 0.00019 |
| `player_bias` residual shrink, k = 50 | 0.36069 | 0.00039 |

`player_bias` is a leak-free empirical-Bayes construction: fit a base model on
strictly prior seasons, accumulate each player's residual (actual − expected),
and shrink toward zero as `sum(prior residuals) / (prior games + k)`. The
shrinkage constant comes free from the GAM: for a `bs="re"` term mgcv's
smoothing parameter *is* the variance ratio σ²_resid/σ²_player, and the fitted
model gives **sp = 5.81**.

It works — 0.0004 better than no player effect — but recovered only a third of
what the GAM coefficient did, and lost by ~2 sd.

**That verdict was wrong, and the caveat was the reason.** The base residual
model had been fitted at 150 rounds rather than the tuned 355 to keep the
experiment quick. An underfit base leaves real signal in its residuals, so
"player bias" was partly just base-model error. Retested at full depth on the
regenerated features, 5 seeds, scale-to-6 throughout:

| Player effect | RMSE | vs none |
|---|---:|---:|
| `player_bias` k = 5.81 (full depth) | **0.36003** | −0.00087 |
| `player_coef` (GAM random effect) | 0.36019 | −0.00070 |
| none | 0.36089 | baseline |

pooled sd 0.00034, so anything under 0.00067 is not decidable. The two are a
**dead heat**, and the GAM-derived k = 5.81 is the best of k in {5.81, 20, 50}.
Both runs reproduce the no-effect baseline at exactly 0.36089, which is what
makes them comparable across scripts.

That looked like it might retire the `bam` — but it does not hold under IPF,
which is the normalizer actually being shipped. Retested under IPF, 5 seeds:

| Config | RMSE | vs none | verdict |
|---|---:|---:|---|
| both | 0.35755 | −0.00158 | better |
| `player_coef` only (needs the GAM) | 0.35801 | −0.00113 | better |
| `player_bias` only (no GAM) | 0.35851 | −0.00062 | **within noise** |
| neither | 0.35914 | baseline | |

`player_coef` clears the bar against baseline; `player_bias` alone does not. The
scale-to-6 tie did not survive the change of normalizer. **The GAM stays.**

`both` was the nominal best, so it went to a 10-seed tiebreak:

| Config | RMSE (10 seeds) | delta | verdict |
|---|---:|---:|---|
| both | 0.35779 | −0.00010 | **within noise** |
| `player_coef` only | 0.35789 | baseline | |

The difference fell from 0.00046 at 5 seeds to 0.00010 at 10 — shrinking toward
zero as precision improves, which is the signature of a null effect rather than
a small real one. **`player_bias` is not adopted**; it would add ten
expanding-window fits per pipeline run for nothing measurable.

Two things worth keeping from the exercise. The GAM-derived k = 5.81 was the
best of the constants tried, so reading a shrinkage parameter off a fitted
random effect works as a technique. And a 5-seed run would have been enough to
adopt this change — the 10-seed check is what stopped it.

A cruder alternative, the shrunk *raw* player mean with no residualisation,
scored 0.36026 single-seed against the GAM's 0.36016 — also a tie. Residualising
is what makes the feature orthogonal to the box score, which is why it edges the
raw mean at full depth.

## 5e. player_coef coverage is a train/serve skew

After regenerating, the GAM's `s(player_name)` has 1,007 levels against 1,560
players in the data, so `player_coef` is missing for:

| Split | missing |
|---|---:|
| train | 2.1% |
| test | 17.3% |
| **holdout (2025)** | **32.6%** (277 distinct players) |

xgboost learns a default branch direction for `NA` from the 2% of training rows
where it occurs, then applies it to a third of the rows it must predict.
Imputing **zero** is the principled fix — partial pooling shrinks an unobserved
group to exactly zero, so "never seen" and "no measured deviation" are the same
statement. Imputing the *mean of observed coefficients* would be wrong: that is
the average deviation of established players, a selected non-zero quantity.

Measured, 5 seeds, IPF throughout:

| Config | RMSE | vs none |
|---|---:|---:|
| `player_coef`, NA imputed to 0 | **0.35801** | −0.00113 |
| `player_coef`, NA left as NA | 0.35825 | −0.00089 |
| no `player_coef` | 0.35914 | baseline |
| `player_coef` imputed + scale-to-6 | 0.36019 | +0.00105 |

`player_coef` is worth 0.00113, comfortably real. **The imputation itself is
worth 0.00024 — inside the noise band.** Keep it because it is principled and
free, not because it measurably helps. The last row reconfirms IPF at 0.00218 on
the final feature set, up from the 0.00176 measured without `player_coef`.

## 5c. Club names change in the season being predicted — FIXED 2026-09-01

The raw `data/brownlow_datathon_dataset.csv` uses different club names for the
historical block and the current season:

| Seasons | Brisbane | GWS |
|---|---|---|
| 2015–2024 | `Brisbane` | `GWS` |
| 2025 | `Brisbane Lions` | `Greater Western Sydney` |

Consequences: the GAM's `s(team)` random effect was **dead for two clubs in the
one season being predicted** (mgcv warned `factor levels Brisbane Lions, Greater
Western Sydney not in original fit`), and every feature keyed on team across
seasons was suspect for those clubs. No NAs were produced, so nothing failed
loudly. Team-feature year-on-year shifts for the two clubs ran ~2× a typical
club's, which is suggestive but two clubs is too small a sample to call — the
fix is justified on correctness, not on a measured RMSE gain.

**Verified after regenerating features (2026-09-01):** all 18 clubs now present
in all 11 seasons; `Brisbane` and `Greater Western Sydney` are the only spellings
remaining. The feature set also went from 260 to 258 columns, because
canonicalising the names changed the correlation structure enough for two more
features to be pruned as collinear — independent evidence that team features
really were affected, not just the GAM's factor levels.

Fixed at the single point of entry in `enhance_datathon_data()`:
`player_team`, `match_home_team` and `match_away_team` are canonicalised on
load; `standardize_team_name()` gained the full club-nickname set; and a new
`assert_team_names_consistent()` errors loudly if any club name is missing from
any season. Expect Betfair to ship 2026 the same way.

## 5d. Constraint method: IPF wins, Plackett-Luce loses (5 seeds, clean features)

Run through the new harness on the regenerated 258-feature data, base model
fitted once per seed and reused across every method so the comparison isolates
the constraint choice. `analysis/harness/constraint_method_comparison.csv`.

```
pooled seed sd = 0.00042  -> differences under 0.00085 are not decidable

                  label     rmse         sd       delta      verdict
     0.25 PL + 0.75 IPF  0.35903  0.00042895  -0.00186       BETTER
    IPF (4 constraints)  0.35914  0.00045809  -0.00176       BETTER
     0.50 PL + 0.50 IPF  0.35944  0.00040202  -0.00145       BETTER
     0.75 PL + 0.25 IPF  0.36037  0.00037814  -0.00053  within noise
   scale-to-6 (current)  0.36089  0.00051345   0.00000     baseline
 Plackett-Luce beta=0.8  0.36179  0.00036179  +0.00090        worse
 Plackett-Luce beta=1.0  0.36451  0.00028606  +0.00362        worse
 Plackett-Luce beta=1.2  0.36981  0.00028595  +0.00892        worse
 Plackett-Luce beta=0.6  0.37261  0.00066671  +0.01172        worse
 Plackett-Luce beta=1.5  0.37704  0.00029560  +0.01614        worse
```

**IPF is worth 0.00176, four times the noise floor.** Note the single-seed
estimate in §5 was 0.0007 — the real effect is 2.5x larger. Adopted in
`09_submission_predictions.R`.

**The Plackett-Luce rewrite is worse than the method it replaced**, and the best
blend beats IPF alone by 0.00011, well inside noise. Why the structurally-correct
model lost: IPF *reconciles* all four of the multinomial's heads to the
constraints, preserving what the model learned separately about P(1 vote) and
P(2 votes). The PL reparameterisation discards three heads and regenerates them
from P(3 votes) via the sequential-draw recursion, which assumes 2nd and 3rd
placings follow mechanically from the same worth that decides 1st. The data says
the separately-fitted heads carry information that assumption throws away. The
umpires' *process* is Plackett-Luce; their *votes* are not well described by one
worth per player.

A true PL model would fit the ranking likelihood directly with a custom
objective rather than reparameterising a fitted multinomial. That is a real
build, and this evidence does not justify it inside the 2026 window.
`R/plackett_luce.R` is kept — exact, 3.4 s per season, unit-tested against brute
force — in case it is worth revisiting.

## 5f. Out-of-sample replication on 2024

Everything above was measured on the 2025 holdout, and three candidates had
their parameters *chosen* on that same holdout: Plackett-Luce beta (5 values
tried), the PL/IPF blend weight (4 tried), and `player_bias` k (3 tried).
Selecting and scoring on one season biases those margins upward, which matters
when the margins are ~0.0001.

`analysis/replicate_on_2024.R` refits on 2015-2023, scores on 2024, and holds
every parameter FIXED at its 2025-chosen value. Nothing is tuned.

```
pooled seed sd = 0.00052  -> differences under 0.00104 are not decidable

                          label    rmse         sd       delta       verdict
  player_bias k=5.81 (rejected) 0.36775 0.00028792  -0.00060  within noise
  0.25 PL + 0.75 IPF (rejected) 0.36808 0.00052920  -0.00027  within noise
                  IPF (adopted) 0.36835 0.00045995   0.00000      baseline
 Plackett-Luce alone (rejected) 0.37049 0.00080114  +0.00214        worse
        scale-to-6 (old method) 0.37087 0.00036731  +0.00252        worse
```

| Change | 2025 (params chosen here) | 2024 (params frozen) |
|---|---:|---:|
| IPF vs scale-to-6 | +0.00218 | **+0.00252** |
| Plackett-Luce alone vs IPF | −0.00265 | −0.00214 |
| PL blend vs IPF | +0.00011 | +0.00027 |
| `player_bias` standalone | +0.00062 | +0.00060 |

**IPF replicates, slightly larger out of sample.** The adopted change survives the
scrutiny applied to the rejected ones. Pure Plackett-Luce is confirmed worse on
both seasons, and the PL blend stays inside the noise band on both.

**Revised verdict on `player_bias`: redundant, not useless.** +0.00062 on 2025 and
+0.00060 on 2024 — same sign, near-identical magnitude, two independent holdouts
with k frozen. The effect is probably real. It is not adopted because layering it
on `player_coef` added only 0.00010 at 10 seeds: the GAM's random effect already
carries that signal. If the GAM is ever retired, `player_bias` is its replacement
and costs seconds rather than an hour.

Note 2024 is not perfectly independent — 2023 and 2024 were the test seasons, so
xgboost's hyperparameters saw 2023 in their CV folds. The constraint methods and
`player_bias` are genuinely untuned on 2024, which is the question at issue.

## 5g. OPEN: hyperparameters were tuned under random k-fold CV

`tune_xgboost_hyperparameters()` in `R/model_utils.R:542` calls `xgb.cv` with
the time-aware folds **commented out**:

```r
cv_result <- xgboost::xgb.cv(
  params = params,
  data = dtrain_full,
  # folds = cv_folds,                        <- ignored
  nrounds = 500,
  nfold = 5,                                 <- random 5-fold
  stratified = (model_type == "multinomial"),
  early_stopping_rounds = 50,
  ...
)
```

`05_xgboost_model.R` builds proper expanding-window folds with
`create_time_aware_cv()` and passes them in as `cv_folds`; the function drops
them. So every tuned hyperparameter — including `best_iteration = 355` and the
reported `best_cv_score = 0.1544` — was selected while training on future
seasons and validating on past ones. `stratified = TRUE` compounds it by
splitting a match's three vote-getters across folds.

This contradicts the repo's own rule in `CLAUDE.md`: *"Never randomly shuffle
time-series data or train on future data."*

**Uncommenting the line does not fix it.** `xgb.cv`'s `folds` argument takes only
validation indices and trains on everything else; expanding-window CV needs
training restricted to *prior* seasons. It would also error, since
`create_time_aware_cv()` returns `list(train=, validation=)` per fold rather than
bare index vectors. Honest tuning needs a manual walk-forward loop: `xgb.train`
on prior seasons, watchlist on the next, `early_stopping_rounds` per fold.

Not fixed here — it is a rewrite of the tuning function, and pre-existing rather
than introduced by this work. Cost to redo properly: ~250 fits, 6-7 hours,
feasible overnight. Size it first: the wins this session all came from plumbing,
and depth-3/eta-0.05 may well be a sensible config regardless of how it was
found. Note also that 355 rounds was chosen on 68k training rows and the
production model now trains on 87k, so the honest number may be *higher*, not
lower.

## 5h. Verified end to end

Scored on the submission file the pipeline actually writes, not on a harness
approximation:

```
what shipped in 2025      0.36063
what this pipeline makes  0.35734
improvement               0.00329
```

That is 2nd of 23 against the model field in `data/brownlow-results-2025.csv` —
though by 0.00004 over `consensus`, which is a tie in everything but the sort
order. Clearly ahead of 3rd, level with 2nd, still 0.0076 behind `qp`.

Note this number IS the "train to last season, predict the next one" test: the
model fits 2015-2024 and 2025 is a holdout it never sees. When 2026 data lands,
rerunning it is a regression check rather than a new question.

The full pipeline runs `02` -> `03` -> `04` -> `05` -> `06` -> `09` under R 4.5.1
and xgboost 3.2.1. `09` reports `Constraints OK: 207 matches, worst total error
8.88e-16` — machine precision, against the old method's +/-0.006.

Two further candidates were tested at this point and rejected, both within noise:
454 rounds vs 355 (-0.00041 against a 0.00127 threshold) and within-match rank
features (-0.00061). The round count is still switched to 454 on procedure
grounds — it comes from leak-free walk-forward validation while 355 comes from
random k-fold on time-series data, and when two numbers are tied the correctly
derived one wins.

### The 2025 winner, and why every model missed him

Matt Rowell won with 39 votes. Predicted season totals across the whole field:

```
min 19.0   median 25.1   max 28.3   ours 24.0
models predicting him above 30 votes: 0 of 22
```

Not one model had him within 11 votes, and none had him first — ours, consensus
and wheelo all led with Daicos, `qp` with Bailey Smith. Meanwhile every other
contender was predicted well (Daicos 29.2 vs 32, Smith 26.9 vs 29, Dawson 25.9
vs 27). The field's collective error was concentrated on one player.

A miss that 22 independent models make, in the same direction, at the same
magnitude, is not a feature-engineering defect — it says the signal is absent
from the shared inputs. All 22 are built on the same box scores and coaches
votes. That reframes the season-to-date result: it failed to clear the noise
floor partly because the effect it targets may not be recoverable from this
dataset at all. Further gains need different inputs, not more transforms of
these ones.

## 6. Codebase review

28 files, 13,842 lines in `R/`, 173 exported functions.

### Cut

| Item | Lines | Why |
|---|---:|---|
| `R/simulation_utils.R` | 2,833 | Monte Carlo winner simulation. Scoring is RMSE per prediction; who wins the medal is not scored. |
| `R/simulation_visualization.R` | 569 | Same. |
| `08_brownlow_simulation.R` | — | Same. |
| `demo_*.R` (4 files) | ~700 | Scratch work committed to root. |
| `test_*.R` (2 files) | ~300 | Not tests — scripts named "test". No `testthat/` exists. |
| `compare_submission_vs_actual.R` | 380 | Superseded by `10_submission_analysis.R`. |

That is roughly 4,800 lines, a third of the package, removable without touching
the scored path. Keep the simulation code on a branch or in `attic/` if the
winner probabilities are still wanted for betting — but it should not be in the
critical path.

### Fix

- **32 `tryCatch` blocks** across 10 files, 12 of them in
  `R/enhanced_datathon_data.R` alone. The repo's own `CLAUDE.md` says "Let code
  error clearly instead of adding fallbacks" and "Don't wrap everything in
  tryCatch with fallback strategies". A silent fallback in a data loader is how
  a model trains on the wrong eight seasons and nobody notices — which is
  exactly what happened (§4.1).
- **No test suite.** For a pipeline whose failure mode is *silently correct-looking
  numbers*, three `testthat` assertions would have caught the two costliest bugs:
  1. the training frame contains every season except the holdout;
  2. within every match, `sum(P(1)) == sum(P(2)) == sum(P(3)) == 1`;
  3. joins are 1:1 — assert `nrow()` before and after every `left_join`.
- **`results/` is 297 MB**, including a 155 MB `gam_results.rds` and 17 MB CSV
  duplicates of RDS files that are already saved. Gitignored, but it is slowing
  every load. Stop writing the CSV twins.
- **Hyperparameter search is thin.** `n_iter = 50` random draws over a grid with
  14,580 combinations, with `nrounds` fixed at the tuned `best_iteration` rather
  than early-stopped per fold.

### Keep

The feature engineering is genuinely good — 261 features with `coaches_votes`
(legitimately published weekly), venue and weather context, and a leakage
discipline that is properly documented in `LEAKAGE_FIXES_SUMMARY.md` and
correctly implemented via `dplyr::lag()`. The time-aware CV and the
train/test/holdout separation are right in principle. The problem was never the
features; it was that the final model was fed the wrong slice of them.

---

## 7. Plan for 2026

Deadline **2:59pm AEST Monday 21 September 2026**. Template lands **8 September**.
That is 20 days from today, 7 until the template.

Ranked by measured or expected value per hour of work.

**Banked so far (2026-09-01):** training-window fix (+0.0072, well above noise);
club-name canonicalisation (§5c, correctness); `03` and `05` made runnable under
xgboost 3.2.1.1. **Settled and closed:** keep the GAM (§5b); keep the multinomial
objective (regression is worse); global recalibration is worth 0.0004, ignore it;
residual player-bias loses to `player_coef`.

1. **Build a multi-seed evaluation harness, and route every future claim through
   it.** This is now the top priority, ahead of any modelling idea. §5a shows the
   noise floor is ~0.001 while the whole 1st-to-4th spread is 0.011 — three of
   the four "improvements" measured single-seed sit inside the noise. Without
   `mean ± sd over N seeds` we cannot tell a real gain from a lucky draw, and
   there are only 20 days to spend.
2. **Rerun `02` → `06` clean** with the club-name fix in place and re-measure
   everything on regenerated features. Every number in this document is based on
   features built before §5c was fixed.
3. **Re-test the IPF constraint under the harness.** Four constraints, not one.
   Single-seed it looked worth 0.0007; that needs confirming before adoption.
   It is still the right structure rather than a rescaling hack.
4. **Add a `testthat` suite** with the assertions above plus a
   `nrow()`-preserving join helper. `assert_team_names_consistent()` is the
   first of these and already exists.
5. **Attack the discrimination gap** (0.0090, the whole remaining margin). In
   priority order:
   - a within-match relative-form feature set that can react to a team's
     mid-season step change faster than career averages (the Gold Coast miss);
   - non-midfielder vote pathways — ruck hitout-to-advantage and contested
     marks, defensive one-on-one wins, forward score involvement per minute;
   - consider replacing the independent per-row multinomial with a
     **rank-ordered (Plackett-Luce) model over the 44 players in a match**,
     which is what the umpires actually do and which makes the three constraints
     structural rather than a post-processing step.
6. **Delete the dead weight** — the simulation stack (§6) and
   `R/submission_utils.R` (§4.8) — so the pipeline that has to run on deadline
   day is the small one.

Do 1 and 2 before the template drops on 8 September. They are independent of the
new data, and everything else depends on them: item 1 is what makes items 3 and 5
decidable, and item 2 is what makes the measurements real.

---

## Evidence

Analysis scripts used to produce every number above live in the session
scratchpad and should be moved into `analysis/` if this is to be rerun:

- `postmortem.R` — leaderboard, SSE decomposition, calibration, worst rows
- `na_check.R` — leaderboard validity under NA handling
- `experiment.R` — training-window and rank-feature comparison
- `experiment2.R` — constraint normalisation, regression objective, blends

Inputs: `analysis/submission_comparison/detailed_predictions_vs_actual.csv`,
`data/processed/enhanced_modeling_data.rds`,
`models/xgboost_tuning_results.rds`.
