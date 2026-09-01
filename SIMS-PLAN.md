# Simulation and betting workstream

Separate from the datathon. The datathon scores RMSE over 9,522 player-matches,
8,901 of which polled nothing. Betting cares about who wins and by how much —
which lives almost entirely in rows the competition metric barely weighs. A
change can be right for one and irrelevant to the other, and this session has
already produced two such cases.

Status as of 2026-09-01.

---

## Where the sims currently stand

They are better than expected. On the September 2025 artefacts — which contained
the scrambled XGBoost matrix, the 90%-GAM ensemble and the wrong training window
— the sim still had the eventual winner as third favourite:

| | simulated | actual |
|---|---|---|
| Nick Daicos | 51.3%, 30.9 votes | 2nd, 32 |
| Noah Anderson | 12.7% | — |
| **Matt Rowell** | **11.2%, 25.9 votes** | **WON, 39** |
| Bailey Smith | 10.6%, 26.0 votes | 3rd, 29 |
| Jordan Dawson | 9.8%, 25.9 votes | 4th, 27 |

11.2% is about $8.90 and perfectly usable. Note where the error sits: Daicos,
Smith and Dawson were all within 3 votes; Rowell was 13 low. And 39 fell outside
the 90th percentile of our simulated distribution (q90 = 31), so the spread was
too tight as well as the centre too low.

---

## 1. In-count momentum updating — Pete's idea, 2026-09-01

**The idea.** During the count, rounds are read out in order. After round *r* you
observe every player's actual votes for rounds 1..*r*. Compare that to what the
model predicted for those same rounds. A player running hot — Rowell in 2025 —
is revealing something the pre-count model did not know. Use that overperformance
to update the forecast for rounds *r+1*..*R*, and re-simulate.

**Why it is different from the season-to-date features already tested.** Those
use *predicted* votes over prior rounds, known before the count, and were
rejected on RMSE (−0.00039 against a 0.00081 noise floor). This uses *observed*
votes revealed during the count. It cannot help the submission at all — the
submission is locked before a single vote is read — but it is exactly what you
want when betting in-play.

**The crux, and the first thing to measure.** Does overperformance persist? If a
player over-polls relative to the model in rounds 1-8, is he more likely to
over-poll in rounds 9-23? If it is pure noise the correct update weight is zero
and the idea dies cheaply. Testable on 2015-2025 across every (season, cutoff)
pair, which gives hundreds of evaluation points rather than eleven.

Shape of the test:

```
for each season, for each cutoff round r:
    obs_r  = actual votes, rounds 1..r
    pred_r = model expected votes, rounds 1..r
    over_r = obs_r - pred_r                      (per player)
    then: does over_r predict (obs_rest - pred_rest)?
```

Regress the back-half residual on the front-half overperformance. The slope IS
the update weight, estimated rather than assumed, and if it is indistinguishable
from zero we stop.

**If it persists**, the update is a shrunk adjustment to remaining-round expected
votes, with the shrinkage falling as *r* grows and more evidence accumulates.
Same empirical-Bayes shape as the `player_bias` work: `weight = r / (r + k)`.

---

## 2. Optimising the sim — metrics to lock before tuning

Deciding this up front, because choosing a metric after seeing results is how
noise gets adopted.

- **Season-total RMSE across contenders.** Many data points per season, directly
  what the sim gets wrong on Rowell.
- **Log-loss on the actual winner.** The betting objective, but only 11 data
  points across 2015-2025 — too thin to tune on alone, useful as a check.
- **Calibration of top-N probabilities.** Of the players we give ~10%, do about
  10% of them win? Load the `calibration-check` skill before interpreting.
- **Distribution width.** Rowell's 39 sat outside our q90. Under-dispersion makes
  every longshot look worse than it is, which is a systematic betting bias.

---

## 3. Known defects

**Eligibility is not handled at all — highest priority.** A player suspended for
a week or more during the season is ineligible regardless of votes; Dangerfield
polled 33 in 2017 and Martin won on 36. In 2018 there were **34 ineligible
players**. The sim currently assigns win probability to players who cannot win.
Backing one loses every time; and when a contender is ineligible everyone else's
true probability rises, which markets can be slow to price.

Mechanism built (`R/eligibility.R`, `data/brownlow_ineligible.csv`): a register
keyed by season, `apply_eligibility()` zeroing and renormalising after
simulation, and `assert_eligibility_checked()` which refuses to run on a season
with no entries — an empty register and an unmaintained one look identical, so
recording "none-checked" has to be a deliberate act. **The register is empty and
must be populated from an authoritative source before any betting use.**

**The 3-2-1 sampler under-allocates to favourites.** Steps 2 and 3 of
`simulate_match_votes()` use ad-hoc marginal sums (`P(2)+P(3)`) rather than a
conditional. Measured on a real match: the favourite's P(2) comes out 0.135
against an input marginal of 0.172, a 21% shortfall; top-5 players lose 0.6% of
expected votes and the field gains 5%. Small per match, but it compounds over 22
rounds and runs against the favourite.

The fix is already written. `R/plackett_luce.R` draws 1st, 2nd and 3rd
sequentially from one worth vector, verified exact against brute-force
enumeration to 8.3e-17. Plackett-Luce lost as a *predictor* because it discarded
the multinomial's separately-fitted heads — but as a *generative model* for
simulation it is precisely correct, because here we genuinely are drawing three
places from a field.

**Ties.** `win_credit <- 1 / length(winners)` splits credit among tied leaders.
Right for a "wins outright" market, wrong for "wins or shares". Depends which
market is being bet.

**Inputs.** The sim calls the old `normalize_probability_matrix()`, which is 11x
slower than `normalize_votes_ipf()` and lands within ±0.006 rather than exactly.

---

## 3a. Backtest methodology, and a leak that nearly got through

The saved 2023/2024 simulations cannot be evaluated: the production model trains
on 2015-2024, so those seasons are in-sample. Only 2025 was genuinely held out,
which left ONE honest evaluation point. `analysis/simulation_backtest.R` rebuilds
them walk-forward — train on seasons < S, predict S — giving nine.

**`player_coef` had to be dropped from the backtest.** It is a GAM random effect
fitted once over 2015-2024, so it takes exactly one value for a player's entire
career. Handing that to a fold nominally trained on 2015-2016 tells it how
players polled through 2024, including the season being predicted.

The tell was not a check, it was implausibility: models trained on **two seasons**
were calling the winner at 55.5% and 87.0%, while the full-data 2025 sim managed
11.2%. Less data producing more confidence is backwards.

The diagnostic that confirmed it generalises:

| feature | constant across a player's whole career |
|---|---:|
| `player_coef` | **100.0%** |
| `career_avg_brownlow_votes_prior_seasons` | 30.2% |
| `career_avg_coaches_votes` | 5.8% |
| `career_avg_disposals` | 0.0% |
| `recent_avg_impact` | 0.0% |

**A feature that never changes across a career cannot be prior-only** — anything
genuinely accumulated has to move as games accumulate. `n_distinct()` per player
over a long career separates "fitted once over everything" from "built from the
past" in one line.

Correlating with future outcomes did NOT discriminate: `player_coef` correlated
+0.183 with later-season votes while the entirely legitimate `coaches_votes`
correlated +0.456. Good predictors correlate with the future; that is their job.
Constancy is the tell, not correlation.

The shipped pipeline is unaffected — there the GAM fits on 2015-2024 and the
coefficient is applied to 2025 rows it never saw, which is ordinary
fit-on-train-apply-to-test. Only reusing one fitted coefficient across
walk-forward folds is illegitimate. Cost: the backtest is mildly pessimistic
against the shipped model, which legitimately gains ~0.001 RMSE from the feature.

### Scoring

Winner-only log-loss gives 9 points across 9 seasons and is dominated by whether
one longshot came in. The backtest instead scores the actual top 20 — roughly the
best player at each of the 18 clubs — weighted `1/position`, so the winner still
dominates at 1.0 while twentieth contributes 0.05. That is 180 evaluation points
and it measures whether the model has the right *shape* over contenders, which is
what decides whether a book of bets is profitable rather than a single bet on the
favourite.

Reference: spreading probability evenly over ~20 real contenders scores about
`-log(1/20) = 3.00`. Lower is better.

5,000 simulations rather than 2,000, because at 2,000 the smallest resolvable
probability is 1/2000 and a genuine 20th-place finisher would read as an
impossible zero.

## 4. Order of work

1. Populate the eligibility register from an authoritative source, and wire
   `apply_eligibility()` into `08_brownlow_simulation.R`.
2. Backtest win probabilities across 2023, 2024, 2025 — all three winners are
   known. Establishes whether the sims are usable before improving them.
3. Test whether overperformance persists (§1). Cheap, decisive, kills or
   justifies the whole in-count idea.
4. Replace the 3-2-1 sampler with the Plackett-Luce draw.
5. Feed IPF-normalised probabilities in.

Items 1 and 2 gate everything: an uncalibrated sim with ineligible players in it
is not worth optimising.
