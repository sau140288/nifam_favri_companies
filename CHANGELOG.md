# Model changes for co-authors (FAVRI FarmStand / ConnectFarm model)

Branch: `fix-decisionsupport-units`, 25 Oct 2026

## Why

We checked every input against the units the decisionSupport functions expect.
`discount()` and `vv()` read rates and coefficients of variation (CV) as percent (10 means 10%).
`chance_event()` reads chances as probabilities (0.1 means 10%).

## What we found

- Discount rate (3 to 15) and inflation rate (3 to 7): correct, no change.
- All chances and probabilities (0 to 1): correct, no change.
- CV for annual cash flows was entered as 0.1 to 0.4. The model read this as 0.1% to 0.4%
  instead of the intended 10% to 40%, so year to year variation was close to zero.

## What we changed

1. CV fixed: `CV_value` set to 10 to 40 (percent), distribution changed from `tnorm_0_1` to `posnorm`.
2. Input table labels (Table 1): 29 inputs on a 0 to 1 scale were labelled "(%)".
   They now say "(probability 0-1)" or "(proportion 0-1)". Text only, no effect on results.
3. Small label fixes: one copy-paste error ("diversity" for "quality"), one typo,
   and `prob_increased_farmer_reputation` now described as a multiplier, which is how the model uses it.
4. Removed `company_function.R`, an old unused copy of the model.
5. Re-ran the full model (seed 84, 10,000 runs), EVPI, and all figures.

## Effect on results

Very small. The decision does not change.

| Result | Before | After |
|---|---|---|
| FarmStand mean NPV (M VND) | 89.3 | 89.4 |
| FarmStand chance NPV > 0 | 52% | 52% |
| ConnectFarm mean NPV (M VND) | 2.12 | 2.12 |
| ConnectFarm chance NPV > 0 | 81% | 77% |
| ConnectFarm 5th percentile NPV (M VND) | -1.4 | -2.0 |

EVPI (M VND):

| Variable | Before | After |
|---|---|---|
| FarmStand: farmer revenue shared | 8.17 | 8.23 |
| FarmStand: chance of farmer revenue sharing | 0.23 | 0.24 |
| FarmStand: annual HR staff cost | 0.02 | 0.11 |
| FarmStand: annual monitoring and evaluation | 0.03 | 0.02 |
| ConnectFarm: all variables | below 0.011 | below 0.010 |

The same variables lead in EVPI and PLS as before. The only shift is that annual HR staff cost
moves from 4th to 3rd in the FarmStand EVPI (still very small).
Means are unchanged because the CV only adds noise centred on the mean; it widens the spread a little.

## Action for co-authors

Check the manuscript for any quoted numbers from the table above, mainly the ConnectFarm
chance of positive NPV (81% now 77%) and the FarmStand EVPI values.
