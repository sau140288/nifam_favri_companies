# Changelog — branch `fix-decisionsupport-units`

Audit of argument units against decisionSupport conventions:
`discount(discount_rate)`, `vv(var_CV, relative_trend)` and `chance_event(CV_if, CV_if_not)` take **percent** (10 = 10%);
`chance_event(chance)` takes a **probability** (0–1).

## Changes

1. Removed unused `company_function.R` (older copy of `company_model.R`, not sourced by `index.Rmd`).
2. `CV_value` in `data/company_input.csv`: 0.1–0.4 `tnorm_0_1` → 10–40 `posnorm`.
   The old values were read by `vv()` as a 0.1–0.4% CV, so year-to-year variation was near zero.
   Effect (seed 84, 10,000 runs): mean NPVs unchanged; spread a bit wider;
   P(NPV>0) FarmStand 0.52 → 0.52, ConnectFarm 0.81 → 0.77.

## Checked, no change needed
- `discount_rate` 3–15 (percent) ✔
- `inflation_rate` 3–7, used as `relative_trend` (percent) ✔
- all `chance_*` / probability inputs are 0–1, used as `chance_event(chance)` ✔
