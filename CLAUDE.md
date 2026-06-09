# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`sssTaxCalculation` is an R package that computes the minimum gross annual income ("starting income") a family needs to be self-sufficient, given a basic-needs cost dataframe. It solves a circular problem — income determines taxes/credits, which adjust net income, which changes credit eligibility — via an iterative numerical solver. Phase 1 (current) covers federal payroll taxes, federal income tax, EITC, CDCTC, and CTC across 719 family configurations. State tax support (the `state` parameter) is reserved for Phase 2 and is being built on the current branch (`feature/state-tax-integration-`).

## Commands

This is a standard R package developed with `devtools`/`testthat`. Run these from an R session in the package root (or via `Rscript -e "..."`):

```r
devtools::load_all()        # load package source for interactive development
devtools::document()        # regenerate man/ and NAMESPACE from roxygen comments (run after changing @export or roxygen docs)
devtools::test()            # run the full test suite (tests/testthat/)
testthat::test_file("tests/testthat/test-iterative_income_solver.R")  # run a single test file
devtools::check()           # full R CMD check (build, docs, tests)
```

There is no separate lint/build step beyond standard `R CMD check` via `devtools::check()`.

## Architecture

### Data flow through the solver

The package has one exported entry point, `solve_starting_income_iterative()` in `R/iterative_income_solver.R`. It:

1. Validates input via `validate_input()` (`R/validation.R`).
2. Loads year-specific tax parameter CSVs from `inst/extdata/federal/{year}/` (and, for Phase 2, `inst/extdata/state/{year}/`) via `load_federal_tax_params()` (`R/data_loader.R`) — these are accessed at runtime through `system.file(..., package = "sssTaxCalculation")`, not relative paths, since this is an installed package.
3. Initializes `starting_income = subtotal3 * 1.20 * 12` and pre-joins per-row EITC lookup parameters (built once via `build_eitc_lookup()`).
4. Loops up to `max_iterations` times: drops all previous-iteration calculation columns, recomputes payroll tax → income tax → brackets → EITC → CDCTC → CTC → `total_taxes`/`total_credits`, derives `new_starting_income = (subtotal3 * 12) + total_taxes - total_credits`, and checks `abs(new - previous) < tolerance` per row. Rows that converge stop updating their `iteration_count` (each row can converge independently and at a different iteration).
5. Any rows still unconverged after `max_iterations` fall back to `subtotal3 * 1.20 * 12` and are flagged accordingly; a convergence summary is printed via `print_convergence_summary()` / `print_iteration_progress()` (`R/diagnostics.R`, gated by the `debug` flag).
6. Applies final credit ordering via `calculate_final_federal_income_tax()` to produce `final_federal_income_tax`.

### Tax calculation helpers (`R/tax_functions.R`)

All federal tax math lives here as composable dataframe-in/dataframe-out functions designed to be called once per solver iteration:

- `calculate_tax_from_brackets()` — generic progressive-bracket tax calculator (used for federal income tax; designed to be reusable for state brackets too, including an optional local/state rate add-on via `local_income_tax_var`).
- `calculate_federal_payroll_taxes()` / `load_fed_payroll_parameters()` — Social Security + Medicare (incl. additional Medicare tax), with married households split (income halved, SS tax doubled).
- `build_eitc_lookup()` / `calculate_eitc_credit()` — EITC parameters are expanded into a lookup table keyed by `(eitc_children, household_type)` and left-joined onto the main df once before the loop; `eitc_children = pmin(children, 3)` since federal EITC caps at 3+ children.
- `extract_cdctc_params()` / `calculate_cdctc_credit()` — CDCTC: non-refundable, capped by `federal_cumulative_tax`, with a sliding rate based on income brackets.
- `extract_ctc_params()` / `calculate_ctc_credit()` — CTC: splits into non-refundable (limited by post-CDCTC tax liability) and refundable portions, where the refundable calculation differs for 1–2 children (income-based) vs. 3+ children (payroll-tax-based, "additional CTC").
- `calculate_federal_income_tax()` / `calculate_final_federal_income_tax()` — standard deduction + ESI premium deduction → taxable income, then final liability after applying credits in order: CDCTC (non-refundable) → CTC refundable + EITC (refundable).

**Reconciliation needed with `sss_production`:** the federal tax functions here may be out of sync with updates made to the federal tax code in `sss_production` since this package was split out. Before considering the state-tax integration complete, reconcile `R/tax_functions.R` and `R/data_loader.R` in this repo against `tax_federal.R` and `tax_functions.R` in `sss_production` — federal logic changes made there post-split may not have been ported back here.

**File organization to revisit:** in `sss_production`, the federal tax functions were refactored out of `tax_functions.R` into a dedicated `tax_federal.R`. In this package they're still all together in `R/tax_functions.R`. Need to decide whether to split federal functions into their own file here too for consistency with `sss_production` (likely yes, especially as state tax functions are added alongside them).

### Convergence model

Convergence is evaluated **per row, independently**, not for the whole dataframe at once — a row that converges at iteration 5 stops updating while others continue. The loop only exits early when `all(df$converged)`. `iteration_count` and `final_income_diff` reflect each row's own convergence point. Tolerance defaults to $1.

**Known convergence issue — CO credit cliffs:** 13 non-converged rows observed in CO production data, all `single_parent` households near credit phase-out thresholds. Most have small `final_income_diff` but two have ~$4,300 difference. The oscillation is likely caused by the solver jumping across a credit cliff each iteration (income rises → credit drops → income falls → credit returns). Investigate whether a damping factor (e.g., blend `new = 0.5 * new + 0.5 * previous`) or a bisection fallback for non-converged rows would stabilize these cases.

### Tax parameter data (`inst/extdata/`)

CSVs are organized by domain and year: `inst/extdata/federal/{year}/` and `inst/extdata/state/{year}/`. To support a new tax year, add a new `{year}/` directory with CSVs following the existing schema (see `load_federal_tax_params()` for expected filenames and `load_fed_payroll_parameters()`/`extract_cdctc_params()`/`extract_ctc_params()` for expected `variable`/`value` long-format structure within the credits CSV).

**CSV filename note (resolved):** commit `3ed8097` renamed the federal CSVs from `tax_fed_*_df.csv` to `tax_fed_*.csv`. The inline `read_csv` calls in `solve_starting_income_iterative()` have been updated to match — this is no longer an issue.

## Known performance issues

**State EITC fuzzyjoin (resolved):** `apply_CA_eitc()` previously used `fuzzyjoin::fuzzy_left_join()` inside the convergence loop, which was the main bottleneck on large datasets. Refactored: `build_state_eitc_lookup()` (`R/tax_state_special_cases.R`) now pre-processes the lookup table once before the loop into long format keyed by `(bracket_idx, ca_eitc_children)`; `apply_CA_eitc()` uses `findInterval()` + `left_join` each iteration instead. Pattern mirrors the federal EITC (`build_eitc_lookup()` / `calculate_eitc_credit()`). Other income-bracketed credits in the general Case B loop in `calculate_state_tax_credits()` still use fuzzyjoin and may warrant a similar treatment if they become bottlenecks.

## Testing notes

- Mock dataframes in `tests/testthat/test-iterative_income_solver.R` are pulled from real 2026 Iowa basic-needs data — they reflect realistic but state-specific values.
- Tests assert on hardcoded error/warning message strings; if you change a `stop()`/`warning()` message in `validation.R` or elsewhere, update the corresponding `expect_error`/`expect_warning` strings in the matching test file.
- A few tests exercise large datasets and are slow — only run them when relevant to your change.
- The `YEAR` constant used across `test-iterative_income_solver.R` is hardcoded to 2026 to match the current state-data repository; if you see `object 'YEAR' not found`, you likely ran a `test_that()` block without sourcing the setup code above it.
- Coverage targets per the README: ≥80% line, ≥70% branch.
