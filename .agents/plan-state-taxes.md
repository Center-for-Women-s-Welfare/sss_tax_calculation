# Implementation Plan: State Income Tax Support

---
**Date:** 2026-03-18
**Author:** AI Assistant
**Status:** Draft (v2 — production-compatible)

---

## Overview

Extend `solve_starting_income_iterative()` to incorporate state taxes (payroll, income, and credits) inside the iterative convergence loop so that `starting_income` correctly accounts for total state tax burden. The `state` parameter is already reserved in the solver signature (`iterative_income_solver.R:15`). The full state tax logic already exists in `sss_production/src/2026/analysis/tax_state.R`; this plan ports that logic into the package and wires it into the loop.

**Goal:** `solve_starting_income_iterative(df, year = 2026, state = "IA")` converges on a `starting_income` that covers both federal AND state tax burden, and outputs the same state tax columns the production workflow currently produces post-convergence.

**Motivation:** The production workflow (`calculate_taxes.R`) already passes `state` to the solver but the solver ignores it, computing only federal taxes. State taxes are then applied post-convergence from income that is too low. Bringing state taxes into the loop fixes the underestimation and eliminates the redundant post-convergence calculations in production.

---

## Current State Analysis

**Existing Package Implementation:**
- `R/iterative_income_solver.R:15` — `state = NULL` reserved but completely unused
- `R/iterative_income_solver.R:97` — `total_taxes = total_fed_payroll_tax + federal_cumulative_tax` (federal only)
- `R/tax_functions.R:21-49` — `calculate_tax_from_brackets()` already generic; accepts any brackets df
- `R/data_loader.R:10-28` — `load_federal_tax_params(year)` loads from `inst/extdata/federal/{year}/`
- `DESCRIPTION:19-22` — imports only `dplyr`, `tidyr`, `readr`; missing `fuzzyjoin` and `glue`

**Existing Production Implementation (`sss_production`):**
- `src/2026/analysis/calculate_taxes.R:3` — sources solver from `sss_code_base()/sss_tax_calculation/src/calculations/iterative_income_solver.R` (dev path, not installed package)
- `src/2026/analysis/calculate_taxes.R:5-12` — calls `solve_starting_income_iterative(df, year, state=state, ...)` — already passes state
- `src/2026/analysis/calculate_taxes.R:46-252` — after solver returns, runs a full state tax pipeline: `calculate_state_payroll_taxes()` → `calculate_state_taxable_income()` → `calculate_tax_from_brackets()` (state brackets with `state_filing_status` and `local_income_tax_final`) → `calculate_state_tax_credits()` → `apply_IA_credit_max_rule()` → `calculate_final_state_income_tax()`
- `src/2026/analysis/tax_state.R` — contains all four state tax functions
- `src/2026/analysis/tax_state_special_cases.R` — contains `apply_renters_deduction()`, `apply_commuter_deduction()`, `apply_IA_credit_max_rule()`
- `src/2026/analysis/tax_functions.R:40-66` — contains `apply_calculation_method()` used by state tax functions

**Current Behavior:**
Solver converges on `starting_income` using federal taxes only. Production then adds state taxes post-convergence, producing correct state tax column values but applied to an underestimated `starting_income`.

**Current Limitations:**
- `starting_income` is underestimated because state taxes aren't in the convergence loop
- State tax logic lives in production scripts, not in the versioned/tested package
- The production post-convergence state tax pipeline (lines 46-252 of `calculate_taxes.R`) is redundant once the solver is fixed
- Column naming and state bracket call (uses `state_filing_status`, `local_income_tax_final`) differs from what the plan originally assumed

---

## Desired End State

When called with `state = "IA"`:
1. Solver loads all 5 state tax parameter CSVs from `inst/extdata/state/IA/2026/`
2. Each iteration computes: state payroll → state taxable income → state brackets → state credits → `final_state_income_tax`
3. `final_state_income_tax + state_payroll_tax` are added to `total_taxes` inside the loop
4. Solver converges at the correct gross income accounting for all taxes
5. Output includes: `state_payroll_tax`, `state_filing_status`, `state_taxable_income`, `state_cumulative_tax`, `state_nonrefundable_credits`, `state_refundable_credits`, `final_state_income_tax` — the same columns production currently computes post-convergence
6. Production's `calculate_taxes.R` post-convergence state tax block (lines 46-252) is removed (no longer needed)

When called with `state = NULL` (default): behavior is identical to today. Fully backward compatible.

**Success Looks Like:**
- `solve_starting_income_iterative(df, year=2026, state="IA")` produces higher `starting_income` than `state=NULL`
- Output column names match what production currently expects post-convergence
- All existing tests pass; new state tax tests pass
- Production's `calculate_taxes.R` is simplified to just the solver call

---

## What We're NOT Doing

- [ ] Special-case deductions for states other than IA in initial implementation (renter's deduction for IN/MA, commuter deduction for MA — `apply_renters_deduction()` and `apply_commuter_deduction()` ARE ported but only activate when state data includes those variables)
- [ ] Modifying the production data loader (`load_final_sss_data.R`) — state tax data moves into the package
- [ ] Multi-state support within a single call (all rows use the same `state` parameter)
- [ ] Populating data for all 50 states (Iowa as first reference state; others follow same 5-file pattern)

**Rationale:** State payroll taxes and credits are included (unlike v1 plan) because they are essential for accurate convergence and production already computes them. Special-case deductions are included via ported functions — they just won't activate for states whose data doesn't define those variables.

---

## Implementation Approach

**Technical Strategy:**
Port the production's existing state tax functions (`tax_state.R`, `tax_state_special_cases.R`, `apply_calculation_method()`) directly into the package with minimal modification. Migrate the 5-file state data schema into `inst/extdata/state/{state}/{year}/`. Wire all state tax steps into the iterative loop using the same column names production already produces.

**Key Architectural Decisions:**

1. **Decision:** Port production functions verbatim (with minor dependency adjustments) rather than writing new simplified state functions
   - **Rationale:** Zero divergence between package behavior and production behavior; maximum fidelity; no double-computation
   - **Trade-offs:** Package gets more complex functions; but they're already tested in production
   - **Alternatives considered:** Simple SD-only state tax (original plan) — conflicts with production column names and produces wrong results for states with complex deductions

2. **Decision:** Column naming matches production exactly: `state_cumulative_tax`, `state_filing_status`, `state_payroll_tax`, `final_state_income_tax`, `state_nonrefundable_credits`, `state_refundable_credits`
   - **Rationale:** Allows seamless replacement of production's post-convergence state tax block without any downstream column changes

3. **Decision:** `total_taxes` in convergence loop = `fed_payroll + fed_income + state_payroll + state_cumulative_tax`; `total_credits` += `state_refundable_credits`; state nonrefundable credits handled in `calculate_final_state_income_tax()` after convergence
   - **Rationale:** Mirrors federal treatment exactly — `federal_cumulative_tax` is in total_taxes and credits are subtracted separately; same pattern for state

4. **Decision:** All 5 state CSV schemas match production exactly
   - `tax_state_income_brackets_df.csv`: `year, state, filing_status, bracket_num, lower_limit, upper_limit, rate`
   - `tax_state_ti_adjustments_df.csv`: `state, year, filing_status, variable_name, type, calculation_method, income_min, income_max, value`
   - `tax_state_payroll_df.csv`: `state, year, program, variable, value, ...` (matches `tests/test_data/tax_state_payroll_df.csv`)
   - `tax_state_credits_df.csv`: `state, year, filing_status, variable_name, type, calculation_method, income_min, income_max, value, refundable`
   - `tax_state_variable_brackets_df.csv`: same schema as `tax_state_credits_df.csv`
   - **Rationale:** Same files could be shared between package and production in the future

5. **Decision:** Standard deduction is encoded in `tax_state_ti_adjustments_df.csv` as variable `standard_deduction` (not a separate CSV like the federal approach)
   - **Rationale:** This is how production already stores it; mirrors the `calculate_state_taxable_income()` function that reads from `tax_state_adjustments_all_df`

6. **Decision:** Add `fuzzyjoin` and `glue` to package `DESCRIPTION` Imports
   - **Rationale:** `calculate_state_taxable_income()` and `calculate_state_tax_credits()` use `fuzzyjoin::fuzzy_left_join()` for income-bracketed adjustments and credits; `glue` is used for warning messages

**Patterns to Follow:**
- Federal function → federal data load pattern: `R/data_loader.R:10-28`
- State function source: `sss_production/src/2026/analysis/tax_state.R`
- State special cases source: `sss_production/src/2026/analysis/tax_state_special_cases.R`
- `apply_calculation_method()` source: `sss_production/src/2026/analysis/tax_functions.R:40-66`
- Iterative loop column cleanup: `R/iterative_income_solver.R:73-83`
- Production call to solver: `sss_production/src/2026/analysis/calculate_taxes.R:5-12`

---

## Implementation Phases

### Phase 1: State Data Infrastructure

**Objective:** Add `load_state_tax_params()` to the package and populate Iowa reference data in the correct 5-file schema.

**Tasks:**
- [ ] Add `load_state_tax_params(state, year)` to `R/data_loader.R` after line 28
  - Loads from `inst/extdata/state/{state}/{year}/`
  - Returns named list: `list(state_brackets, state_ti_adjustments, state_payroll, state_credits, state_variable_brackets)`
  - Constructs `state_adjustments_all` = `bind_rows(state_ti_adjustments, state_variable_brackets)` (mirrors production loader)
  - `stop()` with path-naming informative message if directory not found
- [ ] Create `inst/extdata/state/IA/2026/` directory
- [ ] Create `inst/extdata/state/IA/2026/tax_state_income_brackets_df.csv`
  - Schema: `year, state, filing_status, bracket_num, lower_limit, upper_limit, rate`
  - Populate with Iowa 2026 income tax brackets
- [ ] Create `inst/extdata/state/IA/2026/tax_state_ti_adjustments_df.csv`
  - Schema: `state, year, filing_status, variable_name, type, calculation_method, income_min, income_max, value`
  - Populate Iowa 2026 standard deduction (type=`taxable_income_subtraction`, variable_name=`standard_deduction`)
  - Populate Iowa 2026 personal/dependent exemptions if applicable
- [ ] Create `inst/extdata/state/IA/2026/tax_state_payroll_df.csv`
  - Schema matching `sss_production/tests/test_data/tax_state_payroll_df.csv`
  - Iowa has no state payroll tax programs — file has zero data rows (program column empty)
- [ ] Create `inst/extdata/state/IA/2026/tax_state_credits_df.csv`
  - Schema: `state, year, filing_status, variable_name, type, calculation_method, income_min, income_max, value, refundable`
  - Populate Iowa 2026 state tax credits (CDCC, early childhood credit)
- [ ] Create `inst/extdata/state/IA/2026/tax_state_variable_brackets_df.csv`
  - Same schema as credits file; populate with Iowa income-bracketed credits if any

**Dependencies:** None

**Verification:**
- [ ] `load_state_tax_params("IA", 2026)` returns list with all 5 dataframes without error
- [ ] `load_state_tax_params("XX", 2026)` stops with informative error message

---

### Phase 2: Port State Tax Functions into Package

**Objective:** Move all state tax calculation logic from production's `tax_state.R`, `tax_state_special_cases.R`, and `tax_functions.R` into the package.

**Tasks:**
- [ ] Add `fuzzyjoin` and `glue` to `Imports` in `DESCRIPTION` (after line 22)
- [ ] Add `fuzzyjoin` and `glue` namespace imports to `R/imports.R`
- [ ] Add `apply_calculation_method()` to `R/tax_functions.R` (port from `sss_production/src/2026/analysis/tax_functions.R:40-66`)
  - No changes needed; port verbatim
- [ ] Add `apply_renters_deduction()` to `R/tax_functions.R` (port from `tax_state_special_cases.R:7-36`)
  - No changes needed; port verbatim
- [ ] Add `apply_commuter_deduction()` to `R/tax_functions.R` (port from `tax_state_special_cases.R:39-60`)
  - No changes needed; port verbatim
- [ ] Add `apply_IA_credit_max_rule()` to `R/tax_functions.R` (port from `tax_state_special_cases.R:74-85`)
  - No changes needed; port verbatim
- [ ] Add `calculate_state_payroll_taxes()` to `R/tax_functions.R` (port from `tax_state.R:31-64`)
  - Change global reference `sss_year` to parameter `year`; `state` is already a parameter
- [ ] Add `calculate_state_taxable_income()` to `R/tax_functions.R` (port from `tax_state.R:69-224`)
  - No interface changes needed; `state` and `sss_year` are already parameters
- [ ] Add `calculate_state_tax_credits()` to `R/tax_functions.R` (port from `tax_state.R:230-482`)
  - No interface changes needed
  - Remove internal call to `apply_IA_credit_max_rule()` (line 456) — this will be called separately in the solver, same as production does it
- [ ] Add `calculate_final_state_income_tax()` to `R/tax_functions.R` (port from `tax_state.R:487-504`)
  - No changes needed; port verbatim

**Dependencies:** Phase 1 (for package to load cleanly)

**Verification:**
- [ ] `devtools::check()` passes with no errors on function definitions
- [ ] Each function can be called in isolation without error using test data

---

### Phase 3: Integrate State Taxes into Iterative Solver

**Objective:** Activate the `state` parameter — load state params before the loop, call all state functions per iteration, include state taxes in `total_taxes`.

**Tasks:**
- [ ] In `R/iterative_income_solver.R`, after `tax_params <- load_federal_tax_params(year)` (line 24), add conditional state param loading:
  ```r
  if (!is.null(state)) {
    state_tax_params <- load_state_tax_params(state, year)
    state_adjustments_all <- state_tax_params$state_adjustments_all
    state_tax_brackets     <- state_tax_params$state_brackets %>%
      dplyr::filter(year == !!year, state == !!state) %>%
      dplyr::select(-year, -state)
    state_payroll_df       <- state_tax_params$state_payroll
    state_credits_df       <- state_tax_params$state_credits
    state_variable_brackets_df <- state_tax_params$state_variable_brackets
  }
  ```
- [ ] Add all state intermediate columns to the cleanup `select(-any_of(...))` block at `iterative_income_solver.R:73-83`:
  `"state_payroll_tax"`, `"state_filing_status"`, `"state_taxable_income"`, `"total_state_deductions"`, `"state_cumulative_tax"`, `"state_nonrefundable_credits"`, `"state_refundable_credits"`, `"state_nonrefundable_credit_applied"`, `"state_tax_after_nonrefundable"`, `"state_tax_liability_with_refund"`, `"final_state_income_tax"`, `"income_for_tax"`, plus any `credit_*` and `payroll_tax_*` dynamic columns
  - Use `dplyr::select(-any_of(...), -dplyr::starts_with("credit_"), -dplyr::starts_with("payroll_tax_"))`
- [ ] After `calculate_ctc_credit()` (line 93), add state tax block inside the loop:
  ```r
  if (!is.null(state)) {
    df <- calculate_state_payroll_taxes(df, state_payroll_df, year, state)
    df <- calculate_state_taxable_income(df, state_adjustments_all, state, year)
    df <- calculate_tax_from_brackets(df, state_tax_brackets,
                                      taxable_income_var = "state_taxable_income",
                                      filing_status_var  = "state_filing_status",
                                      output_col         = "state_cumulative_tax",
                                      local_income_tax_var = "local_income_tax_final")
    df <- calculate_state_tax_credits(df, state_credits_df, state_variable_brackets_df, state, year)
    df <- apply_IA_credit_max_rule(df, state)
    df <- calculate_final_state_income_tax(df)
  } else {
    df$state_payroll_tax           <- 0
    df$state_cumulative_tax        <- 0
    df$state_refundable_credits    <- 0
    df$final_state_income_tax      <- 0
  }
  ```
- [ ] Update `total_taxes` line (currently `iterative_income_solver.R:97`):
  ```r
  total_taxes = coalesce(total_fed_payroll_tax, 0) +
                coalesce(federal_cumulative_tax, 0) +
                coalesce(state_payroll_tax, 0) +
                coalesce(state_cumulative_tax, 0)
  ```
- [ ] Update `total_credits` line (currently `iterative_income_solver.R:98`):
  ```r
  total_credits = coalesce(eitc_credit, 0) +
                  coalesce(cdctc_credit, 0) +
                  coalesce(ctc_credit, 0) +
                  coalesce(state_refundable_credits, 0)
  ```
- [ ] In the final `select(-any_of(...))` at line 133, do NOT add state output columns to the drop list — they should be retained in the output: `state_payroll_tax`, `state_filing_status`, `state_taxable_income`, `state_cumulative_tax`, `state_nonrefundable_credits`, `state_refundable_credits`, `final_state_income_tax`

**Dependencies:** Phases 1 and 2

**Verification:**
- [ ] With `state = NULL`: `starting_income` identical to current output
- [ ] With `state = "IA"`: `starting_income` is higher than `state = NULL`
- [ ] `state_cumulative_tax > 0` for Iowa rows
- [ ] `final_state_income_tax` is present in output

---

### Phase 4: Update Production `calculate_taxes.R`

**Objective:** Remove the now-redundant post-convergence state tax block from production, simplifying `calculate_taxes.R` to just the solver call.

**Tasks:**
- [ ] In `sss_production/src/2026/analysis/calculate_taxes.R`:
  - Remove lines 46-51: `calculate_state_payroll_taxes()` call
  - Remove lines 215-252: `calculate_state_taxable_income()`, state brackets, `calculate_state_tax_credits()`, `apply_IA_credit_max_rule()`, `calculate_final_state_income_tax()` calls
  - Retain the solver call (lines 5-12) unchanged — no parameter changes needed
  - The `source(file.path(..., "tax_functions.R"))` on line 2 can stay (it defines `calculate_tax_from_brackets` and `apply_calculation_method` which may be used elsewhere) but `tax_state.R` no longer needs to be sourced via `tax_functions.R`
- [ ] Confirm that `tax_state_payroll_df`, `tax_state_income_brackets_df`, etc. variables (currently loaded by `load_final_sss_data.R`) are no longer needed by `calculate_taxes.R` after this change. Data is now loaded internally by the solver.

**Dependencies:** Phase 3

**Verification:**
- [ ] `calculate_taxes.R` produces the same output columns as before (same state tax output columns, now from solver)
- [ ] No NA values in `final_state_income_tax` or other state columns in production output

---

### Phase 5: Tests

**Objective:** Add state tax path tests; confirm backward compatibility.

**Tasks:**
- [ ] Add to `tests/testthat/test-iterative_income_solver.R`:
  - Test: `solve_starting_income_iterative(df, year=YEAR, state="IA")` — all rows converge
  - Test: output contains `state_cumulative_tax`, `state_payroll_tax`, `final_state_income_tax`, `state_filing_status`, `state_taxable_income` columns when `state="IA"`
  - Test: `state_cumulative_tax > 0` for Iowa rows with meaningful income
  - Test: `starting_income` with `state="IA"` > `starting_income` with `state=NULL` for all rows
  - Test: `state=NULL` — state columns are 0, `starting_income` identical to current (regression guard)
  - Test: `state_filing_status` == `household_type` for all rows (set by `calculate_state_taxable_income`)

**Verification:**
- [ ] `testthat::test_dir("tests/testthat/")` — all tests pass

---

## Success Criteria

### Automated Verification

- [ ] `testthat::test_dir("tests/testthat/")` — all existing tests pass (no regression)
- [ ] New state tax tests pass: all rows converge with `state="IA"`
- [ ] `devtools::check()` passes with no errors or warnings
- [ ] Files exist: `inst/extdata/state/IA/2026/tax_state_income_brackets_df.csv`, `tax_state_ti_adjustments_df.csv`, `tax_state_payroll_df.csv`, `tax_state_credits_df.csv`, `tax_state_variable_brackets_df.csv`
- [ ] `fuzzyjoin` and `glue` present in `DESCRIPTION` Imports

### Manual Verification

- [ ] `solve_starting_income_iterative(df, year=2026, state=NULL)` — output identical to pre-change behavior
- [ ] `solve_starting_income_iterative(df, year=2026, state="IA")` — output contains all state tax columns with reasonable values
- [ ] `starting_income` with `state="IA"` is higher than `state=NULL` by a plausible amount
- [ ] Production `calculate_taxes.R` (after Phase 4 edits) produces the same final output columns as before
- [ ] `load_state_tax_params("XX", 2026)` gives clear error message naming the missing directory

---

## Testing Strategy

**Unit Tests** (`tests/testthat/test-iterative_income_solver.R`):
- State path: convergence, column presence, value sanity
- Regression: `state=NULL` path unchanged

**Test Data:**
- Reuse existing `create_mock_df()` (Iowa rows, `stusps="IA"`, `local_income_tax_final=0`)
- Iowa state tax CSVs in `inst/extdata/state/IA/2026/`

---

## Migration Strategy

**Backward Compatibility:** `state=NULL` default; all existing callers unchanged.

**Production Migration:** Phase 4 simplifies `calculate_taxes.R` — the removed state tax code is now inside the solver. Output column names are identical so downstream code (anything using `final_state_income_tax`, `state_cumulative_tax`, etc.) is unaffected.

**Rollback Plan:** Revert `R/` file changes, delete `inst/extdata/state/`, revert `calculate_taxes.R` to re-add the post-convergence state tax block.

---

## Risk Assessment

1. **Risk:** `fuzzyjoin::fuzzy_left_join()` in `calculate_state_taxable_income()` is slow on large dataframes
   - **Likelihood:** Medium (production dataframes can be 50k+ rows)
   - **Impact:** Medium (slow but correct)
   - **Mitigation:** Profile after integration; consider a non-fuzzy fallback for flat-value adjustments (the `all(is.na(rows$income_min))` path avoids fuzzy join for most variables)

2. **Risk:** Dynamic `credit_*` and `payroll_tax_*` column names created inside state functions may not be fully cleaned up by the `any_of()` cleanup block
   - **Likelihood:** Medium
   - **Impact:** Low (extra columns in output; doesn't affect convergence)
   - **Mitigation:** Use `dplyr::starts_with("credit_")` and `dplyr::starts_with("payroll_tax_")` in the cleanup block

3. **Risk:** `apply_IA_credit_max_rule()` is called twice in the current production code (once inside `calculate_state_tax_credits()` at line 456 and once externally at line 247) — after porting, we need to ensure it's called only once
   - **Likelihood:** High (it's literally in both places now)
   - **Impact:** Medium (double-applying would produce wrong credit values)
   - **Mitigation:** Phase 2 task explicitly removes the internal call from `calculate_state_tax_credits()` before porting

---

## References

**Files Analyzed:**
- `R/iterative_income_solver.R`
- `R/tax_functions.R`
- `R/data_loader.R`
- `R/validation.R`
- `DESCRIPTION`
- `inst/extdata/federal/2026/tax_fed_income_brackets_df.csv`
- `inst/extdata/federal/2026/tax_fed_sd_df.csv`
- `tests/testthat/test-iterative_income_solver.R`
- `sss_production/src/2026/analysis/calculate_taxes.R`
- `sss_production/src/2026/analysis/tax_state.R`
- `sss_production/src/2026/analysis/tax_state_special_cases.R`
- `sss_production/src/2026/analysis/tax_functions.R`
- `sss_production/tests/test_data/tax_state_payroll_df.csv`

---

## Review History

### Version 1.0 — 2026-03-18
- Initial plan created (federal-only convergence, simple state SD approach)

### Version 2.0 — 2026-03-18
- Major revision after production workflow analysis
- Expanded scope: state payroll taxes and state credits added (essential for correctness)
- Corrected column names to match production: `state_cumulative_tax`, `state_filing_status`, `state_payroll_tax`, `final_state_income_tax`
- Corrected data schema: 5 CSVs (not 2); SD is a variable in `tax_state_ti_adjustments_df`
- Added Phase 2 (port production functions) and Phase 4 (update production `calculate_taxes.R`)
- Added `fuzzyjoin` + `glue` as required package dependencies
- Added `apply_IA_credit_max_rule()` double-call risk (Risk #3)
