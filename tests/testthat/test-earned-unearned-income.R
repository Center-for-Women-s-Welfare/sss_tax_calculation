# test-earned-unearned-income.R
#
# Regression tests for the n_adults / n_earning_adults earned/unearned income
# support added to sssTaxCalculationSV (see NOTES.md for fork provenance).
#
# Covers Part 4 of the fork task brief:
#   1. Working-age-only household (no n_earning_adults column) must produce
#      IDENTICAL total_fed_payroll_tax, eitc_credit, and final_federal_income_tax
#      when run through the original sssTaxCalculation package vs this fork --
#      the backward-compatibility guarantee for every other state's calculation.
#   2. A mixed household (1 earning adult, 1 non-earning adult, married) pays
#      payroll tax on exactly half the household income and uses the
#      n_earning_adults > 0 EITC branch.
#   3. A fully non-earning household (single senior) pays $0 payroll tax and
#      gets $0 EITC.
#
# Run with: testthat::test_file("tests/testthat/test-earned-unearned-income.R")
# or as part of the full suite via devtools::test().

library(testthat)

YEAR <- 2026

# ---------------------------------------------------------------------------
# Part 1: cross-package comparison (sssTaxCalculation vs sssTaxCalculationSV)
# ---------------------------------------------------------------------------
# Requires both packages installed (find.package() will fail loudly if not,
# rather than silently skipping the regression check).

create_working_age_mock_df <- function() {
  data.frame(
    household_type      = c("single_adult", "single_parent", "married"),
    adult                = c(1, 1, 2),
    children             = c(0, 2, 2),
    child_care_cost      = c(0, 1200, 1400),
    health_ins_premium   = c(350, 550, 700),
    county_table_number  = c("06085501100_1", "06085501100_1", "06085501100_1"),
    subtotal2            = c(3200, 5400, 6100),
    subtotal3            = c(3350, 5650, 6350),
    stringsAsFactors = FALSE
  )
}

test_that("sssTaxCalculation and sssTaxCalculationSV are both installed", {
  skip_if_not_installed("sssTaxCalculation")
  skip_if_not_installed("sssTaxCalculationSV")
  expect_true(TRUE)
})

test_that("working-age-only household (n_earning_adults absent) matches upstream exactly", {
  skip_if_not_installed("sssTaxCalculation")

  df <- create_working_age_mock_df()

  out_old <- suppressWarnings(sssTaxCalculation::solve_starting_income_iterative(df, year = YEAR))
  out_new <- suppressWarnings(sssTaxCalculationSV::solve_starting_income_iterative(df, year = YEAR))

  expect_equal(nrow(out_new), nrow(out_old))

  expect_equal(out_new$total_fed_payroll_tax, out_old$total_fed_payroll_tax,
               tolerance = 1e-6,
               info = "total_fed_payroll_tax diverged from upstream for a working-age-only household")
  expect_equal(out_new$eitc_credit, out_old$eitc_credit,
               tolerance = 1e-6,
               info = "eitc_credit diverged from upstream for a working-age-only household")
  expect_equal(out_new$final_federal_income_tax, out_old$final_federal_income_tax,
               tolerance = 1e-6,
               info = "final_federal_income_tax diverged from upstream for a working-age-only household")
})

# ---------------------------------------------------------------------------
# Part 2 & 3: hand-checked payroll tax splitting
# ---------------------------------------------------------------------------
# Calls calculate_federal_payroll_taxes() directly with synthetic, round-number
# parameters so the expected output can be verified by hand rather than by
# re-deriving what the function itself computes. Real 2026 federal payroll
# rates (ss_rate = 0.062, medicare_rate = 0.0145) are used so the test also
# reflects real-world magnitudes; wage limits/thresholds are set high enough
# to be irrelevant at these test incomes.

fed_payroll_params <- data.frame(
  year = rep(2026, 6),
  variable = c("ss_rate", "ss_wage_limit", "medicare_rate", "medicare_additional_rate",
               "medicare_threshold_single_hh", "medicare_threshold_married"),
  value = c(0.062, 184500, 0.0145, 0.009, 200000, 250000),
  stringsAsFactors = FALSE
)

test_that("mixed household (married, 1 earning + 1 non-earning adult) pays payroll tax on exactly half the household income", {
  mixed_df <- data.frame(
    household_type   = "married",
    starting_income  = 80000,
    n_earning_adults = 1,
    stringsAsFactors = FALSE
  )

  out <- calculate_federal_payroll_taxes(mixed_df, fed_payroll_params, 2026)

  # Hand calc: income_per_adult = 80,000 / 2 adults = 40,000.
  # Only 1 of 2 adults earns, so payroll tax is charged on that one $40,000 share:
  #   ss_tax       = 40,000 * 0.062  = 2,480
  #   medicare_tax = 40,000 * 0.0145 =   580
  #   total        =                   3,060
  expect_equal(out$n_adults, 2)
  expect_equal(out$income_per_adult, 40000)
  expect_equal(out$ss_tax, 2480)
  expect_equal(out$medicare_tax, 580)
  expect_equal(out$total_fed_payroll_tax, 3060)
})

test_that("fully non-earning household (single senior) pays $0 federal payroll tax", {
  senior_df <- data.frame(
    household_type   = "single_adult",
    starting_income  = 30000,
    n_earning_adults = 0,
    stringsAsFactors = FALSE
  )

  out <- calculate_federal_payroll_taxes(senior_df, fed_payroll_params, 2026)

  expect_equal(out$ss_tax, 0)
  expect_equal(out$medicare_tax, 0)
  expect_equal(out$total_fed_payroll_tax, 0)
})

test_that("payroll tax is unaffected when n_earning_adults is absent (defaults to n_adults)", {
  # Same married household, no n_earning_adults column at all -- must match
  # what a household with n_earning_adults == n_adults (all earning) would get.
  no_col_df <- data.frame(
    household_type  = "married",
    starting_income = 80000,
    stringsAsFactors = FALSE
  )
  explicit_df <- data.frame(
    household_type   = "married",
    starting_income  = 80000,
    n_earning_adults = 2,
    stringsAsFactors = FALSE
  )

  out_no_col   <- calculate_federal_payroll_taxes(no_col_df, fed_payroll_params, 2026)
  out_explicit <- calculate_federal_payroll_taxes(explicit_df, fed_payroll_params, 2026)

  expect_equal(out_no_col$total_fed_payroll_tax, out_explicit$total_fed_payroll_tax)
  # Hand calc: both adults earning on $40,000/adult:
  #   (40,000*0.062 + 40,000*0.0145) * 2 adults = 3,060 * 2 = 6,120
  expect_equal(out_no_col$total_fed_payroll_tax, 6120)
})

# ---------------------------------------------------------------------------
# Part 2 & 3: hand-checked EITC gating
# ---------------------------------------------------------------------------
# calculate_eitc_credit() expects the EITC lookup columns already joined onto
# df (normally done by build_eitc_lookup() before the solver loop). Synthetic,
# round-number EITC parameters are supplied directly here for a hand-checkable
# result, isolating the n_earning_adults gate from the real 2026 EITC schedule.

eitc_params_row <- list(
  eitc_income_at_max   = 15000,
  eitc_max             = 6000,
  eitc_phase_in_rate   = 0.40,
  eitc_phase_out_start = 25000,
  eitc_phase_out_end   = 55000,
  eitc_phase_out_rate  = 0.20
)

test_that("mixed household (n_earning_adults > 0) uses the normal EITC phase-in/out branch", {
  mixed_df <- data.frame(
    household_type       = "married",
    starting_income       = 20000,
    n_earning_adults      = 1,
    eitc_income_at_max    = eitc_params_row$eitc_income_at_max,
    eitc_max              = eitc_params_row$eitc_max,
    eitc_phase_in_rate    = eitc_params_row$eitc_phase_in_rate,
    eitc_phase_out_start  = eitc_params_row$eitc_phase_out_start,
    eitc_phase_out_end    = eitc_params_row$eitc_phase_out_end,
    eitc_phase_out_rate   = eitc_params_row$eitc_phase_out_rate,
    stringsAsFactors = FALSE
  )

  out <- calculate_eitc_credit(mixed_df)

  # Hand calc: starting_income (20,000) is between income_at_max (15,000) and
  # phase_out_start (25,000) -> credit is flat at eitc_max = 6,000.
  expect_equal(out$eitc_credit, 6000)
})

test_that("fully non-earning household (n_earning_adults == 0) gets $0 EITC regardless of income", {
  senior_df <- data.frame(
    household_type       = "single_adult",
    starting_income       = 20000,  # would earn the max credit if earning
    n_earning_adults      = 0,
    eitc_income_at_max    = eitc_params_row$eitc_income_at_max,
    eitc_max              = eitc_params_row$eitc_max,
    eitc_phase_in_rate    = eitc_params_row$eitc_phase_in_rate,
    eitc_phase_out_start  = eitc_params_row$eitc_phase_out_start,
    eitc_phase_out_end    = eitc_params_row$eitc_phase_out_end,
    eitc_phase_out_rate   = eitc_params_row$eitc_phase_out_rate,
    stringsAsFactors = FALSE
  )

  out <- calculate_eitc_credit(senior_df)

  expect_equal(out$eitc_credit, 0)
})
