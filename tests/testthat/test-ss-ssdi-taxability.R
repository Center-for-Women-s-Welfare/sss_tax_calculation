# test-ss-ssdi-taxability.R
#
# Regression tests for SS/SSDI benefit taxability support added to
# sssTaxCalculationSV (Part B of the SS/SSDI taxability + test fixture brief).
#
# Covers (federal taxability only -- see test-tax-brackets.R and
# test-ca-ss-ssdi-exclusion.R for the two related-but-separate concerns):
#   1. calculate_ss_benefit_taxability() against hand-verified IRS Social
#      Security Benefits Worksheet arithmetic, across all three bands
#      (0% / partial / up to 85%), for both single and married thresholds.
#   2. Backward compatibility: no annual_ss_ssdi_benefit column at all ->
#      taxable_ss_benefit == 0, gap_income == starting_income, matching
#      pre-existing behavior exactly.
#   3. calculate_federal_income_tax() correctly substitutes the taxable
#      benefit portion for the full benefit within the taxable income base.
#
# Run with: testthat::test_file("tests/testthat/test-ss-ssdi-taxability.R")
# or as part of the full suite via devtools::test().

library(testthat)

# ---------------------------------------------------------------------------
# Part 1: calculate_ss_benefit_taxability() -- hand-verified IRS worksheet
# ---------------------------------------------------------------------------
# Each case is set up as (gap_income, benefit) -> starting_income = gap +
# benefit, so the expected provisional_income and taxable_ss_benefit can be
# hand-derived directly from the IRS Social Security Benefits Worksheet
# (Form 1040 instructions) independent of this package's implementation.

test_that("single filer, provisional income below $25,000: 0% taxable", {
  # gap_income = 10,000, benefit = 9,000 -> starting_income = 19,000
  # provisional = 10,000 + 0.5*9,000 = 14,500 < 25,000 base -> taxable = 0
  df <- data.frame(starting_income = 19000, household_type = "single_adult",
                    annual_ss_ssdi_benefit = 9000, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$gap_income, 10000)
  expect_equal(out$provisional_income, 14500)
  expect_equal(out$taxable_ss_benefit, 0)
})

test_that("single filer, provisional income in the partial band: hand-verified against the IRS worksheet", {
  # gap_income = 25,000, benefit = 9,000 -> starting_income = 34,000
  #
  # IRS Social Security Benefits Worksheet, by hand:
  #   1. Total benefits                                    =  9,000
  #   2. 50% of line 1                                      =  4,500
  #   3. Other income + tax-exempt interest (gap_income + 0) = 25,000
  #   4. Line 2 + line 3 (provisional income)                = 29,500
  #   5. Base amount (single/HOH)                            = 25,000
  #   6. Line 4 - line 5                                     =  4,500
  #   7. Second threshold (single/HOH)                       =  9,000
  #   8. Line 6 - line 7 (>= 0)                              =      0
  #   9. Smaller of line 6 or line 7                         =  4,500
  #  10. 50% of line 9                                       =  2,250
  #  11. Smaller of line 2 or line 10                        =  2,250
  #  12. Line 8 * 85%                                        =      0
  #  13. Line 11 + line 12                                   =  2,250
  #  14. Line 1 * 85%                                        =  7,650
  #  15. Taxable benefits = smaller of line 13 or line 14     =  2,250
  df <- data.frame(starting_income = 34000, household_type = "single_adult",
                    annual_ss_ssdi_benefit = 9000, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$gap_income, 25000)
  expect_equal(out$provisional_income, 29500)
  expect_equal(out$taxable_ss_benefit, 2250)
})

test_that("married filer, provisional income below $32,000: 0% taxable", {
  # gap_income = 15,000, benefit = 10,000 -> starting_income = 25,000
  # provisional = 15,000 + 5,000 = 20,000 < 32,000 base -> taxable = 0
  df <- data.frame(starting_income = 25000, household_type = "married",
                    annual_ss_ssdi_benefit = 10000, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$provisional_income, 20000)
  expect_equal(out$taxable_ss_benefit, 0)
})

test_that("married filer, provisional income above $44,000: hand-verified against the IRS worksheet", {
  # gap_income = 40,000, benefit = 30,000 -> starting_income = 70,000
  #
  # IRS worksheet, by hand:
  #   1. Total benefits                       = 30,000
  #   2. 50% of line 1                        = 15,000
  #   3. Other income                         = 40,000
  #   4. Provisional income                   = 55,000
  #   5. Base amount (MFJ)                    = 32,000
  #   6. Line 4 - line 5                      = 23,000
  #   7. Second threshold (MFJ)               = 12,000
  #   8. Line 6 - line 7                      = 11,000
  #   9. Smaller of line 6 or line 7          = 12,000
  #  10. 50% of line 9                        =  6,000
  #  11. Smaller of line 2 or line 10         =  6,000
  #  12. Line 8 * 85%                         =  9,350
  #  13. Line 11 + line 12                    = 15,350
  #  14. Line 1 * 85%                         = 25,500
  #  15. Taxable = smaller of line 13/line 14 = 15,350
  df <- data.frame(starting_income = 70000, household_type = "married",
                    annual_ss_ssdi_benefit = 30000, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$provisional_income, 55000)
  expect_equal(out$taxable_ss_benefit, 15350)
})

test_that("taxable_ss_benefit never exceeds 85% of the total benefit, even at very high income", {
  df <- data.frame(starting_income = 500000, household_type = "married",
                    annual_ss_ssdi_benefit = 40000, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$taxable_ss_benefit, 0.85 * 40000)
})

test_that("gap_income floors at 0 when the benefit alone exceeds starting_income", {
  # Two seniors with a combined benefit larger than a very-low-cost county's
  # computed starting_income -- an edge case, not the typical row.
  df <- data.frame(starting_income = 20000, household_type = "married",
                    annual_ss_ssdi_benefit = 46128, stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$gap_income, 0)
  expect_equal(out$provisional_income, 0.5 * 46128)
})

# ---------------------------------------------------------------------------
# Part 2: backward compatibility -- no annual_ss_ssdi_benefit column
# ---------------------------------------------------------------------------

test_that("calculate_ss_benefit_taxability is a no-op when annual_ss_ssdi_benefit is absent", {
  df <- data.frame(starting_income = 60000, household_type = "married",
                    stringsAsFactors = FALSE)
  out <- calculate_ss_benefit_taxability(df)

  expect_equal(out$annual_ss_ssdi_benefit, 0)
  expect_equal(out$gap_income, 60000)
  expect_equal(out$provisional_income, 60000)  # gap + 0.5*0
  expect_equal(out$taxable_ss_benefit, 0)
})

# ---------------------------------------------------------------------------
# Part 3: calculate_federal_income_tax() integration
# ---------------------------------------------------------------------------

fed_sd <- list(single_adult = 16100, single_parent = 24150, married = 32200)

test_that("calculate_federal_income_tax substitutes the taxable benefit portion, not the full benefit", {
  # Same senior household as the single-filer partial-band case above.
  df <- data.frame(starting_income = 34000, household_type = "single_adult",
                    health_ins_premium = 0, annual_ss_ssdi_benefit = 9000,
                    stringsAsFactors = FALSE)
  out <- calculate_federal_income_tax(df, fed_sd)

  # federal_taxable_income_base = 34,000 - 9,000 + 2,250 = 27,250
  # taxable_income = 27,250 - 16,100 (std ded) = 11,150
  expect_equal(out$federal_taxable_income_base, 27250)
  expect_equal(out$taxable_income, 11150)
})

test_that("calculate_federal_income_tax matches pre-existing behavior when there is no benefit", {
  df <- data.frame(starting_income = 60000, household_type = "married",
                    health_ins_premium = 500, stringsAsFactors = FALSE)
  out <- calculate_federal_income_tax(df, fed_sd)

  expect_equal(out$taxable_income, 60000 - 32200 - 6000)
  expect_equal(out$taxable_ss_benefit, 0)
})

# Part 4 (taxable_income == 0 bracket regression guard) lives in its own
# file, test-tax-brackets.R -- it's a pre-existing bug this work exposed and
# fixed, not part of the taxability feature itself, so it's committed and
# tested separately.
#
# Part 5 (California's full SS/SSDI exclusion from state taxable income)
# lives in test-ca-ss-ssdi-exclusion.R -- a separate state-tax concern from
# the federal taxability logic tested above, committed separately.
