# test-ca-ss-ssdi-exclusion.R
#
# Regression tests for apply_CA_ss_ssdi_exclusion() -- California excludes
# SS/SSDI benefits from state taxable income entirely, unlike the federal
# treatment (which only excludes the non-taxable portion computed by
# calculate_ss_benefit_taxability() -- see test-ss-ssdi-taxability.R).
#
# Run with: testthat::test_file("tests/testthat/test-ca-ss-ssdi-exclusion.R")
# or as part of the full suite via devtools::test().

library(testthat)

ca_ti_adjustments <- data.frame(
  sss_year = 2026, state = "CA",
  filing_status  = c("single_adult", "single_parent", "married",
                      "single_adult", "single_parent", "married"),
  variable_name  = c(rep("standard_deduction", 3), rep("state_health_ins_deductible", 3)),
  type           = "taxable_income_subtraction",
  calculation_method = c(rep("flat", 3), rep("flag", 3)),
  income_min = NA, income_max = NA,
  value      = c(5706, 11412, 11412, 1, 1, 1),
  stringsAsFactors = FALSE
)

test_that("California excludes the full SS/SSDI benefit from state taxable income", {
  df <- data.frame(starting_income = 40000, household_type = "single_adult",
                    child_care_cost = 0, health_ins_premium = 0,
                    esi_premium_deduction = 0, annual_ss_ssdi_benefit = 9000,
                    stringsAsFactors = FALSE)

  out <- calculate_state_taxable_income(df, ca_ti_adjustments, "CA")

  # 40,000 - 9,000 (full exclusion, not just the $2,250 federally-taxable
  # portion) - 5,706 (standard deduction) = 25,294
  expect_equal(out$ca_ss_ssdi_exclusion, 9000)
  expect_equal(out$state_taxable_income, 25294)
})

test_that("apply_CA_ss_ssdi_exclusion is a no-op for every state other than CA", {
  df <- data.frame(annual_ss_ssdi_benefit = 9000, stringsAsFactors = FALSE)

  out <- apply_CA_ss_ssdi_exclusion(df, "IA")

  expect_false("ca_ss_ssdi_exclusion" %in% names(out))
})

test_that("California exclusion defaults to 0 when annual_ss_ssdi_benefit is absent", {
  df <- data.frame(x = 1, stringsAsFactors = FALSE)

  out <- apply_CA_ss_ssdi_exclusion(df, "CA")

  expect_equal(out$ca_ss_ssdi_exclusion, 0)
})
