# test-mo_local_income_tax.R
# Missouri state and local income-tax coverage for PR 23.
# The tax_rate_local input is supplied by the sss_production pipeline and is
# treated here as an already-verified input.

library(testthat)

YEAR <- 2026
STATE <- "MO"

create_mo_local_tax_df <- function(local_rate = 0.01) {
  data.frame(
    household_type      = "single_adult",
    adult               = 1L,
    children            = 0L,
    subtotal2           = 3250,
    subtotal3           = 3250,
    child_care_cost     = 0,
    health_ins_premium  = 180,
    county_table_number = "2918920000_1",
    housing_cost        = 1200,
    public_transit_cost = 0,
    tax_rate_local      = local_rate,
    stringsAsFactors    = FALSE
  )
}

test_that("Missouri local tax metadata identifies a percentage tax", {
  params <- load_state_tax_params(YEAR, STATE)

  expect_true("local_tax_type" %in% names(params))
  expect_equal(params$local_tax_type, "percent")
  expect_true("local_income_tax_brackets" %in% names(params))
})

test_that("percentage local income tax is calculated from starting income", {
  df <- data.frame(
    starting_income = c(40000, 50000),
    tax_rate_local  = c(0.01, 0.015),
    stringsAsFactors = FALSE
  )

  out <- calculate_local_income_tax(
    df = df,
    tax_type = "percent",
    income_col = "starting_income",
    rate_col = "tax_rate_local",
    out_col = "local_income_tax"
  )

  expect_equal(out$local_income_tax, c(400, 750))
})

test_that("Missouri solver includes local income tax in the output", {
  df <- create_mo_local_tax_df(local_rate = 0.01)

  out <- solve_starting_income_iterative(
    df = df,
    year = YEAR,
    state = STATE
  )

  expect_true("local_income_tax" %in% names(out))
  expect_true("state_cumulative_tax" %in% names(out))
  expect_true("final_state_income_tax" %in% names(out))
  expect_true("total_taxes" %in% names(out))

  expect_equal(
    out$local_income_tax,
    out$starting_income * df$tax_rate_local,
    tolerance = 1e-8
  )
  expect_true(all(out$local_income_tax >= 0))
})

test_that("Missouri local income tax affects the solver's total tax calculation", {
  df_with_local_tax <- create_mo_local_tax_df(local_rate = 0.01)
  df_without_local_tax <- create_mo_local_tax_df(local_rate = 0)

  with_local_tax <- solve_starting_income_iterative(
    df = df_with_local_tax,
    year = YEAR,
    state = STATE
  )
  without_local_tax <- solve_starting_income_iterative(
    df = df_without_local_tax,
    year = YEAR,
    state = STATE
  )

  expect_gt(with_local_tax$local_income_tax, 0)
  expect_equal(without_local_tax$local_income_tax, 0)
  expect_gt(with_local_tax$total_taxes, without_local_tax$total_taxes)
})

# Focused renters-deduction coverage remains separate from this Missouri local
# tax test. Maryland/local bracket special cases are intentionally deferred.
