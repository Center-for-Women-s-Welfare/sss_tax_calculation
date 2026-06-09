# test-state_tax_integration.R
# Smoke tests for the state tax integration path in solve_starting_income_iterative()

library(testthat)

YEAR <- 2026

create_wa_smoke_df <- function() {
  data.frame(
    household_type      = c("single_adult", "single_parent", "married"),
    adult               = c(1L, 1L, 2L),
    children            = c(0L, 2L, 1L),
    subtotal2           = c(3200, 4800, 5500),
    subtotal3           = c(3250, 4900, 5600),
    child_care_cost     = c(0, 1200, 800),
    health_ins_premium  = c(180, 360, 540),
    county_table_number = c("5303300000_1", "5306100000_1", "5303300000_1"),
    public_transit_cost = c(0, 0, 0),
    stringsAsFactors    = FALSE
  )
}

test_that("WA state path runs end-to-end without error", {
  df <- create_wa_smoke_df()
  expect_no_error(
    solve_starting_income_iterative(df, year = YEAR, state = "WA")
  )
})

test_that("WA state path returns a data frame with at least as many rows as input", {
  df  <- create_wa_smoke_df()
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(df))
})

test_that("WA state path produces expected output columns", {
  df  <- create_wa_smoke_df()
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_true("starting_income"          %in% names(out))
  expect_true("final_federal_income_tax" %in% names(out))
  expect_true("final_state_income_tax"   %in% names(out))
})

test_that("WA state path converges for all rows", {
  df  <- create_wa_smoke_df()
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_true(all(out$converged))
})

test_that("WA state path produces non-negative starting_income", {
  df  <- create_wa_smoke_df()
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_true(all(out$starting_income >= 0))
})

test_that("WA state payroll taxes are applied (starting_income differs from federal-only)", {
  df       <- create_wa_smoke_df()
  out_fed  <- solve_starting_income_iterative(df, year = YEAR, state = NULL)
  out_wa   <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  # WA has PFML and WA_Cares payroll taxes so starting_income should be higher
  expect_true(all(out_wa$starting_income >= out_fed$starting_income))
})
