test_that("employer scenario golden-row regression is stable (fixture-backed)", {
  df <- data.frame(
    household_type = "single_adult",
    adult = 1L,
    children = 0L,
    children_under6 = 0L,
    children_6plus = 0L,
    household_size = 1L,
    subtotal2 = 3200,
    subtotal3 = 3200,
    child_care_cost = 0,
    health_ins_premium = 220,   # employer monthly premium
    health_ins_market = 950,    # should not drive employer scenario
    county_table_number = "golden_1",
    housing_cost = 1250,
    public_transit_cost = 0,
    tax_rate_local = 0,
    stringsAsFactors = FALSE
  )

  out <- solve_starting_income_iterative(
    df = df,
    year = 2026,
    state = NULL,
    health_insurance_scenario = "employer",
    max_iterations = 200,
    tolerance = 0.01,
    damping = 0.5,
    debug = FALSE
  )

  # Scenario invariants
  expect_equal(out$premium_tax_credit, 0)
  expect_equal(out$health_insurance_premium_used, 220)
  expect_equal(out$esi_premium_deduction, 220 * 12)
  expect_true(out$converged)

  fixture_path <- testthat::test_path("_snaps", "health_insurance_employer_golden.csv")
  fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)

  row <- fixture |>
    dplyr::filter(
      case == "single_adult_baseline",
      year == 2026,
      health_insurance_scenario == "employer"
    )

  expect_equal(nrow(row), 1)

  expected_starting_income <- as.numeric(row$starting_income[[1]])
  expected_total_taxes     <- as.numeric(row$total_taxes[[1]])
  expected_total_credits   <- as.numeric(row$total_credits[[1]])

  # Bootstrap mode: if fixture values are NA, print current outputs for easy copy.
  if (any(is.na(c(expected_starting_income, expected_total_taxes, expected_total_credits)))) {
    testthat::skip(paste0(
      "Golden fixture not initialized. Fill tests/testthat/_snaps/health_insurance_employer_golden.csv with:\n",
      "starting_income=", out$starting_income,
      ", total_taxes=", out$total_taxes,
      ", total_credits=", out$total_credits
    ))
  }

  expect_equal(out$starting_income, expected_starting_income, tolerance = 1e-6)
  expect_equal(out$total_taxes,     expected_total_taxes,     tolerance = 1e-6)
  expect_equal(out$total_credits,   expected_total_credits,   tolerance = 1e-6)
})