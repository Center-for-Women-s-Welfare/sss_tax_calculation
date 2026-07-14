# test-wa_wftc_comprehensive.R
# Comprehensive unit and integration tests for WA Working Families Tax Credit (WFTC)
# 
# Tests coverage beyond test-state_tax_integration.R:
# - Unit tests for apply_state_eitc_style_credit() edge cases
# - Integration tests for married couples, different family sizes, phase-out ranges
# - Bug fix validation for state_net calculation
# - Parameter loading and threading validation

library(testthat)
library(dplyr)  # Load dplyr BEFORE anything else that might cause conflicts

YEAR <- 2026

# ============================================================================
# UNIT TESTS: apply_state_eitc_style_credit() function
# ============================================================================

test_that("apply_state_eitc_style_credit() returns 0 when parameters are empty", {
  df <- data.frame(
    household_type = "single_parent",
    children = 1L,
    starting_income = 40000
  )
  params_empty <- data.frame(
    filing_status = character(0),
    children = integer(0),
    max_credit = numeric(0),
    phase_out_start = numeric(0),
    phase_out_end = numeric(0),
    phase_out_rate = numeric(0),
    min_credit = numeric(0)
  )
  
  result <- apply_state_eitc_style_credit(df, params_empty)
  expect_equal(result$credit_wftc, 0)
})

test_that("apply_state_eitc_style_credit() computes full credit below phase-out start", {
  df <- data.frame(
    household_type = "single_parent",
    children = 1L,
    starting_income = 30000  # Well below phase-out start of 45434
  )
  params <- data.frame(
    filing_status = c("single", "married"),
    children = c(1L, 1L),
    max_credit = c(660, 660),
    phase_out_start = c(45434, 52554),
    phase_out_end = c(50434, 57554),
    phase_out_rate = c(0.12, 0.12),
    min_credit = c(50, 50)
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(result$credit_wftc, 660)
})

test_that("apply_state_eitc_style_credit() returns 0 above phase-out end", {
  df <- data.frame(
    household_type = "single_parent",
    children = 1L,
    starting_income = 51000  # Above phase-out end of 50434
  )
  params <- data.frame(
    filing_status = c("single", "married"),
    children = c(1L, 1L),
    max_credit = c(660, 660),
    phase_out_start = c(45434, 52554),
    phase_out_end = c(50434, 57554),
    phase_out_rate = c(0.12, 0.12),
    min_credit = c(50, 50)
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(result$credit_wftc, 0)
})

test_that("apply_state_eitc_style_credit() computes correct phase-out (1 child, 12% rate)", {
  # Single parent, 1 child, income $47,212 (from PR description)
  # Expected: $447 (phasing out)
  df <- data.frame(
    household_type = "single_parent",
    children = 1L,
    starting_income = 47212
  )
  params <- data.frame(
    filing_status = c("single", "married"),
    children = c(1L, 1L),
    max_credit = c(660, 660),
    phase_out_start = c(45434, 52554),
    phase_out_end = c(50434, 57554),
    phase_out_rate = c(0.12, 0.12),
    min_credit = c(50, 50)
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  # 660 - 0.12 * (47212 - 45434) = 660 - 213.36 = 446.64 -> 447 (rounded)
  expect_equal(round(result$credit_wftc), 447)
})

test_that("apply_state_eitc_style_credit() applies $50 minimum floor correctly", {
  # Single, 0 children, income $18,500
  # Max: $335, phase-out start: $16,604, phase-out end: $19,104
  # Calculated: 335 - 0.18 * (18500 - 16604) = 335 - 342.12 (negative, floored at min)
  df <- data.frame(
    household_type = "single_adult",
    children = 0L,
    starting_income = 18500
  )
  params <- data.frame(
    filing_status = "single",
    children = 0L,
    max_credit = 335,
    phase_out_start = 16604,
    phase_out_end = 19104,
    phase_out_rate = 0.18,
    min_credit = 50
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(result$credit_wftc, 50)
})

test_that("apply_state_eitc_style_credit() respects filing status (single vs married)", {
  # Same income, different filing status -> different thresholds
  income <- 54000
  df_single <- data.frame(
    household_type = "single_parent",
    children = 2L,
    starting_income = income
  )
  df_married <- data.frame(
    household_type = "married",
    children = 2L,
    starting_income = income
  )
  params <- data.frame(
    filing_status = c("single", "married"),
    children = c(2L, 2L),
    max_credit = c(995, 995),
    phase_out_start = c(52310, 59430),
    phase_out_end = c(57310, 64430),
    phase_out_rate = c(0.15, 0.15),
    min_credit = c(50, 50)
  )
  
  result_single <- apply_state_eitc_style_credit(df_single, params)
  result_married <- apply_state_eitc_style_credit(df_married, params)
  
  # Single: 995 - 0.15 * (54000 - 52310) = 995 - 253.5 = 741.5
  # Married: 995 (below phase-out start of 59430)
  expect_true(result_single$credit_wftc < result_married$credit_wftc)
  expect_equal(result_married$credit_wftc, 995)
})

test_that("apply_state_eitc_style_credit() groups children >= 3 to 3", {
  # 4 children should map to 3-child parameters
  df <- data.frame(
    household_type = "single_parent",
    children = 4L,  # More than 3
    starting_income = 40000
  )
  params <- data.frame(
    filing_status = "single",
    children = 3L,  # Only 3+ available
    max_credit = 1330,
    phase_out_start = 56555,
    phase_out_end = 61555,
    phase_out_rate = 0.18,
    min_credit = 50
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(result$credit_wftc, 1330)  # Full credit below phase-out
})

test_that("apply_state_eitc_style_credit() handles vectorized input correctly", {
  # Multiple rows with different incomes and family types
  df <- data.frame(
    household_type = c("single_parent", "single_parent", "married"),
    children = c(1L, 1L, 2L),
    starting_income = c(35360, 47212, 54000)  # From PR examples
  )
  params <- data.frame(
    filing_status = c("single", "single", "single", "married", "married", "married"),
    children = c(0L, 1L, 2L, 0L, 1L, 2L),
    max_credit = c(335, 660, 995, 335, 660, 995),
    phase_out_start = c(16604, 45434, 52310, 23714, 52554, 59430),
    phase_out_end = c(19104, 50434, 57310, 26214, 57554, 64430),
    phase_out_rate = c(0.18, 0.12, 0.15, 0.18, 0.12, 0.15),
    min_credit = c(50, 50, 50, 50, 50, 50)
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$credit_wftc)))
  # Row 1: Single, 1 child, $35,360 -> full credit $660
  expect_equal(result$credit_wftc[1], 660)
})

test_that("apply_state_eitc_style_credit() handles missing parameters gracefully", {
  # Household type not in parameters (e.g., no "single_parent" match)
  df <- data.frame(
    household_type = "single_parent",
    children = 1L,
    starting_income = 40000
  )
  # Parameters don't exist for this filing status/children combo
  params <- data.frame(
    filing_status = character(0),
    children = integer(0),
    max_credit = numeric(0),
    phase_out_start = numeric(0),
    phase_out_end = numeric(0),
    phase_out_rate = numeric(0),
    min_credit = numeric(0)
  )
  
  result <- apply_state_eitc_style_credit(df, params)
  expect_equal(result$credit_wftc, 0) # Is this result desired? If parameters don't exist, should this alert the user instead of defaulting to 0?
})

test_that("apply_state_eitc_style_credit() correctly handles all 8 WA parameter rows", {
  # Test all 8 WA 2026 EITC parameter combinations with correct expected values
  params <- data.frame(
    filing_status = c("single", "married", "single", "married", "single", "married", "single", "married"),
    children = c(0L, 0L, 1L, 1L, 2L, 2L, 3L, 3L),
    max_credit = c(335, 335, 660, 660, 995, 995, 1330, 1330),
    phase_out_start = c(16604, 23714, 45434, 52554, 52310, 59430, 56555, 63675),
    phase_out_end = c(19104, 26214, 50434, 57554, 57310, 64430, 61555, 68675),
    phase_out_rate = c(0.18, 0.18, 0.12, 0.12, 0.15, 0.15, 0.18, 0.18),
    min_credit = c(50, 50, 50, 50, 50, 50, 50, 50)
  )
  
  # Test cases with calculated expected values based on actual income and phase-out parameters
  test_cases <- list(
    # Single, 0 children: phase_out_start = 16604, rate = 0.18
    # 335 - 0.18 * (17000 - 16604) = 335 - 71.28 = 263.72
    list(household = "single_adult", children = 0L, income = 17000, expected = 264),
    # Married, 0 children: phase_out_start = 23714, rate = 0.18
    # 335 - 0.18 * (24000 - 23714) = 335 - 51.48 = 283.52
    list(household = "married", children = 0L, income = 24000, expected = 284),
    # Single, 1 child: phase_out_start = 45434, rate = 0.12
    # 660 - 0.12 * (40000 - 45434) < 0, but income is below phase_out_start, so 660
    list(household = "single_parent", children = 1L, income = 40000, expected = 660),
    # Married, 1 child: phase_out_start = 52554, rate = 0.12
    # 660 - 0.12 * (50000 - 52554) < 0, but income is below phase_out_start, so 660
    list(household = "married", children = 1L, income = 50000, expected = 660),
    # Single, 2 children: phase_out_start = 52310, rate = 0.15
    # 995 - 0.15 * (50000 - 52310) < 0, but income is below phase_out_start, so 995
    list(household = "single_parent", children = 2L, income = 50000, expected = 995),
    # Married, 2 children: phase_out_start = 59430, rate = 0.15
    # 995 - 0.15 * (60000 - 59430) = 995 - 85.5 = 909.5
    list(household = "married", children = 2L, income = 60000, expected = 910),
    # Single, 3+ children: phase_out_start = 56555, rate = 0.18
    # 1330 - 0.18 * (55000 - 56555) < 0, but income is below phase_out_start, so 1330
    list(household = "single_parent", children = 3L, income = 55000, expected = 1330),
    # Married, 3+ children: phase_out_start = 63675, rate = 0.18
    # 1330 - 0.18 * (65000 - 63675) = 1330 - 237.5 = 1092.5
    list(household = "married", children = 3L, income = 65000, expected = 1093)
  )
  
  for (test_case in test_cases) {
    df <- data.frame(
      household_type = test_case$household,
      children = test_case$children,
      starting_income = test_case$income
    )
    result <- apply_state_eitc_style_credit(df, params)
    expect_equal(round(result$credit_wftc), test_case$expected,
                 label = paste(test_case$household, test_case$children, "children, income", test_case$income))
  }
})
# ============================================================================
# INTEGRATION TESTS: Full WA pipeline with various family types
# ============================================================================

create_wa_family_df <- function(household_type, adult, children, income_level = "low") {
  # income_level: "low" (below phase-out), "mid" (in phase-out), "high" (above phase-out)
  income_map <- list(
    low = list(single_0 = 15000, single_1 = 40000, single_2 = 50000, single_3 = 55000,
               married_0 = 20000, married_1 = 50000, married_2 = 58000, married_3 = 65000),
    mid = list(single_0 = 18000, single_1 = 47212, single_2 = 54000, single_3 = 58000,
               married_0 = 25000, married_1 = 55000, married_2 = 61000, married_3 = 65500),
    high = list(single_0 = 20000, single_1 = 51000, single_2 = 58000, single_3 = 62000,
                married_0 = 27000, married_1 = 58000, married_2 = 65000, married_3 = 70000)
  )
  
  htype <- if (household_type == "single_adult") "single_adult" else 
    if (household_type == "single_parent") "single_parent" else 
      "married"
  key <- paste0(tolower(if (household_type == "married") "married" else "single"), "_", children)
  income <- income_map[[income_level]][[key]]
  
  data.frame(
    household_type = htype,
    adult = as.integer(adult),
    children = as.integer(children),
    subtotal2 = income * 0.8,
    subtotal3 = income * 0.82,
    child_care_cost = if (children > 0) 1000 else 0,
    health_ins_premium = 200,
    county_table_number = "5303300000_1",
    public_transit_cost = 0,
    stringsAsFactors = FALSE
  )
}

test_that("WA WFTC: Married couple, 0 children, low income receives full credit", {
  df <- create_wa_family_df("married", 2, 0, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 335)  # Full credit for married, 0 children
})

# Test above failed. Debug:
test_that("Debug: Check what's in out", {
  df <- create_wa_family_df("married", 2, 0, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  # Print all column names to see what was computed
  print("Column names in out:")
  print(names(out))
  
  # Check if any credit columns exist
  print("Credit columns:")
  print(names(out)[grep("^credit_", names(out))])
  
  # Check the state credits configuration
  params <- load_state_tax_params(year = YEAR, state = "WA")
  print("State credits dataframe:")
  print(params$state_credits)
  
  # Check if state_eitc_params was loaded
  print("State EITC params:")
  print(params$state_eitc_params)
})
# Needed to copy tax_state_credits.csv from PR into sssTaxCalculation\extdata\state\2026

# Now figure out why the credit is calculated as 0 for married couple, 0 children, low income. Check the parameters and the logic in apply_state_eitc_style_credit().
test_that("Debug: Trace WA WFTC calculation step-by-step", {
  df <- create_wa_family_df("married", 2, 0, "low")
  
  # Add debugging output
  print("Input dataframe:")
  print(df)
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  print("Output dataframe:")
  print(out)
  
  # Key diagnostic fields
  print("Household type:")
  print(out$household_type)
  
  print("Children:")
  print(out$children)
  
  print("Starting income:")
  print(out$starting_income)
  
  print("State refundable credits:")
  print(out$state_refundable_credits)
  
  print("State net:")
  print(out$state_net)
  
  # Load params to check thresholds
  params <- load_state_tax_params(year = YEAR, state = "WA")
  print("State EITC params for WA:")
  print(params$state_eitc_params)
  
  # Manually calculate what the credit SHOULD be
  # For married, 0 children: max_credit = 335, phase_out_start = 23714
  income <- out$starting_income[1]
  print(paste("Income:", income))
  
  married_0_params <- params$state_eitc_params %>%
    filter(filing_status == "married", children == 0L)
  print("Matched params for married, 0 children:")
  print(married_0_params)
  
  if (income <= married_0_params$phase_out_start) {
    expected <- married_0_params$max_credit
    print(paste("Income is below phase-out start, expected credit:", expected))
  }
})
# starting_income is way too high (>$200,000). 

test_that("Debug: Run solver with debug output", {
  df <- create_wa_family_df("married", 2, 0, "low")
  
  # Run with debug = TRUE to see what's happening
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA", debug = TRUE)
  
  print("Final output:")
  print(out[, c("starting_income", "credit_wftc", "state_refundable_credits", 
                "state_tax_liability_with_refund", "subtotal2", "subtotal3")])
})

test_that("WA WFTC: Married couple, 1 child, low income receives full credit", {
  df <- create_wa_family_df("married", 2, 1, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 660)  # Full credit for married, 1 child
})

test_that("WA WFTC: Married couple, 2 children, low income receives full credit", {
  df <- create_wa_family_df("married", 2, 2, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 995)  # Full credit for married, 2 children
})

test_that("WA WFTC: Married couple, 3+ children, low income receives full credit", {
  df <- create_wa_family_df("married", 2, 3, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 1330)  # Full credit for married, 3+ children
})

test_that("WA WFTC: Single parent, 1 child, phase-out income receives partial credit", {
  df <- create_wa_family_df("single_parent", 1, 1, "mid")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_true(out$credit_wftc > 0)
  expect_true(out$credit_wftc < 660)  # Partial credit (not full, not zero)
})

test_that("WA WFTC: Single parent, 2 children, phase-out income receives partial credit", {
  df <- create_wa_family_df("single_parent", 1, 2, "mid")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_true(out$credit_wftc > 0)
  expect_true(out$credit_wftc < 995)  # Partial credit
})

test_that("WA WFTC: Single adult, 0 children, low income receives full credit", {
  df <- create_wa_family_df("single_adult", 1, 0, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 335)  # Full credit for single, 0 children
})

test_that("WA WFTC: Above phase-out end, credit is zero", {
  df <- create_wa_family_df("single_parent", 1, 1, "high")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_true("credit_wftc" %in% names(out))
  expect_equal(out$credit_wftc, 0)  # No credit above phase-out end
})

test_that("WA WFTC: Credit varies correctly by filing status at same income", {
  # Same income, single vs married should produce different credits
  income <- 54000
  
  df_single <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 2L,
    subtotal2 = income * 0.8,
    subtotal3 = income * 0.82,
    child_care_cost = 1000,
    health_ins_premium = 200,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  df_married <- data.frame(
    household_type = "married",
    adult = 2L,
    children = 2L,
    subtotal2 = income * 0.8,
    subtotal3 = income * 0.82,
    child_care_cost = 1000,
    health_ins_premium = 200,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  
  out_single <- solve_starting_income_iterative(df_single, year = YEAR, state = "WA")
  out_married <- solve_starting_income_iterative(df_married, year = YEAR, state = "WA")
  
  expect_true(out_single$credit_wftc < out_married$credit_wftc)
})

# ============================================================================
# BUG FIX VALIDATION: state_net for no-income-tax states
# ============================================================================

test_that("WA state_net correctly reflects refundable credits (no income tax)", {
  # WA has no income tax, so state_tax_liability_with_refund is NA
  # The fix ensures state_net = -refundable_credits (not 0)
  df <- create_wa_family_df("single_parent", 1, 1, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  # With the bug: state_net would default to 0, losing the credit
  # With the fix: state_net should be -credit_wftc (negative, reducing net taxes)
  expect_true(!is.na(out$state_net))
  expect_true(out$state_net <= 0)  # Net effect should be negative (credit reducing taxes)
})

test_that("Federal-only calculation doesn't error on state_refundable_credits", {
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 30000,
    subtotal3 = 30500,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "5306100000_1",
    public_transit_cost = 0
  )
  
  # Federal-only (no state) should not error even though it initializes state_refundable_credits
  expect_no_error(
    solve_starting_income_iterative(df, year = YEAR, state = NULL)
  )
})

# ============================================================================
# PARAMETER LOADING & THREADING VALIDATION
# ============================================================================

test_that("load_state_tax_params() returns state_eitc_params for WA 2026", {
  params <- load_state_tax_params(year = YEAR, state = "WA")
  
  expect_true("state_eitc_params" %in% names(params))
  expect_s3_class(params$state_eitc_params, "data.frame")
  expect_true(nrow(params$state_eitc_params) > 0)
})

test_that("load_state_tax_params() WA EITC params contain all required columns", {
  params <- load_state_tax_params(year = YEAR, state = "WA")
  state_eitc <- params$state_eitc_params
  
  required_cols <- c("filing_status", "children", "max_credit", "phase_out_start",
                     "phase_out_end", "phase_out_rate", "min_credit")
  expect_true(all(required_cols %in% names(state_eitc)))
})

test_that("load_state_tax_params() WA EITC params has exactly 8 rows", {
  params <- load_state_tax_params(year = YEAR, state = "WA")
  state_eitc <- params$state_eitc_params
  
  # 2 filing statuses x 4 children categories (0, 1, 2, 3+) = 8 rows
  expect_equal(nrow(state_eitc), 8)
})

test_that("calculate_state_tax_credits() receives and uses state_eitc_params", {
  # This indirectly tests parameter threading through the solver
  df <- create_wa_family_df("single_parent", 1, 1, "low")
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  # If params weren't passed through, credit_wftc would be 0 (or column missing)
  expect_true("credit_wftc" %in% names(out))
  expect_true(out$credit_wftc > 0)
})

# ============================================================================
# REGRESSION TESTS: Ensure other states still work correctly
# ============================================================================

test_that("CA EITC integration still works (regression test)", {
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 30000,
    subtotal3 = 30500,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "0603002000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "CA")
  expect_true("credit_ca_eitc" %in% names(out))
  expect_true(out$converged)
})

test_that("IA childcare credit integration still works (regression test)", {
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 30000,
    subtotal3 = 30500,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "1900100000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "IA")
  expect_true(out$converged)
})

# ============================================================================
# EDGE CASES & BOUNDARY TESTS
# ============================================================================

test_that("WA WFTC: Exactly at phase-out start income", {
  # Single, 1 child: phase-out start = 45434
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 45434 * 0.8,
    subtotal3 = 45434 * 0.82,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_equal(out$credit_wftc, 660)  # Full credit AT phase-out start
})

test_that("WA WFTC: Just above phase-out start income", {
  # Single, 1 child: phase-out start = 45434, rate = 12%
  # At $45,435: 660 - 0.12 * 1 = 659.88
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 45435 * 0.8,
    subtotal3 = 45435 * 0.82,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_true(out$credit_wftc < 660)
  expect_true(out$credit_wftc > 0)
})

test_that("WA WFTC: Exactly at phase-out end income", {
  # Single, 1 child: phase-out end = 50434
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 50434 * 0.8,
    subtotal3 = 50434 * 0.82,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  # At phase-out end, should be at minimum: 660 - 0.12 * (50434 - 45434) = 660 - 600 = 60 (floored at 50)
  expect_equal(out$credit_wftc, 50)
})

test_that("WA WFTC: Exactly one dollar above phase-out end income", {
  # Single, 1 child: phase-out end = 50434
  df <- data.frame(
    household_type = "single_parent",
    adult = 1L,
    children = 1L,
    subtotal2 = 50435 * 0.8,
    subtotal3 = 50435 * 0.82,
    child_care_cost = 500,
    health_ins_premium = 150,
    county_table_number = "5303300000_1",
    public_transit_cost = 0
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  expect_equal(out$credit_wftc, 0)  # No credit above phase-out end
})

test_that("WA WFTC: Vectorized with mixed income levels", {
  # Multiple families at different income levels in one call
  df <- rbind(
    create_wa_family_df("single_parent", 1, 1, "low"),
    create_wa_family_df("single_parent", 1, 1, "mid"),
    create_wa_family_df("single_parent", 1, 1, "high"),
    create_wa_family_df("married", 2, 2, "low"),
    create_wa_family_df("married", 2, 2, "high")
  )
  
  out <- solve_starting_income_iterative(df, year = YEAR, state = "WA")
  
  expect_equal(nrow(out), 5)
  expect_true(all(out$converged))
  # Verify the pattern: low > mid > high credits
  expect_true(out$credit_wftc[1] >= out$credit_wftc[2])
  expect_true(out$credit_wftc[2] >= out$credit_wftc[3])
  expect_equal(out$credit_wftc[3], 0)  # High income family 1
  expect_true(out$credit_wftc[4] >= out$credit_wftc[5])
  expect_equal(out$credit_wftc[5], 0)  # High income family 2
})
