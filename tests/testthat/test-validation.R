# test-validation.R
# Unit tests for the validate_input function

library(testthat)

# helper function for creating a mock
create_mock_df <- function(nrows = 10) {
  data.frame(
    subtotal2 = runif(nrows, min = 1000, max = 5000),
    subtotal3 = runif(nrows, min = 1500, max = 6000),
    household_type = sample(c('single_adult', 'single_parent', 'married'), nrows, replace = TRUE),
    children = sample(0:10, nrows, replace = TRUE),
    adult = sample(1:5, nrows, replace = TRUE),
    child_care_cost = runif(nrows, min = 0, max = 2000),
    health_ins_premium = runif(nrows, min = 0, max = 3000),
    county_table_number = sample(1:50, nrows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Test cases for validate_input function
test_that("validate_input returns TRUE for valid input", {
  mock_df <- create_mock_df()
  expect_true(validate_input(mock_df))
})

# Input validation tests
test_that('input must be of type data frame', {
  expect_error(validate_input(list()), "Input must be a data frame, got: list")
  expect_error(validate_input(matrix()), "Input must be a data frame, got: matrix")
  expect_error(validate_input("not a data frame"), "Input must be a data frame, got: character")
})

# Missing column tests
test_that("validate_input detects missing column", {
  mock_df <- create_mock_df()

  missing_col_df <- mock_df[, -which(names(mock_df) == "subtotal2")]
  expect_error(validate_input(missing_col_df),
               "Missing required columns: subtotal2 \nEnsure basic needs calculations have been completed.")

  missing_cols_df <- mock_df[, -which(names(mock_df) %in% c("subtotal2", "household_type"))]
  expect_error(validate_input(missing_cols_df),
               "Missing required columns: subtotal2, household_type")
  expect_error(validate_input(missing_cols_df),
               "Ensure basic needs calculations have been completed.")
})

# Missing data tests
test_that("validate_input detects missing data", {
  mock_df <- create_mock_df()

  mock_df$subtotal2[1] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values. \nFound 1 NA values out of 10 rows.")

  mock_df <- create_mock_df()
  mock_df$household_type[c(2,4)] <- NA
  expect_error(validate_input(mock_df),
               "Column household_type contains NA values. \nFound 2 NA values out of 10 rows.")

  mock_df <- create_mock_df()
  mock_df$subtotal3[c(3,5,7)] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal3 contains NA values. \nFound 3 NA values out of 10 rows.")

  mock_df <- create_mock_df()
  mock_df$subtotal2[c(1,6)] <- NA
  mock_df$household_type[c(2,4,5)] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values. \nFound 2 NA values out of 10 rows.")
})

# Function will allow NAs in non-critical columns
test_that("validate_input allows NAs in non-critical columns", {
  mock_df <- create_mock_df()
  mock_df$county_table_number[c(1,3,5)] <- NA
  expect_true(validate_input(mock_df))
})

test_that("validate_input detects negative values in subtotals", {
  mock_df <- create_mock_df()
  mock_df$subtotal2[1] <- -100
  expect_error(validate_input(mock_df),
               "subtotal2 contains negative values.")
  expect_error(validate_input(mock_df),
               "Found 1 negative values.")

  mock_df <- create_mock_df()
  mock_df$subtotal3[c(2,4)] <- -200
  expect_error(validate_input(mock_df),
               "subtotal3 contains negative values.")
  expect_error(validate_input(mock_df),
                "Found 2 negative values.")
})

# Household type tests
test_that("validate_input detects invalid household_type values", {
  mock_df <- create_mock_df()
  mock_df$household_type[1] <- "invalid_type"
  expect_error(validate_input(mock_df),
               "Invalid household_type values found: invalid_type \nValid values are: single_adult, single_parent, married")

  mock_df <- create_mock_df()
  mock_df$household_type[c(2,4)] <- c("foo", "bar")
  expect_error(validate_input(mock_df),
               "Invalid household_type values found: foo, bar \nValid values are: single_adult, single_parent, married")
})

test_that("Valid household types raise no errors", {
  mock_df <- create_mock_df(3)
  mock_df$household_type <- c('single_adult', 'single_parent', 'married')
  expect_true(validate_input(mock_df))

  mock_df$household_type <- rep('single_adult', 3)
  expect_true(validate_input(mock_df))

  mock_df$household_type <- rep('single_parent', 3)
  expect_true(validate_input(mock_df))

  mock_df$household_type <- rep('married', 3)
  expect_true(validate_input(mock_df))
})

# Data size tests
test_that("validate_input detects empty data frame", {
  empty_mock <- create_mock_df(0)
  expect_error(validate_input(empty_mock),
               "Input dataframe is empty \\(0 rows\\)")
})

# warning: this takes a long time to run
test_that("Warning is raised for large data frames", {
  large_mock <- create_mock_df(1)
  large_mock <- do.call(rbind, replicate(1000001, large_mock[1, ], simplify = FALSE))

  expect_warning(validate_input(large_mock),
                 "Large dataframe detected: 1000001 rows.")
  expect_warning(validate_input(large_mock),
                 "Processing may take significant time.")
})

test_that("Reasonably sized dataframes do not raise warnings", {
  mock_df <- create_mock_df(100)
  expect_silent(validate_input(mock_df))

  mock_df <- create_mock_df(10000)
  expect_silent(validate_input(mock_df))

  mock_df <- create_mock_df(100000)
  expect_silent(validate_input(mock_df))
})

# also takes a long time to run
test_that("Edge cases handled correctly", {
  mock_df <- create_mock_df(1)
  expect_true(validate_input(mock_df))

  large_mock <- create_mock_df(1)
  large_mock <- do.call(rbind, replicate(1000000, large_mock[1, ], simplify = FALSE))
  expect_silent(validate_input(large_mock))
})

test_that("Numerous errors handled correctly", {
  mock_df <- create_mock_df(10)
  mock_df$subtotal2[c(1,6)] <- NA
  mock_df$household_type[c(2,4,5)] <- NA
  mock_df$subtotal3[c(3,5,7)] <- -100

  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values. \nFound 2 NA values out of 10 rows.")
})

test_that("Order of errors are correct", {
  mock_df <- create_mock_df()
  df_no_subtotal2 <- mock_df[, -which(names(mock_df) == "subtotal2")]
  df_no_subtotal2$subtotal3[1] <- NA
  expect_error(validate_input(df_no_subtotal2),
               "Missing required columns: subtotal2 \nEnsure basic needs calculations have been completed.")

  mock_df <- create_mock_df()
  mock_df$subtotal2[1] <- NA
  mock_df$subtotal3[2] <- -100
  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values.")
})

test_that("Additional columns do not affect validation", {
  mock_df <- create_mock_df()
  mock_df$extra_column1 <- "extra1"
  mock_df$extra_column2 <- 123
  expect_true(validate_input(mock_df))
})

test_that("Extreme values do not affect validation", {
  mock_df <- create_mock_df()
  mock_df$subtotal2[1] <- 1e10
  mock_df$subtotal3[1] <- 1e10
  expect_true(validate_input(mock_df))

  mock_df$subtotal2[2] <- 0.0001
  mock_df$subtotal3[2] <- 0.0001
  expect_true(validate_input(mock_df))
})

test_that("Original dataframe is preserved after validation", {
  df_original <- create_mock_df()
  df_copy <- df_original
  validate_input(df_copy)
  expect_equal(df_original, df_copy)
})
