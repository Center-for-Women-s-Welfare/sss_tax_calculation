# test_validation.R
# Unit tests for the validate_input function

# testing package
library(testthat)

# function to be tested
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "validation.R"))

# helper function for creating a mock
create_mock_df <- function(nrows = 10) {
  data.frame(
    subtotal2 = runif(nrows, min = 1000, max = 5000),
    subtotal3 = runif(nrows, min = 1500, max = 6000),
    household_type = sample(c('single_adult', 'single_parent', 'married'), nrows, replace = TRUE),
    children = sample(0:3, nrows, min = 0, max = 1000),
    county_table_number = sample(1:50, nrows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Test cases for validate_input function
test_that("validate_input returns TRUE for valid input", {
  mock_df <- create_mock_df()
  expect_true(validate_input(mock_df))
})

# Missing column tests
test_that("validate_input detects missing column", {
  mock_df <- create_mock_df()

  # test if missing one column throws error
  missing_col_df <- mock_df[, -which(names(mock_df) == "subtotal2")]
  expect_error(validate_input(missing_col_df),
               "Missing required column: subtotal2")
  expect_error(validate_input(missing_col_df),
               "Ensure basic needs calculations have been completed.")

  # test for multiple missing columns
  missing_cols_df <- mock_df[, -which(names(mock_df) %in% c("subtotal2", "household_type"))]
  expect_error(validate_input(missing_cols_df),
               "Missing required columns: subtotal2, household_type")
  expect_error(validate_input(missing_cols_df),
               "Ensure basic needs calculations have been completed.")
})

# Missing data tests
test_that("validate_input detects missing data", {
  mock_df <- create_mock_df()

  # Missing subtotal2 values
  mock_df$subtotal2[1] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values.")
  expect_error(validate_input(mock_df),
               "Found 1 NA values out of 10 rows.")

  # Missing household_type values
  mock_df <- create_mock_df()
  mock_df$household_type[c(2,4)] <- NA
  expect_error(validate_input(mock_df),
               "Column household_type contains NA values.")
  expect_error(validate_input(mock_df),
                "Found 2 NA values out of 10 rows.")

  # Missing subtotal3 values
  mock_df <- create_mock_df()
  mock_df$subtotal3[c(3,5,7)] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal3 contains NA values.")
  expect_error(validate_input(mock_df),
               "Found 3 NA values out of 10 rows.")

  # Missing values in multiple columns
  mock_df <- create_mock_df()
  mock_df$subtotal2[c(1,6)] <- NA
  mock_df$household_type[c(2,4,5)] <- NA
  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values.")
  expect_error(validate_input(mock_df),
                "Found 2 NA values out of 10 rows.")
  expect_error(validate_input(mock_df),
                "Column household_type contains NA values.")
  expect_error(validate_input(mock_df),
                "Found 3 NA values out of 10 rows.")
})

# Function will allow NAs in non-critical columns
test_that("validate_input allows NAs in non-critical columns", {
  mock_df <- create_mock_df()

  # Missing values in non-critical column
  mock_df$county_table_number[c(1,3,5)] <- NA
  expect_true(validate_input(mock_df))
})

test_that("validate_input detects negative values in subtotals"{
  # Negative value in subtotal2
  mock_df <- create_mock_df()
  mock_df$subtotal2[1] <- -100
  expect_error(validate_input(mock_df),
               "subtotal2 contains negative values.")
  expect_error(validate_input(mock_df),
               "Found 1 negative values.")

  # Multiple negative values in subtotal3
  mock_df <- create_mock_df()
  mock_df$subtotal3[c(2,4)] <- -200
  expect_error(validate_input(mock_df),
               "subtotal3 contains negative values.")
  expect_error(validate_input(mock_df),
                "Found 2 negative values.")
})

# Household type tests
test_that("validate_input detects invalid household_type values", {
  # Invalid household_type value
  mock_df <- create_mock_df()
  mock_df$household_type[1] <- "invalid_type"
  expect_error(validate_input(mock_df),
               "Invalid household_type values found: invalid_type.")
  expect_error(validate_input(mock_df),
               "Valid values are: single_adult, single_parent, married.")

  # Multiple invalid household_type values
  mock_df <- create_mock_df()
  mock_df$household_type[c(2,4)] <- c("foo", "bar")
  expect_error(validate_input(mock_df),
               "Invalid household_type values found: foo, bar.")
  expect_error(validate_input(mock_df),
               "Valid values are: single_adult, single_parent, married.")
})

test_that("Valid household types raise no errors", {
  mock_df <- create_mock_df(3)
  mock_df$household_type <- c('single_adult', 'single_parent', 'married')
  expect_true(validate_input(mock_df))

  # Test with all same valid household type
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
               "Input data frame is empty \\(0 rows\\)")
})

test_that("Warning is raised for large data frames", {
  large_mock <- create_mock_df(1)
  # replicate to speed up processing
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

test_that("Edge cases handled correctly", {
  # Data frame with exactly 1 row
  mock_df <- create_mock_df(1)
  expect_true(validate_input(mock_df))

  # Data frame with exactly 1 million rows (should not raise warning)
  large_mock <- create_mock_df(1)
  large_mock <- do.call(rbind, replicate(1000000, large_mock[1, ], simplify = FALSE))
  expect_silent(validate_input(large_mock))
})

test_that("Numerous errors handled correctly", {
  mock_df <- create_mock_df(10)

  # Introduce multiple errors
  mock_df$subtotal2[c(1,6)] <- NA
  mock_df$household_type[c(2,4,5)] <- NA
  mock_df$subtotal3[c(3,5,7)] <- -100

  expect_error(validate_input(mock_df),
               "Column subtotal2 contains NA values.")
  expect_error(validate_input(mock_df),
                "Found 2 NA values out of 10 rows.")
  expect_error(validate_input(mock_df),
                "Column household_type contains NA values.")
  expect_error(validate_input(mock_df),
                "Found 3 NA values out of 10 rows.")
  expect_error(validate_input(mock_df),
               "subtotal3 contains negative values.")
  expect_error(validate_input(mock_df),
                "Found 3 negative values.")
})

test_that("Order of errors are correct", {
  # Missing columns take precendence
  mock_df <- create_mock_df()
  df_no_subtotal2 <- mock_df[, -which(names(mock_df) == "subtotal2")]
  df_no_subtotal2$subtotal3[1] <- NA
  expect_error(validate_input(df_no_subtotal2),
               "Missing required column: subtotal2")

  # NA detection before negative value detection
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