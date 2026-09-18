# test-tax-brackets.R
#
# Regression test for calculate_tax_from_brackets().
#
# Guards a latent bug fixed on this branch: a strict `>` filter against a
# lower_limit of 0 excluded every bracket for a row whose taxable_income was
# exactly 0, so the row had no bracket rows left to sum and vanished from the
# per-row summary entirely -- the subsequent left_join then left it NA rather
# than 0, silently zeroing out any downstream credit that depended on it.
# Fixed via `>=`, which only changes this degenerate all-excluded case (for
# any bracket where taxable_income == lower_limit, the tax contribution is 0
# either way, so no other value changes).
#
# Run with: testthat::test_file("tests/testthat/test-tax-brackets.R")
# or as part of the full suite via devtools::test().

library(testthat)

test_that("calculate_tax_from_brackets returns $0, not NA, when taxable_income is exactly 0", {
  brackets <- data.frame(
    filing_status = "married",
    bracket_num   = c(1, 2),
    lower_limit   = c(0, 24801),
    upper_limit   = c(24800, 100800),
    rate          = c(0.10, 0.12),
    stringsAsFactors = FALSE
  )
  df <- data.frame(taxable_income = c(0, 100, 24801), filing_status = "married",
                    stringsAsFactors = FALSE)

  out <- calculate_tax_from_brackets(df, brackets, output_col = "federal_cumulative_tax")

  expect_false(any(is.na(out$federal_cumulative_tax)))
  expect_equal(out$federal_cumulative_tax, c(0, 10, 2480))
})
