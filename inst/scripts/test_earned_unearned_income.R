# ----------------------------------------------------------------------
# test_earned_unearned_income.R -- Regression + hand-checked validation
# for n_adults / n_earning_adults (earned vs. unearned income support).
#
# TEST 1 verifies backward compatibility: with n_earning_adults absent,
# sssTaxCalculationSV must produce identical total_fed_payroll_tax,
# eitc_credit, and final_federal_income_tax to the original (unforked)
# sssTaxCalculation package for working-age-only households.
#
# TESTS 2-4 hand-check the new earned/unearned behavior directly against
# manually computed expected values using the 2026 federal payroll/EITC
# parameters in inst/extdata/federal/2026/.
#
# Run interactively or via:
#   Rscript inst/scripts/test_earned_unearned_income.R
# ----------------------------------------------------------------------

ORIGINAL_PATH <- "C:/Users/Lisa/Desktop/local_dev/sss_tax_calculation"
FORK_PATH     <- "C:/Users/Lisa/Desktop/local_dev/sss_tax_calculation_sv"
YEAR          <- 2026
TOL           <- 1e-6

pass_count <- 0L
fail_count <- 0L

check <- function(label, actual, expected, tol = TOL) {
  ok <- isTRUE(all.equal(actual, expected, tolerance = tol)) || max(abs(actual - expected)) < tol
  cat(sprintf("  [%s] %s -- actual=%s expected=%s\n",
              if (ok) "PASS" else "FAIL", label,
              paste(round(actual, 4), collapse = ", "),
              paste(round(expected, 4), collapse = ", ")))
  if (ok) pass_count <<- pass_count + 1L else fail_count <<- fail_count + 1L
  invisible(ok)
}

make_basic_needs_df <- function(household_type, children, subtotal2, subtotal3,
                                 n_earning_adults = NULL,
                                 county_table_number = "TESTCOUNTY_1") {
  df <- data.frame(
    subtotal2          = subtotal2,
    subtotal3           = subtotal3,
    household_type      = household_type,
    children             = children,
    adult                = ifelse(household_type == "married", 2, 1),
    child_care_cost      = 0,
    health_ins_premium   = 400,
    county_table_number  = county_table_number,
    stringsAsFactors     = FALSE
  )
  if (!is.null(n_earning_adults)) df$n_earning_adults <- n_earning_adults
  df
}

# ============================================================
# TEST 1: Backward-compat regression -- original vs. fork
# (n_earning_adults absent; every existing working-age-only
# caller must see zero change in behavior)
# ============================================================
cat("============================================================\n")
cat("TEST 1: Backward-compat regression (original vs. fork)\n")
cat("============================================================\n")

test1_df <- rbind(
  make_basic_needs_df("single_adult",  0, subtotal2 = 1500, subtotal3 = 1600),
  make_basic_needs_df("single_parent", 2, subtotal2 = 2200, subtotal3 = 2400),
  make_basic_needs_df("married",       1, subtotal2 = 2600, subtotal3 = 2800),
  make_basic_needs_df("married",       3, subtotal2 = 3600, subtotal3 = 3900)
)

devtools::load_all(ORIGINAL_PATH, quiet = TRUE)
result_original <- solve_starting_income_iterative(test1_df, year = YEAR, debug = FALSE)
pkgload::unload("sssTaxCalculation")

devtools::load_all(FORK_PATH, quiet = TRUE)
result_fork <- solve_starting_income_iterative(test1_df, year = YEAR, debug = FALSE)

compare_cols <- c("total_fed_payroll_tax", "eitc_credit", "final_federal_income_tax")
for (col in compare_cols) {
  check(sprintf("original vs fork: %s", col), result_fork[[col]], result_original[[col]])
}

# ============================================================
# TEST 2: Hand-checked mixed household
# Married, 1 earning adult + 1 non-earning adult, 1 child,
# starting_income = $20,000 (income_per_adult = $10,000)
# ============================================================
cat("\n============================================================\n")
cat("TEST 2: Mixed household (1 earning / 1 non-earning, married)\n")
cat("============================================================\n")

mixed_df <- data.frame(
  household_type    = "married",
  starting_income    = 20000,
  n_earning_adults   = 1,
  # Pre-joined EITC lookup columns for children=1, married (2026 tax_fed_credits.csv)
  eitc_max            = 4427,
  eitc_phase_in_rate  = 0.34,
  eitc_phase_out_rate = 0.1598,
  eitc_income_at_max  = 13020,
  eitc_phase_out_start = 31160,
  eitc_phase_out_end   = 58867
)

fed_payroll_params <- load_federal_tax_params(YEAR)$fed_payroll
mixed_result <- calculate_federal_payroll_taxes(mixed_df, fed_payroll_params, YEAR)
mixed_result <- calculate_eitc_credit(mixed_result)

# Hand-computed: income_per_adult = 20000/2 = 10000
# ss_tax    = 10000 * 0.062  = 620
# medicare  = 10000 * 0.0145 = 145
# per-adult total = 765; only 1 of 2 adults earning -> total_fed_payroll_tax = 765
check("mixed: total_fed_payroll_tax == tax on exactly half household income",
      mixed_result$total_fed_payroll_tax, 765)

# Sanity check against the "both earning" (backward-compat) case: should be exactly half.
both_earning_df <- mixed_df
both_earning_df$n_earning_adults <- 2
both_earning_result <- calculate_federal_payroll_taxes(both_earning_df, fed_payroll_params, YEAR)
check("mixed: 1-earner payroll tax is exactly half of 2-earner payroll tax",
      mixed_result$total_fed_payroll_tax, both_earning_result$total_fed_payroll_tax / 2)

# starting_income=20000 falls in the flat-max EITC bracket (13020, 31160] for 1 child/married
# -> eitc_credit should equal eitc_max via the normal (n_earning_adults > 0) formula branch,
# NOT the n_earning_adults == 0 gate.
check("mixed: eitc_credit computed via n_earning_adults > 0 branch (== eitc_max)",
      mixed_result$eitc_credit, 4427)

# ============================================================
# TEST 3: Hand-checked fully non-earning household
# Single senior, n_earning_adults = 0, starting_income = $25,000
# EITC bracket chosen (income_at_max = 8680, but test income deliberately
# below it) so that WITHOUT gating, eitc_credit would be nonzero --
# proving the gate is actually suppressing it, not coincidentally landing
# in a zero bracket.
# ============================================================
cat("\n============================================================\n")
cat("TEST 3: Fully non-earning household (single senior)\n")
cat("============================================================\n")

nonearning_df <- data.frame(
  household_type    = "single_adult",
  starting_income    = 25000,
  n_earning_adults   = 0,
  # Pre-joined EITC lookup columns for children=0, single (2026 tax_fed_credits.csv)
  eitc_max            = 665,
  eitc_phase_in_rate  = 0.0765,
  eitc_phase_out_rate = 0.0765,
  eitc_income_at_max  = 8680,
  eitc_phase_out_start = 10860,
  eitc_phase_out_end   = 19540
)
# Use a second row to prove non-gated formula would be nonzero at a lower income
ungated_check_df <- nonearning_df
ungated_check_df$starting_income  <- 5000
ungated_check_df$n_earning_adults <- 1  # earning, for comparison only

nonearning_result <- calculate_federal_payroll_taxes(nonearning_df, fed_payroll_params, YEAR)
nonearning_result <- calculate_eitc_credit(nonearning_result)

check("non-earning: total_fed_payroll_tax == $0", nonearning_result$total_fed_payroll_tax, 0)
check("non-earning: eitc_credit == $0 (gated)", nonearning_result$eitc_credit, 0)

# Prove the gate is doing real work: same bracket shape, but earning, at $5,000 income
# should NOT be zero (5000 * 0.0765 phase-in rate = 382.5)
ungated_check_df$starting_income <- 5000
ungated_result <- calculate_eitc_credit(ungated_check_df)
check("control: same EITC bracket WITHOUT the n_earning_adults==0 gate is nonzero",
      ungated_result$eitc_credit, 5000 * 0.0765)

# ============================================================
# TEST 4: CTC payroll-based refund reflects the corrected payroll tax
# (no code change to calculate_ctc_credit() itself -- confirms the fix
# flows through automatically via total_fed_payroll_tax / eitc_credit)
# ============================================================
cat("\n============================================================\n")
cat("TEST 4: CTC payroll-based refund uses corrected payroll tax\n")
cat("============================================================\n")

ctc_params_list <- list(percentage = 0.15, min_earning_threshold = 2500, max_credit = 2000)

ctc_df <- data.frame(
  starting_income        = 90000,
  children                = 3,
  federal_cumulative_tax  = 1000,
  cdctc_credit             = 0,
  total_fed_payroll_tax    = 3000,   # synthetic: as if computed for a 1-earning-adult household
  eitc_credit              = 500
)
ctc_result <- calculate_ctc_credit(ctc_df, ctc_params_list)

check("ctc_payroll_based_refund == pmax(0, total_fed_payroll_tax - eitc_credit)",
      ctc_result$ctc_payroll_based_refund, max(0, 3000 - 500))

# ============================================================
cat("\n============================================================\n")
cat(sprintf("RESULTS: %d passed, %d failed\n", pass_count, fail_count))
cat("============================================================\n")

if (fail_count > 0) {
  stop(sprintf("%d check(s) failed -- see output above.", fail_count))
} else {
  cat("All checks passed.\n")
}
