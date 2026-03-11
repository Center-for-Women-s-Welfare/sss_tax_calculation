#' Iterative solver for SSS starting income calculation
#'
#' Given a basic needs dataframe, iteratively solves for the gross annual income
#' required to cover those needs after accounting for federal taxes and credits.
#'
#' @param df Dataframe with basic needs columns (see validate_input for required columns)
#' @param year Tax year (e.g., 2026)
#' @param state State abbreviation (reserved for future state tax support)
#' @param max_iterations Maximum number of solver iterations (default: 100)
#' @param tolerance Convergence threshold in dollars (default: 1.0)
#' @param debug If TRUE, print iteration progress and diagnostics
#' @return Input dataframe with starting_income and tax breakdown columns added
#' @export
solve_starting_income_iterative <- function(df,
                                            year,
                                            state = NULL,
                                            max_iterations = 100,
                                            tolerance = 1.0,
                                            debug = FALSE) {

  validate_input(df)

  tax_params <- load_federal_tax_params(year)

  credit_params <- tax_params$fed_credits %>%
    filter(year == !!year)

  eitc_params  <- credit_params %>% filter(credit == "eitc")
  cdctc_params <- credit_params %>% filter(credit == "cdctc")
  ctc_params   <- credit_params %>% filter(credit == "ctc")

  federal_standard_deduction <- tax_params$fed_sd %>%
    filter(year == !!year) %>%
    select(-year) %>%
    pivot_wider(names_from = filing_status, values_from = deduction)

  federal_tax_brackets <- tax_params$fed_brackets %>%
    filter(year == !!year) %>%
    select(-year)

  eitc_lookup_df    <- build_eitc_lookup(eitc_params)
  cdctc_params_list <- extract_cdctc_params(cdctc_params)
  ctc_params_list   <- extract_ctc_params(ctc_params)

  df$starting_income  <- df$subtotal3 * 1.20 * 12
  df$iteration_count  <- 0
  df$converged        <- FALSE

  df <- df %>%
    mutate(eitc_children = pmin(children, 2)) %>%
    left_join(eitc_lookup_df, by = c("eitc_children", "household_type"), relationship = "many-to-one")

  if (debug) {
    cat("\n=== Starting Iterative Solver ===\n")
    cat("Initial starting_income range:",
        sprintf("$%.2f - $%.2f", min(df$starting_income), max(df$starting_income)), "\n")
    cat("Max iterations:", max_iterations, "\n")
    cat("Tolerance: $", tolerance, "\n\n")
  }

  for (iter in 1:max_iterations) {

    df$previous_income <- df$starting_income

    df <- df %>%
      select(-any_of(c(
        "ss_income", "medicare_threshold", "ss_tax", "medicare_tax", "total_fed_payroll_tax",
        "fed_sd", "esi_premium_deduction", "total_fed_deductions", "taxable_income",
        "federal_cumulative_tax",
        "eitc_credit",
        "cdctc_max", "cdctc_eligible_expense", "cdctc_rate", "cdctc_estimate", "cdctc_credit",
        "ctc_credit_base", "federal_tax_after_cdctc", "ctc_nonrefundable",
        "ctc_income_based_refund", "ctc_refund_1to2_children", "ctc_payroll_based_refund",
        "ctc_refund_3plus_children", "ctc_refundable", "ctc_credit",
        "total_taxes", "total_credits", "new_starting_income", "income_diff", "row_converged"
      )))

    df <- calculate_federal_payroll_taxes(df, tax_params$fed_payroll, year)
    df <- calculate_federal_income_tax(df, federal_standard_deduction)
    df <- calculate_tax_from_brackets(df, federal_tax_brackets,
                                      taxable_income_var = "taxable_income",
                                      filing_status_var  = "filing_status",
                                      output_col         = "federal_cumulative_tax")
    df <- calculate_eitc_credit(df)
    df <- calculate_cdctc_credit(df, cdctc_params_list)
    df <- calculate_ctc_credit(df, ctc_params_list)

    df <- df %>%
      mutate(
        total_taxes  = coalesce(total_fed_payroll_tax, 0) + coalesce(federal_cumulative_tax, 0),
        total_credits = coalesce(eitc_credit, 0) + coalesce(cdctc_credit, 0) + coalesce(ctc_credit, 0)
      )

    df$new_starting_income <- (df$subtotal3 * 12) + df$total_taxes - df$total_credits
    df$income_diff         <- abs(df$new_starting_income - df$previous_income)
    df$row_converged       <- df$income_diff < tolerance
    df$final_income_diff   <- df$income_diff
    df$starting_income     <- df$new_starting_income
    df$iteration_count     <- ifelse(df$converged, df$iteration_count, iter)
    df$converged           <- df$converged | df$row_converged

    if (debug) print_iteration_progress(iter, df, show_every = 1)
    if (all(df$converged)) {
      if (debug) cat(sprintf("\n All rows converged at iteration %d\n", iter))
      break
    }
  }

  non_converged_idx <- which(!df$converged)
  if (length(non_converged_idx) > 0) {
    warning(sprintf(
      "%d rows (%.2f%%) did not converge after %d iterations. Using fallback.",
      length(non_converged_idx),
      length(non_converged_idx) / nrow(df) * 100,
      max_iterations
    ))
    df$starting_income[non_converged_idx] <- df$subtotal3[non_converged_idx] * 1.20 * 12
    df$iteration_count[non_converged_idx] <- max_iterations
  }

  print_convergence_summary(df, debug)

  df <- calculate_final_federal_income_tax(df)

  df %>%
    select(-any_of(c("previous_income", "new_starting_income", "income_diff", "row_converged")))
}
