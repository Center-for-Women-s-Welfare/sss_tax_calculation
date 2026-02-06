# Iterative solver for SSS starting income calculation

# Load required libraries
library(dplyr)
library(tidyr)

# Load utilities
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "validation.R"))
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "diagnostics.R"))
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "tax_functions.R"))
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "data_loader.R"))

solve_starting_income_iterative <- function(df, 
                                            year,
                                            state = NULL,  
                                            max_iterations = 100,
                                            tolerance = 1.0,
                                            debug = FALSE) {
  
  # Validate input
  validate_input(df)
  
  # Load federal tax parameters from CSV files
  tax_params <- load_federal_tax_params(year)
  
  # Prepare tax parameters
  credit_params <- tax_params$fed_credits %>%
    filter(year == !!year)
  
  eitc_params <- credit_params %>% filter(credit == "eitc")
  cdctc_params <- credit_params %>% filter(credit == "cdctc")
  ctc_params <- credit_params %>% filter(credit == "ctc")
  
  federal_standard_deduction <- tax_params$fed_sd %>%
    filter(year == !!year) %>%
    select(-year) %>%
    pivot_wider(names_from = filing_status, values_from = deduction)
  
  federal_tax_brackets <- tax_params$fed_brackets %>%
    filter(year == !!year) %>%
    select(-year)
  
  # Build EITC lookup and extract credit parameters
  eitc_lookup_df <- build_eitc_lookup(eitc_params)
  cdctc_params_list <- extract_cdctc_params(cdctc_params)
  ctc_params_list <- extract_ctc_params(ctc_params)
  
  # Initialize
  df$starting_income <- df$subtotal3 * 1.20 * 12
  df$iteration_count <- 0
  df$converged <- FALSE
  
  # Join EITC lookup ONCE before loop
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
  
  # Main iteration loop
  for (iter in 1:max_iterations) {
    
    df$previous_income <- df$starting_income
    
    # Remove calculated columns from previous iteration to prevent .x/.y suffixes in joins
    # Keep parameter columns that were joined before the loop (eitc_*, filing_status, eitc_children)
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
    
    # ==== FEDERAL PAYROLL TAX ====
    df <- calculate_federal_payroll_taxes(
      calculations_df = df,
      tax_fed_payroll_df = tax_params$fed_payroll,
      year = year
    )
    
    # ==== FEDERAL INCOME TAX ====
    df <- calculate_federal_income_tax(df, federal_standard_deduction)
    
    df <- calculate_tax_from_brackets(
      df = df,
      brackets_df = federal_tax_brackets,
      taxable_income_var = "taxable_income",
      filing_status_var = "filing_status",
      output_col = "federal_cumulative_tax"
    )
    
    # ==== EITC ====
    df <- calculate_eitc_credit(df)
    
    # ==== CDCTC ====
    df <- calculate_cdctc_credit(df, cdctc_params_list)
    
    # ==== CTC ====
    df <- calculate_ctc_credit(df, ctc_params_list)
    
    # ==== Calculate Totals (Phase 1: Federal Only) ====
    df <- df %>%
      mutate(
        total_taxes = coalesce(total_fed_payroll_tax, 0) + coalesce(federal_cumulative_tax, 0),
        total_credits = coalesce(eitc_credit, 0) + coalesce(cdctc_credit, 0) + coalesce(ctc_credit, 0)
      )
    
    # ==== Calculate New Starting Income ====
    df$new_starting_income <- (df$subtotal2 * 12) + df$total_taxes - df$total_credits
    
    # ==== Check Convergence ====
    df$income_diff <- abs(df$new_starting_income - df$previous_income)
    df$row_converged <- df$income_diff < tolerance
    
    # Store final income diff for non-converged rows
    df$final_income_diff <- df$income_diff
    
    df$starting_income <- df$new_starting_income
    df$iteration_count <- ifelse(df$converged, df$iteration_count, iter)
    df$converged <- df$converged | df$row_converged
    
    if (debug) {
      print_iteration_progress(iter, df, show_every = 1)
    }
    
    if (all(df$converged)) {
      if (debug) cat(sprintf("\n✓ All rows converged at iteration %d\n", iter))
      break
    }
  }
  
  # ==== Apply Fallback ====
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
  
  # ==== Print Summary ====
  print_convergence_summary(df, debug)
  
  # ==== Calculate Final Federal Income Tax ====
  df <- calculate_final_federal_income_tax(df)
  
  # Clean up temporary iteration variables only.
  # Keep final_income_diff to show convergence distance
  df <- df %>%
    select(-any_of(c("previous_income", "new_starting_income", "income_diff", "row_converged")))
  
  return(df)
}
