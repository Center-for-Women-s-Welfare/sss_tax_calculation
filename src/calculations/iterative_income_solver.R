# Iterative solver for SSS starting income calculation

# Load required libraries
library(dplyr)
library(tidyr)

# Load utilities
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "validation.R"))
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "utils", "diagnostics.R"))

solve_starting_income_iterative <- function(df, 
                                            year, 
                                            tax_params,
                                            max_iterations = 100,
                                            tolerance = 1.0,
                                            debug = FALSE) {
  
  # Validate input
  validate_input(df)
  
  # Source existing tax functions
  source(file.path(sss_code_path(), "src", "2026", "analysis", "tax_functions.R"))
  
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
  
  # Build EITC lookup ONCE
  eitc_lookup_df <- expand.grid(
    eitc_children = 0:2,
    household_type = c("single_parent", "single_adult", "married"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      filing_status = if_else(household_type == "married", "married", "single")
    ) %>%
    left_join(
      eitc_params %>% filter(variable == "max_credit") %>% select(num_children, value),
      by = c("eitc_children" = "num_children")
    ) %>% rename(eitc_max = value) %>%
    left_join(
      eitc_params %>% filter(variable == "phase_in_rate") %>% select(num_children, value),
      by = c("eitc_children" = "num_children")
    ) %>% rename(eitc_phase_in_rate = value) %>%
    left_join(
      eitc_params %>% filter(variable == "phase_out_rate") %>% select(num_children, value),
      by = c("eitc_children" = "num_children")
    ) %>% rename(eitc_phase_out_rate = value) %>%
    left_join(
      eitc_params %>% filter(variable == "income_at_max") %>% select(num_children, value),
      by = c("eitc_children" = "num_children")
    ) %>% rename(eitc_income_at_max = value) %>%
    left_join(
      eitc_params %>% filter(variable == "phase_out_start") %>% select(num_children, filing_status, value),
      by = c("eitc_children" = "num_children", "filing_status")
    ) %>% rename(eitc_phase_out_start = value) %>%
    left_join(
      eitc_params %>% filter(variable == "phase_out_end") %>% select(num_children, filing_status, value),
      by = c("eitc_children" = "num_children", "filing_status")
    ) %>% rename(eitc_phase_out_end = value)
  
  # Extract CDCTC parameters ONCE
  cdctc_lowest_bracket <- cdctc_params$value[cdctc_params$variable == "lowest_bracket"]
  cdctc_bracket_interval <- cdctc_params$value[cdctc_params$variable == "bracket_interval"]
  cdctc_top_bracket_amount <- cdctc_params$value[cdctc_params$variable == "top_bracket_amount"]
  cdctc_rate_interval <- cdctc_params$value[cdctc_params$variable == "rate_interval"]
  cdctc_lowest_rate <- cdctc_params$value[cdctc_params$variable == "lowest_rate"]
  cdctc_highest_rate <- cdctc_params$value[cdctc_params$variable == "highest_rate"]
  cdctc_max_credit_one_child <- cdctc_params %>% filter(num_children == 1, variable == "max_credit") %>% pull(value)
  cdctc_max_credit_two_children <- cdctc_params %>% filter(num_children == 2, variable == "max_credit") %>% pull(value)
  
  # Extract CTC parameters ONCE
  ctc_percentage <- ctc_params %>% filter(variable == "percentage") %>% pull(value)
  ctc_min_earning_threshold <- ctc_params %>% filter(variable == "min_earning_threshold") %>% pull(value)
  ctc_max_credit <- ctc_params %>% filter(variable == "max_per_child") %>% pull(value)
  
  # Initialize
  df$starting_income <- df$subtotal3 * 1.20 * 12
  df$iteration_count <- 0
  df$converged <- FALSE
  
  # Join EITC lookup ONCE before loop (parameters don't change)
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
      tax_fed_payroll_df = tax_params$fed_payroll
    )
    
    # ==== FEDERAL INCOME TAX ====
    df <- df %>%
      mutate(
        fed_sd = case_when(
          household_type == "married" ~ federal_standard_deduction$married,
          household_type == "single_parent" ~ federal_standard_deduction$single_parent,
          household_type == "single_adult" ~ federal_standard_deduction$single_adult
        ),
        esi_premium_deduction = health_ins_premium * 12,
        total_fed_deductions = fed_sd + esi_premium_deduction,
        taxable_income = pmax(starting_income - total_fed_deductions, 0),
        filing_status = household_type
      )
    
    df <- calculate_tax_from_brackets(
      df = df,
      brackets_df = federal_tax_brackets,
      taxable_income_var = "taxable_income",
      filing_status_var = "filing_status",
      output_col = "federal_cumulative_tax"
    )
    
    # ==== EITC ====
    # EITC parameters already joined before loop, just calculate credit
    df <- df %>%
      mutate(
        eitc_credit = case_when(
          starting_income <= eitc_income_at_max ~ starting_income * eitc_phase_in_rate,
          starting_income <= eitc_phase_out_start ~ eitc_max,
          starting_income <= eitc_phase_out_end ~ pmax(eitc_max - (eitc_phase_out_rate * (starting_income - eitc_phase_out_start)), 0),
          TRUE ~ 0
        )
      )
    
    # ==== CDCTC ====
    
    df$cdctc_max <- ifelse(
      df$children == 1,
      cdctc_max_credit_one_child,
      ifelse(df$children >= 2, cdctc_max_credit_two_children, 0)
    )
    
    df <- df %>%
      mutate(
        cdctc_eligible_expense = pmin(child_care_cost * 12, cdctc_max),
        cdctc_rate = case_when(
          starting_income <= cdctc_lowest_bracket ~ cdctc_highest_rate,
          starting_income >= cdctc_top_bracket_amount ~ cdctc_lowest_rate,
          TRUE ~ round(cdctc_highest_rate - ((floor((starting_income - cdctc_lowest_bracket) / cdctc_bracket_interval)) * cdctc_rate_interval), 2)
        ),
        cdctc_estimate = cdctc_eligible_expense * cdctc_rate,
        cdctc_credit = pmin(cdctc_estimate, federal_cumulative_tax)
      )
    
    # ==== CTC ====
    df <- df %>%
      mutate(
        ctc_credit_base = children * ctc_max_credit,
        federal_tax_after_cdctc = pmax(federal_cumulative_tax - cdctc_credit, 0),
        ctc_nonrefundable = pmin(ctc_credit_base, federal_tax_after_cdctc),
        ctc_income_based_refund = pmax(0, ctc_percentage * (starting_income - ctc_min_earning_threshold)),
        ctc_refund_1to2_children = ifelse(
          children <= 2,
          pmin(ctc_credit_base - ctc_nonrefundable, ctc_income_based_refund),
          0
        ),
        ctc_payroll_based_refund = pmax(0, total_fed_payroll_tax - eitc_credit),
        ctc_refund_3plus_children = ifelse(
          children >= 3,
          pmin(ctc_credit_base - ctc_nonrefundable, ctc_payroll_based_refund),
          0
        ),
        ctc_refundable = ctc_refund_1to2_children + ctc_refund_3plus_children,
        ctc_credit = ctc_nonrefundable + ctc_refundable
      )
    
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
  df <- df %>%
    mutate(
      # Step 1: Apply nonrefundable CDCTC credit (capped at cumulative tax)
      fed_nonrefundable_credit_applied = pmin(cdctc_credit, federal_cumulative_tax),
      
      # Step 2: Subtract nonrefundable credit
      federal_tax_after_nonrefundable = pmax(federal_cumulative_tax - fed_nonrefundable_credit_applied, 0),
      
      # Step 3: Sum refundable CTC + EITC
      federal_total_refundable_credits = ctc_refundable + eitc_credit,
      
      # Step 4: Subtract refundable credits
      federal_tax_liability_with_refund = federal_tax_after_nonrefundable - federal_total_refundable_credits,
      
      # Step 5: Final tax owed (never negative)
      final_federal_income_tax = pmax(federal_tax_liability_with_refund, 0)
    )
  
  # Clean up temporary iteration variables only
  # Keep all calculated values and parameters for downstream use
  df <- df %>%
    select(-any_of(c("previous_income", "new_starting_income", "income_diff", "row_converged")))
  
  return(df)
}
