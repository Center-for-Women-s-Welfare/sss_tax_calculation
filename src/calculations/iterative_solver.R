# iterative_solver.R
# Iterative solver for SSS starting income calculation

# Load utilities
source(file.path(dirname(dirname(getwd())), "sss_tax_calculation", "src", "utils", "validation.R"))
source(file.path(dirname(dirname(getwd())), "sss_tax_calculation", "src", "utils", "diagnostics.R"))

#' Solve for Starting Income Using Iterative Convergence (Phase 1: Federal Taxes Only)
#'
#' Finds the minimum starting income needed for families to achieve self-sufficiency
#' by iteratively solving for the equilibrium where:
#' starting_income = (subtotal2 * 12) + total_federal_taxes - total_federal_credits
#'
#' Phase 1 Implementation: Federal taxes only (payroll, income tax, EITC, CDCTC, CTC)
#' Phase 2 (Future): Will add state income tax, state payroll tax, and state credits
#'
#' @param df Dataframe with basic needs data after calculate_basic_needs.R has run
#'           Required columns: subtotal2, subtotal3, household_type, children, adult,
#'           child_care_cost, health_ins_premium, county_table_number
#' @param year Tax year (e.g., 2026)
#' @param state State code (e.g., "IA") - included for future compatibility, not used in Phase 1
#' @param tax_params List of federal tax parameter dataframes:
#'                   - fed_payroll: Federal payroll tax rates and limits
#'                   - fed_credits: EITC, CDCTC, CTC parameters
#'                   - fed_brackets: Federal income tax brackets
#'                   - fed_sd: Federal standard deductions
#' @param max_iterations Maximum number of iterations before fallback (default: 100)
#' @param tolerance Convergence tolerance in dollars (default: 1.0)
#' @param debug If TRUE, print detailed iteration progress (default: FALSE)
#' @return Dataframe with all input columns plus:
#'         - starting_income: Converged annual gross income
#'         - iteration_count: Number of iterations to convergence
#'         - converged: TRUE if converged, FALSE if fallback used
#'         Plus all intermediate federal tax calculation columns
#'
#' @examples
#' # Phase 1: Federal taxes only
#' result <- solve_starting_income_iterative(
#'   df = calculations_df,
#'   year = 2026,
#'   state = "IA",  # Not used in Phase 1, but included for future compatibility
#'   tax_params = list(
#'     fed_payroll = tax_fed_payroll_df,
#'     fed_credits = tax_fed_credits_df,
#'     fed_brackets = tax_fed_income_brackets_df,
#'     fed_sd = tax_fed_sd_df
#'   ),
#'   max_iterations = 100,
#'   tolerance = 1.0,
#'   debug = TRUE
#' )
solve_starting_income_iterative <- function(df, 
                                            year, 
                                            state, 
                                            tax_params,
                                            max_iterations = 100,
                                            tolerance = 1.0,
                                            debug = FALSE) {
  
  # Validate input
  validate_input(df)
  
  # Source existing tax functions from sss_production
  tax_functions_path <- file.path(
    sss_code_path(),
    "sss_production",
    "src",
    "2026",
    "analysis",
    "tax_functions.R"
  )
  
  if (!file.exists(tax_functions_path)) {
    stop(paste(
      "Cannot find tax_functions.R at:", tax_functions_path,
      "\nEnsure sss_production repository is available and SSS_CODE_BASE is set."
    ))
  }
  
  source(tax_functions_path)
  
  # Initialize starting income using fallback formula
  df$starting_income <- df$subtotal3 * 1.20 * 12
  df$iteration_count <- 0
  df$converged <- FALSE
  
  if (debug) {
    cat("\n=== Starting Iterative Solver ===\n")
    cat("Initial starting_income range:", 
        sprintf("$%.2f - $%.2f", min(df$starting_income), max(df$starting_income)), "\n")
    cat("Max iterations:", max_iterations, "\n")
    cat("Tolerance: $", tolerance, "\n\n")
  }
  
  # Main iteration loop
  for (iter in 1:max_iterations) {
    
    # Store previous income for convergence check
    df$previous_income <- df$starting_income
    
    # ---- Calculate Federal Payroll Taxes ----
    df <- calculate_federal_payroll_taxes(
      calculations_df = df,
      tax_fed_payroll_df = tax_params$fed_payroll,
      year = year
    )
    
    # ---- State Payroll Taxes (Phase 2 - Future) ----
    # Phase 1: Set to 0
    df$state_payroll_tax <- 0
    
    # ---- Calculate Federal Income Tax ----
    # Standard deduction
    federal_standard_deduction <- tax_params$fed_sd %>%
      filter(year == !!year) %>%
      select(-year) %>%
      pivot_wider(names_from = filing_status, values_from = deduction)
    
    df <- df %>%
      mutate(
        fed_sd = case_when(
          household_type == "married" ~ federal_standard_deduction$married,
          household_type == "single_parent" ~ federal_standard_deduction$single_parent,
          household_type == "single_adult" ~ federal_standard_deduction$single_adult,
          TRUE ~ 0
        ),
        esi_premium_deduction = health_ins_premium * 12,
        total_fed_deductions = fed_sd + esi_premium_deduction,
        taxable_income = pmax(starting_income - total_fed_deductions, 0),
        filing_status = household_type
      )
    
    # Federal income tax from brackets
    federal_tax_brackets <- tax_params$fed_brackets %>%
      filter(year == !!year) %>%
      select(-year)
    
    df <- calculate_tax_from_brackets(
      df = df,
      brackets_df = federal_tax_brackets,
      taxable_income_var = "taxable_income",
      filing_status_var = "filing_status",
      output_col = "federal_cumulative_tax"
    )
    
    # ---- Calculate Federal Tax Credits ----
    credit_params <- tax_params$fed_credits %>%
      filter(year == !!year)
    
    eitc_params <- credit_params %>% filter(credit == "eitc")
    cdctc_params <- credit_params %>% filter(credit == "cdctc")
    ctc_params <- credit_params %>% filter(credit == "ctc")
    
    # EITC
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
    
    df <- df %>%
      mutate(eitc_children = pmin(children, 2)) %>%
      left_join(eitc_lookup_df, by = c("eitc_children", "household_type"))
    
    df <- df %>%
      mutate(
        eitc_credit = case_when(
          starting_income <= eitc_income_at_max ~ starting_income * eitc_phase_in_rate,
          starting_income <= eitc_phase_out_start ~ eitc_max,
          starting_income <= eitc_phase_out_end ~ pmax(eitc_max - (eitc_phase_out_rate * (starting_income - eitc_phase_out_start)), 0),
          TRUE ~ 0
        )
      )
    
    # CDCTC
    cdctc_lowest_bracket <- cdctc_params$value[cdctc_params$variable == "lowest_bracket"]
    cdctc_bracket_interval <- cdctc_params$value[cdctc_params$variable == "bracket_interval"]
    cdctc_top_bracket_amount <- cdctc_params$value[cdctc_params$variable == "top_bracket_amount"]
    cdctc_rate_interval <- cdctc_params$value[cdctc_params$variable == "rate_interval"]
    cdctc_lowest_rate <- cdctc_params$value[cdctc_params$variable == "lowest_rate"]
    cdctc_highest_rate <- cdctc_params$value[cdctc_params$variable == "highest_rate"]
    
    cdctc_max_credit_one_child <- cdctc_params %>% 
      filter(num_children == 1, variable == "max_credit") %>% 
      pull(value)
    cdctc_max_credit_two_children <- cdctc_params %>% 
      filter(num_children == 2, variable == "max_credit") %>% 
      pull(value)
    
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
    
    # CTC
    ctc_percentage <- ctc_params %>% filter(variable == "percentage") %>% pull(value)
    ctc_min_earning_threshold <- ctc_params %>% filter(variable == "min_earning_threshold") %>% pull(value)
    ctc_max_credit <- ctc_params %>% filter(variable == "max_per_child") %>% pull(value)
    
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
    
    # ---- State Income Tax (Phase 2 - Future) ----
    # Phase 1: Set to 0
    df$state_cumulative_tax <- 0
    
    # ---- State Tax Credits (Phase 2 - Future) ----
    # Phase 1: Set to 0
    df$state_refundable_credits <- 0
    df$state_nonrefundable_credits <- 0
    
    # ---- Calculate Totals (Phase 1: Federal Only) ----
    df <- df %>%
      mutate(
        total_taxes = coalesce(total_fed_payroll_tax, 0) + 
                      coalesce(federal_cumulative_tax, 0),
        
        total_credits = coalesce(eitc_credit, 0) + 
                        coalesce(cdctc_credit, 0) + 
                        coalesce(ctc_credit, 0)
      )
    
    # ---- Calculate New Starting Income ----
    df$new_starting_income <- (df$subtotal2 * 12) + df$total_taxes - df$total_credits
    
    # ---- Check Convergence ----
    df$income_diff <- abs(df$new_starting_income - df$previous_income)
    df$row_converged <- df$income_diff < tolerance
    
    # Update starting_income for next iteration
    df$starting_income <- df$new_starting_income
    
    # Update iteration count and converged status
    df$iteration_count <- ifelse(df$converged, df$iteration_count, iter)
    df$converged <- df$converged | df$row_converged
    
    # Debug output
    if (debug) {
      print_iteration_progress(iter, df, show_every = 10)
    }
    
    # Early exit if all rows converged
    if (all(df$converged)) {
      if (debug) {
        cat(sprintf("\n✓ All rows converged at iteration %d\n", iter))
      }
      break
    }
  }
  
  # ---- Apply Fallback for Non-Converged Rows ----
  non_converged_idx <- which(!df$converged)
  
  if (length(non_converged_idx) > 0) {
    warning(sprintf(
      "%d rows (%.2f%%) did not converge after %d iterations. Using fallback calculation.",
      length(non_converged_idx),
      length(non_converged_idx) / nrow(df) * 100,
      max_iterations
    ))
    
    # Apply fallback formula
    df$starting_income[non_converged_idx] <- df$subtotal3[non_converged_idx] * 1.20 * 12
    df$iteration_count[non_converged_idx] <- max_iterations
    
    if (debug && length(non_converged_idx) <= 10) {
      cat("\nNon-converged rows:\n")
      if ("family_type" %in% names(df)) {
        print(df[non_converged_idx, c("county_table_number", "family_type", "starting_income")])
      }
    }
  }
  
  # ---- Print Summary ----
  print_convergence_summary(df, debug)
  
  # Clean up temporary columns
  df <- df %>%
    select(-previous_income, -new_starting_income, -income_diff, -row_converged)
  
  return(df)
}
