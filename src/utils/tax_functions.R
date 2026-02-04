# tax_functions.R
# Tax calculation helper functions (copied from sss_production)

library(dplyr)

calculate_tax_from_brackets <- function(df, brackets_df, 
                                        taxable_income_var = "taxable_income", 
                                        filing_status_var = "filing_status",
                                        output_col = "income_tax",
                                        local_income_tax_var = NULL) {
  df_original <- df %>%
    mutate(row_id = row_number())
  
  df_calc <- df_original %>%
    transmute(
      row_id,
      taxable_income = .data[[taxable_income_var]],
      filing_status = .data[[filing_status_var]],
      local_income_tax_rate = if (!is.null(local_income_tax_var)) .data[[local_income_tax_var]] else 0
    ) %>%
    left_join(brackets_df, by = "filing_status") %>%
    filter(taxable_income > lower_limit) %>%
    mutate(
      taxable_at_bracket = pmin(taxable_income, upper_limit) - lower_limit,
      effective_rate = rate + local_income_tax_rate,
      tax_at_bracket = taxable_at_bracket * effective_rate
    ) %>%
    group_by(row_id) %>%
    summarize(!!output_col := sum(tax_at_bracket, na.rm = TRUE), .groups = "drop")
  
  df_result <- df_original %>%
    left_join(df_calc, by = "row_id") %>%
    select(-row_id)
  
  return(df_result)
}

# ============================================================================
# FEDERAL TAX FUNCTIONS
# ============================================================================

#' Load Federal Payroll Tax Parameters
#'
#' Extracts federal payroll tax parameters for a specific year
#'
#' @param tax_fed_payroll_df Dataframe with federal payroll tax parameters
#' @param year Tax year
#' @return Named list of payroll tax parameters
load_fed_payroll_parameters <- function(tax_fed_payroll_df, year) {
  params_df <- tax_fed_payroll_df %>%
    dplyr::filter(year == !!year)
  
  if (nrow(params_df) == 0) {
    stop(paste("No payroll parameters found for year", year))
  }
  
  as.list(setNames(params_df$value, params_df$variable))
}

#' Calculate Federal Payroll Taxes
#'
#' Calculates Social Security and Medicare taxes
#'
#' @param calculations_df Dataframe with starting_income and household_type
#' @param tax_fed_payroll_df Dataframe with federal payroll tax parameters
#' @param year Tax year
#' @return Dataframe with ss_tax, medicare_tax, and total_fed_payroll_tax columns added
calculate_federal_payroll_taxes <- function(calculations_df, tax_fed_payroll_df, year) {
  
  params <- load_fed_payroll_parameters(tax_fed_payroll_df, year)
  
  calculations_df$ss_income <- ifelse(
    calculations_df$household_type == "married", 
    calculations_df$starting_income / 2, 
    calculations_df$starting_income
  )
  
  calculations_df$medicare_threshold <- ifelse(
    calculations_df$household_type == "married",
    as.numeric(params$medicare_threshold_married),
    as.numeric(params$medicare_threshold_single_hh)
  )
  
  calculations_df$ss_tax <- pmin(
    calculations_df$ss_income,
    as.numeric(params$ss_wage_limit)
  ) * as.numeric(params$ss_rate)
  
  calculations_df$medicare_tax <- 
    (pmin(calculations_df$ss_income, calculations_df$medicare_threshold) * as.numeric(params$medicare_rate)) +
    (pmax(calculations_df$ss_income - calculations_df$medicare_threshold, 0) * as.numeric(params$medicare_additional_rate))
  
  calculations_df$total_fed_payroll_tax <- 
    ifelse(calculations_df$household_type == "married", 
           calculations_df$ss_tax * 2, 
           calculations_df$ss_tax) + 
    calculations_df$medicare_tax
  
  return(calculations_df)
}


# ============================================================================
# EITC FUNCTIONS
# ============================================================================

#' Build EITC Lookup Table
#'
#' Creates a lookup table with EITC parameters for all combinations of
#' number of children (0-2) and household types
#'
#' @param eitc_params Dataframe with EITC parameters from tax_fed_credits_df
#' @return Dataframe with EITC parameters by eitc_children and household_type
build_eitc_lookup <- function(eitc_params) {
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
  
  return(eitc_lookup_df)
}

#' Calculate EITC Credit
#'
#' Calculates Earned Income Tax Credit based on income and pre-joined EITC parameters
#' Assumes EITC lookup columns (eitc_max, eitc_phase_in_rate, etc.) already exist in df
#'
#' @param df Dataframe with starting_income and EITC parameter columns
#' @return Dataframe with eitc_credit column added
calculate_eitc_credit <- function(df) {
  df %>%
    mutate(
      eitc_credit = case_when(
        starting_income <= eitc_income_at_max ~ starting_income * eitc_phase_in_rate,
        starting_income <= eitc_phase_out_start ~ eitc_max,
        starting_income <= eitc_phase_out_end ~ pmax(eitc_max - (eitc_phase_out_rate * (starting_income - eitc_phase_out_start)), 0),
        TRUE ~ 0
      )
    )
}

# ============================================================================
# CDCTC FUNCTIONS
# ============================================================================

#' Extract CDCTC Parameters
#'
#' Extracts Child and Dependent Care Tax Credit parameters from tax data
#'
#' @param cdctc_params Dataframe with CDCTC parameters from tax_fed_credits_df
#' @return Named list of CDCTC parameters
extract_cdctc_params <- function(cdctc_params) {
  list(
    lowest_bracket = cdctc_params$value[cdctc_params$variable == "lowest_bracket"],
    bracket_interval = cdctc_params$value[cdctc_params$variable == "bracket_interval"],
    top_bracket_amount = cdctc_params$value[cdctc_params$variable == "top_bracket_amount"],
    rate_interval = cdctc_params$value[cdctc_params$variable == "rate_interval"],
    lowest_rate = cdctc_params$value[cdctc_params$variable == "lowest_rate"],
    highest_rate = cdctc_params$value[cdctc_params$variable == "highest_rate"],
    max_credit_one_child = cdctc_params %>% filter(num_children == 1, variable == "max_credit") %>% pull(value),
    max_credit_two_children = cdctc_params %>% filter(num_children == 2, variable == "max_credit") %>% pull(value)
  )
}

#' Calculate CDCTC Credit
#'
#' Calculates Child and Dependent Care Tax Credit based on childcare costs and income
#' Credit is non-refundable and limited by federal tax liability
#'
#' @param df Dataframe with starting_income, children, child_care_cost, federal_cumulative_tax
#' @param cdctc_params_list Named list of CDCTC parameters from extract_cdctc_params()
#' @return Dataframe with CDCTC calculation columns added
calculate_cdctc_credit <- function(df, cdctc_params_list) {
  df$cdctc_max <- ifelse(
    df$children == 1,
    cdctc_params_list$max_credit_one_child,
    ifelse(df$children >= 2, cdctc_params_list$max_credit_two_children, 0)
  )
  
  df %>%
    mutate(
      cdctc_eligible_expense = pmin(child_care_cost * 12, cdctc_max),
      cdctc_rate = case_when(
        starting_income <= cdctc_params_list$lowest_bracket ~ cdctc_params_list$highest_rate,
        starting_income >= cdctc_params_list$top_bracket_amount ~ cdctc_params_list$lowest_rate,
        TRUE ~ round(cdctc_params_list$highest_rate - ((floor((starting_income - cdctc_params_list$lowest_bracket) / cdctc_params_list$bracket_interval)) * cdctc_params_list$rate_interval), 2)
      ),
      cdctc_estimate = cdctc_eligible_expense * cdctc_rate,
      cdctc_credit = pmin(cdctc_estimate, federal_cumulative_tax)
    )
}

# ============================================================================
# CTC FUNCTIONS
# ============================================================================

#' Extract CTC Parameters
#'
#' Extracts Child Tax Credit parameters from tax data
#'
#' @param ctc_params Dataframe with CTC parameters from tax_fed_credits_df
#' @return Named list of CTC parameters
extract_ctc_params <- function(ctc_params) {
  list(
    percentage = ctc_params %>% filter(variable == "percentage") %>% pull(value),
    min_earning_threshold = ctc_params %>% filter(variable == "min_earning_threshold") %>% pull(value),
    max_credit = ctc_params %>% filter(variable == "max_per_child") %>% pull(value)
  )
}

#' Calculate CTC Credit
#'
#' Calculates Child Tax Credit with both refundable and non-refundable portions
#' Refundable portion differs for families with 1-2 children vs 3+ children
#'
#' @param df Dataframe with starting_income, children, federal_cumulative_tax, cdctc_credit, total_fed_payroll_tax, eitc_credit
#' @param ctc_params_list Named list of CTC parameters from extract_ctc_params()
#' @return Dataframe with CTC calculation columns added
calculate_ctc_credit <- function(df, ctc_params_list) {
  df %>%
    mutate(
      ctc_credit_base = children * ctc_params_list$max_credit,
      federal_tax_after_cdctc = pmax(federal_cumulative_tax - cdctc_credit, 0),
      ctc_nonrefundable = pmin(ctc_credit_base, federal_tax_after_cdctc),
      ctc_income_based_refund = pmax(0, ctc_params_list$percentage * (starting_income - ctc_params_list$min_earning_threshold)),
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
}

# ============================================================================
# FEDERAL INCOME TAX FUNCTIONS
# ============================================================================

#' Calculate Federal Income Tax Deductions and Taxable Income
#'
#' Calculates federal standard deduction, ESI premium deduction, and taxable income
#'
#' @param df Dataframe with starting_income, household_type, health_ins_premium
#' @param federal_standard_deduction Dataframe with standard deductions by filing status
#' @return Dataframe with deduction and taxable income columns added
calculate_federal_income_tax <- function(df, federal_standard_deduction) {
  df %>%
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
}

#' Calculate Final Federal Income Tax
#'
#' Applies non-refundable and refundable credits to calculate final federal tax liability
#' Non-refundable credits (CDCTC) reduce tax liability but cannot go below zero
#' Refundable credits (EITC, CTC refundable) can result in negative tax (refund)
#'
#' @param df Dataframe with federal_cumulative_tax, cdctc_credit, ctc_refundable, eitc_credit
#' @return Dataframe with final federal income tax calculation columns added
calculate_final_federal_income_tax <- function(df) {
  df %>%
    mutate(
      fed_nonrefundable_credit_applied = pmin(cdctc_credit, federal_cumulative_tax),
      federal_tax_after_nonrefundable = pmax(federal_cumulative_tax - fed_nonrefundable_credit_applied, 0),
      federal_total_refundable_credits = ctc_refundable + eitc_credit,
      federal_tax_liability_with_refund = federal_tax_after_nonrefundable - federal_total_refundable_credits,
      final_federal_income_tax = pmax(federal_tax_liability_with_refund, 0)
    )
}
