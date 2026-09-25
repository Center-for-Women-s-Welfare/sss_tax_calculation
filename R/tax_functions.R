# tax_functions.R
# Tax calculation helper functions

# ============================================================================
# SHARED TAX HELPERS
# ============================================================================

# ---------- BRACKET LOOKUP HELPER -----------------------------------

# Match each df row to its income bracket value using findInterval().
# Tries exact filing_status first; falls back to "all" for unmatched rows.
# This replaces the per-iteration fuzzyjoin in the generic credit loop.
#
# If number of children is provided, it will first try to match rows with the 
# exact num_children value, then fall back to rows with num_children == NA.
#
# bracket_df must have columns: filing_status, income_min, income_max, value.
# Returns a numeric vector length n with NA where no bracket matched.
.bracket_lookup <- function(income, filing_status, bracket_df, children = NULL) {
  n      <- length(income)
  result <- rep(NA_real_, n)
  
  bracket_df <- bracket_df %>%
    dplyr::mutate(
      filing_status = dplyr::if_else(
        is.na(filing_status) | trimws(filing_status) == "", "all",
        as.character(filing_status)
      )
    )
  
  if (!"num_children" %in% names(bracket_df)) {
    bracket_df$num_children <- NA_real_
  }
  bracket_df <- bracket_df %>%
    dplyr::mutate(num_children = suppressWarnings(as.numeric(num_children)))
  
  has_child_filter <- !is.null(children)
  
  lookup_subset <- function(idx_rows, rows_df) {
    if (length(idx_rows) == 0L || nrow(rows_df) == 0L) return(invisible(NULL))
    
    rows_df <- rows_df %>% dplyr::arrange(income_min)
    
    idx         <- findInterval(income[idx_rows], rows_df$income_min)
    clipped_idx <- pmax(pmin(idx, nrow(rows_df)), 1L)
    in_range    <- idx >= 1L & idx <= nrow(rows_df) &
      income[idx_rows] <= rows_df$income_max[clipped_idx]
    
    hit_rows <- idx_rows[in_range]
    hit_vals <- as.numeric(rows_df$value[clipped_idx[in_range]])
    
    can_fill <- is.na(result[hit_rows])
    result[hit_rows[can_fill]] <<- hit_vals[can_fill]
  }
  
  fs_levels <- c(setdiff(unique(bracket_df$filing_status), "all"), "all")
  
  for (fs in fs_levels) {
    fs_rows <- if (fs == "all") {
      bracket_df %>% dplyr::filter(filing_status == "all")
    } else {
      bracket_df %>% dplyr::filter(filing_status == fs)
    }
    
    idx_rows <- if (fs == "all") which(is.na(result)) else which(filing_status == fs)
    if (length(idx_rows) == 0L || nrow(fs_rows) == 0L) next
    
    if (has_child_filter) {
      child_vals <- children[idx_rows]
      
      # exact child-specific rows
      exact_child_rows <- fs_rows %>% dplyr::filter(!is.na(num_children))
      for (cv in unique(child_vals)) {
        cv_idx  <- idx_rows[child_vals == cv]
        cv_rows <- exact_child_rows %>% dplyr::filter(num_children == cv)
        lookup_subset(cv_idx, cv_rows)
      }
      
      # fallback rows where num_children is NA
      still_unmatched <- idx_rows[is.na(result[idx_rows])]
      fallback_rows   <- fs_rows %>% dplyr::filter(is.na(num_children))
      lookup_subset(still_unmatched, fallback_rows)
    } else {
      # old behavior
      lookup_subset(idx_rows, fs_rows)
    }
  }
  
  result
}



#' Calculate Tax from Progressive Brackets
#'
#' Calculates income tax using progressive tax brackets. For each bracket,
#' calculates the tax on the portion of income that falls within that bracket,
#' then sums across all applicable brackets.
#'
#' @param df Dataframe with taxable income and filing status
#' @param brackets_df Dataframe with tax brackets (filing_status, lower_limit, upper_limit, rate)
#' @param taxable_income_var Column name for taxable income (default: "taxable_income")
#' @param filing_status_var Column name for filing status (default: "filing_status")
#' @param output_col Column name for output tax (default: "income_tax")
#' @param local_income_tax_var Optional column name for local tax rate to add to bracket rates
#' @return Dataframe with calculated tax column added
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
    left_join(brackets_df, by = "filing_status", relationship = "many-to-many") %>%
    filter(taxable_income > lower_limit) %>%
    mutate(
      taxable_at_bracket = pmin(taxable_income, upper_limit) - lower_limit,
      effective_rate = rate + local_income_tax_rate,
      tax_at_bracket = taxable_at_bracket * effective_rate
    ) %>%
    group_by(row_id) %>%
    summarize(!!output_col := sum(tax_at_bracket, na.rm = TRUE), .groups = "drop")

  df_original %>%
    left_join(df_calc, by = "row_id") %>%
    select(-row_id)
}

#' Apply a Calculation Method to a Value Vector
#'
#' Transforms a vector of raw parameter values (typically joined from a tax
#' parameter CSV) into per-row computed amounts by applying one of the
#' supported `calculation_method` strings. This is the shared dispatch function
#' used by both state taxable-income adjustment logic and state credit logic --
#' any new method needed by a state's CSV data should be added here.
#'
#' Supported methods: `fixed` / `flat` / `flag` (return value as-is),
#' `per_person`, `per_adult`, `per_child`, `per_child_minus1`,
#' `per_child_under6_double`, `per_child_under6`, `per_child_6plus`,
#' `percent_of_fed_tax`, `percent_of_fed_eitc`, `percent_of_fed_cdctc`.
#' Any unrecognized method is an error when `strict = TRUE` (the default), or
#' a warning that returns 0 when `strict = FALSE`.
#'
#' @param value_vector Numeric vector of raw parameter values (e.g., joined `value` column)
#' @param method Single string matching a supported calculation_method
#' @param calculations_df Dataframe row-aligned with `value_vector`, supplying context
#'   columns (household_size, adult, children, children_under6, children_6plus,
#'   final_federal_income_tax, eitc_credit, cdctc_credit)
#' @param var_name Variable name used in the error/warning message when the method is unrecognized
#' @param strict If TRUE (default), an unrecognized `method` triggers `stop()`. If FALSE,
#'   it triggers a `warning()` and returns 0.
#' @return Numeric vector the same length as `value_vector`
apply_calculation_method <- function(value_vector, method, calculations_df, var_name = "unknown", strict = TRUE) {
  v <- dplyr::coalesce(value_vector, 0)
  if (method %in% c("fixed", "flat", "flag")) {
    v
  } else if (method == "per_person") {
    v * calculations_df$household_size
  } else if (method == "per_adult") {
    v * calculations_df$adult
  } else if (method == "per_child") {
    v * calculations_df$children
  } else if (method == "per_child_minus1") {
    v * pmax(calculations_df$children - 1, 0)
  } else if (method == "per_child_under6_double") {
    num_under6 <- if ("children_under6" %in% names(calculations_df)) calculations_df$children_under6 else 0
    num_other  <- pmax(calculations_df$children - num_under6, 0)
    v * (2 * num_under6 + num_other)
  } else if (method == "per_child_under6") {
    v * calculations_df$children_under6
  } else if (method == "per_child_6plus") {
    v * calculations_df$children_6plus
  } else if (method == "percent_of_fed_tax") {
    v * dplyr::coalesce(calculations_df$final_federal_income_tax, 0)
  } else if (method == "percent_of_fed_eitc") {
    v * calculations_df$eitc_credit
  } else if (method == "percent_of_fed_cdctc") {
    v * calculations_df$cdctc_credit
  } else if (method == "percent_of_fed_cdctc_estimate") {
    v * calculations_df$cdctc_estimate
  } else if (method == "bracket") {
    v
  } else if (strict) {
    stop(glue::glue("Unknown calculation_method '{method}' for '{var_name}'."))
  } else {
    warning(glue::glue("Unknown calculation_method '{method}' for '{var_name}' -- returning 0."))
    rep(0, length(value_vector))
  }
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
    filter(sss_year == !!year)

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
           (calculations_df$ss_tax + calculations_df$medicare_tax) * 2,
           calculations_df$ss_tax + calculations_df$medicare_tax)

  return(calculations_df)
}

# ============================================================================
# EITC FUNCTIONS
# ============================================================================

#' Build EITC Lookup Table
#'
#' Creates a lookup table with EITC parameters for all combinations of
#' number of children (0-3) and household types
#'
#' @param eitc_params Dataframe with EITC parameters from tax_fed_credits_df
#' @return Dataframe with EITC parameters by eitc_children and household_type
build_eitc_lookup <- function(eitc_params) {
  expand.grid(
    eitc_children = 0:3,
    household_type = c("single_parent", "single_adult", "married"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(filing_status = ifelse(household_type == "married", "married", "single")) %>%
    left_join(eitc_params %>% filter(variable == "max_credit") %>% select(num_children, value),
              by = c("eitc_children" = "num_children")) %>% rename(eitc_max = value) %>%
    left_join(eitc_params %>% filter(variable == "phase_in_rate") %>% select(num_children, value),
              by = c("eitc_children" = "num_children")) %>% rename(eitc_phase_in_rate = value) %>%
    left_join(eitc_params %>% filter(variable == "phase_out_rate") %>% select(num_children, value),
              by = c("eitc_children" = "num_children")) %>% rename(eitc_phase_out_rate = value) %>%
    left_join(eitc_params %>% filter(variable == "income_at_max") %>% select(num_children, value),
              by = c("eitc_children" = "num_children")) %>% rename(eitc_income_at_max = value) %>%
    left_join(eitc_params %>% filter(variable == "phase_out_start") %>% select(num_children, filing_status, value),
              by = c("eitc_children" = "num_children", "filing_status")) %>% rename(eitc_phase_out_start = value) %>%
    left_join(eitc_params %>% filter(variable == "phase_out_end") %>% select(num_children, filing_status, value),
              by = c("eitc_children" = "num_children", "filing_status")) %>% rename(eitc_phase_out_end = value)
}

#' Calculate EITC Credit
#'
#' Calculates Earned Income Tax Credit based on income and pre-joined EITC parameters.
#' Assumes EITC lookup columns already exist in df.
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
    lowest_bracket       = cdctc_params$value[cdctc_params$variable == "lowest_bracket"],
    bracket_interval     = cdctc_params$value[cdctc_params$variable == "bracket_interval"],
    top_bracket_amount   = cdctc_params$value[cdctc_params$variable == "top_bracket_amount"],
    rate_interval        = cdctc_params$value[cdctc_params$variable == "rate_interval"],
    lowest_rate          = cdctc_params$value[cdctc_params$variable == "lowest_rate"],
    highest_rate         = cdctc_params$value[cdctc_params$variable == "highest_rate"],
    max_credit_one_child = cdctc_params %>% filter(num_children == 1, variable == "max_credit") %>% pull(value),
    max_credit_two_children = cdctc_params %>% filter(num_children == 2, variable == "max_credit") %>% pull(value)
  )
}

#' Calculate CDCTC Credit
#'
#' Calculates Child and Dependent Care Tax Credit based on childcare costs and income.
#' Credit is non-refundable and limited by federal tax liability.
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
    percentage            = ctc_params %>% filter(variable == "percentage") %>% pull(value),
    min_earning_threshold = ctc_params %>% filter(variable == "min_earning_threshold") %>% pull(value),
    max_credit            = ctc_params %>% filter(variable == "max_per_child") %>% pull(value)
  )
}

#' Calculate CTC Credit
#'
#' Calculates Child Tax Credit with both refundable and non-refundable portions.
#' Refundable portion differs for families with 1-2 children vs 3+ children.
#'
#' @param df Dataframe with starting_income, children, federal_cumulative_tax, cdctc_credit, total_fed_payroll_tax, eitc_credit
#' @param ctc_params_list Named list of CTC parameters from extract_ctc_params()
#' @return Dataframe with CTC calculation columns added
calculate_ctc_credit <- function(df, ctc_params_list) {
  df %>%
    mutate(
      ctc_credit_base           = children * ctc_params_list$max_credit,
      federal_tax_after_cdctc   = pmax(federal_cumulative_tax - cdctc_credit, 0),
      ctc_nonrefundable         = pmin(ctc_credit_base, federal_tax_after_cdctc),
      ctc_income_based_refund   = pmax(0, ctc_params_list$percentage * (starting_income - ctc_params_list$min_earning_threshold)),
      ctc_refund_1to2_children  = ifelse(children <= 2, pmin(ctc_credit_base - ctc_nonrefundable, ctc_income_based_refund), 0),
      ctc_payroll_based_refund  = pmax(0, total_fed_payroll_tax - eitc_credit),
      ctc_refund_3plus_children = ifelse(children >= 3, pmin(ctc_credit_base - ctc_nonrefundable, ctc_payroll_based_refund), 0),
      ctc_refundable            = ctc_refund_1to2_children + ctc_refund_3plus_children,
      ctc_credit                = ctc_nonrefundable + ctc_refundable
    )
}

# ============================================================================
# FEDERAL INCOME TAX FUNCTIONS
# ============================================================================

#' Calculate Federal Income Tax Deductions and Taxable Income
#'
#' @param df Dataframe with starting_income, household_type, and health-insurance
#'   premium columns.
#' @param federal_standard_deduction Dataframe with standard deductions by filing status
#' @param use_marketplace_premium If TRUE, use `health_ins_market` when present,
#'   falling back to `health_ins_premium` when the marketplace value is missing.
#' @return Dataframe with deduction and taxable income columns added
calculate_federal_income_tax <- function(df, federal_standard_deduction, use_marketplace_premium = FALSE) {

    employer_premium <- if ("health_ins_premium" %in% names(df)) {
        df$health_ins_premium
      } else {
          rep(NA_real_, nrow(df))
        }
  
    marketplace_premium <- if ("health_ins_market" %in% names(df)) {
          df$health_ins_market
        } else {
            rep(NA_real_, nrow(df))
          }
    
    selected_health_premium <- if (use_marketplace_premium) {
            dplyr::coalesce(marketplace_premium, employer_premium)
          } else {
              employer_premium
            }
      
  
  df %>%
    mutate(
      fed_sd = case_when(
        household_type == "married"       ~ federal_standard_deduction$married,
        household_type == "single_parent" ~ federal_standard_deduction$single_parent,
        household_type == "single_adult"  ~ federal_standard_deduction$single_adult
      ),
      health_insurance_premium_used = selected_health_premium,
      esi_premium_deduction = health_insurance_premium_used * 12, # employer-sponsored insurance premiums are annualized
      total_fed_deductions  = fed_sd + esi_premium_deduction,
      taxable_income        = pmax(starting_income - total_fed_deductions, 0),
      filing_status         = household_type
    )
}

#' Calculate Final Federal Income Tax
#'
#' Applies credits in statutory order to calculate final federal income tax liability:
#' (1) CDCTC (non-refundable) against gross tax, (2) non-refundable CTC against remaining
#' tax, (3) refundable credits (CTC refundable + EITC) against remaining liability.
#'
#' @param df Dataframe with federal_cumulative_tax, cdctc_credit, ctc_nonrefundable,
#'   ctc_refundable, eitc_credit
#' @return Dataframe with final federal income tax calculation columns added:
#'   fed_cdctc_applied, federal_tax_after_cdctc, fed_ctc_nonrefundable_applied,
#'   federal_tax_after_nonrefundable, federal_total_refundable_credits,
#'   federal_tax_liability_with_refund, final_federal_income_tax
calculate_final_federal_income_tax <- function(df) {
  df %>%
    mutate(
      fed_cdctc_applied                 = pmin(cdctc_credit, federal_cumulative_tax),
      federal_tax_after_cdctc           = pmax(federal_cumulative_tax - fed_cdctc_applied, 0),
      fed_ctc_nonrefundable_applied     = pmin(ctc_nonrefundable, federal_tax_after_cdctc),
      federal_tax_after_nonrefundable   = pmax(federal_tax_after_cdctc - fed_ctc_nonrefundable_applied, 0),
      federal_total_refundable_credits  = ctc_refundable + eitc_credit,
      federal_tax_liability_with_refund = federal_tax_after_nonrefundable - federal_total_refundable_credits,
      final_federal_income_tax          = pmax(federal_tax_liability_with_refund, 0)
    )
}
