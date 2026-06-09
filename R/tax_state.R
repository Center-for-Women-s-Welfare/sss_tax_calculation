# R/tax_state.R
# State tax functions: payroll, taxable income, credits, and final tax

# ---------- STATE PAYROLL TAXES -----------------------------------

#' Get a State Payroll Tax Parameter
#'
#' Extracts a single numeric parameter value (e.g., rate, wage cap, flat tax)
#' for a given payroll program from the state payroll parameters dataframe.
#'
#' @param df Dataframe of state payroll parameters (program, variable, value)
#' @param program Payroll program name (e.g., "SUI", "WBF")
#' @param variable Parameter name to extract (e.g., "rate", "wage_cap", "flat_tax")
#' @return Numeric parameter value, or NA_real_ if not found
get_state_payroll_param <- function(df, program, variable) {
  value <- df %>%
    dplyr::filter(program == !!program, variable == !!variable) %>%
    dplyr::pull(value)
  if (length(value) == 0) return(NA_real_)
  as.numeric(value)
}

#' Get a Flat-Rate Program Tax Amount
#'
#' Looks up the flat-dollar tax amount for a payroll program that charges a
#' fixed amount rather than a rate-based tax (e.g., Workers' Benefit
#' Fund-style programs).
#'
#' @param df Dataframe of state payroll parameters (program, variable, value)
#' @param program Payroll program name to look up
#' @return Numeric flat tax amount, or 0 if the program has no flat_tax entry
get_flat_program_tax <- function(df, program) {
  value <- df %>%
    dplyr::filter(program == !!program, variable == "flat_tax") %>%
    dplyr::pull(value)
  if (length(value) == 0) return(0)
  as.numeric(value)
}

#' Calculate Payroll Tax for a Single Program
#'
#' Applies a flat rate (optionally capped by a wage limit) to an income vector
#' for a single state payroll tax program.
#'
#' @param income Numeric vector of income subject to the tax
#' @param rate Tax rate to apply (NA or 0 yields no tax)
#' @param cap Wage cap above which income is not taxed (NA or Inf means uncapped)
#' @return Numeric vector of tax amounts
calculate_program_tax <- function(income, rate, cap) {
  if (is.na(rate) || rate == 0) return(0)
  if (is.na(cap) || is.infinite(cap)) {
    income * rate
  } else {
    pmin(income, cap) * rate
  }
}

#' Calculate State Payroll Taxes
#'
#' Applies all state-level payroll tax programs (e.g., SUI, WBF) found in
#' `tax_state_payroll_df`, summing per-program taxes into a `state_payroll_tax`
#' total. Married households split income in half before applying program rates,
#' matching federal payroll tax treatment.
#'
#' @param calculations_df Dataframe with starting_income and household_type
#' @param tax_state_payroll_df Dataframe of state payroll parameters already
#'   filtered to the target year and state (program, variable, value columns)
#' @param year Tax year, used only in the diagnostic message when no programs are found
#' @param state State postal code, used only in the diagnostic message when no programs are found
#' @return Dataframe with one `payroll_tax_<program>` column per program plus a
#'   `state_payroll_tax` total column added
calculate_state_payroll_taxes <- function(calculations_df, tax_state_payroll_df, year, state) {

  state_params <- tax_state_payroll_df

  if (nrow(state_params) == 0) {
    message("No state payroll taxes for ", state, " in ", year)
    calculations_df$state_payroll_tax <- 0
    return(calculations_df)
  }

  programs <- unique(state_params$program)

  calculations_df <- calculations_df %>% dplyr::mutate(state_payroll_tax = 0)

  for (program in programs) {
    rate <- get_state_payroll_param(state_params, program, "rate")
    cap  <- get_state_payroll_param(state_params, program, "wage_cap")
    flat <- get_state_payroll_param(state_params, program, "flat_tax")

    tax_column <- paste0("payroll_tax_", gsub(" ", "_", tolower(program)))

    calculations_df <- calculations_df %>%
      dplyr::mutate(
        income_for_tax = dplyr::if_else(household_type == "married", starting_income / 2, starting_income),
        !!tax_column   := if (!is.na(flat) && flat > 0) flat else calculate_program_tax(income_for_tax, rate, cap),
        state_payroll_tax = state_payroll_tax + !!rlang::sym(tax_column)
      )
  }

  calculations_df <- calculations_df %>% dplyr::select(-income_for_tax)
  return(calculations_df)
}


# ---------- STATE TAXABLE INCOME -----------------------------------

#' Calculate State Taxable Income
#'
#' Derives `state_taxable_income` by subtracting all state-level
#' taxable-income adjustments (standard deduction, exemptions, and
#' special-case deductions such as renters/commuter deductions) from
#' `starting_income`.
#'
#' Adjustments are read from `tax_state_adjustments_all_df` (already filtered
#' to year/state). Each `variable_name` provides either a flat per-filing-status
#' value or an income-bracketed value, transformed via [apply_calculation_method()].
#' State-specific formulas (renters/commuter deductions, CDCTC subtraction) are
#' applied afterward via the special-case helpers in `tax_state_special_cases.R`.
#'
#' @param calculations_df Dataframe with starting_income, household_type,
#'   child_care_cost, health_ins_premium, and any other columns referenced
#'   by state adjustment methods
#' @param tax_state_adjustments_all_df Dataframe of state TI-adjustment
#'   parameters already filtered to the target year and state
#' @param state State postal code, used for diagnostic warnings and
#'   special-case dispatch
#' @param debug If TRUE, print a summary of deduction components and
#'   final taxable income
#' @return Dataframe with per-adjustment columns, `total_state_deductions`,
#'   and `state_taxable_income` added
calculate_state_taxable_income <- function(calculations_df,
                                           tax_state_adjustments_all_df,
                                           state,
                                           debug = FALSE) {
  if (!"state_filing_status" %in% names(calculations_df)) {
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_filing_status = household_type)
  }

  state_adjustments <- tax_state_adjustments_all_df %>%
    dplyr::filter(type == "taxable_income_subtraction")

  state_adjustments <- state_adjustments %>%
    dplyr::mutate(
      calculation_method = trimws(calculation_method),
      filing_status      = trimws(filing_status),
      variable_name      = trimws(variable_name)
    )

  calculation_vars <- unique(state_adjustments$variable_name)

  special_cases <- c("renters_deduction", "commuter_deduction", "commuter_threshold",
                     "renters_rate", "property_tax_deduction")
  general_vars  <- setdiff(calculation_vars, special_cases)

  for (var in general_vars) {
    rows   <- state_adjustments %>% dplyr::filter(variable_name == var)
    method <- unique(rows$calculation_method)

    if (length(method) != 1) {
      warning(glue::glue("Variable '{var}' has multiple or missing calculation_method entries."))
      next
    }

    if (all(is.na(rows$income_min))) {
      # Flat value: join on filing status then apply method
      value_df <- rows %>% dplyr::select(filing_status, value)

      calculations_df <- calculations_df %>%
        dplyr::left_join(value_df, by = c("state_filing_status" = "filing_status"))

      calculations_df[[var]] <- apply_calculation_method(
        value_vector    = calculations_df$value,
        method          = method,
        calculations_df = calculations_df,
        var_name        = var
      )

      calculations_df <- calculations_df %>% dplyr::select(-value)
    } else {
      # Bracketed value: fuzzy join on filing status and income range
      df_bracketed <- calculations_df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        fuzzyjoin::fuzzy_left_join(
          rows,
          by = c("state_filing_status" = "filing_status",
                 "starting_income"     = "income_min",
                 "starting_income"     = "income_max"),
          match_fun = list(`==`, `>=`, `<=`)
        ) %>%
        dplyr::group_by(row_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(!!var := value) %>%
        dplyr::select(row_id, !!var)

      calculations_df <- calculations_df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        dplyr::left_join(df_bracketed, by = "row_id") %>%
        dplyr::select(-row_id)
    }
  }

  # === Apply special-case formulas ===
  calculations_df <- apply_renters_deduction(calculations_df, state_adjustments, calculation_vars)
  calculations_df <- apply_commuter_deduction(calculations_df, state_adjustments, calculation_vars)

  # === Compute state_cdctc_subtraction (e.g., ID) ===
  if ("cdctc_subtraction_max" %in% names(calculations_df)) {
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_cdctc_subtraction = pmin(
        dplyr::coalesce(child_care_cost, 0) * 12,
        dplyr::coalesce(cdctc_subtraction_max, 0),
        starting_income
      ))
  } else {
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_cdctc_subtraction = 0)
  }

  # === Total deductions and taxable income ===
  # Build in a fixed order so deduction components are transparent for review
  total_vars <- c(
    "standard_deduction", "personal_exemption", "dependent_exemption",
    "state_health_ins_deductible", "cdctc_subtraction_max", "state_cdctc_subtraction",
    setdiff(calculation_vars, c("standard_deduction", "personal_exemption",
                                "dependent_exemption", "state_health_ins_deductible",
                                "cdctc_subtraction_max"))
  )

  for (col in total_vars) {
    if (!col %in% names(calculations_df)) {
      calculations_df <- calculations_df %>%
        dplyr::mutate(!!rlang::sym(col) := 0)
    } else {
      calculations_df <- calculations_df %>%
        dplyr::mutate(!!rlang::sym(col) := dplyr::coalesce(.data[[col]], 0))
    }
  }

  calculations_df <- calculations_df %>%
    dplyr::mutate(
      total_state_deductions = standard_deduction +
        personal_exemption +
        dependent_exemption +
        dplyr::if_else(state_health_ins_deductible == 1, esi_premium_deduction, 0) +
        state_cdctc_subtraction +
        rowSums(dplyr::across(dplyr::all_of(setdiff(calculation_vars, c(
          "standard_deduction", "personal_exemption", "dependent_exemption",
          "state_health_ins_deductible", "cdctc_subtraction_max"
        )))), na.rm = TRUE),
      state_taxable_income = pmax(starting_income - total_state_deductions, 0)
    )

  missing_vars <- setdiff(calculation_vars, names(calculations_df))
  if (length(missing_vars) > 0) {
    warning(glue::glue(
      "State '{state}' is missing the following TI variables in the final calculations_df: ",
      "{paste(missing_vars, collapse = ', ')}"
    ))
  }

  if (debug) {
    print(calculations_df %>%
      dplyr::select(starting_income, dplyr::all_of(total_vars),
                    total_state_deductions, state_taxable_income) %>%
      utils::head(10))
    print(glue::glue("State taxable income variables: {paste(total_vars, collapse = ', ')}"))
  }

  calculations_df
}


# ---------- STATE TAX CREDITS ---------------------------------------

#' Calculate State Tax Credits
#'
#' Applies state-level tax credits — both flat/per-unit credits and
#' income-bracketed credits from `tax_state_credits_df` and
#' `tax_state_variable_brackets_df` — accumulating them into
#' `state_nonrefundable_credits` and `state_refundable_credits`. Each credit's
#' value is resolved via [apply_calculation_method()].
#'
#' Also dispatches to special-case credit formulas (CA EITC, CA Young Child
#' Tax Credit, IA combined childcare credit cap) when the state's
#' `calculation_method` contains a `special_*` indicator.
#'
#' @param calculations_df Dataframe with starting_income, household_type,
#'   children, child_care_cost, eitc_credit, and related columns
#' @param tax_state_credits_df Dataframe of state credit parameters already
#'   filtered to the target year and state
#' @param tax_state_variable_brackets_df Dataframe of state variable-bracket
#'   parameters already filtered to the target year and state
#' @param tax_state_eitc_lookup_df Dataframe of state EITC lookup values
#'   already filtered to the target year and state
#' @param year Tax year, used only in the diagnostic message when no credits
#'   are found
#' @param state State postal code, used for diagnostic messaging and
#'   special-case dispatch (e.g., "IA", "CA")
#' @param debug If TRUE, print a summary of computed credit columns and totals
#' @return Dataframe with per-credit `credit_<name>` columns plus
#'   `state_nonrefundable_credits` and `state_refundable_credits` totals added
calculate_state_tax_credits <- function(calculations_df,
                                        tax_state_credits_df,
                                        tax_state_variable_brackets_df,
                                        tax_state_eitc_lookup_df,
                                        year,
                                        state,
                                        debug = FALSE) {
  if (!"state_filing_status" %in% names(calculations_df)) {
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_filing_status = household_type)
  }

  # Remove pre-existing credit columns/totals to avoid carryover across iterations
  calculations_df <- calculations_df %>%
    dplyr::select(
      -tidyselect::matches("^credit_"),
      -tidyselect::any_of(c("state_nonrefundable_credits", "state_refundable_credits"))
    )

  # 1) Build combined credit table: main sheet + variable_brackets (type == "credit")
  base_credits    <- tax_state_credits_df
  brackets_credit <- tax_state_variable_brackets_df %>%
    dplyr::filter(type == "credit")

  if (nrow(brackets_credit) > 0) {
    base_ref <- base_credits %>%
      dplyr::select(variable_name, refundable) %>%
      dplyr::distinct()

    brackets_credit <- brackets_credit %>%
      dplyr::left_join(base_ref, by = "variable_name")

    vars_with_brackets <- unique(brackets_credit$variable_name)

    base_credits <- base_credits %>%
      dplyr::filter(
        !(variable_name %in% vars_with_brackets & is.na(income_min) & is.na(income_max))
      )

    state_credits <- dplyr::bind_rows(base_credits, brackets_credit)
  } else {
    state_credits <- base_credits
  }

  if (nrow(state_credits) == 0L) {
    if (debug) message("No state credits found for ", state, " in ", year, ".")
    return(
      calculations_df %>%
        dplyr::mutate(
          state_nonrefundable_credits = 0,
          state_refundable_credits    = 0
        )
    )
  }

  state_credits <- state_credits %>%
    dplyr::mutate(
      refundable         = dplyr::coalesce(as.integer(trimws(as.character(refundable))), 0L),
      calculation_method = trimws(calculation_method),
      filing_status      = trimws(filing_status),
      variable_name      = trimws(variable_name)
    )

  # Parameter rows used only by special-case handlers; skip in generic loop
  special_param_vars <- c("early_childhood_pct", "early_childhood_max")

  credit_vars <- state_credits %>%
    dplyr::filter(!grepl("^special_", calculation_method)) %>%
    dplyr::pull(variable_name) %>%
    unique() %>%
    setdiff(c("dummy", special_param_vars))

  calculations_df <- calculations_df %>%
    dplyr::mutate(
      state_nonrefundable_credits = 0,
      state_refundable_credits    = 0
    )

  # Guard rails: ensure base columns are present and non-NA
  needed_bases <- c("household_size", "children", "child_care_cost",
                    "eitc_credit", "starting_income")
  for (col in needed_bases) {
    if (!col %in% names(calculations_df)) {
      calculations_df[[col]] <- 0
    } else {
      calculations_df[[col]] <- dplyr::coalesce(calculations_df[[col]], 0)
    }
  }

  # 2) General credit loop
  for (var in credit_vars) {
    rows   <- state_credits %>% dplyr::filter(variable_name == var)
    method <- unique(rows$calculation_method)

    if (length(method) != 1 || is.na(method)) {
      warning(glue::glue("Credit '{var}' has multiple or missing calculation_method entries."))
      next
    }
    method <- method[[1]]

    refundable_flag <- unique(rows$refundable)
    if (length(refundable_flag) != 1 || is.na(refundable_flag)) {
      warning(glue::glue(
        "Credit '{var}' has multiple or missing refundable flags; defaulting to nonrefundable (0)."
      ))
      refundable_flag <- 0L
    } else {
      refundable_flag <- as.integer(refundable_flag[[1]])
    }

    credit_col <- paste0("credit_", tolower(var))

    if (all(is.na(rows$income_min))) {
      # Case A: flat per-filing-status value
      value_df <- rows %>% dplyr::select(filing_status, value)

      tmp <- calculations_df %>%
        dplyr::left_join(value_df, by = c("state_filing_status" = "filing_status"))

      tmp[[credit_col]] <- apply_calculation_method(
        value_vector    = tmp$value,
        method          = method,
        calculations_df = calculations_df,
        var_name        = var
      )

      calculations_df[[credit_col]] <- dplyr::coalesce(tmp[[credit_col]], 0)

    } else {
      # Case B: income-bracketed value; try exact filing_status then fall back to "all"
      bracket_df <- rows %>%
        dplyr::mutate(
          filing_status = dplyr::if_else(
            is.na(filing_status) | filing_status == "", "all", filing_status
          )
        )

      df_exact <- calculations_df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        fuzzyjoin::fuzzy_left_join(
          bracket_df %>% dplyr::filter(filing_status != "all"),
          by = c("state_filing_status" = "filing_status",
                 "starting_income"     = "income_min",
                 "starting_income"     = "income_max"),
          match_fun = list(`==`, `>=`, `<=`)
        ) %>%
        dplyr::group_by(row_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(row_id, value)

      df_all <- calculations_df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        fuzzyjoin::fuzzy_left_join(
          bracket_df %>% dplyr::filter(filing_status == "all"),
          by = c("starting_income" = "income_min",
                 "starting_income" = "income_max"),
          match_fun = list(`>=`, `<=`)
        ) %>%
        dplyr::group_by(row_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(row_id, value_all = value)

      df_bracketed <- dplyr::full_join(df_exact, df_all, by = "row_id") %>%
        dplyr::mutate(value = dplyr::coalesce(value, value_all)) %>%
        dplyr::select(row_id, value)

      calc_joined <- calculations_df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        dplyr::left_join(df_bracketed, by = "row_id")

      calc_joined[[credit_col]] <- apply_calculation_method(
        value_vector    = calc_joined$value,
        method          = method,
        calculations_df = calculations_df,
        var_name        = var
      )

      calculations_df[[credit_col]] <- dplyr::coalesce(calc_joined[[credit_col]], 0)
    }

    if (refundable_flag == 1L) {
      calculations_df <- calculations_df %>%
        dplyr::mutate(state_refundable_credits = state_refundable_credits + .data[[credit_col]])
    } else {
      calculations_df <- calculations_df %>%
        dplyr::mutate(state_nonrefundable_credits = state_nonrefundable_credits + .data[[credit_col]])
    }
  }

  # Special-case credit formulas
  if ("special_ca_eitc" %in% state_credits$calculation_method) {
    calculations_df <- apply_CA_eitc(
      calculations_df          = calculations_df,
      tax_state_eitc_lookup_df = tax_state_eitc_lookup_df
    )
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_refundable_credits = state_refundable_credits + credit_ca_eitc)
  }

  if ("special_ca_yctc" %in% state_credits$calculation_method) {
    calculations_df <- apply_CA_yctc(
      calculations_df      = calculations_df,
      tax_state_credits_df = tax_state_credits_df
    )
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_refundable_credits = state_refundable_credits + credit_young_child_tax_credit)
  }

  # Special-case overrides (IA combined credit cap rule)
  calculations_df <- apply_IA_credit_max_rule(calculations_df, state)

  expected_cols <- paste0("credit_", tolower(credit_vars))
  missing <- setdiff(expected_cols, names(calculations_df))
  if (length(missing) > 0) {
    warning(glue::glue(
      "State '{state}' is missing the following credit columns in the final calculations_df: ",
      "{paste(missing, collapse = ', ')}"
    ))
  }

  if (debug) {
    keep <- c("starting_income", "state_nonrefundable_credits", "state_refundable_credits",
              grep("^credit_", names(calculations_df), value = TRUE))
    print(calculations_df %>% dplyr::select(dplyr::all_of(keep)) %>% utils::head(10))
  }

  calculations_df
}


# ---------- STATE INCOME TAX FINAL -----------------------------------

#' Calculate Final State Income Tax
#'
#' Applies non-refundable then refundable state credits to `state_cumulative_tax`
#' to derive final state tax liability, mirroring the credit-ordering logic of
#' [calculate_final_federal_income_tax()].
#'
#' @param calculations_df Dataframe with state_cumulative_tax,
#'   state_nonrefundable_credits, and state_refundable_credits columns
#' @return Dataframe with state_nonrefundable_credit_applied,
#'   state_tax_after_nonrefundable, state_tax_liability_with_refund, and
#'   final_state_income_tax columns added
calculate_final_state_income_tax <- function(calculations_df) {
  calculations_df %>%
    dplyr::mutate(
      state_nonrefundable_credit_applied = pmin(state_nonrefundable_credits, state_cumulative_tax),
      state_tax_after_nonrefundable      = pmax(state_cumulative_tax - state_nonrefundable_credit_applied, 0),
      state_tax_liability_with_refund    = state_tax_after_nonrefundable - state_refundable_credits,
      final_state_income_tax             = pmax(state_tax_liability_with_refund, 0)
    )
}
