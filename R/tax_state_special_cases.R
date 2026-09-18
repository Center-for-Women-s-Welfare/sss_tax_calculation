# R/tax_state_special_cases.R
# State-specific custom rules: special deduction formulas and credit helpers

# ---------- HELPERS --------------------------------

#' Validate grouped special-case parameter rows
#'
#' For a given `variable_name`, checks that required `calculation_method` rows
#' exist and that their `value` entries are non-missing, returning named values
#' when valid.
#'
#' @param rows_df Dataframe already filtered to one `variable_name`
#' @param variable_name Character scalar; used in warning messages
#' @param required_methods Character vector of required calculation_method names
#' @param fn_name Character scalar; calling function name for warnings
#' @return Named numeric vector of required method values, or `NULL` if invalid
.get_required_method_values <- function(rows_df, variable_name, required_methods, fn_name) {
  methods <- unique(stats::na.omit(rows_df$calculation_method))
  missing_methods <- setdiff(required_methods, methods)
  
  if (length(missing_methods) > 0) {
    warning(
      sprintf(
        "%s: variable_name '%s' is missing required calculation_method(s): %s. Returning unchanged calculations_df.",
        fn_name, variable_name, paste(missing_methods, collapse = ", ")
      ),
      call. = FALSE
    )
    return(NULL)
  }
  
  vals <- stats::setNames(
    lapply(required_methods, function(m) {
      rows_df %>%
        dplyr::filter(calculation_method == m) %>%
        dplyr::pull(value) %>%
        dplyr::first()
    }),
    required_methods
  )
  
  vals_num <- as.numeric(vals)
  names(vals_num) <- required_methods
  
  missing_vals <- names(vals_num)[is.na(vals_num)]
  if (length(missing_vals) > 0) {
    warning(
      sprintf(
        "%s: variable_name '%s' has NA value(s) for: %s. Returning unchanged calculations_df.",
        fn_name, variable_name, paste(missing_vals, collapse = ", ")
      ),
      call. = FALSE
    )
    return(NULL)
  }
  
  vals_num
}

# ---------- DEDUCTION SPECIAL CASES --------------------------------

#' Apply Renters Deduction (State Special Case)
#'
#' Computes a state-specific renters deduction when taxable-income adjustment
#' parameters include `variable_name == "renters_deduction"`.
#'
#' Supports grouped parameter rows distinguished by `calculation_method`:
#' - If `renters_formula_min` is present, applies:
#'   `pmin(12 * housing_cost, renters_max)`
#' - If both `renters_formula_cap` and `renters_formula_rate` are present,
#'   applies:
#'   `pmin(12 * housing_cost * renters_rate, renters_max)`
#'
#' For grouped-method schemas, `renters_max` is taken from:
#' - `renters_formula_min` (min formula), or
#' - `renters_formula_cap` (cap+rate formula)
#' and `renters_rate` is taken from `renters_formula_rate` for cap+rate.
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment loop.
#'
#' @param calculations_df Dataframe with `housing_cost`
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `renters_deduction` column added when applicable,
#'   otherwise unchanged
apply_renters_deduction <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"renters_deduction" %in% calculation_vars) return(calculations_df)
  
  renters_rows <- state_adjustments %>%
    dplyr::filter(variable_name == "renters_deduction")
  
  renters_methods <- unique(stats::na.omit(renters_rows$calculation_method))
  
  # Priority: if explicit min formula exists, use it.
  if ("renters_formula_min" %in% renters_methods) {
    vals <- .get_required_method_values(
      rows_df = renters_rows,
      variable_name = "renters_deduction",
      required_methods = c("renters_formula_min"),
      fn_name = "apply_renters_deduction"
    )
    if (is.null(vals)) return(calculations_df)
    
    renters_max <- vals[["renters_formula_min"]]
    
    return(
      calculations_df %>%
        dplyr::mutate(
          renters_deduction = pmin(12 * housing_cost, renters_max)
        )
    )
  }
  
  # Otherwise require cap + rate formula.
  vals <- .get_required_method_values(
    rows_df = renters_rows,
    variable_name = "renters_deduction",
    required_methods = c("renters_formula_cap", "renters_formula_rate"),
    fn_name = "apply_renters_deduction"
  )
  if (is.null(vals)) return(calculations_df)
  
  renters_max  <- vals[["renters_formula_cap"]]
  renters_rate <- vals[["renters_formula_rate"]]
  
  calculations_df %>%
    dplyr::mutate(
      renters_deduction = pmin(12 * housing_cost * renters_rate, renters_max)
    )
}

#' Apply Commuter Deduction (State Special Case)
#'
#' Computes a state-specific commuter expense deduction when taxable-income
#' adjustment parameters include `variable_name == "commuter_deduction"`.
#'
#' Parameters are grouped under `commuter_deduction` and distinguished by
#' `calculation_method`. This rule requires:
#' - `commuter_max`
#' - `commuter_exclusion`
#'
#' Formula:
#' `pmin(pmax(public_transit_cost - commuter_exclusion, 0), commuter_max)`
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment
#' loop.
#'
#' @param calculations_df Dataframe with `public_transit_cost`
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `commuter_deduction` column added when applicable,
#'   otherwise unchanged
apply_commuter_deduction <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"commuter_deduction" %in% calculation_vars) return(calculations_df)
  
  commuter_rows <- state_adjustments %>%
    dplyr::filter(variable_name == "commuter_deduction")
  
  vals <- .get_required_method_values(
    rows_df = commuter_rows,
    variable_name = "commuter_deduction",
    required_methods = c("commuter_max", "commuter_exclusion"),
    fn_name = "apply_commuter_deduction"
  )
  if (is.null(vals)) return(calculations_df)
  
  commuter_max <- vals[["commuter_max"]]
  commuter_exclusion <- vals[["commuter_exclusion"]]
  
  calculations_df %>%
    dplyr::mutate(
      commuter_deduction = pmin(
        pmax(public_transit_cost - commuter_exclusion, 0),
        commuter_max
      )
    )
}

#' Apply Low- and Middle-Income Tax Exemption (State Special Case)
#'
#' Computes a state-specific (e.g., TN) low- and middle-income deduction when the
#' state's TI-adjustment parameters define a `low_middle_income_exemption` variable using
#' the `low_middle_income_formula` method: `if_else(taxable_income <= lmi_agi_limit,
#' (adult + children) * (lmi_base_exemption - (lmi_phaseout_rate * pmax(taxable_income - lmi_base_income,0))),
#' 0)`.
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment loop.
#'
#' @param calculations_df Dataframe with public_transit_cost
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `low_middle_income_exemption` column added when applicable,
#'   otherwise unchanged
apply_low_middle_income_exemption <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"low_middle_income_exemption" %in% calculation_vars) return(calculations_df)
  
  method <- state_adjustments %>%
    dplyr::filter(variable_name == "low_middle_income_exemption") %>%
    dplyr::pull(calculation_method) %>%
    unique()
  
  if (method == "low_middle_income_formula") {
    lmi_agi_limit <- state_adjustments %>%
      dplyr::filter(variable_name == "lmi_agi_limit") %>%
      dplyr::pull(value) %>%
      dplyr::first()
    
    lmi_base_income <- state_adjustments %>%
      dplyr::filter(variable_name == "lmi_base_income") %>%
      dplyr::pull(value) %>%
      dplyr::first()
    
    lmi_phaseout_rate <- state_adjustments %>%
      dplyr::filter(variable_name == "lmi_phaseout_rate") %>%
      dplyr::pull(value) %>%
      dplyr::first()
    
    lmi_base_exemption <- state_adjustments %>%
      dplyr::filter(variable_name == "lmi_base_exemption") %>%
      dplyr::pull(value) %>%
      dplyr::first()
    
    calculations_df <- calculations_df %>%
      dplyr::mutate(
        low_middle_income_exemption = if_else(taxable_income <= lmi_agi_limit,
                                              (adult + children) * (lmi_base_exemption - 
                                                (lmi_phaseout_rate * 
                                                   pmax(taxable_income - lmi_base_income,0))),
                                              0)
      )
  }
  
  calculations_df
}

#' Apply Property Tax Deduction (State Special Case)
#'
#' Computes a state-specific property tax deduction when taxable-income
#' adjustment parameters include `variable_name == "property_tax_deduction"`.
#'
#' Parameters are grouped under `property_tax_deduction` and distinguished by
#' `calculation_method`. This rule requires all of:
#' - `property_tax_deduction_income_floor`
#' - `property_tax_rate`
#' - `property_tax_deduction_cap`
#' - `property_tax_deduction_choice`
#'
#' Formula:
#' - If `taxable_income >= property_tax_deduction_income_floor` and
#'   `property_tax_deduction_choice == 1`, then
#'   `pmin(property_tax_rate * housing_cost * 12, property_tax_deduction_cap)`
#' - Otherwise `0`
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment loop.
#'
#' @param calculations_df Dataframe with `taxable_income` and `housing_cost`
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `property_tax_deduction` column added when applicable,
#'   otherwise unchanged
apply_property_tax_deduction <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"property_tax_deduction" %in% calculation_vars) return(calculations_df)
  
  prop_rows <- state_adjustments %>%
    dplyr::filter(variable_name == "property_tax_deduction")
  
  vals <- .get_required_method_values(
    rows_df = prop_rows,
    variable_name = "property_tax_deduction",
    required_methods = c(
      "property_tax_deduction_income_floor",
      "property_tax_rate",
      "property_tax_deduction_cap",
      "property_tax_deduction_choice"
    ),
    fn_name = "apply_property_tax_deduction"
  )
  if (is.null(vals)) return(calculations_df)
  
  property_tax_deduction_income_floor <- vals[["property_tax_deduction_income_floor"]]
  property_tax_rate                   <- vals[["property_tax_rate"]]
  property_tax_deduction_cap          <- vals[["property_tax_deduction_cap"]]
  property_tax_deduction_choice       <- vals[["property_tax_deduction_choice"]]
  
  calculations_df %>%
    dplyr::mutate(
      property_tax_deduction = dplyr::if_else(
        taxable_income >= property_tax_deduction_income_floor &
          property_tax_deduction_choice == 1,
        pmin(property_tax_rate * housing_cost * 12, property_tax_deduction_cap),
        0
      )
    )
}

# ---------- CREDIT SPECIAL CASES -----------------------------------

#' Build State EITC Lookup Table
#'
#' Pre-processes the state EITC lookup CSV (wide format with one value column
#' per child count) into a long-format table keyed by `(bracket_idx,
#' ca_eitc_children)`, plus an `income_min` breaks vector for
#' [findInterval()]. Called once before the solver loop so that
#' [apply_CA_eitc()] can do an O(n log m) bracket lookup each iteration
#' instead of a per-iteration fuzzyjoin.
#'
#' @param eitc_lookup_df State EITC lookup table already filtered to year/state,
#'   with `income_min`, `income_max`, and `value_0` through `value_3` columns
#' @return Named list with `table` (long-format lookup keyed by `bracket_idx`
#'   and `ca_eitc_children`, also carrying `eitc_income_max`) and `breaks`
#'   (sorted `income_min` vector for [findInterval()])
build_state_eitc_lookup <- function(eitc_lookup_df) {
  if (nrow(eitc_lookup_df) == 0L) {
    return(list(table = NULL, breaks = numeric(0)))
  }

  lookup_sorted <- eitc_lookup_df %>%
    dplyr::arrange(income_min) %>%
    dplyr::mutate(bracket_idx = dplyr::row_number())

  value_cols <- names(lookup_sorted)[startsWith(names(lookup_sorted), "value_")]

  lookup_long <- lookup_sorted %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(value_cols),
      names_to  = "ca_eitc_children",
      names_prefix = "value_",
      values_to = "credit_ca_eitc"
    ) %>%
    dplyr::mutate(ca_eitc_children = as.integer(ca_eitc_children)) %>%
    dplyr::select(bracket_idx, ca_eitc_children, credit_ca_eitc,
                  eitc_income_max = income_max)

  list(
    table  = lookup_long,
    breaks = lookup_sorted$income_min
  )
}

#' Apply California EITC (State Special Case)
#'
#' Looks up California's EITC credit amount using a pre-built lookup list from
#' `build_state_eitc_lookup()`. Uses [findInterval()] to assign each row to an
#' income bracket (O(n log m)) and then a simple left_join — replacing the
#' per-iteration fuzzyjoin that was the main solver bottleneck on large datasets.
#'
#' Must be called before [apply_CA_yctc()], which depends on `credit_ca_eitc`.
#'
#' @param calculations_df Dataframe with starting_income and children
#' @param state_eitc_lookup Pre-built lookup list from `build_state_eitc_lookup()`,
#'   with elements `table` (long-format keyed by `bracket_idx` and
#'   `ca_eitc_children`) and `breaks` (income_min vector for [findInterval()])
#' @return Dataframe with `credit_ca_eitc` column added
apply_CA_eitc <- function(calculations_df, state_eitc_lookup) {
  if (is.null(state_eitc_lookup$table)) {
    return(calculations_df %>% dplyr::mutate(credit_ca_eitc = 0))
  }

  calculations_df %>%
    dplyr::mutate(
      ca_eitc_children = pmin(children, 3L),
      bracket_idx      = findInterval(starting_income, state_eitc_lookup$breaks)
    ) %>%
    dplyr::left_join(
      state_eitc_lookup$table,
      by           = c("ca_eitc_children", "bracket_idx"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      credit_ca_eitc = dplyr::if_else(starting_income > eitc_income_max, 0, credit_ca_eitc),
      credit_ca_eitc = dplyr::coalesce(credit_ca_eitc, 0)
    ) %>%
    dplyr::select(-bracket_idx, -eitc_income_max)
}

#' Apply California Young Child Tax Credit (State Special Case)
#'
#' Computes California's Young Child Tax Credit (YCTC). The credit is zero unless
#' the family has at least one child under 6 and also qualifies for the CA EITC
#' (`credit_ca_eitc > 0`). Above a phase-out threshold it reduces by a fixed amount
#' per $100 of income, rounding down to zero.
#'
#' Must be called after [apply_CA_eitc()] since it depends on `credit_ca_eitc`.
#'
#' @param calculations_df Dataframe with children_under6, starting_income, and credit_ca_eitc
#' @param tax_state_credits_df State credit parameters containing `young_child_tax_credit`
#'   rows with `special_ca_yctc` (max value, phase-out start) and
#'   `special_ca_yctc_phaseout` (phase-out rate per $100) methods
#' @return Dataframe with `credit_young_child_tax_credit` column added
apply_CA_yctc <- function(calculations_df, tax_state_credits_df) {

  yctc_max <- tax_state_credits_df %>%
    dplyr::filter(variable_name == "young_child_tax_credit",
                  calculation_method == "special_ca_yctc") %>%
    dplyr::pull(value) %>%
    dplyr::first()

  yctc_phaseout_rate <- tax_state_credits_df %>%
    dplyr::filter(variable_name == "young_child_tax_credit",
                  calculation_method == "special_ca_yctc_phaseout") %>%
    dplyr::pull(value) %>%
    dplyr::first()

  yctc_phaseout_start <- tax_state_credits_df %>%
    dplyr::filter(variable_name == "young_child_tax_credit",
                  calculation_method == "special_ca_yctc") %>%
    dplyr::pull(income_max) %>%
    dplyr::first()

  calculations_df %>%
    dplyr::mutate(
      credit_young_child_tax_credit = dplyr::case_when(
        children_under6 <= 0                    ~ 0,
        credit_ca_eitc  <= 0                    ~ 0,
        starting_income <= yctc_phaseout_start  ~ yctc_max,
        TRUE ~ pmax(
          round(yctc_max - (floor((starting_income - yctc_phaseout_start) / 100) * yctc_phaseout_rate)),
          0
        )
      )
    )
}


#' Apply State EITC-Style Credit (e.g., WA Working Families Tax Credit)
#'
#' Computes a refundable, EITC-style credit for states (e.g., WA's Working
#' Families Tax Credit) that define their credit via flat per-family-type
#' parameters rather than income brackets. The credit is `max_credit` up to
#' `phase_out_start`, then phases out linearly at `phase_out_rate` per dollar
#' of income above that threshold, floored at `min_credit` through
#' `phase_out_end`; above `phase_out_end` the credit is $0.
#'
#' @param calculations_df Dataframe with household_type, children, and
#'   starting_income
#' @param state_eitc_params State EITC-style credit parameters already
#'   filtered to year/state, with filing_status, children, max_credit,
#'   phase_out_start, phase_out_end, phase_out_rate, and min_credit columns
#' @return Dataframe with `credit_wftc` column added
apply_state_eitc_style_credit <- function(calculations_df, state_eitc_params) {
  if (nrow(state_eitc_params) == 0L) {
    return(calculations_df %>% dplyr::mutate(credit_wftc = 0))
  }

  params <- state_eitc_params %>%
    dplyr::mutate(children = pmin(children, 3L)) %>%
    dplyr::select(filing_status, children, max_credit, phase_out_start,
                  phase_out_end, phase_out_rate, min_credit)

  calculations_df %>%
    dplyr::mutate(
      wftc_filing_status = dplyr::if_else(household_type == "married", "married", "single"),
      wftc_children      = pmin(children, 3L)
    ) %>%
    dplyr::left_join(
      params,
      by           = c("wftc_filing_status" = "filing_status", "wftc_children" = "children"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      credit_wftc = dplyr::case_when(
        is.na(max_credit)                  ~ 0,
        starting_income <= phase_out_start  ~ max_credit,
        starting_income >  phase_out_end    ~ 0,
        TRUE ~ pmax(
          max_credit - phase_out_rate * (starting_income - phase_out_start),
          min_credit
        )
      )
    ) %>%
    dplyr::select(-wftc_filing_status, -wftc_children, -max_credit,
                  -phase_out_start, -phase_out_end, -phase_out_rate, -min_credit)
}

#' Apply NY Pre-2026 Child and Dependent Care Tax Credit (Legacy)
#'
#' Computes New York's pre-2026 child and dependent care credit using a
#' federal-based structure. The calculation uses three parameter components
#' from `tax_state_credits_df` rows where
#' `calculation_method == "ny_pre2026_federal_brackets"`:
#'
#' 1. A federal-like rate lookup by income (`federal_rate_bracket` /
#'    `federal_bracket_rate`)
#' 2. A New York coefficient lookup by income (`ny_coefficient_bracket` /
#'    `ny_coefficient`)
#' 3. A child-count-specific cap on allowable childcare expenses
#'    (`child_expense_cap` / `expense_cap`)
#'
#' The function annualizes childcare costs (`child_care_cost * 12`), applies
#' the child-based cap, computes a federal-style estimate, then applies the
#' NY coefficient:
#'
#' `credit = min(annual_child_care_cost, child_cap) * fed_rate * ny_coefficient`
#'
#' This is legacy logic retained for historical tax years (through 2025) and is
#' not intended for future NY policy years.
#'
#' @param calculations_df Dataframe with `starting_income`,
#'   `state_filing_status`, `children`, and `child_care_cost`
#' @param cdcc_rows Subset of state credits parameters for
#'   `variable_name == "child_dependent_care"` (already filtered to year/state)
#' @param bracket_lookup_children Function used to match bracket values by
#'   income, filing status, and optional `num_children` with NA fallback
#' @return Numeric vector of NY pre-2026 CDCTC amounts (coalesced to 0)
apply_NY_pre2026_cdctc <- function(calculations_df, cdcc_rows, bracket_lookup_children) {
  rows <- cdcc_rows %>%
    dplyr::filter(calculation_method == "ny_pre2026_federal_brackets")
  
  # Expect multiple parameter rows distinguished by type.
  # 1) federal_rate_bracket: federal-like percentage by income
  fed_rate_rows <- rows %>%
    dplyr::filter(type %in% c("federal_rate_bracket", "federal_bracket_rate"))
  
  # 2) ny_coefficient_bracket: NY multiplier by income
  ny_coef_rows <- rows %>%
    dplyr::filter(type %in% c("ny_coefficient_bracket", "ny_coefficient"))
  
  # 3) child_expense_cap: max allowable expense by num_children
  cap_rows <- rows %>%
    dplyr::filter(type %in% c("child_expense_cap", "expense_cap"))
  
  fed_rate <- bracket_lookup_children(
    income = calculations_df$starting_income,
    filing_status = calculations_df$state_filing_status,
    bracket_df = fed_rate_rows,
    children = calculations_df$children
  )
  
  ny_coef <- bracket_lookup_children(
    income = calculations_df$starting_income,
    filing_status = calculations_df$state_filing_status,
    bracket_df = ny_coef_rows,
    children = calculations_df$children
  )
  
  # Cap lookup may be child-only; use neutral income for matching.
  cap_lookup_income <- rep(0, nrow(calculations_df))
  child_cap <- bracket_lookup_children(
    income = cap_lookup_income,
    filing_status = calculations_df$state_filing_status,
    bracket_df = cap_rows,
    children = calculations_df$children
  )
  
  eligible_expense <- pmin(
    dplyr::coalesce(calculations_df$child_care_cost, 0) * 12,
    dplyr::coalesce(child_cap, 0)
  )
  
  fed_estimate <- eligible_expense * dplyr::coalesce(fed_rate, 0)
  credit <- fed_estimate * dplyr::coalesce(ny_coef, 0)
  
  dplyr::coalesce(credit, 0)
}

#' Calculate State Child and Dependent Care Tax Credit (CDCTC)
#'
#' Computes state child and dependent care credits from `tax_state_credits_df`
#' rows for `variable_name == "child_dependent_care"`, using one or more
#' `calculation_method` values. This function handles common generic pathways
#' and delegates legacy NY pre-2026 logic to [apply_NY_pre2026_cdctc()].
#'
#' Supported methods in this function:
#' - `percent_of_fed_cdctc`: Apply a matched percentage to `cdctc_credit`
#' - `percent_of_fed_cdctc_estimate`: Compute a federal-style estimate from
#'   either bracketed or non-bracketed federal-rate parameters, then use that
#'   estimate as the state credit basis
#'
#' Matching behavior:
#' - If a method's rows have income brackets, match by filing status + income
#'   (with `filing_status == "all"` fallback), plus optional exact
#'   `num_children` match (`NA` fallback).
#' - If a method's rows have `income_min`/`income_max` all `NA`, match directly
#'   by filing status (and optional `num_children`) without bracket lookup.
#'
#' Assumptions:
#' - State AGI proxy is `starting_income`
#' - Federal CDCTC amount is already present in `cdctc_credit`
#' - `tax_state_credits_df` is pre-filtered to one year/state
#'
#' @param calculations_df Dataframe with `starting_income`, `household_type`,
#'   `state_filing_status` (optional), `children`, `child_care_cost`,
#'   and `cdctc_credit`
#' @param tax_state_credits_df State credit parameters already filtered to
#'   year/state
#' @return Dataframe with `state_cdctc_credit` column added
calculate_state_cdctc_credit <- function(calculations_df, tax_state_credits_df) {
  if (!"state_filing_status" %in% names(calculations_df)) {
    calculations_df <- calculations_df %>%
      dplyr::mutate(state_filing_status = household_type)
  }
  
  # Guard rails
  if (!"children" %in% names(calculations_df)) calculations_df$children <- 0
  if (!"child_care_cost" %in% names(calculations_df)) calculations_df$child_care_cost <- 0
  if (!"cdctc_credit" %in% names(calculations_df)) calculations_df$cdctc_credit <- 0
  
  cdcc_rows <- tax_state_credits_df %>%
    dplyr::filter(variable_name == "child_dependent_care") %>%
    dplyr::mutate(
      calculation_method = trimws(calculation_method),
      filing_status      = trimws(filing_status)
    )
  
  if (!"num_children" %in% names(cdcc_rows)) {
    cdcc_rows <- cdcc_rows %>% dplyr::mutate(num_children = NA_real_)
  } else {
    cdcc_rows <- cdcc_rows %>% dplyr::mutate(num_children = suppressWarnings(as.numeric(num_children)))
  }
  
  if (nrow(cdcc_rows) == 0) {
    calculations_df$state_cdctc_credit <- 0
    return(calculations_df)
  }
  
  methods <- unique(stats::na.omit(cdcc_rows$calculation_method))
  
  # ---------- helpers ----------
  bracket_lookup_children <- function(income, filing_status, bracket_df, children = NULL) {
    n <- length(income)
    out <- rep(NA_real_, n)
    
    if (!"num_children" %in% names(bracket_df)) bracket_df$num_children <- NA_real_
    bracket_df <- bracket_df %>%
      dplyr::mutate(
        filing_status = dplyr::if_else(is.na(filing_status) | filing_status == "", "all", filing_status),
        num_children  = suppressWarnings(as.numeric(num_children))
      )
    
    fs_levels <- c(setdiff(unique(bracket_df$filing_status), "all"), "all")
    
    for (fs in fs_levels) {
      fs_df <- if (fs == "all") bracket_df %>% dplyr::filter(filing_status == "all") else bracket_df %>% dplyr::filter(filing_status == fs)
      if (nrow(fs_df) == 0) next
      
      idx_rows <- if (fs == "all") which(is.na(out)) else which(filing_status == fs)
      if (length(idx_rows) == 0) next
      
      assign_from <- function(row_idx, tbl) {
        if (length(row_idx) == 0 || nrow(tbl) == 0) return(invisible(NULL))
        tbl <- tbl %>% dplyr::arrange(income_min)
        idx <- findInterval(income[row_idx], tbl$income_min)
        good <- idx >= 1 & idx <= nrow(tbl)
        pick <- pmax(pmin(idx, nrow(tbl)), 1L)
        in_range <- good & income[row_idx] <= tbl$income_max[pick]
        hit_rows <- row_idx[in_range]
        hit_vals <- as.numeric(tbl$value[pick[in_range]])
        can_fill <- is.na(out[hit_rows])
        out[hit_rows[can_fill]] <<- hit_vals[can_fill]
      }
      
      if (!is.null(children)) {
        exact_df <- fs_df %>% dplyr::filter(!is.na(num_children))
        for (cv in unique(children[idx_rows])) {
          cv_idx <- idx_rows[children[idx_rows] == cv]
          cv_df  <- exact_df %>% dplyr::filter(num_children == cv)
          assign_from(cv_idx, cv_df)
        }
        fallback_idx <- idx_rows[is.na(out[idx_rows])]
        fallback_df  <- fs_df %>% dplyr::filter(is.na(num_children))
        assign_from(fallback_idx, fallback_df)
      } else {
        assign_from(idx_rows, fs_df)
      }
    }
    
    out
  }
  
  nonbracket_lookup_children <- function(filing_status, rows_df, children = NULL) {
    n <- length(filing_status)
    out <- rep(NA_real_, n)
    
    if (!"num_children" %in% names(rows_df)) rows_df$num_children <- NA_real_
    rows_df <- rows_df %>%
      dplyr::mutate(
        filing_status = dplyr::if_else(is.na(filing_status) | filing_status == "", "all", filing_status),
        num_children  = suppressWarnings(as.numeric(num_children))
      )
    
    # exact filing status first, then "all" fallback
    fs_levels <- c(setdiff(unique(rows_df$filing_status), "all"), "all")
    
    for (fs in fs_levels) {
      fs_df <- if (fs == "all") rows_df %>% dplyr::filter(filing_status == "all") else rows_df %>% dplyr::filter(filing_status == fs)
      idx_rows <- if (fs == "all") which(is.na(out)) else which(filing_status == fs)
      if (nrow(fs_df) == 0 || length(idx_rows) == 0) next
      
      # child-specific first
      if (!is.null(children)) {
        exact_df <- fs_df %>% dplyr::filter(!is.na(num_children))
        for (cv in unique(children[idx_rows])) {
          cv_idx <- idx_rows[children[idx_rows] == cv]
          cv_row <- exact_df %>% dplyr::filter(num_children == cv)
          if (nrow(cv_row) > 0) out[cv_idx[is.na(out[cv_idx])]] <- as.numeric(cv_row$value[[1]])
        }
      }
      
      # NA-child fallback
      fb_row <- fs_df %>% dplyr::filter(is.na(num_children))
      if (nrow(fb_row) > 0) {
        fill_idx <- idx_rows[is.na(out[idx_rows])]
        out[fill_idx] <- as.numeric(fb_row$value[[1]])
      }
      
      # last resort: if only one row exists, use it
      if (nrow(fs_df) == 1) {
        fill_idx <- idx_rows[is.na(out[idx_rows])]
        out[fill_idx] <- as.numeric(fs_df$value[[1]])
      }
    }
    
    out
  }
  
  resolve_value_vector <- function(rows_df) {
    has_brackets <- !all(is.na(rows_df$income_min) & is.na(rows_df$income_max))
    if (has_brackets) {
      bracket_lookup_children(
        income        = calculations_df$starting_income,
        filing_status = calculations_df$state_filing_status,
        bracket_df    = rows_df,
        children      = calculations_df$children
      )
    } else {
      nonbracket_lookup_children(
        filing_status = calculations_df$state_filing_status,
        rows_df       = rows_df,
        children      = calculations_df$children
      )
    }
  }
  
  credit <- rep(0, nrow(calculations_df))
  
  # Common method: percent of federal CDCTC
  if ("percent_of_fed_cdctc" %in% methods) {
    rows <- cdcc_rows %>% dplyr::filter(calculation_method == "percent_of_fed_cdctc")
    pct  <- resolve_value_vector(rows)
    credit <- dplyr::coalesce(pct, 0) * dplyr::coalesce(calculations_df$cdctc_credit, 0)
  }
  
  # Common method: percent_of_fed_cdctc_estimate
  if ("percent_of_fed_cdctc_estimate" %in% methods) {
    rows <- cdcc_rows %>% dplyr::filter(calculation_method == "percent_of_fed_cdctc_estimate")
    pct  <- resolve_value_vector(rows)
    
    if (!"cdctc_credit_estimate" %in% names(calculations_df)) {
      warning("percent_of_fed_cdctc_estimate requested but cdctc_credit_estimate is missing; defaulting to 0.")
      base_estimate <- rep(0, nrow(calculations_df))
    } else {
      base_estimate <- dplyr::coalesce(calculations_df$cdctc_credit_estimate, 0)
    }
    
    credit <- dplyr::coalesce(pct, 0) * base_estimate
  }
  
  # Legacy NY path delegated to dedicated helper
  if ("ny_pre2026_federal_brackets" %in% methods) {
    credit <- apply_NY_pre2026_cdctc(
      calculations_df = calculations_df,
      cdcc_rows = cdcc_rows,
      bracket_lookup_children = bracket_lookup_children
    )
  }
  
  calculations_df$state_cdctc_credit <- dplyr::coalesce(credit, 0)
  calculations_df
}

#' Apply Special Property Tax Credit
#'
#' Computes a flat refundable property tax credit for a starting_income meeting
#' the set eligibility minimum.
#'
#' @param calculations_df Dataframe with household_type and
#'   starting_income
#' @param tax_state_credits_df State credit parameters containing `property_tax_credit`
#'   rows with `fixed` (credit value) methods
#' @return Dataframe with `property_tax_credit` column added
apply_property_tax_credit <- function(calculations_df, tax_state_credits_df) {
  if (nrow(state_eitc_params) == 0L) {
    return(calculations_df %>% dplyr::mutate(credit_wftc = 0))
  }
  
  params <- state_eitc_params %>%
    dplyr::mutate(children = pmin(children, 3L)) %>%
    dplyr::select(filing_status, children, max_credit, phase_out_start,
                  phase_out_end, phase_out_rate, min_credit)
  
  calculations_df %>%
    dplyr::mutate(
      wftc_filing_status = dplyr::if_else(household_type == "married", "married", "single"),
      wftc_children      = pmin(children, 3L)
    ) %>%
    dplyr::left_join(
      params,
      by           = c("wftc_filing_status" = "filing_status", "wftc_children" = "children"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      credit_wftc = dplyr::case_when(
        is.na(max_credit)                  ~ 0,
        starting_income <= phase_out_start  ~ max_credit,
        starting_income >  phase_out_end    ~ 0,
        TRUE ~ pmax(
          max_credit - phase_out_rate * (starting_income - phase_out_start),
          min_credit
        )
      )
    ) %>%
    dplyr::select(-wftc_filing_status, -wftc_children, -max_credit,
                  -phase_out_start, -phase_out_end, -phase_out_rate, -min_credit)
}



# ---------- CREDIT RESOLUTION HELPER --------------------------------

#' Resolve the Base Childcare Credit Column Name
#'
#' Identifies which computed credit column represents a state's base
#' child-and-dependent-care credit, trying a prioritized list of candidate
#' names before falling back to a regex match. Intended for use by
#' special-case rules (e.g., [apply_IA_credit_max_rule()]) that need to
#' reconcile a state CDCTC-style credit against another childcare credit.
#'
#' @param df Dataframe of computed credit columns
#' @param candidates Character vector of candidate column names, tried in order
#' @return The resolved column name as a string, or NULL if no match is found
.resolve_base_credit <- function(df, candidates = c("credit_state_cdctc", "credit_cdctc", "credit_cdcc")) {
  cols    <- names(df)
  hit     <- candidates[candidates %in% cols]
  if (length(hit) > 0) return(hit[[1]])
  rx_hits <- grep("^credit_.*(cdctc|cdcc)$", cols, ignore.case = TRUE, value = TRUE)
  if (length(rx_hits) > 0) return(rx_hits[[1]])
  NULL
}


# ---------- STATE-SPECIFIC OVERRIDES --------------------------------

#' Apply Iowa Combined Childcare Credit Cap (State Special Case)
#'
#' Iowa disallows double-claiming between its Child and Dependent Care Credit
#' (`credit_cdcc`) and Early Childhood Development Credit
#' (`credit_early_childhood`): the smaller of the two is subtracted back out
#' of `state_refundable_credits` to enforce the combined cap.
#'
#' @param calculations_df Dataframe with credit_cdcc, credit_early_childhood,
#'   and state_refundable_credits
#' @param state State postal code; the rule is a no-op for any state other
#'   than "IA"
#' @return Dataframe with state_refundable_credits adjusted when the IA rule
#'   applies, otherwise unchanged
apply_IA_credit_max_rule <- function(calculations_df, state) {
  if (state == "IA" &&
      all(c("credit_cdcc", "credit_early_childhood", "state_refundable_credits") %in%
          names(calculations_df))) {
    calculations_df <- calculations_df %>%
      dplyr::mutate(
        smaller_childcare_credit = pmin(credit_cdcc, credit_early_childhood, na.rm = TRUE),
        state_refundable_credits = state_refundable_credits - smaller_childcare_credit
      ) %>%
      dplyr::select(-smaller_childcare_credit)
  }
  calculations_df
}
