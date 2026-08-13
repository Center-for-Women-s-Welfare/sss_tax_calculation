# R/tax_state_special_cases.R
# State-specific custom rules: special deduction formulas and credit helpers

# ---------- DEDUCTION SPECIAL CASES --------------------------------

#' Apply Renters Deduction (State Special Case)
#'
#' Computes a state-specific renters deduction when the state's TI-adjustment
#' parameters define a `renters_deduction` variable. Supports two formulas:
#' - `renters_formula_pct`: `min(12 * housing_cost * rate, max)` (used by IN, MA)
#' - `renters_formula_min`: `min(12 * housing_cost, max)`
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment loop.
#'
#' @param calculations_df Dataframe with housing_cost
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `renters_deduction` column added when applicable,
#'   otherwise unchanged
apply_renters_deduction <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"renters_deduction" %in% calculation_vars) return(calculations_df)

  renters_method <- state_adjustments %>%
    dplyr::filter(variable_name == "renters_deduction") %>%
    dplyr::pull(calculation_method) %>%
    unique()

  if (renters_method == "renters_formula_pct") {
    renters_max <- state_adjustments %>%
      dplyr::filter(variable_name == "renters_deduction") %>%
      dplyr::pull(value) %>%
      dplyr::first()

    renters_rate <- state_adjustments %>%
      dplyr::filter(variable_name == "renters_rate") %>%
      dplyr::pull(value) %>%
      dplyr::first()

    calculations_df <- calculations_df %>%
      dplyr::mutate(renters_deduction = pmin(12 * housing_cost * renters_rate, renters_max))

  } else if (renters_method == "renters_formula_min") {
    renters_max <- state_adjustments %>%
      dplyr::filter(variable_name == "renters_deduction") %>%
      dplyr::pull(value) %>%
      dplyr::first()

    calculations_df <- calculations_df %>%
      dplyr::mutate(renters_deduction = pmin(12 * housing_cost, renters_max))
  }

  calculations_df
}

#' Apply Commuter Deduction (State Special Case)
#'
#' Computes a state-specific commuter expense deduction (e.g., MA) when the
#' state's TI-adjustment parameters define a `commuter_deduction` variable using
#' the `commuter_formula` method: `min(max(public_transit_cost - threshold, 0), max)`.
#'
#' Called by [calculate_state_taxable_income()] after the general adjustment loop.
#'
#' @param calculations_df Dataframe with public_transit_cost
#' @param state_adjustments Dataframe of state TI-adjustment rows already
#'   filtered to taxable_income_subtraction type
#' @param calculation_vars Character vector of all variable_name values present
#'   in state_adjustments
#' @return Dataframe with a `commuter_deduction` column added when applicable,
#'   otherwise unchanged
apply_commuter_deduction <- function(calculations_df, state_adjustments, calculation_vars) {
  if (!"commuter_deduction" %in% calculation_vars) return(calculations_df)

  commuter_method <- state_adjustments %>%
    dplyr::filter(variable_name == "commuter_deduction") %>%
    dplyr::pull(calculation_method) %>%
    unique()

  if (commuter_method == "commuter_formula") {
    commuter_max <- state_adjustments %>%
      dplyr::filter(variable_name == "commuter_deduction") %>%
      dplyr::pull(value) %>%
      dplyr::first()

    commuter_threshold <- state_adjustments %>%
      dplyr::filter(variable_name == "commuter_threshold") %>%
      dplyr::pull(value) %>%
      dplyr::first()

    calculations_df <- calculations_df %>%
      dplyr::mutate(
        commuter_deduction = pmin(pmax(public_transit_cost - commuter_threshold, 0), commuter_max)
      )
  }

  calculations_df
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
#' Like the federal EITC, this is an earned-income credit -- gated to $0 when
#' `n_earning_adults == 0` (default: `n_adults`, i.e. all adults earning, if
#' the column is absent, matching prior behavior exactly).
#'
#' @param calculations_df Dataframe with starting_income, children, household_type,
#'   and (optionally) n_earning_adults
#' @param state_eitc_lookup Pre-built lookup list from `build_state_eitc_lookup()`,
#'   with elements `table` (long-format keyed by `bracket_idx` and
#'   `ca_eitc_children`) and `breaks` (income_min vector for [findInterval()])
#' @return Dataframe with `credit_ca_eitc` column added
apply_CA_eitc <- function(calculations_df, state_eitc_lookup) {
  if (is.null(state_eitc_lookup$table)) {
    return(calculations_df %>% dplyr::mutate(credit_ca_eitc = 0))
  }

  if (!"n_earning_adults" %in% names(calculations_df)) {
    calculations_df$n_earning_adults <- ifelse(calculations_df$household_type == "married", 2, 1)
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
      credit_ca_eitc = dplyr::coalesce(credit_ca_eitc, 0),
      credit_ca_eitc = dplyr::if_else(n_earning_adults == 0, 0, credit_ca_eitc)
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
#' Like the federal EITC it's modeled on, this is an earned-income credit --
#' gated to $0 when `n_earning_adults == 0` (default: `n_adults`, i.e. all
#' adults earning, if the column is absent, matching prior behavior exactly).
#'
#' @param calculations_df Dataframe with household_type, children,
#'   starting_income, and (optionally) n_earning_adults
#' @param state_eitc_params State EITC-style credit parameters already
#'   filtered to year/state, with filing_status, children, max_credit,
#'   phase_out_start, phase_out_end, phase_out_rate, and min_credit columns
#' @return Dataframe with `credit_wftc` column added
apply_state_eitc_style_credit <- function(calculations_df, state_eitc_params) {
  if (nrow(state_eitc_params) == 0L) {
    return(calculations_df %>% dplyr::mutate(credit_wftc = 0))
  }

  if (!"n_earning_adults" %in% names(calculations_df)) {
    calculations_df$n_earning_adults <- ifelse(calculations_df$household_type == "married", 2, 1)
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
        n_earning_adults == 0               ~ 0,
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
