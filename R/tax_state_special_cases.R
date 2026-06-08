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

#' Apply California EITC (State Special Case)
#'
#' Looks up California's EITC credit amount from the state EITC lookup table,
#' which provides credit values by income bracket and number of qualifying
#' children (0–3+). Uses a fuzzy join on `starting_income` against the lookup's
#' income ranges, then selects the column matching `ca_eitc_children`.
#'
#' Must be called before [apply_CA_yctc()], which depends on `credit_ca_eitc`.
#'
#' @param calculations_df Dataframe with starting_income and children
#' @param tax_state_eitc_lookup_df CA EITC lookup table with income_min, income_max,
#'   and value_0 through value_3 columns
#' @return Dataframe with `credit_ca_eitc` column added
apply_CA_eitc <- function(calculations_df, tax_state_eitc_lookup_df) {

  calculations_df <- calculations_df %>%
    dplyr::mutate(
      ca_eitc_children      = pmin(children, 3),
      ca_eitc_lookup_income = starting_income
    )

  ca_eitc_joined <- calculations_df %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    fuzzyjoin::fuzzy_left_join(
      tax_state_eitc_lookup_df,
      by = c("ca_eitc_lookup_income" = "income_min",
             "ca_eitc_lookup_income" = "income_max"),
      match_fun = list(`>=`, `<=`)
    ) %>%
    dplyr::group_by(row_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      credit_ca_eitc = dplyr::case_when(
        ca_eitc_children == 0  ~ value_0,
        ca_eitc_children == 1  ~ value_1,
        ca_eitc_children == 2  ~ value_2,
        ca_eitc_children >= 3  ~ value_3,
        TRUE                   ~ 0
      ),
      credit_ca_eitc = dplyr::coalesce(credit_ca_eitc, 0)
    ) %>%
    dplyr::select(row_id, credit_ca_eitc)

  calculations_df %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::left_join(ca_eitc_joined, by = "row_id") %>%
    dplyr::select(-row_id) %>%
    dplyr::mutate(credit_ca_eitc = dplyr::coalesce(credit_ca_eitc, 0))
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
