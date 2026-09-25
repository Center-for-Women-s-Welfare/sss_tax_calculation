#' Iterative solver for SSS starting income calculation
#'
#' Given a basic needs dataframe, iteratively solves for the gross annual income
#' required to cover those needs after accounting for federal taxes and credits.
#'
#' @param df Dataframe with basic needs columns
#' (see validate_input for required columns)
#' @param year Tax year (e.g., 2026)
#' @param state State abbreviation (reserved for future state tax support)
#' @param health_insurance_scenario Backward-compatible default scenario used
#'   when a row does not provide `health_insurance_scenario`. Supported values:
#'   `"employer"`, `"marketplace"` (treated as unsubsidized marketplace by
#'   default), `"marketplace_unsubsidized"`, and `"marketplace_ptc"`.
#' @param max_iterations Maximum number of solver iterations (default: 100)
#' @param tolerance Convergence threshold in dollars (default: 1.0)
#' @param damping Blending weight applied to the previous estimate on each step (0 = no damping,
#'   1 = never moves). Default 0.5 blends equally, which stabilises oscillation near credit cliffs.
#' @param debug If TRUE, print iteration progress and diagnostics
#' @return Input dataframe with starting_income and tax breakdown columns added
#' @export
solve_starting_income_iterative <- function(df,
                                            year,
                                            state = NULL,
                                            health_insurance_scenario = "employer",
                                            max_iterations = 100,
                                            tolerance = 1.0,
                                            damping = 0.5,
                                            debug = FALSE) {

  # Needed if state = NULL, since local tax calculations still require a tax_rate_local column.
  if (!"tax_rate_local" %in% names(df)) {
    df$tax_rate_local <- 0
  }

  validate_input(df, year, state)

  valid_health_insurance_scenarios <- c(
    "employer",
    "marketplace",
    "marketplace_unsubsidized",
    "marketplace_ptc"
  )

  if (
    length(health_insurance_scenario) != 1L ||
    is.na(health_insurance_scenario) ||
    !health_insurance_scenario %in% valid_health_insurance_scenarios
  ) {
    stop(
      "health_insurance_scenario must be one of: ",
      paste(valid_health_insurance_scenarios, collapse = ", "),
      "."
    )
  }

  if (!"health_insurance_scenario" %in% names(df)) {
    df$health_insurance_scenario <- health_insurance_scenario
    df$health_insurance_scenario_from_input <- FALSE
  } else {
    row_scenarios <- trimws(as.character(df$health_insurance_scenario))
    row_scenarios[row_scenarios == ""] <- NA_character_
    df$health_insurance_scenario_from_input <- !is.na(row_scenarios)
    invalid_row_scenarios <- sort(unique(
      row_scenarios[!is.na(row_scenarios) & !row_scenarios %in% valid_health_insurance_scenarios]
    ))
    if (length(invalid_row_scenarios) > 0) {
      stop(
        "Invalid values found in health_insurance_scenario: ",
        paste(invalid_row_scenarios, collapse = ", "),
        ". Valid values are: ",
        paste(valid_health_insurance_scenarios, collapse = ", "),
        "."
      )
    }
    df$health_insurance_scenario <- dplyr::coalesce(row_scenarios, health_insurance_scenario)
  }

  # Upstream `marketplace` does not distinguish subsidized vs unsubsidized rows.
  # Use unsubsidized behavior by default and apply PTC only when explicitly tagged
  # as `marketplace_ptc`.
  df$apply_premium_tax_credit <- df$health_insurance_scenario == "marketplace_ptc"

  premium_used_col <- if ("health_insurance_premium_used" %in% names(df)) {
    !is.na(df$health_insurance_premium_used)
  } else {
    rep(FALSE, nrow(df))
  }
  employer_premium_col <- if ("health_ins_premium" %in% names(df)) {
    !is.na(df$health_ins_premium)
  } else {
    rep(FALSE, nrow(df))
  }
  marketplace_premium_col <- if ("health_ins_market" %in% names(df)) {
    !is.na(df$health_ins_market)
  } else {
    rep(FALSE, nrow(df))
  }

  marketplace_scenarios <- c("marketplace", "marketplace_unsubsidized", "marketplace_ptc")
  marketplace_rows <- df$health_insurance_scenario %in% marketplace_scenarios
  employer_rows <- df$health_insurance_scenario == "employer"

  invalid_marketplace_rows <- marketplace_rows & !(premium_used_col | marketplace_premium_col)
  if (any(invalid_marketplace_rows)) {
    stop(
      "Marketplace scenario rows require health_insurance_premium_used or health_ins_market. ",
      "Invalid rows: ", sum(invalid_marketplace_rows), "."
    )
  }

  invalid_employer_rows <- employer_rows & !(premium_used_col | employer_premium_col)
  if (any(invalid_employer_rows)) {
    stop(
      "Employer scenario rows require health_insurance_premium_used or health_ins_premium. ",
      "Invalid rows: ", sum(invalid_employer_rows), "."
    )
  }

  if (!is.null(state)) {
    # Load all state-specific parameter tables once, outside the iteration loop.
    # This avoids repeated disk reads and keeps each iteration focused on recomputation
    # of row-level tax outputs given the current starting_income guess.
    state_params      <- load_state_tax_params(year, state)
    # Pre-build EITC lookup object used by special state credit handlers.
    state_eitc_lookup <- build_state_eitc_lookup(state_params$state_eitc_lookup)
  }

  federal_params <- load_federal_tax_params(year)

  credit_params <- federal_params$fed_credits %>%
    dplyr::filter(sss_year == !!year)

  eitc_params  <- credit_params %>% dplyr::filter(credit == "eitc")
  cdctc_params <- credit_params %>% dplyr::filter(credit == "cdctc")
  ctc_params   <- credit_params %>% dplyr::filter(credit == "ctc")

  federal_standard_deduction <- federal_params$fed_sd %>%
    dplyr::filter(sss_year == !!year) %>%
    dplyr::select(-"sss_year") %>%
    tidyr::pivot_wider(names_from = filing_status, values_from = deduction)

  federal_tax_brackets <- federal_params$fed_brackets %>%
    dplyr::filter(sss_year == !!year) %>%
    dplyr::select(-"sss_year")

  federal_payroll <- federal_params$fed_payroll
  fed_premium_tax_credit <- federal_params$fed_premium_tax_credit
  fed_poverty_line <- federal_params$fed_poverty_line

  eitc_lookup_df    <- build_eitc_lookup(eitc_params)
  cdctc_params_list <- extract_cdctc_params(cdctc_params)
  ctc_params_list   <- extract_ctc_params(ctc_params)

  df$starting_income  <- df$subtotal3 * 1.20 * 12
  df$iteration_count  <- 0
  df$converged        <- FALSE

  df <- df %>%
    dplyr::mutate(eitc_children = pmin(children, 3)) %>%
    dplyr::left_join(eitc_lookup_df, by = c("eitc_children", "household_type"), relationship = "many-to-one")

  if (debug) {
    cat("\n=== Starting Iterative Solver ===\n")
    cat("Initial starting_income range:",
        sprintf("$%.2f - $%.2f", min(df$starting_income), max(df$starting_income)), "\n")
    cat("Max iterations:", max_iterations, "\n")
    cat("Tolerance: $", tolerance, "\n\n")
  }

  for (iter in 1:max_iterations) {

    # Keep previous estimate for convergence checks and damping blend.
    df$previous_income <- df$starting_income

    df <- df %>%
      # Remove derived columns from the prior iteration so all taxes/credits
      # are recomputed from scratch from the current starting_income estimate.
      # This prevents stale values from leaking across iterations.
      dplyr::select(-any_of(c(
        "ss_income", "medicare_threshold", "ss_tax", "medicare_tax", "total_fed_payroll_tax",
        "fed_sd", "esi_premium_deduction",
        "total_fed_deductions", "taxable_income", "federal_cumulative_tax",
        "eitc_credit",
        "cdctc_max", "cdctc_eligible_expense", "cdctc_rate", "cdctc_estimate", "cdctc_credit",
        "ctc_credit_base", "federal_tax_after_cdctc", "ctc_nonrefundable",
        "ctc_income_based_refund", "ctc_refund_1to2_children", "ctc_payroll_based_refund",
        "ctc_refund_3plus_children", "ctc_refundable", "ctc_credit",
        "fed_cdctc_applied", "federal_tax_after_nonrefundable", "fed_ctc_nonrefundable_applied",
        "federal_total_refundable_credits", "federal_tax_liability_with_refund", "final_federal_income_tax",
        "fpl", "pct_fpl", "required_income_rate", "premium_tax_credit",
        "monthly_premium_selected", "annual_selected_premium", "annual_required_contribution",
        "premium_tax_credit_raw",
        "state_nonrefundable_credit_applied", "state_tax_after_nonrefundable",
        "state_tax_liability_with_refund", "final_state_income_tax",
        "federal_net", "state_net","local_income_tax",
        "total_taxes", "total_credits", "new_starting_income", "income_diff", "row_converged",
        "state_payroll_tax", "total_state_deductions", "state_taxable_income", "state_cumulative_tax"
      ))) %>%
      dplyr::select(-dplyr::matches("^payroll_tax_"))

    df <- calculate_federal_payroll_taxes(df, federal_payroll, year)
    df <- calculate_federal_income_tax(
      df,
      federal_standard_deduction,
      default_health_insurance_scenario = health_insurance_scenario
    )
    if (any(df$apply_premium_tax_credit)) {
      df$fpl <- NA_real_
      df$pct_fpl <- NA_real_
      df$required_income_rate <- NA_real_
      df$monthly_premium_selected <- NA_real_
      df$annual_selected_premium <- NA_real_
      df$annual_required_contribution <- NA_real_
      df$premium_tax_credit_raw <- NA_real_
      df$premium_tax_credit <- 0

      ptc_idx <- which(df$apply_premium_tax_credit)
      ptc_rows <- df[ptc_idx, , drop = FALSE] %>%
        calculate_premium_tax_credit(
          fed_poverty_line = fed_poverty_line,
          fed_premium_tax_credit = fed_premium_tax_credit,
          effective_year = year,
          premium_col = "health_insurance_premium_used"
        )
      df$fpl[ptc_idx] <- ptc_rows$fpl
      df$pct_fpl[ptc_idx] <- ptc_rows$pct_fpl
      df$required_income_rate[ptc_idx] <- ptc_rows$required_income_rate
      df$monthly_premium_selected[ptc_idx] <- ptc_rows$monthly_premium_selected
      df$annual_selected_premium[ptc_idx] <- ptc_rows$annual_selected_premium
      df$annual_required_contribution[ptc_idx] <- ptc_rows$annual_required_contribution
      df$premium_tax_credit_raw[ptc_idx] <- ptc_rows$premium_tax_credit_raw
      df$premium_tax_credit[ptc_idx] <- dplyr::coalesce(ptc_rows$premium_tax_credit, 0)
    } else {
      df$premium_tax_credit <- 0
      df$fpl <- NA_real_
      df$pct_fpl <- NA_real_
      df$required_income_rate <- NA_real_
      df$monthly_premium_selected <- NA_real_
      df$annual_selected_premium <- NA_real_
      df$annual_required_contribution <- NA_real_
      df$premium_tax_credit_raw <- NA_real_
    }
    df <- df %>%
      dplyr::mutate(
        premium_tax_credit = dplyr::if_else(.data$apply_premium_tax_credit, .data$premium_tax_credit, 0)
      )
    df <- calculate_tax_from_brackets(df, federal_tax_brackets,
                                      taxable_income_var = "taxable_income",
                                      filing_status_var  = "filing_status",
                                      output_col         = "federal_cumulative_tax")
    df <- calculate_eitc_credit(df)
    df <- calculate_cdctc_credit(df, cdctc_params_list)
    df <- calculate_ctc_credit(df, ctc_params_list)

    if (!is.null(state)) {
      # State payroll taxes (e.g., SUI/WBF-style programs), typically separate
      # from state income tax brackets/credits.
      df <- calculate_state_payroll_taxes(df, state_params$state_payroll, year, state)
      # Build state_taxable_income by applying state deductions/exemptions/adjustments.
      df <- calculate_state_taxable_income(df, state_params$state_ti_adjustments, state, debug)
      # Convert taxable income into pre-credit state income tax.
      df <- calculate_tax_from_brackets(df, state_params$state_brackets,
                                        taxable_income_var = "state_taxable_income",
                                        filing_status_var  = "filing_status",
                                        output_col         = "state_cumulative_tax")
      # Compute refundable/nonrefundable state credits.
      df <- calculate_state_tax_credits(df, state_params$state_credits,
                                        state_params$state_variable_brackets,
                                        state_eitc_lookup,
                                        state_params$state_eitc_params,
                                        year, state, debug)
    }

    df <- calculate_final_federal_income_tax(df)
    if (!is.null(state)) {
      # Apply credit ordering rules to get final state liability (can be negative
      # when refundable credits exceed tax after nonrefundable credits).
      df <- calculate_final_state_income_tax(df)
    } else {
      # No state selected: explicitly zero state tax components used downstream.
      df$state_payroll_tax              <- 0
      df$state_tax_liability_with_refund <- 0
      df$state_refundable_credits        <- 0
    }

    # Local income tax (Phase 2)
    # tax_rate_local is required by validate_input() and defaults to 0 upstream.
    if (!is.null(state)) {
      df <- calculate_local_income_tax(
        df = df,
        tax_type = state_params$local_tax_type,                # scalar for this run/state
        brackets_df = state_params$local_income_tax_brackets,  # NULL unless bracket type
        income_col = "starting_income",
        rate_col = "tax_rate_local",
        state_tax_col = "state_tax_after_nonrefundable",
        out_col = "local_income_tax"
      )
    } else {
      df$local_income_tax <- 0
    }

    df <- df %>%
      dplyr::mutate(
        federal_net = coalesce(federal_tax_liability_with_refund, 0),
        # If state_tax_liability_with_refund is unavailable, treat refundable credits
        # as a negative net liability.
        state_net   = coalesce(
          state_tax_liability_with_refund,
          -coalesce(state_refundable_credits, 0)  # no income tax but refundable credits exist
        ),
        # Taxes count only positive net liabilities; credits are tracked separately.
        total_taxes   = coalesce(total_fed_payroll_tax, 0) +
          coalesce(state_payroll_tax, 0) +
          coalesce(local_income_tax, 0) +
          pmax(federal_net, 0) +
          pmax(state_net, 0),
        total_credits = pmax(-federal_net, 0) +
          pmax(-state_net, 0)
      )

    raw_new_income         <- (df$subtotal3 * 12) + df$total_taxes - df$total_credits
    # Damped fixed-point update:
    # next = damping * previous + (1 - damping) * raw_new
    # Higher damping improves stability near credit cliffs but slows convergence.
    df$new_starting_income <- ifelse(
      df$converged,
      df$starting_income,
      damping * df$previous_income + (1 - damping) * raw_new_income
    )
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
      "%d rows (%.2f%%) did not converge after %d iterations. Last iterative estimate retained.",
      length(non_converged_idx),
      length(non_converged_idx) / nrow(df) * 100,
      max_iterations
    ))
    df$iteration_count[non_converged_idx] <- max_iterations
  }

  print_convergence_summary(df, debug)

  df <- calculate_final_federal_income_tax(df)
  if (!is.null(state)) {
    df <- calculate_final_state_income_tax(df)
  }

  df %>%
    dplyr::select(-any_of(c("previous_income", "new_starting_income",
                            "income_diff", "row_converged")))
}
