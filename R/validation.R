# validation.R
# Input validation functions for SSS Tax Calculation Engine

#' Validate input dataframe and required derived columns
#'
#' @param df Input calculations dataframe
#' @param year Tax year (optional)
#' @param state State abbreviation (optional)
#' @param methods_present Character vector of calculation_method values found in
#'   loaded tax parameter tables. If NULL, only base required columns are checked.
#' @return Invisibly TRUE; errors if required columns are missing.
validate_input <- function(df, year = NULL, state = NULL, methods_present = NULL) {
  
  if (!is.data.frame(df)) {
    stop("Input must be a data frame, got: ", class(df))
  }
  
  # Always-required columns (existing behavior)
  required_cols <- c(
    # Subtotals
    "subtotal2", "subtotal3",
    # Family info
    "household_type", "children", "adult",
    # Costs
    "child_care_cost",
    # Geography
    "county_table_number",
    # Local income tax rate/fee/surtax
    "tax_rate_local"
  )
  
  # Conditional requirements keyed by apply_calculation_method branches
  method_required_cols <- list(
    per_person              = c("household_size"),
    per_adult               = c("adult"),
    per_child               = c("children"),
    per_child_minus1        = c("children"),
    per_child_under6_double = c("children", "children_under6"),
    per_child_under6        = c("children_under6"),
    per_child_6plus         = c("children_6plus"),
    percent_of_fed_tax      = c("final_federal_income_tax"),
    percent_of_fed_eitc     = c("eitc_credit"),
    percent_of_fed_cdctc    = c("cdctc_credit"),
    percent_of_fed_cdctc_estimate    = c("cdctc_estimate")
    
  )
  
  methods_present <- unique(as.character(methods_present)[!is.na(methods_present)])
  
  conditional_cols <- character(0)
  if (length(methods_present) > 0) {
    matched_methods <- intersect(methods_present, names(method_required_cols))
    if (length(matched_methods) > 0) {
      conditional_cols <- unique(unlist(method_required_cols[matched_methods], use.names = FALSE))
    }
  }
  
  all_required_cols <- unique(c(required_cols, conditional_cols))
  missing_cols <- setdiff(all_required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    missing_base <- intersect(missing_cols, required_cols)
    missing_cond <- setdiff(missing_cols, required_cols)
    
    msg <- c(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", "),
      "\nEnsure basic needs calculations have been completed."
    ))
    
    if (length(missing_base) > 0) {
      msg <- c(msg, sprintf("Base required missing: %s", paste(missing_base, collapse = ", ")))
    }
    
    if (length(missing_cond) > 0) {
      # helpful method->missing map
      matched_methods <- intersect(methods_present, names(method_required_cols))
      by_method <- vapply(
        matched_methods,
        function(m) {
          miss <- intersect(method_required_cols[[m]], missing_cond)
          if (!length(miss)) return(NA_character_)
          sprintf("%s -> %s", m, paste(miss, collapse = ", "))
        },
        character(1)
      )
      by_method <- by_method[!is.na(by_method)]
      if (length(by_method) > 0) {
        msg <- c(
          msg,
          sprintf("Method-dependent missing: %s", paste(missing_cond, collapse = ", ")),
          sprintf("Triggered by methods: %s", paste(by_method, collapse = " | "))
        )
      }
    }
    
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  health_premium_cols <- c(
    "health_ins_premium",
    "health_ins_market",
    "health_insurance_premium_used"
  )
  if (!any(health_premium_cols %in% names(df))) {
      stop(
        "Missing required health insurance column: provide health_insurance_premium_used, ",
        "health_ins_premium, or health_ins_market."
      )
    }
  # Check for NA values in critical columns
  critical_cols <- c("subtotal2", "subtotal3", "household_type")
  for (col in critical_cols) {
    if (any(is.na(df[[col]]))) {
      stop(paste(
        "Column", col, "contains NA values.",
        "\nFound", sum(is.na(df[[col]])), "NA values out of", nrow(df), "rows."
      ))
    }
  }
  
  # Check for negative values in subtotals
  if (any(df$subtotal2 < 0, na.rm = TRUE)) {
    stop(paste(
      "subtotal2 contains negative values.",
      "\nFound", sum(df$subtotal2 < 0, na.rm = TRUE), "negative values."
    ))
  }
  
  if (any(df$subtotal3 < 0, na.rm = TRUE)) {
    stop(paste(
      "subtotal3 contains negative values.",
      "\nFound", sum(df$subtotal3 < 0, na.rm = TRUE), "negative values."
    ))
  }
  
  # Check household_type values
  valid_household_types <- c("single_adult", "single_parent", "married")
  invalid_types <- unique(df$household_type[!df$household_type %in% valid_household_types])
  if (length(invalid_types) > 0) {
    stop(paste(
      "Invalid household_type values found:",
      paste(invalid_types, collapse = ", "),
      "\nValid values are:", paste(valid_household_types, collapse = ", ")
    ))
  }
  
  # Check for reasonable data size
  if (nrow(df) == 0) {
    stop("Input dataframe is empty (0 rows)")
  }
  
  if (nrow(df) > 1000000) {
    warning(paste(
      "Large dataframe detected:", nrow(df), "rows.",
      "\nProcessing may take significant time."
    ))
  }
  
  invisible(TRUE)
}  
