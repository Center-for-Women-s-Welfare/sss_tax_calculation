# validation.R
# Input validation functions for SSS Tax Calculation Engine

#' Validate Input Dataframe
#'
#' Checks that the input dataframe has all required columns and valid data
#'
#' @param df Input dataframe (calculations_df with basic needs)
#' @return TRUE if valid, stops with error message if invalid
validate_input <- function(df) {
  
  # Required columns for tax calculations
  required_cols <- c(
    # Subtotals
    "subtotal2", "subtotal3",
    # Family info
    "household_type", "children", "adult",
    # Costs
    "child_care_cost", "health_ins_premium",
    # Geography
    "county_table_number"
  )
  
  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", "),
      "\nEnsure basic needs calculations have been completed."
    ))
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
  
  return(TRUE)
}
