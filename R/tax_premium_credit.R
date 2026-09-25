# tax_premium_credit.R
# Marketplace premium tax credit helper functions

#' Calculate marketplace premium tax credit
#'
#' Matches each household's size to the applicable federal poverty-line (FPL)
#' amount for the year before the premium tax-credit schedule year, calculates
#' income as a percentage of that FPL amount, interpolates the required
#' household contribution rate from the premium tax-credit bracket schedule,
#' and calculates the resulting premium tax credit.
#'
#' The final formula is:
#'
#' \deqn{
#' premium\_tax\_credit =
#' (health\_ins\_market \times 12) -
#' (required\_income\_rate \times starting\_income)
#' }
#'
#' `health_ins_market` is assumed to be a monthly amount and is annualized
#' (multiplied by 12) before being compared to annual `starting_income`.
#'
#' Household sizes above 20 use the 20-person FPL row, which is the largest
#' household size represented in the federal poverty-line data. FPL
#' percentages below the lowest premium tax-credit bracket are clamped to
#' that bracket. For an open-ended bracket with an infinite upper bound, the
#' minimum bracket rate is used directly.
#'
#' @param calculations_df Dataframe containing `household_size`,
#'   `starting_income`, and `health_ins_market`.
#' @param fed_poverty_line Dataframe containing `fpl_year`, `fpl_area`,
#'   `hh_size`, and `fpl`.
#' @param fed_premium_tax_credit Dataframe containing `effective_year`,
#'   `income_pct_fpl_min`, `income_pct_fpl_max`,
#'   `required_income_rate_min`, and `required_income_rate_max`.
#' @param effective_year Premium tax-credit schedule year. The FPL lookup
#'   uses `effective_year - 1`.
#' @param fpl_area FPL geography to use. Defaults to `"FORTY_EIGHT_DC"`.
#' @return `calculations_df` with `fpl`, `pct_fpl`, `required_income_rate`,
#'   and `premium_tax_credit` columns added.
#' @export
calculate_premium_tax_credit <- function(calculations_df,
                                         fed_poverty_line,
                                         fed_premium_tax_credit,
                                         effective_year,
                                         fpl_area = "FORTY_EIGHT_DC") {
  
  # ---- Validate inputs -------------------------------------------------
  required_calculation_cols <- c(
    "household_size",
    "starting_income",
    "health_ins_market"
  )
  missing_calculation_cols <- setdiff(
    required_calculation_cols,
    names(calculations_df)
  )
  if (length(missing_calculation_cols) > 0) {
    stop(
      "calculations_df is missing required columns: ",
      paste(missing_calculation_cols, collapse = ", ")
    )
  }
  
  required_fpl_cols <- c("fpl_year", "fpl_area", "hh_size", "fpl")
  missing_fpl_cols <- setdiff(required_fpl_cols, names(fed_poverty_line))
  if (length(missing_fpl_cols) > 0) {
    stop(
      "fed_poverty_line is missing required columns: ",
      paste(missing_fpl_cols, collapse = ", ")
    )
  }
  
  required_ptc_cols <- c(
    "effective_year",
    "income_pct_fpl_min",
    "income_pct_fpl_max",
    "required_income_rate_min",
    "required_income_rate_max"
  )
  missing_ptc_cols <- setdiff(
    required_ptc_cols,
    names(fed_premium_tax_credit)
  )
  if (length(missing_ptc_cols) > 0) {
    stop(
      "fed_premium_tax_credit is missing required columns: ",
      paste(missing_ptc_cols, collapse = ", ")
    )
  }
  
  # ---- Step 1: household income as a percentage of the FPL -------------
  # The PTC schedule for a given effective year uses the prior year's FPL.
  fpl_year_value <- effective_year - 1
  
  fpl_lookup <- fed_poverty_line %>%
    dplyr::filter(
      .data$fpl_year == fpl_year_value,
      .data$fpl_area == fpl_area
    ) %>%
    dplyr::select(.data$hh_size, .data$fpl)
  
  if (nrow(fpl_lookup) == 0) {
    stop(
      "No federal poverty-line data found for fpl_year = ",
      fpl_year_value,
      " and fpl_area = ",
      fpl_area,
      "."
    )
  }
  
  calculations_df <- calculations_df %>%
    dplyr::mutate(
      fpl_household_size = pmin(.data$household_size, max(fpl_lookup$hh_size))
    ) %>%
    dplyr::left_join(
      fpl_lookup,
      by = c("fpl_household_size" = "hh_size"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      pct_fpl = (.data$starting_income / .data$fpl) * 100
    ) %>%
    dplyr::select(-.data$fpl_household_size)
  
  # ---- Step 2: required income rate from the PTC bracket schedule ------
  ptc_schedule <- fed_premium_tax_credit %>%
    dplyr::filter(.data$effective_year == !!effective_year) %>%
    dplyr::arrange(.data$income_pct_fpl_min)
  
  if (nrow(ptc_schedule) == 0) {
    stop(
      "No premium tax-credit schedule found for effective_year = ",
      effective_year,
      "."
    )
  }
  
  minimum_pct_fpl <- min(ptc_schedule$income_pct_fpl_min)
  
  # Clamp values below the schedule to its first bracket. Values above a
  # finite final bracket remain unmatched and receive NA.
  pct_fpl_for_rate <- pmax(calculations_df$pct_fpl, minimum_pct_fpl)
  
  bracket_index <- vapply(
    pct_fpl_for_rate,
    function(pct_fpl_value) {
      if (is.na(pct_fpl_value)) {
        return(NA_integer_)
      }
      
      matched_rows <- which(
        pct_fpl_value >= ptc_schedule$income_pct_fpl_min &
          (
            pct_fpl_value < ptc_schedule$income_pct_fpl_max |
              is.infinite(ptc_schedule$income_pct_fpl_max)
          )
      )
      
      if (length(matched_rows) == 0) {
        return(NA_integer_)
      }
      
      matched_rows[[1]]
    },
    integer(1)
  )
  
  required_income_rate <- rep(NA_real_, nrow(calculations_df))
  matched <- !is.na(bracket_index)
  
  if (any(matched)) {
    matched_min_pct_fpl <- ptc_schedule$income_pct_fpl_min[bracket_index[matched]]
    matched_max_pct_fpl <- ptc_schedule$income_pct_fpl_max[bracket_index[matched]]
    matched_min_rate <- ptc_schedule$required_income_rate_min[bracket_index[matched]]
    matched_max_rate <- ptc_schedule$required_income_rate_max[bracket_index[matched]]
    
    interpolation_proportion <- ifelse(
      is.infinite(matched_max_pct_fpl),
      0,
      (pct_fpl_for_rate[matched] - matched_min_pct_fpl) /
        (matched_max_pct_fpl - matched_min_pct_fpl)
    )
    
    required_income_rate[matched] <- matched_min_rate +
      interpolation_proportion * (matched_max_rate - matched_min_rate)
  }
  
  calculations_df$required_income_rate <- required_income_rate
  
  # ---- Step 3: final premium tax credit ---------------------------------
  calculations_df %>%
    dplyr::mutate(
      premium_tax_credit =
        (.data$health_ins_market * 12) -
        (.data$required_income_rate * .data$starting_income)
    )
}