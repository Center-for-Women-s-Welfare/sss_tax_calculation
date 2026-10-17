#' Calculate marketplace premium tax credit
#'
#' Expects pre-filtered parameter tables supplied by the solver:
#' - fpl_params: one row per household size for the applicable FPL year/area
#' - ptc_params: bracket schedule for one effective_year_ptc
#'
#' @param df Dataframe containing household_size, starting_income, health_ins_market
#' @param fpl_params Dataframe with columns household_size, fpl_amount
#' @param ptc_params Dataframe with columns income_pct_fpl_min, income_pct_fpl_max,
#'   required_income_rate_min, required_income_rate_max
#' @return df with fpl_amount, pct_fpl, required_income_rate, premium_tax_credit
#' @export
calculate_premium_tax_credit <- function(df, fpl_params, ptc_params) {
  
  required_calculation_cols <- c("household_size", "starting_income", "health_ins_market")
  missing_calculation_cols <- setdiff(required_calculation_cols, names(df))
  if (length(missing_calculation_cols) > 0) {
    stop("df is missing required columns: ",
         paste(missing_calculation_cols, collapse = ", "))
  }
  
  required_fpl_cols <- c("household_size", "fpl_amount")
  missing_fpl_cols <- setdiff(required_fpl_cols, names(fpl_params))
  if (length(missing_fpl_cols) > 0) {
    stop("fpl_params is missing required columns: ",
         paste(missing_fpl_cols, collapse = ", "))
  }
  
  required_ptc_cols <- c(
    "income_pct_fpl_min", "income_pct_fpl_max",
    "required_income_rate_min", "required_income_rate_max"
  )
  missing_ptc_cols <- setdiff(required_ptc_cols, names(ptc_params))
  if (length(missing_ptc_cols) > 0) {
    stop("ptc_params is missing required columns: ",
         paste(missing_ptc_cols, collapse = ", "))
  }
  
  ptc_params <- ptc_params %>%
    dplyr::arrange(income_pct_fpl_min)
  
  # 1) Income as percent of FPL
  df <- df %>%
    mutate(
      pct_fpl = (starting_income / fpl_amount) * 100
    )
  
  # 2) Required contribution rate via bracket interpolation
  minimum_pct_fpl <- min(ptc_params$income_pct_fpl_min, na.rm = TRUE)
  pct_fpl_for_rate <- pmax(df$pct_fpl, minimum_pct_fpl)
  
  bracket_index <- vapply(
    pct_fpl_for_rate,
    function(x) {
      if (is.na(x)) return(NA_integer_)
      idx <- which(
        x >= ptc_params$income_pct_fpl_min &
          (x < ptc_params$income_pct_fpl_max | is.infinite(ptc_params$income_pct_fpl_max))
      )
      if (length(idx) == 0) return(NA_integer_)
      idx[[1]]
    },
    integer(1)
  )
  
  required_income_rate <- rep(NA_real_, nrow(df))
  matched <- !is.na(bracket_index)
  
  if (any(matched)) {
    min_pct <- ptc_params$income_pct_fpl_min[bracket_index[matched]]
    max_pct <- ptc_params$income_pct_fpl_max[bracket_index[matched]]
    min_rt  <- ptc_params$required_income_rate_min[bracket_index[matched]]
    max_rt  <- ptc_params$required_income_rate_max[bracket_index[matched]]
    
    prop <- ifelse(
      is.infinite(max_pct),
      0,
      (pct_fpl_for_rate[matched] - min_pct) / (max_pct - min_pct)
    )
    
    required_income_rate[matched] <- min_rt + prop * (max_rt - min_rt)
  }
  
  df <- df %>%
    dplyr::mutate(required_income_rate = required_income_rate)
  # Highest finite upper bound of the schedule. If the schedule has no
  # open-ended (Inf) bracket, incomes at or above this are ineligible
  # (e.g. 2026: no PTC at or above 400% FPL).
  has_open_top  <- any(is.infinite(ptc_params$income_pct_fpl_max))
  schedule_top  <- max(ptc_params$income_pct_fpl_max[is.finite(ptc_params$income_pct_fpl_max)])
  
  df <- df %>%
    mutate(
      required_income_rate = required_income_rate,
      ptc_eligible = if (has_open_top) {
        !is.na(pct_fpl)
      } else {
        !is.na(pct_fpl) & pct_fpl < schedule_top
      }
    )
  
  # Rows that are eligible must have a rate; ineligible rows legitimately do not.
  if (anyNA(df$pct_fpl)) {
    stop("pct_fpl is NA for some rows; check starting_income and FPL join.")
  }
  if (any(df$ptc_eligible & is.na(df$required_income_rate))) {
    stop("required_income_rate is NA for eligible rows; check ptc_params bracket coverage.")
  }
  
  # 3) PTC amount: max(0, annual benchmark premium - expected contribution)
  df  <- df %>%
    dplyr::mutate(
      annual_market_premium = .data$health_ins_market * 12,
      expected_contribution = dplyr::if_else(
        .data$ptc_eligible, .data$required_income_rate * .data$starting_income, NA_real_
      ),
      premium_tax_credit = dplyr::if_else(
        .data$ptc_eligible,
        pmax(0, .data$annual_market_premium - .data$expected_contribution),
        0
      )
    )
}