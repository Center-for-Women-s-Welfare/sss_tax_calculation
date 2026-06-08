# data_loader.R
# Functions to load tax parameter CSVs

#' Load Federal Tax Parameters from CSV Files
#'
#' Loads all federal tax parameter CSVs for a specific year from the package data directory
#'
#' @param year Tax year (e.g., 2026)
#' @return Named list with four dataframes: fed_payroll, fed_credits, fed_brackets, fed_sd
load_federal_tax_params <- function(year) {

  fed_tax_dir <- system.file("extdata", "federal", as.character(year),
                             package = "sssTaxCalculation")

  if (!nzchar(fed_tax_dir)) {
    stop(paste(
      "Federal tax data not found for year", year,
      "\nEnsure tax parameter CSVs exist in inst/extdata/federal/", year, "/"
    ))
  }

  list(
    fed_payroll  = readr::read_csv(file.path(fed_tax_dir, "tax_fed_payroll.csv"),  show_col_types = FALSE),
    fed_credits  = readr::read_csv(file.path(fed_tax_dir, "tax_fed_credits.csv"),  show_col_types = FALSE),
    fed_brackets = readr::read_csv(file.path(fed_tax_dir, "tax_fed_income_brackets.csv"), show_col_types = FALSE),
    fed_sd       = readr::read_csv(file.path(fed_tax_dir, "tax_fed_sd.csv"),       show_col_types = FALSE)
  )
}

#' Load State Tax Parameters from CSV Files
#'
#' Loads all state tax parameter CSVs for a specific year and state from the
#' package data directory, filtering each to the requested `sss_year` and `state`
#'
#' @param year Tax year (e.g., 2026)
#' @param state State postal code (e.g., "IA")
#' @return Named list with six dataframes: state_brackets, state_credits, state_payroll,
#'   state_ti_adjustments, state_variable_brackets, state_eitc_lookup
load_state_tax_params <- function(year, state) {

  state_tax_dir <- system.file("extdata", "state", as.character(year),
                               package = "sssTaxCalculation")

  if (!nzchar(state_tax_dir)) {
    stop(paste(
      "State tax data not found for year", year,
      "\nEnsure tax parameter CSVs exist in inst/extdata/state/", year, "/"
    ))
  }

  filter_to_year_state <- function(df) {
    df %>%
      filter(sss_year == !!year, state == !!state)
  }

  list(
    state_brackets          = readr::read_csv(file.path(state_tax_dir, "tax_state_income_brackets.csv"),  show_col_types = FALSE) %>% filter_to_year_state(),
    state_credits           = readr::read_csv(file.path(state_tax_dir, "tax_state_credits.csv"),          show_col_types = FALSE) %>% filter_to_year_state(),
    state_payroll           = readr::read_csv(file.path(state_tax_dir, "tax_state_payroll.csv"),          show_col_types = FALSE) %>% filter_to_year_state(),
    state_ti_adjustments    = readr::read_csv(file.path(state_tax_dir, "tax_state_ti_adjustments.csv"),   show_col_types = FALSE) %>% filter_to_year_state(),
    state_variable_brackets = readr::read_csv(file.path(state_tax_dir, "tax_state_variable_brackets.csv"), show_col_types = FALSE) %>% filter_to_year_state(),
    state_eitc_lookup       = readr::read_csv(file.path(state_tax_dir, "tax_state_eitc_lookup.csv"),      show_col_types = FALSE) %>% filter_to_year_state()
  )
}
