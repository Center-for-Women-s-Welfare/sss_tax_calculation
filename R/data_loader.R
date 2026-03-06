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
    fed_payroll  = read_csv(file.path(fed_tax_dir, "tax_fed_payroll_df.csv"),  show_col_types = FALSE),
    fed_credits  = read_csv(file.path(fed_tax_dir, "tax_fed_credits_df.csv"),  show_col_types = FALSE),
    fed_brackets = read_csv(file.path(fed_tax_dir, "tax_fed_income_brackets_df.csv"), show_col_types = FALSE),
    fed_sd       = read_csv(file.path(fed_tax_dir, "tax_fed_sd_df.csv"),       show_col_types = FALSE)
  )
}
