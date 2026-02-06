# data_loader.R
# Functions to load tax parameter CSVs

library(readr)

#' Load Federal Tax Parameters from CSV Files
#'
#' Loads all federal tax parameter CSVs for a specific year from the package data directory
#'
#' @param year Tax year (e.g., 2026)
#' @return Named list with four dataframes: fed_payroll, fed_credits, fed_brackets, fed_sd
load_federal_tax_params <- function(year) {
  
  # Build path to federal tax data directory
  fed_tax_dir <- file.path(
    sss_code_path(repo = "sss_tax_calculation"),
    "src",
    "data",
    "federal",
    as.character(year)
  )
  
  # Check if directory exists
  if (!dir.exists(fed_tax_dir)) {
    stop(paste(
      "Federal tax data directory not found for year", year,
      "\nExpected path:", fed_tax_dir,
      "\nEnsure tax parameter CSVs exist in sss_tax_calculation/src/data/federal/", year, "/"
    ))
  }
  
  # Load each CSV file
  tax_fed_payroll_df <- read_csv(
    file.path(fed_tax_dir, "tax_fed_payroll_df.csv"),
    show_col_types = FALSE
  )
  
  tax_fed_credits_df <- read_csv(
    file.path(fed_tax_dir, "tax_fed_credits_df.csv"),
    show_col_types = FALSE
  )
  
  tax_fed_income_brackets_df <- read_csv(
    file.path(fed_tax_dir, "tax_fed_income_brackets_df.csv"),
    show_col_types = FALSE
  )
  
  tax_fed_sd_df <- read_csv(
    file.path(fed_tax_dir, "tax_fed_sd_df.csv"),
    show_col_types = FALSE
  )
  
  # Return as named list
  list(
    fed_payroll = tax_fed_payroll_df,
    fed_credits = tax_fed_credits_df,
    fed_brackets = tax_fed_income_brackets_df,
    fed_sd = tax_fed_sd_df
  )
}
