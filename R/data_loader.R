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
#' @return Named list with seven dataframes: state_brackets, state_credits, state_payroll,
#'   state_ti_adjustments, state_variable_brackets, state_eitc_lookup, state_eitc_params
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

  ensure_num_children_column <- function(df) {
    if (!"num_children" %in% names(df)) {
      df <- df %>% mutate(num_children = NA_real_)
    }
    df
  }
  
  list(
    state_brackets          = readr::read_csv(file.path(state_tax_dir, "tax_state_income_brackets.csv"),  show_col_types = FALSE) %>% filter_to_year_state(),
    state_credits           = readr::read_csv(file.path(state_tax_dir, "tax_state_credits.csv"),          show_col_types = FALSE) %>% ensure_num_children_column() %>% filter_to_year_state(),
    state_payroll           = readr::read_csv(file.path(state_tax_dir, "tax_state_payroll.csv"),          show_col_types = FALSE) %>% filter_to_year_state(),
    state_ti_adjustments    = readr::read_csv(file.path(state_tax_dir, "tax_state_ti_adjustments.csv"),   show_col_types = FALSE) %>% filter_to_year_state(),
    state_variable_brackets = readr::read_csv(file.path(state_tax_dir, "tax_state_variable_brackets.csv"), show_col_types = FALSE) %>% ensure_num_children_column() %>% filter_to_year_state(),
    state_eitc_lookup       = readr::read_csv(file.path(state_tax_dir, "tax_state_eitc_lookup.csv"),      show_col_types = FALSE) %>% filter_to_year_state(),
    state_eitc_params       = readr::read_csv(file.path(state_tax_dir, "tax_state_eitc_params.csv"),      show_col_types = FALSE) %>% filter_to_year_state()
  )
}

#' Load local income tax rates for a single state/year
#'
#' Expects files in:
#'   sss_production/data/[year]/processed/taxes_local
#'
#' Expected filename format:
#'   {STATE}{YEAR}_processed_local_income_tax_{YYYYMMDD}{INITIALS}.csv
#' Example:
#'   AL2026_processed_local_income_tax_20260429KSe.csv
#'
#' @param state Character. Two-letter state abbreviation (e.g., "AL").
#' @param year Integer or character. Tax year (e.g., 2026).
#' @param require_file Logical. If TRUE, error when no matching file is found.
#'   If FALSE (default), return NULL when no file is found.
#'
#' @return A tibble with required standardized columns, or NULL if no file is
#'   found and require_file = FALSE.
#' @export
load_local_income_tax_rates <- function(state, year, require_file = FALSE) {
  # Dependencies (explicit namespace where helpful)
  state <- toupper(trimws(as.character(state)))
  year_chr <- as.character(year)
  
  if (!nzchar(state) || nchar(state) != 2) {
    stop("`state` must be a 2-letter abbreviation, e.g. 'AL'.", call. = FALSE)
  }
  if (!grepl("^[0-9]{4}$", year_chr)) {
    stop("`year` must be a 4-digit year, e.g. 2026.", call. = FALSE)
  }
  
  # Build processed data directory:
  # sss_production/data/[year]/processed/taxes_local
  # NOTE: Adjust argument names/order below only if your installed sssUtils differs.
  input_dir <- sssUtils::build_data_processed_path(
    dataset = "taxes_local",
    year = as.integer(year_chr),
    root = "sss_production"
  )
  
  if (!dir.exists(input_dir)) {
    msg <- paste0("Local tax input directory does not exist: ", input_dir)
    if (isTRUE(require_file)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    return(NULL)
  }
  
  # Filename regex:
  # ^{STATE}{YEAR}_processed_local_income_tax_{YYYYMMDD}{INITIALS}.csv$
  # initials = at least 1 letter
  pattern <- paste0(
    "^", state, year_chr,
    "_processed_local_income_tax_",
    "([0-9]{8})",
    "([A-Za-z]+)",
    "\\.csv$"
  )
  
  files <- list.files(
    path = input_dir,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    msg <- paste0(
      "No local income tax file found for ", state, " ", year_chr,
      " in ", input_dir,
      ". Expected pattern: ", state, year_chr,
      "_processed_local_income_tax_YYYYMMDD<initials>.csv"
    )
    if (isTRUE(require_file)) stop(msg, call. = FALSE)
    return(NULL)
  }
  
  # If multiple files exist, pick latest by YYYYMMDD in filename.
  file_names <- basename(files)
  date_match <- regexec(pattern, file_names)
  date_parts <- regmatches(file_names, date_match)
  
  # Extract captured YYYYMMDD group
  file_dates <- vapply(
    date_parts,
    FUN = function(x) if (length(x) >= 2) x[2] else NA_character_,
    FUN.VALUE = character(1)
  )
  
  # Parse safely to Date
  parsed_dates <- as.Date(file_dates, format = "%Y%m%d")
  if (all(is.na(parsed_dates))) {
    msg <- paste0(
      "Found matching local tax files for ", state, " ", year_chr,
      " but could not parse YYYYMMDD date segment."
    )
    if (isTRUE(require_file)) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    return(NULL)
  }
  
  selected_idx <- which.max(parsed_dates)
  selected_file <- files[selected_idx]
  
  # Read CSV
  out <- readr::read_csv(
    file = selected_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  required_cols <- c(
    "fips_state",
    "fips_county",
    "fips_countysub",
    "stusps",
    "countyname",
    "county_town_name",
    "pop2023",
    "tax_rate"
  )
  
  missing_cols <- setdiff(required_cols, names(out))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Local income tax file is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        ". File: ", selected_file
      ),
      call. = FALSE
    )
  }
  
  # Keep required columns first; preserve any extras afterward
  out <- dplyr::select(out, dplyr::all_of(required_cols), dplyr::everything())
  
  # Standardize types
  out <- dplyr::mutate(
    out,
    fips_state = as.character(.data$fips_state),
    fips_county = as.character(.data$fips_county),
    fips_countysub = as.character(.data$fips_countysub),
    stusps = toupper(as.character(.data$stusps)),
    countyname = as.character(.data$countyname),
    county_town_name = as.character(.data$county_town_name),
    pop2023 = suppressWarnings(as.numeric(.data$pop2023)),
    tax_rate = suppressWarnings(as.numeric(.data$tax_rate))
  )
  
  # Light normalization for FIPS-like fields (preserve missing)
  out <- dplyr::mutate(
    out,
    fips_state = ifelse(is.na(.data$fips_state), NA_character_, stringr::str_pad(.data$fips_state, width = 2, side = "left", pad = "0")),
    fips_county = ifelse(is.na(.data$fips_county), NA_character_, stringr::str_pad(.data$fips_county, width = 3, side = "left", pad = "0")),
    fips_countysub = ifelse(is.na(.data$fips_countysub), NA_character_, stringr::str_pad(.data$fips_countysub, width = 5, side = "left", pad = "0"))
  )
  
  # Validate state consistency in file (soft check)
  bad_state_rows <- which(!is.na(out$stusps) & out$stusps != state)
  if (length(bad_state_rows) > 0) {
    warning(
      paste0(
        "Loaded local tax file contains stusps values not equal to requested state ",
        state, ". File: ", selected_file
      ),
      call. = FALSE
    )
  }
  
  # Validate tax_rate
  if (any(is.na(out$tax_rate))) {
    stop(
      paste0("`tax_rate` contains NA/non-numeric values. File: ", selected_file),
      call. = FALSE
    )
  }
  if (any(out$tax_rate < 0)) {
    stop(
      paste0("`tax_rate` contains negative values. File: ", selected_file),
      call. = FALSE
    )
  }
  # Rates are expected to be decimal fractions; warn if unusually high
  if (any(out$tax_rate > 1, na.rm = TRUE)) {
    warning(
      paste0(
        "Some `tax_rate` values are > 1. Expected decimal fractions (e.g., 0.01). ",
        "File: ", selected_file
      ),
      call. = FALSE
    )
  }
  
  attr(out, "source_file") <- selected_file
  out
}
