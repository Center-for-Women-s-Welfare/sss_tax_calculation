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

  # --- Local income tax metadata (registry) ---
  local_registry <- readr::read_csv(
    system.file("extdata", "local", as.character(year), "tax_local_income_tax.csv",
                package = "sssTaxCalculation"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      state_id = toupper(as.character(.data$state_id)),
      geography = tolower(trimws(as.character(.data$geography))),
      tax_type = tolower(trimws(as.character(.data$tax_type)))
    ) %>%
    dplyr::filter(.data$state_id == toupper(state))
  
  if (nrow(local_registry) == 0) {
    local_tax_type <- "percent"              # harmless default with tax_rate_local=0
    local_income_tax_brackets <- tibble::tibble()
  } else {
    # Keep this strict for now: one tax_type per state run
    # (if you later support multiple components in one run, change this to list-column/model)
    types <- unique(local_registry$tax_type)
    if (length(types) > 1) {
      stop(
        "Multiple local tax types found for state ", state, ": ",
        paste(types, collapse = ", "),
        ". Current solver expects one local_tax_type per run."
      )
    }
    local_tax_type <- types[[1]]
    
    if (identical(local_tax_type, "bracket")) {
      # expected local bracket file path
      # adjust filename if your repo uses a different convention
      bracket_path <- system.file(
        "extdata", "local", as.character(year),
        paste0("tax_local_income_brackets_", state, ".csv"),
        package = "sssTaxCalculation"
      )
      
      if (identical(bracket_path, "")) {
        stop("Local tax type is 'bracket' but bracket file not found for ", state)
      }
      
      local_income_tax_brackets <- readr::read_csv(bracket_path, show_col_types = FALSE) %>%
        dplyr::mutate(
          income_min = as.numeric(.data$income_min),
          income_max = as.numeric(.data$income_max),
          value      = as.numeric(.data$value),
          filing_status = dplyr::if_else(
            is.na(.data$filing_status) | trimws(.data$filing_status) == "",
            "all",
            as.character(.data$filing_status)
          )
        )
    } else {
      local_income_tax_brackets <- tibble::tibble()
    }
  }
  
  list(
    state_brackets          = readr::read_csv(file.path(state_tax_dir, "tax_state_income_brackets.csv"),   show_col_types = FALSE) %>% filter_to_year_state(),
    state_credits           = readr::read_csv(file.path(state_tax_dir, "tax_state_credits.csv"),           show_col_types = FALSE) %>% ensure_num_children_column() %>% filter_to_year_state(),
    state_payroll           = readr::read_csv(file.path(state_tax_dir, "tax_state_payroll.csv"),           show_col_types = FALSE) %>% filter_to_year_state(),
    state_ti_adjustments    = readr::read_csv(file.path(state_tax_dir, "tax_state_ti_adjustments.csv"),    show_col_types = FALSE) %>% filter_to_year_state(),
    state_variable_brackets = readr::read_csv(file.path(state_tax_dir, "tax_state_variable_brackets.csv"), show_col_types = FALSE) %>% ensure_num_children_column() %>% filter_to_year_state(),
    state_eitc_lookup       = readr::read_csv(file.path(state_tax_dir, "tax_state_eitc_lookup.csv"),       show_col_types = FALSE) %>% filter_to_year_state(),
    state_eitc_params       = readr::read_csv(file.path(state_tax_dir, "tax_state_eitc_params.csv"),       show_col_types = FALSE) %>% filter_to_year_state(),
    
    # new local-tax entries
    local_tax_type          = local_tax_type,            # scalar string
    local_income_tax_brackets   = local_income_tax_brackets      # tibble(), or bracket table when needed
  )
  }

