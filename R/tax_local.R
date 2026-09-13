# R/tax_local.R
# Local income tax function

#' Calculate local income tax amount
#'
#' Supports four local tax types:
#' - percent: starting_income * tax_rate_local
#' - flat_fee: tax_rate_local (annual dollar amount)
#' - surtax: tax_rate_local * state_tax_after_nonrefundable
#' - bracket: uses bracket helper on starting_income with local bracket params
#'
#' Notes:
#' - This function assumes `tax_rate_local` is already present in `df`
#'   (defaulted to 0 upstream when not applicable).
#' - For states with multiple local components already combined upstream,
#'   `tax_rate_local` can represent that pre-combined effective input.
#'
#' @param df Data frame / tibble with tax inputs.
#' @param tax_type Character scalar in c("percent","flat_fee","surtax","bracket").
#' @param brackets_df Optional bracket parameters for tax_type = "bracket".
#' @param income_col Column name used as income base (default "starting_income").
#' @param rate_col Column name for local tax input (default "tax_rate_local").
#' @param state_tax_col Column name used by surtax (default "state_tax_after_nonrefundable").
#' @param out_col Output column name (default "local_income_tax").
#'
#' @return `df` with added/updated `out_col`.
calculate_local_income_tax <- function(df,
                                       tax_type,
                                       brackets_df = NULL,
                                       income_col = "starting_income",
                                       rate_col = "tax_rate_local",
                                       state_tax_col = "state_tax_after_nonrefundable",
                                       out_col = "local_income_tax") {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  
  tax_type <- tolower(trimws(as.character(tax_type)))
  allowed_tax_types <- c("percent", "flat_fee", "surtax", "bracket")
  if (!tax_type %in% allowed_tax_types) {
    stop(
      paste0("`tax_type` must be one of: ", paste(allowed_tax_types, collapse = ", ")),
      call. = FALSE
    )
  }
  
  if (!income_col %in% names(df)) {
    stop("Missing required income column: `", income_col, "`.", call. = FALSE)
  }
  if (!rate_col %in% names(df)) {
    stop("Missing required local tax rate column: `", rate_col, "`.", call. = FALSE)
  }
  
  # Standardize numeric inputs
  df <- df %>%
    dplyr::mutate(
      !!income_col := suppressWarnings(as.numeric(.data[[income_col]])),
      !!rate_col   := suppressWarnings(as.numeric(.data[[rate_col]]))
    )
  
  if (any(is.na(df[[rate_col]]))) {
    # upstream should default to 0; enforce here for safety
    df[[rate_col]][is.na(df[[rate_col]])] <- 0
  }
  if (any(df[[rate_col]] < 0, na.rm = TRUE)) {
    stop("`", rate_col, "` contains negative values.", call. = FALSE)
  }
  
  n <- nrow(df)
  local_tax <- rep(0, n)
  
  if (tax_type == "percent") {
    local_tax <- df[[income_col]] * df[[rate_col]]
  }
  
  if (tax_type == "flat_fee") {
    # annual fixed amount; rate_col stores dollars in this case
    local_tax <- df[[rate_col]]
  }
  
  if (tax_type == "surtax") {
    if (!state_tax_col %in% names(df)) {
      stop("Missing required surtax base column: `", state_tax_col, "`.", call. = FALSE)
    }
    df[[state_tax_col]] <- suppressWarnings(as.numeric(df[[state_tax_col]]))
    local_tax <- df[[rate_col]] * df[[state_tax_col]]
  }
  
  if (tax_type == "bracket") {
    if (is.null(brackets_df) || !is.data.frame(brackets_df) || nrow(brackets_df) == 0) {
      stop("`brackets_df` is required and must be non-empty for tax_type = 'bracket'.", call. = FALSE)
    }
    
    req <- c("income_min", "income_max", "value")
    miss <- setdiff(req, names(brackets_df))
    if (length(miss) > 0) {
      stop("`brackets_df` missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
    }
    
    fs <- if ("filing_status" %in% names(df)) as.character(df$filing_status) else rep("all", nrow(df))
    
    bracket_rate <- .bracket_lookup(
      income = df[[income_col]],
      filing_status = fs,
      bracket_df = brackets_df
    )
    
    bracket_rate <- suppressWarnings(as.numeric(bracket_rate))
    bracket_rate[is.na(bracket_rate)] <- 0
    
    # For local bracket taxes, bracket `value` is a rate; tax = income * rate
    local_tax <- df[[income_col]] * bracket_rate
  }  
  
  # Finalize
  local_tax[is.na(local_tax)] <- 0
  # guard against tiny negative floating artifacts
  local_tax <- pmax(local_tax, 0)
  
  df[[out_col]] <- local_tax
  df
}