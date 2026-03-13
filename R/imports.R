# R/imports.R
# Necessary to ensure that the required packages are imported when the package is loaded.

#' @importFrom dplyr filter select mutate left_join coalesce any_of transmute
#'   group_by summarize rename pull case_when if_else
#' @importFrom tidyr pivot_wider expand.grid
#' @importFrom readr read_csv
#' @importFrom rlang `!!` .data
NULL