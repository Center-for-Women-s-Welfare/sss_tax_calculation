# R/imports.R
# Necessary to ensure that the required packages are imported when the package is loaded.

#' @importFrom dplyr filter select mutate left_join coalesce any_of transmute
#'   group_by summarize rename pull case_when if_else row_number bind_rows
#'   across all_of distinct slice ungroup full_join
#' @importFrom dplyr `%>%`
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom readr read_csv
#' @importFrom rlang `!!` .data sym
#' @importFrom utils head
NULL