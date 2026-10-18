# Helper to initialize/update employer golden fixture for regression tests.
# Usage (from repo root):
#   source("tests/testthat/helper-update-employer-golden-fixture.R")
#   update_employer_golden_fixture()
#   devtools::test(filter = "health-insurance-scenarios-regression")
#
# Optional:
#   update_employer_golden_fixture(write = FALSE)  # preview only

update_employer_golden_fixture <- function(write = TRUE) {
  # Build the same baseline row used in the test
  df <- data.frame(
    household_type = "single_adult",
    adult = 1L,
    children = 0L,
    children_under6 = 0L,
    children_6plus = 0L,
    household_size = 1L,
    subtotal2 = 3200,
    subtotal3 = 3200,
    child_care_cost = 0,
    health_ins_premium = 220,
    health_ins_market = 950,
    county_table_number = "golden_1",
    housing_cost = 1250,
    public_transit_cost = 0,
    tax_rate_local = 0,
    stringsAsFactors = FALSE
  )

  out <- solve_starting_income_iterative(
    df = df,
    year = 2026,
    state = NULL,
    health_insurance_scenario = "employer",
    max_iterations = 200,
    tolerance = 0.01,
    damping = 0.5,
    debug = FALSE
  )

  # Repo-relative path (works if run from repo root)
  fixture_path <- file.path(
    "tests", "testthat", "_snaps", "health_insurance_employer_golden.csv"
  )

  if (!file.exists(fixture_path)) {
    dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)
    fixture <- data.frame(
      case = "single_adult_baseline",
      year = 2026,
      state = NA_character_,
      health_insurance_scenario = "employer",
      starting_income = NA_real_,
      total_taxes = NA_real_,
      total_credits = NA_real_,
      stringsAsFactors = FALSE
    )
  } else {
    fixture <- readr::read_csv(fixture_path, show_col_types = FALSE)
  }

  key_match <- fixture$case == "single_adult_baseline" &
    fixture$year == 2026 &
    fixture$health_insurance_scenario == "employer"

  if (!any(key_match)) {
    fixture <- dplyr::bind_rows(
      fixture,
      data.frame(
        case = "single_adult_baseline",
        year = 2026,
        state = NA_character_,
        health_insurance_scenario = "employer",
        starting_income = NA_real_,
        total_taxes = NA_real_,
        total_credits = NA_real_,
        stringsAsFactors = FALSE
      )
    )
    key_match <- fixture$case == "single_adult_baseline" &
      fixture$year == 2026 &
      fixture$health_insurance_scenario == "employer"
  }

  fixture$starting_income[key_match] <- out$starting_income[[1]]
  fixture$total_taxes[key_match]     <- out$total_taxes[[1]]
  fixture$total_credits[key_match]   <- out$total_credits[[1]]

  message("Computed golden values:")
  message("  starting_income = ", signif(out$starting_income[[1]], 12))
  message("  total_taxes     = ", signif(out$total_taxes[[1]], 12))
  message("  total_credits   = ", signif(out$total_credits[[1]], 12))

  if (isTRUE(write)) {
    readr::write_csv(fixture, fixture_path, na = "")
    message("Fixture updated: ", fixture_path)
  } else {
    message("Preview only (write = FALSE). Fixture not written.")
  }

  invisible(fixture)
}