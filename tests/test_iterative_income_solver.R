# test_iterative_income_solver.R
# Unit tests for the solve_starting_income_iterative function

# required packages
library(testthat)
library(sssUtils)

# function to be tested
source(file.path(sss_code_path(repo = "sss_tax_calculation"), "src", "calculations", "iterative_income_solver.R"))

# Create a valid basic needs dataframe (values from IA)
create_mock_df <- function() {
  data.frame(
    # Household composition
    family_type                = c("a1i0p0s2t4", "a3i0p4s1t0", "a2i1p2s2t0", "a1i3p2s0t1",
                                   "a3i2p1s0t3", "a3i0p0s1t1", "a2i1p0s0t0", "a7c1",
                                   "a2i0p2s1t2", "a3i0p3s1t0"),
    adult                      = c(1, 3, 2, 1, 3, 3, 2, 7, 2, 3),
    infant                     = c(0, 0, 1, 3, 2, 0, 1, 0, 0, 0),
    presch                     = c(0, 4, 2, 2, 1, 0, 0, 0, 2, 3),
    school                     = c(2, 1, 2, 0, 0, 1, 0, 0, 1, 1),
    teen                       = c(4, 0, 0, 1, 3, 1, 0, 0, 2, 0),
    average_child              = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    family_type_id             = c(208, 535, 308, 139, NA, NA, NA, NA, NA, NA),
    household_size             = c(7, 8, 7, 7, 9, 5, 3, 8, 7, 7),
    children                   = c(6, 5, 5, 6, 6, 2, 1, 1, 5, 4),
    children_2ormore           = c(2, 2, 2, 2, 2, 2, 1, 1, 2, 2),
    children_under6            = c(0, 4, 3, 5, 3, 0, 1, 0, 2, 3),
    household_type             = c("single_parent", "married", "married", "single_parent",
                                   "married", "married", "married", "married",
                                   "married", "married"),

    # Bedroom info
    adult_bedrooms             = c(1, 2, 1, 1, 2, 2, 1, 2, 1, 2),
    child_bedrooms             = c(3, 3, 3, 3, 3, 1, 1, 3, 3, 2),  # approximated from bedrooms - adult_bedrooms
    bedrooms                   = c(4, 5, 4, 4, 5, 3, 2, 5, 4, 4),

    # Geographic
    county_table_number        = c("1909599999_1", "1914599999_1", "1914199999_1", "1903799999_1",
                                   "1918999999_1", "1915599999_1", "1904799999_1", "1910199999_1",
                                   "1912999999_1", "1902399999_1"),
    fips_county                = c(19095, 19145, 19141, 19037, 19189, 19155, 19047, 19101, 19129, 19023),
    fips_countysub             = c(1909599999, 1914599999, 1914199999, 1903799999,
                                   1918999999, 1915599999, 1904799999, 1910199999,
                                   1912999999, 1902399999),
    stusps                     = rep("IA", 10),
    areaname                   = c("Iowa County, IA", "Page County, IA", "O'Brien County, IA",
                                   "Chickasaw County, IA", "Winnebago County, IA",
                                   "Pottawattamie County, IA", "Crawford County, IA",
                                   "Jefferson County, IA", "Mills County, IA", "Butler County, IA"),
    countyname                 = c("Iowa County", "Page County", "O'Brien County", "Chickasaw County",
                                   "Winnebago County", "Pottawattamie County", "Crawford County",
                                   "Jefferson County", "Mills County", "Butler County"),
    county_town_name           = rep(NA_character_, 10),
    pop22                      = c(16575, 15225, 14140, 11955, NA, NA, NA, NA, NA, NA),

    # FMR values
    final_weighted_fmr1        = rep(668, 10),
    final_weighted_fmr2        = rep(877, 10),
    final_weighted_fmr3        = c(1229, 1107, 1158, 1229, NA, NA, NA, NA, NA, NA),
    final_weighted_fmr4        = c(1358, 1257, 1163, 1329, NA, NA, NA, NA, NA, NA),

    # Childcare market rates
    cc_infant                  = c(579, 623, 640, 591, NA, NA, NA, NA, NA, NA),
    cc_presch                  = c(550, 592, 608, 561, NA, NA, NA, NA, NA, NA),
    cc_school                  = c(442, 476, 488, 451, NA, NA, NA, NA, NA, NA),

    # Health insurance rates
    health_ins_family          = rep(534, 10),
    health_ins_plus1           = rep(333.25, 10),
    health_ins_single          = rep(164.416666666667, 10),

    # Ratio/misc parameters
    grocery_ratio              = c(1.04513444353212, 0.949894374337348, 0.969944915220459,
                                   1.01505863220746, NA, NA, NA, NA, NA, NA),
    placeholdercolumn1         = rep(NA, 10),
    auto_ins_ratio             = c(1.0327425973929, 1.15822886137129, 1.03357199603826,
                                   1.03743664128938, NA, NA, NA, NA, NA, NA),
    placeholdercolumn2         = rep(NA, 10),
    public_transit_cost        = rep(0, 10),
    local_income_tax_final     = rep(0, 10),
    ct_planning_region         = rep(NA, 10),
    dummy                      = rep(1, 10),

    # Cost columns
    housing_cost               = c(1358.00, 1445.55, 1163.00, 1329.00,
                                   1515.70, 1405.89706624081, 877.00,
                                   1573.20, 1528.50443662002, 1320.00),
    child_care_cost            = c(916.709613534611, 2949.23319105479, 2936.7891691516,
                                   3002.12028414332, NA, NA, NA, NA, NA, NA),
    food_cost                  = c(2024.37316039954, 1714.70182983506, 1543.81282431064,
                                   1422.833061231, 2304.75553805313, 1372.91373085784,
                                   728.290157282591, 2064.2683168451, 1708.23502913674,
                                   1657.53999346317),
    health_ins_premium         = rep(549.045410284133, 10),
    infant_oop_cost            = c(0, 0, 27.5309809472068, 82.5929428416206,
                                   55.0619618944137, 0, 27.5309809472068, 0, 0, 0),
    preschool_oop_cost         = c(0, 92.9398751590532, 46.4699375795266, 46.4699375795266,
                                   23.2349687897633, 0, 0, 0, 46.4699375795266, 69.7049063692899),
    school_oop_cost            = c(88.256509592677, 44.1282547963385, 88.256509592677,
                                   0, 0, 44.1282547963385, 0, 0, 44.1282547963385, 44.1282547963385),
    teen_oop_cost              = c(263.528488245289, 0, 0, 65.8821220613224,
                                   197.646366183967, 65.8821220613224, 0, 0,
                                   131.764244122645, 0),
    adult_oop_cost             = c(91.9170222484868, 275.75106674546, 183.834044496974,
                                   91.9170222484868, 275.75106674546, 275.75106674546,
                                   183.834044496974, 643.419155739408, 183.834044496974,
                                   275.75106674546),
    total_oop_cost             = c(457.735906621424, 425.876287955088, 357.03802737057,
                                   295.935206560243, 569.143948583778, 397.962723029431,
                                   218.050306669555, 663.769911465772, 419.044101847163,
                                   401.906418486767),
    transportation_cost        = c(368.281052619231, 724.811909866424, 702.338358068173,
                                   368.704181495322, 692.749557903065, 733.783514543967,
                                   708.039462924974, 703.397771000273, 730.344399424999,
                                   702.300181042627),

    # Subtotals and misc costs
    subtotal1                  = c(5674.14514345894, 7809.2186289955, 7252.02378918512,
                                   6967.63814371403, 7466.88768554386, 5067.2855145617,
                                   3735.81049072898, 6049.51346232768, 6958.36393840585,
                                   6989.97115575548),
    other_necessities          = c(567.414514345894, 780.92186289955, 725.202378918513,
                                   696.763814371403, 746.688768554386, 506.72855145617,
                                   373.581049072898, 604.951346232768, 695.836393840585,
                                   698.997115575548),
    broadband_cost             = rep(73.995993981, 10),
    phone_cost                 = rep(35.6155842587778, 10),
    phone_and_broadband_cost   = c(109.611578239778, 180.842746757333, 145.227162498556,
                                   109.611578239778, 180.842746757333, 180.842746757333,
                                   145.227162498556, 323.305083792444, 145.227162498556,
                                   180.842746757333),
    subtotal2                  = c(6351.17123604461, 8770.98323865238, 8122.45333060219,
                                   7774.0135363252, 8394.41920085558, 5754.85681277521,
                                   4254.61870230044, 6977.76989235289, 7799.42749474498,
                                   7869.81101808836),
    sales_tax_monthly          = c(39.3955897310354, 54.2194049411158, 50.3508011683123,
                                   48.3763116318065, NA, NA, NA, NA, NA, NA),
    grocery_tax_monthly        = rep(0, 10),
    sales_grocery_tax_annual   = c(472.747076772425, 650.632859293389, 604.209614019748,
                                   580.515739581678, NA, NA, NA, NA, NA, NA),
    subtotal3                  = c(6390.56682577565, 8825.2026435935, 8172.80413177051,
                                   7822.38984795701, 8446.26180205631, 5790.03897610281,
                                   4280.55643453757, 7019.77166432183, 7847.73941556934,
                                   7918.34238782277),

    stringsAsFactors = FALSE
  )
}

# matching mock data
YEAR <- 2026

# Testing output structure and shape
test_that("output has same number of rows as input", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  expect_equal(nrow(out), nrow(df))
})

test_that("output has more columns than input", {
    df  <- create_mock_df()
    out <- solve_starting_income_iterative(df, year = YEAR)
    expect_true(ncol(out) > ncol(df))
})

test_that("transitory columns are removed from output", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  
  dropped <- c("previous_income", "new_starting_income", "income_diff", "row_converged")
  for (col in dropped) {
    expect_false(col %in% names(out), label = paste("column present:", col))
  }
})

test_that("subtotal3 is retained in output", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  expect_true("subtotal3" %in% names(out))
})

test_that("all original input columns are retained", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  for (col in names(df)) {
    expect_true(col %in% names(out), label = paste("missing column:", col))
  }
})

test_that("output contains new columns for federal tax calculations", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  
  expected_cols <- c("starting_income", "iteration_count", "converged", "final_income_diff",
                     "total_fed_payroll_tax", "federal_cumulative_tax", "eitc_credit",
                     "cdctc_credit", "ctc_credit", "total_taxes", "total_credits")
  
  for (col in expected_cols) {
    expect_true(col %in% names(out), label = paste("missing column:", col))
  }
})

# Testing convergence under normal conditions
test_that("all rows converge under normal conditions", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  expect_true(all(out$converged))
})

test_that("iteration_count is between 1 and max_iterations when converged", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  
  expect_true(all(out$iteration_count >= 1))
  expect_true(all(out$iteration_count <= 100))
})

test_that("starting_income is positive after convergence", {
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR)
  expect_true(all(out$starting_income > 0))
})

test_that("final_income_diff is below tolerance for converged rows", {
  tol <- 1.0
  df  <- create_mock_df()
  out <- solve_starting_income_iterative(df, year = YEAR, tolerance = tol)
  
  converged_rows <- out[out$converged, ]
  expect_true(all(converged_rows$final_income_diff < tol))
})

# Testing non-convergence handling
test_that("non-converged rows trigger a warning", {
  df <- create_mock_df()
  expect_warning(
    solve_starting_income_iterative(df, year = YEAR, max_iterations = 2, tolerance = 0.0001),
    regexp = "did not converge"
  )
})

test_that("non-converged rows get fallback starting_income = subtotal3 * 1.20 * 12", {
  df <- create_mock_df()
  
  # Suppressing warnings for clean output
  out <- withCallingHandlers(
    solve_starting_income_iterative(df, year = YEAR, max_iterations = 2, tolerance = 0.0001),
    warning = function(w) invokeRestart("muffleWarning")
  )
  
  non_conv <- out[!out$converged, ]
  if (nrow(non_conv) > 0) {
    expected_fallback <- non_conv$subtotal3 * 1.20 * 12
    expect_equal(non_conv$starting_income, expected_fallback, tolerance = 0.01)
  } else {
    skip("All rows converged — reduce max_iterations or tolerance to test fallback")
  }
})

test_that("non-converged rows have iteration_count == max_iterations", {
  max_iter <- 2
  df  <- create_mock_df()
  out <- withCallingHandlers(
    solve_starting_income_iterative(df, year = YEAR, max_iterations = max_iter, tolerance = 0.0001),
    warning = function(w) invokeRestart("muffleWarning")
  )
  
  non_conv <- out[!out$converged, ]
  if (nrow(non_conv) > 0) {
    expect_true(all(non_conv$iteration_count == max_iter))
  } else {
    skip("All rows converged — tighten tolerance to test this path")
  }
})

test_that("rows that converge do not have fallback starting_income", {
  df  <- create_mock_df()
  out <- withCallingHandlers(
    solve_starting_income_iterative(df, year = YEAR, max_iterations = 3, tolerance = 0.01),
    warning = function(w) invokeRestart("muffleWarning")
  )
  
  converged_rows <- out[out$converged, ]
  if (nrow(converged_rows) > 0) {
    expect_false(any(converged_rows$starting_income == converged_rows$subtotal3 * 1.20 * 12))
  } else {
    skip("All rows non-converged — increase max_iterations or loosen tolerance to test this")
}})

test_that("warning message reports the correct non-convergence percentage", {
  df <- create_mock_df()

  # looking for a percentage in the warning message, e.g. "80.00%"
  w_msg <- tryCatch(
    solve_starting_income_iterative(df, year = YEAR, max_iterations = 2, tolerance = 0.0001),
    warning = function(w) conditionMessage(w)
  )

  expect_match(w_msg, "\\d+\\.\\d{2}%")
})

# Testing function stability
test_that("results are identical across two runs with same input", {
  df   <- create_mock_df()
  out1 <- solve_starting_income_iterative(df, year = YEAR)
  out2 <- solve_starting_income_iterative(df, year = YEAR)
  expect_equal(out1$starting_income, out2$starting_income)
  expect_equal(out1$iteration_count, out2$iteration_count)
})

test_that("tighter tolerance requires more iterations", {
  df       <- create_mock_df()
  out_loose <- solve_starting_income_iterative(df, year = YEAR, tolerance = 10.0)
  out_tight <- solve_starting_income_iterative(df, year = YEAR, tolerance  = 0.01)
  
  expect_true(mean(out_tight$iteration_count) >= mean(out_loose$iteration_count))
})

# Fun little edge case
test_that("single-row input converges and returns one row", {
  df  <- create_mock_df()[1, ]
  out <- solve_starting_income_iterative(df, year = YEAR)
  expect_equal(nrow(out), 1)
  expect_true(out$converged)
})
