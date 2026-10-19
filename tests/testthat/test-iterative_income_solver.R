# test-iterative_income_solver.R
# Unit tests for the solve_starting_income_iterative function

library(testthat)

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
    child_bedrooms             = c(3, 3, 3, 3, 3, 1, 1, 3, 3, 2),
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
    stringsAsFactors = FALSE
  )
}
