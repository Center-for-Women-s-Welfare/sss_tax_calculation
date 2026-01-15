# sss_tax_calculation

An R package for calculating the minimum starting income needed for families to achieve self-sufficiency across all US counties, accounting for federal taxes and tax credits.

## Overview

The Self-Sufficiency Standard (SSS) Tax Calculation Engine provides a modern, reproducible alternative to Excel-based circular calculations. It computes the minimum gross income required for 719 different family configurations to meet basic needs without public or private assistance.

### What is the Self-Sufficiency Standard?

The Self-Sufficiency Standard measures the income families need to meet basic needs (housing, childcare, food, healthcare, transportation, etc.) at the county level. This engine solves the complex recursive tax calculation problem where:

- Income determines tax liability
- Tax credits adjust net income  
- Adjusted income affects eligibility for other credits/deductions
- Creating a feedback loop requiring iterative convergence

## Features

### Phase 1: Federal Taxes (Current Implementation)

-  **Federal Income Tax**: Progressive bracket calculations with filing status support
-  **Federal Payroll Taxes**: Social Security and Medicare (including additional Medicare tax)
-  **EITC**: Earned Income Tax Credit with phase-in/phase-out
-  **CDCTC**: Child and Dependent Care Tax Credit
-  **CTC**: Child Tax Credit (refundable and non-refundable portions)
-  **Iterative Solver**: Convergence detection with $1 tolerance
-  **Fallback Logic**: Handles edge cases where convergence fails
-  **719 Family Types**: All household permutations (1-2 adults, 0-4 children by age)

## Installation

```r
# Install from GitHub
devtools::install_github("Center-for-Women-s-Welfare/sss_tax_calculation")

# For development
git clone https://github.com/Center-for-Women-s-Welfare/sss_tax_calculation.git
cd sss_tax_calculation
```

## Quick Start

```r
library(SSSTax)

# Load your basic needs data
basic_needs <- read.csv("county_basic_needs.csv")

# Calculate starting income (federal taxes only in Phase 1)
# Note: 'state' parameter is optional and not currently used in Phase 1
result <- calculate_sss_income(
  basic_needs_df = basic_needs,
  year = 2026
)

# View results
head(result[, c("family_type", "countyname", "calculated_starting_income")])
```

## Usage Examples

### Basic Calculation

```r
# Process a single county's data
result <- calculate_sss_income(
  basic_needs_df = iowa_county_data,
  year = 2026
)
```

### Processing Multiple Counties

```r
# Split by county and process
counties <- split(state_data, state_data$countyname)

results <- lapply(counties, function(county_df) {
  calculate_sss_income(county_df, year = 2026)
})

# Combine results
final_results <- do.call(rbind, results)
```

### Debug Mode

```r
# Enable detailed diagnostic output
result <- calculate_sss_income(
  basic_needs_df = problem_data,
  year = 2026,
  debug = TRUE
)

# Check convergence
non_converged <- result[!result$converged, ]
print(non_converged[, c("family_type", "iteration_count")])
```

### Custom Parameters

```r
# Adjust convergence settings
result <- calculate_sss_income(
  basic_needs_df = basic_needs,
  year = 2026,
  tolerance = 0.5,        # $0.50 tolerance
  max_iterations = 150    # Allow more iterations
)
```

## Function Parameters

The main function signature is:

```r
calculate_sss_income(
  basic_needs_df,           # Required: Input dataframe with 58 required columns
  year,                     # Required: Tax year (e.g., 2026)
  state = NULL,             # Optional: State code - NOT CURRENTLY USED in Phase 1
  max_iterations = 100,     # Optional: Maximum solver iterations
  tolerance = 1.0,          # Optional: Convergence tolerance in dollars
  debug = FALSE             # Optional: Enable detailed diagnostic output
)
```

**Important Note about the `state` Parameter:**
- **Phase 1 (Current)**: The `state` parameter is included in the function signature for backward compatibility but is **not currently used**. All calculations are federal-only.
- **Phase 2 (Future)**: The `state` parameter will be required when state tax calculations are implemented.
- **Recommendation**: You can safely omit the `state` parameter in Phase 1, or include it for future-proofing your code.

## Input Requirements

### Required Input Columns (58 total)

Your `basic_needs_df` must include:

**Family Identification:**
- `family_type`: Family configuration code (e.g., "a1i1p0s0t0")
- `adult`, `infant`, `presch`, `school`, `teen`: Household composition
- `children`: Total number of children
- `household_type`: "single_adult", "single_parent", or "married"

**Geographic Identifiers:**
- `stusps`: State postal code (e.g., "IA")
- `countyname`: County name
- `county_table_number`: County table identifier

**Monthly Cost Components:**
- `housing_cost`: Monthly housing cost
- `child_care_cost`: Monthly childcare cost
- `food_cost`: Monthly food cost
- `health_ins_premium`: Monthly health insurance premium
- `*_oop_cost`: Out-of-pocket health costs by age
- `transportation_cost`: Monthly transportation cost

**Cost Subtotals:**
- `subtotal2`: Monthly total before taxes
- `subtotal3`: Monthly total used in fallback calculation

**Note:** See full schema in LLD documentation for all 58 required columns.

## Output Schema

Returns input dataframe with **18 additional columns**:

**Federal Payroll Taxes:**
- `ss_tax`: Social Security tax
- `medicare_tax`: Medicare tax
- `total_fed_payroll_tax`: Total federal payroll taxes

**Federal Income Tax:**
- `total_fed_deductions`: Total federal deductions
- `taxable_income`: Federal taxable income
- `federal_income_tax`: Federal income tax liability

**Federal Tax Credits:**
- `eitc_credit`: Earned Income Tax Credit
- `cdctc_credit`: Child and Dependent Care Credit
- `ctc_credit`: Child Tax Credit (total)
- `ctc_nonrefundable`: Non-refundable CTC portion
- `ctc_refundable`: Refundable CTC portion

**Calculation Results:**
- `starting_income`: Calculated annual gross income needed
- `iteration_count`: Number of iterations to convergence
- `converged`: TRUE if converged, FALSE if fallback used

## Repository Structure

```
sss_tax_calculations/
├── src/
│   ├── data/
│   │   └── federal/              # Federal tax parameters by year
│   │       └── 2026/
│   │           ├── tax_fed_credits_df.csv
│   │           ├── tax_fed_income_brackets_df.csv
│   │           ├── tax_fed_payroll_df.csv
│   │           └── tax_fed_sd_df.csv
│   ├── calculations/
│   │   ├── tax_calculation.R     # Main entry point
│   │   ├── federal_taxes.R       # Federal tax components
│   │   └── iterative_solver.R    # Convergence logic
│   └── utils/
│       ├── data_validation.R     # Input validation
│       ├── data_loader.R         # Load tax parameters
│       └── helpers.R             # Utility functions
├── tests/                         # Test suite
│   ├── test_tax_calculation.R
│   ├── test_federal_taxes.R
│   ├── test_iterative_solver.R
│   └── test_validation.R
├── LLD.md                        # Low-Level Design document
└── README.md                     # This file
```

## Algorithm Overview

The engine uses an iterative solver to handle the recursive nature of tax calculations:

1. **Initialize**: Start with `subtotal3 * 1.20 * 12`
2. **Iterate** (up to 100 times):
   - Calculate federal payroll taxes
   - Calculate federal income tax
   - Calculate EITC (income-based)
   - Calculate CDCTC (tax liability-based)
   - Calculate CTC (income and tax-based)
   - Compute new starting income
   - Check convergence ($1 tolerance)
3. **Converge or Fallback**: Exit when stable or apply fallback

### Convergence Criteria

- **Tolerance**: $1 absolute difference between iterations
- **Max Iterations**: 100 (prevents infinite loops)
- **Success Rate**: >99.9% convergence in testing

## Testing

### Run Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/test_federal_taxes.R")
```

### Coverage Requirements

- **Line Coverage**: ≥80%
- **Branch Coverage**: ≥70%

### Test Categories

-  Unit tests for each tax component
-  Integration tests for full calculation flow
-  Input validation tests

## Tax Parameter Updates

Tax parameters are stored in CSV files organized by year:

```
src/data/federal/2026/
├── tax_fed_credits_df.csv       # EITC, CDCTC, CTC parameters
├── tax_fed_income_brackets_df.csv  # Federal tax brackets
├── tax_fed_payroll_df.csv       # Social Security, Medicare rates
└── tax_fed_sd_df.csv            # Standard deductions
```

### Annual Updates

Tax laws change annually. To update for a new year:

1. Create new directory: `src/data/federal/{year}/`
2. Populate with updated CSV files following existing schema
3. Update tests with new expected values
4. Run validation suite

## Troubleshooting

### Non-Convergence Issues

If families fail to converge:

```r
# Check which families didn't converge
non_converged <- result[!result$converged, ]
View(non_converged)

# Enable debug mode for detailed trace
result <- calculate_sss_income(
  basic_needs_df = df, 
  year = 2026, 
  debug = TRUE
)
```

### Common Issues

**Missing Required Columns**
```r
# Error: Required column 'subtotal3' not found
# Solution: Ensure all 58 required columns are present
names(basic_needs_df)  # Check column names
```

**Invalid Household Type**
```r
# Error: Invalid household_type values found
# Solution: Must be 'single_adult', 'single_parent', or 'married'
unique(basic_needs_df$household_type)
```

**Negative Cost Values**
```r
# Error: Negative values in 'housing_cost'
# Solution: All cost values must be >= 0
summary(basic_needs_df[, grep("_cost", names(basic_needs_df))])
```

## Family Configuration Guide

The engine supports **719 family types** defined by:

- **Adults**: 1 or 2
- **Infants** (0-2 years): 0-4
- **Preschoolers** (3-5 years): 0-4
- **School-age** (6-12 years): 0-4
- **Teenagers** (13-17 years): 0-4

**Example family codes:**
- `a1i0p0s0t0`: Single adult, no children
- `a1i1p0s0t0`: Single parent with 1 infant
- `a2i0p0s2t0`: Married couple with 2 school-age children
- `a2i1p1s1t1`: Married couple with 4 children (1 of each age group)

## Contributing

We welcome contributions! Please:

1. Fork the repository at https://github.com/Center-for-Women-s-Welfare/sss_tax_calculation
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines

- Maintain test coverage ≥80%
- Document all functions
- Update tests for any new features
