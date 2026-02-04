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

## Usage

### In calculate_taxes.R

The solver is integrated into the SSS production pipeline:

```r
# In sss_production/src/2026/analysis/calculate_taxes.R

# Source the iterative solver
source(file.path(sss_code_base(), "sss_tax_calculation", "src", "calculations", "iterative_income_solver.R"))

# Call the solver (loads tax params from CSV automatically)
calculations_df <- solve_starting_income_iterative(
  df = calculations_df,
  year = sss_year,
  state = state,  # For Phase 2 compatibility
  max_iterations = 100,
  tolerance = 1.0,
  debug = TRUE  # Set to FALSE for production
)
```

### Function Signature

```r
solve_starting_income_iterative(
  df,                    # Required: Dataframe with basic needs data
  year,                  # Required: Tax year (e.g., 2026)
  state = NULL,          # Optional: State code (for Phase 2)
  max_iterations = 100,  # Optional: Max iterations before fallback
  tolerance = 1.0,       # Optional: Convergence tolerance in dollars
  debug = FALSE          # Optional: Enable detailed output
)
```

**Key Changes from Original Design:**
- Tax parameters are **loaded automatically** from `src/data/federal/{year}/` CSV files
- No need to pass `tax_params` parameter
- Simpler function signature

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

The solver returns the input dataframe with many additional columns. Here are the key columns to check:

### Convergence Metadata
- `starting_income`: **Converged annual gross income** needed for self-sufficiency
- `iteration_count`: Number of iterations required to reach convergence
- `converged`: TRUE if converged within tolerance, FALSE if fallback was used
- `final_income_diff`: **Absolute income difference in final iteration** - shows how close non-converged rows were to convergence (in dollars)

### Federal Payroll Taxes
- `ss_tax`: Social Security tax amount
- `medicare_tax`: Medicare tax amount (includes additional Medicare tax if applicable)
- `total_fed_payroll_tax`: **Total federal payroll taxes**

### Federal Income Tax
- `fed_sd`: Federal standard deduction amount
- `taxable_income`: Federal taxable income after deductions
- `federal_cumulative_tax`: **Federal income tax before credits**

### Federal Tax Credits
- `eitc_credit`: **Earned Income Tax Credit amount**
- `cdctc_credit`: **Child and Dependent Care Tax Credit amount**
- `ctc_credit`: **Child Tax Credit amount** (total)
- `ctc_nonrefundable`: Non-refundable CTC portion
- `ctc_refundable`: Refundable CTC portion

### Final Tax Calculations
- `final_federal_income_tax`: **Final federal income tax owed** (after all credits, never negative)
- `total_taxes`: Total federal taxes (payroll + income)
- `total_credits`: Total federal credits (EITC + CDCTC + CTC)

### Key Columns to Verify

After running the solver, check these columns:

```r
# 1. Check convergence success
table(result$converged)  # Should be mostly TRUE
summary(result$iteration_count)  # Should average 15-25 iterations

# 2. For non-converged rows, check how close they were
non_converged <- result[!result$converged, ]
if (nrow(non_converged) > 0) {
  summary(non_converged$final_income_diff)  # Shows convergence distance in dollars
  hist(non_converged$final_income_diff, 
       main = "Income Difference for Non-Converged Rows",
       xlab = "Final Income Difference ($)")
}

# 3. Check starting income is reasonable
summary(result$starting_income)  # Should be positive
range(result$starting_income)  # Should be $20k-$300k range

# 3. Verify convergence formula holds
result %>%
  mutate(
    expected_income = (subtotal2 * 12) + total_taxes - total_credits,
    diff = abs(starting_income - expected_income)
  ) %>%
  filter(converged) %>%
  summary(diff)  # Should be < $1 for converged rows

# 4. Check tax components
summary(result$total_fed_payroll_tax)  # Should be positive
summary(result$federal_cumulative_tax)  # Should be >= 0
summary(result$eitc_credit)  # Should be >= 0
summary(result$cdctc_credit)  # Should be >= 0
summary(result$ctc_credit)  # Should be >= 0

# 5. View sample results by family type
result %>%
  select(family_type, countyname, starting_income, total_taxes, total_credits, 
         iteration_count, converged, final_income_diff) %>%
  head(20)
```

## Repository Structure

```
sss_tax_calculation/
├── src/
│   ├── data/
│   │   └── federal/                      # Federal tax parameters by year
│   │       └── 2026/
│   │           ├── tax_fed_credits_df.csv
│   │           ├── tax_fed_income_brackets_df.csv
│   │           ├── tax_fed_payroll_df.csv
│   │           └── tax_fed_sd_df.csv
│   ├── calculations/
│   │   └── iterative_income_solver.R    # Main iterative solver
│   └── utils/
│       ├── diagnostics.R                 # Convergence diagnostics
│       ├── validation.R                  # Input validation
│       ├── tax_functions.R               # Tax calculation helpers
│       └── data_loader.R                 # CSV data loading
├── tests/                                 # Test suite (if needed)
│   ├── requirements.md
│   ├── design.md
│   └── tasks.md
└── README.md                              # This file
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

The solver is now integrated into the main SSS pipeline. To test it:

### 1. Run the Full Pipeline

```r
# Set your working directory
setwd("<path to SSS_CODE_BASE>")  # Or your SSS_CODE_BASE path

# Run the full SSS calculation pipeline
source("sss_production/src/2026/analysis/final_sss_calculations.R")
```

### 2. What to Expect

The solver will run automatically when `calculate_taxes.R` is sourced. You should see:

```
=== Starting Iterative Solver ===
Initial starting_income range: $X - $Y
Max iterations: 100
Tolerance: $ 1

Iteration   1: 0/719 converged (0.0%)
Iteration  10: 450/719 converged (62.6%)
Iteration  20: 710/719 converged (98.7%)

✓ All rows converged at iteration 23

=== Convergence Summary ===
Total rows: 719
Converged: 719
Non-converged: 0
Convergence rate: 100.00%
Avg iterations (converged): 18.5
Min iterations: 3
Max iterations: 23
===========================
```

### 3. Verify Results

After the pipeline completes, check the results:

```r
# View starting income distribution
summary(calculations_df$starting_income)

# Check convergence
table(calculations_df$converged)

# View iteration statistics
summary(calculations_df$iteration_count)

# Check for non-converged rows
non_converged <- calculations_df[!calculations_df$converged, ]
nrow(non_converged)  # Should be 0 or very few

# View sample results
calculations_df %>%
  select(family_type, countyname, starting_income, iteration_count, converged) %>%
  head(20)
```

### 4. Debug Mode

If you encounter issues, enable debug mode in `calculate_taxes.R`:

```r
# In calculate_taxes.R, change line 20:
debug = TRUE  # Shows detailed iteration progress
```

This will print:
- Iteration progress every iterations
- Non-converged row details
- Income ranges and convergence statistics

### 5. Troubleshooting

**If you get "Cannot find tax_functions.R" error:**
- Check that `SSS_CODE_BASE` environment variable is set
- Verify path: `Sys.getenv("SSS_CODE_BASE")`
- Ensure `sss_production` repository exists at that location

**If convergence rate is low (<99%):**
- Check input data for anomalies (negative costs, missing values)
- Review non-converged rows for patterns
- Consider increasing max_iterations or adjusting tolerance

**If processing is slow:**
- Disable debug mode (`debug = FALSE`)
- Check data size (should be ~719 rows per county)
- Ensure you're processing one county at a time

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
