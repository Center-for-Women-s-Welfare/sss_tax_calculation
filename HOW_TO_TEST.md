# How to Test the Iterative Solver

## Quick Test

The solver is now integrated into the main SSS pipeline. To test it:

### 1. Run the Full Pipeline

```r
# Set your working directory
setwd("C:/Users/Lisa/Desktop/local_dev")  # Or your SSS_CODE_BASE path

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
- Iteration progress every 10 iterations
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

## Expected Performance

- **1 county (719 family types)**: 3-5 seconds
- **Convergence rate**: >99.9%
- **Average iterations**: 15-25 iterations

## Validation

Compare results with previous Excel-based calculations:

```r
# Load Excel baseline (if available)
excel_results <- read.csv("path/to/excel_baseline.csv")

# Compare starting_income
comparison <- calculations_df %>%
  left_join(excel_results, by = c("county_table_number", "family_type")) %>%
  mutate(
    diff = starting_income - excel_starting_income,
    within_tolerance = abs(diff) <= 1
  )

# Check match rate
sum(comparison$within_tolerance) / nrow(comparison) * 100  # Should be >99%
```

## Next Steps

Once testing is successful:
1. Set `debug = FALSE` in `calculate_taxes.R` for production runs
2. Process additional counties/states
3. Document any edge cases or non-convergent patterns
4. Consider Phase 2: Adding state tax calculations
