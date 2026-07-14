# ----------------------------------------------------------------------
# WA Working Families Tax Credit 2026 — Validation Script
# 
# Generates a dollar-by-dollar lookup table to validate the 
# apply_state_eitc_style_credit() function in sssTaxCalculation.
# NOT used in production — parameters live in tax_state_eitc_params.csv
#
# Source: https://dor.wa.gov/about/news-releases/2026/working-families-tax-credit-application-window-opens-feb-1
# RCW 82.08.0206
# ----------------------------------------------------------------------

library(dplyr)

# 2026 credit parameters
wftc_params <- tibble::tribble(
  ~children, ~max_credit, ~phase_out_rate, ~phase_out_start_single, ~phase_out_start_married, ~phase_out_end_single, ~phase_out_end_married,
  0,         335,         0.18,            16604,                    23714,                    19104,                 26214,
  1,         660,         0.12,            45434,                    52554,                    50434,                 57554,
  2,         995,         0.15,            52310,                    59430,                    57310,                 64430,
  3,         1330,        0.18,            56555,                    63675,                    61555,                 68675
)

# Generate lookup table
generate_wftc_lookup <- function(params, income_step = 100) {
  
  purrr::map_dfr(c("single", "married"), function(fs) {
    purrr::map_dfr(0:3, function(children) {
      
      p <- params %>% filter(children == !!children)
      
      phase_out_start <- if (fs == "single") p$phase_out_start_single else p$phase_out_start_married
      phase_out_end   <- if (fs == "single") p$phase_out_end_single   else p$phase_out_end_married
      max_credit      <- p$max_credit
      phase_out_rate  <- p$phase_out_rate
      
      # Income sequence from 0 to just above phase_out_end
      incomes <- seq(0, phase_out_end, by = 1)
      
      credits <- purrr::map_dbl(incomes, function(income) {
        if (income > phase_out_end) {
          credit <- 0
        } else if (income <= phase_out_start) {
          credit <- max_credit
        } else {
          credit <- max_credit - (income - phase_out_start) * phase_out_rate
          credit <- pmax(credit, 0)
          # Apply $50 minimum floor for any positive credit below threshold
          credit <- if (credit > 0 & credit < 50) 50 else credit
        }
        # If credit calculated to zero but income still below threshold, apply $50 floor
        if (credit == 0 & income <= phase_out_end) credit <- 50
        credit
      })
      
      tibble::tibble(
        sss_year      = 2026,
        state         = "WA",
        filing_status = fs,
        children      = children,
        income_min    = incomes,
        income_max    = pmin(c(incomes[-1] - 1, phase_out_end + income_step), phase_out_end),
        credit        = credits
      ) %>%
        filter(credit > 0, income_min <= phase_out_end)
    })
  })
}

wftc_lookup <- generate_wftc_lookup(wftc_params)

# Collapse consecutive same-credit rows into brackets
wftc_lookup_collapsed <- wftc_lookup %>%
  group_by(filing_status, children) %>%
  mutate(
    credit_group = cumsum(credit != lag(credit, default = first(credit)))
  ) %>%
  group_by(filing_status, children, credit_group, credit, sss_year, state) %>%
  summarise(
    income_min = min(income_min),
    income_max = max(income_max),
    .groups = "drop"
  ) %>%
  select(sss_year, state, filing_status, children, income_min, income_max, credit) %>%
  arrange(filing_status, children, income_min)

# Preview size reduction
wftc_lookup_collapsed %>%
  group_by(filing_status, children) %>%
  summarise(n_brackets = n(), .groups = "drop") %>%
  print()

# Preview
wftc_lookup %>% 
  group_by(filing_status, children) %>% 
  summarise(
    min_income = min(income_min),
    max_income = max(income_max),
    max_credit = max(credit),
    n_brackets = n()
  ) %>% 
  print()