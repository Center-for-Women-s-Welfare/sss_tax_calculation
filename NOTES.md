# Fork Notes

This repository (`sssTaxCalculationSV`) is a fork of
[`Center-for-Women-s-Welfare/sss_tax_calculation`](https://github.com/Center-for-Women-s-Welfare/sss_tax_calculation)
(package `sssTaxCalculation`).

- **Forked from commit:** `868ac7fc07d8ae6ceb3ccd2609df1fe5e68cef47` (upstream `main`, dated 2026-07-14)
- **Fork date:** 2026-08-13
- **Reason for fork:** The Silicon Valley localized SSS project (`sv_localized_sss`)
  needs to support senior and disabled-adult households whose income is
  partly or fully unearned (Social Security/SSDI plus gap-filling
  retirement/other income), rather than the wages-only assumption the
  upstream solver makes. This fork adds earned/unearned income support
  (`n_adults` / `n_earning_adults`) to the payroll tax and EITC logic while
  remaining backward-compatible with every existing working-age-only caller.

Upstream `sss_tax_calculation` and `sss_production` are intentionally left
untouched by this work — this fork exists so SV-specific changes don't affect
other states' SSS calculations. Periodically diff against upstream `main` to
check for changes worth pulling in.
