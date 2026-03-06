# diagnostics.R
# Convergence diagnostics and logging for SSS Tax Calculation Engine

#' Print Convergence Summary
#'
#' Displays convergence statistics after solver completes
#'
#' @param df Result dataframe with converged column
#' @param debug If TRUE, show additional details
print_convergence_summary <- function(df, debug = FALSE) {
  
  total_rows <- nrow(df)
  converged_count <- sum(df$converged, na.rm = TRUE)
  non_converged_count <- sum(!df$converged, na.rm = TRUE)
  convergence_rate <- converged_count / total_rows * 100
  
  cat("\n=== Convergence Summary ===\n")
  cat("Total rows:", total_rows, "\n")
  cat("Converged:", converged_count, "\n")
  cat("Non-converged:", non_converged_count, "\n")
  cat("Convergence rate:", sprintf("%.2f%%", convergence_rate), "\n")
  
  if (converged_count > 0) {
    converged_rows <- df[df$converged, ]
    avg_iter <- mean(converged_rows$iteration_count, na.rm = TRUE)
    min_iter <- min(converged_rows$iteration_count, na.rm = TRUE)
    max_iter <- max(converged_rows$iteration_count, na.rm = TRUE)
    
    cat("Avg iterations (converged):", round(avg_iter, 1), "\n")
    cat("Min iterations:", min_iter, "\n")
    cat("Max iterations:", max_iter, "\n")
  }
  
  if (debug && non_converged_count > 0) {
    cat("\n=== Non-Converged Rows ===\n")
    non_conv <- df[!df$converged, ]
    
    if ("county_table_number" %in% names(non_conv) && "family_type" %in% names(non_conv)) {
      print(non_conv[, c("county_table_number", "family_type", 
                         "starting_income", "iteration_count")])
    } else {
      cat("Non-converged row indices:", which(!df$converged), "\n")
    }
  }
  
  cat("===========================\n\n")
}

#' Print Iteration Progress
#'
#' Shows progress during iteration loop (debug mode)
#'
#' @param iter Current iteration number
#' @param df Current dataframe state
#' @param show_every Print every N iterations
print_iteration_progress <- function(iter, df, show_every = 10) {
  if (iter %% show_every == 0 || iter == 1) {
    converged_count <- sum(df$converged, na.rm = TRUE)
    total_rows <- nrow(df)
    pct <- converged_count / total_rows * 100
    
    cat(sprintf(
      "Iteration %3d: %d/%d converged (%.1f%%)\n",
      iter, converged_count, total_rows, pct
    ))
  }
}
