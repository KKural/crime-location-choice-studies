# Test script to check if the Rmd can render
# Test if the analysis objects exist and are valid

# First run the analysis script
cat("Running analysis script...\n")
source("20250714_new_csv_analysis_clean.R")

# Check if key objects exist
cat("Checking key objects...\n")
cat("spatial_stats exists:", exists("spatial_stats"), "\n")
cat("data exists:", exists("data"), "\n")
cat("temporal_model exists:", exists("temporal_model"), "\n")

# Check spatial_stats structure
if(exists("spatial_stats")) {
  cat("spatial_stats columns:", paste(names(spatial_stats), collapse = ", "), "\n")
  cat("spatial_stats values:\n")
  print(spatial_stats)
}

# Test a minimal version of the problematic calculations
if(exists("data")) {
  cat("Testing size category calculation...\n")
  
  # Test safe functions first
  safe_round <- function(x, digits = 0) {
    if(is.numeric(x) && !is.na(x) && !is.nan(x) && is.finite(x)) {
      round(x, digits)
    } else {
      0
    }
  }
  
  safe_percentage <- function(numerator, denominator, digits = 1, default = 0) {
    if(is.numeric(numerator) && is.numeric(denominator) && 
       !is.na(numerator) && !is.na(denominator) && 
       denominator != 0) {
      safe_round((numerator / denominator) * 100, digits)
    } else {
      default
    }
  }
  
  # Test the size category calculation
  library(dplyr)
  
  size_category_stats <- data %>%
    count(Size_category, .drop = FALSE) %>%
    mutate(
      total_n = sum(n),
      percentage = safe_percentage(n, total_n, 0)
    )
  
  cat("Size category stats:\n")
  print(size_category_stats)
  
  cat("Test completed successfully!\n")
} else {
  cat("Error: data object not found\n")
}
