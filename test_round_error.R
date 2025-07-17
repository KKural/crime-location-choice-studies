# Test script to identify the source of the round() error
library(readr)
library(dplyr)

# Load the analysis script to get all the objects
source("20250714_new_csv_analysis_clean.R")

# Function to safely test round() calls
safe_round <- function(x, digits = 0) {
  cat("Testing value:", deparse(substitute(x)), "\n")
  cat("Class:", class(x), "\n")
  cat("Value:", x, "\n")
  cat("Is numeric:", is.numeric(x), "\n")
  cat("Is NA:", is.na(x), "\n")
  cat("Is NaN:", is.nan(x), "\n")
  
  if(is.numeric(x) && !is.na(x) && !is.nan(x)) {
    result <- round(x, digits)
    cat("Round result:", result, "\n")
    return(result)
  } else {
    cat("ERROR: Cannot round this value!\n")
    return(NA)
  }
  cat("---\n")
}

# Test all the round() calls from the Rmd
cat("Testing round() calls from the Rmd setup chunk:\n\n")

# Test spatial_stats values
cat("=== Testing spatial_stats values ===\n")
if(exists("spatial_stats")) {
  safe_round(as.numeric(spatial_stats$Mean_unit_size), 3)
  safe_round(as.numeric(spatial_stats$SD_unit_size), 3)
  safe_round(log10(as.numeric(spatial_stats$Max_unit_size) / as.numeric(spatial_stats$Min_unit_size)), 1)
} else {
  cat("spatial_stats does not exist!\n")
}

# Test other calculations that use round()
cat("\n=== Testing other round() calls ===\n")
if(exists("data")) {
  safe_round(mean(data$Year > 2010, na.rm = TRUE) * 100, 0)
  
  # Test size category stats
  size_category_stats <- data %>%
    count(Size_category, .drop = FALSE) %>%
    mutate(percentage = n / sum(n) * 100)
  
  safe_round(size_category_stats$percentage[1], 0)
}
