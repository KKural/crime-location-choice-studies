# Test script to debug the round() error
library(readr)
library(dplyr)

# Load the data
input_file <- "20250716_Analysis & Results/20250716_standardized_unit_sizes_with_groups_new.csv"
data_raw <- read_csv(input_file, show_col_types = FALSE)

# Check limitation columns
limitation_cols <- grep("limitation", names(data_raw), ignore.case = TRUE, value = TRUE)
cat("Found limitation columns:", paste(limitation_cols, collapse = ", "), "\n")

# Try the limitation calculation
if(length(limitation_cols) > 0) {
  limitation_data <- data_raw[limitation_cols]
  cat("Sample limitation data:\n")
  print(head(limitation_data[1:2]))
  
  # Try the improved conversion
  limitation_logical <- lapply(limitation_data, function(x) {
    if(is.logical(x)) {
      x
    } else if(is.character(x)) {
      # Count non-empty, non-NA text as limitation present
      !is.na(x) & trimws(x) != "" & trimws(x) != "NA" & trimws(x) != "Not mentioned"
    } else if(is.numeric(x)) {
      !is.na(x) & x > 0
    } else {
      rep(FALSE, length(x))
    }
  })
  
  limitation_logical <- as.data.frame(limitation_logical)
  limitation_counts <- rowSums(limitation_logical, na.rm = TRUE)
  mean_lim_value <- mean(limitation_counts, na.rm = TRUE)
  
  cat("Mean limitation value:", mean_lim_value, "\n")
  cat("Is numeric:", is.numeric(mean_lim_value), "\n")
  cat("Is NA:", is.na(mean_lim_value), "\n")
  cat("Is NaN:", is.nan(mean_lim_value), "\n")
  
  # Test rounding
  if(is.numeric(mean_lim_value) && !is.na(mean_lim_value) && !is.nan(mean_lim_value)) {
    final_value <- round(mean_lim_value, 1)
    cat("Rounded value:", final_value, "\n")
  } else {
    cat("Failed - using fallback\n")
  }
}
