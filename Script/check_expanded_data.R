# Check the expanded dataset structure and values
library(readr)

# Read the expanded dataset
expanded_data <- read_csv("Output/simple_expanded_dataset.csv")

# Check dimensions
cat("Expanded dataset dimensions:", nrow(expanded_data), "rows x", ncol(expanded_data), "columns\n\n")

# Check the first few columns related to basic study identification
basic_cols <- grep("BASIC_STUDY_IDENTIFICATION_", names(expanded_data), value = TRUE)
cat("Basic study identification columns found:", length(basic_cols), "\n")
cat("Column names:", head(basic_cols, 10), "\n\n")

# Check the first article's extracted values
cat("First article's extracted basic information:\n")
for(col in head(basic_cols, 6)) {
  cat(col, ":", expanded_data[[col]][1], "\n")
}

# Check if we have any empty/NA values in key columns
cat("\nChecking for missing values in key columns (first 10 rows):\n")
for(col in head(basic_cols, 6)) {
  missing_count <- sum(is.na(expanded_data[[col]][1:10]) | expanded_data[[col]][1:10] == "" | is.null(expanded_data[[col]][1:10]))
  cat(col, "- Missing values:", missing_count, "out of 10\n")
}

# Check a few more articles
cat("\nSecond article's Title and Year:\n")
title_col <- "BASIC_STUDY_IDENTIFICATION_Title"
year_col <- "BASIC_STUDY_IDENTIFICATION_Year"
if(title_col %in% names(expanded_data)) {
  cat("Title:", expanded_data[[title_col]][2], "\n")
}
if(year_col %in% names(expanded_data)) {
  cat("Year:", expanded_data[[year_col]][2], "\n")
}
