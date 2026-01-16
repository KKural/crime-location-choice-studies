# Clean SUoA dataset by removing supporting and reasoning columns
# Keeps only the main variables for analysis

# Source functions
source(here::here("Script", "Functions.R"))

library(openxlsx)
library(dplyr)
library(here)

# File paths - Using CSV to avoid Excel lock issues
input_file <- here("Data", "20260106_working.csv")
output_file <- here("Data", "20260113_cleaned.xlsx")
csv_file <- here("Data", "20260113_cleaned.csv")

# Check if file exists
if (!file.exists(input_file)) {
  stop("ERROR: Input file not found: ", input_file)
}

cat("Reading file:", input_file, "\n\n")

# Read CSV (no lock issues)
df <- read.csv(input_file, stringsAsFactors = FALSE)

cat("\nOriginal dataset shape:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Total columns:", ncol(df), "\n")

# Identify columns to remove (containing 'supporting' or 'reasoning')
columns_to_remove <- grep("supporting|reasoning|Supporting|Reasoning", 
                          names(df), 
                          value = TRUE, 
                          ignore.case = TRUE)

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("COLUMNS TO REMOVE (", length(columns_to_remove), "):\n", sep = "")
cat(paste(rep("=", 80), collapse = ""), "\n")
for (i in seq_along(columns_to_remove)) {
  cat(sprintf("%3d. %s\n", i, columns_to_remove[i]))
}

# Remove the columns
df_cleaned <- df %>% select(-all_of(columns_to_remove))

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("REMAINING COLUMNS (", ncol(df_cleaned), "):\n", sep = "")
cat(paste(rep("=", 80), collapse = ""), "\n")
for (i in seq_along(names(df_cleaned))) {
  cat(sprintf("%3d. %s\n", i, names(df_cleaned)[i]))
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Original columns: ", ncol(df), "\n", sep = "")
cat("Removed columns:  ", length(columns_to_remove), "\n", sep = "")
cat("Remaining columns:", ncol(df_cleaned), "\n", sep = "")
cat("Rows:            ", nrow(df_cleaned), "\n", sep = "")

# Save cleaned dataset
cat("\nSaving cleaned dataset to:", output_file, "\n")
write.xlsx(df_cleaned, output_file)

# Also save as CSV for easier inspection
cat("Also saving as CSV:", csv_file, "\n")
write.csv(df_cleaned, csv_file, row.names = FALSE)

cat("\n✅ CLEANING COMPLETE!\n")
cat("\nNext steps:\n")
cat("1. Review the cleaned file:", output_file, "\n")
cat("2. Verify all necessary variables are retained\n")
cat("3. Begin analysis with the clean dataset\n")
