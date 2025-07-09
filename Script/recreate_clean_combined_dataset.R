# RECREATE CLEAN COMBINED DATASET FROM ELICIT OUTPUTS
# This script combines multiple Elicit CSV files into a single clean dataset

library(dplyr)
library(stringr)
library(readr)

cat("=== RECREATING CLEAN COMBINED DATASET FROM ELICIT OUTPUTS ===\n")

# Define the Elicit files to combine
elicit_files <- c(
  "Elicit - New table - BASIC STUDY IDENTIFICATION TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - DESCRIPTION _ JUSTIFICATION STUDY CONTEXT _ G.csv",
  "Elicit - New table - THEORETICAL FRAMEWORK _ OBJECTIVES STUDY DESIGN _ METHODOLOGY DATA PREPARATION _ PROCESSING DEMOGRAPHIC _ SOCIAL VARIA.csv",
  "Elicit - New table - TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS MAJOR FINDINGS _ RESULTS ENVIRONMENTAL _ CRIME ATTRACTOR .csv",
  "Elicit - New table - SCALE EFFECTS _ SPATIAL FINDINGS DATA LIMITATIONS _ METHODOLOGICAL ISSUES GENERALIZABILITY _ COMPARATIVE LIMITATIONS I.csv"
)

# Check which files exist
existing_files <- character()
for (file in elicit_files) {
  if (file.exists(file)) {
    existing_files <- c(existing_files, file)
    cat("Found:", file, "\n")
  } else {
    cat("Missing:", file, "\n")
  }
}

if (length(existing_files) == 0) {
  stop("No Elicit files found! Please check file paths.")
}

# Function to clean column names
clean_column_names <- function(df) {
  # Keep only the main content columns, not the supporting quotes/tables/reasoning
  main_cols <- colnames(df)[!grepl("Supporting|Reasoning", colnames(df))]
  return(df[, main_cols])
}

# Function to standardize basic metadata columns
standardize_metadata <- function(df) {
  # Ensure consistent metadata columns
  required_cols <- c("Title", "Authors", "DOI", "Year", "Venue", "Citation count")
  
  for (col in required_cols) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA
    }
  }
  
  return(df)
}

# Read and process first file to establish the base structure
cat("\nProcessing base file:", existing_files[1], "\n")
base_df <- read_csv(existing_files[1], show_col_types = FALSE)
base_df <- clean_column_names(base_df)
base_df <- standardize_metadata(base_df)

cat("Base file dimensions:", nrow(base_df), "rows x", ncol(base_df), "columns\n")

# Get the content columns (excluding metadata)
metadata_cols <- c("Title", "Authors", "DOI", "DOI link", "Venue", "Citation count", "Year")
base_content_cols <- setdiff(colnames(base_df), metadata_cols)

cat("Content columns in base file:", length(base_content_cols), "\n")
cat("Base content columns:", paste(base_content_cols, collapse = ", "), "\n")

# Process additional files and merge by Title
if (length(existing_files) > 1) {
  for (i in 2:length(existing_files)) {
    file <- existing_files[i]
    cat("\nProcessing additional file:", file, "\n")
    
    additional_df <- read_csv(file, show_col_types = FALSE)
    additional_df <- clean_column_names(additional_df)
    additional_df <- standardize_metadata(additional_df)
    
    cat("Additional file dimensions:", nrow(additional_df), "rows x", ncol(additional_df), "columns\n")
    
    # Get new content columns
    additional_content_cols <- setdiff(colnames(additional_df), metadata_cols)
    cat("New content columns:", length(additional_content_cols), "\n")
    cat("Additional content columns:", paste(additional_content_cols, collapse = ", "), "\n")
    
    # Select only Title and new content columns for merging
    merge_cols <- c("Title", additional_content_cols)
    additional_subset <- additional_df[, merge_cols]
    
    # Merge with base dataset
    base_df <- merge(base_df, additional_subset, by = "Title", all.x = TRUE, all.y = TRUE)
    cat("After merge:", nrow(base_df), "rows x", ncol(base_df), "columns\n")
  }
}

# Clean up the final dataset
cat("\nCleaning final dataset...\n")

# Remove duplicate columns if any
base_df <- base_df[, !duplicated(colnames(base_df))]

# Reorder columns - put Title first, then content columns
title_col <- "Title"
metadata_cols_present <- intersect(metadata_cols, colnames(base_df))
content_cols_present <- setdiff(colnames(base_df), metadata_cols_present)

# Final column order: Title, then all content columns
final_col_order <- c(title_col, content_cols_present)
base_df <- base_df[, final_col_order]

# Remove rows where Title is missing
base_df <- base_df[!is.na(base_df$Title) & base_df$Title != "", ]

# Clean up Title column - remove leading/trailing whitespace
base_df$Title <- str_trim(base_df$Title)

# Replace empty strings with NA for consistency
base_df[base_df == ""] <- NA

# Remove completely empty rows
base_df <- base_df[rowSums(!is.na(base_df)) > 1, ]

# Sort by Title for consistency
base_df <- base_df[order(base_df$Title), ]

cat("\nFinal dataset preparation complete!\n")
cat("Final dimensions:", nrow(base_df), "rows x", ncol(base_df), "columns\n")

# Show column names
cat("\nFinal column structure:\n")
for (i in 1:ncol(base_df)) {
  col_name <- colnames(base_df)[i]
  non_na_count <- sum(!is.na(base_df[[col_name]]))
  cat(sprintf("%2d. %-50s (%d non-NA values)\n", i, col_name, non_na_count))
}

# Save the clean combined dataset
output_file <- "Output/clean_combined_dataset_recreated.csv"
write_csv(base_df, output_file)

cat("\n=== CLEAN COMBINED DATASET CREATED SUCCESSFULLY ===\n")
cat("Output file:", output_file, "\n")
cat("Dataset contains", nrow(base_df), "studies with", ncol(base_df), "columns\n")

# Show a sample of the data
cat("\n=== SAMPLE DATA ===\n")
sample_cols <- c("Title", head(content_cols_present, 3))
available_sample_cols <- intersect(sample_cols, colnames(base_df))
print(head(base_df[, available_sample_cols], 3))

cat("\n=== RECREATION COMPLETE ===\n")
cat("The clean_combined_dataset.csv has been successfully recreated from Elicit outputs!\n")
