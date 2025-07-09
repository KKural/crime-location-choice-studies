# Multi-column Variable Extraction Script
# This script extracts key-value pairs from ALL columns in the dataset
# and creates a single wide-format dataset with one column per unique key

library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# Read the clean combined dataset
df <- read_csv("Output/clean_combined_dataset.csv")

cat("Original dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Column names:\n")
print(names(df))

# Function to extract key-value pairs from a single text string
extract_key_value_pairs <- function(text) {
  if (is.na(text) || text == "" || text == "N/A" || text == "Not available") {
    return(NULL)
  }
  
  # Initialize result list
  pairs <- list()
  
  # Clean the text first
  text <- str_trim(text)
  
  # Pattern 1: **Key:** Value (markdown bold)
  markdown_matches <- str_extract_all(text, "\\*\\*([^*]+?):\\*\\*\\s*([^\\n-]+?)(?=\\n|$|\\s*-\\s*\\*\\*|\\z)")[[1]]
  if (length(markdown_matches) > 0) {
    for (match in markdown_matches) {
      key_val <- str_match(match, "\\*\\*([^*]+?):\\*\\*\\s*(.+)")
      if (!is.na(key_val[1])) {
        key <- str_trim(key_val[2])
        value <- str_trim(key_val[3])
        if (key != "" && value != "") {
          pairs[[key]] <- value
        }
      }
    }
  }
  
  # Pattern 2: - Key: Value (dash format)
  dash_matches <- str_extract_all(text, "-\\s*([^:]+?):\\s*([^\\n-]+?)(?=\\n|$|\\s*-\\s*|\\z)")[[1]]
  if (length(dash_matches) > 0) {
    for (match in dash_matches) {
      key_val <- str_match(match, "-\\s*([^:]+?):\\s*(.+)")
      if (!is.na(key_val[1])) {
        key <- str_trim(key_val[2])
        value <- str_trim(key_val[3])
        # Clean markdown from key and value
        key <- str_remove_all(key, "\\*\\*")
        value <- str_remove_all(value, "\\*\\*")
        if (key != "" && value != "") {
          pairs[[key]] <- value
        }
      }
    }
  }
  
  # Pattern 3: Key: Value (simple colon format)
  colon_matches <- str_extract_all(text, "([A-Za-z][^:]*?):\\s*([^\\n-]+?)(?=\\n|$|\\s*[A-Za-z][^:]*?:|\\z)")[[1]]
  if (length(colon_matches) > 0) {
    for (match in colon_matches) {
      key_val <- str_match(match, "([A-Za-z][^:]*?):\\s*(.+)")
      if (!is.na(key_val[1])) {
        key <- str_trim(key_val[2])
        value <- str_trim(key_val[3])
        # Clean markdown from key and value
        key <- str_remove_all(key, "\\*\\*")
        value <- str_remove_all(value, "\\*\\*")
        # Skip if key looks like it's part of a different pattern
        if (!str_detect(key, "^\\d+\\.") && !str_detect(key, "^-") && key != "" && value != "") {
          pairs[[key]] <- value
        }
      }
    }
  }
  
  # Pattern 4: Numbered lists (1. Item, 2. Item)
  numbered_matches <- str_extract_all(text, "\\d+\\.\\s*([^\\n]+?)(?=\\n|$|\\s*\\d+\\.|\\z)")[[1]]
  if (length(numbered_matches) > 0) {
    for (i in seq_along(numbered_matches)) {
      match <- numbered_matches[i]
      item <- str_match(match, "\\d+\\.\\s*(.+)")[2]
      if (!is.na(item)) {
        item <- str_trim(str_remove_all(item, "\\*\\*"))
        if (item != "") {
          pairs[[paste0("Item_", i)]] <- item
        }
      }
    }
  }
  
  # Pattern 5: Pipe-separated values (Value1 | Value2 | Value3)
  if (str_detect(text, "\\|")) {
    pipe_items <- str_split(text, "\\|")[[1]]
    if (length(pipe_items) > 1) {
      for (i in seq_along(pipe_items)) {
        item <- str_trim(str_remove_all(pipe_items[i], "\\*\\*"))
        if (item != "" && item != "N/A") {
          pairs[[paste0("Pipe_Item_", i)]] <- item
        }
      }
    }
  }
  
  return(pairs)
}

# Function to extract from all columns except Title
extract_from_all_columns <- function(row_data) {
  all_pairs <- list()
  
  # Skip the Title column (first column) and extract from all other columns
  for (col_name in names(row_data)[-1]) {
    if (!is.na(row_data[[col_name]]) && row_data[[col_name]] != "") {
      pairs <- extract_key_value_pairs(row_data[[col_name]])
      if (!is.null(pairs) && length(pairs) > 0) {
        # Add pairs to the master list
        for (key in names(pairs)) {
          # Use the key directly without column prefix since we want unified columns
          all_pairs[[key]] <- pairs[[key]]
        }
      }
    }
  }
  
  return(all_pairs)
}

# Extract all key-value pairs from all rows
cat("\nExtracting key-value pairs from all columns...\n")
all_extracted_data <- list()

for (i in 1:nrow(df)) {
  if (i %% 100 == 0) cat("Processing row", i, "of", nrow(df), "\n")
  
  row_data <- df[i, ]
  extracted_pairs <- extract_from_all_columns(row_data)
  
  # Start with the original Title from the first column (never extract from it)
  result_row <- list(Title = as.character(row_data$Title))
  
  # Add all extracted pairs, but if there's a "Title" key from extraction, 
  # rename it to avoid overwriting the original
  if (length(extracted_pairs) > 0) {
    for (key in names(extracted_pairs)) {
      if (key == "Title") {
        # Rename extracted title to avoid conflict
        result_row[["Extracted_Title"]] <- extracted_pairs[[key]]
      } else {
        result_row[[key]] <- extracted_pairs[[key]]
      }
    }
  }
  
  all_extracted_data[[i]] <- result_row
}

# Convert to data frame
cat("\nConverting to data frame...\n")

# Get all unique column names across all rows
all_columns <- unique(unlist(lapply(all_extracted_data, names)))
cat("Found", length(all_columns), "unique columns\n")

# Create a matrix to store the data
result_df <- data.frame(matrix(NA, nrow = length(all_extracted_data), ncol = length(all_columns)))
names(result_df) <- all_columns

# Fill the data frame
for (i in seq_along(all_extracted_data)) {
  row_data <- all_extracted_data[[i]]
  for (col_name in names(row_data)) {
    if (col_name %in% names(result_df)) {
      result_df[i, col_name] <- row_data[[col_name]]
    }
  }
}

# Clean and organize the columns
cat("\nCleaning and organizing columns...\n")

# Function to clean extracted values
clean_value <- function(x) {
  if (is.na(x) || x == "" || x == "N/A" || x == "Not available") {
    return(NA)
  }
  
  # Remove markdown formatting
  x <- str_remove_all(x, "\\*\\*")
  x <- str_remove_all(x, "\\*")
  
  # Remove leading/trailing whitespace and clean up
  x <- str_trim(x)
  x <- str_remove(x, "^-\\s*")
  x <- str_remove(x, "^\\d+\\.\\s*")
  
  # Remove extra whitespace
  x <- str_squish(x)
  
  return(x)
}

# Apply cleaning to all columns except Title
for (col in names(result_df)) {
  if (col != "Title") {
    result_df[[col]] <- sapply(result_df[[col]], clean_value)
  } else {
    # Keep Title as-is but ensure it's character
    result_df[[col]] <- as.character(result_df[[col]])
  }
}

# Reorder columns to put important ones first
important_cols <- c("Title", "Extracted_Title", "Year", "Authors", "Journal", "DOI", "Country", "Study Period", 
                   "Data Collection Period", "Data Sources", "Data Availability",
                   "SUoA Type", "Unit of Analysis", "Sample Size", "Filename")

# Get columns that exist in our data
existing_important <- important_cols[important_cols %in% names(result_df)]
other_cols <- setdiff(names(result_df), existing_important)

# Reorder
result_df <- result_df[, c(existing_important, sort(other_cols))]

cat("\nFinal dataset dimensions:", nrow(result_df), "rows x", ncol(result_df), "columns\n")

# Show summary of key columns
key_cols_to_show <- intersect(c("Title", "Year", "Authors", "Country", "Study Period", "Data Sources"), names(result_df))
if (length(key_cols_to_show) > 0) {
  cat("\nSample of key extracted columns:\n")
  for (col in key_cols_to_show) {
    non_na_count <- sum(!is.na(result_df[[col]]))
    cat(sprintf("- %s: %d/%d rows have data (%.1f%%)\n", 
                col, non_na_count, nrow(result_df), (non_na_count/nrow(result_df))*100))
  }
}

# Write the result
output_file <- "Output/multi_column_extracted_dataset.csv"
write_csv(result_df, output_file)
cat("\nResults saved to:", output_file, "\n")

# Show column names for verification
cat("\nAll extracted column names:\n")
cat(paste(names(result_df), collapse = ", "), "\n")

cat("\nExtraction complete!\n")
