# ChatGPT-inspired structured data extraction
# Extract key-value pairs from systematic review dataset

library(dplyr)
library(tidyr)
library(stringr)

# Load the data
input_file <- "Output/clean_combined_dataset.csv"
df <- read.csv(input_file, stringsAsFactors = FALSE)

cat("Original dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Define helper function to extract key-value pairs
extract_key_value_pairs <- function(cell_content) {
  if (is.na(cell_content) || cell_content == "" || cell_content == "Not specified") {
    return(data.frame())
  }
  
  pairs <- data.frame()
  
  # Pattern 1: **Key:** Value or **Key**: Value
  pattern1_matches <- str_match_all(cell_content, "\\*\\*([^*]+?)\\*\\*:?\\s*([^*\\n]+?)(?=\\s*\\*\\*|$)")[[1]]
  if (nrow(pattern1_matches) > 0) {
    for (i in 1:nrow(pattern1_matches)) {
      key <- str_trim(pattern1_matches[i, 2])
      value <- str_trim(pattern1_matches[i, 3])
      if (key != "" && value != "") {
        pairs <- rbind(pairs, data.frame(key = key, value = value, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Pattern 2: - Key: Value
  pattern2_matches <- str_match_all(cell_content, "-\\s*([^:]+?):\\s*([^\\n-]+?)(?=\\s*-|$)")[[1]]
  if (nrow(pattern2_matches) > 0) {
    for (i in 1:nrow(pattern2_matches)) {
      key <- str_trim(pattern2_matches[i, 2])
      value <- str_trim(pattern2_matches[i, 3])
      if (key != "" && value != "") {
        pairs <- rbind(pairs, data.frame(key = key, value = value, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Pattern 3: Key: Value (simple colon-separated)
  # Only if no bold or dash patterns found
  if (nrow(pairs) == 0) {
    pattern3_matches <- str_match_all(cell_content, "([^:\\n]+?):\\s*([^\\n]+?)(?=\\n|$)")[[1]]
    if (nrow(pattern3_matches) > 0) {
      for (i in 1:nrow(pattern3_matches)) {
        key <- str_trim(pattern3_matches[i, 2])
        value <- str_trim(pattern3_matches[i, 3])
        if (key != "" && value != "" && !str_detect(key, "^\\d+\\.$")) {  # Skip numbered lists
          pairs <- rbind(pairs, data.frame(key = key, value = value, stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  # Pattern 4: Numbered lists (1. Item | Description | Unit)
  pattern4_matches <- str_match_all(cell_content, "\\d+\\.\\s*([^|\\n]+?)\\s*\\|\\s*([^|\\n]+?)\\s*\\|\\s*([^|\\n]+?)(?=\\s*\\d+\\.|$)")[[1]]
  if (nrow(pattern4_matches) > 0) {
    for (i in 1:nrow(pattern4_matches)) {
      variable <- str_trim(pattern4_matches[i, 2])
      description <- str_trim(pattern4_matches[i, 3])
      unit <- str_trim(pattern4_matches[i, 4])
      if (variable != "") {
        pairs <- rbind(pairs, data.frame(key = paste0("Variable_", variable), value = variable, stringsAsFactors = FALSE))
        if (description != "") {
          pairs <- rbind(pairs, data.frame(key = paste0("Description_", variable), value = description, stringsAsFactors = FALSE))
        }
        if (unit != "") {
          pairs <- rbind(pairs, data.frame(key = paste0("Unit_", variable), value = unit, stringsAsFactors = FALSE))
        }
      }
    }
  }
  
  return(pairs)
}

# Extract all key-value pairs
cat("Extracting key-value pairs from all cells...\n")
all_extracted <- list()
extraction_count <- 0

for (i in 1:nrow(df)) {
  if (i %% 100 == 0) {
    cat("Processing row", i, "of", nrow(df), "\n")
  }
  
  row_data <- list()
  row_data[["row_id"]] <- i
  
  # Extract from each column
  for (col in colnames(df)) {
    cell_content <- df[i, col]
    if (!is.na(cell_content) && cell_content != "" && cell_content != "Not specified") {
      pairs <- extract_key_value_pairs(cell_content)
      if (nrow(pairs) > 0) {
        for (j in 1:nrow(pairs)) {
          clean_key <- str_replace_all(pairs$key[j], "[^A-Za-z0-9_]", "_")
          clean_key <- str_replace_all(clean_key, "_+", "_")
          clean_key <- str_replace(clean_key, "^_|_$", "")
          
          clean_value <- str_trim(pairs$value[j])
          clean_value <- str_replace_all(clean_value, "\\*\\*", "")  # Remove markdown
          clean_value <- str_replace_all(clean_value, "\\s+", " ")  # Normalize whitespace
          
          if (clean_key != "" && clean_value != "") {
            row_data[[clean_key]] <- clean_value
            extraction_count <- extraction_count + 1
          }
        }
      }
    }
  }
  
  all_extracted[[i]] <- row_data
}

cat("Total extractions made:", extraction_count, "\n")

# Convert to data frame
cat("Converting to wide-format data frame...\n")
all_keys <- unique(unlist(lapply(all_extracted, names)))
all_keys <- all_keys[all_keys != "row_id"]
cat("Total unique keys extracted:", length(all_keys), "\n")

# Create wide format data frame
result_df <- data.frame(row_id = 1:nrow(df))

for (key in all_keys) {
  values <- sapply(all_extracted, function(x) {
    if (key %in% names(x)) {
      return(x[[key]])
    } else {
      return(NA)
    }
  })
  result_df[[key]] <- values
}

cat("Final dataset dimensions:", nrow(result_df), "rows,", ncol(result_df), "columns\n")

# Save the result
output_file <- "Output/chatgpt_extracted_dataset.csv"
write.csv(result_df, output_file, row.names = FALSE)
cat("Saved extracted dataset to:", output_file, "\n")

# Show summary statistics
non_na_counts <- sapply(result_df[, -1], function(x) sum(!is.na(x)))
cat("\nTop 20 most populated extracted fields:\n")
print(head(sort(non_na_counts, decreasing = TRUE), 20))

# Show extraction success for key fields
key_fields <- c("Title", "Year", "Authors", "Author", "Country", "Study_design", "Sample_size", "Methodology")
existing_key_fields <- key_fields[key_fields %in% names(result_df)]
if (length(existing_key_fields) > 0) {
  cat("\nExtraction success for key fields:\n")
  for (field in existing_key_fields) {
    success_rate <- round(100 * sum(!is.na(result_df[[field]])) / nrow(result_df), 1)
    cat(field, ":", success_rate, "%\n")
  }
}

cat("\nExtraction complete!\n")
