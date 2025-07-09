# Extract Individual Items to Separate Columns
# This script takes each cell and extracts individual items into separate columns

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Read the clean combined dataset
clean_data <- read_csv("Output/clean_combined_dataset.csv")

# Function to extract key-value pairs from text
extract_items_to_columns <- function(text, prefix = "") {
  if (is.na(text) || text == "" || text == "Not mentioned") {
    return(tibble())
  }
  
  # Split by common delimiters and clean
  items <- text %>%
    str_split("\\n|;|\\|") %>%
    unlist() %>%
    str_trim() %>%
    .[. != ""]
  
  # Extract key-value pairs
  result <- tibble()
  
  for (item in items) {
    # Look for patterns like "- **Key:** Value" or "Key: Value" or numbered items
    if (str_detect(item, "\\*\\*(.+?)\\*\\*:")) {
      # Extract **Key**: Value pattern
      key <- str_extract(item, "(?<=\\*\\*)(.+?)(?=\\*\\*:)")
      value <- str_extract(item, "(?<=\\*\\*:.{0,5})(.+)") %>% str_trim()
      if (!is.na(key) && !is.na(value)) {
        col_name <- paste0(prefix, str_replace_all(key, "[^A-Za-z0-9]", "_"))
        result[[col_name]] <- value
      }
    } else if (str_detect(item, "^\\d+\\.\\s*(.+?)\\s*\\|")) {
      # Extract numbered items with | separator (like "1. Variable | Description | Unit | Source")
      parts <- str_split(item, "\\|")[[1]] %>% str_trim()
      if (length(parts) >= 2) {
        var_name <- str_extract(parts[1], "(?<=\\d+\\.\\s)(.+)") %>% str_trim()
        if (!is.na(var_name)) {
          var_name_clean <- str_replace_all(var_name, "[^A-Za-z0-9]", "_")
          result[[paste0(prefix, var_name_clean)]] <- item
        }
      }
    } else if (str_detect(item, ":")) {
      # Extract simple Key: Value pattern
      parts <- str_split(item, ":", n = 2)[[1]]
      if (length(parts) == 2) {
        key <- str_trim(parts[1]) %>% str_remove_all("^[-•\\d\\.\\s*]+")
        value <- str_trim(parts[2])
        if (key != "" && value != "") {
          col_name <- paste0(prefix, str_replace_all(key, "[^A-Za-z0-9]", "_"))
          result[[col_name]] <- value
        }
      }
    } else if (str_detect(item, "^[-•]\\s*(.+)")) {
      # Extract bullet points
      content <- str_extract(item, "(?<=^[-•]\\s)(.+)")
      if (!is.na(content)) {
        # Try to find key-value in content
        if (str_detect(content, ":")) {
          parts <- str_split(content, ":", n = 2)[[1]]
          if (length(parts) == 2) {
            key <- str_trim(parts[1])
            value <- str_trim(parts[2])
            col_name <- paste0(prefix, str_replace_all(key, "[^A-Za-z0-9]", "_"))
            result[[col_name]] <- value
          }
        }
      }
    }
  }
  
  return(result)
}

# Initialize the expanded dataset with Title
expanded_data <- clean_data %>% select(Title)

# Process each column
for (col_name in names(clean_data)[-1]) {  # Skip Title column
  cat("Processing:", col_name, "\n")
  
  # Clean column name for prefix
  prefix <- str_replace_all(col_name, "[^A-Za-z0-9]", "_") %>% 
           str_replace_all("_+", "_") %>% 
           str_remove("_$") %>%
           paste0("_")
  
  # Extract items for each row
  for (i in 1:nrow(clean_data)) {
    cell_content <- clean_data[[col_name]][i]
    extracted <- extract_items_to_columns(cell_content, prefix)
    
    if (ncol(extracted) > 0) {
      # Add extracted columns to the main dataset
      for (ext_col in names(extracted)) {
        if (!ext_col %in% names(expanded_data)) {
          expanded_data[[ext_col]] <- NA_character_
        }
        value <- extracted[[ext_col]]
        if (length(value) > 0 && !is.null(value)) {
          expanded_data[[ext_col]][i] <- as.character(value)
        }
      }
    }
  }
}

# Save the expanded dataset
write_csv(expanded_data, "Output/expanded_wide_dataset.csv")

# Print summary
cat("\nExpansion completed!\n")
cat("Original columns:", ncol(clean_data), "\n")
cat("Expanded columns:", ncol(expanded_data), "\n")
cat("New columns created:", ncol(expanded_data) - 1, "\n")
