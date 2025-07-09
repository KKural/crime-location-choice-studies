# Comprehensive Item Extraction with Pattern Recognition
# Handles all different data patterns: **, -, numbered lists, pipes

library(dplyr)
library(readr)
library(stringr)

# Read the clean combined dataset
clean_data <- read_csv("Output/clean_combined_dataset.csv")

# Comprehensive extraction function for all patterns
extract_all_patterns <- function(text, prefix = "") {
  if (is.na(text) || text == "" || text == "Not mentioned") {
    return(list())
  }
  
  result <- list()
  
  # Split by \r\n or \n and process each line
  lines <- str_split(text, "\\r\\n|\\n")[[1]] %>% str_trim()
  
  for (line in lines) {
    if (line == "" || nchar(line) < 3) next
    
    # Pattern 1: - **Key:** Value
    if (str_detect(line, "^-\\s*\\*\\*(.+?)\\*\\*:\\s*(.+)")) {
      matches <- str_match(line, "^-\\s*\\*\\*(.+?)\\*\\*:\\s*(.+)")
      if (!is.na(matches[2]) && !is.na(matches[3])) {
        key <- str_replace_all(matches[2], "[^A-Za-z0-9]", "_")
        value <- str_trim(matches[3])
        result[[paste0(prefix, key)]] <- value
      }
    }
    # Pattern 2: - Key: Value (simple dash)
    else if (str_detect(line, "^-\\s*([^:]+):\\s*(.+)")) {
      matches <- str_match(line, "^-\\s*([^:]+):\\s*(.+)")
      if (!is.na(matches[2]) && !is.na(matches[3])) {
        key <- str_trim(matches[2]) %>% str_replace_all("[^A-Za-z0-9]", "_")
        value <- str_trim(matches[3])
        if (key != "" && value != "") {
          result[[paste0(prefix, key)]] <- value
        }
      }
    }
    # Pattern 3: Numbered items with pipes "1. Variable | Description | Unit | Source"
    else if (str_detect(line, "^\\d+\\.\\s*(.+)\\s*\\|")) {
      # Split by pipes
      parts <- str_split(line, "\\|")[[1]] %>% str_trim()
      if (length(parts) >= 2) {
        # Extract variable name from first part
        first_part <- parts[1]
        var_name <- str_remove(first_part, "^\\d+\\.\\s*") %>% str_trim()
        
        if (var_name != "") {
          var_key <- str_replace_all(var_name, "[^A-Za-z0-9]", "_")
          
          # For different structures
          if (length(parts) == 4) {
            # Format: Variable | Description | Unit | Source
            result[[paste0(prefix, "Variable_", var_key)]] <- var_name
            result[[paste0(prefix, "Description_", var_key)]] <- parts[2]
            result[[paste0(prefix, "Unit_", var_key)]] <- parts[3]
            result[[paste0(prefix, "Source_", var_key)]] <- parts[4]
          } else if (length(parts) == 3) {
            # Format: Variable | Description | Value/Unit
            result[[paste0(prefix, "Variable_", var_key)]] <- var_name
            result[[paste0(prefix, "Description_", var_key)]] <- parts[2]
            result[[paste0(prefix, "Value_", var_key)]] <- parts[3]
          } else {
            # Just store the full content
            result[[paste0(prefix, "Item_", var_key)]] <- line
          }
        }
      }
    }
    # Pattern 4: Just numbered items without pipes
    else if (str_detect(line, "^\\d+\\.\\s*(.+)")) {
      content <- str_remove(line, "^\\d+\\.\\s*") %>% str_trim()
      if (content != "") {
        # Check if it has internal structure like "Key | Value"
        if (str_detect(content, "\\|")) {
          parts <- str_split(content, "\\|")[[1]] %>% str_trim()
          if (length(parts) >= 2) {
            key <- str_replace_all(parts[1], "[^A-Za-z0-9]", "_")
            result[[paste0(prefix, "Variable_", key)]] <- parts[1]
            result[[paste0(prefix, "Info_", key)]] <- paste(parts[-1], collapse=" | ")
          }
        } else {
          # Look for "Key: Value" pattern within numbered item
          if (str_detect(content, ":")) {
            parts <- str_split(content, ":", n = 2)[[1]]
            if (length(parts) == 2) {
              key <- str_trim(parts[1]) %>% str_replace_all("[^A-Za-z0-9]", "_")
              value <- str_trim(parts[2])
              result[[paste0(prefix, key)]] <- value
            }
          } else {
            # Just use the item number
            item_num <- str_extract(line, "^\\d+") 
            result[[paste0(prefix, "Item_", item_num)]] <- content
          }
        }
      }
    }
    # Pattern 5: Simple Key: Value (no dash or number)
    else if (str_detect(line, "^[^-\\d][^:]*:\\s*(.+)")) {
      parts <- str_split(line, ":", n = 2)[[1]]
      if (length(parts) == 2) {
        key <- str_trim(parts[1]) %>% str_replace_all("[^A-Za-z0-9]", "_")
        value <- str_trim(parts[2])
        if (key != "" && value != "") {
          result[[paste0(prefix, key)]] <- value
        }
      }
    }
  }
  
  return(result)
}

# Initialize result with Title
result_data <- clean_data %>% select(Title)

# Process each column
cat("Processing columns with comprehensive pattern recognition...\n")

for (col_name in names(clean_data)[-1]) {
  cat("Processing:", col_name, "\n")
  
  # Clean column name for prefix
  prefix <- str_replace_all(col_name, "[^A-Za-z0-9]", "_") %>% 
           str_replace_all("_+", "_") %>% 
           str_remove("_$") %>%
           paste0("_")
  
  # Collect all possible keys for this column first
  all_keys <- c()
  for (i in 1:nrow(clean_data)) {
    items <- extract_all_patterns(clean_data[[col_name]][i], prefix)
    all_keys <- c(all_keys, names(items))
  }
  all_keys <- unique(all_keys)
  
  # Initialize columns
  for (key in all_keys) {
    result_data[[key]] <- NA_character_
  }
  
  # Fill in the data
  for (i in 1:nrow(clean_data)) {
    items <- extract_all_patterns(clean_data[[col_name]][i], prefix)
    for (key in names(items)) {
      result_data[[key]][i] <- items[[key]]
    }
  }
}

# Save the result
write_csv(result_data, "Output/comprehensive_expanded_dataset.csv")

# Print summary
cat("\nComprehensive extraction completed!\n")
cat("Original columns:", ncol(clean_data), "\n")
cat("Expanded columns:", ncol(result_data), "\n")
cat("Articles:", nrow(result_data), "\n")

# Quick validation - show some sample extracted data
cat("\n=== SAMPLE EXTRACTED DATA ===\n")
basic_cols <- names(result_data)[str_detect(names(result_data), "^BASIC_STUDY_IDENTIFICATION_")]
if (length(basic_cols) > 0) {
  cat("Basic Study ID columns found:", length(basic_cols), "\n")
  sample_data <- result_data[1:3, c("Title", head(basic_cols, 5))]
  print(sample_data)
}

cat("\n=== EXTRACTION SUMMARY BY CATEGORY ===\n")
category_counts <- tibble(
  Category = str_extract(names(result_data)[-1], "^[^_]+"),
  Column_Count = 1
) %>%
  group_by(Category) %>%
  summarise(Total_Columns = sum(Column_Count), .groups = "drop") %>%
  arrange(desc(Total_Columns))

print(category_counts)
