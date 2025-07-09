# Simple Item Extraction to Columns
# Extract key-value pairs from each cell into separate columns

library(dplyr)
library(readr)
library(stringr)

# Read the clean combined dataset
clean_data <- read_csv("Output/clean_combined_dataset.csv")

# Function to extract all key-value pairs from text
extract_simple_items <- function(text, prefix = "") {
  if (is.na(text) || text == "" || text == "Not mentioned") {
    return(list())
  }
  
  result <- list()
  
  # First clean up the text to handle CSV formatting issues
  text <- gsub('""', '"', text)  # Fix double quotes
  text <- gsub('^"', '', text)   # Remove leading quote
  text <- gsub('"$', '', text)   # Remove trailing quote
  
  # Additional cleaning for markdown formatting
  text <- gsub('\\*\\*(.*?)\\*\\*:', '\\1:', text)  # Convert **Key**: to Key:
  
  # Split by lines and process each line
  lines <- str_split(text, "\\n")[[1]] %>% str_trim()
  
  for (line in lines) {
    if (line == "") next
    
    # Pattern 1: - **Key:** Value
    if (str_detect(line, "^-\\s*\\*\\*(.+?)\\*\\*:\\s*(.+)")) {
      matches <- str_match(line, "^-\\s*\\*\\*(.+?)\\*\\*:\\s*(.+)")
      if (!is.na(matches[2]) && !is.na(matches[3])) {
        key <- str_replace_all(matches[2], "[^A-Za-z0-9]", "_")
        value <- str_trim(matches[3]) %>%
                 str_remove_all("^\\*\\*|\\*\\*$") %>%  # Remove leading/trailing **
                 str_trim()
        result[[paste0(prefix, key)]] <- value
      }
    }
    # Pattern 2: Key: Value (simple colon)
    else if (str_detect(line, "^[^:]+:\\s*.+")) {
      parts <- str_split(line, ":", n = 2)[[1]]
      if (length(parts) == 2) {
        key <- str_trim(parts[1]) %>% 
               str_remove_all("^[-•\\d\\.\\s*]+") %>%
               str_replace_all("[^A-Za-z0-9]", "_")
        value <- str_trim(parts[2]) %>%
                 str_remove_all("^\\*\\*|\\*\\*$") %>%  # Remove leading/trailing **
                 str_trim()
        if (key != "" && value != "") {
          result[[paste0(prefix, key)]] <- value
        }
      }
    }
    # Pattern 3: Numbered items "1. Variable | Description | Unit | Source"
    else if (str_detect(line, "^\\d+\\.\\s*(.+)")) {
      content <- str_remove(line, "^\\d+\\.\\s*")
      if (!is.na(content) && content != "") {
        # If it has pipes, split and use first part as key
        if (str_detect(content, "\\|")) {
          var_name <- str_split(content, "\\|")[[1]][1] %>% str_trim()
          key <- str_replace_all(var_name, "[^A-Za-z0-9]", "_")
          result[[paste0(prefix, "Variable_", key)]] <- content
        } else {
          # Just use as generic numbered item
          item_num <- str_extract(line, "^\\d+")
          result[[paste0(prefix, "Item_", item_num)]] <- content
        }
      }
    }
    # Pattern 4: Simple bullet points
    else if (str_detect(line, "^-\\s+(.+)")) {
      content <- str_remove(line, "^-\\s+")
      if (!is.na(content) && content != "") {
        # Check if it contains key-value pattern
        if (str_detect(content, ":")) {
          parts <- str_split(content, ":", n = 2)[[1]]
          if (length(parts) == 2) {
            key <- str_trim(parts[1]) %>% str_replace_all("[^A-Za-z0-9]", "_")
            value <- str_trim(parts[2])
            if (key != "" && value != "") {
              result[[paste0(prefix, key)]] <- value
            }
          }
        }
      }
    }
  }
  
  return(result)
}

# Initialize result with Title
result_data <- clean_data %>% select(Title)

# Process each column
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
    items <- extract_simple_items(clean_data[[col_name]][i], prefix)
    all_keys <- c(all_keys, names(items))
  }
  all_keys <- unique(all_keys)
  
  # Initialize columns
  for (key in all_keys) {
    result_data[[key]] <- NA_character_
  }
  
  # Fill in the data
  for (i in 1:nrow(clean_data)) {
    items <- extract_simple_items(clean_data[[col_name]][i], prefix)
    for (key in names(items)) {
      result_data[[key]][i] <- items[[key]]
    }
  }
}

# Save the result
write_csv(result_data, "Output/simple_expanded_dataset.csv")

# Print summary
cat("\nSimple expansion completed!\n")
cat("Original columns:", ncol(clean_data), "\n")
cat("Expanded columns:", ncol(result_data), "\n")
cat("Articles:", nrow(result_data), "\n")
