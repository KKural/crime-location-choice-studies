# Improved Multi-Column Extraction Script
# Extract all key-value pairs from all columns into separate columns

library(dplyr)
library(stringr)

# Read the input data
cat("Reading input data...\n")
data <- read.csv("Output/clean_combined_dataset.csv", stringsAsFactors = FALSE)

# Start with the Title column (preserve original)
result <- data.frame(
  Title = data$Title,
  stringsAsFactors = FALSE
)

# Function to extract key-value pairs from a single cell
extract_key_value_pairs <- function(text) {
  if (is.na(text) || text == "" || nchar(text) == 0) {
    return(list())
  }
  
  # Remove extra whitespace
  text <- str_trim(text)
  
  pairs <- list()
  
  # Pattern 1: Markdown format (- **Key:** Value)
  markdown_matches <- str_match_all(text, "(?:-\\s*)?\\*\\*([^*:]+):\\*\\*\\s*([^\\n\\r]*?)(?=\\s*(?:-\\s*\\*\\*|$))")[[1]]
  if (nrow(markdown_matches) > 0) {
    for (i in 1:nrow(markdown_matches)) {
      key <- str_trim(markdown_matches[i, 2])
      value <- str_trim(markdown_matches[i, 3])
      if (key != "" && value != "" && !is.na(key) && !is.na(value)) {
        pairs[[key]] <- value
      }
    }
  }
  
  # Pattern 2: Dash format (- Key: Value)
  dash_matches <- str_match_all(text, "-\\s*([^:]+?):\\s*([^\\n\\r]*?)(?=\\s*(?:-|$))")[[1]]
  if (nrow(dash_matches) > 0) {
    for (i in 1:nrow(dash_matches)) {
      key <- str_trim(dash_matches[i, 2])
      value <- str_trim(dash_matches[i, 3])
      # Skip if this looks like it was already captured by markdown pattern
      if (key != "" && value != "" && !is.na(key) && !is.na(value) && !str_detect(key, "\\*\\*")) {
        pairs[[key]] <- value
      }
    }
  }
  
  # Pattern 3: Simple colon format (Key: Value)
  colon_matches <- str_match_all(text, "([A-Za-z][^:]*?):\\s*([^\\n\\r]*?)(?=\\s*[A-Z]|$)")[[1]]
  if (nrow(colon_matches) > 0) {
    for (i in 1:nrow(colon_matches)) {
      key <- str_trim(colon_matches[i, 2])
      value <- str_trim(colon_matches[i, 3])
      # Skip if this looks like it was already captured
      if (key != "" && value != "" && !is.na(key) && !is.na(value) && 
          !str_detect(key, "\\*\\*") && !key %in% names(pairs)) {
        pairs[[key]] <- value
      }
    }
  }
  
  # Pattern 4: Numbered format (1. Key: Value, 2. Key: Value)
  numbered_matches <- str_match_all(text, "\\d+\\.\\s*([^:]+?):\\s*([^\\n\\r]*?)(?=\\s*\\d+\\.|$)")[[1]]
  if (nrow(numbered_matches) > 0) {
    for (i in 1:nrow(numbered_matches)) {
      key <- str_trim(numbered_matches[i, 2])
      value <- str_trim(numbered_matches[i, 3])
      if (key != "" && value != "" && !is.na(key) && !is.na(value)) {
        pairs[[key]] <- value
      }
    }
  }
  
  # Pattern 5: Pipe-separated format (Key1: Value1 | Key2: Value2)
  pipe_parts <- str_split(text, "\\s*\\|\\s*")[[1]]
  for (part in pipe_parts) {
    if (str_detect(part, ":")) {
      pipe_match <- str_match(part, "([^:]+?):\\s*(.*)")
      if (!is.na(pipe_match[1, 2]) && !is.na(pipe_match[1, 3])) {
        key <- str_trim(pipe_match[1, 2])
        value <- str_trim(pipe_match[1, 3])
        if (key != "" && value != "" && !key %in% names(pairs)) {
          pairs[[key]] <- value
        }
      }
    }
  }
  
  return(pairs)
}

# Function to clean column names
clean_column_name <- function(name) {
  # Remove markdown formatting
  name <- str_replace_all(name, "\\*\\*", "")
  # Replace problematic characters with dots
  name <- str_replace_all(name, "[^A-Za-z0-9_]", ".")
  # Remove multiple consecutive dots
  name <- str_replace_all(name, "\\.+", ".")
  # Remove leading/trailing dots
  name <- str_replace_all(name, "^\\.|\\.$", "")
  # Ensure it starts with a letter or dot (R requirement)
  if (str_detect(name, "^[0-9]")) {
    name <- paste0("X", name)
  }
  return(name)
}

# Extract from all columns except Title
all_pairs <- list()

cat("Extracting key-value pairs from all columns...\n")
for (col_idx in 2:ncol(data)) {  # Skip the Title column
  col_name <- names(data)[col_idx]
  cat(paste("Processing column:", col_name, "\n"))
  
  for (row_idx in 1:nrow(data)) {
    cell_content <- data[row_idx, col_idx]
    pairs <- extract_key_value_pairs(cell_content)
    
    for (key in names(pairs)) {
      # Clean the key name
      clean_key <- clean_column_name(key)
      
      # Handle special case where extracted "Title" conflicts with original Title
      if (clean_key == "Title") {
        clean_key <- "Extracted.Title"
      }
      
      # Initialize column if it doesn't exist
      if (!clean_key %in% names(all_pairs)) {
        all_pairs[[clean_key]] <- rep(NA, nrow(data))
      }
      
      # Store the value
      all_pairs[[clean_key]][row_idx] <- pairs[[key]]
    }
  }
}

cat("Combining all extracted data...\n")

# Convert the list to a data frame
if (length(all_pairs) > 0) {
  extracted_df <- data.frame(all_pairs, stringsAsFactors = FALSE)
  
  # Combine with the original Title
  final_result <- cbind(result, extracted_df)
} else {
  final_result <- result
}

# Reorder columns to put important ones first
important_cols <- c("Title", "Extracted.Title", "Year", "Authors", "Journal", "DOI", "Country", 
                   "Study.Period", "Data.Collection.Period", "Data.Sources", "Data.Availability", 
                   "SUoA.Type", "Sample.Size", "Filename")

# Find which important columns exist
existing_important <- important_cols[important_cols %in% names(final_result)]

# Get remaining columns
remaining_cols <- setdiff(names(final_result), existing_important)

# Reorder
final_result <- final_result[, c(existing_important, remaining_cols)]

cat(paste("Final dataset dimensions:", nrow(final_result), "rows,", ncol(final_result), "columns\n"))

# Calculate extraction statistics
non_empty_counts <- sapply(final_result, function(x) sum(!is.na(x) & x != ""))
cat("Top 20 columns by data completeness:\n")
top_cols <- head(sort(non_empty_counts, decreasing = TRUE), 20)
for (i in 1:length(top_cols)) {
  cat(paste(names(top_cols)[i], ":", top_cols[i], "rows\n"))
}

# Write the result
output_file <- "Output/improved_multi_column_extracted_dataset.csv"
write.csv(final_result, output_file, row.names = FALSE, na = "")
cat(paste("Results saved to:", output_file, "\n"))

cat("Extraction complete!\n")
