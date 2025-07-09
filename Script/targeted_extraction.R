# Targeted Key-Value Extraction Script
# Extract specific key-value pairs from systematic review dataset

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Load the data
input_file <- "Output/clean_combined_dataset.csv"
df <- read_csv(input_file, show_col_types = FALSE)

cat("Original dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Column names:", paste(colnames(df), collapse = ", "), "\n\n")

# Function to extract key-value pairs from text using regex
extract_key_value <- function(text, key) {
  if (is.na(text) || text == "" || text == "Not specified") {
    return(NA)
  }
  
  # Try multiple patterns for the same key
  patterns <- c(
    # Pattern 1: **Key:** Value (with bold)
    paste0("\\*\\*", key, "\\*\\*:?\\s*([^\\*\\n-]+?)(?=\\s*\\*\\*|\\s*-\\s*\\*\\*|$)"),
    # Pattern 2: - Key: Value (with dash)
    paste0("-\\s*", key, "\\s*:?\\s*([^\\n-]+?)(?=\\s*-\\s*[A-Z]|$)"),
    # Pattern 3: Key: Value (simple)
    paste0("(?<![\\*\\w])", key, "\\s*:?\\s*([^\\n-]+?)(?=\\s*[A-Z][^:]*:|$)")
  )
  
  for (pattern in patterns) {
    match <- str_match(text, pattern)
    if (!is.na(match[1, 2])) {
      result <- str_trim(match[1, 2])
      # Clean up the result
      result <- str_replace_all(result, "\\*\\*", "")  # Remove bold markers
      result <- str_replace_all(result, "\\s+", " ")   # Normalize whitespace
      result <- str_trim(result)
      if (result != "" && result != "Not specified") {
        return(result)
      }
    }
  }
  
  return(NA)
}

# Define the keys we want to extract
keys_to_extract <- c(
  "Title",
  "Year",
  "Authors", "Author",
  "Journal",
  "DOI",
  "Filename",
  "Study Period", "Study_Period",
  "Data Collection Period", "Data_Collection_Period",
  "Data Sources", "Data_Sources",
  "Data Availability", "Data_Availability",
  "SUoA Type", "SUoA_Type",
  "SUoA Size", "SUoA_Size", 
  "SUoA Description", "SUoA_Description",
  "Number of Units", "Number_of_Units",
  "Population per Unit", "Population_per_Unit",
  "Quoted Rationale", "Quoted_Rationale",
  "Rationale Category", "Rationale_Category",
  "Justification Summary", "Justification_Summary",
  "Country",
  "City Region", "City_Region",
  "Study Area Size", "Study_Area_Size",
  "Study Area Description", "Study_Area_Description",
  "Crime Type", "Crime_Type",
  "Crime Types", "Crime_Types",
  "Sample Size", "Sample_Size",
  "Number of Crimes Analyzed", "Number_of_Crimes_Analyzed",
  "Number of Offenders", "Number_of_Offenders",
  "Choice Set Definition", "Choice_Set_Definition",
  "Alternative Selection", "Alternative_Selection",
  "Sample Restrictions", "Sample_Restrictions",
  "Theoretical Framework", "Theoretical_Framework",
  "Research Objectives", "Research_Objectives",
  "Literature Gap", "Literature_Gap",
  "Study Motivation", "Study_Motivation",
  "Study Design", "Study_Design",
  "Model Specification", "Model_Specification",
  "Software Used", "Software_Used",
  "Estimation Method", "Estimation_Method",
  "Model Extensions", "Model_Extensions",
  "Data Cleaning", "Data_Cleaning",
  "Variable Construction", "Variable_Construction",
  "Missing Data Handling", "Missing_Data_Handling",
  "Data Integration", "Data_Integration",
  "Quality Control", "Quality_Control",
  "Model Performance", "Model_Performance",
  "Information Criteria", "Information_Criteria",
  "Goodness of Fit Tests", "Goodness_of_Fit_Tests",
  "Model Comparison", "Model_Comparison",
  "Sample Size Effects", "Sample_Size_Effects",
  "Convergence Issues", "Convergence_Issues",
  "Main Results", "Main_Results",
  "Significant Predictors", "Significant_Predictors",
  "Effect Directions", "Effect_Directions",
  "Effect Magnitudes", "Effect_Magnitudes",
  "Surprising Findings", "Surprising_Findings",
  "Robustness Checks", "Robustness_Checks",
  "Scale Effects", "Scale_Effects",
  "Scale Sensitivity", "Scale_Sensitivity",
  "Spatial Autocorrelation", "Spatial_Autocorrelation",
  "Scale Recommendations", "Scale_Recommendations",
  "Scale Limitations", "Scale_Limitations",
  "Cross Scale Comparisons", "Cross_Scale_Comparisons",
  "Data Quality Issues", "Data_Quality_Issues",
  "Missing Data", "Missing_Data",
  "Data Source Limitations", "Data_Source_Limitations",
  "Measurement Issues", "Measurement_Issues",
  "Temporal Limitations", "Temporal_Limitations",
  "Geographic Limitations", "Geographic_Limitations",
  "Model Limitations", "Model_Limitations",
  "Analytical Constraints", "Analytical_Constraints",
  "Sample Limitations", "Sample_Limitations",
  "Causal Inference", "Causal_Inference",
  "Generalizability",
  "Comparative Limitations", "Comparative_Limitations",
  "Context Specificity", "Context_Specificity",
  "Population Constraints", "Population_Constraints",
  "Theoretical Contributions", "Theoretical_Contributions",
  "Policy Implications", "Policy_Implications",
  "Crime Prevention Implications", "Crime_Prevention_Implications",
  "Urban Planning Implications", "Urban_Planning_Implications",
  "Policy Recommendations", "Policy_Recommendations",
  "Future Research Directions", "Future_Research_Directions",
  "Spatial Scale Recommendations", "Spatial_Scale_Recommendations",
  "Data Collection Suggestions", "Data_Collection_Suggestions",
  "Methodological Improvements", "Methodological_Improvements",
  "Broader Societal Implications", "Broader_Societal_Implications",
  "Interdisciplinary Connections", "Interdisciplinary_Connections"
)

# Remove duplicates and create clean column names
unique_keys <- unique(keys_to_extract)
clean_keys <- str_replace_all(unique_keys, "[^A-Za-z0-9_]", "_")
clean_keys <- str_replace_all(clean_keys, "_+", "_")
clean_keys <- str_replace(clean_keys, "^_|_$", "")

# Create the result dataframe
result_df <- data.frame(row_id = 1:nrow(df))

# Extract from all columns
cat("Extracting key-value pairs...\n")
for (i in 1:length(unique_keys)) {
  key <- unique_keys[i]
  clean_key <- clean_keys[i]
  
  cat("Processing key:", key, "->", clean_key, "\n")
  
  # Extract this key from all columns and combine
  extracted_values <- rep(NA, nrow(df))
  
  for (row in 1:nrow(df)) {
    for (col in colnames(df)) {
      cell_content <- df[row, col]
      if (!is.na(cell_content) && cell_content != "" && cell_content != "Not specified") {
        value <- extract_key_value(cell_content, key)
        if (!is.na(value) && value != "") {
          extracted_values[row] <- value
          break  # Stop at first match for this row
        }
      }
    }
  }
  
  result_df[[clean_key]] <- extracted_values
}

# Add summary statistics
cat("\nExtraction Summary:\n")
extraction_counts <- sapply(result_df[, -1], function(x) sum(!is.na(x)))
extraction_rates <- round(100 * extraction_counts / nrow(df), 1)

# Show top extracted fields
cat("Top 20 successfully extracted fields:\n")
top_fields <- head(sort(extraction_counts, decreasing = TRUE), 20)
for (i in 1:length(top_fields)) {
  field_name <- names(top_fields)[i]
  count <- top_fields[i]
  rate <- extraction_rates[field_name]
  cat(sprintf("%-30s: %3d/%d (%5.1f%%)\n", field_name, count, nrow(df), rate))
}

# Save the result
output_file <- "Output/targeted_extracted_dataset.csv"
write_csv(result_df, output_file)

cat("\nFinal dataset dimensions:", nrow(result_df), "rows,", ncol(result_df), "columns\n")
cat("Saved to:", output_file, "\n")

# Show sample of extracted data for verification
cat("\n=== SAMPLE EXTRACTED DATA ===\n")
sample_cols <- c("Title", "Year", "Authors", "Country", "Study_Period", "Data_Sources")
available_cols <- intersect(sample_cols, colnames(result_df))
if (length(available_cols) > 0) {
  print(result_df[1:5, c("row_id", available_cols)] %>% as.data.frame())
} else {
  print(head(result_df[1:5, 1:min(10, ncol(result_df))]))
}

cat("\nExtraction complete! Dataset ready for analysis.\n")
