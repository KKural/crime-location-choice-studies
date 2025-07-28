# Direct Variable Extraction from CSV Text Data
# This script extracts variables directly from the text content in each cell

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# Set up output folder
analysis_date <- Sys.Date()
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Direct_Variable_Extraction")

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Read and merge CSV files ------------------------------------------------

combined_csv_files <- c(
  "Data/Study_Identification_Spatial_Units.csv",
  "Data/Theoretical_Framework_Methodology.csv", 
  "Data/Temporal_Variables_Findings.csv",
  "Data/Scale_Effects_Limitations.csv",
  "Data/study area size.csv"
)

# Function to clean column names
clean_column_names <- function(df) {
  main_cols <- colnames(df)[!grepl("tables|DOI|Venue|Citation", colnames(df))]
  return(df[, main_cols])
}

# Read and merge all CSV files
df_combined <- NULL

for (i in 1:length(combined_csv_files)) {
  if (file.exists(combined_csv_files[i])) {
    temp_df <- read_csv(combined_csv_files[i], show_col_types = FALSE)
    temp_df <- clean_column_names(temp_df)
    
    if (is.null(df_combined)) {
      df_combined <- temp_df
    } else {
      # Remove duplicate columns and merge
      cols_to_remove <- c("Authors", "Year")
      remaining_cols <- setdiff(names(temp_df), cols_to_remove)
      temp_df <- temp_df[, remaining_cols]
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
  }
}

cat("Data loaded successfully!\n")
cat("Total studies:", nrow(df_combined), "\n")
cat("Total columns:", ncol(df_combined), "\n\n")

# Define variable extraction patterns -------------------------------------

# Enhanced function to extract variables from descriptive text
extract_variables_from_descriptive_text <- function(text_content, column_name) {
  if (is.na(text_content) || text_content == "" || nchar(text_content) < 10) {
    return(data.frame(variable_name = character(0), measurement_type = character(0), stringsAsFactors = FALSE))
  }
  
  variables <- data.frame(variable_name = character(0), measurement_type = character(0), stringsAsFactors = FALSE)
  
  # Split by common delimiters and extract numbered items
  # Look for patterns like "1. Variable Name |", "2. Another Variable |", etc.
  numbered_items <- str_extract_all(text_content, "\\d+\\.\\s*[^\\d\\n\\r]+")[[1]]
  
  for (item in numbered_items) {
    # Clean the item
    clean_item <- str_remove(item, "^\\d+\\.\\s*")
    clean_item <- str_trim(clean_item)
    
    if (nchar(clean_item) < 3) next
    
    # Split by pipe to get variable name and measurement info
    parts <- str_split(clean_item, "\\|", simplify = TRUE)
    
    if (length(parts) > 0 && parts[1] != "") {
      variable_name <- str_trim(parts[1])
      
      # Filter out non-variables
      if (is_actual_variable(variable_name)) {
        
        # Determine measurement type
        measurement_type <- "Unknown"
        if (ncol(parts) > 1) {
          full_text <- paste(parts, collapse = " ")
          measurement_type <- infer_measurement_type(full_text)
        } else {
          measurement_type <- infer_measurement_type(variable_name)
        }
        
        variables <- rbind(variables, data.frame(
          variable_name = variable_name,
          measurement_type = measurement_type,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(variables)
}

# Function to check if text represents an actual variable
is_actual_variable <- function(text) {
  if (is.na(text) || text == "" || nchar(text) < 3) return(FALSE)
  
  text_lower <- tolower(text)
  
  # Skip if it starts with statistical results
  if (str_detect(text, "^\\d+(\\.\\d+)?\\s*(for|times|more|likely|%|\\)|,)")) return(FALSE)
  
  # Skip if it contains statistical language
  statistical_terms <- c(
    "model performance", "pseudo r", "log-likelihood", "information criteria",
    "goodness-of-fit", "main results", "theoretical contributions", 
    "study design", "data cleaning", "sampling approach", "research objectives",
    "study period", "data collection", "country:", "city/region:", 
    "study area size", "not mentioned", "not explicitly", "varies"
  )
  
  for (term in statistical_terms) {
    if (str_detect(text_lower, term)) return(FALSE)
  }
  
  # Skip if it's clearly a description rather than a variable name
  if (str_detect(text_lower, "^(title:|year:|authors:|study|data|country|city|approach|design|framework|method)")) return(FALSE)
  
  # Skip if it contains measurement results with numbers
  if (str_detect(text, "\\d+(\\.\\d+)?\\s*(km²|km2|residents|inhabitants|€|\\$|%)")) return(FALSE)
  
  return(TRUE)
}

# Function to infer measurement type from text
infer_measurement_type <- function(text) {
  if (is.na(text) || text == "") return("Unknown")
  
  text_lower <- tolower(text)
  
  # Binary/Categorical indicators
  if (str_detect(text_lower, "presence of|whether|binary|dummy|categorical|yes/no|dichotomous")) {
    return("Binary")
  }
  
  # Percentage indicators
  if (str_detect(text_lower, "percentage|percent|%|proportion|ratio")) {
    return("Percentage")
  }
  
  # Distance indicators
  if (str_detect(text_lower, "distance|km|meter|mile|proximity|accessibility|euclidean")) {
    return("Distance")
  }
  
  # Rate indicators
  if (str_detect(text_lower, "rate|per\\s+\\d+|frequency|density")) {
    return("Rate")
  }
  
  # Count/Number indicators
  if (str_detect(text_lower, "number of|count|size|population|residents|units|total")) {
    return("Number")
  }
  
  # Index/Scale indicators
  if (str_detect(text_lower, "index|score|scale|level|measure")) {
    return("Index")
  }
  
  return("Number")  # Default
}

# Extract variables from specific columns --------------------------------

# Define the main variable columns to focus on
main_variable_columns <- c(
  "DEMOGRAPHIC & SOCIAL VARIABLES",
  "ECONOMIC VARIABLES", 
  "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
  "DISTANCE & ACCESSIBILITY VARIABLES",
  "TEMPORAL & CONTROL VARIABLES"
)

# Initialize results
all_extracted_variables <- data.frame(
  study_title = character(0),
  year = character(0),
  column_source = character(0),
  variable_name = character(0),
  measurement_type = character(0),
  stringsAsFactors = FALSE
)

cat("Extracting variables from main variable columns...\n")

# Process each study and each main variable column
for (i in 1:nrow(df_combined)) {
  study_title <- df_combined$Title[i]
  year <- df_combined$Year[i]
  
  for (col_name in main_variable_columns) {
    if (col_name %in% names(df_combined)) {
      cell_content <- df_combined[[col_name]][i]
      
      # Extract variables from this cell
      extracted_vars <- extract_variables_from_descriptive_text(cell_content, col_name)
      
      if (nrow(extracted_vars) > 0) {
        # Add study and column information
        extracted_vars$study_title <- study_title
        extracted_vars$year <- year
        extracted_vars$column_source <- col_name
        
        # Append to results
        all_extracted_variables <- rbind(all_extracted_variables, extracted_vars)
      }
    }
  }
  
  if (i %% 10 == 0) {
    cat("Processed", i, "studies...\n")
  }
}

cat("\nExtraction completed!\n")
cat("Total variable instances extracted:", nrow(all_extracted_variables), "\n")
cat("Unique variables found:", length(unique(all_extracted_variables$variable_name)), "\n\n")

# Create summary and unique list ------------------------------------------

# Generate frequency summary
variable_frequency <- table(all_extracted_variables$variable_name)
variable_summary <- data.frame(
  variable_name = names(variable_frequency),
  frequency = as.numeric(variable_frequency),
  stringsAsFactors = FALSE
)

# Add measurement type and source info
variable_summary$measurement_type <- ""
variable_summary$source_columns <- ""
variable_summary$studies_count <- 0

for (i in 1:nrow(variable_summary)) {
  var_name <- variable_summary$variable_name[i]
  var_rows <- all_extracted_variables[all_extracted_variables$variable_name == var_name, ]
  
  # Get most common measurement type
  measurement_types <- table(var_rows$measurement_type)
  variable_summary$measurement_type[i] <- names(which.max(measurement_types))
  
  # Get source columns
  variable_summary$source_columns[i] <- paste(unique(var_rows$column_source), collapse = "; ")
  
  # Count unique studies
  variable_summary$studies_count[i] <- length(unique(var_rows$study_title))
}

# Sort by frequency
variable_summary <- variable_summary[order(-variable_summary$frequency), ]

# Create unique variables list for manual categorization
unique_variables_list <- data.frame(
  variable_name = sort(unique(all_extracted_variables$variable_name)),
  measurement_type = "",
  category = "",
  notes = "",
  stringsAsFactors = FALSE
)

# Fill in measurement types
for (i in 1:nrow(unique_variables_list)) {
  var_name <- unique_variables_list$variable_name[i]
  var_data <- all_extracted_variables[all_extracted_variables$variable_name == var_name, ]
  if (nrow(var_data) > 0) {
    measurement_counts <- table(var_data$measurement_type)
    unique_variables_list$measurement_type[i] <- names(which.max(measurement_counts))
  }
}

# Display results summary
cat("Top 20 most frequent variables:\n")
print(head(variable_summary[, c("variable_name", "frequency", "measurement_type")], 20))

cat("\n\nVariables by measurement type:\n")
measurement_breakdown <- table(all_extracted_variables$measurement_type)
print(measurement_breakdown)

cat("\n\nVariables by source column:\n")
column_breakdown <- table(all_extracted_variables$column_source)
print(column_breakdown)

# Save results ------------------------------------------------------------

# Save all variable instances
write_csv(all_extracted_variables, 
          file.path(output_folder, "all_variable_instances_direct.csv"))
cat("\n✓ All variable instances saved\n")

# Save variable summary
write_csv(variable_summary, 
          file.path(output_folder, "variable_frequency_summary.csv"))
cat("✓ Variable frequency summary saved\n")

# Save unique variables for categorization
write_csv(unique_variables_list, 
          file.path(output_folder, "unique_variables_for_categorization.csv"))
cat("✓ Unique variables list for categorization saved\n")

# Save sample of extracted content for review
sample_extractions <- all_extracted_variables[1:min(100, nrow(all_extracted_variables)), ]
write_csv(sample_extractions, 
          file.path(output_folder, "sample_extractions_review.csv"))
cat("✓ Sample extractions saved for review\n")

cat("\n", rep("=", 60), "\n")
cat("DIRECT VARIABLE EXTRACTION COMPLETED!\n")
cat(rep("=", 60), "\n")
cat("Output folder:", output_folder, "\n")
cat("Total variables extracted:", nrow(all_extracted_variables), "\n")
cat("Unique variables:", nrow(unique_variables_list), "\n")
cat("\nNext steps:\n")
cat("1. Review 'sample_extractions_review.csv' to check extraction quality\n")
cat("2. Open 'unique_variables_for_categorization.csv' to categorize variables\n")
cat("3. Add categories in the 'category' column and save the file\n")
