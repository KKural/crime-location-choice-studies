# Proper Variable Extraction Script
# This script extracts variables from the numbered format: "1. Variable Name | Description | Type | Source"

# Load required libraries
library(dplyr)
library(stringr)
library(readr)

# Set up output folder
analysis_date <- Sys.Date()
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Proper_Variable_Extraction")

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

cat("=== PROPER VARIABLE EXTRACTION SCRIPT ===\n")
cat("Output folder:", output_folder, "\n\n")

# Data reading and processing -----------------------------------------

# Read and merge CSV files
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

# Function to remove duplicate columns
remove_duplicate_cols <- function(df, is_first_file = FALSE) {
  if (is_first_file) {
    return(df)
  } else {
    cols_to_remove <- c("Authors", "Year")
    remaining_cols <- setdiff(names(df), cols_to_remove)
    return(df[, remaining_cols])
  }
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
      temp_df <- remove_duplicate_cols(temp_df, is_first_file = FALSE)
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
  }
}

cat("Data loaded successfully!\n")
cat("Total studies:", nrow(df_combined), "\n")
cat("Total columns:", ncol(df_combined), "\n\n")

# Select required columns
df_combined <- df_combined %>%
  select(
    Title, Year, Authors,
    `BASIC STUDY IDENTIFICATION`, `TEMPORAL SCOPE & DATA SOURCES`, 
    `study area size`, `SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`, `STUDY CONTEXT & GEOGRAPHY`, 
    `SAMPLING & CHOICE SETS`, `THEORETICAL FRAMEWORK & OBJECTIVES`, 
    `STUDY DESIGN & METHODOLOGY`, `DATA PREPARATION & PROCESSING`, `DEMOGRAPHIC & SOCIAL VARIABLES`, 
    `ECONOMIC VARIABLES`, `TEMPORAL & CONTROL VARIABLES`, `MODEL FIT & PERFORMANCE METRICS`, 
    `MAJOR FINDINGS & RESULTS`, `ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES`, `DISTANCE & ACCESSIBILITY VARIABLES`,
    `SCALE EFFECTS & SPATIAL FINDINGS`, `DATA LIMITATIONS & METHODOLOGICAL ISSUES`, 
    `GENERALIZABILITY & COMPARATIVE LIMITATIONS`, `IMPLICATIONS & FUTURE DIRECTIONS`
  )

# Variable Extraction Functions --------------------------------------------

# Function to extract variables from numbered format
extract_variables_from_numbered_text <- function(text_content) {
  if (is.na(text_content) || text_content == "" || text_content == "Not mentioned (the paper does not provide specific demographic or social variables)" || text_content == "Not mentioned (no demographic or social variables are explicitly mentioned in the paper)" || str_detect(text_content, "^Not mentioned")) {
    return(data.frame())
  }
  
  # Split by line breaks and clean
  lines <- str_split(text_content, "\\r\\n|\\n")[[1]]
  lines <- str_trim(lines)
  lines <- lines[lines != ""]
  
  # Extract numbered items (1., 2., 3., etc.)
  numbered_items <- lines[str_detect(lines, "^\\d+\\.\\s+")]
  
  if (length(numbered_items) == 0) {
    return(data.frame())
  }
  
  # Initialize results
  results <- data.frame(
    variable_name = character(),
    description = character(),
    measurement_type = character(),
    data_source = character(),
    stringsAsFactors = FALSE
  )
  
  for (item in numbered_items) {
    # Remove the number prefix (1., 2., etc.)
    clean_item <- str_remove(item, "^\\d+\\.\\s+")
    
    # Split by pipe symbol
    parts <- str_split(clean_item, "\\|")[[1]]
    parts <- str_trim(parts)
    
    if (length(parts) >= 1) {
      variable_name <- parts[1]
      description <- if(length(parts) >= 2) parts[2] else ""
      measurement_type <- if(length(parts) >= 3) parts[3] else ""
      data_source <- if(length(parts) >= 4) parts[4] else ""
      
      # Clean up measurement type
      measurement_type <- str_remove_all(measurement_type, "Unit:\\s*|Measure:\\s*")
      measurement_type <- str_trim(measurement_type)
      
      # Clean up data source
      data_source <- str_remove_all(data_source, "Data[_\\s]*Source:\\s*")
      data_source <- str_trim(data_source)
      
      # Add to results
      results <- rbind(results, data.frame(
        variable_name = variable_name,
        description = description,
        measurement_type = measurement_type,
        data_source = data_source,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# Main extraction process -----------------------------------------------

# Define variable columns (columns that contain actual variables)
variable_columns <- c(
  "DEMOGRAPHIC & SOCIAL VARIABLES",
  "ECONOMIC VARIABLES", 
  "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
  "DISTANCE & ACCESSIBILITY VARIABLES",
  "TEMPORAL & CONTROL VARIABLES"
)

# Initialize master results
all_extracted_variables <- data.frame(
  study_title = character(),
  year = character(),
  authors = character(),
  column_source = character(),
  variable_name = character(),
  description = character(),
  measurement_type = character(),
  data_source = character(),
  stringsAsFactors = FALSE
)

cat("Extracting variables from", length(variable_columns), "variable columns:\n")
for (col in variable_columns) {
  cat("-", col, "\n")
}
cat("\n")

# Process each study and each variable column
for (i in 1:nrow(df_combined)) {
  study_title <- df_combined$Title[i]
  year <- df_combined$Year[i]
  authors <- df_combined$Authors[i]
  
  # Process each variable column
  for (col_name in variable_columns) {
    if (col_name %in% names(df_combined)) {
      cell_content <- df_combined[[col_name]][i]
      
      if (!is.na(cell_content) && cell_content != "") {
        # Extract variables from this cell
        extracted_vars <- extract_variables_from_numbered_text(cell_content)
        
        if (nrow(extracted_vars) > 0) {
          # Add study information
          extracted_vars$study_title <- study_title
          extracted_vars$year <- year
          extracted_vars$authors <- authors
          extracted_vars$column_source <- col_name
          
          # Reorder columns
          extracted_vars <- extracted_vars[, c("study_title", "year", "authors", "column_source", 
                                               "variable_name", "description", "measurement_type", "data_source")]
          
          # Add to master results
          all_extracted_variables <- rbind(all_extracted_variables, extracted_vars)
        }
      }
    }
  }
  
  # Progress indicator
  if (i %% 10 == 0) {
    cat("Processed", i, "of", nrow(df_combined), "studies...\n")
  }
}

cat("\n=== EXTRACTION COMPLETED ===\n")
cat("Total variable instances extracted:", nrow(all_extracted_variables), "\n")
cat("Unique variables:", length(unique(all_extracted_variables$variable_name)), "\n\n")

# Generate summary statistics ----------------------------------------

# Variables by column source
cat("Variables by column source:\n")
column_counts <- table(all_extracted_variables$column_source)
for (i in 1:length(column_counts)) {
  cat("-", names(column_counts)[i], ":", column_counts[i], "\n")
}
cat("\n")

# Most common measurement types
cat("Most common measurement types:\n")
type_counts <- table(all_extracted_variables$measurement_type)
type_counts <- sort(type_counts, decreasing = TRUE)
print(head(type_counts, 10))
cat("\n")

# Create unique variable list for categorization
unique_variables <- all_extracted_variables %>%
  select(variable_name, measurement_type, description) %>%
  distinct() %>%
  arrange(variable_name)

# Add empty category column
unique_variables$category <- ""
unique_variables$notes <- ""

cat("Unique variables for categorization:", nrow(unique_variables), "\n\n")

# Save results -------------------------------------------------------

# Function to save CSV with error handling
save_csv_safely <- function(data, filepath, description) {
  tryCatch({
    write_csv(data, filepath)
    cat("✓", description, "saved successfully\n")
  }, error = function(e) {
    write.csv(data, filepath, row.names = FALSE)
    cat("✓", description, "saved with base R (readr failed)\n")
  })
}

# Save all extracted variables
save_csv_safely(
  all_extracted_variables,
  file.path(output_folder, "all_extracted_variables.csv"),
  "All extracted variables"
)

# Save unique variables for manual categorization
save_csv_safely(
  unique_variables,
  file.path(output_folder, "unique_variables_for_categorization.csv"),
  "Unique variables list (for manual categorization)"
)

# Create and save summary
summary_stats <- data.frame(
  Statistic = c("Total Studies", "Total Variable Instances", "Unique Variables", 
                "Variables from Demographics", "Variables from Economics", 
                "Variables from Environmental", "Variables from Distance", "Variables from Temporal"),
  Count = c(
    length(unique(all_extracted_variables$study_title)),
    nrow(all_extracted_variables),
    nrow(unique_variables),
    sum(all_extracted_variables$column_source == "DEMOGRAPHIC & SOCIAL VARIABLES"),
    sum(all_extracted_variables$column_source == "ECONOMIC VARIABLES"),
    sum(all_extracted_variables$column_source == "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES"),
    sum(all_extracted_variables$column_source == "DISTANCE & ACCESSIBILITY VARIABLES"),
    sum(all_extracted_variables$column_source == "TEMPORAL & CONTROL VARIABLES")
  ),
  stringsAsFactors = FALSE
)

save_csv_safely(
  summary_stats,
  file.path(output_folder, "extraction_summary.csv"),
  "Extraction summary statistics"
)

# Show sample of results
cat("=== SAMPLE OF EXTRACTED VARIABLES ===\n")
sample_vars <- head(all_extracted_variables[, c("variable_name", "measurement_type", "column_source")], 20)
print(sample_vars)

cat("\n", rep("=", 60), "\n")
cat("VARIABLE EXTRACTION COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 60), "\n")
cat("Files saved in:", output_folder, "\n\n")
cat("Next steps:\n")
cat("1. Review 'unique_variables_for_categorization.csv'\n")
cat("2. Add appropriate categories in the 'category' column\n")
cat("3. Categories might include: Demographics, Economic, Environmental, Temporal, Distance, etc.\n")
cat("4. Save the categorized file and provide it back for grouping analysis\n\n")
cat("Total unique variables to categorize:", nrow(unique_variables), "\n")
