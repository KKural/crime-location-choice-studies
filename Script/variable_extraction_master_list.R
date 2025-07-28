# Variable Extraction Master List Script
# This script extracts ALL variables from ALL sources, then provides a complete list for categorization

# Note: Using explicit namespace calls (package::function) instead of loading libraries
# This approach avoids potential namespace conflicts and makes dependencies clear

# Set up input/output folder configuration -----------------
analysis_date <- Sys.Date()  # Use today's date
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Variable_Analysis")  # Today's folder for output

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Data reading and initial processing -----------------------------------------

# List of CSV files to merge
combined_csv_files <- c(
  "Data/Study_Identification_Spatial_Units.csv",
  "Data/Theoretical_Framework_Methodology.csv", 
  "Data/Temporal_Variables_Findings.csv",
  "Data/Scale_Effects_Limitations.csv",
  "Data/study area size.csv"
)

# Function to clean column names (remove supporting/reasoning columns)
clean_column_names <- function(df) {
  main_cols <- colnames(df)[!grepl("tables|DOI|Venue|Citation", colnames(df))]
  return(df[, main_cols])
}

# Function to remove duplicate columns (Authors, Year) from subsequent files
remove_duplicate_cols <- function(df, is_first_file = FALSE) {
  if (is_first_file) {
    return(df)
  } else {
    # Remove Authors and Year columns from subsequent files to avoid duplicates
    cols_to_remove <- c("Authors", "Year")
    remaining_cols <- setdiff(names(df), cols_to_remove)
    return(df[, remaining_cols])
  }
}

# Read and merge all CSV files
df_combined <- NULL

for (i in 1:length(combined_csv_files)) {
  if (file.exists(combined_csv_files[i])) {
    temp_df <- readr::read_csv(combined_csv_files[i], show_col_types = FALSE)
    temp_df <- clean_column_names(temp_df)
    
    if (is.null(df_combined)) {
      # First file becomes the base dataset (keep Authors and Year)
      df_combined <- temp_df
    } else {
      # Remove duplicate columns from subsequent files
      temp_df <- remove_duplicate_cols(temp_df, is_first_file = FALSE)
      # Merge subsequent files by Title
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
  }
}

# Print basic info
cat("Total studies:", nrow(df_combined), "\n")
cat("Total columns:", ncol(df_combined), "\n\n")

# Select required columns
df_combined <- df_combined |>
  dplyr::select(
    Title, Year, Authors,
    `BASIC STUDY IDENTIFICATION`, `TEMPORAL SCOPE & DATA SOURCES`, 
    `study area size`,`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`, `STUDY CONTEXT & GEOGRAPHY`, 
    `SAMPLING & CHOICE SETS`, `THEORETICAL FRAMEWORK & OBJECTIVES`, 
    `STUDY DESIGN & METHODOLOGY`, `DATA PREPARATION & PROCESSING`, `DEMOGRAPHIC & SOCIAL VARIABLES`, 
    `ECONOMIC VARIABLES`, `TEMPORAL & CONTROL VARIABLES`,`MODEL FIT & PERFORMANCE METRICS`, 
    `MAJOR FINDINGS & RESULTS`, `ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES`, `DISTANCE & ACCESSIBILITY VARIABLES`,
    `SCALE EFFECTS & SPATIAL FINDINGS`, `DATA LIMITATIONS & METHODOLOGICAL ISSUES`, 
    `GENERALIZABILITY & COMPARATIVE LIMITATIONS`, `IMPLICATIONS & FUTURE DIRECTIONS`
  )

# Variable Extraction Functions --------------------------------------------

# Function to check if text represents a valid variable (not methodological terms or area measurements)
is_valid_variable <- function(clean_item) {
  if (is.na(clean_item) || clean_item == "" || nchar(clean_item) < 3) {
    return(FALSE)
  }
  
  clean_lower <- tolower(clean_item)
  
  # Skip area measurements and spatial descriptions
  if (stringr::str_detect(clean_lower, "km²|km2|kmâ²|square\\s+km|square\\s+kilometers|surface\\s+area|hectares|acres|area\\s+of|average\\s+surface|total\\s+area|\\d+\\s+km|\\d+\\s+square|average\\)|\\(average")) {
    return(FALSE)
  }
  
  # Skip methodological terms and study design elements
  if (stringr::str_detect(clean_lower, "conditional\\s+logit|logit\\s+model|variance\\s+inflation|gvif|model\\s+specification|estimation\\s+method|robustness\\s+check|goodness.*fit|information\\s+criteria|pseudo\\s+r|mcfadden|likelihood\\s+ratio|odds\\s+ratio|model\\s+fit")) {
    return(FALSE)
  }
  
  # Skip DOI numbers and citations
  if (stringr::str_detect(clean_lower, "doi|\\d{4}/|journal\\.|pone\\.|pdf|\\d+\\.\\d+/")) {
    return(FALSE)
  }
  
  # Skip statistical results and model outputs - ENHANCED
  if (stringr::str_detect(clean_lower, "^\\d+\\.\\d+\\s+(for|times|more|likely|increase|decrease)|^or\\s*=|chi2|p\\s*<|p\\s*=|\\d+%\\s+(for|increase|decrease)|\\d+\\.\\d+\\s+times")) {
    return(FALSE)
  }
  
  # Skip entries that are clearly statistical results or findings - ENHANCED
  if (stringr::str_detect(clean_lower, "\\d+\\.\\d+\\s+(and|to|for)\\s+\\d+|\\d+%\\s+of\\s+(identified|cases)|million\\s+inhabitants|degrees\\s+of\\s+freedom")) {
    return(FALSE)
  }
  
  # Skip entries that start with numbers, percentages, or statistical measures - ENHANCED
  if (stringr::str_detect(clean_item, "^\\d+(\\.\\d+)?%|^\\d+(\\.\\d+)?;|^\\d+(\\.\\d+)?\\s+(to|for|and|times|more|less|increase|decrease|factor|unit)")) {
    return(FALSE)
  }
  
  # Skip entries with odds ratios, correlations, and statistical coefficients - NEW
  if (stringr::str_detect(clean_lower, "or\\s*=|odds\\s+ratio|coefficient|correlation|r\\s*=|\\d+\\.\\d+\\s+for\\s+|factor\\s+of\\s+\\d+|\\d+\\.\\d+\\s+\\(|vif\\s+of|maximum\\s+vif")) {
    return(FALSE)
  }
  
  # Skip model performance and statistical test results - NEW
  if (stringr::str_detect(clean_lower, "model\\s+\\d+\\s*=|pseudo.*r.*\\d+|likelihood.*\\d+|wald\\s+test|root\\s+likelihood|mcfadden|\\d+\\.\\d+\\s+\\(.*\\)|chi.*square")) {
    return(FALSE)
  }
  
  # Skip entries that are clearly numerical results or findings - NEW
  if (stringr::str_detect(clean_item, "^\\d+\\.\\d+.*\\)|\\)\\s*$|^\\d+\\s*\\)|^\\d+,\\d+|\\d+\\.\\d+.*\\s+(model|full|restricted|sample)")) {
    return(FALSE)
  }
  
  # Skip entries with mathematical expressions or statistical notation - NEW
  if (stringr::str_detect(clean_item, "ã—|â²|âĸ|â\\.|×\\s*10|e\\s*ï¢|\\^\\d+|\\s*=\\s*\\d+")) {
    return(FALSE)
  }
  
  # Skip entries that are just statistical abbreviations or codes - NEW
  if (stringr::str_detect(clean_item, "^[A-Z]\\s*ij$|^[A-Z]\\s*\\d+$|^u\\s*\\d+$|^[A-Z]\\.[A-Z\\.]+$")) {
    return(FALSE)
  }
  
  # Skip entries describing time periods, distances with numerical values - NEW
  if (stringr::str_detect(clean_lower, "\\d+\\s+(days|hours|minutes|weeks|months|years)\\s+(difference|within|since|elapsed)|same\\s+(weekend|weekday)\\s+day")) {
    return(FALSE)
  }
  
  # Skip spatial unit names and administrative divisions
  if (stringr::str_detect(clean_lower, "statistical\\s+districts|census\\s+tracts|super\\s+output\\s+areas|neighborhoods|wards|precincts|administrative\\s+units")) {
    return(FALSE)
  }
  
  # Skip study design terms
  if (stringr::str_detect(clean_lower, "single-offender\\s+selection|choice\\s+set|alternative\\s+selection|sampling\\s+approach|data\\s+collection|study\\s+design|bootstrapping\\s+procedure")) {
    return(FALSE)
  }
  
  # Skip entries that start with numbers followed by spatial units
  if (stringr::str_detect(clean_item, "^\\d+(\\.\\d+)?\\s+(km|square|hectares|acres)")) {
    return(FALSE)
  }
  
  # Skip entries that are just numbers or number ranges
  if (stringr::str_detect(clean_item, "^\\d+\\s*$|^\\d+\\.\\d+$|^\\d+\\s*-\\s*\\d+$")) {
    return(FALSE)
  }
  
  # Skip methodological procedures and data processing terms
  if (stringr::str_detect(clean_lower, "data\\s+collection|estimation\\s+procedure|model\\s+specification|variable\\s+construction|data\\s+preparation")) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Function to standardize measurement types
standardize_measurement_type <- function(text) {
  if (is.na(text) || text == "") return("Unknown")
  
  text_lower <- tolower(text)
  
  # Check for specific measurement type keywords
  if (stringr::str_detect(text_lower, "percentage|percent|%|proportion|ratio")) {
    return("Percentage")
  } else if (stringr::str_detect(text_lower, "binary|yes/no|dummy|categorical|dichotomous")) {
    return("Binary")
  } else if (stringr::str_detect(text_lower, "rate|per\\s+\\d+|frequency")) {
    return("Rate")
  } else if (stringr::str_detect(text_lower, "distance|km|meter|mile|proximity|accessibility")) {
    return("Distance")
  } else if (stringr::str_detect(text_lower, "number|count|numeric|integer|population|residents|units|size")) {
    return("Number")
  } else if (stringr::str_detect(text_lower, "index|score|scale")) {
    return("Index")
  } else if (stringr::str_detect(text_lower, "density|per\\s+km")) {
    return("Density")
  } else {
    return("Number")  # Default fallback
  }
}

# Function to extract clean variable name and measurement type
extract_clean_variable <- function(item_text) {
  if (is.na(item_text) || item_text == "") return(NA)
  
  # Remove the number prefix (1., 2., etc.)
  clean_item <- stringr::str_remove(item_text, "^\\d+\\.\\s*")
  clean_item <- stringr::str_trim(clean_item)
  
  # Check if this is a valid variable
  if (!is_valid_variable(clean_item)) {
    return(NA)
  }
  
  # Split by pipe symbol to get parts
  parts <- stringr::str_split(clean_item, "\\|")[[1]]
  parts <- stringr::str_trim(parts)
  
  if (length(parts) == 0) return(NA)
  
  # Extract variable name (first part)
  variable_name <- parts[1]
  variable_name <- stringr::str_trim(variable_name)
  
  # Additional check on variable name
  if (!is_valid_variable(variable_name)) {
    return(NA)
  }
  
  # Extract measurement type from all parts
  full_text <- paste(parts, collapse = " ")
  measurement_type <- standardize_measurement_type(full_text)
  
  # Format as "Variable Name | Measurement Type"
  return(paste0(variable_name, " | ", measurement_type))
}

# Function to extract variables from text
extract_variables_from_text <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(character())
  }
  
  # Extract numbered items (1., 2., 3., etc.)
  items <- stringr::str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
  
  if (length(items) == 0) {
    return(character())
  }
  
  variables <- character()
  
  for (item in items) {
    clean_var <- extract_clean_variable(item)
    if (!is.na(clean_var) && clean_var != "") {
      variables <- c(variables, clean_var)
    }
  }
  
  return(variables)
}

# Master Variable Collection Function ----------------------------------------

# Function to collect ALL variables from ALL sources across ALL studies 
collect_all_variables_master <- function(df) {
  
  # Initialize master list
  master_variable_list <- data.frame(
    Title = character(),
    Variable = character(),
    Source_Column = character(),
    Study_Index = numeric(),
    stringsAsFactors = FALSE
  )
  
  # List of all columns that might contain variables
  variable_columns <- c(
    "DEMOGRAPHIC & SOCIAL VARIABLES",
    "ECONOMIC VARIABLES", 
    "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
    "DISTANCE & ACCESSIBILITY VARIABLES",
    "TEMPORAL & CONTROL VARIABLES",
    "BASIC STUDY IDENTIFICATION",
    "TEMPORAL SCOPE & DATA SOURCES",
    "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION",
    "STUDY CONTEXT & GEOGRAPHY",
    "SAMPLING & CHOICE SETS",
    "THEORETICAL FRAMEWORK & OBJECTIVES",
    "STUDY DESIGN & METHODOLOGY",
    "DATA PREPARATION & PROCESSING",
    "MODEL FIT & PERFORMANCE METRICS",
    "MAJOR FINDINGS & RESULTS",
    "SCALE EFFECTS & SPATIAL FINDINGS",
    "DATA LIMITATIONS & METHODOLOGICAL ISSUES",
    "GENERALIZABILITY & COMPARATIVE LIMITATIONS",
    "IMPLICATIONS & FUTURE DIRECTIONS"
  )
  
  # Process each study
  for (i in 1:nrow(df)) {
    title <- df$Title[i]
    cat("Processing study", i, "of", nrow(df), ":", substr(title, 1, 50), "...\n")
    
    # Extract variables from each column
    for (col in variable_columns) {
      if (col %in% names(df)) {
        text <- df[[col]][i]
        if (!is.na(text) && text != "") {
          variables <- extract_variables_from_text(text)
          
          if (length(variables) > 0) {
            # Add to master list
            new_rows <- data.frame(
              Title = rep(title, length(variables)),
              Variable = variables,
              Source_Column = rep(col, length(variables)),
              Study_Index = rep(i, length(variables)),
              stringsAsFactors = FALSE
            )
            master_variable_list <- rbind(master_variable_list, new_rows)
          }
        }
      }
    }
  }
  
  return(master_variable_list)
}

# Execute Master Variable Collection --------------------------------------

cat("Starting master variable collection...\n")
cat("This will extract ALL variables from ALL studies and ALL columns.\n\n")

# Collect all variables
master_variables <- collect_all_variables_master(df_combined)

cat("\n=== MASTER VARIABLE COLLECTION COMPLETE ===\n")
cat("Total variable instances found:", nrow(master_variables), "\n")
cat("Unique variables found:", length(unique(master_variables$Variable)), "\n\n")

# Create summary statistics (using base R to avoid dplyr dependency issues)
variable_counts <- table(master_variables$Variable)
variable_summary <- data.frame(
  Variable = names(variable_counts),
  Frequency = as.numeric(variable_counts),
  stringsAsFactors = FALSE
)

# Add study counts and source columns for each variable
variable_summary$Studies <- 0
variable_summary$Source_Columns <- ""

for (i in 1:nrow(variable_summary)) {
  var_name <- variable_summary$Variable[i]
  var_rows <- master_variables[master_variables$Variable == var_name, ]
  variable_summary$Studies[i] <- length(unique(var_rows$Title))
  variable_summary$Source_Columns[i] <- paste(unique(var_rows$Source_Column), collapse = "; ")
}

# Sort by frequency
variable_summary <- variable_summary[order(-variable_summary$Frequency), ]

cat("Top 20 most frequent variables:\n")
print(head(variable_summary, 20))

# Save outputs with better error handling
output_file_all <- file.path(output_folder, paste0(format(analysis_date, "%Y%m%d"), "_all_variables_master_list.csv"))
output_file_summary <- file.path(output_folder, paste0(format(analysis_date, "%Y%m%d"), "_variable_summary.csv"))
output_file_unique <- file.path(output_folder, paste0(format(analysis_date, "%Y%m%d"), "_unique_variables_for_categorization.csv"))

# Save all variable instances
tryCatch({
  readr::write_csv(master_variables, output_file_all)
  cat("\nAll variable instances saved to:", output_file_all, "\n")
}, error = function(e) {
  # Fallback to base R write.csv
  write.csv(master_variables, output_file_all, row.names = FALSE)
  cat("\nAll variable instances saved to:", output_file_all, "(using base R)\n")
})

# Save summary statistics
tryCatch({
  readr::write_csv(variable_summary, output_file_summary)
  cat("Variable summary saved to:", output_file_summary, "\n")
}, error = function(e) {
  # Fallback to base R write.csv
  write.csv(variable_summary, output_file_summary, row.names = FALSE)
  cat("Variable summary saved to:", output_file_summary, "(using base R)\n")
})

# Create unique variable list for manual categorization
unique_variables <- data.frame(
  Variable = sort(unique(master_variables$Variable)),
  Category = "", 
  Notes = "",
  stringsAsFactors = FALSE
)

tryCatch({
  readr::write_csv(unique_variables, output_file_unique)
  cat("Unique variables list for categorization saved to:", output_file_unique, "\n")
}, error = function(e) {
  # Fallback to base R write.csv
  write.csv(unique_variables, output_file_unique, row.names = FALSE)
  cat("Unique variables list for categorization saved to:", output_file_unique, "(using base R)\n")
})

cat("\n=== READY FOR MANUAL CATEGORIZATION ===\n")
cat("Please review and categorize the variables in:", output_file_unique, "\n")
cat("Add appropriate categories (demographic, economic, environmental, distance, temporal, uncategorized) in the 'Category' column.\n")
cat("Then run the categorization script with your updated file.\n")

# Print column source breakdown (using base R)
cat("\n=== VARIABLES BY SOURCE COLUMN ===\n")
column_counts <- table(master_variables$Source_Column)
column_breakdown <- data.frame(
  Source_Column = names(column_counts),
  Total_Variables = as.numeric(column_counts),
  stringsAsFactors = FALSE
)

# Calculate unique variables per column
column_breakdown$Unique_Variables <- 0
for (i in 1:nrow(column_breakdown)) {
  col_name <- column_breakdown$Source_Column[i]
  col_vars <- master_variables[master_variables$Source_Column == col_name, "Variable"]
  column_breakdown$Unique_Variables[i] <- length(unique(col_vars))
}

# Sort by total variables
column_breakdown <- column_breakdown[order(-column_breakdown$Total_Variables), ]
print(column_breakdown)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Review the output files and provide your categorized variable list.\n")
