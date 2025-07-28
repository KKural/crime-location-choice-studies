# Improved Variable Extraction Master List Script
# This script extracts ALL variables from ALL sources with enhanced filtering

# Note: Using explicit namespace calls (package::function) instead of loading libraries
# This approach avoids potential namespace conflicts and makes dependencies clear

# Set up input/output folder configuration -----------------
analysis_date <- Sys.Date()  # Use today's date
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Variable_Analysis_Improved")  # Today's folder for output

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

# Enhanced function to check if text represents a valid variable
is_valid_variable <- function(clean_item) {
  if (is.na(clean_item) || clean_item == "" || nchar(clean_item) < 3) {
    return(FALSE)
  }
  
  clean_lower <- tolower(clean_item)
  
  # Skip entries that start with statistical markers or numerical patterns
  if (stringr::str_detect(clean_item, "^\\d+\\.\\d+\\s+|^\\d+%\\s+|^\\d+\\)|^\\d+,\\d+|^\\d+\\s*[-–]\\s*\\d+|^\\d+\\.\\d+,|^\\d+\\.\\d+\\)|^\\(\\d+|^0\\.\\d+|^\\d+\\s*$|^\\d+\\.\\d+$")) {
    return(FALSE)
  }
  
  # Skip odds ratios and statistical coefficients with very specific patterns
  if (stringr::str_detect(clean_lower, "^or\\s*=|odds\\s*=|^\\d+\\.\\d+\\s+(for|times|more|likely|increase|decrease)|^\\d+\\.\\d+\\s+and\\s+\\d+|coefficient\\s*=|beta\\s*=|exp\\s*\\(|^e\\s*=")) {
    return(FALSE)
  }
  
  # Skip percentage results and statistical findings
  if (stringr::str_detect(clean_lower, "\\d+%\\s+(each\\s+unit|increase|decrease|for|of\\s+identified|of\\s+cases)|by\\s+\\d+\\.\\d+%|increases\\s+the\\s+(odds|rate)|decreases\\s+(odds|choice)|\\d+\\s+to\\s+\\d+\\s+times")) {
    return(FALSE)
  }
  
  # Skip model performance metrics and fit statistics
  if (stringr::str_detect(clean_lower, "pseudo[\\s-]*r\\s*2?\\s*=|r\\s*2?\\s*=\\s*0\\.|mcfadden|log[\\s-]*likelihood|aic\\s*=|bic\\s*=|gvif|variance\\s+inflation|model\\s+fit|goodness.*fit|root\\s+likelihood")) {
    return(FALSE)
  }
  
  # Skip area measurements and spatial unit descriptions
  if (stringr::str_detect(clean_lower, "km²|km2|kmâ²|square\\s+km|square\\s+kilometers|surface\\s+area|hectares|acres|area\\s+of|average\\s+surface|total\\s+area|\\d+\\s+km|\\d+\\s+square|million\\s+inhabitants|average\\)|\\(average")) {
    return(FALSE)
  }
  
  # Skip methodological terms and study design elements
  if (stringr::str_detect(clean_lower, "conditional\\s+logit|logit\\s+model|estimation\\s+method|robustness\\s+check|information\\s+criteria|likelihood\\s+ratio|model\\s+specification|study\\s+design|data\\s+collection|sampling\\s+approach")) {
    return(FALSE)
  }
  
  # Skip DOI numbers and citations
  if (stringr::str_detect(clean_lower, "doi|\\d{4}/|journal\\.|pone\\.|pdf|\\d+\\.\\d+/")) {
    return(FALSE)
  }
  
  # Skip statistical test results and p-values
  if (stringr::str_detect(clean_lower, "chi2|p\\s*<|p\\s*=|wald\\s+test|likelihood\\s+ratio\\s+test|degrees\\s+of\\s+freedom|chi.*square")) {
    return(FALSE)
  }
  
  # Skip complex statistical statements that describe relationships
  if (stringr::str_detect(clean_lower, "unit\\s+increase.*increases.*odds|kilometer\\s+decrease.*increases.*odds|relationship\\s+between|moderates\\s+the\\s+relationship|factor\\s+of\\s+\\d+|inversely\\s+related|more\\s+likely\\s+to\\s+offend")) {
    return(FALSE)
  }
  
  # Skip entries that describe model selection or comparison
  if (stringr::str_detect(clean_lower, "could\\s+not\\s+identify|underreporting\\s+of|exclusion\\s+of|selection\\s+of|seemingly\\s+unrelated|series\\s+of\\s+models|fixed\\s+effects\\s+analysis|model\\s+\\d+\\s*=")) {
    return(FALSE)
  }
  
  # Skip entries with mathematical notation or special characters indicating statistical results
  if (stringr::str_detect(clean_item, "ã—|â²|âĸ|â\\.|×\\s*10|e\\s*ï¢|\\^\\d+|\\s*=\\s*\\d+|ï¢|â|ã|\\s*>\\s*\\d+")) {
    return(FALSE)
  }
  
  # Skip entries that are clearly measurement results rather than variable names
  if (stringr::str_detect(clean_lower, "maximum\\s+vif|\\d+\\.\\d+\\s+\\(.*\\)|\\d+\\.\\d+.*full.*model|\\d+\\.\\d+.*restricted.*model|\\d+\\.\\d+.*sample")) {
    return(FALSE)
  }
  
  # Skip temporal measurements with specific numerical patterns
  if (stringr::str_detect(clean_lower, "\\d+\\s+(days|hours|minutes|weeks|months|years)\\s+(difference|within|since|elapsed|before|after)|same\\s+(weekend|weekday)\\s+day")) {
    return(FALSE)
  }
  
  # Skip administrative/spatial unit classifications
  if (stringr::str_detect(clean_lower, "statistical\\s+districts|census\\s+tracts|super\\s+output\\s+areas|administrative\\s+units|choice\\s+set|alternative\\s+selection|single-offender\\s+selection|bootstrapping\\s+procedure")) {
    return(FALSE)
  }
  
  # Skip entries that start with brackets or parentheses (often statistical results)
  if (stringr::str_detect(clean_item, "^\\[|^\\(\\d+")) {
    return(FALSE)
  }
  
  # Skip entries that are just statistical abbreviations or model notation
  if (stringr::str_detect(clean_item, "^[A-Z]\\s*ij$|^[A-Z]\\s*\\d+$|^u\\s*\\d+$|^[A-Z]\\.[A-Z\\.]+$|^[A-Z]+\\s*\\d*$") && nchar(clean_item) < 6) {
    return(FALSE)
  }
  
  # Skip entries describing data processing or methodological procedures
  if (stringr::str_detect(clean_lower, "data\\s+preparation|variable\\s+construction|estimation\\s+procedure")) {
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
  parts <- stringr::str_split(clean_item, "\\|", simplify = TRUE)
  
  # Extract variable name (first part before |)
  variable_name <- stringr::str_trim(parts[1])
  
  # Extract measurement type (second part after |, if exists)
  measurement_type <- if(ncol(parts) > 1 && parts[2] != "") {
    standardize_measurement_type(parts[2])
  } else {
    standardize_measurement_type(variable_name)  # Infer from variable name
  }
  
  return(list(
    variable_name = variable_name,
    measurement_type = measurement_type
  ))
}

# Variable extraction from all columns and studies ------------------------

all_variables <- data.frame(
  study_title = character(0),
  year = numeric(0),
  column_source = character(0),
  variable_name = character(0),
  measurement_type = character(0),
  stringsAsFactors = FALSE
)

# Define the columns to extract variables from (excluding Title, Year, Authors)
variable_columns <- names(df_combined)[!names(df_combined) %in% c("Title", "Year", "Authors")]

cat("Extracting variables from columns:\n")
for (col_name in variable_columns) {
  cat("-", col_name, "\n")
}
cat("\n")

# Process each study (row) and each variable column
for (i in 1:nrow(df_combined)) {
  study_title <- df_combined$Title[i]
  year <- df_combined$Year[i]
  
  # Process each variable column
  for (col_name in variable_columns) {
    cell_content <- df_combined[[col_name]][i]
    
    if (!is.na(cell_content) && cell_content != "") {
      # Split by semicolon to separate multiple variables
      items <- stringr::str_split(cell_content, ";", simplify = FALSE)[[1]]
      items <- stringr::str_trim(items)
      items <- items[items != "" & !is.na(items)]
      
      # Extract each variable
      for (item in items) {
        extracted <- extract_clean_variable(item)
        
        if (!is.na(extracted) && !is.null(extracted$variable_name)) {
          # Add to results
          all_variables <- rbind(all_variables, data.frame(
            study_title = study_title,
            year = year,
            column_source = col_name,
            variable_name = extracted$variable_name,
            measurement_type = extracted$measurement_type,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Progress indicator
  if (i %% 20 == 0) {
    cat("Processed", i, "studies...\n")
  }
}

cat("Variable extraction completed!\n")
cat("Total variable instances extracted:", nrow(all_variables), "\n\n")

# Generate summary statistics using base R -------------------------------

# Count by measurement type
measurement_counts <- table(all_variables$measurement_type)
cat("Variables by measurement type:\n")
for (i in 1:length(measurement_counts)) {
  cat("-", names(measurement_counts)[i], ":", measurement_counts[i], "\n")
}
cat("\n")

# Count by column source
column_counts <- table(all_variables$column_source)
cat("Variables by column source (top 10):\n")
top_columns <- head(sort(column_counts, decreasing = TRUE), 10)
for (i in 1:length(top_columns)) {
  cat("-", names(top_columns)[i], ":", top_columns[i], "\n")
}
cat("\n")

# Create unique variable list for manual categorization
unique_variables <- unique(all_variables[, c("variable_name", "measurement_type")])
unique_variables <- unique_variables[order(unique_variables$variable_name), ]

# Add empty category column for manual categorization
unique_variables$category <- ""

cat("Unique variables found:", nrow(unique_variables), "\n")

# Save outputs using base R functions with error handling ----------------

save_csv_safe <- function(data, filepath, description) {
  tryCatch({
    # Try readr first
    readr::write_csv(data, filepath)
    cat("✓", description, "saved successfully using readr\n")
  }, error = function(e) {
    # Fallback to base R
    write.csv(data, filepath, row.names = FALSE)
    cat("✓", description, "saved successfully using base R (readr failed)\n")
  })
}

# Save all three output files
save_csv_safe(
  all_variables, 
  file.path(output_folder, "all_variable_instances.csv"),
  "All variable instances"
)

save_csv_safe(
  unique_variables, 
  file.path(output_folder, "unique_variables_for_categorization.csv"),
  "Unique variables list (for manual categorization)"
)

# Create summary table using base R
summary_data <- data.frame(
  Metric = c("Total studies", "Total variable instances", "Unique variables", 
             "Most common measurement type", "Most productive column"),
  Value = c(
    length(unique(all_variables$study_title)),
    nrow(all_variables),
    nrow(unique_variables),
    names(which.max(measurement_counts)),
    names(which.max(column_counts))
  ),
  stringsAsFactors = FALSE
)

save_csv_safe(
  summary_data, 
  file.path(output_folder, "extraction_summary.csv"),
  "Extraction summary"
)

# Final instructions for user
cat("\n" , rep("=", 60), "\n")
cat("VARIABLE EXTRACTION COMPLETED!\n")
cat(rep("=", 60), "\n")
cat("Files saved in folder:", output_folder, "\n\n")
cat("Next steps:\n")
cat("1. Open 'unique_variables_for_categorization.csv'\n")
cat("2. Review each variable and assign appropriate categories in the 'category' column\n")
cat("3. Save the file and provide it back for grouping analysis\n")
cat("4. Categories could include: Demographics, Economic, Environmental, Temporal, etc.\n\n")
cat("Total unique variables to categorize:", nrow(unique_variables), "\n")
