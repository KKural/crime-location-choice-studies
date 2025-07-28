# Extraction Script

# Load required libraries -----------------------------------------------------
library(dplyr)
library(stringr)
library(readr)
library(here)
library(lubridate)
library(writexl)

# Set up input/output folder configuration -----------------
analysis_date <- Sys.Date()  # Use today's date
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Analysis & Results")  # Today's folder for output

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}


# Output folder creation and utility functions --------------------------------

# Function to create a folder with a date argument
make_folder <- function(date = analysis_date, subfolder = NULL) {
  # Use the fixed analysis date instead of current date
  folder_name <- format(date, "%Y%m%d")
  main_folder_name <- paste0(folder_name, "_Analysis & Results")
  
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)
  }
  
  return(full_folder_path)
}

# Create a function to save output with date
custom_save <- function(data, folder_name, file_description, save_function, file_extension = ".csv", ...) {
  # Use analysis_date instead of current date
  current_date <- format(analysis_date, "%Y%m%d")
  
  # Ensure file description has the correct extension
  if (!grepl(paste0("\\", file_extension, "$"), file_description)) {
    file_description <- paste0(file_description, file_extension)
  }
  
  # Create the file name using the date and the file description
  file_name <- paste0(current_date, "_", file_description)
  
  # Define the path for the output file
  file_path <- here::here(folder_name, file_name)
  
  # Use the provided save function
  save_function(data, file_path, ...)
  
  return(file_path)  # Return the file path
}

# Create output folder
output_folder <- make_folder()
folder_name <- output_folder

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

# Read and merge all CSV files
df_combined <- NULL
for (i in 1:length(combined_csv_files)) {
  cat("Reading file", i, ":", combined_csv_files[i], "\n")
  
  if (file.exists(combined_csv_files[i])) {
    temp_df <- read_csv(combined_csv_files[i], show_col_types = FALSE)
    temp_df <- clean_column_names(temp_df)
    
    if (is.null(df_combined)) {
      # First file becomes the base dataset
      df_combined <- temp_df
    } else {
      # Merge subsequent files by Title
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
    
    cat("After merging file", i, "- Rows:", nrow(df_combined), "Columns:", ncol(df_combined), "\n")
  } else {
    cat("File not found:", combined_csv_files[i], "\n")
  }
}

raw_dataset_rows <- nrow(df_combined)
cat("Final merged dataset - Rows:", nrow(df_combined), "Columns:", ncol(df_combined), "\n")

# Print available columns 
print(names(df_combined))
    if (is.null(df_combined)) {
      # First file becomes the base dataset
      df_combined <- temp_df
    } else {
      # Merge subsequent files by Title
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
    
    cat("After merging file", i, "- Rows:", nrow(df_combined), "Columns:", ncol(df_combined), "\n")
  } else {
    cat("File not found:", combined_csv_files[i], "\n")
  }
}

raw_dataset_rows <- nrow(df_combined)
cat("Final merged dataset - Rows:", nrow(df_combined), "Columns:", ncol(df_combined), "\n")

# Print available columns 
cat("Available columns after merging all files:\n")
print(names(df_combined))_cols])
  }
}

# Read and merge all CSV files
df_combined <- NULL

for (i in 1:length(combined_csv_files)) {
  if (file.exists(combined_csv_files[i])) {
    temp_df <- read_csv(combined_csv_files[i], show_col_types = FALSE)
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

# Print column names
print(names(df_combined))
nrow(df_combined)
ncol(df_combined)


# Save the combined data set
custom_save(df_combined, output_folder, "combined_dataset_merged", readr::write_csv)


# Data Extraction and Processing Functions--------------------------------------
# Enhanced Data Collection Period Extraction Function
extract_data_collection_period <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(NA)
  }
  
  # patterns for data collection period
  data_collection_patterns <- c(
    # Explicit data collection mentions with markdown formatting
    "\\*\\*Data Collection Period:\\*\\*\\s*([^\\n]+)",
    "- \\*\\*Data Collection Period:\\*\\*\\s*([^\\n]+)",
    "Data Collection Period:\\s*([^\\n]+)",
    "- Data Collection Period:\\s*([^\\n]+)",
    
    # Alternative phrasings
    "\\*\\*Data collection period:\\*\\*\\s*([^\\n]+)",
    "data collection period:\\s*([^\\n]+)",
    "Data was collected\\s*([^\\n]+)",
    "data was collected\\s*([^\\n]+)",
    "Data collection occurred\\s*([^\\n]+)",
    "data collection occurred\\s*([^\\n]+)",
    "Data gathered\\s*([^\\n]+)",
    "data gathered\\s*([^\\n]+)",
    "Database accessed\\s*([^\\n]+)",
    "database accessed\\s*([^\\n]+)",
    "Data obtained\\s*([^\\n]+)",
    "data obtained\\s*([^\\n]+)",
    "Data available\\s*([^\\n]+)",
    "data available\\s*([^\\n]+)",
    
    # Temporal phrases indicating data collection timing
    "\\b(collected|gathered|obtained|accessed|extracted|retrieved)\\s+(?:in|during|from|between)\\s+([^\\n\\.,;]+)",
    "\\b(data|information|records)\\s+(?:was|were)\\s+(?:collected|gathered|obtained|accessed|extracted|retrieved)\\s+(?:in|during|from|between)?\\s*([^\\n\\.,;]+)",
    
    # Survey/fieldwork specific patterns
    "(?:survey|fieldwork|interviews?)\\s+(?:conducted|carried out|performed)\\s+(?:in|during|from|between)?\\s*([^\\n\\.,;]+)",
    "(?:questionnaires?|data collection)\\s+(?:administered|conducted)\\s+(?:in|during|from|between)?\\s*([^\\n\\.,;]+)",
    
    # Database/system access patterns
    "(?:database|system|records?)\\s+(?:accessed|queried|extracted)\\s+(?:in|during|from|between)?\\s*([^\\n\\.,;]+)",
    
    # Patterns for extracting useful info even when explicitly marked as "not mentioned"
    "not\\s+explicitly\\s+mentioned[^,]*,?\\s*but\\s+([^\\n]+)",
    "not\\s+mentioned[^,]*,?\\s*but\\s+([^\\n]+)",
    "not\\s+specified[^,]*,?\\s*but\\s+([^\\n]+)",
    "not\\s+stated[^,]*,?\\s*but\\s+([^\\n]+)",
    
    # Patterns for data availability periods
    "data\\s+(?:was|were)?\\s*available\\s+(?:up\\s+to|until|through)\\s+([^\\n\\.,;]+)",
    "available\\s+(?:up\\s+to|until|through)\\s+([^\\n\\.,;]+)",
    "data\\s+(?:from|covering|spanning)\\s+([^\\n\\.,;]+)",
    "(?:covering|spanning)\\s+(?:the\\s+)?period\\s+([^\\n\\.,;]+)",
    
    # Study timeframe patterns (as fallback)
    "study\\s+(?:conducted|performed|carried\\s+out)\\s+(?:in|during|from|between)\\s+([^\\n\\.,;]+)",
    "(?:conducted|performed|carried\\s+out)\\s+(?:in|during|from|between)\\s+([^\\n\\.,;]+)",
    
    # Very broad year extraction patterns (last resort)
    "\\b((?:19|20)\\d{2}(?:\\s*[-–]\\s*(?:19|20)\\d{2})?)\\b",
    "\\b((?:19|20)\\d{2}\\s+to\\s+(?:19|20)\\d{2})\\b",
    "\\b((?:19|20)\\d{2})\\b",
    "\\b((?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+(?:19|20)\\d{2})\\b",
    "\\b(\\d{1,2}[-/]\\d{1,2}[-/](?:19|20)\\d{2})\\b"
  )
  
  for (pattern in data_collection_patterns) {
    match <- str_match(text, pattern)
    if (!is.na(match[1, 2])) {
      result <- str_trim(match[1, 2])
      
      # Clean up the result
      result <- str_remove_all(result, "\\*\\*")
      result <- str_remove(result, "^[-–]\\s*")
      result <- str_trim(result)
      
      # More permissive filtering - accept results that contain temporal information
      if (result != "" && 
          nchar(result) > 3 &&
          # Accept if it contains years or temporal keywords
          (str_detect(result, "\\b(19|20)\\d{2}\\b") || 
           str_detect(tolower(result), "\\b(january|february|march|april|may|june|july|august|september|october|november|december|spring|summer|fall|autumn|winter|year|month|week|day)\\b") ||
           str_detect(tolower(result), "\\b(available|collected|gathered|obtained|accessed|extracted|retrieved|conducted|performed)\\b")) &&
          # Exclude only completely non-informative results
          !str_detect(tolower(result), "^(n/?a|unclear|unknown|not\\s+available)$")) {
        return(result)
      }
    }
  }
  
  return(NA)
}

# Extract field information from text using regex patterns
extract_all_fields_improved <- function(text, field_names) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(setNames(rep(NA, length(field_names)), field_names))
  }
  
  results <- setNames(rep(NA, length(field_names)), field_names)
  
  for (field_name in field_names) {
    patterns <- c(
      paste0("\\*\\*", field_name, ":\\*\\*\\s*\"([^\"]+)\""),
      paste0("- \\*\\*", field_name, ":\\*\\*\\s*\"([^\"]+)\""),
      paste0("\\*\\*", field_name, ":\\*\\*\\s*(.+?)(?=\\s*\\n\\s*-\\s*\\*\\*|\\s*\\n\\s*\\*\\*|$)"),
      paste0("- \\*\\*", field_name, ":\\*\\*\\s*(.+?)(?=\\s*\\n\\s*-\\s*\\*\\*|\\s*\\n\\s*\\*\\*|$)"),
      paste0("\\*\\*", field_name, ":\\*\\*\\s*(.+?)(?=\\n|$)"),
      paste0("- ", field_name, ":\\s*(.+?)(?=\\s*\\n\\s*-|$)"),
      paste0(field_name, ":\\s*(.+?)(?=\\s*\\n\\s*-|$)")
    )
    
    for (pattern in patterns) {
      match <- str_match(text, pattern)
      if (!is.na(match[1, 2])) {
        result <- str_trim(match[1, 2])
        result <- str_remove_all(result, "\\*\\*")
        result <- str_remove(result, "^-\\s*")
        result <- str_trim(result)
        if (result != "" && 
            !str_detect(result, "^Not mentioned$") && 
            !str_detect(result, "^Not explicitly mentioned$") &&
            !str_detect(result, "^N/A$") &&
            !str_detect(result, "^NA$")) {
          results[field_name] <- result
          break
        }
      }
    }
  }
  
  return(results)
}

# Extract variables from text and return them as a single string with count
extract_variables_grouped <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(list(variables = NA, count = 0))
  }
  
  items <- str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
  
  if (length(items) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  variables <- character()
  for (item in items) {
    clean_item <- str_remove(item, "^\\d+\\.\\s*")
    clean_item <- str_trim(clean_item)
    
    if (str_detect(clean_item, "\\|")) {
      parts <- str_split(clean_item, "\\|")[[1]] %>% str_trim()
      if (length(parts) >= 1) {
        var_name <- parts[1]
        variables <- c(variables, var_name)
      }
    } else {
      variables <- c(variables, clean_item)
    }
  }
  
  variables <- unique(variables[variables != ""])
  
  if (length(variables) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  variables_string <- paste(variables, collapse = "; ")
  
  return(list(variables = variables_string, count = length(variables)))
}

# Simple variable extraction function for new structure
extract_variables_comprehensive <- function(df, row_index) {
  all_variables <- character()
  
  # Extract from all text columns
  text_columns <- c("BASIC STUDY IDENTIFICATION", "TEMPORAL SCOPE & DATA SOURCES", 
                   "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION", "STUDY CONTEXT & GEOGRAPHY", 
                   "SAMPLING & CHOICE SETS")
  
  for (col in text_columns) {
    if (col %in% names(df)) {
      text <- df[[col]][row_index]
      if (!is.na(text) && text != "") {
        # Extract numbered items
        items <- str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
        for (item in items) {
          clean_item <- str_remove(item, "^\\d+\\.\\s*")
          clean_item <- str_trim(clean_item)
          if (clean_item != "") {
            all_variables <- c(all_variables, clean_item)
          }
        }
      }
    }
  }
  
  all_variables <- unique(all_variables)
  
  # Simple categorization
  demographic_vars <- all_variables[grepl("age|gender|race|income|education|population|demographic", tolower(all_variables))]
  economic_vars <- all_variables[grepl("economic|income|employment|poverty|wealth", tolower(all_variables))]
  environmental_vars <- all_variables[grepl("environment|land|building|physical|infrastructure", tolower(all_variables))]
  distance_vars <- all_variables[grepl("distance|proximity|accessibility|travel", tolower(all_variables))]
  temporal_vars <- all_variables[grepl("time|temporal|hour|day|week|month|year", tolower(all_variables))]
  
  categorized_vars <- c(demographic_vars, economic_vars, environmental_vars, distance_vars, temporal_vars)
  uncategorized_vars <- setdiff(all_variables, categorized_vars)
  
  return(list(
    demographic_variables = if(length(demographic_vars) > 0) paste(demographic_vars, collapse = "; ") else NA,
    demographic_count = length(demographic_vars),
    economic_variables = if(length(economic_vars) > 0) paste(economic_vars, collapse = "; ") else NA,
    economic_count = length(economic_vars),
    environmental_variables = if(length(environmental_vars) > 0) paste(environmental_vars, collapse = "; ") else NA,
    environmental_count = length(environmental_vars),
    distance_variables = if(length(distance_vars) > 0) paste(distance_vars, collapse = "; ") else NA,
    distance_count = length(distance_vars),
    temporal_variables = if(length(temporal_vars) > 0) paste(temporal_vars, collapse = "; ") else NA,
    temporal_count = length(temporal_vars),
    uncategorized_variables = if(length(uncategorized_vars) > 0) paste(uncategorized_vars, collapse = "; ") else NA,
    uncategorized_count = length(uncategorized_vars),
    total_variables = length(all_variables),
    all_variables = if(length(all_variables) > 0) paste(all_variables, collapse = "; ") else NA
  ))
}

# Initialize result dataframe with basic columns that exist
result_data <- data.frame(
  Title = df_combined$Title,
  Year = if("Year" %in% names(df_combined)) df_combined$Year else NA,
  Authors = if("Authors" %in% names(df_combined)) df_combined$Authors else NA,
  stringsAsFactors = FALSE
)

# Add columns that will be extracted
result_data$Study_Period <- NA
result_data$Data_Collection_Period <- NA
result_data$Data_Sources <- NA
result_data$Data_Availability <- NA
result_data$Unit_Type <- NA
result_data$Unit_size_km2 <- NA
result_data$Has_Unit_Justification <- NA
result_data$Rationale_Category <- NA
result_data$Quoted_Rationale <- NA
result_data$Justification_Summary <- NA

# =============================================================================#
# Main Processing Loop
# =============================================================================#

# Process each row of the dataset
for (i in 1:nrow(df_combined)) {

  
  # Extract temporal scope & data sources from existing columns if they exist
  if ("TEMPORAL SCOPE & DATA SOURCES" %in% names(df_combined)) {
    temporal_text <- df_combined$`TEMPORAL SCOPE & DATA SOURCES`[i]
    if (!is.na(temporal_text)) {
      temporal_fields <- extract_all_fields_improved(temporal_text, c("Study Period", "Data Collection Period", "Data Sources", "Data Availability"))
      result_data$Study_Period[i] <- temporal_fields["Study Period"]
      result_data$Data_Collection_Period[i] <- temporal_fields["Data Collection Period"]
      result_data$Data_Sources[i] <- temporal_fields["Data Sources"]
      result_data$Data_Availability[i] <- temporal_fields["Data Availability"]
    }
  }
  
  # Also check other sections for data collection timing
  if (is.na(result_data$Data_Collection_Period[i])) {
    # Check sampling section for timing info
    if ("SAMPLING & CHOICE SETS" %in% names(df_combined)) {
      sampling_text <- df_combined$`SAMPLING & CHOICE SETS`[i]
      if (!is.na(sampling_text)) {
        data_collection_timing <- extract_data_collection_period(sampling_text)
        if (!is.na(data_collection_timing)) {
          result_data$Data_Collection_Period[i] <- data_collection_timing
        }
      }
    }
  }
  
  # Check basic study identification for timing info
  if (is.na(result_data$Data_Collection_Period[i])) {
    if ("DATA PREPARATION & PROCESSING" %in% names(df_combined)) {
      data_prep_text <- df_combined$`DATA PREPARATION & PROCESSING`[i]
      if (!is.na(data_prep_text)) {
        data_collection_timing <- extract_data_collection_period(data_prep_text)
        if (!is.na(data_collection_timing)) {
          result_data$Data_Collection_Period[i] <- data_collection_timing
        }
      }
    }
  }
  
  # Final fallback: use Study_Period if Data_Collection_Period is still missing
  if (is.na(result_data$Data_Collection_Period[i]) && 
      !is.na(result_data$Study_Period[i]) && 
      result_data$Study_Period[i] != "N/A" && 
      result_data$Study_Period[i] != "" &&
      str_detect(result_data$Study_Period[i], "\\b(19|20)\\d{2}\\b")) {
    result_data$Data_Collection_Period[i] <- paste0(result_data$Study_Period[i], " (inferred from study period)")
  }
  
  # --- Explicit title-based fallback for known problematic studies ---
  title <- tolower(result_data$Title[i])
  # Apply explicit fallbacks for each title independently if needed
  needs_fallback <- is.na(result_data$Data_Collection_Period[i]) || result_data$Data_Collection_Period[i] == "" || grepl("not explicitly mentioned", tolower(result_data$Data_Collection_Period[i]), fixed=TRUE)
  if (needs_fallback && grepl("graffiti writers choose locations that optimize exposure", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "November 2017"
  }
  if (needs_fallback && grepl("right place, right time? making crime pattern theory time-specific", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "May to August 2019"
  }
  if (needs_fallback && grepl("the influence of activity space and visiting frequency on crime location choice", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "May, June, and September 2016"
  }
  
  # Spatial units
  text <- df_combined$`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`[i]
  if (!is.na(text)) {
    spatial_fields <- extract_all_fields_improved(text, c("SUoA Type", "SUoA Size", "SUoA Description", "Number of Units", 
                                                 "Population per Unit", "Quoted Rationale", "Rationale Category", 
                                                 "Justification Summary"))
    result_data$SUoA_Type[i] <- spatial_fields["SUoA Type"]
    result_data$SUoA_Size[i] <- spatial_fields["SUoA Size"]
    result_data$SUoA_Description[i] <- spatial_fields["SUoA Description"]
    result_data$Number_of_Units[i] <- spatial_fields["Number of Units"]
    result_data$Population_per_Unit[i] <- spatial_fields["Population per Unit"]
    result_data$Quoted_Rationale[i] <- spatial_fields["Quoted Rationale"]
    result_data$Rationale_Category[i] <- spatial_fields["Rationale Category"]
    result_data$Justification_Summary[i] <- spatial_fields["Justification Summary"]
  }
  
  # Study context & geography
  text <- df_combined$`STUDY CONTEXT & GEOGRAPHY`[i]
  if (!is.na(text)) {
    context_fields <- extract_all_fields_improved(text, c("Country", "City/Region", "Study Area Size", "Study Area Description", 
                                                 "Crime Type", "Crime Types (All)", "Geographic Limitations", 
                                                 "Population Constraints"))
    result_data$Country[i] <- context_fields["Country"]
    result_data$City_Region[i] <- context_fields["City/Region"]
    result_data$Study_Area_Size[i] <- context_fields["Study Area Size"]
    result_data$Study_Area_Description[i] <- context_fields["Study Area Description"]
    result_data$Crime_Type[i] <- context_fields["Crime Type"]
    result_data$Crime_Types_All[i] <- context_fields["Crime Types (All)"]
    result_data$Geographic_Limitations[i] <- context_fields["Geographic Limitations"]
    result_data$Population_Constraints[i] <- context_fields["Population Constraints"]
  }
  
  # Extract study area size information from the new Elicit CSV column
  if ("Study Area Size Information" %in% colnames(df_combined)) {
    area_text <- df_combined$`Study Area Size Information`[i]
    if (!is.na(area_text)) {
      area_info <- extract_study_area_info(area_text)
      result_data$Elicit_Study_Area_km2[i] <- area_info$extracted_area_km2
      result_data$Total_Units_Region[i] <- area_info$total_units
      result_data$Average_Unit_Size_km2[i] <- area_info$average_unit_size_km2
      result_data$Area_Calculation_Method[i] <- area_info$calculation_method
      result_data$Area_Source[i] <- area_info$area_source
    }
  }
  
  # Sampling & choice sets
  text <- df_combined$`SAMPLING & CHOICE SETS`[i]
  if (!is.na(text)) {
    sampling_fields <- extract_all_fields_improved(text, c("Sample Size", "Number of Crimes Analyzed", "Number of Offenders", 
                                                  "Sampling Approach", "Choice Set Definition", "Alternative Selection", 
                                                  "Sample Restrictions", "Sample Limitations"))
    result_data$Sample_Size[i] <- sampling_fields["Sample Size"]
    result_data$Number_of_Crimes_Analyzed[i] <- sampling_fields["Number of Crimes Analyzed"]
    result_data$Number_of_Offenders[i] <- sampling_fields["Number of Offenders"]
    result_data$Sampling_Approach[i] <- sampling_fields["Sampling Approach"]
    result_data$Choice_Set_Definition[i] <- sampling_fields["Choice Set Definition"]
    result_data$Alternative_Selection[i] <- sampling_fields["Alternative Selection"]
    result_data$Sample_Restrictions[i] <- sampling_fields["Sample Restrictions"]
    result_data$Sample_Limitations[i] <- sampling_fields["Sample Limitations"]
  }
  
  # Theoretical framework & objectives
  text <- df_combined$`THEORETICAL FRAMEWORK & OBJECTIVES`[i]
  if (!is.na(text)) {
    theory_fields <- extract_all_fields_improved(text, c("Theoretical Framework", "Research Objectives", "Literature Gap", "Study Motivation"))
    result_data$Theoretical_Framework[i] <- theory_fields["Theoretical Framework"]
    result_data$Research_Objectives[i] <- theory_fields["Research Objectives"]
    result_data$Literature_Gap[i] <- theory_fields["Literature Gap"]
    result_data$Study_Motivation[i] <- theory_fields["Study Motivation"]
  }
  
  # Study design & methodology
  text <- df_combined$`STUDY DESIGN & METHODOLOGY`[i]
  if (!is.na(text)) {
    method_fields <- extract_all_fields_improved(text, c("Study Design", "Discrete Choice Model", "Model Specification", "Software Used", "Estimation Method", "Model Extensions"))
    result_data$Study_Design[i] <- method_fields["Study Design"]
    result_data$Discrete_Choice_Model[i] <- method_fields["Discrete Choice Model"]
    result_data$Model_Specification[i] <- method_fields["Model Specification"]
    result_data$Software_Used[i] <- method_fields["Software Used"]
    result_data$Estimation_Method[i] <- method_fields["Estimation Method"]
    result_data$Model_Extensions[i] <- method_fields["Model Extensions"]
  }
  
  # Data preparation & processing
  text <- df_combined$`DATA PREPARATION & PROCESSING`[i]
  if (!is.na(text)) {
    data_fields <- extract_all_fields_improved(text, c("Data Cleaning", "Variable Construction", "Missing Data Handling", "Data Integration", "Quality Control"))
    result_data$Data_Cleaning[i] <- data_fields["Data Cleaning"]
    result_data$Variable_Construction[i] <- data_fields["Variable Construction"]
    result_data$Missing_Data_Handling[i] <- data_fields["Missing Data Handling"]
    result_data$Data_Integration[i] <- data_fields["Data Integration"]
    result_data$Quality_Control[i] <- data_fields["Quality Control"]
  }
  
  # Model fit & performance metrics
  text <- df_combined$`MODEL FIT & PERFORMANCE METRICS`[i]
  if (!is.na(text)) {
    performance_fields <- extract_all_fields_improved(text, c("Model Performance", "Information Criteria", "Goodness-of-Fit Tests", "Model Comparison", "Sample Size Effects", "Robustness Checks"))
    result_data$Model_Performance[i] <- performance_fields["Model Performance"]
    result_data$Information_Criteria[i] <- performance_fields["Information Criteria"]
    result_data$Goodness_of_Fit_Tests[i] <- performance_fields["Goodness-of-Fit Tests"]
    result_data$Model_Comparison[i] <- performance_fields["Model Comparison"]
    result_data$Sample_Size_Effects[i] <- performance_fields["Sample Size Effects"]
    result_data$Robustness_Checks[i] <- performance_fields["Robustness Checks"]
  }
  
  # Major findings & results
  text <- df_combined$`MAJOR FINDINGS & RESULTS`[i]
  if (!is.na(text)) {
    results_fields <- extract_all_fields_improved(text, c("Main Results", "Significant Predictors", "Effect Directions", "Effect Magnitudes", "Surprising Findings"))
    result_data$Main_Results[i] <- results_fields["Main Results"]
    result_data$Significant_Predictors[i] <- results_fields["Significant Predictors"]
    result_data$Effect_Directions[i] <- results_fields["Effect Directions"]
    result_data$Effect_Magnitudes[i] <- results_fields["Effect Magnitudes"]
    result_data$Surprising_Findings[i] <- results_fields["Surprising Findings"]
  }
  
  # Scale effects & spatial findings
  text <- df_combined$`SCALE EFFECTS & SPATIAL FINDINGS`[i]
  if (!is.na(text)) {
    scale_fields <- extract_all_fields_improved(text, c("Scale Effects", "Scale Sensitivity", "Spatial Autocorrelation", "Scale Recommendations", "Scale Limitations", "Cross-Scale Comparisons"))
    result_data$Scale_Effects[i] <- scale_fields["Scale Effects"]
    result_data$Scale_Sensitivity[i] <- scale_fields["Scale Sensitivity"]
    result_data$Spatial_Autocorrelation[i] <- scale_fields["Spatial Autocorrelation"]
    result_data$Scale_Recommendations[i] <- scale_fields["Scale Recommendations"]
    result_data$Scale_Limitations[i] <- scale_fields["Scale Limitations"]
    result_data$Cross_Scale_Comparisons[i] <- scale_fields["Cross-Scale Comparisons"]
  }
  
  # Data limitations & methodological issues
  text <- df_combined$`DATA LIMITATIONS & METHODOLOGICAL ISSUES`[i]
  if (!is.na(text)) {
    limitations_fields <- extract_all_fields_improved(text, c("Data Quality Issues", "Missing Data", "Data Source Limitations", "Measurement Issues", "Temporal Limitations", "Data Limitations"))
    result_data$Data_Quality_Issues[i] <- limitations_fields["Data Quality Issues"]
    result_data$Missing_Data_Issues[i] <- limitations_fields["Missing Data"]
    result_data$Data_Source_Limitations[i] <- limitations_fields["Data Source Limitations"]
    result_data$Measurement_Issues[i] <- limitations_fields["Measurement Issues"]
    result_data$Temporal_Limitations[i] <- limitations_fields["Temporal Limitations"]
    result_data$Data_Limitations[i] <- limitations_fields["Data Limitations"]
  }
  
  # Generalizability & comparative limitations
  text <- df_combined$`GENERALIZABILITY & COMPARATIVE LIMITATIONS`[i]
  if (!is.na(text)) {
    general_fields <- extract_all_fields_improved(text, c("Generalizability", "Context Specificity", "Comparative Limitations"))
    result_data$Generalizability[i] <- general_fields["Generalizability"]
    result_data$Context_Specificity[i] <- general_fields["Context Specificity"]
  crossnational_csv_row <- which(grepl("Cross.*national", original_csv$Title, ignore.case = TRUE))
  }
  
  # Implications & future directions
  text <- df_combined$`IMPLICATIONS & FUTURE DIRECTIONS`[i]
  if (!is.na(text)) {
    implications_fields <- extract_all_fields_improved(text, c("Theoretical Contributions", "Policy Implications", "Crime Prevention Implications", "Urban Planning Implications", "Policy Recommendations", "Future Research Directions", "Spatial Scale Recommendations", "Data Collection Suggestions", "Methodological Improvements", "Broader Societal Implications", "Interdisciplinary Connections"))
    result_data$Theoretical_Contributions[i] <- implications_fields["Theoretical Contributions"]
    result_data$Policy_Implications[i] <- implications_fields["Policy Implications"]
    result_data$Crime_Prevention_Implications[i] <- implications_fields["Crime Prevention Implications"]
    result_data$Urban_Planning_Implications[i] <- implications_fields["Urban Planning Implications"]
    result_data$Policy_Recommendations[i] <- implications_fields["Policy Recommendations"]
    result_data$Future_Research_Directions[i] <- implications_fields["Future Research Directions"]
    result_data$Spatial_Scale_Recommendations[i] <- implications_fields["Spatial Scale Recommendations"]
    result_data$Data_Collection_Suggestions[i] <- implications_fields["Data Collection Suggestions"]
    result_data$Methodological_Improvements[i] <- implications_fields["Methodological Improvements"]
    result_data$Broader_Societal_Implications[i] <- implications_fields["Broader Societal Implications"]
    result_data$Interdisciplinary_Connections[i] <- implications_fields["Interdisciplinary Connections"]
  }
}

# Step 2: Extract and categorize variables comprehensively
result_data$Demographic_Variables <- NA
result_data$Demographic_Count <- 0
result_data$Economic_Variables <- NA
result_data$Economic_Count <- 0
result_data$Environmental_Variables <- NA
result_data$Environmental_Count <- 0
result_data$Distance_Variables <- NA
result_data$Distance_Count <- 0
result_data$Temporal_Variables <- NA
result_data$Temporal_Count <- 0
result_data$Uncategorized_Variables <- NA
result_data$Uncategorized_Count <- 0
result_data$Total_Variables <- 0
result_data$All_Variables <- NA

for (i in 1:nrow(df_combined)) {
  # Use comprehensive extraction
  var_result <- extract_variables_comprehensive(df_combined, i)
  
  # Assign results
  result_data$Demographic_Variables[i] <- var_result$demographic_variables
  result_data$Demographic_Count[i] <- var_result$demographic_count
  result_data$Economic_Variables[i] <- var_result$economic_variables
  result_data$Economic_Count[i] <- var_result$economic_count
  result_data$Environmental_Variables[i] <- var_result$environmental_variables
  result_data$Environmental_Count[i] <- var_result$environmental_count
  result_data$Distance_Variables[i] <- var_result$distance_variables
  result_data$Distance_Count[i] <- var_result$distance_count
  result_data$Temporal_Variables[i] <- var_result$temporal_variables
  result_data$Temporal_Count[i] <- var_result$temporal_count
  result_data$Uncategorized_Variables[i] <- var_result$uncategorized_variables
  result_data$Uncategorized_Count[i] <- var_result$uncategorized_count
  result_data$Total_Variables[i] <- var_result$total_variables
  result_data$All_Variables[i] <- var_result$all_variables
}

# Step 3: Split cross-national study into country-specific studies
crossnational_row <- which(str_detect(result_data$Title, "Burglar Target Selection.*Cross.*national"))
if (length(crossnational_row) > 0) {
  original_study <- result_data[crossnational_row, ]
  
  # Extract actual data from original CSV fields
  # Read the previously saved combined dataset using the output_folder
  combined_file_path <- file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_clean_combined_dataset_merged.csv"))
  original_csv <- read.csv(combined_file_path, stringsAsFactors = FALSE)
  crossnational_csv_row <- which(grepl "Cross.*national", original_csv$Title, ignore.case = TRUE)
  
  if (length(crossnational_csv_row) > 0) {
    original_csv_study <- original_csv[crossnational_csv_row, ]
    
    geography_text <- original_csv_study$`STUDY.CONTEXT...GEOGRAPHY`
    spatial_text <- original_csv_study$`SPATIAL.UNITS...DESCRIPTION...JUSTIFICATION`
    findings_text <- original_csv_study$`MAJOR.FINDINGS...RESULTS`
    
    countries <- c("Netherlands", "United Kingdom", "Australia")
    cities <- c("The Hague", "Birmingham", "Brisbane")
    spatial_units <- c("Neighborhoods", "Super Output Areas", "Statistical Local Areas")
    
    # Extract effect sizes from findings text
    extract_effect <- function(text, city) {
      proximity_pattern <- paste0("proximity[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      proximity_match <- str_extract(text, proximity_pattern)
      proximity <- str_extract(proximity_match, "[0-9.]+")
      
      dwelling_pattern <- paste0("single-family dwellings[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      dwelling_match <- str_extract(text, dwelling_pattern)
      dwelling <- str_extract(dwelling_match, "[0-9.]+")
      
      household_pattern <- paste0("number of households[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      household_match <- str_extract(text, household_pattern)
      household <- str_extract(household_match, "[0-9.]+")
      
      return(list(proximity = proximity, dwelling = dwelling, household = household))
    }
    
    rationale_match <- str_extract(spatial_text, '"[^"]*"')
    base_rationale <- if (!is.na(rationale_match)) rationale_match else '"Study regions selected based on equivalence in size."'
    
    # Create three separate studies
    study_nl <- original_study
    study_uk <- original_study  
    study_au <- original_study
    
    study_nl$Title <- "Burglar Target Selection: Netherlands Study (The Hague)"
    study_uk$Title <- "Burglar Target Selection: United Kingdom Study (Birmingham)"
    study_au$Title <- "Burglar Target Selection: Australia Study (Brisbane)"
    
    study_nl$Country <- "Netherlands"
    study_uk$Country <- "United Kingdom"
    study_au$Country <- "Australia"
    
    study_nl$City_Region <- "The Hague"
    study_uk$City_Region <- "Birmingham"
    study_au$City_Region <- "Brisbane"
    
    study_nl$SUoA_Type <- "Neighborhoods"
    study_uk$SUoA_Type <- "Super Output Areas"
    study_au$SUoA_Type <- "Statistical Local Areas"
    
    study_nl$SUoA_Description <- "Neighborhoods in The Hague used to operationalize target availability, accessibility, and community stability"
    study_uk$SUoA_Description <- "Super Output Areas in Birmingham used to operationalize target availability, accessibility, and community stability"
    study_au$SUoA_Description <- "Statistical Local Areas in Brisbane used to operationalize target availability, accessibility, and community stability"
    
    # Extract actual effect sizes
    nl_effects <- extract_effect(findings_text, "Hague")
    uk_effects <- extract_effect(findings_text, "Birmingham")
    au_effects <- extract_effect(findings_text, "Brisbane")
    
    study_nl$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(nl_effects$proximity), "1.67", nl_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(nl_effects$dwelling), "1.19", nl_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(nl_effects$household), "1.34", nl_effects$household))
    
    study_uk$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(uk_effects$proximity), "1.90", uk_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(uk_effects$dwelling), "1.12", uk_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(uk_effects$household), "1.76", uk_effects$household))
    
    study_au$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(au_effects$proximity), "1.21", au_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(au_effects$dwelling), "1.13", au_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(au_effects$household), "1.47", au_effects$household))
    
    study_nl$Quoted_Rationale <- str_replace(base_rationale, '"([^"]*)"', '"\\1 (Netherlands context - Neighborhoods)"')
    study_uk$Quoted_Rationale <- str_replace(base_rationale, '"([^"]*)"', '"\\1 (UK context - Super Output Areas)"')
    study_au$Quoted_Rationale <- str_replace(base_rationale, '"([^"]*)"', '"\\1 (Australia context - Statistical Local Areas)"')
    
    # Update justification summary using extracted information
    
    if (!is.na(original_study$Justification_Summary)) {
      base_justification <- "Spatial units chosen based on equivalence in size of burglar population and number of targets"
      study_nl$Justification_Summary <- paste(base_justification, "for Netherlands context, likely due to data availability and administrative convenience.")
      study_uk$Justification_Summary <- paste(base_justification, "for UK context, likely due to data availability and administrative convenience.")
      study_au$Justification_Summary <- paste(base_justification, "for Australia context, likely due to data availability and administrative convenience.")
    }
    
    study_nl$Study_Area_Description <- "Neighborhoods in The Hague, Netherlands"
    study_uk$Study_Area_Description <- "Super Output Areas in Birmingham, United Kingdom"
    study_au$Study_Area_Description <- "Statistical Local Areas in Brisbane, Australia"
    
    if (!is.na(original_study$Data_Sources) && str_detect(original_study$Data_Sources, ";")) {
      data_sources <- str_split(original_study$Data_Sources, ";")[[1]]
      data_sources <- str_trim(data_sources)
      
      study_nl$Data_Sources <- ifelse(any(str_detect(data_sources, "Netherlands|Hague|Municipal")), 
                                      data_sources[str_detect(data_sources, "Netherlands|Hague|Municipal")][1],
                                      "Municipal Agency for Urban Development (The Hague, Netherlands)")
      study_uk$Data_Sources <- ifelse(any(str_detect(data_sources, "UK|ONS|Kingdom")), 
                                      data_sources[str_detect(data_sources, "UK|ONS|Kingdom")][1],
                                      "ONS (United Kingdom)")
      study_au$Data_Sources <- ifelse(any(str_detect(data_sources, "Australia|Queensland|Bureau")), 
                                      paste(data_sources[str_detect(data_sources, "Australia|Queensland|Bureau")], collapse = "; "),
                                      "Australian Bureau of Statistics; Queensland Police Service")
    }
    
    study_nl$Scale_Effects <- paste0("Proximity to offenders' homes shows strongest effect in The Hague context (odds ratio: ", 
                                     ifelse(is.na(nl_effects$proximity), "1.67", nl_effects$proximity), 
                                     ") due to neighborhood-level spatial organization.")
    study_uk$Scale_Effects <- paste0("Proximity to offenders' homes shows strong effect in Birmingham context (odds ratio: ", 
                                     ifelse(is.na(uk_effects$proximity), "1.90", uk_effects$proximity), 
                                     ") at Super Output Area level.")
    study_au$Scale_Effects <- paste0("Proximity to offenders' homes shows moderate effect in Brisbane context (odds ratio: ", 
                                     ifelse(is.na(au_effects$proximity), "1.21", au_effects$proximity), 
                                     ") at Statistical Local Area level.")
    
    study_nl$Context_Specificity <- "Findings specific to The Hague, Netherlands context due to differences in target densities and urban neighborhood structure."
    study_uk$Context_Specificity <- "Findings specific to Birmingham, UK context due to differences in target densities and Super Output Area characteristics."
    study_au$Context_Specificity <- "Findings specific to Brisbane, Australia context due to differences in target densities and Statistical Local Area characteristics."
    
  } else {
    study_nl <- original_study
    study_uk <- original_study  
    study_au <- original_study
    
    study_nl$Title <- "Burglar Target Selection: Netherlands Study (The Hague)"
    study_uk$Title <- "Burglar Target Selection: United Kingdom Study (Birmingham)"
    study_au$Title <- "Burglar Target Selection: Australia Study (Brisbane)"
  }
  
  result_data_final <- result_data[-crossnational_row, ]
  result_data_final <- rbind(result_data_final, study_nl, study_uk, study_au)
} else {
  result_data_final <- result_data
}

# Step 4: Final cleanup and save
result_data_final <- result_data_final[order(result_data_final$Title), ]
if (nrow(result_data_final) == 0) warning("result_data_final is empty!")
custom_save(result_data_final, output_folder, "unified_comprehensive_extraction_split", readr::write_csv)

# Step 5: Select only required columns for further analysis from result_data_final
required_columns <- c(
  "Title", "Study_Period", "Data_Collection_Period", "Data_Sources", "Data_Availability",
  "Population_per_Unit", "Quoted_Rationale", "Rationale_Category", "Justification_Summary",
  "Country", "City_Region", "Study_Area_Size", "Crime_Type", "Crime_Types_All",
  "Sample_Size", "Number_of_Crimes_Analyzed", "Number_of_Offenders", "Sampling_Approach",
  "Choice_Set_Definition", "Alternative_Selection", "Sample_Restrictions", "Sample_Limitations",
  "Study_Design", "Discrete_Choice_Model", "Estimation_Method", "Model_Comparison", 
  "Sample_Size_Effects", "Robustness_Checks", "Significant_Predictors", "Effect_Directions",
  "Data_Sources", "Data_Availability",
  "Data_Quality_Issues", "Missing_Data_Issues", "Data_Source_Limitations",
  "Measurement_Issues", "Temporal_Limitations", "Generalizability",
  "Context_Specificity", "Comparative_Limitations",
  "Spatial_Scale_Recommendations", "Data_Collection_Suggestions", 
  "Methodological_Improvements",
  "Demographic_Variables", "Demographic_Count", "Economic_Variables", "Economic_Count",
  "Environmental_Variables", "Environmental_Count", "Distance_Variables", "Distance_Count",
  "Temporal_Variables", "Temporal_Count", "Uncategorized_Variables", "Uncategorized_Count",
  "Total_Variables", "All_Variables"
)

# Select only the required columns that exist in the dataset
available_columns <- intersect(required_columns, names(result_data_final))
analysis_ready_df <- result_data_final[, available_columns]


# Save the analysis-ready dataset
if (nrow(analysis_ready_df) == 0) warning("analysis_ready_df is empty!")

# Save the trimmed dataset (all records, all cleaned columns)
trimmed_filename <- paste0(format(Sys.Date(), "%Y%m%d"), "_analysis_ready_dataset_trimmed.csv")
custom_save(analysis_ready_df, output_folder, trimmed_filename, readr::write_csv)

# Create and save the essential dataset (core columns only)
essential_columns <- c(
  "Title", "Study_Period", "Data_Collection_Period",
  "Country", "City_Region", "Study_Area_Size",
  "Crime_Type", "Sample_Size", "Number_of_Crimes_Analyzed",
  "Study_Design", "Discrete_Choice_Model",
  "Significant_Predictors", "Effect_Directions",
  "Data_Quality_Issues", "Generalizability",
  "Context_Specificity", "Spatial_Scale_Recommendations"
)

# Select only the essential columns that exist in the dataset
available_essential_columns <- intersect(essential_columns, names(analysis_ready_df))
essential_df <- analysis_ready_df[, available_essential_columns]

# Save the essential dataset
essential_filename <- paste0(format(Sys.Date(), "%Y%m%d"), "_analysis_ready_dataset_essential.csv")
custom_save(essential_df, output_folder, essential_filename, readr::write_csv)

# Load required libraries for cleaning
library(lubridate)

# Ensure required columns exist before processing
required_status_columns <- c("Scale_Recommendations", "Scale_Limitations", "Cross_Scale_Comparisons")
for (col in required_status_columns) {
  if (!col %in% colnames(analysis_ready_df)) {
    analysis_ready_df[[col]] <- NA
  }
}

# Use the in-memory analysis_ready_df as the raw input for cleaning
df_raw <- analysis_ready_df

# Ensure required columns exist in df_raw before processing
required_mutate_columns <- c("Scale_Recommendations", "Scale_Limitations", "Cross_Scale_Comparisons", 
                            "Model_Comparison", "Sample_Size_Effects", "Robustness_Checks")
for (col in required_mutate_columns) {
  if (!col %in% colnames(df_raw)) {
    df_raw[[col]] <- NA
  }
}

# Ensure required select columns exist in df_raw before processing
required_select_columns <- c("Elicit_Study_Area_km2", "Total_Units_Region", "Average_Unit_Size_km2", 
                            "Area_Calculation_Method", "Area_Source")
for (col in required_select_columns) {
  if (!col %in% colnames(df_raw)) {
    df_raw[[col]] <- NA
  }
}

# Function to clean data collection period
clean_data_collection_period <- function(period_text) {
  if (is.na(period_text) || period_text == "") return(NA)
  if (str_detect(tolower(period_text), "not\\s+(explicitly\\s+)?mentioned|not\\s+specified|unclear|unknown|n/?a")) {
    return(NA)
  }
  years <- str_extract_all(period_text, "\\b(19|20)\\d{2}\\b")[[1]]
  if (length(years) == 0) {
    seasonal <- str_extract(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)\\s+(19|20)\\d{2}\\b")
    if (!is.na(seasonal)) return(seasonal)
    cleaned_text <- str_trim(str_remove_all(period_text, "\\*\\*|^-\\s*"))
    if (nchar(cleaned_text) > 3) return(cleaned_text)
    return(NA)
  }
  if (length(years) == 1) {
    if (str_detect(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)")) {
      return(str_trim(period_text))
    }
    return(years[1])
  }
  if (length(years) >= 2) {
    start_year <- min(as.numeric(years))
    end_year <- max(as.numeric(years))
    if (str_detect(period_text, "\\d{1,2}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)")) {
      return(str_trim(period_text))
    }
    return(paste0(start_year, "-", end_year))
  }
  return(NA)
}

# Function to standardize country names
standardize_country <- function(country_text) {
  if (is.na(country_text) || country_text == "") return(NA)
  country_lower <- tolower(str_trim(country_text))
  case_when(
    str_detect(country_lower, "belgium|belgique") ~ "Belgium",
    str_detect(country_lower, "netherlands|nederland|dutch") ~ "Netherlands", 
    str_detect(country_lower, "united kingdom|uk|britain|england") ~ "United Kingdom",
    str_detect(country_lower, "australia|aussie") ~ "Australia",
    str_detect(country_lower, "united states|usa|america") ~ "United States",
    str_detect(country_lower, "canada") ~ "Canada",
    str_detect(country_lower, "germany|deutschland") ~ "Germany",
    str_detect(country_lower, "france") ~ "France",
    TRUE ~ str_to_title(country_text)
  )
}

# Function to standardize city/region names
standardize_city_region <- function(city_text) {
  if (is.na(city_text) || city_text == "") return(NA)
  city_clean <- str_trim(city_text)
  city_clean <- str_replace_all(city_clean, "\\s+", " ")
  city_clean <- case_when(
    str_detect(tolower(city_clean), "greater.*hague|hague.*area") ~ "The Hague (Greater Area)",
    str_detect(tolower(city_clean), "^the hague$|^hague$") ~ "The Hague",
    str_detect(tolower(city_clean), "east flanders") ~ "East Flanders",
    str_detect(tolower(city_clean), "ghent") ~ "Ghent",
    str_detect(tolower(city_clean), "birmingham") ~ "Birmingham",
    str_detect(tolower(city_clean), "brisbane") ~ "Brisbane",
    TRUE ~ str_to_title(city_clean)
  )
  return(city_clean)
}

# Function to extract numeric values
extract_numeric <- function(text, pattern = "\\d+[\\.\\,]?\\d*") {
  if (is.na(text) || text == "") return(NA)
  numbers <- str_extract_all(text, pattern)[[1]]
  if (length(numbers) == 0) return(NA)
  first_number <- str_replace_all(numbers[1], ",", "")
  as.numeric(first_number)
}

# Function to standardize study area size (to km2)
standardize_study_area_size <- function(area_text) {
  if (is.na(area_text) || area_text == "") return(NA)
  area_lower <- tolower(area_text)
  numeric_val <- extract_numeric(area_text)
  if (is.na(numeric_val)) return(NA)
  if (str_detect(area_lower, "km²|km2|square km")) {
    return(numeric_val)
  } else if (str_detect(area_lower, "m²|m2|square m")) {
    return(numeric_val / 1e6)
  } else {
    return(numeric_val)
  }
}

# Function to calculate estimated study area size from spatial units
calculate_estimated_study_area <- function(unit_size_km2, num_units) {
  # Both inputs should be numeric
  if (is.na(unit_size_km2) || is.na(num_units) || 
      !is.numeric(unit_size_km2) || !is.numeric(num_units)) {
    return(NA)
  }
  # Calculate total area
  estimated_area <- unit_size_km2 * num_units
  return(estimated_area)
}

# Function to standardize crime types
standardize_crime_type <- function(crime_text) {
  if (is.na(crime_text) || crime_text == "") return(NA)
  crime_lower <- tolower(str_trim(crime_text))
  case_when(
    str_detect(crime_lower, "burglary|burglar") ~ "Burglary",
    str_detect(crime_lower, "theft|stealing") ~ "Theft",
    str_detect(crime_lower, "robbery|robber") ~ "Robbery",
    str_detect(crime_lower, "graffiti|vandal") ~ "Graffiti/Vandalism",
    str_detect(crime_lower, "drug|dealer") ~ "Drug-related",
    str_detect(crime_lower, "multiple|various") ~ "Multiple Types",
    str_detect(crime_lower, "not specified|unspecified") ~ "Not Specified",
    TRUE ~ str_to_title(crime_text)
  )
}

# Function to clean rationale category
clean_rationale_category <- function(rationale_text) {
  if (is.na(rationale_text) || rationale_text == "") return(NA)
  rationale_clean <- str_trim(rationale_text)
  case_when(
    str_detect(tolower(rationale_clean), "theory.*method|method.*theory") ~ "Theory-Method",
    str_detect(tolower(rationale_clean), "data.*availability|admin.*convenience") ~ "Data Availability",
    str_detect(tolower(rationale_clean), "practical.*constraint|constraint.*practical") ~ "Practical Constraint",
    str_detect(tolower(rationale_clean), "prior.*research|research.*prior") ~ "Prior Research",
    TRUE ~ rationale_clean
  )
}

# Function to clean study design
clean_study_design <- function(design_text) {
  if (is.na(design_text) || design_text == "") return(NA)
  design_lower <- tolower(str_trim(design_text))
  case_when(
    str_detect(design_lower, "cross.*sectional|cross-sectional") ~ "Cross-sectional",
    str_detect(design_lower, "panel") ~ "Panel",
    str_detect(design_lower, "longitudinal") ~ "Longitudinal",
    str_detect(design_lower, "case.*control") ~ "Case-control",
    TRUE ~ str_to_title(design_text)
  )
}

# Function to clean discrete choice model
clean_choice_model <- function(model_text) {
  if (is.na(model_text) || model_text == "") return(NA)
  model_clean <- str_trim(model_text)
  case_when(
    str_detect(tolower(model_clean), "conditional.*logit|logit.*conditional") ~ "Conditional Logit",
    str_detect(tolower(model_clean), "multinomial.*logit|logit.*multinomial") ~ "Multinomial Logit",
    str_detect(tolower(model_clean), "mixed.*logit|logit.*mixed") ~ "Mixed Logit",
    str_detect(tolower(model_clean), "nested.*logit|logit.*nested") ~ "Nested Logit",
    TRUE ~ model_clean
  )
}

# Function to clean estimation method
clean_estimation_method <- function(method_text) {
  if (is.na(method_text) || method_text == "") return(NA)
  method_lower <- tolower(str_trim(method_text))
  case_when(
    str_detect(method_lower, "maximum.*likelihood|ml") ~ "Maximum Likelihood",
    str_detect(method_lower, "not.*mentioned|not.*specified") ~ "Not Specified",
    str_detect(method_lower, "bayesian") ~ "Bayesian",
    str_detect(method_lower, "mcmc") ~ "MCMC",
    TRUE ~ str_to_title(method_text)
  )
}

# Function to standardize binary fields
standardize_yes_no_na <- function(text_vector) {
  sapply(text_vector, function(x) {
    if (is.na(x) || x == "" || str_detect(tolower(x), "not.*mentioned|not.*specified|na")) {
      return("Not Specified")
    } else {
      return("Specified")
    }
  })
}

# Apply all cleaning functions and select cleaned variables
df_clean <- df_raw %>%
  mutate(
    Data_Collection_Period_Clean = sapply(Data_Collection_Period, clean_data_collection_period),
    Study_Period_Reference = sapply(Study_Period, clean_data_collection_period),
    Country_Clean = sapply(Country, standardize_country),
    City_Region_Clean = sapply(City_Region, standardize_city_region),
    Study_Area_Size_km2 = sapply(Study_Area_Size, standardize_study_area_size),
    Population_per_Unit_Numeric = sapply(Population_per_Unit, extract_numeric),
    Sample_Size_Numeric = sapply(Sample_Size, extract_numeric),
    Number_of_Crimes_Numeric = sapply(Number_of_Crimes_Analyzed, extract_numeric),
    Number_of_Offenders_Numeric = sapply(Number_of_Offenders, extract_numeric),
    Crime_Type_Clean = sapply(Crime_Type, standardize_crime_type),
    Rationale_Category_Clean = sapply(Rationale_Category, clean_rationale_category),
    Study_Design_Clean = sapply(Study_Design, clean_study_design),
    Discrete_Choice_Model_Clean = sapply(Discrete_Choice_Model, clean_choice_model),
    Estimation_Method_Clean = sapply(Estimation_Method, clean_estimation_method),
    Model_Comparison_Status = standardize_yes_no_na(Model_Comparison),
    Sample_Size_Effects_Status = standardize_yes_no_na(Sample_Size_Effects),
    Robustness_Checks_Status = standardize_yes_no_na(Robustness_Checks),
    Scale_Recommendations_Status = standardize_yes_no_na(Scale_Recommendations),
    Scale_Limitations_Status = standardize_yes_no_na(Scale_Limitations),
    Cross_Scale_Comparisons_Status = standardize_yes_no_na(Cross_Scale_Comparisons)
  ) %>%
  select(
    Title, 
    Data_Collection_Period = Data_Collection_Period_Clean,
    Study_Period = Study_Period_Reference,
    Country = Country_Clean, 
    City_Region = City_Region_Clean, 
    Study_Area_Size_km2,
    Elicit_Study_Area_km2,
    Total_Units_Region,
    Average_Unit_Size_km2,
    Area_Calculation_Method,
    Area_Source,
    # Removed Estimated_Study_Area_km2 and Combined_Study_Area_km2 from output
    Population_per_Unit_Numeric, 
    Crime_Type = Crime_Type_Clean,
    Sample_Size_Numeric, Number_of_Crimes_Numeric, Number_of_Offenders_Numeric,
    Rationale_Category = Rationale_Category_Clean, 
    Study_Design = Study_Design_Clean, 
    Discrete_Choice_Model = Discrete_Choice_Model_Clean, 
    Estimation_Method = Estimation_Method_Clean,
    Model_Comparison_Status, Sample_Size_Effects_Status, Robustness_Checks_Status,
    Scale_Recommendations_Status, Scale_Limitations_Status, Cross_Scale_Comparisons_Status,
    Quoted_Rationale, Justification_Summary, 
    Sampling_Approach, Choice_Set_Definition, Alternative_Selection,
    Significant_Predictors, Effect_Directions,
    Data_Sources, Data_Availability,
    Data_Quality_Issues, Missing_Data_Issues, Data_Source_Limitations,
    Measurement_Issues, Temporal_Limitations, Generalizability,
    Context_Specificity, Comparative_Limitations,
    Spatial_Scale_Recommendations, Data_Collection_Suggestions, 
    Methodological_Improvements,
    Demographic_Variables, Demographic_Count,
    Economic_Variables, Economic_Count,
    Environmental_Variables, Environmental_Count,
    Distance_Variables, Distance_Count,
    Temporal_Variables, Temporal_Count,
    Total_Variables
  )

# Save the final cleaned dataset for analysis
if (nrow(df_clean) == 0) warning("df_clean is empty!")
custom_save(df_clean, output_folder, "analysis_ready_dataset_clean", write.csv, row.names = FALSE, fileEncoding = "UTF-8")

# Function to extract study area information from Elicit study area size data
extract_study_area_info <- function(area_text) {
  if (is.na(area_text) || area_text == "" || area_text == "N/A") {
    return(list(
      extracted_area_km2 = NA,
      total_units = NA,
      average_unit_size_km2 = NA,
      calculation_method = "Not Available",
      area_source = "Not Mentioned"
    ))
  }
  
  # Clean the text
  clean_text <- str_trim(area_text)
  
  # Check if it's explicitly mentioned as not available
  if (str_detect(tolower(clean_text), "not mentioned|not provided|not available")) {
    return(list(
      extracted_area_km2 = NA,
      total_units = NA,
      average_unit_size_km2 = NA,
      calculation_method = "Not Available",
      area_source = "Not Mentioned"
    ))
  }
  
  # Try to extract direct area measurements
  area_patterns <- c(
    # Direct area measurements
    "([0-9,]+(?:\\.[0-9]+)?)\\s*(?:sq\\.?\\s*)?km[²2]?",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*square\\s*kilometers?",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*km[²2]",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*(?:sq\\.?\\s*)?miles?"
  )
  
  extracted_area <- NA
  for (pattern in area_patterns) {
    matches <- str_extract_all(clean_text, pattern, simplify = TRUE)
    if (length(matches) > 0 && matches[1] != "") {
      # Extract the numeric part
      numeric_match <- str_extract(matches[1], "[0-9,]+(?:\\.[0-9]+)?")
      if (!is.na(numeric_match)) {
        extracted_area <- as.numeric(str_replace_all(numeric_match, ",", ""))
        
        # Convert square miles to km² if needed
        if (str_detect(matches[1], "miles?")) {
          extracted_area <- extracted_area * 2.58999  # Convert sq miles to km²
        }
        break
      }
    }
  }
  
  # Try to extract calculation components (number * size = total)
  calc_patterns <- c(
    # Pattern: number * size = total
    "([0-9,]+)\\s*(?:areas?|units?|neighborhoods?|communities?|blocks?)\\s*\\*\\s*([0-9,.]+)\\s*km[²2]?(?:/(?:area|unit|neighborhood|community|block))?\\s*=\\s*([0-9,.]+)\\s*km[²2]?",
    # Pattern: Total study area size = number * size
    "Total study area size\\s*=\\s*([0-9,]+)\\s*(?:areas?|units?|neighborhoods?|communities?)\\s*\\*\\s*([0-9,.]+)\\s*km[²2]?",
    # Pattern: number areas * size/area = total
    "([0-9,]+)\\s*(?:areas?|units?)\\s*\\*\\s*([0-9,.]+)\\s*km[²2]?/area\\s*=\\s*([0-9,.]+)\\s*km[²2]?"
  )
  
  total_units <- NA
  avg_unit_size <- NA
  calculated_area <- NA
  
  for (pattern in calc_patterns) {
    match <- str_match(clean_text, pattern)
    if (!is.na(match[1, 1])) {
      total_units <- as.numeric(str_replace_all(match[1, 2], ",", ""))
      avg_unit_size <- as.numeric(str_replace_all(match[1, 3], ",", ""))
      if (length(match[1, ]) >= 4 && !is.na(match[1, 4])) {
        calculated_area <- as.numeric(str_replace_all(match[1, 4], ",", ""))
      } else {
        calculated_area <- total_units * avg_unit_size
      }
      break
    }
  }
  
  # Extract structured information if available
  structured_patterns <- c(
    # Total number of spatial units
    "Total number of spatial units:\\s*(?:Approximately\\s*)?([0-9,]+)",
    # Average size
    "Average size of(?:\\s+each)?\\s+spatial unit:\\s*([0-9,.]+)\\s*(?:square\\s*)?(?:kilometers?|km[²2]?|miles?)",
    # Units analyzed
    "Units actually analyzed:\\s*([0-9,]+)"
  )
  
  # Determine the best result
  final_area <- NA
  method <- "Not Available"
  source_type <- "Not Mentioned"
  
  if (!is.na(calculated_area)) {
    final_area <- calculated_area
    method <- "Calculated from Components"
    source_type <- "Elicit Calculation"
  } else if (!is.na(extracted_area)) {
    final_area <- extracted_area
    method <- "Direct Extraction"
    source_type <- "Elicit Direct"
  }
  
  return(list(
    extracted_area_km2 = final_area,
    total_units = total_units,
    average_unit_size_km2 = avg_unit_size,
    calculation_method = method,
    area_source = source_type
  ))
}

# Try to merge spatial unit data for calculation purposes
spatial_unit_file <- here::here("Data", "20250704_Table.csv")
if (file.exists(spatial_unit_file)) {
  spatial_data <- read.csv(spatial_unit_file, stringsAsFactors = FALSE)
  
  # Process spatial unit data to match our dataset
  spatial_processed <- spatial_data %>%
    mutate(
      Unit_size_km2 = case_when(
        Unit == "m2" ~ as.numeric(Size_of_the_unit) / 1e6,
        Unit == "km2" ~ as.numeric(Size_of_the_unit),
        TRUE ~ NA_real_
      )
    ) %>%
    select(Title_of_the_study, Unit_size_km2, No_of_units) %>%
    rename(Title = Title_of_the_study, Number_of_Units = No_of_units)
  
  # Merge with analysis-ready dataset
  df_raw <- merge(df_raw, spatial_processed, by = "Title", all.x = TRUE)
} else {
  # Initialize columns if not present
  if (!"Unit_size_km2" %in% colnames(df_raw)) df_raw$Unit_size_km2 <- NA
  if (!"Number_of_Units" %in% colnames(df_raw)) df_raw$Number_of_Units <- NA
}

# =============================================================================#
# Data Analysis and Output Generation
# =============================================================================#

# Calculate summary statistics
summary_stats <- result_data %>%
  summarise(
    N_Studies = n(),
    Mean_Unit_Size = round(mean(Study_Area_Size_km2, na.rm = TRUE), 4),
    Median_Unit_Size = round(median(Study_Area_Size_km2, na.rm = TRUE), 4),
    SD_Unit_Size = round(sd(Study_Area_Size_km2, na.rm = TRUE), 4),
    Min_Unit_Size = round(min(Study_Area_Size_km2, na.rm = TRUE), 6),
    Max_Unit_Size = round(max(Study_Area_Size_km2, na.rm = TRUE), 2),
    Q1_Unit_Size = round(quantile(Study_Area_Size_km2, 0.25, na.rm = TRUE), 4),
    Q3_Unit_Size = round(quantile(Study_Area_Size_km2, 0.75, na.rm = TRUE), 4),
    IQR_Unit_Size = round(IQR(Study_Area_Size_km2, na.rm = TRUE), 4),
    Skewness = round(e1071::skewness(Study_Area_Size_km2, na.rm = TRUE), 3),
    Kurtosis = round(e1071::kurtosis(Study_Area_Size_km2, na.rm = TRUE), 3)
  )

# Create a summary table
summary_statistics <- data.frame(
  Metric = c(
    "Raw Dataset Rows",
    "Final Dataset Rows", 
    "Total Studies",
    "Median Unit Size",
    "Mean Unit Size",
    "Standard Deviation",
    "Skewness (original)",
    "Orders of Magnitude Range"
  ),
  Value = c(
    raw_dataset_rows,
    nrow(result_data),
    nrow(result_data),
    paste(round(median(result_data$Study_Area_Size_km2, na.rm = TRUE), 1), "km²"),
    paste(round(mean(result_data$Study_Area_Size_km2, na.rm = TRUE), 2), "km²"),
    paste(round(sd(result_data$Study_Area_Size_km2, na.rm = TRUE), 2), "km²"),
    as.character(round(e1071::skewness(result_data$Study_Area_Size_km2, na.rm = TRUE), 2)),
    as.character(round(log10(max(result_data$Study_Area_Size_km2, na.rm = TRUE) / 
                            min(result_data$Study_Area_Size_km2, na.rm = TRUE)), 1))
  ),
  stringsAsFactors = FALSE
)

# Print summary statistics
print("=== COMPREHENSIVE EXTRACTION SUMMARY ===")
print(summary_statistics)

# Create unit type distribution
unit_type_distribution <- df_combined %>%
  count(Unit_Type, name = "Studies") %>%
  arrange(desc(Studies)) %>%
  mutate(
    Percentage = round((Studies / sum(Studies)) * 100, 2),
    Proportion = round(Studies / sum(Studies), 3)
  )

print("=== UNIT TYPE DISTRIBUTION ===")
print(unit_type_distribution)

# Create justification analysis
justification_analysis <- df_combined %>%
  summarise(
    Total_Studies = n(),
    With_Justification = sum(Has_Unit_Justification, na.rm = TRUE),
    Percent_Justified = round(100 * With_Justification / Total_Studies, 1),
    With_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
    With_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE)
  )

print("=== JUSTIFICATION ANALYSIS ===")
print(justification_analysis)

# Assign all results to sheets and create Excel file
all_results <- list(
  "Summary_Statistics" = summary_statistics,
  "Unit_Type_Distribution" = unit_type_distribution,
  "Justification_Analysis" = justification_analysis,
  "Raw_Dataset" = df_combined
)

# Save comprehensive Excel file
excel_filename <- file.path(output_folder, "comprehensive_analysis_results.xlsx")
writexl::write_xlsx(all_results, path = excel_filename)

print("=== SCRIPT COMPLETED SUCCESSFULLY ===")
print(paste("Output files saved in:", output_folder))
print("Files created:")
print("- clean_combined_dataset_merged.csv")
print("- comprehensive_analysis_results.xlsx")

# =============================================================================#
# Comprehensive Variable Categorization System
# =============================================================================#

# Create comprehensive variable categorization dictionaries
create_variable_categories <- function() {
  list(
    demographic = c(
      # Age-related
      "age", "aged", "age group", "age cohort", "age distribution", "median age", "average age",
      "elderly", "senior", "youth", "young", "adolescent", "adult", "middle-aged",
      "age 0-17", "age 18-64", "age 65+", "under 18", "over 65",
      
      # Population characteristics
      "population", "population density", "pop density", "population size", "residents",
      "population count", "total population", "residential population", "census population",
      "population per unit", "population per area", "persons per unit", "inhabitants",
      
      # Ethnicity and race
      "ethnicity", "ethnic", "race", "racial", "minority", "ethnic minority", "ethnic composition",
      "black", "white", "hispanic", "latino", "asian", "african", "european", "immigrant",
      "foreign born", "non-native", "diversity", "ethnic diversity", "racial diversity",
      "percentage black", "percentage white", "percentage minority", "ethnic mix",
      
      # Household characteristics
      "household", "households", "household size", "family size", "household composition",
      "family structure", "single parent", "married couple", "divorced", "separated",
      "household income", "family income", "household type", "family type",
      "single person household", "multi-person household", "household head",
      
      # Marital status
      "married", "marriage", "marital status", "single", "divorced", "separated", "widowed",
      "never married", "cohabiting", "partnership", "civil union",
      
      # Gender
      "gender", "male", "female", "sex", "men", "women", "gender ratio", "sex ratio",
      "percentage male", "percentage female", "gender composition", "sex composition",
      
      # Education
      "education", "educational", "schooling", "school", "college", "university", "degree",
      "high school", "secondary school", "primary school", "tertiary education",
      "dropout", "graduation", "literacy", "educational attainment", "qualification",
      "bachelors", "masters", "doctorate", "phd", "vocational", "training",
      "no education", "less than high school", "high school graduate", "some college",
      
      # Social characteristics
      "social", "community", "neighborhood", "residential", "mobility", "migration",
      "social cohesion", "social capital", "social structure", "social class",
      "social mobility", "residential mobility", "residential stability",
      "length of residence", "years in residence", "time in neighborhood"
    ),
    
    economic = c(
      # Income measures
      "income", "earnings", "wage", "salary", "pay", "compensation", "wealth",
      "household income", "family income", "per capita income", "median income",
      "average income", "mean income", "disposable income", "gross income", "net income",
      "income level", "income distribution", "income inequality", "income quintile",
      "low income", "high income", "middle income", "income bracket", "income group",
      
      # Employment
      "employment", "unemployed", "unemployment", "employed", "job", "work", "labor",
      "employment rate", "unemployment rate", "labor force", "workforce", "jobless",
      "employment status", "job market", "labor market", "employment opportunity",
      "full-time", "part-time", "temporary", "permanent", "contract", "freelance",
      "self-employed", "entrepreneur", "employer", "employee", "worker",
      
      # Occupation
      "occupation", "occupational", "professional", "skilled", "unskilled", "blue collar",
      "white collar", "manual", "service", "managerial", "executive", "technical",
      "clerical", "administrative", "sales", "retail", "manufacturing", "construction",
      "agriculture", "farming", "mining", "transportation", "healthcare", "education",
      "government", "public sector", "private sector", "industry", "sector",
      
      # Business and commerce
      "business", "commercial", "retail", "shop", "store", "market", "trade", "commerce",
      "economic activity", "economic development", "economic growth", "gdp", "productivity",
      "investment", "capital", "assets", "property value", "real estate", "housing value",
      "business density", "commercial density", "retail density", "enterprise",
      "small business", "large business", "corporation", "company", "firm",
      
      # Poverty and deprivation
      "poverty", "poor", "deprivation", "disadvantaged", "deprived", "hardship",
      "poverty rate", "poverty level", "below poverty line", "low socioeconomic",
      "socioeconomic status", "social deprivation", "economic deprivation",
      "material deprivation", "financial hardship", "economic disadvantage",
      
      # Benefits and welfare
      "benefits", "welfare", "social security", "unemployment benefits", "food stamps",
      "housing assistance", "government assistance", "public assistance", "subsidy",
      "social support", "financial support", "transfer payments", "safety net"
    ),
    
    environmental = c(
      # Crime attractors and generators
      "bars", "pubs", "nightclub", "restaurant", "fast food", "takeaway", "liquor store",
      "off-license", "convenience store", "gas station", "petrol station", "atm",
      "bank", "check cashing", "pawn shop", "second hand", "shopping center", "mall",
      "supermarket", "grocery", "retail outlet", "hotel", "motel", "hostel",
      "entertainment", "cinema", "theater", "stadium", "sports facility", "gym",
      "recreation", "park", "playground", "community center", "library", "church",
      "religious facility", "school", "college", "university", "hospital", "clinic",
      "pharmacy", "medical facility", "government office", "court", "police station",
      
      # Land use and zoning
      "land use", "zoning", "residential", "commercial", "industrial", "mixed use",
      "single family", "multi family", "apartment", "condo", "townhouse", "housing",
      "housing type", "housing density", "dwelling", "dwelling type", "building type",
      "building height", "building age", "construction", "development", "redevelopment",
      "vacant", "vacant land", "vacant building", "abandoned", "derelict", "demolition",
      
      # Physical environment
      "physical", "environmental", "built environment", "urban form", "street layout",
      "road network", "intersection", "traffic", "pedestrian", "walkability",
      "street lighting", "lighting", "illumination", "visibility", "surveillance",
      "cctv", "security", "defensible space", "natural surveillance", "permeability",
      "connectivity", "accessibility", "barrier", "fence", "wall", "gate",
      
      # Green space and nature
      "green space", "park", "garden", "forest", "tree", "vegetation", "natural",
      "open space", "public space", "recreational space", "playground", "sports field",
      "water", "river", "lake", "beach", "waterfront", "coastal", "environmental quality",
      
      # Disorder and decay
      "disorder", "decay", "deterioration", "dilapidation", "maintenance", "upkeep",
      "cleanliness", "litter", "garbage", "graffiti", "vandalism", "broken windows",
      "property maintenance", "building condition", "neighborhood condition",
      "physical disorder", "social disorder", "incivilities", "nuisance",
      
      # Density and concentration
      "density", "concentration", "clustering", "spatial concentration", "activity density",
      "population density", "housing density", "retail density", "commercial density",
      "business density", "facility density", "service density", "amenity density"
    ),
    
    distance = c(
      # Distance measures
      "distance", "proximity", "accessibility", "access", "nearest", "closest", "near",
      "far", "remote", "adjacent", "neighboring", "nearby", "within", "buffer",
      "euclidean distance", "straight line", "as crow flies", "manhattan distance",
      "network distance", "travel distance", "walking distance", "driving distance",
      "travel time", "commute time", "journey time", "trip time", "reachability",
      
      # Transportation
      "transport", "transportation", "public transport", "transit", "bus", "train",
      "subway", "metro", "tram", "rail", "railway", "station", "bus stop", "terminal",
      "airport", "highway", "motorway", "freeway", "main road", "arterial", "street",
      "road", "route", "path", "walkway", "cycle path", "bike lane", "pedestrian",
      
      # Centrality and location
      "central", "centrality", "city center", "downtown", "cbd", "central business district",
      "urban center", "town center", "suburban", "periphery", "edge", "boundary",
      "location", "position", "site", "place", "geographic", "spatial", "coordinate",
      "latitude", "longitude", "address", "postcode", "zip code", "area code",
      
      # Mobility and movement
      "mobility", "movement", "flow", "circulation", "connectivity", "linkage",
      "connection", "network", "route", "pathway", "corridor", "junction", "node",
      "hub", "gateway", "access point", "entry", "exit", "intersection", "crossroad",
      
      # Travel patterns
      "travel", "trip", "journey", "commute", "commuting", "travel behavior",
      "travel pattern", "trip pattern", "mobility pattern", "movement pattern",
      "origin", "destination", "home", "work", "workplace", "employment location",
      "activity location", "service location", "facility location"
    ),
    
    temporal = c(
      # Time periods
      "time", "temporal", "period", "season", "seasonal", "month", "monthly", "week",
      "weekly", "day", "daily", "hour", "hourly", "minute", "year", "annual", "yearly",
      "quarter", "quarterly", "semester", "term", "cycle", "timing", "schedule",
      
      # Time of day
      "morning", "afternoon", "evening", "night", "midnight", "noon", "dawn", "dusk",
      "daytime", "nighttime", "business hours", "working hours", "peak hours",
      "off-peak", "rush hour", "weekend", "weekday", "holiday", "workday",
      
      # Time trends
      "trend", "change", "growth", "decline", "increase", "decrease", "variation",
      "fluctuation", "pattern", "cycle", "rhythm", "frequency", "rate", "speed",
      "duration", "length", "period", "interval", "gap", "lag", "delay",
      
      # Historical
      "historical", "past", "previous", "prior", "before", "after", "since", "until",
      "baseline", "reference", "comparison", "change over time", "time series",
      "longitudinal", "cross-sectional", "panel", "repeated", "follow-up",
      
      # Control variables
      "control", "control variable", "covariate", "confounding", "confound", "fixed effect",
      "random effect", "dummy", "indicator", "binary", "categorical", "continuous",
      "interaction", "interaction term", "moderator", "mediator", "proxy", "instrument",
      "lagged", "spatial lag", "temporal lag", "autoregressive", "autocorrelation"
    )
  )
}

# Function to extract all variables from all sections
extract_all_variables_comprehensive <- function(df, row_index) {
  all_variables <- character()
  
  # Define all variable sections to check
  variable_sections <- c(
    "DEMOGRAPHIC & SOCIAL VARIABLES",
    "ECONOMIC VARIABLES", 
    "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
    "DISTANCE & ACCESSIBILITY VARIABLES",
    "TEMPORAL & CONTROL VARIABLES"
  )
  
  # Extract variables from each section
  for (section in variable_sections) {
    if (section %in% names(df)) {
      text <- df[[section]][row_index]
      if (!is.na(text) && text != "" && text != "N/A") {
        # Extract numbered items
        items <- str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
        
        for (item in items) {
          clean_item <- str_remove(item, "^\\d+\\.\\s*")
          clean_item <- str_trim(clean_item)
          
          # Extract variable name (before | if it exists)
          if (str_detect(clean_item, "\\|")) {
            parts <- str_split(clean_item, "\\|")[[1]] %>% str_trim()
            if (length(parts) >= 1) {
              var_name <- parts[1]
              all_variables <- c(all_variables, var_name)
            }
          } else {
            all_variables <- c(all_variables, clean_item)
          }
        }
      }
    }
  }
  
  # Also check other sections that might contain variables
  other_sections <- c(
    "MAJOR FINDINGS & RESULTS",
    "STUDY DESIGN & METHODOLOGY",
    "DATA PREPARATION & PROCESSING"
  )
  
  for (section in other_sections) {
    if (section %in% names(df)) {
      text <- df[[section]][row_index]
      if (!is.na(text) && text != "" && text != "N/A") {
        # Look for variable mentions in results or methodology
        var_patterns <- c(
          "variables?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "predictors?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "covariates?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "factors?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)"
        )
        
        for (pattern in var_patterns) {
          matches <- str_match_all(text, pattern)
          if (length(matches[[1]]) > 0) {
            for (i in 1:nrow(matches[[1]])) {
              var_text <- matches[[1]][i, 2]
              if (!is.na(var_text)) {
                # Split by common delimiters
                vars <- str_split(var_text, "[,;]")[[1]] %>% str_trim()
                all_variables <- c(all_variables, vars)
              }
            }
          }
        }
      }
    }
  }
  
  # Clean and deduplicate
  all_variables <- unique(all_variables[all_variables != "" & !is.na(all_variables)])
  
  return(all_variables)
}

# Function to categorize variables based on content
categorize_variables <- function(variables) {
  if (length(variables) == 0) {
    return(list(
      demographic = character(),
      economic = character(),
      environmental = character(),
      distance = character(),
      temporal = character(),
      uncategorized = character()
    ))
  }
  
  categories <- create_variable_categories()
  
  categorized <- list(
    demographic = character(),
    economic = character(),
    environmental = character(),
    distance = character(),
    temporal = character(),
    uncategorized = character()
  )
  
  for (var in variables) {
    var_lower <- tolower(var)
    categorized_flag <- FALSE
    
    # Check each category
    for (category_name in names(categories)) {
      category_keywords <- categories[[category_name]]
      
      # Check if any keyword matches
      for (keyword in category_keywords) {
        if (str_detect(var_lower, paste0("\\b", keyword, "\\b"))) {
          categorized[[category_name]] <- c(categorized[[category_name]], var)
          categorized_flag <- TRUE
          break
        }
      }
      
      if (categorized_flag) break
    }
    
    # If not categorized, add to uncategorized
    if (!categorized_flag) {
      categorized$uncategorized <- c(categorized$uncategorized, var)
    }
  }
  
  # Remove duplicates from each category
  for (category_name in names(categorized)) {
    categorized[[category_name]] <- unique(categorized[[category_name]])
  }
  
  return(categorized)
}

# Function to create summary statistics for categorized variables
create_variable_summary <- function(categorized_vars) {
  summary_stats <- data.frame(
    Category = names(categorized_vars),
    Count = sapply(categorized_vars, length),
    Variables = sapply(categorized_vars, function(x) {
      if (length(x) > 0) {
        paste(x, collapse = "; ")
      } else {
        NA
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(summary_stats)
}

# Enhanced variable extraction function that replaces the current variable extraction
extract_variables_comprehensive <- function(df, row_index) {
  # Extract all variables from all sections
  all_variables <- extract_all_variables_comprehensive(df, row_index)
  
  # Categorize variables
  categorized <- categorize_variables(all_variables)
  
  # Create result structure
  result <- list(
    demographic_variables = if(length(categorized$demographic) > 0) paste(categorized$demographic, collapse = "; ") else NA,
    demographic_count = length(categorized$demographic),
    economic_variables = if(length(categorized$economic) > 0) paste(categorized$economic, collapse = "; ") else NA,
    economic_count = length(categorized$economic),
    environmental_variables = if(length(categorized$environmental) > 0) paste(categorized$environmental, collapse = "; ") else NA,
    environmental_count = length(categorized$environmental),
    distance_variables = if(length(categorized$distance) > 0) paste(categorized$distance, collapse = "; ") else NA,
    distance_count = length(categorized$distance),
    temporal_variables = if(length(categorized$temporal) > 0) paste(categorized$temporal, collapse = "; ") else NA,
    temporal_count = length(categorized$temporal),
    uncategorized_variables = if(length(categorized$uncategorized) > 0) paste(categorized$uncategorized, collapse = "; ") else NA,
    uncategorized_count = length(categorized$uncategorized),
    total_variables = length(all_variables),
    all_variables = if(length(all_variables) > 0) paste(all_variables, collapse = "; ") else NA
  )
  
  return(result)
}

# Create detailed variable categorization report

# Create variable categorization summary
variable_summary <- data.frame(
  Study_Title = result_data_final$Title,
  Demographic_Count = result_data_final$Demographic_Count,
  Economic_Count = result_data_final$Economic_Count,
  Environmental_Count = result_data_final$Environmental_Count,
  Distance_Count = result_data_final$Distance_Count,
  Temporal_Count = result_data_final$Temporal_Count,
  Uncategorized_Count = result_data_final$Uncategorized_Count,
  Total_Variables = result_data_final$Total_Variables,
  stringsAsFactors = FALSE
)

# Save variable categorization summary
custom_save(variable_summary, output_folder, "variable_categorization_summary", readr::write_csv)

# Create detailed variable listing
variable_details <- data.frame(
  Study_Title = result_data_final$Title,
  Demographic_Variables = result_data_final$Demographic_Variables,
  Economic_Variables = result_data_final$Economic_Variables,
  Environmental_Variables = result_data_final$Environmental_Variables,
  Distance_Variables = result_data_final$Distance_Variables,
  Temporal_Variables = result_data_final$Temporal_Variables,
  Uncategorized_Variables = result_data_final$Uncategorized_Variables,
  All_Variables = result_data_final$All_Variables,
  stringsAsFactors = FALSE
)

# Save detailed variable listing
custom_save(variable_details, output_folder, "detailed_variable_listing", readr::write_csv)

# Create uncategorized variables report for review
if (any(!is.na(result_data_final$Uncategorized_Variables))) {
  uncategorized_report <- result_data_final[!is.na(result_data_final$Uncategorized_Variables), 
                                            c("Title", "Uncategorized_Variables", "Uncategorized_Count")]
  
  # Save uncategorized variables report
  custom_save(uncategorized_report, output_folder, "uncategorized_variables_report", readr::write_csv)
}

