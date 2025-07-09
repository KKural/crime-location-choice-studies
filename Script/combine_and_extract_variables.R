# Combine Multiple Elicit CSV Files and Extract Variables
# This script combines the 4 Elicit CSV files and processes them using the existing variable extraction logic

# Load required libraries
library(tidyverse)
library(stringr)
library(here)
library(readr)

# Set the working directory to the main project folder
setwd("c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")

# Define the 4 CSV files to combine
csv_files <- c(
  "Elicit - New table - BASIC STUDY IDENTIFICATION TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - DESCRIPTION _ JUSTIFICATION STUDY CONTEXT _ G.csv",
  "Elicit - New table - THEORETICAL FRAMEWORK _ OBJECTIVES STUDY DESIGN _ METHODOLOGY DATA PREPARATION _ PROCESSING DEMOGRAPHIC _ SOCIAL VARIA.csv",
  "Elicit - New table - TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS MAJOR FINDINGS _ RESULTS ENVIRONMENTAL _ CRIME ATTRACTOR .csv",
  "Elicit - New table - SCALE EFFECTS _ SPATIAL FINDINGS DATA LIMITATIONS _ METHODOLOGICAL ISSUES GENERALIZABILITY _ COMPARATIVE LIMITATIONS I.csv"
)

# Function to classify a variable based on keywords (from your original script)
classify_variable_by_keywords <- function(variable_name) {
  variable_lower <- tolower(variable_name)
  
  # Infrastructure/Physical Environment
  if (str_detect(variable_lower, "construction type|garage|central heating|air conditioning|built surface area|house type|building type|dwelling type|residential units|building height|high-rise|floor area|property type|housing type|apartment|detached|semi-detached|terraced|condominium|single-family|multi-family|fence|wall|barrier|road|river|access control|gated")) {
    return("infrastructure")
  }
  
  # Distance/Access/Proximity
  if (str_detect(variable_lower, "distance|proximity|accessibility|ease of escape|escape route|centrality|network centrality|highway|ramp|connector|train|transport|transportation|access|travel time|commute")) {
    return("distance_access")
  }
  
  # Demographic
  if (str_detect(variable_lower, "population|age|gender|sex|ethnic|race|diversity|indigenous|household|family|single-person|composition|structure|migrants|foreign|immigration|birth country|hukou|residential mobility|mobility|churn|transience")) {
    return("demographic")
  }
  
  # Economic/Socioeconomic
  if (str_detect(variable_lower, "income|wealth|affluence|poverty|deprivation|seifa|unemployment|employment|rent|housing value|property value|repayment|price|socioeconomic|economic|financial|imd|index of multiple deprivation")) {
    return("economic")
  }
  
  # Land Use/Commercial Activity
  if (str_detect(variable_lower, "retail|commercial|business|shops|stores|restaurants|bars|hotels|banks|atm|schools|hospitals|health care|cultural|sport|leisure|entertainment|mixed land-use|land use|zoning")) {
    return("land_use")
  }
  
  # Social/Behavioral/Crime Environment
  if (str_detect(variable_lower, "collective efficacy|social|surveillance|surveillability|guardianship|crime|offense|prior|previous|victimization|disorder|unrest|contagion|repeat|recency|criminal activity|offender")) {
    return("social_behavioral")
  }
  
  # Temporal
  if (str_detect(variable_lower, "time|temporal|day|night|hour|season|duration|period|timing|when|schedule")) {
    return("temporal_control")
  }
  
  # Environmental/Physical Features
  if (str_detect(variable_lower, "trees|vegetation|grass|plant|green|park|natural|weather|lighting|visibility|cover|concealment|window|sidewalk|street")) {
    return("environmental")
  }
  
  # Default to other if no match
  return("other")
}

# Function to extract variables from the new CSV format
extract_variables_from_csv_row <- function(row_data) {
  # Initialize empty lists for each variable category
  demographics <- c()
  economic <- c()
  land_use <- c()
  infrastructure <- c()
  distance_access <- c()
  social_behavioral <- c()
  environmental <- c()
  temporal_control <- c()
  other_vars <- c()
  
  # Get all variable-related columns
  variable_columns <- names(row_data)[str_detect(names(row_data), 
    "DEMOGRAPHIC|ECONOMIC|ENVIRONMENTAL|DISTANCE|TEMPORAL|SOCIAL|VARIABLE")]
  
  for (col_name in variable_columns) {
    if (!is.na(row_data[[col_name]]) && nchar(as.character(row_data[[col_name]])) > 0) {
      text <- as.character(row_data[[col_name]])
      
      # Extract variables from numbered list format: "1. Variable_Name | Description | Unit | Source"
      var_matches <- str_extract_all(text, "\\d+\\. ([^|\\n]+)")[[1]]
      
      for (match in var_matches) {
        var_name <- str_replace(match, "^\\d+\\. ", "")
        var_name <- str_trim(var_name)
        
        if (nchar(var_name) > 1) {
          # Classify the variable
          category <- classify_variable_by_keywords(var_name)
          
          # Add to appropriate category
          if (category == "demographic") {
            demographics <- c(demographics, var_name)
          } else if (category == "economic") {
            economic <- c(economic, var_name)
          } else if (category == "land_use") {
            land_use <- c(land_use, var_name)
          } else if (category == "infrastructure") {
            infrastructure <- c(infrastructure, var_name)
          } else if (category == "distance_access") {
            distance_access <- c(distance_access, var_name)
          } else if (category == "social_behavioral") {
            social_behavioral <- c(social_behavioral, var_name)
          } else if (category == "environmental") {
            environmental <- c(environmental, var_name)
          } else if (category == "temporal_control") {
            temporal_control <- c(temporal_control, var_name)
          } else {
            other_vars <- c(other_vars, var_name)
          }
        }
      }
    }
  }
  
  # Remove duplicates
  demographics <- unique(demographics)
  economic <- unique(economic)
  land_use <- unique(land_use)
  infrastructure <- unique(infrastructure)
  distance_access <- unique(distance_access)
  social_behavioral <- unique(social_behavioral)
  environmental <- unique(environmental)
  temporal_control <- unique(temporal_control)
  other_vars <- unique(other_vars)
  
  return(list(
    demographic = demographics,
    economic = economic,
    land_use = land_use,
    infrastructure = infrastructure,
    distance_access = distance_access,
    social_behavioral = social_behavioral,
    environmental = environmental,
    temporal_control = temporal_control,
    other = other_vars
  ))
}

# Function to read and standardize each CSV
read_and_standardize_csv <- function(file_path) {
  cat("Reading file:", file_path, "\n")
  
  # Read the CSV
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Standardize key columns that should be present in all files
  if (!"Title" %in% names(df)) {
    stop(paste("Title column not found in", file_path))
  }
  
  # Ensure Authors column exists
  if (!"Authors" %in% names(df)) {
    df$Authors <- NA
  }
  
  # Ensure Year column exists
  if (!"Year" %in% names(df)) {
    df$Year <- NA
  }
  
  cat("File loaded successfully. Columns:", ncol(df), "Rows:", nrow(df), "\n")
  return(df)
}

# Read all CSV files
cat("===========================================\n")
cat("COMBINING ELICIT CSV FILES\n")
cat("===========================================\n")

csv_data <- list()
for (i in seq_along(csv_files)) {
  csv_data[[i]] <- read_and_standardize_csv(csv_files[i])
}

# Combine all dataframes by merging on Title, Authors, and Year
cat("\nCombining dataframes...\n")

# Start with the first dataframe
combined_df <- csv_data[[1]]

# Merge with each subsequent dataframe
for (i in 2:length(csv_data)) {
  cat("Merging dataframe", i, "\n")
  
  # Get common joining columns
  join_cols <- c("Title")
  if ("Authors" %in% names(combined_df) && "Authors" %in% names(csv_data[[i]])) {
    join_cols <- c(join_cols, "Authors")
  }
  if ("Year" %in% names(combined_df) && "Year" %in% names(csv_data[[i]])) {
    join_cols <- c(join_cols, "Year")
  }
  
  # Perform left join to preserve all studies from the first file
  combined_df <- combined_df %>%
    left_join(csv_data[[i]], by = join_cols, suffix = c("", paste0("_", i)))
}

cat("Combined dataframe created with", nrow(combined_df), "rows and", ncol(combined_df), "columns\n")

# Save the combined dataframe for reference
write_csv(combined_df, "combined_elicit_data.csv")
cat("Combined data saved as 'combined_elicit_data.csv'\n")

# Process each study in the combined dataset
cat("\n===========================================\n")
cat("EXTRACTING VARIABLES AND METADATA\n")
cat("===========================================\n")

studies_metadata <- data.frame()
variables_long <- data.frame()

for (i in 1:nrow(combined_df)) {
  cat("Processing study", i, "of", nrow(combined_df), "\n")
  
  # Extract basic metadata
  title <- as.character(combined_df$Title[i])
  authors <- as.character(combined_df$Authors[i])
  year <- as.character(combined_df$Year[i])
  
  # Extract variables
  variables <- extract_variables_from_csv_row(combined_df[i, ])
  
  # Store metadata
  studies_metadata <- bind_rows(studies_metadata, data.frame(
    study_id = i,
    title = title,
    authors = authors,
    year = year,
    stringsAsFactors = FALSE
  ))
  
  # Convert variables to long format
  for (category in names(variables)) {
    if (length(variables[[category]]) > 0) {
      for (var in variables[[category]]) {
        variables_long <- bind_rows(variables_long, data.frame(
          study_id = i,
          title = title,
          category = category,
          variable = var,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Create summary statistics
cat("\n===========================================\n")
cat("CREATING SUMMARIES\n")
cat("===========================================\n")

# Variable category counts per study
var_counts <- variables_long %>%
  group_by(study_id, title, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = count, values_fill = 0)

# Overall variable frequency
var_frequency <- variables_long %>%
  group_by(category) %>%
  summarise(
    total_variables = n(),
    unique_variables = n_distinct(variable),
    studies_using = n_distinct(study_id)
  ) %>%
  arrange(desc(total_variables))

# Most common variables
common_variables <- variables_long %>%
  group_by(variable, category) %>%
  summarise(frequency = n(), studies = n_distinct(study_id), .groups = "drop") %>%
  arrange(desc(frequency))

# Create comprehensive combined dataset
comprehensive_dataset <- studies_metadata %>%
  left_join(var_counts, by = c("study_id", "title")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Create output directory if it doesn't exist
if (!dir.exists("Output")) {
  dir.create("Output")
}

# Save all results
write_csv(comprehensive_dataset, "Output/comprehensive_studies_dataset_combined.csv")
write_csv(studies_metadata, "Output/studies_metadata_combined.csv")
write_csv(variables_long, "Output/variables_extracted_combined.csv")
write_csv(var_counts, "Output/variable_counts_by_study_combined.csv")
write_csv(var_frequency, "Output/variable_frequency_summary_combined.csv")
write_csv(common_variables, "Output/common_variables_combined.csv")
write_csv(combined_df, "Output/raw_combined_elicit_data.csv")

# Print summary statistics
cat("\n===========================================\n")
cat("EXTRACTION SUMMARY\n")
cat("===========================================\n")
cat("Total studies processed:", nrow(studies_metadata), "\n")
cat("Total variables extracted:", nrow(variables_long), "\n")
cat("Studies with complete title info:", sum(!is.na(studies_metadata$title) & studies_metadata$title != ""), "\n")

cat("\n===========================================\n")
cat("VARIABLE CATEGORIES SUMMARY\n")
cat("===========================================\n")
print(var_frequency)

cat("\n===========================================\n")
cat("TOP 10 MOST COMMON VARIABLES\n")
cat("===========================================\n")
print(head(common_variables, 10))

cat("\n===========================================\n")
cat("FILES CREATED\n")
cat("===========================================\n")
cat("1. comprehensive_studies_dataset_combined.csv - Main dataset with studies and variable counts\n")
cat("2. studies_metadata_combined.csv - Study metadata only\n")
cat("3. variables_extracted_combined.csv - All variables in long format\n")
cat("4. variable_counts_by_study_combined.csv - Variable counts per study\n")
cat("5. variable_frequency_summary_combined.csv - Summary by category\n")
cat("6. common_variables_combined.csv - Most frequent variables\n")
cat("7. raw_combined_elicit_data.csv - Original combined data\n")
cat("8. combined_elicit_data.csv - Initial combined file\n")

cat("\nProcessing completed successfully!\n")
