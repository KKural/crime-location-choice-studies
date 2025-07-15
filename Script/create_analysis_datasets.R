# =============================================================================#
# Create Analysis Datasets Script
# =============================================================================#
# Purpose: Create trimmed and essential datasets for analysis from standardized input
# Input: standardized_unit_sizes_with_groups_merged.csv from yesterday's folder
# Output: analysis_ready_dataset_trimmed.csv and analysis_ready_dataset_essential.csv
# Author: Research Team
# Date: July 14, 2025
# =============================================================================#

# Load required libraries
library(dplyr)
library(readr)
library(here)
library(lubridate)

# Set up dates and folders
today_date <- Sys.Date()
yesterday_date <- today_date - 1

input_folder <- paste0(format(yesterday_date, "%Y%m%d"), "_Analysis & Results")
output_folder <- paste0(format(today_date, "%Y%m%d"), "_Analysis & Results")

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("Created output folder:", output_folder, "\n")
}

# Set up input file path
input_file <- here::here(input_folder, paste0(format(yesterday_date, "%Y%m%d"), "_standardized_unit_sizes_with_groups_merged.csv"))

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

cat("Reading input file:", input_file, "\n")
cat("Output will be saved to:", output_folder, "\n")

# Read data
df <- read_csv(input_file, show_col_types = FALSE)
cat("\nLoaded data. Rows:", nrow(df), "Columns:", ncol(df), "\n")

# Create trimmed dataset with all available columns
trimmed_data <- df %>%
  select(
    Title = Title_of_the_study,
    Year,
    Journal,
    DOI,
    Study_Period,
    Data_Collection_Period,
    Data_Sources,
    Data_Availability,
    Unit_Type = Name_of_the_unit,
    Unit_size_km2,
    Study_Area_Size_km2,
    Country,
    City_Region,
    Crime_Type,
    Sample_Size = No_of_incidents,
    Number_of_Crimes_Numeric,
    Number_of_Offenders_Numeric,
    Study_Design,
    Discrete_Choice_Model,
    Estimation_Method,
    Model_Comparison_Status,
    Sample_Size_Effects_Status,
    Robustness_Checks_Status,
    Scale_Recommendations_Status,
    Scale_Limitations_Status,
    Cross_Scale_Comparisons_Status,
    Quoted_Rationale,
    Justification_Summary,
    everything()  # Include any other columns
  )

# Create essential dataset with core columns only
essential_columns <- c(
  "Title", "Study_Period", "Data_Collection_Period",
  "Country", "City_Region", "Study_Area_Size_km2",
  "Crime_Type", "Sample_Size", "Number_of_Crimes_Numeric",
  "Study_Design", "Discrete_Choice_Model",
  "Significant_Predictors", "Effect_Directions",
  "Data_Quality_Issues", "Generalizability",
  "Context_Specificity", "Spatial_Scale_Recommendations"
)

# Select only the essential columns that exist in the dataset
essential_data <- trimmed_data %>%
  select(any_of(essential_columns))

# Save datasets
cat("\nSaving trimmed dataset...\n")
write_csv(
  trimmed_data,
  file.path(output_folder, paste0(format(today_date, "%Y%m%d"), "_analysis_ready_dataset_trimmed.csv"))
)

cat("Saving essential dataset...\n")
write_csv(
  essential_data,
  file.path(output_folder, paste0(format(today_date, "%Y%m%d"), "_analysis_ready_dataset_essential.csv"))
)

cat("\nDone! Created:\n")
cat("1.", format(today_date, "%Y%m%d"), "_analysis_ready_dataset_trimmed.csv\n")
cat("2.", format(today_date, "%Y%m%d"), "_analysis_ready_dataset_essential.csv\n")
