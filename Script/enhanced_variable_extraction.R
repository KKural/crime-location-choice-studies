# Simple CSV Combination Script
# This script combines 4 Elicit CSV files using right join based on Title

# Load required libraries
library(tidyverse)
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

# Function to read and standardize each CSV
read_and_standardize_csv <- function(file_path) {
  # Read the CSV
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Ensure Title column exists
  if (!"Title" %in% names(df)) {
    stop(paste("Title column not found in", file_path))
  }
  
  # Add Authors column if missing
  if (!"Authors" %in% names(df)) {
    df$Authors <- NA
  }
  
  # Add Year column if missing
  if (!"Year" %in% names(df)) {
    df$Year <- NA
  }
  
  return(df)
}

# Read all CSV files
csv_data <- list()
for (i in seq_along(csv_files)) {
  csv_data[[i]] <- read_and_standardize_csv(csv_files[i])
}

# Combine all dataframes using right join based on Title
combined_df <- csv_data[[1]]

for (i in 2:length(csv_data)) {
  # Use Title as join column
  join_cols <- c("Title")
  
  # Right join to keep all records from the right dataframe
  combined_df <- combined_df %>%
    right_join(csv_data[[i]], by = join_cols, suffix = c("", paste0("_", i)))
}

# Create output directory if it doesn't exist
if (!dir.exists("Output")) {
  dir.create("Output")
}

# Save the combined dataset
write_csv(combined_df, "Output/combined_elicit_dataset.csv")

# Create clean dataset with only needed columns
clean_columns <- c(
  "Title",
  "BASIC STUDY IDENTIFICATION",
  "TEMPORAL SCOPE & DATA SOURCES",
  "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION",
  "STUDY CONTEXT & GEOGRAPHY",
  "SAMPLING & CHOICE SETS",
  "THEORETICAL FRAMEWORK & OBJECTIVES",
  "STUDY DESIGN & METHODOLOGY",
  "DATA PREPARATION & PROCESSING",
  "DEMOGRAPHIC & SOCIAL VARIABLES",
  "ECONOMIC VARIABLES",
  "TEMPORAL & CONTROL VARIABLES",
  "MODEL FIT & PERFORMANCE METRICS",
  "MAJOR FINDINGS & RESULTS",
  "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
  "DISTANCE & ACCESSIBILITY VARIABLES",
  "SCALE EFFECTS & SPATIAL FINDINGS",
  "DATA LIMITATIONS & METHODOLOGICAL ISSUES",
  "GENERALIZABILITY & COMPARATIVE LIMITATIONS",
  "IMPLICATIONS & FUTURE DIRECTIONS"
)

# Select only the needed columns
clean_df <- combined_df %>%
  select(all_of(clean_columns))

names(clean_df)

# Save the clean dataset
write_csv(clean_df, "Output/clean_combined_dataset.csv")
