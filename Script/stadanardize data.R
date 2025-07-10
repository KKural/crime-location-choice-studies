# Standardize spatial unit sizes to km²
rm(list=ls())
library(dplyr)
library(here)
library(stringr)

# Workspace setup - Reproducible output folder creation --------------------

# Function to create a folder with a date argument
make_folder <- function(date = Sys.Date(), subfolder = NULL) {
  # Convert the provided date to "YYYYMMDD" format
  folder_name <- format(as.Date(date), "%Y%m%d")
  
  # Define the main folder name
  main_folder_name <- paste0(folder_name, "_Analysis & Results")
  
  # If a subfolder is specified, append it to the main folder path
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)  # Create nested folders if necessary
  }
  
  return(full_folder_path)  # Return the folder path to use later
}

# Create a function to save output with date
custom_save <- function(data, folder_name, file_description, save_function, file_extension = ".csv", ...) {
  # Current date in YYYYMMDD format
  current_date <- format(Sys.Date(), "%Y%m%d")
  
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
}

# Create the output folder
output_folder <- make_folder()

# Read input data - using the comprehensive dataset with Elicit data
df_raw_data <- read.csv("Data/20250704_Table.csv", stringsAsFactors = FALSE)

# Convert unit sizes to km², sort, create Study_ID (if not exists), rearrange columns
df_data_clean <- df_raw_data %>%
  mutate(
    Unit_size_km2 = case_when(
      Unit == "m2" ~ as.numeric(`Size_of_the_unit`) / 1e6,
      Unit == "km2" ~ as.numeric(`Size_of_the_unit`),
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(Unit_size_km2) %>%
  # Only create Study_ID if it doesn't exist or renumber for sorting
  mutate(Study_ID = row_number()) %>%
  select(
    Study_ID, `Title_of_the_study`, Citation, `Size_of_the_unit`, Unit, 
    Unit_size_km2, `No_of_units`, `No_of_incidents`, `Name_of_the_unit`, `Inferred_size`,
    `DOI`, `ISSN`, `Journal`, `Volume`, `Issue`
  )

# Save results with UTF-8 encoding - now includes Elicit data
custom_save(df_data_clean, output_folder, "standardized_unit_sizes", write.csv, row.names = FALSE, fileEncoding = "UTF-8")

str(df_data_clean)

# Print summary statistics for Unit_size_km2
print(summary(df_data_clean$Unit_size_km2))

# Print frequency table for Name_of_the_unit
print(table(df_data_clean$Name_of_the_unit))

# Add size group column based on preferred breakpoints
size_breaks <- c(-Inf, 0.001, 1.2, 1.63293, 5, Inf)
size_labels <- c("very small", "small", "medium", "large", "very large")

# Add size group column and extract publication year from Citation column
df_data_clean <- df_data_clean %>%
  mutate(
    Size_group = cut(
      Unit_size_km2,
      breaks = size_breaks,
      labels = size_labels,
      right = FALSE
    ),
    # Extract 4-digit year from citation format "(Author et al., YYYY)"
    Year = as.numeric(str_extract(Citation, "\\b(19|20)\\d{2}\\b"))
  ) %>%
  select(
    Study_ID, `Title_of_the_study`, Citation, Year, `Size_of_the_unit`, Unit, 
    Unit_size_km2, Size_group, `Name_of_the_unit`, `No_of_units`, `No_of_incidents`, `Inferred_size`,
    `DOI`, `ISSN`, `Journal`, `Volume`, `Issue`
  )

# Print frequency table for size groups
print(table(df_data_clean$Size_group))

str(df_data_clean)

# Optionally, save the updated data with the new column and Elicit data
custom_save(df_data_clean, output_folder, "standardized_unit_sizes_with_groups", write.csv, row.names = FALSE, fileEncoding = "UTF-8")