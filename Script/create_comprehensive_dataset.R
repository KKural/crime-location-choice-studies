# Combined Dataset Creator for Crime Location Choice Studies
# Author: Kuralarasan Kumar
# Date: July 7, 2025
# Purpose: Create a single comprehensive CSV with metadata, variable counts, and variable lists

# Load required libraries
library(tidyverse)
library(stringr)
library(here)
library(readr)

# Source the main extraction script to get the functions
source(here("Script", "improved_variable_extraction.R"))

print("Creating comprehensive combined dataset...")

# After running the main script, we should have:
# - studies_metadata
# - variables_long
# - var_counts

# Create the comprehensive dataset with better organization
create_comprehensive_dataset <- function() {
  
  # 1. Base metadata
  base_data <- studies_metadata
  
  # 2. Add variable counts (with better column names)
  var_counts_clean <- var_counts %>%
    rename_with(~paste0("count_", .), -study_id)
  
  base_data <- base_data %>%
    left_join(var_counts_clean, by = "study_id")
  
  # 3. Add total variable count
  total_vars <- variables_long %>%
    group_by(study_id) %>%
    summarise(total_variables = n(), .groups = "drop")
  
  base_data <- base_data %>%
    left_join(total_vars, by = "study_id")
  
  # 4. Create variable lists by category (cleaned and organized)
  create_variable_list <- function(category_name) {
    variables_long %>%
      filter(variable_category == category_name) %>%
      group_by(study_id) %>%
      summarise(
        !!paste0("vars_", category_name) := paste(variable_name, collapse = " | "),
        .groups = "drop"
      )
  }
  
  # Get all unique categories
  categories <- unique(variables_long$variable_category)
  
  # Create variable lists for each category
  for (cat in categories) {
    cat_vars <- create_variable_list(cat)
    base_data <- base_data %>%
      left_join(cat_vars, by = "study_id")
  }
  
  # 5. Add a complete variable list
  all_vars <- variables_long %>%
    group_by(study_id) %>%
    arrange(variable_category, variable_name) %>%
    summarise(
      all_variables_list = paste(paste0("[", variable_category, "] ", variable_name), collapse = " | "),
      unique_categories = n_distinct(variable_category),
      .groups = "drop"
    )
  
  base_data <- base_data %>%
    left_join(all_vars, by = "study_id")
  
  # 6. Clean up and organize columns
  # Replace NA values appropriately
  count_columns <- names(base_data)[str_detect(names(base_data), "^count_")]
  var_columns <- names(base_data)[str_detect(names(base_data), "^vars_")]
  
  base_data <- base_data %>%
    mutate(
      # Replace NA with 0 for counts
      across(all_of(count_columns), ~replace_na(., 0)),
      # Replace NA with empty string for variable lists
      across(all_of(var_columns), ~replace_na(., "")),
      across(c(all_variables_list), ~replace_na(., "")),
      # Clean up total_variables
      total_variables = replace_na(total_variables, 0),
      unique_categories = replace_na(unique_categories, 0)
    )
  
  # 7. Reorder columns for better readability
  column_order <- c(
    "study_id", "original_title", "extracted_title", "authors", "year",
    "country", "city", "crime_type", "suoa_type", "suoa_size", "num_units", "sample_size",
    "total_variables", "unique_categories",
    count_columns,
    var_columns,
    "all_variables_list"
  )
  
  # Only include columns that exist
  existing_columns <- column_order[column_order %in% names(base_data)]
  base_data <- base_data %>% select(all_of(existing_columns))
  
  return(base_data)
}

# Create the comprehensive dataset
comprehensive_dataset <- create_comprehensive_dataset()

# Save the comprehensive dataset
write_csv(comprehensive_dataset, here("Output", "comprehensive_crime_studies.csv"))

# Create a data dictionary for the comprehensive dataset
create_data_dictionary <- function() {
  dict_data <- data.frame(
    Column_Name = names(comprehensive_dataset),
    Description = c(
      "Study ID", "Original title from source", "Extracted title from text", "Authors", "Publication year",
      "Country of study", "City/Region", "Crime type studied", "Spatial unit of analysis type", 
      "Spatial unit size", "Number of units", "Sample size", "Total number of variables", 
      "Number of unique variable categories"
    ),
    stringsAsFactors = FALSE
  )
  
  # Add descriptions for count columns
  count_cols <- names(comprehensive_dataset)[str_detect(names(comprehensive_dataset), "^count_")]
  var_cols <- names(comprehensive_dataset)[str_detect(names(comprehensive_dataset), "^vars_")]
  
  # Update descriptions for dynamic columns
  for (i in 1:nrow(dict_data)) {
    col_name <- dict_data$Column_Name[i]
    if (str_detect(col_name, "^count_")) {
      category <- str_replace(col_name, "^count_", "")
      dict_data$Description[i] <- paste("Number of", str_replace_all(category, "_", " "), "variables")
    } else if (str_detect(col_name, "^vars_")) {
      category <- str_replace(col_name, "^vars_", "")
      dict_data$Description[i] <- paste("List of", str_replace_all(category, "_", " "), "variables (separated by |)")
    } else if (col_name == "all_variables_list") {
      dict_data$Description[i] <- "Complete list of all variables with categories [category] variable_name"
    }
  }
  
  return(dict_data)
}

# Create and save data dictionary
data_dictionary <- create_data_dictionary()
write_csv(data_dictionary, here("Output", "data_dictionary.csv"))

# Create summary statistics
summary_stats <- list(
  total_studies = nrow(comprehensive_dataset),
  studies_with_variables = sum(comprehensive_dataset$total_variables > 0),
  max_variables_per_study = max(comprehensive_dataset$total_variables),
  avg_variables_per_study = round(mean(comprehensive_dataset$total_variables), 2),
  unique_countries = length(unique(comprehensive_dataset$country[!is.na(comprehensive_dataset$country)])),
  unique_crime_types = length(unique(comprehensive_dataset$crime_type[!is.na(comprehensive_dataset$crime_type)])),
  unique_spatial_units = length(unique(comprehensive_dataset$suoa_type[!is.na(comprehensive_dataset$suoa_type)]))
)

# Print summary
print("\n=== COMPREHENSIVE DATASET CREATED ===")
print(paste("Filename: comprehensive_crime_studies.csv"))
print(paste("Total studies:", summary_stats$total_studies))
print(paste("Studies with variables:", summary_stats$studies_with_variables))
print(paste("Maximum variables per study:", summary_stats$max_variables_per_study))
print(paste("Average variables per study:", summary_stats$avg_variables_per_study))
print(paste("Unique countries:", summary_stats$unique_countries))
print(paste("Unique crime types:", summary_stats$unique_crime_types))
print(paste("Unique spatial units:", summary_stats$unique_spatial_units))
print(paste("Total columns in dataset:", ncol(comprehensive_dataset)))

# Display column structure
print("\n=== DATASET STRUCTURE ===")
print("Columns:")
for (i in 1:nrow(data_dictionary)) {
  print(paste("-", data_dictionary$Column_Name[i], ":", data_dictionary$Description[i]))
}

print("\n=== FILES CREATED ===")
print("1. comprehensive_crime_studies.csv - Main comprehensive dataset")
print("2. data_dictionary.csv - Description of all columns")

# Show a preview of the data
print("\n=== DATA PREVIEW (first 3 studies) ===")
preview_cols <- c("study_id", "extracted_title", "year", "country", "crime_type", "total_variables", "unique_categories")
existing_preview_cols <- preview_cols[preview_cols %in% names(comprehensive_dataset)]
print(comprehensive_dataset[1:min(3, nrow(comprehensive_dataset)), existing_preview_cols])

print("\nScript completed successfully!")
