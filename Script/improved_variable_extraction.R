# Improved Variable Extraction Script for Crime Location Choice Studies
# Author: Kuralarasan Kumar
# Date: July 7, 2025

# Load required libraries
library(tidyverse)
library(stringr)
library(here)
library(readr)

# Function to extract variables from the core information text
extract_variables_from_text <- function(text) {
  # Initialize empty lists for each variable category
  demographics <- c()
  economic <- c()
  land_use <- c()
  infrastructure <- c()
  distance_access <- c()
  crime_opportunity <- c()
  social_behavioral <- c()
  environmental <- c()
  temporal_control <- c()
  other_vars <- c()
  
  # Split text into lines
  lines <- str_split(text, "\n")[[1]]
  
  # Find variable sections
  current_category <- ""
  
  for (line in lines) {
    # Check for category headers
    if (str_detect(line, "DEMOGRAPHIC VARIABLES")) {
      current_category <- "demographic"
    } else if (str_detect(line, "ECONOMIC VARIABLES")) {
      current_category <- "economic"
    } else if (str_detect(line, "LAND USE VARIABLES")) {
      current_category <- "land_use"
    } else if (str_detect(line, "INFRASTRUCTURE VARIABLES")) {
      current_category <- "infrastructure"
    } else if (str_detect(line, "DISTANCE/ACCESSIBILITY VARIABLES")) {
      current_category <- "distance_access"
    } else if (str_detect(line, "CRIME OPPORTUNITY VARIABLES")) {
      current_category <- "crime_opportunity"
    } else if (str_detect(line, "SOCIAL/BEHAVIORAL VARIABLES")) {
      current_category <- "social_behavioral"
    } else if (str_detect(line, "ENVIRONMENTAL VARIABLES")) {
      current_category <- "environmental"
    } else if (str_detect(line, "TEMPORAL/CONTROL VARIABLES")) {
      current_category <- "temporal_control"
    } else if (str_detect(line, "OTHER VARIABLES")) {
      current_category <- "other"
    }
    
    # Extract variable names (lines that start with "  - " or similar patterns)
    if (str_detect(line, "^\\s*-\\s+") && current_category != "") {
      # Clean the variable name
      var_name <- str_replace(line, "^\\s*-\\s+", "")
      var_name <- str_replace(var_name, ":.*$", "") # Remove description after colon
      var_name <- str_trim(var_name)
      
      # Add to appropriate category
      if (current_category == "demographic") {
        demographics <- c(demographics, var_name)
      } else if (current_category == "economic") {
        economic <- c(economic, var_name)
      } else if (current_category == "land_use") {
        land_use <- c(land_use, var_name)
      } else if (current_category == "infrastructure") {
        infrastructure <- c(infrastructure, var_name)
      } else if (current_category == "distance_access") {
        distance_access <- c(distance_access, var_name)
      } else if (current_category == "crime_opportunity") {
        crime_opportunity <- c(crime_opportunity, var_name)
      } else if (current_category == "social_behavioral") {
        social_behavioral <- c(social_behavioral, var_name)
      } else if (current_category == "environmental") {
        environmental <- c(environmental, var_name)
      } else if (current_category == "temporal_control") {
        temporal_control <- c(temporal_control, var_name)
      } else if (current_category == "other") {
        other_vars <- c(other_vars, var_name)
      }
    }
  }
  
  # Return a list with all variables
  return(list(
    demographic = demographics,
    economic = economic,
    land_use = land_use,
    infrastructure = infrastructure,
    distance_access = distance_access,
    crime_opportunity = crime_opportunity,
    social_behavioral = social_behavioral,
    environmental = environmental,
    temporal_control = temporal_control,
    other = other_vars
  ))
}

# Function to extract study metadata
extract_study_metadata <- function(text) {
  # Extract key metadata using regex patterns
  title <- str_extract(text, "(?<=\\*\\*Title:\\*\\*)\\s*[^\n]*")
  year <- str_extract(text, "(?<=\\*\\*Year:\\*\\*)\\s*[^\n]*")
  authors <- str_extract(text, "(?<=\\*\\*Authors:\\*\\*)\\s*[^\n]*")
  country <- str_extract(text, "(?<=\\*\\*Country:\\*\\*)\\s*[^\n]*")
  city <- str_extract(text, "(?<=\\*\\*City/Region:\\*\\*)\\s*[^\n]*")
  crime_type <- str_extract(text, "(?<=\\*\\*Crime Type:\\*\\*)\\s*[^\n]*")
  suoa_type <- str_extract(text, "(?<=\\*\\*SUoA Type:\\*\\*)\\s*[^\n]*")
  suoa_size <- str_extract(text, "(?<=\\*\\*SUoA Size:\\*\\*)\\s*[^\n]*")
  num_units <- str_extract(text, "(?<=\\*\\*Number of Units:\\*\\*)\\s*[^\n]*")
  sample_size <- str_extract(text, "(?<=\\*\\*Sample Size:\\*\\*)\\s*[^\n]*")
  
  # Clean extracted values
  title <- str_trim(str_replace_all(title, "\\*", ""))
  year <- str_trim(str_replace_all(year, "\\*", ""))
  authors <- str_trim(str_replace_all(authors, "\\*", ""))
  country <- str_trim(str_replace_all(country, "\\*", ""))
  city <- str_trim(str_replace_all(city, "\\*", ""))
  crime_type <- str_trim(str_replace_all(crime_type, "\\*", ""))
  suoa_type <- str_trim(str_replace_all(suoa_type, "\\*", ""))
  suoa_size <- str_trim(str_replace_all(suoa_size, "\\*", ""))
  num_units <- str_trim(str_replace_all(num_units, "\\*", ""))
  sample_size <- str_trim(str_replace_all(sample_size, "\\*", ""))
  
  return(list(
    title = title,
    year = year,
    authors = authors,
    country = country,
    city = city,
    crime_type = crime_type,
    suoa_type = suoa_type,
    suoa_size = suoa_size,
    num_units = num_units,
    sample_size = sample_size
  ))
}

# Load the CSV file
print("Loading CSV file...")
elicit_data <- read_csv(here("Data", "Elicit - Extract.csv"))

print(paste("Loaded", nrow(elicit_data), "studies"))

# Create empty dataframes for results
studies_metadata <- data.frame()
variables_long <- data.frame()

# Process each study
print("Processing studies...")
for (i in 1:nrow(elicit_data)) {
  print(paste("Processing study", i, "of", nrow(elicit_data)))
  
  # Skip if core_infor is missing
  if (is.na(elicit_data$`core infor`[i])) {
    print(paste("Skipping study", i, "- no core information"))
    next
  }
  
  # Extract metadata
  metadata <- extract_study_metadata(elicit_data$`core infor`[i])
  
  # Create metadata row
  study_row <- data.frame(
    study_id = i,
    original_title = elicit_data$Title[i],
    extracted_title = ifelse(is.na(metadata$title), elicit_data$Title[i], metadata$title),
    authors = ifelse(is.na(metadata$authors), elicit_data$Authors[i], metadata$authors),
    year = ifelse(is.na(metadata$year), elicit_data$Year[i], metadata$year),
    country = metadata$country,
    city = metadata$city,
    crime_type = metadata$crime_type,
    suoa_type = metadata$suoa_type,
    suoa_size = metadata$suoa_size,
    num_units = metadata$num_units,
    sample_size = metadata$sample_size,
    stringsAsFactors = FALSE
  )
  
  # Add to metadata dataframe
  studies_metadata <- rbind(studies_metadata, study_row)
  
  # Extract variables
  variables <- extract_variables_from_text(elicit_data$`core infor`[i])
  
  # Convert variables to long format
  for (category in names(variables)) {
    if (length(variables[[category]]) > 0) {
      var_rows <- data.frame(
        study_id = i,
        variable_category = category,
        variable_name = variables[[category]],
        stringsAsFactors = FALSE
      )
      variables_long <- rbind(variables_long, var_rows)
    }
  }
}

# Create summary statistics
print("Creating summary statistics...")

# Variable category counts per study
var_counts <- variables_long %>%
  group_by(study_id, variable_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = variable_category, values_from = count, values_fill = 0)

# Overall variable frequency
var_frequency <- variables_long %>%
  group_by(variable_category) %>%
  summarise(
    total_variables = n(),
    unique_variables = n_distinct(variable_name),
    studies_using = n_distinct(study_id)
  )

# Most common variables
common_variables <- variables_long %>%
  group_by(variable_name, variable_category) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  arrange(desc(frequency))

# Save results
print("Saving results...")

# Create output folder if it doesn't exist
if (!dir.exists(here("Output"))) {
  dir.create(here("Output"), recursive = TRUE)
}

# Save dataframes
write_csv(studies_metadata, here("Output", "studies_metadata.csv"))
write_csv(variables_long, here("Output", "variables_extracted.csv"))
write_csv(var_counts, here("Output", "variable_counts_by_study.csv"))
write_csv(var_frequency, here("Output", "variable_frequency_summary.csv"))
write_csv(common_variables, here("Output", "common_variables.csv"))

print("Variable extraction completed!")
print("Files saved:")
print("- studies_metadata.csv: Basic information about each study")
print("- variables_extracted.csv: All variables in long format")
print("- variable_counts_by_study.csv: Count of variables by category for each study")
print("- variable_frequency_summary.csv: Summary of variable usage across studies")
print("- common_variables.csv: Most frequently used variables")

# Display summary
print("\nSummary:")
print(paste("Total studies processed:", nrow(studies_metadata)))
print(paste("Total variables extracted:", nrow(variables_long)))
print("\nVariable categories and counts:")
print(var_frequency)
