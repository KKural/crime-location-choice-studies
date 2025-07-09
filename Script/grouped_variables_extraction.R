# Grouped Variables Extraction with Counts
# Groups variables by category instead of creating individual columns

library(dplyr)
library(stringr)
library(readr)

# Read the data
df <- read_csv("Output/clean_combined_dataset.csv")

cat("Starting grouped variables extraction with counts...\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")

# Function to extract a specific field from text using multiple patterns
extract_field <- function(text, field_name) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(NA)
  }
  
  # Try multiple patterns to extract the field
  patterns <- c(
    paste0("\\*\\*", field_name, ":\\*\\*\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-\\s*\\*\\*|$)"),
    paste0("- \\*\\*", field_name, ":\\*\\*\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-\\s*\\*\\*|$)"),
    paste0("\\*\\*", field_name, ":\\*\\*\\s*(.+?)(?=\\n|$)"),
    paste0("- ", field_name, ":\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-|$)"),
    paste0(field_name, ":\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-|$)")
  )
  
  for (pattern in patterns) {
    match <- str_match(text, pattern)
    if (!is.na(match[1, 2])) {
      result <- str_trim(match[1, 2])
      result <- str_remove_all(result, "\\*\\*")
      result <- str_remove(result, "^-\\s*")
      result <- str_trim(result)
      if (result != "" && result != "Not mentioned" && result != "Not explicitly mentioned") {
        return(result)
      }
    }
  }
  
  return(NA)
}

# Function to extract variables from text and return them as a single string
extract_variables_grouped <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(list(variables = NA, count = 0))
  }
  
  # Find numbered items (1. Variable | Description | Unit | Source)
  items <- str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
  
  if (length(items) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  variables <- character()
  for (item in items) {
    # Remove the number prefix
    clean_item <- str_remove(item, "^\\d+\\.\\s*")
    clean_item <- str_trim(clean_item)
    
    if (str_detect(clean_item, "\\|")) {
      # Split by pipes - take the first part as variable name
      parts <- str_split(clean_item, "\\|")[[1]] %>% str_trim()
      if (length(parts) >= 1) {
        var_name <- parts[1]
        variables <- c(variables, var_name)
      }
    } else {
      # Simple item without pipes
      variables <- c(variables, clean_item)
    }
  }
  
  # Remove duplicates and clean up
  variables <- unique(variables[variables != ""])
  
  if (length(variables) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  # Join variables with semicolon separator
  variables_string <- paste(variables, collapse = "; ")
  
  return(list(variables = variables_string, count = length(variables)))
}

# Initialize result dataframe
result_data <- data.frame(Title = df$Title, stringsAsFactors = FALSE)

# Extract core fields
cat("Extracting core fields...\n")
for (i in 1:nrow(df)) {
  # Basic identification
  text <- df$`BASIC STUDY IDENTIFICATION`[i]
  if (!is.na(text)) {
    result_data$Year[i] <- extract_field(text, "Year")
    result_data$Authors[i] <- extract_field(text, "Authors")
    result_data$Journal[i] <- extract_field(text, "Journal")
    result_data$DOI[i] <- extract_field(text, "DOI")
    result_data$Filename[i] <- extract_field(text, "Filename")
  }
  
  # Study context
  text <- df$`STUDY CONTEXT & GEOGRAPHY`[i]
  if (!is.na(text)) {
    result_data$Country[i] <- extract_field(text, "Country")
    result_data$City_Region[i] <- extract_field(text, "City/Region")
    result_data$Study_Area_Size[i] <- extract_field(text, "Study Area Size")
    result_data$Crime_Type[i] <- extract_field(text, "Crime Type")
  }
  
  # Temporal scope
  text <- df$`TEMPORAL SCOPE & DATA SOURCES`[i]
  if (!is.na(text)) {
    result_data$Study_Period[i] <- extract_field(text, "Study Period")
    result_data$Data_Collection_Period[i] <- extract_field(text, "Data Collection Period")
  }
  
  # Spatial units
  text <- df$`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`[i]
  if (!is.na(text)) {
    result_data$SUoA_Type[i] <- extract_field(text, "SUoA Type")
    result_data$SUoA_Size[i] <- extract_field(text, "SUoA Size")
    result_data$Number_of_Units[i] <- extract_field(text, "Number of Units")
  }
  
  # Sampling
  text <- df$`SAMPLING & CHOICE SETS`[i]
  if (!is.na(text)) {
    result_data$Sample_Size[i] <- extract_field(text, "Sample Size")
    result_data$Number_of_Crimes_Analyzed[i] <- extract_field(text, "Number of Crimes Analyzed")
  }
  
  # Methodology
  text <- df$`STUDY DESIGN & METHODOLOGY`[i]
  if (!is.na(text)) {
    result_data$Discrete_Choice_Model[i] <- extract_field(text, "Discrete Choice Model")
    result_data$Software_Used[i] <- extract_field(text, "Software Used")
  }
  
  # Results
  text <- df$`MAJOR FINDINGS & RESULTS`[i]
  if (!is.na(text)) {
    result_data$Main_Results[i] <- extract_field(text, "Main Results")
  }
}

# Extract grouped variables with counts
cat("Extracting grouped variables...\n")

# Initialize variable columns
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
result_data$Total_Variables <- 0

for (i in 1:nrow(df)) {
  total_vars <- 0
  
  # Demographic & Social Variables
  text <- df$`DEMOGRAPHIC & SOCIAL VARIABLES`[i]
  if (!is.na(text)) {
    demo_result <- extract_variables_grouped(text)
    result_data$Demographic_Variables[i] <- demo_result$variables
    result_data$Demographic_Count[i] <- demo_result$count
    total_vars <- total_vars + demo_result$count
  }
  
  # Economic Variables
  text <- df$`ECONOMIC VARIABLES`[i]
  if (!is.na(text)) {
    econ_result <- extract_variables_grouped(text)
    result_data$Economic_Variables[i] <- econ_result$variables
    result_data$Economic_Count[i] <- econ_result$count
    total_vars <- total_vars + econ_result$count
  }
  
  # Environmental & Crime Attractor Variables
  text <- df$`ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES`[i]
  if (!is.na(text)) {
    env_result <- extract_variables_grouped(text)
    result_data$Environmental_Variables[i] <- env_result$variables
    result_data$Environmental_Count[i] <- env_result$count
    total_vars <- total_vars + env_result$count
  }
  
  # Distance & Accessibility Variables
  text <- df$`DISTANCE & ACCESSIBILITY VARIABLES`[i]
  if (!is.na(text)) {
    dist_result <- extract_variables_grouped(text)
    result_data$Distance_Variables[i] <- dist_result$variables
    result_data$Distance_Count[i] <- dist_result$count
    total_vars <- total_vars + dist_result$count
  }
  
  # Temporal & Control Variables
  text <- df$`TEMPORAL & CONTROL VARIABLES`[i]
  if (!is.na(text)) {
    temp_result <- extract_variables_grouped(text)
    result_data$Temporal_Variables[i] <- temp_result$variables
    result_data$Temporal_Count[i] <- temp_result$count
    total_vars <- total_vars + temp_result$count
  }
  
  # Set total variables count
  result_data$Total_Variables[i] <- total_vars
}

# Save the result
write_csv(result_data, "Output/grouped_variables_extraction.csv")

# Generate summary report
cat("\n=== GROUPED VARIABLES EXTRACTION SUMMARY ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Grouped dataset:", nrow(result_data), "rows x", ncol(result_data), "columns\n")
cat("Output saved to: Output/grouped_variables_extraction.csv\n")

# Core fields summary
cat("\n=== CORE FIELD EXTRACTION SUMMARY ===\n")
key_fields <- c("Title", "Year", "Authors", "Country", "Study_Period", "Crime_Type", 
               "Discrete_Choice_Model", "Sample_Size", "Main_Results", "SUoA_Type")

for (field in key_fields) {
  if (field %in% names(result_data)) {
    non_na_count <- sum(!is.na(result_data[[field]]))
    success_rate <- round((non_na_count/nrow(result_data))*100, 1)
    cat(sprintf("%-25s: %2d/%d (%5.1f%%)\n", field, non_na_count, nrow(result_data), success_rate))
  }
}

# Variable counts summary
cat("\n=== VARIABLE COUNTS SUMMARY ===\n")
demo_total <- sum(result_data$Demographic_Count, na.rm = TRUE)
econ_total <- sum(result_data$Economic_Count, na.rm = TRUE)
env_total <- sum(result_data$Environmental_Count, na.rm = TRUE)
dist_total <- sum(result_data$Distance_Count, na.rm = TRUE)
temp_total <- sum(result_data$Temporal_Count, na.rm = TRUE)
overall_total <- sum(result_data$Total_Variables, na.rm = TRUE)

cat("Demographic variables:", demo_total, "\n")
cat("Economic variables:", econ_total, "\n")
cat("Environmental variables:", env_total, "\n")
cat("Distance variables:", dist_total, "\n")
cat("Temporal variables:", temp_total, "\n")
cat("TOTAL VARIABLES:", overall_total, "\n")

# Average variables per study
cat("\n=== AVERAGE VARIABLES PER STUDY ===\n")
cat(sprintf("Demographic: %.1f per study\n", mean(result_data$Demographic_Count, na.rm = TRUE)))
cat(sprintf("Economic: %.1f per study\n", mean(result_data$Economic_Count, na.rm = TRUE)))
cat(sprintf("Environmental: %.1f per study\n", mean(result_data$Environmental_Count, na.rm = TRUE)))
cat(sprintf("Distance: %.1f per study\n", mean(result_data$Distance_Count, na.rm = TRUE)))
cat(sprintf("Temporal: %.1f per study\n", mean(result_data$Temporal_Count, na.rm = TRUE)))
cat(sprintf("Total: %.1f per study\n", mean(result_data$Total_Variables, na.rm = TRUE)))

# Range of variables per study
cat("\n=== VARIABLE RANGE PER STUDY ===\n")
cat(sprintf("Total variables range: %d - %d per study\n", 
           min(result_data$Total_Variables, na.rm = TRUE), 
           max(result_data$Total_Variables, na.rm = TRUE)))

cat("\nGrouped variables extraction completed!\n")
cat("Each study's variables are now grouped by category in single cells.\n")
