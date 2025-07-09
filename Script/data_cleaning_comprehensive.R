# Comprehensive Data Cleaning Script for Analysis-Ready Dataset
# Standardizes dates, locations, numeric variables, and text fields

library(dplyr)
library(stringr)
library(lubridate)
library(here)

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

# Read the analysis-ready dataset
df_raw <- read.csv(here::here(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_analysis_ready_dataset.csv")), 
                   stringsAsFactors = FALSE)

cat("Original dataset dimensions:", dim(df_raw), "\n")

# 1. CLEAN DATA COLLECTION PERIODS ----

clean_data_collection_period <- function(period_text) {
  if (is.na(period_text) || period_text == "") return(NA)
  
  # Clean up common non-informative phrases
  if (str_detect(tolower(period_text), "not\\s+(explicitly\\s+)?mentioned|not\\s+specified|unclear|unknown|n/?a")) {
    return(NA)
  }
  
  # Extract years using regex - prioritize actual collection timing
  years <- str_extract_all(period_text, "\\b(19|20)\\d{2}\\b")[[1]]
  
  if (length(years) == 0) {
    # Try to extract months/seasons with years
    seasonal <- str_extract(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)\\s+(19|20)\\d{2}\\b")
    if (!is.na(seasonal)) return(seasonal)
    
    # Return cleaned text if no years found but text exists
    cleaned_text <- str_trim(str_remove_all(period_text, "\\*\\*|^-\\s*"))
    if (nchar(cleaned_text) > 3) return(cleaned_text)
    return(NA)
  }
  
  if (length(years) == 1) {
    # Single year - check if there's month/season info
    if (str_detect(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)")) {
      return(str_trim(period_text))
    }
    return(years[1])
  }
  
  if (length(years) >= 2) {
    start_year <- min(as.numeric(years))
    end_year <- max(as.numeric(years))
    
    # Check for specific date ranges
    if (str_detect(period_text, "\\d{1,2}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)")) {
      return(str_trim(period_text))
    }
    
    return(paste0(start_year, "-", end_year))
  }
  
  return(NA)
}

# 2. STANDARDIZE COUNTRIES ----

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

# 3. STANDARDIZE CITY/REGION ----

standardize_city_region <- function(city_text) {
  if (is.na(city_text) || city_text == "") return(NA)
  
  city_clean <- str_trim(city_text)
  city_clean <- str_replace_all(city_clean, "\\s+", " ")
  
  # Standardize common variations
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

# 4. EXTRACT NUMERIC VALUES ----

extract_numeric <- function(text, pattern = "\\d+[\\.\\,]?\\d*") {
  if (is.na(text) || text == "") return(NA)
  
  # Remove commas and extract first number
  numbers <- str_extract_all(text, pattern)[[1]]
  if (length(numbers) == 0) return(NA)
  
  # Take first number and clean it
  first_number <- str_replace_all(numbers[1], ",", "")
  as.numeric(first_number)
}

# 5. CLEAN STUDY AREA SIZE ----

standardize_study_area_size <- function(area_text) {
  if (is.na(area_text) || area_text == "") return(NA)
  
  # Extract number and unit
  area_lower <- tolower(area_text)
  
  # Extract numeric value
  numeric_val <- extract_numeric(area_text)
  if (is.na(numeric_val)) return(NA)
  
  # Determine unit and convert to km²
  if (str_detect(area_lower, "km²|km2|square km")) {
    return(numeric_val)
  } else if (str_detect(area_lower, "m²|m2|square m")) {
    return(numeric_val / 1e6)  # Convert m² to km²
  } else {
    return(numeric_val)  # Assume km² if no unit specified
  }
}

# 6. STANDARDIZE CRIME TYPES ----

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

# 7. CLEAN ADDITIONAL TEXT VARIABLES ----

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

standardize_yes_no_na <- function(text_vector) {
  sapply(text_vector, function(x) {
    if (is.na(x) || x == "" || str_detect(tolower(x), "not.*mentioned|not.*specified|na")) {
      return("Not Specified")
    } else {
      return("Specified")
    }
  })
}

# 8. APPLY ALL CLEANING FUNCTIONS ----

df_clean <- df_raw %>%
  mutate(
    # Clean data collection periods (focus on when data was actually collected)
    Data_Collection_Period_Clean = sapply(Data_Collection_Period, clean_data_collection_period),
    
    # Keep study period for reference but focus on data collection
    Study_Period_Reference = sapply(Study_Period, clean_data_collection_period),
    
    # Standardize locations
    Country_Clean = sapply(Country, standardize_country),
    City_Region_Clean = sapply(City_Region, standardize_city_region),
    
    # Clean numeric fields
    Study_Area_Size_km2 = sapply(Study_Area_Size, standardize_study_area_size),
    Population_per_Unit_Numeric = sapply(Population_per_Unit, extract_numeric),
    Sample_Size_Numeric = sapply(Sample_Size, extract_numeric),
    Number_of_Crimes_Numeric = sapply(Number_of_Crimes_Analyzed, extract_numeric),
    Number_of_Offenders_Numeric = sapply(Number_of_Offenders, extract_numeric),
    
    # Standardize crime types
    Crime_Type_Clean = sapply(Crime_Type, standardize_crime_type),
    
    # Clean categorical text fields
    Rationale_Category_Clean = sapply(Rationale_Category, clean_rationale_category),
    Study_Design_Clean = sapply(Study_Design, clean_study_design),
    Discrete_Choice_Model_Clean = sapply(Discrete_Choice_Model, clean_choice_model),
    Estimation_Method_Clean = sapply(Estimation_Method, clean_estimation_method),
    
    # Standardize binary fields to "Specified"/"Not Specified"
    Model_Comparison_Status = standardize_yes_no_na(Model_Comparison),
    Sample_Size_Effects_Status = standardize_yes_no_na(Sample_Size_Effects),
    Robustness_Checks_Status = standardize_yes_no_na(Robustness_Checks),
    Scale_Recommendations_Status = standardize_yes_no_na(Scale_Recommendations),
    Scale_Limitations_Status = standardize_yes_no_na(Scale_Limitations),
    Cross_Scale_Comparisons_Status = standardize_yes_no_na(Cross_Scale_Comparisons)
  ) %>%
  
  # Keep only cleaned variables and essential original ones
  select(
    # Core identifiers and cleaned temporal data
    Title, 
    Data_Collection_Period = Data_Collection_Period_Clean,
    Study_Period = Study_Period_Reference,
    
    # Cleaned geographic data
    Country = Country_Clean, 
    City_Region = City_Region_Clean, 
    Study_Area_Size_km2,
    
    # Cleaned sample and crime data
    Population_per_Unit_Numeric, 
    Crime_Type = Crime_Type_Clean,
    Sample_Size_Numeric, Number_of_Crimes_Numeric, Number_of_Offenders_Numeric,
    
    # Cleaned categorical fields
    Rationale_Category = Rationale_Category_Clean, 
    Study_Design = Study_Design_Clean, 
    Discrete_Choice_Model = Discrete_Choice_Model_Clean, 
    Estimation_Method = Estimation_Method_Clean,
    
    # Status fields (binary indicators)
    Model_Comparison_Status, Sample_Size_Effects_Status, Robustness_Checks_Status,
    Scale_Recommendations_Status, Scale_Limitations_Status, Cross_Scale_Comparisons_Status,
    
    # Important original text fields (cleaned versions would be too long)
    Quoted_Rationale, Justification_Summary, 
    Sampling_Approach, Choice_Set_Definition, Alternative_Selection,
    Significant_Predictors, Effect_Directions,
    Data_Sources, Data_Availability,
    
    # Limitations and quality assessments
    Data_Quality_Issues, Missing_Data_Issues, Data_Source_Limitations,
    Measurement_Issues, Temporal_Limitations, Generalizability,
    Context_Specificity, Comparative_Limitations,
    
    # Research recommendations
    Spatial_Scale_Recommendations, Data_Collection_Suggestions, 
    Methodological_Improvements,
    
    # Variable counts
    Demographic_Variables, Demographic_Count,
    Economic_Variables, Economic_Count,
    Environmental_Variables, Environmental_Count,
    Distance_Variables, Distance_Count,
    Temporal_Variables, Temporal_Count,
    Total_Variables
  )

# 9. CREATE SUMMARY STATISTICS ----

cat("\nCleaning Summary:\n")
cat("================\n")

# Study periods
cat("Data Collection Period Cleaning:\n")
cat("Original unique values:", length(unique(df_raw$Data_Collection_Period[!is.na(df_raw$Data_Collection_Period)])), "\n")
cat("Cleaned unique values:", length(unique(df_clean$Data_Collection_Period[!is.na(df_clean$Data_Collection_Period)])), "\n")
cat("Non-missing data collection periods:", sum(!is.na(df_clean$Data_Collection_Period)), "out of", nrow(df_clean), "\n")

# Countries
cat("\nCountry Standardization:\n")
cat("Original:", paste(unique(df_raw$Country[!is.na(df_raw$Country)]), collapse = ", "), "\n")
cat("Cleaned:", paste(unique(df_clean$Country_Clean[!is.na(df_clean$Country_Clean)]), collapse = ", "), "\n")

# Crime types
cat("\nCrime Type Standardization:\n")
cat("Original unique values:", length(unique(df_raw$Crime_Type[!is.na(df_raw$Crime_Type)])), "\n")
cat("Cleaned unique values:", length(unique(df_clean$Crime_Type_Clean[!is.na(df_clean$Crime_Type_Clean)])), "\n")

# Numeric conversions
cat("\nNumeric Conversions:\n")
cat("Sample sizes extracted:", sum(!is.na(df_clean$Sample_Size_Numeric)), "out of", nrow(df_clean), "\n")
cat("Study area sizes extracted:", sum(!is.na(df_clean$Study_Area_Size_km2)), "out of", nrow(df_clean), "\n")
cat("Population per unit extracted:", sum(!is.na(df_clean$Population_per_Unit_Numeric)), "out of", nrow(df_clean), "\n")

# 10. SAVE CLEANED DATA ----

custom_save(df_clean, output_folder, "analysis_ready_dataset_final_clean", write.csv, row.names = FALSE, fileEncoding = "UTF-8")

cat("\nFinal cleaned dataset saved with", nrow(df_clean), "rows and", ncol(df_clean), "columns\n")
cat("File saved as:", paste0(format(Sys.Date(), "%Y%m%d"), "_analysis_ready_dataset_final_clean.csv"), "\n")

# 11. DISPLAY STRUCTURE OF FINAL CLEANED DATA ----

cat("\nStructure of key cleaned variables:\n")
str(df_clean[, c("Study_Area_Size_km2", "Population_per_Unit_Numeric", 
                 "Sample_Size_Numeric", "Number_of_Crimes_Numeric", 
                 "Number_of_Offenders_Numeric")])

cat("\nUnique values in categorical cleaned variables:\n")
cat("Countries:", paste(sort(unique(df_clean$Country[!is.na(df_clean$Country)])), collapse = ", "), "\n")
cat("Crime Types:", paste(sort(unique(df_clean$Crime_Type[!is.na(df_clean$Crime_Type)])), collapse = ", "), "\n")
cat("Study Designs:", paste(sort(unique(df_clean$Study_Design[!is.na(df_clean$Study_Design)])), collapse = ", "), "\n")
cat("Choice Models:", paste(sort(unique(df_clean$Discrete_Choice_Model[!is.na(df_clean$Discrete_Choice_Model)])), collapse = ", "), "\n")
cat("Estimation Methods:", paste(sort(unique(df_clean$Estimation_Method[!is.na(df_clean$Estimation_Method)])), collapse = ", "), "\n")

cat("\nStatus variables distribution:\n")
cat("Model Comparison Status:\n")
print(table(df_clean$Model_Comparison_Status, useNA = "always"))
cat("Robustness Checks Status:\n")
print(table(df_clean$Robustness_Checks_Status, useNA = "always"))

cat("\nDataset now contains only cleaned variables - ready for analysis!\n")
