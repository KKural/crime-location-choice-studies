# SIMPLE: Just add Crime_Type_Standardized to the master dataset
library(dplyr)
library(readr)
library(stringr)

# Read master dataset (it already has all Crime_Type data)
data <- read_csv("20250710_Analysis & Results/20250710_standardized_unit_sizes_with_groups.csv")

cat("Original dataset rows:", nrow(data), "\n")
cat("Crime_Type column exists:", "Crime_Type" %in% colnames(data), "\n")

# Simple standardization function
standardize_crime_types <- function(crime_type_vector) {
  crime_type_vector[is.na(crime_type_vector) | crime_type_vector == "" | crime_type_vector == "Not Specified"] <- "Not Specified"
  crime_lower <- tolower(str_trim(crime_type_vector))
  
  result <- case_when(
    str_detect(crime_lower, "burglary|burglar|residential break|breaking and enter|break.*enter") ~ "Burglary",
    str_detect(crime_lower, "robbery|robber") ~ "Robbery",
    str_detect(crime_lower, "theft|stealing") ~ "Theft",
    str_detect(crime_lower, "multiple types|any.*crime.*type|group crime|mixed") ~ "Multiple/Mixed Crime Types",
    crime_type_vector == "Not Specified" ~ "Multiple/Mixed Crime Types",
    str_detect(crime_lower, "drug|narcotic|graffiti|vandal|terrorist|riot|serious acquisitive") ~ "Other Specific Crimes",
    TRUE ~ str_to_title(crime_type_vector)
  )
  return(result)
}

# Add standardized crime types
data_final <- data %>%
  mutate(Crime_Type_Standardized = standardize_crime_types(Crime_Type))

# Check results
cat("\n=== RESULTS ===\n")
cat("Final dataset rows:", nrow(data_final), "\n")
cat("Crime_Type_Standardized values:\n")
print(table(data_final$Crime_Type_Standardized, useNA = 'always'))

# Save the final dataset
write_csv(data_final, "20250710_Analysis & Results/20250710_final_dataset_standardized.csv")

cat("\n✓ DONE! File saved: 20250710_final_dataset_standardized.csv\n")
cat("✓ All", nrow(data_final), "studies have standardized crime types\n")
