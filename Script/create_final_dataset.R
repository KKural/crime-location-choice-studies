# Create Final Dataset with Standardized Crime Types
# Simple approach: Standardize crime types first, then merge

library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# STEP 1: STANDARDIZE CRIME TYPES FUNCTION
# =============================================================================

standardize_crime_types <- function(crime_type_vector) {
  # Handle NA and empty values
  crime_type_vector[is.na(crime_type_vector) | crime_type_vector == "" | crime_type_vector == "Not Specified"] <- "Not Specified"
  
  # Convert to lowercase for pattern matching
  crime_lower <- tolower(str_trim(crime_type_vector))
  
  # Apply standardization
  result <- case_when(
    # Group all burglary-related crimes together
    str_detect(crime_lower, "burglary|burglar|residential break|breaking and enter|break.*enter") ~ "Burglary",
    
    # Keep robbery separate
    str_detect(crime_lower, "robbery|robber") ~ "Robbery",
    
    # Keep theft separate  
    str_detect(crime_lower, "theft|stealing") ~ "Theft",
    
    # Group all multiple/mixed crime types together (including "Not Specified" as general crimes)
    str_detect(crime_lower, "multiple types|any.*crime.*type|group crime|mixed") ~ "Multiple/Mixed Crime Types",
    crime_type_vector == "Not Specified" ~ "Multiple/Mixed Crime Types",
    
    # Group other specific crime types that appear only once or rarely (including drug-related)
    str_detect(crime_lower, "drug|narcotic") ~ "Other Specific Crimes",
    str_detect(crime_lower, "graffiti|vandal") ~ "Other Specific Crimes",
    str_detect(crime_lower, "terrorist") ~ "Other Specific Crimes",
    str_detect(crime_lower, "riot") ~ "Other Specific Crimes", 
    str_detect(crime_lower, "serious acquisitive") ~ "Other Specific Crimes",
    
    # Default: keep as is but clean up formatting
    TRUE ~ str_to_title(crime_type_vector)
  )
  
  return(result)
}

# =============================================================================
# STEP 2: READ AND PREPARE DATASETS
# =============================================================================

cat("Reading datasets...\n")

# Read master dataset (unit sizes)
master <- read_csv("20250710_Analysis & Results/20250710_standardized_unit_sizes_with_groups.csv")
cat("Master dataset rows:", nrow(master), "\n")

# Read clean dataset (crime types and other variables)
clean <- read_csv("20250710_Analysis & Results/20250710_analysis_ready_dataset_clean.csv")
cat("Clean dataset rows:", nrow(clean), "\n")

# =============================================================================
# STEP 3: STANDARDIZE CRIME TYPES IN CLEAN DATASET
# =============================================================================

cat("Standardizing crime types in clean dataset...\n")

clean_standardized <- clean %>%
  mutate(
    # Clean title for matching
    Title_Clean = str_remove_all(Title, '^"') %>%  
                  str_remove_all('"$') %>%         
                  str_trim(),
    
    # Standardize crime types
    Crime_Type_Standardized = standardize_crime_types(Crime_Type)
  )

# Check standardization results
cat("\n=== CRIME TYPE STANDARDIZATION RESULTS ===\n")
print(table(clean_standardized$Crime_Type_Standardized, useNA = 'always'))

# =============================================================================
# STEP 4: MERGE DATASETS BY TITLE
# =============================================================================

cat("\nMerging datasets by title...\n")

# Simple title normalization for matching
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%
    str_remove_all('[""\'\"*]') %>%
    str_remove_all('\\([^)]*\\)') %>%
    str_replace_all('[[:punct:]]', ' ') %>%
    str_replace_all('\\s+', ' ') %>%
    str_trim()
}

# Create normalized titles for matching
master_for_merge <- master %>%
  mutate(title_norm = normalize_title(Title_of_the_study))

clean_for_merge <- clean_standardized %>%
  mutate(title_norm = normalize_title(Title_Clean))

# Merge on normalized titles
final_dataset <- master_for_merge %>%
  left_join(clean_for_merge, by = "title_norm") %>%
  select(-title_norm, -Title_Clean)  # Remove helper columns

cat("Final dataset rows:", nrow(final_dataset), "\n")
cat("Rows with Crime_Type_Standardized:", sum(!is.na(final_dataset$Crime_Type_Standardized)), "\n")

# =============================================================================
# STEP 5: CREATE ANALYSIS SUMMARY
# =============================================================================

cat("\n=== FINAL CRIME TYPE ANALYSIS ===\n")

crime_summary <- final_dataset %>%
  filter(!is.na(Crime_Type_Standardized)) %>%
  group_by(Crime_Type_Standardized) %>%
  summarise(
    N_Studies = n(),
    Mean_Unit_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Unit_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    Countries = paste(unique(Country[!is.na(Country) & Country != ""]), collapse = "; "),
    .groups = 'drop'
  ) %>%
  arrange(desc(N_Studies)) %>%
  mutate(Percentage = round((N_Studies / sum(N_Studies)) * 100, 1))

print(crime_summary)

# Create mapping table
mapping_table <- final_dataset %>%
  filter(!is.na(Crime_Type) & !is.na(Crime_Type_Standardized)) %>%
  select(Original_Crime_Type = Crime_Type, Crime_Type_Standardized) %>%
  distinct() %>%
  arrange(Crime_Type_Standardized, Original_Crime_Type)

cat("\n=== CRIME TYPE MAPPING ===\n")
print(mapping_table)

# =============================================================================
# STEP 6: SAVE FINAL DATASET AND RESULTS
# =============================================================================

cat("\nSaving final dataset and results...\n")

# Save the final merged dataset with standardized crime types
write_csv(final_dataset, "20250710_Analysis & Results/20250710_final_dataset_with_standardized_crime_types.csv")

# Save the crime type analysis
write_csv(crime_summary, "20250710_Analysis & Results/20250710_final_crime_type_analysis.csv")

# Save the mapping table
write_csv(mapping_table, "20250710_Analysis & Results/20250710_final_crime_type_mapping.csv")

# =============================================================================
# STEP 7: VERIFY RESULTS
# =============================================================================

cat("\n=== VERIFICATION ===\n")
cat("Final dataset columns with Crime_Type_Standardized:\n")
crime_cols <- colnames(final_dataset)[grepl("crime|Crime", colnames(final_dataset), ignore.case = TRUE)]
print(crime_cols)

cat("\nSample of Crime_Type_Standardized values:\n")
print(head(final_dataset[, c("Title_of_the_study", "Crime_Type", "Crime_Type_Standardized")], 10))

cat("\n=== FILES CREATED ===\n")
cat("1. Main dataset: 20250710_final_dataset_with_standardized_crime_types.csv\n")
cat("2. Crime analysis: 20250710_final_crime_type_analysis.csv\n") 
cat("3. Mapping table: 20250710_final_crime_type_mapping.csv\n")

cat("\n✓ DONE! Final dataset created with Crime_Type_Standardized column.\n")
