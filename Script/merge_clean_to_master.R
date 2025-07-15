# Merge new columns from clean dataset into master dataset by title
library(dplyr)
library(readr)
library(stringr)

# Get today's date for dynamic file paths
today_date <- format(Sys.Date(), "%Y%m%d")
output_folder <- paste0(today_date, "_Analysis & Results")

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Read both datasets using today's date
master <- read_csv(paste0(output_folder, "/", today_date, "_standardized_unit_sizes_with_groups.csv"))
clean <- read_csv(paste0(output_folder, "/", today_date, "_analysis_ready_dataset_clean.csv"))

# Clean the title column in the clean dataset to match master format
clean <- clean %>%
  mutate(Title = str_remove_all(Title, '^"') %>%  # Remove leading quotes
                 str_remove_all('"$') %>%         # Remove trailing quotes
                 str_trim())                      # Remove extra whitespace

# Function to normalize titles for better matching
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%                          # Convert to lowercase
    str_remove_all('[""\'\"*]') %>%            # Remove quotes and asterisks
    str_remove_all('\\([^)]*\\)') %>%          # Remove parenthetical content
    str_replace_all('[[:punct:]]', ' ') %>%    # Replace punctuation with spaces
    str_replace_all('\\s+', ' ') %>%           # Replace multiple spaces with single space
    str_trim()                                  # Trim whitespace
}

# Create normalized versions for matching
master_norm <- master %>%
  mutate(title_norm = normalize_title(Title_of_the_study))

clean_norm <- clean %>%
  mutate(title_norm = normalize_title(Title))

# Create comprehensive matching using multiple strategies
matches <- data.frame(
  master_title = character(),
  clean_title = character(),
  match_type = character(),
  stringsAsFactors = FALSE
)

# Method 1: Exact normalized match
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  exact_match <- clean_norm$Title[clean_norm$title_norm == master_norm_title]
  if (length(exact_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = master_title,
      clean_title = exact_match[1],
      match_type = "exact_normalized"
    ))
  }
}

# Method 2: Fuzzy match (master contained in clean)
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  # Skip if already matched
  if (master_title %in% matches$master_title) next
  
  # Look for clean titles that contain the master title
  fuzzy_match <- clean_norm$Title[str_detect(clean_norm$title_norm, str_escape(master_norm_title))]
  if (length(fuzzy_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = master_title,
      clean_title = fuzzy_match[1],
      match_type = "fuzzy_contained"
    ))
  }
}

# Method 3: Partial word matching (60% of words match)
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  # Skip if already matched
  if (master_title %in% matches$master_title) next
  
  master_words <- str_split(master_norm_title, "\\s+")[[1]]
  master_words <- master_words[nchar(master_words) > 3]  # Only significant words
  
  if (length(master_words) < 3) next  # Skip very short titles
  
  for (j in 1:nrow(clean_norm)) {
    clean_words <- str_split(clean_norm$title_norm[j], "\\s+")[[1]]
    clean_words <- clean_words[nchar(clean_words) > 3]
    
    if (length(clean_words) < 3) next
    
    # Calculate word overlap
    common_words <- intersect(master_words, clean_words)
    overlap_ratio <- length(common_words) / length(master_words)
    
    if (overlap_ratio >= 0.6) {  # 60% word overlap
      matches <- bind_rows(matches, data.frame(
        master_title = master_title,
        clean_title = clean_norm$Title[j],
        match_type = sprintf("word_overlap_%.0f%%", overlap_ratio * 100)
      ))
      break
    }
  }
}

# Method 4: Special cases for known study patterns
special_matches <- list(
  # Burglar Target Selection studies
  list(
    master_pattern = "Burglar Target Selection.*\\(NL\\)",
    clean_pattern = "Burglar Target Selection.*Netherlands"
  ),
  list(
    master_pattern = "Burglar Target Selection.*UK\\)",
    clean_pattern = "Burglar Target Selection.*United Kingdom"
  ),
  list(
    master_pattern = "Burglar Target Selection.*AU\\)",
    clean_pattern = "Burglar Target Selection.*Australia"
  )
)

for (special in special_matches) {
  master_candidates <- master$Title_of_the_study[str_detect(master$Title_of_the_study, special$master_pattern)]
  clean_candidates <- clean$Title[str_detect(clean$Title, special$clean_pattern)]
  
  if (length(master_candidates) > 0 && length(clean_candidates) > 0) {
    for (mc in master_candidates) {
      for (cc in clean_candidates) {
        # Skip if already matched
        if (mc %in% matches$master_title) next
        
        matches <- bind_rows(matches, data.frame(
          master_title = mc,
          clean_title = cc,
          match_type = "special_pattern"
        ))
        break
      }
    }
  }
}

# Method 5: Reverse fuzzy match (clean contained in master)
for (j in 1:nrow(clean_norm)) {
  clean_title <- clean_norm$Title[j]
  clean_norm_title <- clean_norm$title_norm[j]
  
  # Skip if already matched
  if (clean_title %in% matches$clean_title) next
  
  # Look for master titles that contain the clean title
  reverse_match <- master_norm$Title_of_the_study[str_detect(master_norm$title_norm, str_escape(clean_norm_title))]
  if (length(reverse_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = reverse_match[1],
      clean_title = clean_title,
      match_type = "reverse_fuzzy"
    ))
  }
}


# Merge using the comprehensive matching results - START WITH MASTER DATASET
merged <- master %>%
  left_join(matches, by = c("Title_of_the_study" = "master_title")) %>%
  left_join(clean, by = c("clean_title" = "Title")) %>%
  select(-clean_title)  # Remove the helper column

# =============================================================================
# CRIME TYPE STANDARDIZATION
# =============================================================================

# Function to standardize and group similar crime types (vectorized)
standardize_crime_types <- function(crime_type_vector) {
  # Handle NA and empty values
  crime_type_vector[is.na(crime_type_vector) | crime_type_vector == "" | crime_type_vector == "Not Specified"] <- "Not Specified"
  
  # Convert to lowercase for pattern matching
  crime_lower <- tolower(str_trim(crime_type_vector))
  
  # Apply standardization
  result <- case_when(
    # Group all burglary-related crimes together (as you requested)
    str_detect(crime_lower, "burglary|burglar|residential break|breaking and enter|break.*enter") ~ "Burglary",
    
    # Keep robbery separate
    str_detect(crime_lower, "robbery|robber") ~ "Robbery",
    
    # Keep theft separate  
    str_detect(crime_lower, "theft|stealing") ~ "Theft",
    
    # Group all multiple/mixed crime types together (including "Not Specified" as general crimes)
    # Handle "Any-Crime-Type Offences" specifically first
    str_detect(crime_lower, "any.*crime.*type.*offences|any-crime-type offences") ~ "Multiple/Mixed Crime Types",
    str_detect(crime_lower, "multiple types|any.*crime.*type|group crime|mixed|any type of crime|any-crime-type") ~ "Multiple/Mixed Crime Types",
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

# Apply standardization to the merged dataset
merged <- merged %>%
  mutate(
    # Create standardized crime type column
    Crime_Type_Standardized = standardize_crime_types(Crime_Type)
  ) %>%
  # Remove any extra standardized columns that might exist
  select(-any_of("Crime_Type_Enhanced_Standardized")) %>%
  # Rearrange columns to put Crime_Type_Standardized right after Crime_Type
  {
    # Find position of Crime_Type column
    crime_type_pos <- which(names(.) == "Crime_Type")
    
    if(length(crime_type_pos) > 0) {
      # Get all column names
      all_cols <- names(.)
      
      # Create new column order
      cols_before <- all_cols[1:crime_type_pos]
      cols_after <- all_cols[-(1:(crime_type_pos+1))]  # Skip Crime_Type_Standardized if it exists
      cols_after <- cols_after[cols_after != "Crime_Type_Standardized"]  # Remove if exists elsewhere
      
      new_order <- c(cols_before, "Crime_Type_Standardized", cols_after)
      
      # Select columns in new order
      select(., all_of(new_order))
    } else {
      .
    }
  }

# Create crime type analysis summary
crime_analysis_summary <- merged %>%
  filter(!is.na(Crime_Type_Standardized) & Crime_Type_Standardized != "Not Specified") %>%
  group_by(Crime_Type_Standardized) %>%
  summarise(
    N_Studies = n(),
    Mean_Unit_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Unit_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    SD_Unit_Size = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    Min_Unit_Size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_Unit_Size = round(max(Unit_size_km2, na.rm = TRUE), 4),
    Countries = paste(unique(Country[!is.na(Country)]), collapse = "; "),
    Sample_Size_Range = if("Sample_Size_Numeric" %in% names(merged)) {
      paste0(
        round(min(Sample_Size_Numeric, na.rm = TRUE), 0), " - ", 
        round(max(Sample_Size_Numeric, na.rm = TRUE), 0)
      )
    } else {
      "N/A"
    },
    .groups = 'drop'
  ) %>%
  arrange(desc(N_Studies)) %>%
  mutate(
    # Add percentage of total studies
    Percentage_of_Studies = round((N_Studies / sum(N_Studies)) * 100, 1),
    
    # Add size categories
    Size_Category = case_when(
      Median_Unit_Size_km2 < 0.001 ~ "Micro (< 0.001 km²)",
      Median_Unit_Size_km2 < 0.01 ~ "Very Small (0.001-0.01 km²)",
      Median_Unit_Size_km2 < 0.1 ~ "Small (0.01-0.1 km²)",
      Median_Unit_Size_km2 < 1 ~ "Medium (0.1-1 km²)",
      Median_Unit_Size_km2 < 10 ~ "Large (1-10 km²)",
      TRUE ~ "Very Large (≥10 km²)"
    )
  )

# Print the standardized crime type analysis
cat("\n=== STANDARDIZED CRIME TYPE ANALYSIS ===\n")
print(crime_analysis_summary)

# Create mapping table showing original to standardized groupings
mapping_table <- merged %>%
  select(Original_Crime_Type = Crime_Type, Standardized_Crime_Type = Crime_Type_Standardized) %>%
  filter(!is.na(Original_Crime_Type) & !is.na(Standardized_Crime_Type)) %>%
  distinct() %>%
  arrange(Standardized_Crime_Type, Original_Crime_Type)

cat("\n=== CRIME TYPE MAPPING ===\n")
print(mapping_table)

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total Studies:", nrow(merged), "\n")
cat("Studies with Crime Type Info:", sum(!is.na(merged$Crime_Type_Standardized) & 
                                         merged$Crime_Type_Standardized != "Not Specified"), "\n")
cat("Most Common Crime Type:", crime_analysis_summary$Crime_Type_Standardized[1], 
    "(", crime_analysis_summary$N_Studies[1], "studies,", 
    crime_analysis_summary$Percentage_of_Studies[1], "%)\n")
cat("Crime Types with Single Studies:", sum(crime_analysis_summary$N_Studies == 1), "\n")

# Save the merged result with standardized crime types
write_csv(merged, paste0(output_folder, "/", today_date, "_standardized_unit_sizes_with_groups_merged.csv"))

# Save the crime type analysis summary
write_csv(crime_analysis_summary, paste0(output_folder, "/", today_date, "_standardized_crime_type_analysis.csv"))

# Save the mapping table
write_csv(mapping_table, paste0(output_folder, "/", today_date, "_crime_type_mapping.csv"))

cat("\n=== FILES SAVED ===\n")
cat("1. Main dataset (with standardized crime types):", paste0(today_date, "_standardized_unit_sizes_with_groups_merged.csv\n"))
cat("2. Crime type analysis summary:", paste0(today_date, "_standardized_crime_type_analysis.csv\n"))
cat("3. Original to standardized mapping:", paste0(today_date, "_crime_type_mapping.csv\n"))




