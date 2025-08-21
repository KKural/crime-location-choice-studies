# Extraction Script

# Note: Using explicit namespace calls (package::function) instead of loading libraries
# This approach avoids potential namespace conflicts and makes dependencies clear

# Set up input/output folder configuration -----------------
analysis_date <- Sys.Date()  # Use today's date
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Analysis & Results")  # Today's folder for output

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}


# Output folder creation and utility functions --------------------------------

# Function to create a folder with a date argument
make_folder <- function(date = analysis_date, subfolder = NULL) {
  # Use the fixed analysis date instead of current date
  folder_name <- format(date, "%Y%m%d")
  main_folder_name <- paste0(folder_name, "_Analysis & Results")
  
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)
  }
  
  return(full_folder_path)
}

# Create a function to save output with date
custom_save <- function(data, folder_name, file_description, save_function, file_extension = ".csv", ...) {
  # Use analysis_date instead of current date
  current_date <- format(analysis_date, "%Y%m%d")
  
  # Ensure file description has the correct extension
  if (!grepl(paste0("\\", file_extension, "$"), file_description)) {
    file_description <- paste0(file_description, file_extension)
  }
  
  # Create the file name using the date and the file description
  file_name <- paste0(current_date, "_", file_description)
  
  # Define the path for the output file
  file_path <- here::here(folder_name, file_name)
  
  # Use the provided save function with appropriate parameters for CSV files
  if (file_extension == ".csv" && identical(save_function, readr::write_csv)) {
    # For readr::write_csv, ensure UTF-8 encoding by not specifying locale (UTF-8 is default)
    save_function(data, file_path, ...)
  } else {
    save_function(data, file_path, ...)
  }
  
  return(file_path)  # Return the file path
}

# Create output folder
output_folder <- make_folder()
folder_name <- output_folder

# Data reading and initial processing -----------------------------------------

# List of CSV files to merge - All 6 Elicit extraction files
combined_csv_files <- c(
  "Data/20250730_geo_unit_basic.csv",
  "Data/20250730_unit_details_rationale.csv",
  "Data/20250730_constraints_data.csv",
  "Data/20250730_data_crime.csv",
  "Data/20250730_model_stats.csv",
  "Data/20250730_analysis_methods.csv"
)

# Function to clean column names (keep supporting quotes and reasoning, remove only tables and metadata)
clean_column_names <- function(df) {
  # Remove only supporting tables columns and metadata, but keep supporting quotes and reasoning
  main_cols <- colnames(df)[!grepl("Supporting  tables|DOI|Venue|Citation count", colnames(df))]
  return(df[, main_cols])
}

# Function to remove duplicate columns (Citation, Year) from subsequent files
remove_duplicate_cols <- function(df, is_first_file = FALSE) {
  if (is_first_file) {
    return(df)
  } else {
    # Remove Citation, Year, and DOI link columns from subsequent files to avoid duplicates
    cols_to_remove <- c("Citation", "Year", "DOI link", "Authors")  # Include Authors in case it exists
    remaining_cols <- setdiff(names(df), cols_to_remove)
    return(df[, remaining_cols])
  }
}

# Read and merge all CSV files
df_combined <- NULL

for (i in 1:length(combined_csv_files)) {
  if (file.exists(combined_csv_files[i])) {
    temp_df <- readr::read_csv(combined_csv_files[i], 
                              show_col_types = FALSE, 
                              locale = readr::locale(encoding = "UTF-8"))
    temp_df <- clean_column_names(temp_df)
    
    if (is.null(df_combined)) {
      # First file becomes the base dataset (keep Authors and Year)
      df_combined <- temp_df
    } else {
      # Remove duplicate columns from subsequent files
      temp_df <- remove_duplicate_cols(temp_df, is_first_file = FALSE)
      # Merge subsequent files by Title
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
  }
}

# Clean column names - remove backslashes and convert dots to underscores
names(df_combined) <- gsub('\\\\"', '"', names(df_combined))
# Convert to proper R column names and replace dots with underscores
names(df_combined) <- make.names(names(df_combined))
names(df_combined) <- gsub("\\.", "_", names(df_combined))

# Define the desired column order - each variable followed by its supporting quotes and reasoning
# Using proper R column names with underscores instead of dots
desired_order <- c(
  "Title", "Citation", "Year",
  "Country", "Supporting_quotes_for__Country_", "Reasoning_for__Country_",
  "City", "Supporting_quotes_for__City__", "Reasoning_for__City__",
  "Total_Study_Area_Size", "Supporting_quotes_for__Total_Study_Area_Size_", "Reasoning_for__Total_Study_Area_Size_",
  "Spatial_Unit_Name", "Supporting_quotes_for__Spatial_Unit_Name_", "Reasoning_for__Spatial_Unit_Name_",
  "Unit_Size", "Supporting_quotes_for__Unit_Size_", "Reasoning_for__Unit_Size_",
  "Number_of_Units", "Supporting_quotes_for__Number_of_Units_", "Reasoning_for__Number_of_Units_",
  "Average_Population_per_Unit", "Supporting_quotes_for__Average_Population_per_Unit_", "Reasoning_for__Average_Population_per_Unit_",
  "Spatial_Aggregation", "Supporting_quotes_for__Spatial_Aggregation_", "Reasoning_for__Spatial_Aggregation_",
  "Unit_Selection_Rationale", "Supporting_quotes_for__Unit_Selection_Rationale_", "Reasoning_for__Unit_Selection_Rationale_",
  "Rationale_Category", "Supporting_quotes_for___Rationale_Category_", "Reasoning_for___Rationale_Category_",
  "Data_Limitations", "Supporting_quotes_for__Data_Limitations_", "Reasoning_for__Data_Limitations_",
  "Computational_Constraints", "Supporting_quotes_for__Computational_Constraints_", "Reasoning_for__Computational_Constraints_",
  "Alternative_Units", "Supporting_quotes_for__Alternative_Units_", "Reasoning_for__Alternative_Units_",
  "Data_Collection_Period", "Supporting_quotes_for__Data_Collection_Period_", "Reasoning_for__Data_Collection_Period_",
  "Data_Sources", "Supporting_quotes_for__Data_Sources_", "Reasoning_for__Data_Sources_",
  "Number_of_Data_Sources", "Supporting_quotes_for__Number_of_Data_Sources_", "Reasoning_for__Number_of_Data_Sources_",
  "Crime_Type", "Supporting_quotes_for__Crime_Type_", "Reasoning_for__Crime_Type_",
  "Crime_Type_Group", "Supporting_quotes_for__Crime_Type_Group_", "Reasoning_for__Crime_Type_Group_",
  "Crime_Incidents", "Supporting_quotes_for__Crime_Incidents_", "Reasoning_for__Crime_Incidents_",
  "Model_Type", "Supporting_quotes_for__Model_Type_", "Reasoning_for__Model_Type_",
  "Independent_Variables", "Supporting_quotes_for__Independent_Variables_", "Reasoning_for__Independent_Variables_",
  "Number_of_Variables", "Supporting_quotes_for__Number_of_Variables_", "Reasoning_for__Number_of_Variables_",
  "Model_Fit_Statistics", "Supporting_quotes_for___Model_Fit_Statistics_", "Reasoning_for___Model_Fit_Statistics_",
  "Coefficients", "Supporting_quotes_for__Coefficients_", "Reasoning_for__Coefficients_",
  "Confidence_Intervals", "Supporting_quotes_for__Confidence_Intervals_", "Reasoning_for__Confidence_Intervals_",
  "Effect_Sizes", "Supporting_quotes_for__Effect_Sizes_", "Reasoning_for__Effect_Sizes_",
  "Software_Used", "Supporting_quotes_for__Software_Used_", "Reasoning_for__Software_Used_",
  "MAUP_Discussion", "Supporting_quotes_for__MAUP_Discussion_", "Reasoning_for__MAUP_Discussion_",
  "Sensitivity_Analysis", "Supporting_quotes_for__Sensitivity_Analysis_", "Reasoning_for__Sensitivity_Analysis_",
  "Future_Suggestions", "Supporting_quotes_for__Future_Suggestions_", "Reasoning_for__Future_Suggestions_"
)

# Check which columns exist in the data and only select those
existing_columns <- intersect(desired_order, names(df_combined))
missing_columns <- setdiff(desired_order, names(df_combined))

# Reorder columns according to the desired order
df_combined <- df_combined[, existing_columns]

# =============================================================================
# FUZZY MATCHING AND STUDY ID ASSIGNMENT
# =============================================================================

# Reference study mapping with correct titles, citations, and years
reference_studies <- data.frame(
  Study_ID = 1:51,
  Title_of_the_study = c(
    "A discrete spatial choice model of burglary target selection at the house-level",
    "Residential burglary target selection: An analysis at the property-level using Google Street View",
    "Graffiti Writers Choose Locations That Optimize Exposure",
    "Where Do Dealers Solicit Customers and Sell Them Drugs",
    "Go where the money is: Modeling street robbers' location choices",
    "Do Street Robbery Location Choices Vary Over Time of Day or Day of Week? A Test in Chicago",
    "The usefulness of past crime data as an attractiveness index for residential burglars",
    "Role of the Street Network in Burglars' Spatial Decision-Making",
    "discrete choice analysis of spatial attack sites",
    "Adolescent offenders' current whereabouts predict locations of their future crime",
    "spatial analysis with preference specification of latent decision makers for criminal event prediction",
    "Modelling the spatial decision making of terrorists: The discrete choice approach",
    "Testing Ecological Theories of Offender Spatial Decision Making Using a Discrete Choice Model (LSOA)",
    "Target Choice During Extreme Events: A Discrete Spatial Choice Model of the 2011 London Riots (LSOA)",
    "Situating Crime Pattern Theory Into The Explanation Of Co-Offending: Considering Area-Level Convergence Spaces",
    "Modelling taste heterogeneity regarding offence location choices (Census output area)",
    "Learning where to offend: Effects of past on future burglary locations (UK Census Lower Level Super Output Area (LSOA)",
    "How do residential burglars select target areas? : A new approach to the analysis of criminal location choice",
    "Co-offending and the Choice of Target Areas in Burglary",
    "Burglar Target Selection: A Cross-national Comparison (NL)",
    "Effect Attractiveness Opportunity And Accessibility To Burglars On Residential Burglary Rates Of Urban Neighborhoods",
    "Where offenders choose to attack: A discrete choice model of robberies in Chicago",
    "Location Location Location: Effects of Neighborhood and House Attributes on Burglars' Target Selection",
    "Relative Difference and Burglary Location: Can Ecological Characteristics of a Burglar's Home Neighborhood Predict Offense Location?",
    "divergent decisionmaking in context neighborhood context shapes effects of physical disorder and spatial knowledge on burglars location choice",
    "familiar locations and similar activities examining the contributions of reliable and relevant knowledge in offenders crime location choices",
    "Traveling Alone or Together? Neighborhood Context on Individual and Group Juvenile and Adult Burglary Decisions",
    "Relationships Between Offenders' Crime Locations and Different Prior Activity Locations as Recorded in Police Data",
    "Crime Feeds on Legal Activities: Daily Mobility Flows Help to Explain Thieves' Target Location Choices",
    "Assessing the influence of prior on subsequent street robbery location choices: A case study in ZG City China",
    "Ambient population and surveillance cameras: The guardianship role in street robbers' crime location choice",
    "Do Migrant and Native Robbers Target Different Places?",
    "Do juvenile, young adult, and adult offenders target different places in the Chinese context?",
    "Awareness×Opportunity: Testing Interactions Between Activity Nodes and Criminal Opportunity in Predicting Crime Location Choice",
    "The Importance of Importance Sampling: Exploring Methods of Sampling from Alternatives in Discrete Choice Models of Crime Location Choice",
    "Burglar Target Selection: A Cross-national Comparison (Super Output Areas - UK)",
    "Location Choice of Snatching Offenders in Chennai City (Wards)",
    "The Influence of Activity Space and Visiting Frequency on Crime Location Choice: Findings from an Online Self-Report Survey",
    "Right place right time? Making crime pattern theory time-specific",
    "Burglars blocked by barriers The impact of physical and social barriers on residential burglars target location choices in China",
    "Investigating the effect of people on the street and streetscape physical environment on the location choice of street theft crime offenders using street view images and a discrete spatial choice model",
    "Biting Once Twice: the Influence of Prior on Subsequent Crime Location Choice",
    "Co-offenders' crime location choice: Do co-offending groups commit crimes in their shared awareness space?",
    "Family Matters: Effects of Family Members' Residential Areas on Crime Location Choice",
    "A Time for a Crime: Temporal Aspects of Repeat Offenders' Crime Location Choices",
    "A Sentimental Journey To Crime : Effects of Residential History on Crime Location Choice",
    "Modeling micro-level crime location choice: Application of the discrete choice framework to crime at places",
    "Effects of Residential history on Commercial Robbers' Crime Location Choices",
    "Formal evaluation of the impact of barriers and connectors on residential burglars' macro-level offending location choices",
    "Burglar Target Selection: A Cross-national Comparison (statistical local areas - AU)",
    "Target Selection Models with Preference Variation Between Offenders"
  ),
  Citation = c(
    "(Vandeviver et al., 2015)",
    "(Langton & Steenbeek, 2017)",
    "(Kuralarasa et al, 2024)",
    "(Bernasco & Jacques, 2015)",
    "(Bernasco et al., 2013)",
    "(Bernasco et al., 2017)",
    "(Hanayama et al., 2018)",
    "(Frith et al., 2017)",
    "(Smith & Brown, 2007)",
    "(Bernasco, 2019)",
    "(Xue & Brown, 2006)",
    "(Marchment & Gill, 2019)",
    "(Johnson & Summers, 2015)",
    "(Baudains et al., 2013)",
    "(Rowan, Appleby & McGloin, 2022)",
    "(Frith, 2019)",
    "(Bernasco et al., 2015)",
    "(Bernasco & Nieuwbeerta, 2005)",
    "(Bernasco, 2006)",
    "(Townsley et al., 2015)",
    "(Bernasco & Luykx, 2003)",
    "(Bernasco & Block, 2009)",
    "(Vandeviver & Bernasco, 2020)",
    "(A. W. Chamberlain & Boggess, 2016)",
    "(Cai et al., 2024)",
    "(Curtis-Ham et al., 2025)",
    "(A. Chamberlain et al., 2022)",
    "(Curtis-Ham et al., 2022a)",
    "(Song et al., 2019)",
    "(Long et al., 2018)",
    "(Long et al., 2021)",
    "(Long & Liu, 2021)",
    "(Long & Liu, 2022)",
    "(Menting, 2018)",
    "(Curtis-Ham et al., 2022b)",
    "(Townsley et al., 2015)",
    "(Kuralarasan & Bernasco, 2022)",
    "(Menting et al., 2020)",
    "(S. van Sleeuwen et al., 2021)",
    "(Xiao et al., 2021)",
    "(Yue et al., 2023)",
    "(Lammers et al., 2015)",
    "(Lammers, 2017)",
    "(Menting et al., 2016)",
    "(S. E. M. van Sleeuwen et al., 2018)",
    "(Bernasco, 2010a)",
    "(Bernasco, 2010b)",
    "(Bernasco & Kooistra, 2010)",
    "(Clare et al., 2009)",
    "(Townsley et al., 2015)",
    "(Townsley et al., 2016)"
  ),
  Year = c(
    2015, 2017, 2024, 2015, 2013, 2017, 2018, 2017, 2007, 2019,
    2006, 2019, 2015, 2013, 2022, 2019, 2015, 2005, 2006, 2015,
    2003, 2009, 2020, 2016, 2024, 2025, 2022, 2022, 2019, 2018,
    2021, 2021, 2022, 2018, 2022, 2015, 2022, 2020, 2021, 2021,
    2023, 2015, 2017, 2016, 2018, 2010, 2010, 2010, 2009, 2015,
    2016
  ),
  stringsAsFactors = FALSE
)

# Function for fuzzy string matching
fuzzy_match_title <- function(title, reference_titles) {
  if (is.na(title) || title == "") return(NA)
  
  # Normalize titles for comparison
  normalize_for_matching <- function(text) {
    text <- tolower(stringr::str_trim(text))
    text <- stringr::str_replace_all(text, "[^a-z0-9\\s]", " ")
    text <- stringr::str_replace_all(text, "\\s+", " ")
    text <- stringr::str_trim(text)
    return(text)
  }
  
  normalized_input <- normalize_for_matching(title)
  normalized_refs <- sapply(reference_titles, normalize_for_matching)
  
  # Calculate similarity scores using different methods
  scores <- sapply(normalized_refs, function(ref) {
    if (nchar(normalized_input) == 0 || nchar(ref) == 0) return(0)
    
    # Jaccard similarity for word sets
    words_input <- unique(unlist(strsplit(normalized_input, "\\s+")))
    words_ref <- unique(unlist(strsplit(ref, "\\s+")))
    intersection <- length(intersect(words_input, words_ref))
    union <- length(union(words_input, words_ref))
    jaccard <- if(union > 0) intersection / union else 0
    
    # Substring matching score
    common_substrings <- 0
    for(word in words_input) {
      if(any(stringr::str_detect(words_ref, word))) {
        common_substrings <- common_substrings + 1
      }
    }
    substring_score <- if(length(words_input) > 0) common_substrings / length(words_input) else 0
    
    # Combined score
    return(0.7 * jaccard + 0.3 * substring_score)
  })
  
  max_score <- max(scores)
  best_match_idx <- which.max(scores)
  
  # Return match if score is above threshold
  if(max_score > 0.6) {
    return(best_match_idx)
  } else {
    return(NA)
  }
}

# Apply fuzzy matching to get Study_IDs
df_combined$Study_ID <- sapply(df_combined$Title, function(title) {
  match_idx <- fuzzy_match_title(title, reference_studies$Title_of_the_study)
  if(!is.na(match_idx)) {
    return(reference_studies$Study_ID[match_idx])
  } else {
    return(NA)
  }
})

# Update titles to correct format and Title Case
df_combined$Title <- sapply(1:nrow(df_combined), function(i) {
  study_id <- df_combined$Study_ID[i]
  if(!is.na(study_id)) {
    # Get correct title from reference
    correct_title <- reference_studies$Title_of_the_study[reference_studies$Study_ID == study_id]
    return(stringr::str_to_title(correct_title))
  } else {
    # Convert existing title to Title Case
    return(stringr::str_to_title(df_combined$Title[i]))
  }
})

# Clean titles - remove UGent repository text and other unwanted content
df_combined$Title <- sapply(df_combined$Title, function(title) {
  if (is.na(title) || title == "") return(title)
  
  # Remove UGent repository text
  title <- stringr::str_replace_all(title, 
                                    "The Ugent Institutional Repository Is The Electronic Archiving And Dissemination Platform For All Ugent Research Publications\\. Ghent University Has Implemented A Mandate Stipulating That All Academic Publications Of Ugent Researchers Should Be Deposited And Archived In This Repository\\. Except For Items Where Current Copyright Restrictions Apply, These Papers Are Available In Open Access", 
                                    "")
  
  # Clean up extra whitespace
  title <- stringr::str_trim(title)
  title <- stringr::str_replace_all(title, "\\s+", " ")
  
  return(title)
})

# Handle specific manual corrections before fuzzy matching
df_combined$Title <- sapply(df_combined$Title, function(title) {
  if (is.na(title) || title == "") return(title)
  
  # Handle the specific case for Study ID 35
  if (stringr::str_detect(tolower(title), "the importance of importance sampling")) {
    return("The Importance of Importance Sampling: Exploring Methods of Sampling from Alternatives in Discrete Choice Models of Crime Location Choice")
  }
  
  return(title)
})

# Apply fuzzy matching with updated Study_IDs (overwriting the previous assignment)
df_combined$Study_ID <- sapply(df_combined$Title, function(title) {
  match_idx <- fuzzy_match_title(title, reference_studies$Title_of_the_study)
  if(!is.na(match_idx)) {
    return(reference_studies$Study_ID[match_idx])
  } else {
    return(NA)
  }
})

# Update titles to correct format and Title Case
df_combined$Title <- sapply(1:nrow(df_combined), function(i) {
  study_id <- df_combined$Study_ID[i]
  if(!is.na(study_id)) {
    # Get correct title from reference
    correct_title <- reference_studies$Title_of_the_study[reference_studies$Study_ID == study_id]
    return(stringr::str_to_title(correct_title))
  } else {
    # Convert existing title to Title Case
    return(stringr::str_to_title(df_combined$Title[i]))
  }
})

# Add Citation and Year columns based on Study_ID matches
df_combined$Citation <- sapply(df_combined$Study_ID, function(study_id) {
  if(!is.na(study_id) && study_id != "") {
    match_row <- which(reference_studies$Study_ID == study_id)
    if(length(match_row) > 0) {
      return(reference_studies$Citation[match_row[1]])
    }
  }
  return("")  # Return empty string for unmatched studies
})

df_combined$Year <- sapply(df_combined$Study_ID, function(study_id) {
  if(!is.na(study_id) && study_id != "") {
    match_row <- which(reference_studies$Study_ID == study_id)
    if(length(match_row) > 0) {
      return(reference_studies$Year[match_row[1]])
    }
  }
  return("")  # Return empty string for unmatched studies
})

# Handle special case: Studies 36 and 50 are the same as Study 20 but different regions
# Only create these studies if they don't already exist in the dataset
base_study_20 <- df_combined[df_combined$Study_ID == 20 & !is.na(df_combined$Study_ID), ]
existing_study_36 <- any(df_combined$Study_ID == 36 & !is.na(df_combined$Study_ID))
existing_study_50 <- any(df_combined$Study_ID == 50 & !is.na(df_combined$Study_ID))

if(nrow(base_study_20) > 0 && (!existing_study_36 || !existing_study_50)) {
  studies_to_add <- list()
  
  # Create Study 36 (UK version) only if it doesn't exist
  if(!existing_study_36) {
    study_36 <- base_study_20
    study_36$Study_ID <- 36
    study_36$Title <- stringr::str_to_title(reference_studies$Title_of_the_study[reference_studies$Study_ID == 36])
    study_36$Citation <- reference_studies$Citation[reference_studies$Study_ID == 36]
    study_36$Year <- reference_studies$Year[reference_studies$Study_ID == 36]
    studies_to_add <- append(studies_to_add, list(study_36))
  }
  
  # Create Study 50 (AU version) only if it doesn't exist
  if(!existing_study_50) {
    study_50 <- base_study_20
    study_50$Study_ID <- 50
    study_50$Title <- stringr::str_to_title(reference_studies$Title_of_the_study[reference_studies$Study_ID == 50])
    study_50$Citation <- reference_studies$Citation[reference_studies$Study_ID == 50]
    study_50$Year <- reference_studies$Year[reference_studies$Study_ID == 50]
    studies_to_add <- append(studies_to_add, list(study_50))
  }
  
  # Add new studies to combined dataset if any were created
  if(length(studies_to_add) > 0) {
    for(study in studies_to_add) {
      df_combined <- rbind(df_combined, study)
    }
  }
}

# Remove rows that contain only UGent repository artifacts or are empty studies
df_combined <- df_combined[!(is.na(df_combined$Study_ID) | df_combined$Study_ID == "") | 
                             (!is.na(df_combined$Title) & df_combined$Title != "" & 
                                !stringr::str_detect(tolower(df_combined$Title), "ugent|institutional repository")), ]

# Convert Study_ID NAs to empty strings for unmatched studies
df_combined$Study_ID[is.na(df_combined$Study_ID)] <- ""

# Add verification columns
df_combined$Verified_by_Kural <- ""
df_combined$Verified_by_Stephanie <- ""

# Order the dataframe by Study_ID in ascending order (put empty Study_IDs at the end)
df_combined$Study_ID_numeric <- as.numeric(df_combined$Study_ID)
df_combined <- df_combined[order(df_combined$Study_ID_numeric, na.last = TRUE), ]
df_combined$Study_ID_numeric <- NULL

# Reorganize columns to put Study_ID first, then Title, Citation, Year, and verification columns last
verification_cols <- c("Verified_by_Kural", "Verified_by_Stephanie")
other_cols <- setdiff(names(df_combined), c("Study_ID", "Title", "Citation", "Year", verification_cols))
df_combined <- df_combined[, c("Study_ID", "Title", "Citation", "Year", other_cols, verification_cols)]

# Clean up formatting in all columns
for (col in names(df_combined)) {
  # Skip the Study_ID and verification columns (keep them as empty strings)
  if (col %in% c("Study_ID", "Verified_by_Kural", "Verified_by_Stephanie")) {
    next
  }
  
  # Process each cell in the column
  for (i in 1:nrow(df_combined)) {
    cell_value <- df_combined[[col]][i]
    if (!is.na(cell_value) && cell_value != "" && !cell_value %in% c("-", "- ", "--", "---", "N/A", "n/a", "NA")) {
      # Clean up quotes and dashes at the beginning and end of content only
      cell_value <- stringr::str_trim(cell_value)  # Remove leading/trailing whitespace
      
      # Only remove leading dashes if they appear to be formatting artifacts
      # (i.e., at the very beginning of the cell, not part of legitimate content like "2009 - 2010")
      if (stringr::str_detect(cell_value, "^-+\\s")) {
        cell_value <- stringr::str_replace(cell_value, "^-+\\s*", "")  # Remove leading dashes with space
      }
      
      # Remove leading quotes if they appear to be formatting artifacts
      if (stringr::str_detect(cell_value, "^\"")) {
        cell_value <- stringr::str_replace(cell_value, "^\"\\s*", "")  # Remove leading quote
      }
      
      # Only remove trailing dashes if they appear to be formatting artifacts
      # (i.e., at the very end of the cell, not part of legitimate content)
      if (stringr::str_detect(cell_value, "\\s-+$")) {
        cell_value <- stringr::str_replace(cell_value, "\\s*-+$", "")  # Remove trailing dashes with space
      }
      
      # Remove trailing quotes if they appear to be formatting artifacts
      if (stringr::str_detect(cell_value, "\\s*\"$")) {
        cell_value <- stringr::str_replace(cell_value, "\\s*\"$", "")  # Remove trailing quote
      }
      
      # Replace double quotes with single quotes (but preserve legitimate quoted content)
      cell_value <- stringr::str_replace_all(cell_value, '""', '"')
      
      # Final trim
      cell_value <- stringr::str_trim(cell_value)
      
      df_combined[[col]][i] <- cell_value
    } else if (is.na(cell_value) || cell_value %in% c("-", "- ", "--", "---", "N/A", "n/a", "NA")) {
      # Set truly empty cells to empty string
      df_combined[[col]][i] <- ""
    }
  }
}

# Enhanced area parsing: extract numeric km^2 values where possible and create numeric columns
parse_area_to_km2 <- function(s) {
  # Returns numeric km^2 or NA_real_ if cannot parse
  if (is.null(s)) return(NA_real_)
  s_orig <- as.character(s)
  s <- tolower(s_orig)
  s <- stringr::str_replace_all(s, "Â", "")
  s <- stringr::str_replace_all(s, "\u00a0", " ")
  s <- stringr::str_replace_all(s, ",", "") # remove commas for parsing
  s <- stringr::str_replace_all(s, "\\s+", " ")
  s <- stringr::str_trim(s)

  if (s == "" || stringr::str_detect(s, "not specified|not provided|no specific|does not provide|n/a|none")) return(NA_real_)

  # If an explicit '=' with a numeric and unit on the right exists, prefer the RHS
  if (stringr::str_detect(s, "=\\s*[0-9]+(\\.[0-9]+)?\\s*(km|km2|km\\^2|km²|sq|square|m2|m\\^2|m²|square miles|sq miles|mi2)")) {
    rhs <- stringr::str_replace(s, ".*=", "")
    s <- stringr::str_trim(rhs)
  }

  # Try to extract a number followed by km variants
  m_km <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(km²|km2|km\\^2|square kilometers|square kilometres|sq km|sq\\. km|sqkm)")
  if (!all(is.na(m_km))) {
    return(as.numeric(m_km[2]))
  }

  # Explicit number with 'km' (e.g., '3197.26 km 2' or 'more than 3000 km 2')
  m_km2 <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(km|km2|km\\^2|km²)\\b")
  if (!all(is.na(m_km2))) return(as.numeric(m_km2[2]))

  # Range with median in parentheses: prefer median
  m_median <- stringr::str_match(s, "median[^0-9]*([0-9]+(?:\\.[0-9]+)?)\\s*(km|km2|km\\^2|km²)")
  if (!all(is.na(m_median))) return(as.numeric(m_median[2]))

  # Range 'a to b km2' -> take midpoint
  m_range <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(km|km2|km\\^2|km²)\\s*to\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(km|km2|km\\^2|km²)")
  if (!all(is.na(m_range))) {
    a <- as.numeric(m_range[2]); b <- as.numeric(m_range[4]); return((a + b) / 2)
  }

  # Multiplication expressions like '1971 * 1.62' or '142 * 2.96 km²' -> compute product
  m_mult <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*[*x\\u00D7]\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(km|km2|km\\^2|km²)?")
  if (!all(is.na(m_mult))) {
    prod <- as.numeric(m_mult[2]) * as.numeric(m_mult[3])
    # if unit explicitly km present use product as km^2, else also assume second number is km^2 avg
    return(as.numeric(prod))
  }

  # Look for an explicit final numeric with km mention anywhere
  m_any_km <- stringr::str_match_all(s, "([0-9]+(?:\\.[0-9]+)?)")[[1]]
  if (nrow(m_any_km) > 0 && stringr::str_detect(s, "km")) {
    # return the last numeric token
    val <- as.numeric(m_any_km[nrow(m_any_km), 1])
    return(val)
  }

  # m^2 patterns -> convert to km^2
  m_m2 <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(m²|m2|m\\^2|square metres|square meters|square metres|square meters)\\b")
  if (!all(is.na(m_m2))) {
    return(as.numeric(m_m2[2]) / 1e6)
  }

  # Patterns like '140 m x 140 m' or '140 m Â 140 m' -> compute area in m^2 then convert
  m_box <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(m)\\s*[x×\\*\\s]+\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(m)")
  if (!all(is.na(m_box))) {
    area_m2 <- as.numeric(m_box[2]) * as.numeric(m_box[4])
    return(area_m2 / 1e6)
  }

  # '500 m on a side' or '500 m on one side' -> square
  m_side <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*m\\s*(on a side|on one side|on the side)")
  if (!all(is.na(m_side))) {
    side <- as.numeric(m_side[2]); return((side * side) / 1e6)
  }

  # '200x200m' variant
  m_compact <- stringr::str_match(s, "([0-9]+)x([0-9]+)m")
  if (!all(is.na(m_compact))) {
    area_m2 <- as.numeric(m_compact[2]) * as.numeric(m_compact[3]); return(area_m2 / 1e6)
  }

  # square miles -> convert to km^2 (1 mi^2 = 2.58999 km^2)
  m_miles <- stringr::str_match(s, "([0-9]+(?:\\.[0-9]+)?)\\s*(square miles|sq miles|sq\\. miles|mi2|mi\\^2)\\b")
  if (!all(is.na(m_miles))) return(as.numeric(m_miles[2]) * 2.58999)

  # If nothing matched, return NA
  return(NA_real_)
}

# Create numeric km^2 columns where possible and tidy textual columns
if ("Total_Study_Area_Size" %in% names(df_combined)) {
  df_combined$Total_Study_Area_km2 <- vapply(df_combined$Total_Study_Area_Size, parse_area_to_km2, FUN.VALUE = numeric(1))
  # If parsed, overwrite textual column with clean formatting (no unnecessary decimals)
  parsed_idx <- which(!is.na(df_combined$Total_Study_Area_km2))
  if (length(parsed_idx) > 0) {
    # Use smart formatting: show decimals only when needed
    df_combined$Total_Study_Area_Size[parsed_idx] <- sapply(df_combined$Total_Study_Area_km2[parsed_idx], function(x) {
      if (x == round(x)) {
        paste0(round(x), " km²")  # No decimals for whole numbers
      } else {
        paste0(formatC(x, format = "f", digits = 2, drop0trailing = TRUE), " km²")  # Clean decimals
      }
    })
  }
}

if ("Unit_Size" %in% names(df_combined)) {
  df_combined$Unit_Size_km2 <- vapply(df_combined$Unit_Size, parse_area_to_km2, FUN.VALUE = numeric(1))
  parsed_idx2 <- which(!is.na(df_combined$Unit_Size_km2))
  if (length(parsed_idx2) > 0) {
    # Use smart formatting for unit sizes too
    df_combined$Unit_Size[parsed_idx2] <- sapply(df_combined$Unit_Size_km2[parsed_idx2], function(x) {
      if (x >= 1 && x == round(x)) {
        paste0(round(x), " km²")  # No decimals for whole numbers >= 1
      } else if (x >= 1) {
        paste0(formatC(x, format = "f", digits = 2, drop0trailing = TRUE), " km²")  # 2 decimals for >= 1
      } else {
        paste0(formatC(x, format = "f", digits = 4, drop0trailing = TRUE), " km²")  # 4 decimals for < 1
      }
    })
  }
}

# Save the combined and reorganized data set
# Use readr::write_excel_csv to ensure UTF-8 with BOM so the file opens correctly in Excel on Windows
custom_save(df_combined, output_folder, "combined_dataset", readr::write_excel_csv)
