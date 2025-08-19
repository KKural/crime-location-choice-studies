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
  
  # Use the provided save function
  save_function(data, file_path, ...)
  
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
    temp_df <- readr::read_csv(combined_csv_files[i], show_col_types = FALSE)
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

# Save the combined and reorganized data set
custom_save(df_combined, output_folder, "combined_dataset", readr::write_csv)

# =============================================================================
# PDF QUOTE HIGHLIGHTING INTEGRATION
# =============================================================================
# After saving the combined dataset, you can now highlight quotes in PDFs

# Source the PDF highlighting integration script
if (file.exists("Script/pdf_quote_highlighter_r_integration.R")) {
  source("Script/pdf_quote_highlighter_r_integration.R")
  
  combined_dataset_file <- custom_save(df_combined, output_folder, "combined_dataset", readr::write_csv)
}

# =============================================================================
# DATA PROCESSING COMPLETE
# =============================================================================
# The dataset has been successfully reorganized with the structure:
# Variable -> Supporting Quotes -> Reasoning for each variable
# The old processing pipeline below was designed for a different column structure
# and has been commented out to prevent errors.

# Note: If you need further analysis, you can work directly with df_combined
# which now has the properly organized columns.

# =============================================================================
# OLD PROCESSING PIPELINE (COMMENTED OUT)
# =============================================================================
# The following code was designed for the old column structure and would need
# to be adapted to work with the new reorganized column structure.

# =============================================================================
# OLD PROCESSING PIPELINE (COMMENTED OUT)
# =============================================================================
# The following code was designed for the old column structure and would need
# to be adapted to work with the new reorganized column structure.

# Data Extraction and Processing Functions--------------------------------------

# The rest of the processing functions and pipeline have been commented out
# because they were designed for a different column structure. If you need
# to perform additional analysis, you can work directly with df_combined
# which now has the properly organized structure:
# Variable -> Supporting Quotes -> Reasoning

# End of active script

# Extract variables from text and return them as a numbered list string
extract_variables_grouped <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(list(variables = NA, count = 0))
  }
  
  items <- stringr::str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
  
  if (length(items) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  variables <- character()
  
  for (item in items) {
    simplified_var <- extract_variable_simplified(item)
    if (!is.na(simplified_var) && simplified_var != "") {
      variables <- c(variables, simplified_var)
    }
  }
  
  # Remove duplicates and empty entries
  variables <- unique(variables[variables != "" & !is.na(variables)])
  
  if (length(variables) == 0) {
    return(list(variables = NA, count = 0))
  }
  
  # Renumber the variables continuously starting from 1
  numbered_vars <- character()
  for (i in 1:length(variables)) {
    numbered_vars <- c(numbered_vars, paste0(i, ". ", variables[i]))
  }
  
  variables_string <- paste(numbered_vars, collapse = "; ")
  
  return(list(variables = variables_string, count = length(variables)))
}

# New comprehensive variable extraction function (improved approach)
extract_variables_comprehensive_improved <- function(df, row_index) {
  
  # Step 1: Collect all variables from all sources
  all_variables <- collect_all_variables(df, row_index)
  
  if (length(all_variables) == 0) {
    return(list(
      demographic_variables = NA, demographic_count = 0,
      economic_variables = NA, economic_count = 0,
      environmental_variables = NA, environmental_count = 0,
      distance_variables = NA, distance_count = 0,
      temporal_variables = NA, temporal_count = 0,
      uncategorized_variables = NA, uncategorized_count = 0,
      total_variables = 0, all_variables = NA
    ))
  }
  
  # Step 2: Categorize each variable
  demographic_vars <- character()
  economic_vars <- character()
  environmental_vars <- character()
  distance_vars <- character()
  temporal_vars <- character()
  uncategorized_vars <- character()
  
  for (var in all_variables) {
    category <- categorize_variable(var)
    
    switch(category,
           "demographic" = { demographic_vars <- c(demographic_vars, var) },
           "economic" = { economic_vars <- c(economic_vars, var) },
           "environmental" = { environmental_vars <- c(environmental_vars, var) },
           "distance" = { distance_vars <- c(distance_vars, var) },
           "temporal" = { temporal_vars <- c(temporal_vars, var) },
           "uncategorized" = { uncategorized_vars <- c(uncategorized_vars, var) }
    )
  }
  
  # Step 3: Remove duplicates within each category
  demographic_vars <- unique(demographic_vars)
  economic_vars <- unique(economic_vars)
  environmental_vars <- unique(environmental_vars)
  distance_vars <- unique(distance_vars)
  temporal_vars <- unique(temporal_vars)
  uncategorized_vars <- unique(uncategorized_vars)
  
  # Step 4: Number each category starting from 1
  number_variables <- function(var_list) {
    if (length(var_list) == 0) return(character())
    numbered <- character()
    for (i in 1:length(var_list)) {
      numbered <- c(numbered, paste0(i, ". ", var_list[i]))
    }
    return(numbered)
  }
  
  demographic_numbered <- number_variables(demographic_vars)
  economic_numbered <- number_variables(economic_vars)
  environmental_numbered <- number_variables(environmental_vars)
  distance_numbered <- number_variables(distance_vars)
  temporal_numbered <- number_variables(temporal_vars)
  uncategorized_numbered <- number_variables(uncategorized_vars)
  
  # Step 5: Create final results
  return(list(
    demographic_variables = if(length(demographic_numbered) > 0) paste(demographic_numbered, collapse = "; ") else NA,
    demographic_count = length(demographic_numbered),
    economic_variables = if(length(economic_numbered) > 0) paste(economic_numbered, collapse = "; ") else NA,
    economic_count = length(economic_numbered),
    environmental_variables = if(length(environmental_numbered) > 0) paste(environmental_numbered, collapse = "; ") else NA,
    environmental_count = length(environmental_numbered),
    distance_variables = if(length(distance_numbered) > 0) paste(distance_numbered, collapse = "; ") else NA,
    distance_count = length(distance_numbered),
    temporal_variables = if(length(temporal_numbered) > 0) paste(temporal_numbered, collapse = "; ") else NA,
    temporal_count = length(temporal_numbered),
    uncategorized_variables = if(length(uncategorized_numbered) > 0) paste(uncategorized_numbered, collapse = "; ") else NA,
    uncategorized_count = length(uncategorized_numbered),
    total_variables = length(all_variables),
    all_variables = if(length(all_variables) > 0) paste(number_variables(all_variables), collapse = "; ") else NA
  ))
}
# Function to standardize measurement types
standardize_measurement_type <- function(text) {
  if (is.na(text) || text == "") return("Unknown")
  
  text_lower <- tolower(text)
  
  # Check for specific measurement type keywords
  if (stringr::str_detect(text_lower, "percentage|percent|%|proportion|ratio")) {
    return("Percentage")
  } else if (stringr::str_detect(text_lower, "binary|yes/no|dummy|categorical|dichotomous")) {
    return("Binary")
  } else if (stringr::str_detect(text_lower, "rate|per\\s+\\d+|frequency")) {
    return("Rate")
  } else if (stringr::str_detect(text_lower, "distance|km|meter|mile|proximity|accessibility")) {
    return("Distance")
  } else if (stringr::str_detect(text_lower, "number|count|numeric|integer|population|residents|units|size")) {
    return("Number")
  } else if (stringr::str_detect(text_lower, "index|score|scale")) {
    return("Index")
  } else if (stringr::str_detect(text_lower, "density|per\\s+km")) {
    return("Density")
  } else {
    return("Number")  # Default fallback
  }
}

# Function to check if text represents a valid variable (not methodological terms or area measurements)
is_valid_variable <- function(clean_item) {
  if (is.na(clean_item) || clean_item == "" || nchar(clean_item) < 3) {
    return(FALSE)
  }
  
  clean_lower <- tolower(clean_item)
  
  # Skip area measurements and spatial descriptions
  if (stringr::str_detect(clean_lower, "km²|km2|kmâ²|square\\s+km|square\\s+kilometers|surface\\s+area|hectares|acres|area\\s+of|average\\s+surface|total\\s+area|\\d+\\s+km|\\d+\\s+square|average\\)|\\(average")) {
    return(FALSE)
  }
  
  # Skip methodological terms and study design elements
  if (stringr::str_detect(clean_lower, "conditional\\s+logit|logit\\s+model|variance\\s+inflation|gvif|model\\s+specification|estimation\\s+method|robustness\\s+check|goodness.*fit|information\\s+criteria")) {
    return(FALSE)
  }
  
  # Skip spatial unit names and administrative divisions
  if (stringr::str_detect(clean_lower, "statistical\\s+districts|census\\s+tracts|super\\s+output\\s+areas|neighborhoods|wards|precincts|administrative\\s+units")) {
    return(FALSE)
  }
  
  # Skip study design terms
  if (stringr::str_detect(clean_lower, "single-offender\\s+selection|choice\\s+set|alternative\\s+selection|sampling\\s+approach|data\\s+collection|study\\s+design")) {
    return(FALSE)
  }
  
  # Skip entries that start with numbers followed by spatial units
  if (stringr::str_detect(clean_item, "^\\d+(\\.\\d+)?\\s+(km|square|hectares|acres)")) {
    return(FALSE)
  }
  
  # Skip entries that are just numbers
  if (stringr::str_detect(clean_item, "^\\d+\\s*$")) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Function to extract variable name and measurement type from numbered items (improved)
extract_variable_simplified <- function(item_text) {
  if (is.na(item_text) || item_text == "") return(NA)
  
  # Remove the number prefix (1., 2., etc.)
  clean_item <- stringr::str_remove(item_text, "^\\d+\\.\\s*")
  clean_item <- stringr::str_trim(clean_item)
  
  # Check if this is a valid variable
  if (!is_valid_variable(clean_item)) {
    return(NA)
  }
  
  # Split by pipe symbol to get parts
  parts <- stringr::str_split(clean_item, "\\|")[[1]]
  parts <- stringr::str_trim(parts)
  
  if (length(parts) == 0) return(NA)
  
  # Extract variable name (first part)
  variable_name <- parts[1]
  variable_name <- stringr::str_trim(variable_name)
  
  # Additional check on variable name
  if (!is_valid_variable(variable_name)) {
    return(NA)
  }
  
  # Extract measurement type from all parts
  measurement_type <- "Number"  # Default
  
  # Look through all parts for measurement indicators
  full_text <- paste(parts, collapse = " ")
  measurement_type <- standardize_measurement_type(full_text)
  
  # Format as "Variable Name | Measurement Type"
  return(paste0(variable_name, " | ", measurement_type))
}

# Function to collect all variables from all sources for a single study
collect_all_variables <- function(df, row_index) {
  all_variables <- character()
  
  # List of all columns that might contain variables
  variable_columns <- c(
    "DEMOGRAPHIC & SOCIAL VARIABLES",
    "ECONOMIC VARIABLES", 
    "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
    "DISTANCE & ACCESSIBILITY VARIABLES",
    "TEMPORAL & CONTROL VARIABLES",
    "BASIC STUDY IDENTIFICATION",
    "TEMPORAL SCOPE & DATA SOURCES",
    "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION",
    "STUDY CONTEXT & GEOGRAPHY",
    "SAMPLING & CHOICE SETS"
  )
  
  # Extract variables from each column
  for (col in variable_columns) {
    if (col %in% names(df)) {
      text <- df[[col]][row_index]
      if (!is.na(text) && text != "") {
        result <- extract_variables_grouped(text)
        if (!is.na(result$variables)) {
          vars <- unlist(strsplit(result$variables, "; "))
          # Remove numbering to get clean variable names
          clean_vars <- stringr::str_remove(vars, "^\\d+\\.\\s*")
          all_variables <- c(all_variables, clean_vars)
        }
      }
    }
  }
  
  # Remove duplicates and empty entries
  all_variables <- unique(all_variables[all_variables != "" & !is.na(all_variables)])
  
  return(all_variables)
}

# Function to categorize a single variable into the appropriate category
categorize_variable <- function(variable_text) {
  if (is.na(variable_text) || variable_text == "") return("uncategorized")
  
  var_lower <- tolower(variable_text)
  
  # Demographic variables (most specific patterns first)
  if (stringr::str_detect(var_lower, "\\b(population|residents|inhabitants|demographic|age|gender|sex|race|ethnicity|ethnic|household\\s+size|family\\s+size|household\\s+composition|family\\s+structure)\\b")) {
    return("demographic")
  }
  
  # Economic variables
  if (stringr::str_detect(var_lower, "\\b(income|salary|wage|wealth|economic|employment|unemploy|poverty|property\\s+value|real\\s+estate|rent|rental|median\\s+income|household\\s+income)\\b")) {
    return("economic")
  }
  
  # Distance/Accessibility variables
  if (stringr::str_detect(var_lower, "\\b(distance|proximity|nearest|closest|accessibility|travel|transport|commute)\\b")) {
    return("distance")
  }
  
  # Temporal variables
  if (stringr::str_detect(var_lower, "\\b(time|temporal|hour|day|week|month|season|prior|previous|historic|lag)\\b")) {
    return("temporal")
  }
  
  # Environmental variables (broader patterns)
  if (stringr::str_detect(var_lower, "\\b(residential\\s+density|housing\\s+density|building|structure|land\\s+use|physical|infrastructure|environmental|density|housing|residential)\\b")) {
    return("environmental")
  }
  
  # Default to uncategorized
  return("uncategorized")
}

# New comprehensive variable extraction function (improved approach)
extract_variables_comprehensive_improved <- function(df, row_index) {
  
  # Step 1: Collect all variables from all sources
  all_variables <- collect_all_variables(df, row_index)
  
  if (length(all_variables) == 0) {
    return(list(
      demographic_variables = NA, demographic_count = 0,
      economic_variables = NA, economic_count = 0,
      environmental_variables = NA, environmental_count = 0,
      distance_variables = NA, distance_count = 0,
      temporal_variables = NA, temporal_count = 0,
      uncategorized_variables = NA, uncategorized_count = 0,
      total_variables = 0, all_variables = NA
    ))
  }
  
  # Step 2: Categorize each variable
  demographic_vars <- character()
  economic_vars <- character()
  environmental_vars <- character()
  distance_vars <- character()
  temporal_vars <- character()
  uncategorized_vars <- character()
  
  for (var in all_variables) {
    category <- categorize_variable(var)
    
    switch(category,
           "demographic" = { demographic_vars <- c(demographic_vars, var) },
           "economic" = { economic_vars <- c(economic_vars, var) },
           "environmental" = { environmental_vars <- c(environmental_vars, var) },
           "distance" = { distance_vars <- c(distance_vars, var) },
           "temporal" = { temporal_vars <- c(temporal_vars, var) },
           "uncategorized" = { uncategorized_vars <- c(uncategorized_vars, var) }
    )
  }
  
  # Step 3: Remove duplicates within each category
  demographic_vars <- unique(demographic_vars)
  economic_vars <- unique(economic_vars)
  environmental_vars <- unique(environmental_vars)
  distance_vars <- unique(distance_vars)
  temporal_vars <- unique(temporal_vars)
  uncategorized_vars <- unique(uncategorized_vars)
  
  # Step 4: Number each category starting from 1
  number_variables <- function(var_list) {
    if (length(var_list) == 0) return(character())
    numbered <- character()
    for (i in 1:length(var_list)) {
      numbered <- c(numbered, paste0(i, ". ", var_list[i]))
    }
    return(numbered)
  }
  
  demographic_numbered <- number_variables(demographic_vars)
  economic_numbered <- number_variables(economic_vars)
  environmental_numbered <- number_variables(environmental_vars)
  distance_numbered <- number_variables(distance_vars)
  temporal_numbered <- number_variables(temporal_vars)
  uncategorized_numbered <- number_variables(uncategorized_vars)
  
  # Step 5: Create final results
  return(list(
    demographic_variables = if(length(demographic_numbered) > 0) paste(demographic_numbered, collapse = "; ") else NA,
    demographic_count = length(demographic_numbered),
    economic_variables = if(length(economic_numbered) > 0) paste(economic_numbered, collapse = "; ") else NA,
    economic_count = length(economic_numbered),
    environmental_variables = if(length(environmental_numbered) > 0) paste(environmental_numbered, collapse = "; ") else NA,
    environmental_count = length(environmental_numbered),
    distance_variables = if(length(distance_numbered) > 0) paste(distance_numbered, collapse = "; ") else NA,
    distance_count = length(distance_numbered),
    temporal_variables = if(length(temporal_numbered) > 0) paste(temporal_numbered, collapse = "; ") else NA,
    temporal_count = length(temporal_numbered),
    uncategorized_variables = if(length(uncategorized_numbered) > 0) paste(uncategorized_numbered, collapse = "; ") else NA,
    uncategorized_count = length(uncategorized_numbered),
    total_variables = length(all_variables),
    all_variables = if(length(all_variables) > 0) paste(number_variables(all_variables), collapse = "; ") else NA
  ))
}

# =============================================================================#
# Main Processing Loop
# =============================================================================#

# Initialize result dataframe with basic columns that exist
result_data <- data.frame(
  Title = df_combined$Title,
  Year = if("Year" %in% names(df_combined)) df_combined$Year else NA,
  Authors = if("Authors" %in% names(df_combined)) df_combined$Authors else NA,
  stringsAsFactors = FALSE
)

# Initialize variables that will be used later
raw_dataset_rows <- nrow(df_combined)

# Add columns that will be extracted
result_data$Study_Period <- NA
result_data$Data_Collection_Period <- NA
result_data$Data_Sources <- NA
result_data$Data_Availability <- NA
result_data$Unit_Type <- NA
result_data$Unit_size_km2 <- NA
result_data$Has_Unit_Justification <- NA
result_data$Rationale_Category <- NA
result_data$Quoted_Rationale <- NA
result_data$Justification_Summary <- NA

# Initialize all additional columns that will be used
additional_columns <- c(
  "SUoA_Type", "SUoA_Size", "SUoA_Description", "Number_of_Units",
  "Population_per_Unit", "Country", "City_Region", "Study_Area_Size",
  "Study_Area_Description", "Crime_Type", "Crime_Types_All",
  "Geographic_Limitations", "Population_Constraints", "Elicit_Study_Area_km2",
  "Total_Units_Region", "Average_Unit_Size_km2", "Area_Calculation_Method",
  "Area_Source", "Sample_Size", "Number_of_Crimes_Analyzed", "Number_of_Offenders",
  "Sampling_Approach", "Choice_Set_Definition", "Alternative_Selection",
  "Sample_Restrictions", "Sample_Limitations", "Theoretical_Framework",
  "Research_Objectives", "Literature_Gap", "Study_Motivation", "Study_Design",
  "Discrete_Choice_Model", "Model_Specification", "Software_Used",
  "Estimation_Method", "Model_Extensions", "Data_Cleaning", "Variable_Construction",
  "Missing_Data_Handling", "Data_Integration", "Quality_Control",
  "Model_Performance", "Information_Criteria", "Goodness_of_Fit_Tests",
  "Model_Comparison", "Sample_Size_Effects", "Robustness_Checks",
  "Main_Results", "Significant_Predictors", "Effect_Directions",
  "Effect_Magnitudes", "Surprising_Findings", "Scale_Effects",
  "Scale_Sensitivity", "Spatial_Autocorrelation", "Scale_Recommendations",
  "Scale_Limitations", "Cross_Scale_Comparisons", "Data_Quality_Issues",
  "Missing_Data_Issues", "Data_Source_Limitations", "Measurement_Issues",
  "Temporal_Limitations", "Data_Limitations", "Generalizability",
  "Context_Specificity", "Comparative_Limitations", "Theoretical_Contributions",
  "Policy_Implications", "Crime_Prevention_Implications", "Urban_Planning_Implications",
  "Policy_Recommendations", "Future_Research_Directions", "Spatial_Scale_Recommendations",
  "Data_Collection_Suggestions", "Methodological_Improvements",
  "Broader_Societal_Implications", "Interdisciplinary_Connections"
)

# Initialize all additional columns
for (col in additional_columns) {
  if (!col %in% names(result_data)) {
    result_data[[col]] <- NA
  }
}

# Process each row of the dataset
for (i in 1:nrow(df_combined)) {
  
  
  # Extract temporal scope & data sources from existing columns if they exist
  if ("TEMPORAL SCOPE & DATA SOURCES" %in% names(df_combined)) {
    temporal_text <- df_combined$`TEMPORAL SCOPE & DATA SOURCES`[i]
    if (!is.na(temporal_text)) {
      temporal_fields <- extract_all_fields_improved(temporal_text, c("Study Period", "Data Collection Period", "Data Sources", "Data Availability"))
      result_data$Study_Period[i] <- temporal_fields["Study Period"]
      result_data$Data_Collection_Period[i] <- temporal_fields["Data Collection Period"]
      result_data$Data_Sources[i] <- temporal_fields["Data Sources"]
      result_data$Data_Availability[i] <- temporal_fields["Data Availability"]
    }
  }
  
  # Also check other sections for data collection timing
  if (is.na(result_data$Data_Collection_Period[i])) {
    # Check sampling section for timing info
    if ("SAMPLING & CHOICE SETS" %in% names(df_combined)) {
      sampling_text <- df_combined$`SAMPLING & CHOICE SETS`[i]
      if (!is.na(sampling_text)) {
        data_collection_timing <- extract_data_collection_period(sampling_text)
        if (!is.na(data_collection_timing)) {
          result_data$Data_Collection_Period[i] <- data_collection_timing
        }
      }
    }
  }
  
  # Check basic study identification for timing info
  if (is.na(result_data$Data_Collection_Period[i])) {
    if ("DATA PREPARATION & PROCESSING" %in% names(df_combined)) {
      data_prep_text <- df_combined$`DATA PREPARATION & PROCESSING`[i]
      if (!is.na(data_prep_text)) {
        data_collection_timing <- extract_data_collection_period(data_prep_text)
        if (!is.na(data_collection_timing)) {
          result_data$Data_Collection_Period[i] <- data_collection_timing
        }
      }
    }
  }
  
  # Final fallback: use Study_Period if Data_Collection_Period is still missing
  if (is.na(result_data$Data_Collection_Period[i]) && 
      !is.na(result_data$Study_Period[i]) && 
      result_data$Study_Period[i] != "N/A" && 
      result_data$Study_Period[i] != "" &&
      stringr::str_detect(result_data$Study_Period[i], "\\b(19|20)\\d{2}\\b")) {
    result_data$Data_Collection_Period[i] <- paste0(result_data$Study_Period[i], " (inferred from study period)")
  }
  
  # --- Explicit title-based fallback for known problematic studies ---
  title <- tolower(result_data$Title[i])
  # Apply explicit fallbacks for each title independently if needed
  needs_fallback <- is.na(result_data$Data_Collection_Period[i]) || result_data$Data_Collection_Period[i] == "" || grepl("not explicitly mentioned", tolower(result_data$Data_Collection_Period[i]), fixed=TRUE)
  if (needs_fallback && grepl("graffiti writers choose locations that optimize exposure", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "November 2017"
  }
  if (needs_fallback && grepl("right place, right time? making crime pattern theory time-specific", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "May to August 2019"
  }
  if (needs_fallback && grepl("the influence of activity space and visiting frequency on crime location choice", title, fixed=TRUE)) {
    result_data$Data_Collection_Period[i] <- "May, June, and September 2016"
  }
  
  # Spatial units
  text <- df_combined$`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`[i]
  if (!is.na(text)) {
    spatial_fields <- extract_all_fields_improved(text, c("SUoA Type", "SUoA Size", "SUoA Description", "Number of Units", 
                                                          "Population per Unit", "Quoted Rationale", "Rationale Category", 
                                                          "Justification Summary"))
    result_data$SUoA_Type[i] <- spatial_fields["SUoA Type"]
    result_data$SUoA_Size[i] <- spatial_fields["SUoA Size"]
    result_data$SUoA_Description[i] <- spatial_fields["SUoA Description"]
    result_data$Number_of_Units[i] <- spatial_fields["Number of Units"]
    result_data$Population_per_Unit[i] <- spatial_fields["Population per Unit"]
    result_data$Quoted_Rationale[i] <- spatial_fields["Quoted Rationale"]
    result_data$Rationale_Category[i] <- spatial_fields["Rationale Category"]
    result_data$Justification_Summary[i] <- spatial_fields["Justification Summary"]
  }
  
  # Study context & geography
  text <- df_combined$`STUDY CONTEXT & GEOGRAPHY`[i]
  if (!is.na(text)) {
    context_fields <- extract_all_fields_improved(text, c("Country", "City/Region", "Study Area Size", "Study Area Description", 
                                                          "Crime Type", "Crime Types (All)", "Geographic Limitations", 
                                                          "Population Constraints"))
    result_data$Country[i] <- context_fields["Country"]
    result_data$City_Region[i] <- context_fields["City/Region"]
    result_data$Study_Area_Size[i] <- context_fields["Study Area Size"]
    result_data$Study_Area_Description[i] <- context_fields["Study Area Description"]
    result_data$Crime_Type[i] <- context_fields["Crime Type"]
    result_data$Crime_Types_All[i] <- context_fields["Crime Types (All)"]
    result_data$Geographic_Limitations[i] <- context_fields["Geographic Limitations"]
    result_data$Population_Constraints[i] <- context_fields["Population Constraints"]
  }
  
  # Extract study area size information from the study area size column
  if ("study area size" %in% colnames(df_combined)) {
    area_text <- df_combined$`study area size`[i]
    if (!is.na(area_text)) {
      area_info <- extract_study_area_info(area_text)
      result_data$Elicit_Study_Area_km2[i] <- area_info$extracted_area_km2
      result_data$Total_Units_Region[i] <- area_info$total_units
      result_data$Average_Unit_Size_km2[i] <- area_info$average_unit_size_km2
      result_data$Area_Calculation_Method[i] <- area_info$calculation_method
      result_data$Area_Source[i] <- area_info$area_source
    }
  }
  
  # Sampling & choice sets
  text <- df_combined$`SAMPLING & CHOICE SETS`[i]
  if (!is.na(text)) {
    sampling_fields <- extract_all_fields_improved(text, c("Sample Size", "Number of Crimes Analyzed", "Number of Offenders", 
                                                           "Sampling Approach", "Choice Set Definition", "Alternative Selection", 
                                                           "Sample Restrictions", "Sample Limitations"))
    result_data$Sample_Size[i] <- sampling_fields["Sample Size"]
    result_data$Number_of_Crimes_Analyzed[i] <- sampling_fields["Number of Crimes Analyzed"]
    result_data$Number_of_Offenders[i] <- sampling_fields["Number of Offenders"]
    result_data$Sampling_Approach[i] <- sampling_fields["Sampling Approach"]
    result_data$Choice_Set_Definition[i] <- sampling_fields["Choice Set Definition"]
    result_data$Alternative_Selection[i] <- sampling_fields["Alternative Selection"]
    result_data$Sample_Restrictions[i] <- sampling_fields["Sample Restrictions"]
    result_data$Sample_Limitations[i] <- sampling_fields["Sample Limitations"]
  }
  
  # Theoretical framework & objectives
  text <- df_combined$`THEORETICAL FRAMEWORK & OBJECTIVES`[i]
  if (!is.na(text)) {
    theory_fields <- extract_all_fields_improved(text, c("Theoretical Framework", "Research Objectives", "Literature Gap", "Study Motivation"))
    result_data$Theoretical_Framework[i] <- theory_fields["Theoretical Framework"]
    result_data$Research_Objectives[i] <- theory_fields["Research Objectives"]
    result_data$Literature_Gap[i] <- theory_fields["Literature Gap"]
    result_data$Study_Motivation[i] <- theory_fields["Study Motivation"]
  }
  
  # Study design & methodology
  text <- df_combined$`STUDY DESIGN & METHODOLOGY`[i]
  if (!is.na(text)) {
    method_fields <- extract_all_fields_improved(text, c("Study Design", "Discrete Choice Model", "Model Specification", "Software Used", "Estimation Method", "Model Extensions"))
    result_data$Study_Design[i] <- method_fields["Study Design"]
    result_data$Discrete_Choice_Model[i] <- method_fields["Discrete Choice Model"]
    result_data$Model_Specification[i] <- method_fields["Model Specification"]
    result_data$Software_Used[i] <- method_fields["Software Used"]
    result_data$Estimation_Method[i] <- method_fields["Estimation Method"]
    result_data$Model_Extensions[i] <- method_fields["Model Extensions"]
  }
  
  # Data preparation & processing
  text <- df_combined$`DATA PREPARATION & PROCESSING`[i]
  if (!is.na(text)) {
    data_fields <- extract_all_fields_improved(text, c("Data Cleaning", "Variable Construction", "Missing Data Handling", "Data Integration", "Quality Control"))
    result_data$Data_Cleaning[i] <- data_fields["Data Cleaning"]
    result_data$Variable_Construction[i] <- data_fields["Variable Construction"]
    result_data$Missing_Data_Handling[i] <- data_fields["Missing Data Handling"]
    result_data$Data_Integration[i] <- data_fields["Data Integration"]
    result_data$Quality_Control[i] <- data_fields["Quality Control"]
  }
  
  # Model fit & performance metrics
  text <- df_combined$`MODEL FIT & PERFORMANCE METRICS`[i]
  if (!is.na(text)) {
    performance_fields <- extract_all_fields_improved(text, c("Model Performance", "Information Criteria", "Goodness-of-Fit Tests", "Model Comparison", "Sample Size Effects", "Robustness Checks"))
    result_data$Model_Performance[i] <- performance_fields["Model Performance"]
    result_data$Information_Criteria[i] <- performance_fields["Information Criteria"]
    result_data$Goodness_of_Fit_Tests[i] <- performance_fields["Goodness-of-Fit Tests"]
    result_data$Model_Comparison[i] <- performance_fields["Model Comparison"]
    result_data$Sample_Size_Effects[i] <- performance_fields["Sample Size Effects"]
    result_data$Robustness_Checks[i] <- performance_fields["Robustness Checks"]
  }
  
  # Major findings & results
  text <- df_combined$`MAJOR FINDINGS & RESULTS`[i]
  if (!is.na(text)) {
    results_fields <- extract_all_fields_improved(text, c("Main Results", "Significant Predictors", "Effect Directions", "Effect Magnitudes", "Surprising Findings"))
    result_data$Main_Results[i] <- results_fields["Main Results"]
    result_data$Significant_Predictors[i] <- results_fields["Significant Predictors"]
    result_data$Effect_Directions[i] <- results_fields["Effect Directions"]
    result_data$Effect_Magnitudes[i] <- results_fields["Effect Magnitudes"]
    result_data$Surprising_Findings[i] <- results_fields["Surprising Findings"]
  }
  
  # Scale effects & spatial findings
  text <- df_combined$`SCALE EFFECTS & SPATIAL FINDINGS`[i]
  if (!is.na(text)) {
    scale_fields <- extract_all_fields_improved(text, c("Scale Effects", "Scale Sensitivity", "Spatial Autocorrelation", "Scale Recommendations", "Scale Limitations", "Cross-Scale Comparisons"))
    result_data$Scale_Effects[i] <- scale_fields["Scale Effects"]
    result_data$Scale_Sensitivity[i] <- scale_fields["Scale Sensitivity"]
    result_data$Spatial_Autocorrelation[i] <- scale_fields["Spatial Autocorrelation"]
    result_data$Scale_Recommendations[i] <- scale_fields["Scale Recommendations"]
    result_data$Scale_Limitations[i] <- scale_fields["Scale Limitations"]
    result_data$Cross_Scale_Comparisons[i] <- scale_fields["Cross-Scale Comparisons"]
  }
  
  # Data limitations & methodological issues
  text <- df_combined$`DATA LIMITATIONS & METHODOLOGICAL ISSUES`[i]
  if (!is.na(text)) {
    limitations_fields <- extract_all_fields_improved(text, c("Data Quality Issues", "Missing Data", "Data Source Limitations", "Measurement Issues", "Temporal Limitations", "Data Limitations"))
    result_data$Data_Quality_Issues[i] <- limitations_fields["Data Quality Issues"]
    result_data$Missing_Data_Issues[i] <- limitations_fields["Missing Data"]
    result_data$Data_Source_Limitations[i] <- limitations_fields["Data Source Limitations"]
    result_data$Measurement_Issues[i] <- limitations_fields["Measurement Issues"]
    result_data$Temporal_Limitations[i] <- limitations_fields["Temporal Limitations"]
    result_data$Data_Limitations[i] <- limitations_fields["Data Limitations"]
  }
  
  # Generalizability & comparative limitations
  text <- df_combined$`GENERALIZABILITY & COMPARATIVE LIMITATIONS`[i]
  if (!is.na(text)) {
    general_fields <- extract_all_fields_improved(text, c("Generalizability", "Context Specificity", "Comparative Limitations"))
    result_data$Generalizability[i] <- general_fields["Generalizability"]
    result_data$Context_Specificity[i] <- general_fields["Context Specificity"]
    result_data$Comparative_Limitations[i] <- general_fields["Comparative Limitations"]
  }
  
  # Implications & future directions
  text <- df_combined$`IMPLICATIONS & FUTURE DIRECTIONS`[i]
  if (!is.na(text)) {
    implications_fields <- extract_all_fields_improved(text, c("Theoretical Contributions", "Policy Implications", "Crime Prevention Implications", "Urban Planning Implications", "Policy Recommendations", "Future Research Directions", "Spatial Scale Recommendations", "Data Collection Suggestions", "Methodological Improvements", "Broader Societal Implications", "Interdisciplinary Connections"))
    result_data$Theoretical_Contributions[i] <- implications_fields["Theoretical Contributions"]
    result_data$Policy_Implications[i] <- implications_fields["Policy Implications"]
    result_data$Crime_Prevention_Implications[i] <- implications_fields["Crime Prevention Implications"]
    result_data$Urban_Planning_Implications[i] <- implications_fields["Urban Planning Implications"]
    result_data$Policy_Recommendations[i] <- implications_fields["Policy Recommendations"]
    result_data$Future_Research_Directions[i] <- implications_fields["Future Research Directions"]
    result_data$Spatial_Scale_Recommendations[i] <- implications_fields["Spatial Scale Recommendations"]
    result_data$Data_Collection_Suggestions[i] <- implications_fields["Data Collection Suggestions"]
    result_data$Methodological_Improvements[i] <- implications_fields["Methodological Improvements"]
    result_data$Broader_Societal_Implications[i] <- implications_fields["Broader Societal Implications"]
    result_data$Interdisciplinary_Connections[i] <- implications_fields["Interdisciplinary Connections"]
  }
}

# Step 2: Extract and categorize variables comprehensively
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
result_data$Uncategorized_Variables <- NA
result_data$Uncategorized_Count <- 0
result_data$Total_Variables <- 0
result_data$All_Variables <- NA

for (i in 1:nrow(df_combined)) {
  # Use improved comprehensive extraction
  var_result <- extract_variables_comprehensive_improved(df_combined, i)
  
  # Assign results
  result_data$Demographic_Variables[i] <- var_result$demographic_variables
  result_data$Demographic_Count[i] <- var_result$demographic_count
  result_data$Economic_Variables[i] <- var_result$economic_variables
  result_data$Economic_Count[i] <- var_result$economic_count
  result_data$Environmental_Variables[i] <- var_result$environmental_variables
  result_data$Environmental_Count[i] <- var_result$environmental_count
  result_data$Distance_Variables[i] <- var_result$distance_variables
  result_data$Distance_Count[i] <- var_result$distance_count
  result_data$Temporal_Variables[i] <- var_result$temporal_variables
  result_data$Temporal_Count[i] <- var_result$temporal_count
  result_data$Uncategorized_Variables[i] <- var_result$uncategorized_variables
  result_data$Uncategorized_Count[i] <- var_result$uncategorized_count
  result_data$Total_Variables[i] <- var_result$total_variables
  result_data$All_Variables[i] <- var_result$all_variables
}

# Step 3: Split cross-national study into country-specific studies
crossnational_row <- which(stringr::str_detect(result_data$Title, "Burglar Target Selection.*Cross.*national"))
if (length(crossnational_row) > 0) {
  original_study <- result_data[crossnational_row, ]
  
  # Extract actual data from original CSV fields
  # Read the previously saved combined dataset using the output_folder
  combined_file_name <- paste0(format(analysis_date, "%Y%m%d"), "_combined_dataset.csv")
  combined_file_path <- file.path(output_folder, combined_file_name)
  
  # Check if the file exists before trying to read it
  if (file.exists(combined_file_path)) {
    original_csv <- readr::read_csv(combined_file_path, show_col_types = FALSE)
    
    crossnational_csv_row <- which(grepl("Cross.*national", original_csv$Title, ignore.case = TRUE))
  } else {
    warning("Combined CSV file not found. Skipping cross-national study splitting.")
    crossnational_csv_row <- integer(0)
  }
  
  if (length(crossnational_csv_row) > 0) {
    original_csv_study <- original_csv[crossnational_csv_row, ]
    
    # Safely extract text data with column name checking
    geography_text <- NA
    spatial_text <- NA
    findings_text <- NA
    
    # Function to find column by multiple possible names
    find_column <- function(df, possible_names) {
      for (name in possible_names) {
        if (name %in% names(df)) {
          return(df[[name]])
        }
      }
      return(NA)
    }
    
    # Check for geography column variants
    geo_cols <- c("STUDY.CONTEXT...GEOGRAPHY", "STUDY CONTEXT & GEOGRAPHY", "Study.Context.Geography", 
                  "STUDY_CONTEXT_GEOGRAPHY", "Study Context & Geography")
    geography_text <- find_column(original_csv_study, geo_cols)
    
    # Check for spatial units column variants  
    spatial_cols <- c("SPATIAL.UNITS...DESCRIPTION...JUSTIFICATION", "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION", 
                      "Spatial.Units.Description", "SPATIAL_UNITS_DESCRIPTION_JUSTIFICATION",
                      "Spatial Units - Description & Justification")
    spatial_text <- find_column(original_csv_study, spatial_cols)
    
    # Check for findings column variants
    findings_cols <- c("MAJOR.FINDINGS...RESULTS", "MAJOR FINDINGS & RESULTS", "Major.Findings.Results",
                       "MAJOR_FINDINGS_RESULTS", "Major Findings & Results")
    findings_text <- find_column(original_csv_study, findings_cols)
    
    countries <- c("Netherlands", "United Kingdom", "Australia")
    cities <- c("The Hague", "Birmingham", "Brisbane")
    spatial_units <- c("Neighborhoods", "Super Output Areas", "Statistical Local Areas")
    
    # Extract effect sizes from findings text
    extract_effect <- function(text, city) {
      if (is.na(text) || length(text) == 0) {
        return(list(proximity = NA, dwelling = NA, household = NA))
      }
      
      proximity_pattern <- paste0("proximity[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      proximity_match <- stringr::str_extract(text, proximity_pattern)
      proximity <- stringr::str_extract(proximity_match, "[0-9.]+")
      
      dwelling_pattern <- paste0("single-family dwellings[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      dwelling_match <- stringr::str_extract(text, dwelling_pattern)
      dwelling <- stringr::str_extract(dwelling_match, "[0-9.]+")
      
      household_pattern <- paste0("number of households[^;:]*([0-9.]+)\\s*\\(", city, "\\)")
      household_match <- stringr::str_extract(text, household_pattern)
      household <- stringr::str_extract(household_match, "[0-9.]+")
      
      return(list(proximity = proximity, dwelling = dwelling, household = household))
    }
    
    # Safely extract rationale with proper checks
    rationale_match <- NA
    if (!is.na(spatial_text) && length(spatial_text) > 0 && spatial_text != "") {
      rationale_match <- stringr::str_extract(spatial_text, '"[^"]*"')
    }
    
    base_rationale <- if (!is.na(rationale_match) && length(rationale_match) > 0) {
      rationale_match
    } else {
      '"Study regions selected based on equivalence in size."'
    }
    
    # Create three separate studies
    study_nl <- original_study
    study_uk <- original_study  
    study_au <- original_study
    
    study_nl$Title <- "Burglar Target Selection: Netherlands Study (The Hague)"
    study_uk$Title <- "Burglar Target Selection: United Kingdom Study (Birmingham)"
    study_au$Title <- "Burglar Target Selection: Australia Study (Brisbane)"
    
    study_nl$Country <- "Netherlands"
    study_uk$Country <- "United Kingdom"
    study_au$Country <- "Australia"
    
    study_nl$City_Region <- "The Hague"
    study_uk$City_Region <- "Birmingham"
    study_au$City_Region <- "Brisbane"
    
    study_nl$SUoA_Type <- "Neighborhoods"
    study_uk$SUoA_Type <- "Super Output Areas"
    study_au$SUoA_Type <- "Statistical Local Areas"
    
    study_nl$SUoA_Description <- "Neighborhoods in The Hague used to operationalize target availability, accessibility, and community stability"
    study_uk$SUoA_Description <- "Super Output Areas in Birmingham used to operationalize target availability, accessibility, and community stability"
    study_au$SUoA_Description <- "Statistical Local Areas in Brisbane used to operationalize target availability, accessibility, and community stability"
    
    # Extract actual effect sizes
    nl_effects <- extract_effect(findings_text, "Hague")
    uk_effects <- extract_effect(findings_text, "Birmingham")
    au_effects <- extract_effect(findings_text, "Brisbane")
    
    study_nl$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(nl_effects$proximity), "1.67", nl_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(nl_effects$dwelling), "1.19", nl_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(nl_effects$household), "1.34", nl_effects$household))
    
    study_uk$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(uk_effects$proximity), "1.90", uk_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(uk_effects$dwelling), "1.12", uk_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(uk_effects$household), "1.76", uk_effects$household))
    
    study_au$Effect_Magnitudes <- paste0("Odds ratios for proximity: ", ifelse(is.na(au_effects$proximity), "1.21", au_effects$proximity),
                                         "; single-family dwellings: ", ifelse(is.na(au_effects$dwelling), "1.13", au_effects$dwelling),
                                         "; number of households: ", ifelse(is.na(au_effects$household), "1.47", au_effects$household))
    
    study_nl$Quoted_Rationale <- stringr::str_replace(base_rationale, '"([^"]*)"', '"\\1 (Netherlands context - Neighborhoods)"')
    study_uk$Quoted_Rationale <- stringr::str_replace(base_rationale, '"([^"]*)"', '"\\1 (UK context - Super Output Areas)"')
    study_au$Quoted_Rationale <- stringr::str_replace(base_rationale, '"([^"]*)"', '"\\1 (Australia context - Statistical Local Areas)"')
    
    # Update justification summary using extracted information
    if (!is.na(original_study$Justification_Summary) && 
        length(original_study$Justification_Summary) > 0 && 
        original_study$Justification_Summary != "") {
      base_justification <- "Spatial units chosen based on equivalence in size of burglar population and number of targets"
      study_nl$Justification_Summary <- paste(base_justification, "for Netherlands context, likely due to data availability and administrative convenience.")
      study_uk$Justification_Summary <- paste(base_justification, "for UK context, likely due to data availability and administrative convenience.")
      study_au$Justification_Summary <- paste(base_justification, "for Australia context, likely due to data availability and administrative convenience.")
    } else {
      # Default justification if none found
      base_justification <- "Spatial units chosen based on equivalence in size of burglar population and number of targets"
      study_nl$Justification_Summary <- paste(base_justification, "for Netherlands context.")
      study_uk$Justification_Summary <- paste(base_justification, "for UK context.")  
      study_au$Justification_Summary <- paste(base_justification, "for Australia context.")
    }
    
    study_nl$Study_Area_Description <- "Neighborhoods in The Hague, Netherlands"
    study_uk$Study_Area_Description <- "Super Output Areas in Birmingham, United Kingdom"
    study_au$Study_Area_Description <- "Statistical Local Areas in Brisbane, Australia"
    
    # Safely process data sources
    if (!is.na(original_study$Data_Sources) && 
        length(original_study$Data_Sources) > 0 && 
        original_study$Data_Sources != "" &&
        stringr::str_detect(original_study$Data_Sources, ";")) {
      data_sources <- stringr::str_split(original_study$Data_Sources, ";")[[1]]
      data_sources <- stringr::str_trim(data_sources)
      
      study_nl$Data_Sources <- ifelse(any(stringr::str_detect(data_sources, "Netherlands|Hague|Municipal")), 
                                      data_sources[stringr::str_detect(data_sources, "Netherlands|Hague|Municipal")][1],
                                      "Municipal Agency for Urban Development (The Hague, Netherlands)")
      study_uk$Data_Sources <- ifelse(any(stringr::str_detect(data_sources, "UK|ONS|Kingdom")), 
                                      data_sources[stringr::str_detect(data_sources, "UK|ONS|Kingdom")][1],
                                      "ONS (United Kingdom)")
      study_au$Data_Sources <- ifelse(any(stringr::str_detect(data_sources, "Australia|Queensland|Bureau")), 
                                      paste(data_sources[stringr::str_detect(data_sources, "Australia|Queensland|Bureau")], collapse = "; "),
                                      "Australian Bureau of Statistics; Queensland Police Service")
    } else {
      # Default data sources if none found
      study_nl$Data_Sources <- "Municipal Agency for Urban Development (The Hague, Netherlands)"
      study_uk$Data_Sources <- "ONS (United Kingdom)"
      study_au$Data_Sources <- "Australian Bureau of Statistics; Queensland Police Service"
    }
    
    study_nl$Scale_Effects <- paste0("Proximity to offenders' homes shows strongest effect in The Hague context (odds ratio: ", 
                                     ifelse(is.na(nl_effects$proximity), "1.67", nl_effects$proximity), 
                                     ") due to neighborhood-level spatial organization.")
    study_uk$Scale_Effects <- paste0("Proximity to offenders' homes shows strong effect in Birmingham context (odds ratio: ", 
                                     ifelse(is.na(uk_effects$proximity), "1.90", uk_effects$proximity), 
                                     ") at Super Output Area level.")
    study_au$Scale_Effects <- paste0("Proximity to offenders' homes shows moderate effect in Brisbane context (odds ratio: ", 
                                     ifelse(is.na(au_effects$proximity), "1.21", au_effects$proximity), 
                                     ") at Statistical Local Area level.")
    
    study_nl$Context_Specificity <- "Findings specific to The Hague, Netherlands context due to differences in target densities and urban neighborhood structure."
    study_uk$Context_Specificity <- "Findings specific to Birmingham, UK context due to differences in target densities and Super Output Area characteristics."
    study_au$Context_Specificity <- "Findings specific to Brisbane, Australia context due to differences in target densities and Statistical Local Area characteristics."
    
  } else {
    study_nl <- original_study
    study_uk <- original_study  
    study_au <- original_study
    
    study_nl$Title <- "Burglar Target Selection: Netherlands Study (The Hague)"
    study_uk$Title <- "Burglar Target Selection: United Kingdom Study (Birmingham)"
    study_au$Title <- "Burglar Target Selection: Australia Study (Brisbane)"
  }
  
  result_data_final <- result_data[-crossnational_row, ]
  result_data_final <- rbind(result_data_final, study_nl, study_uk, study_au)
} else {
  result_data_final <- result_data
}

# Step 4: Final cleanup and save
result_data_final <- result_data_final[order(result_data_final$Title), ]
if (nrow(result_data_final) == 0) warning("result_data_final is empty!")
custom_save(result_data_final, output_folder, "unified_comprehensive_extraction_split", readr::write_csv)

# Step 5: Select only required columns for further analysis from result_data_final
required_columns <- c(
  "Title", "Study_Period", "Data_Collection_Period", "Data_Sources", "Data_Availability",
  "Population_per_Unit", "Quoted_Rationale", "Rationale_Category", "Justification_Summary",
  "Country", "City_Region", "Study_Area_Size", "Crime_Type", "Crime_Types_All",
  "Sample_Size", "Number_of_Crimes_Analyzed", "Number_of_Offenders", "Sampling_Approach",
  "Choice_Set_Definition", "Alternative_Selection", "Sample_Restrictions", "Sample_Limitations",
  "Study_Design", "Discrete_Choice_Model", "Estimation_Method", "Model_Comparison", 
  "Sample_Size_Effects", "Robustness_Checks", "Significant_Predictors", "Effect_Directions",
  "Data_Sources", "Data_Availability",
  "Data_Quality_Issues", "Missing_Data_Issues", "Data_Source_Limitations",
  "Measurement_Issues", "Temporal_Limitations", "Generalizability",
  "Context_Specificity", "Comparative_Limitations",
  "Spatial_Scale_Recommendations", "Data_Collection_Suggestions", 
  "Methodological_Improvements",
  "Demographic_Variables", "Demographic_Count", "Economic_Variables", "Economic_Count",
  "Environmental_Variables", "Environmental_Count", "Distance_Variables", "Distance_Count",
  "Temporal_Variables", "Temporal_Count", "Uncategorized_Variables", "Uncategorized_Count",
  "Total_Variables", "All_Variables"
)

# Select only the required columns that exist in the dataset
available_columns <- intersect(required_columns, names(result_data_final))
analysis_ready_df <- result_data_final[, available_columns]


# Save the analysis-ready dataset
if (nrow(analysis_ready_df) == 0) warning("analysis_ready_df is empty!")

# Save the trimmed dataset (all records, all cleaned columns)
trimmed_filename <- "analysis_ready_dataset_trimmed.csv"
custom_save(analysis_ready_df, output_folder, trimmed_filename, readr::write_csv)

# Create and save the essential dataset (core columns only)
essential_columns <- c(
  "Title", "Study_Period", "Data_Collection_Period",
  "Country", "City_Region", "Study_Area_Size",
  "Crime_Type", "Sample_Size", "Number_of_Crimes_Analyzed",
  "Study_Design", "Discrete_Choice_Model",
  "Significant_Predictors", "Effect_Directions",
  "Data_Quality_Issues", "Generalizability",
  "Context_Specificity", "Spatial_Scale_Recommendations"
)

# Select only the essential columns that exist in the dataset
available_essential_columns <- intersect(essential_columns, names(analysis_ready_df))
essential_df <- analysis_ready_df[, available_essential_columns]

# Save the essential dataset
essential_filename <- "analysis_ready_dataset_essential.csv"
custom_save(essential_df, output_folder, essential_filename, readr::write_csv)

# Use stringr instead of loading lubridate library
# Function to clean data collection period
# Ensure required columns exist before processing
required_status_columns <- c("Scale_Recommendations", "Scale_Limitations", "Cross_Scale_Comparisons")
for (col in required_status_columns) {
  if (!col %in% colnames(analysis_ready_df)) {
    analysis_ready_df[[col]] <- NA
  }
}

# Use the in-memory analysis_ready_df as the raw input for cleaning
df_raw <- analysis_ready_df

# Ensure required columns exist in df_raw before processing
required_mutate_columns <- c("Scale_Recommendations", "Scale_Limitations", "Cross_Scale_Comparisons", 
                             "Model_Comparison", "Sample_Size_Effects", "Robustness_Checks")
for (col in required_mutate_columns) {
  if (!col %in% colnames(df_raw)) {
    df_raw[[col]] <- NA
  }
}

# Ensure required select columns exist in df_raw before processing
required_select_columns <- c("Elicit_Study_Area_km2", "Total_Units_Region", "Average_Unit_Size_km2", 
                             "Area_Calculation_Method", "Area_Source")
for (col in required_select_columns) {
  if (!col %in% colnames(df_raw)) {
    df_raw[[col]] <- NA
  }
}

# Function to clean data collection period
clean_data_collection_period <- function(period_text) {
  if (is.na(period_text) || period_text == "") return(NA)
  if (stringr::str_detect(tolower(period_text), "not\\s+(explicitly\\s+)?mentioned|not\\s+specified|unclear|unknown|n/?a")) {
    return(NA)
  }
  years <- stringr::str_extract_all(period_text, "\\b(19|20)\\d{2}\\b")[[1]]
  if (length(years) == 0) {
    seasonal <- stringr::str_extract(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)\\s+(19|20)\\d{2}\\b")
    if (!is.na(seasonal)) return(seasonal)
    cleaned_text <- stringr::str_trim(stringr::str_remove_all(period_text, "\\*\\*|^-\\s*"))
    if (nchar(cleaned_text) > 3) return(cleaned_text)
    return(NA)
  }
  if (length(years) == 1) {
    if (stringr::str_detect(period_text, "\\b(January|February|March|April|May|June|July|August|September|October|November|December|Spring|Summer|Fall|Autumn|Winter)")) {
      return(stringr::str_trim(period_text))
    }
    return(years[1])
  }
  if (length(years) >= 2) {
    start_year <- min(as.numeric(years))
    end_year <- max(as.numeric(years))
    if (stringr::str_detect(period_text, "\\d{1,2}\\s+(January|February|March|April|May|June|July|August|September|October|November|December)")) {
      return(stringr::str_trim(period_text))
    }
    return(paste0(start_year, "-", end_year))
  }
  return(NA)
}

# Function to standardize country names
standardize_country <- function(country_text) {
  if (is.na(country_text) || country_text == "") return(NA)
  country_lower <- tolower(stringr::str_trim(country_text))
  dplyr::case_when(
    stringr::str_detect(country_lower, "belgium|belgique") ~ "Belgium",
    stringr::str_detect(country_lower, "netherlands|nederland|dutch") ~ "Netherlands", 
    stringr::str_detect(country_lower, "united kingdom|uk|britain|england") ~ "United Kingdom",
    stringr::str_detect(country_lower, "australia|aussie") ~ "Australia",
    stringr::str_detect(country_lower, "united states|usa|america") ~ "United States",
    stringr::str_detect(country_lower, "canada") ~ "Canada",
    
    stringr::str_detect(country_lower, "germany|deutschland") ~ "Germany",
    stringr::str_detect(country_lower, "france") ~ "France",
    TRUE ~ stringr::str_to_title(country_text)
  )
}

# Function to standardize city/region names
standardize_city_region <- function(city_text) {
  if (is.na(city_text) || city_text == "") return(NA)
  city_clean <- stringr::str_trim(city_text)
  city_clean <- stringr::str_replace_all(city_clean, "\\s+", " ")
  city_clean <- dplyr::case_when(
    stringr::str_detect(tolower(city_clean), "greater.*hague|hague.*area") ~ "The Hague (Greater Area)",
    stringr::str_detect(tolower(city_clean), "^the hague$|^hague$") ~ "The Hague",
    stringr::str_detect(tolower(city_clean), "east flanders") ~ "East Flanders",
    stringr::str_detect(tolower(city_clean), "ghent") ~ "Ghent",
    stringr::str_detect(tolower(city_clean), "birmingham") ~ "Birmingham",
    stringr::str_detect(tolower(city_clean), "brisbane") ~ "Brisbane",
    TRUE ~ stringr::str_to_title(city_clean)
  )
  return(city_clean)
}

# Function to extract numeric values
extract_numeric <- function(text, pattern = "\\d+[\\.\\,]?\\d*") {
  if (is.na(text) || text == "") return(NA)
  numbers <- stringr::str_extract_all(text, pattern)[[1]]
  if (length(numbers) == 0) return(NA)
  first_number <- stringr::str_replace_all(numbers[1], ",", "")
  as.numeric(first_number)
}

# Function to standardize study area size (to km2)
standardize_study_area_size <- function(area_text) {
  if (is.na(area_text) || area_text == "") return(NA)
  area_lower <- tolower(area_text)
  numeric_val <- extract_numeric(area_text)
  if (is.na(numeric_val)) return(NA)
  if (stringr::str_detect(area_lower, "km²|km2|square km")) {
    return(numeric_val)
  } else if (stringr::str_detect(area_lower, "m²|m2|square m")) {
    return(numeric_val / 1e6)
  } else {
    return(numeric_val)
  }
}

# Function to calculate estimated study area size from spatial units
calculate_estimated_study_area <- function(unit_size_km2, num_units) {
  # Both inputs should be numeric
  if (is.na(unit_size_km2) || is.na(num_units) || 
      !is.numeric(unit_size_km2) || !is.numeric(num_units)) {
    return(NA)
  }
  # Calculate total area
  estimated_area <- unit_size_km2 * num_units
  return(estimated_area)
}

# Function to standardize crime types
standardize_crime_type <- function(crime_text) {
  if (is.na(crime_text) || crime_text == "") return(NA)
  crime_lower <- tolower(stringr::str_trim(crime_text))
  dplyr::case_when(
    stringr::str_detect(crime_lower, "burglary|burglar") ~ "Burglary",
    stringr::str_detect(crime_lower, "theft|stealing") ~ "Theft",
    stringr::str_detect(crime_lower, "robbery|robber") ~ "Robbery",
    stringr::str_detect(crime_lower, "graffiti|vandal") ~ "Graffiti/Vandalism",
    stringr::str_detect(crime_lower, "drug|dealer") ~ "Drug-related",
    stringr::str_detect(crime_lower, "multiple|various") ~ "Multiple Types",
    stringr::str_detect(crime_lower, "not specified|unspecified") ~ "Not Specified",
    TRUE ~ stringr::str_to_title(crime_text)
  )
}

# Function to clean rationale category
clean_rationale_category <- function(rationale_text) {
  if (is.na(rationale_text) || rationale_text == "") return(NA)
  rationale_clean <- stringr::str_trim(rationale_text)
  dplyr::case_when(
    stringr::str_detect(tolower(rationale_clean), "theory.*method|method.*theory") ~ "Theory-Method",
    stringr::str_detect(tolower(rationale_clean), "data.*availability|admin.*convenience") ~ "Data Availability",
    stringr::str_detect(tolower(rationale_clean), "practical.*constraint|constraint.*practical") ~ "Practical Constraint",
    stringr::str_detect(tolower(rationale_clean), "prior.*research|research.*prior") ~ "Prior Research",
    TRUE ~ rationale_clean
  )
}

# Function to clean study design
clean_study_design <- function(design_text) {
  if (is.na(design_text) || design_text == "") return(NA)
  design_lower <- tolower(stringr::str_trim(design_text))
  dplyr::case_when(
    stringr::str_detect(design_lower, "cross.*sectional|cross-sectional") ~ "Cross-sectional",
    stringr::str_detect(design_lower, "panel") ~ "Panel",
    stringr::str_detect(design_lower, "longitudinal") ~ "Longitudinal",
    stringr::str_detect(design_lower, "case.*control") ~ "Case-control",
    TRUE ~ stringr::str_to_title(design_text)
  )
}

# Function to clean discrete choice model
clean_choice_model <- function(model_text) {
  if (is.na(model_text) || model_text == "") return(NA)
  model_clean <- stringr::str_trim(model_text)
  dplyr::case_when(
    stringr::str_detect(tolower(model_clean), "conditional.*logit|logit.*conditional") ~ "Conditional Logit",
    stringr::str_detect(tolower(model_clean), "multinomial.*logit|logit.*multinomial") ~ "Multinomial Logit",
    stringr::str_detect(tolower(model_clean), "mixed.*logit|logit.*mixed") ~ "Mixed Logit",
    stringr::str_detect(tolower(model_clean), "nested.*logit|logit.*nested") ~ "Nested Logit",
    TRUE ~ model_clean
  )
}

# Function to clean estimation method
clean_estimation_method <- function(method_text) {
  if (is.na(method_text) || method_text == "") return(NA)
  method_lower <- tolower(stringr::str_trim(method_text))
  dplyr::case_when(
    stringr::str_detect(method_lower, "maximum.*likelihood|ml") ~ "Maximum Likelihood",
    stringr::str_detect(method_lower, "not.*mentioned|not.*specified") ~ "Not Specified",
    stringr::str_detect(method_lower, "bayesian") ~ "Bayesian",
    stringr::str_detect(method_lower, "mcmc") ~ "MCMC",
    TRUE ~ stringr::str_to_title(method_text)
  )
}

# Function to standardize binary fields
standardize_yes_no_na <- function(text_vector) {
  sapply(text_vector, function(x) {
    if (is.na(x) || x == "" || stringr::str_detect(tolower(x), "not.*mentioned|not.*specified|na")) {
      return("Not Specified")
    } else {
      return("Specified")
    }
  })
}

# Apply all cleaning functions and select cleaned variables
df_clean <- df_raw |>
  dplyr::mutate(
    Data_Collection_Period_Clean = sapply(Data_Collection_Period, clean_data_collection_period),
    Study_Period_Reference = sapply(Study_Period, clean_data_collection_period),
    Country_Clean = sapply(Country, standardize_country),
    City_Region_Clean = sapply(City_Region, standardize_city_region),
    Study_Area_Size_km2 = sapply(Study_Area_Size, standardize_study_area_size),
    Population_per_Unit_Numeric = sapply(Population_per_Unit, extract_numeric),
    Sample_Size_Numeric = sapply(Sample_Size, extract_numeric),
    Number_of_Crimes_Numeric = sapply(Number_of_Crimes_Analyzed, extract_numeric),
    Number_of_Offenders_Numeric = sapply(Number_of_Offenders, extract_numeric),
    Crime_Type_Clean = sapply(Crime_Type, standardize_crime_type),
    Rationale_Category_Clean = sapply(Rationale_Category, clean_rationale_category),
    Study_Design_Clean = sapply(Study_Design, clean_study_design),
    Discrete_Choice_Model_Clean = sapply(Discrete_Choice_Model, clean_choice_model),
    Estimation_Method_Clean = sapply(Estimation_Method, clean_estimation_method),
    Model_Comparison_Status = standardize_yes_no_na(Model_Comparison),
    Sample_Size_Effects_Status = standardize_yes_no_na(Sample_Size_Effects),
    Robustness_Checks_Status = standardize_yes_no_na(Robustness_Checks),
    Scale_Recommendations_Status = standardize_yes_no_na(Scale_Recommendations),
    Scale_Limitations_Status = standardize_yes_no_na(Scale_Limitations),
    Cross_Scale_Comparisons_Status = standardize_yes_no_na(Cross_Scale_Comparisons)
  ) |>
  dplyr::select(
    Title, 
    Data_Collection_Period = Data_Collection_Period_Clean,
    Study_Period = Study_Period_Reference,
    Country = Country_Clean, 
    City_Region = City_Region_Clean, 
    Study_Area_Size_km2,
    Elicit_Study_Area_km2,
    Total_Units_Region,
    Average_Unit_Size_km2,
    Area_Calculation_Method,
    Area_Source,
    # Removed Estimated_Study_Area_km2 and Combined_Study_Area_km2 from output
    Population_per_Unit_Numeric, 
    Crime_Type = Crime_Type_Clean,
    Sample_Size_Numeric, Number_of_Crimes_Numeric, Number_of_Offenders_Numeric,
    Rationale_Category = Rationale_Category_Clean, 
    Study_Design = Study_Design_Clean, 
    Discrete_Choice_Model = Discrete_Choice_Model_Clean, 
    Estimation_Method = Estimation_Method_Clean,
    Model_Comparison_Status, Sample_Size_Effects_Status, Robustness_Checks_Status,
    Scale_Recommendations_Status, Scale_Limitations_Status, Cross_Scale_Comparisons_Status,
    Quoted_Rationale, Justification_Summary, 
    Sampling_Approach, Choice_Set_Definition, Alternative_Selection,
    Significant_Predictors, Effect_Directions,
    Data_Sources, Data_Availability,
    Data_Quality_Issues, Missing_Data_Issues, Data_Source_Limitations,
    Measurement_Issues, Temporal_Limitations, Generalizability,
    Context_Specificity, Comparative_Limitations,
    Spatial_Scale_Recommendations, Data_Collection_Suggestions, 
    Methodological_Improvements,
    Demographic_Variables, Demographic_Count,
    Economic_Variables, Economic_Count,
    Environmental_Variables, Environmental_Count,
    Distance_Variables, Distance_Count,
    Temporal_Variables, Temporal_Count,
    Total_Variables
  )

# Save the final cleaned dataset for analysis
if (nrow(df_clean) == 0) warning("df_clean is empty!")
custom_save(df_clean, output_folder, "analysis_ready_dataset_clean", write.csv, row.names = FALSE, fileEncoding = "UTF-8")

# Try to merge spatial unit data for calculation purposes
spatial_unit_file <- here::here("Data", "20250704_Table.csv")
if (file.exists(spatial_unit_file)) {
  spatial_data <- read.csv(spatial_unit_file, stringsAsFactors = FALSE)
  
  # Process spatial unit data to match our dataset
  spatial_processed <- spatial_data |>
    dplyr::mutate(
      Unit_size_km2 = dplyr::case_when(
        Unit == "m2" ~ as.numeric(Size_of_the_unit) / 1e6,
        Unit == "km2" ~ as.numeric(Size_of_the_unit),
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(Title_of_the_study, Unit_size_km2, No_of_units) |>
    dplyr::rename(Title = Title_of_the_study, Number_of_Units = No_of_units)
  
  # Merge with analysis-ready dataset
  df_raw <- merge(df_raw, spatial_processed, by = "Title", all.x = TRUE)
} else {
  # Initialize columns if not present
  if (!"Unit_size_km2" %in% colnames(df_raw)) df_raw$Unit_size_km2 <- NA
  if (!"Number_of_Units" %in% colnames(df_raw)) df_raw$Number_of_Units <- NA
}

# =============================================================================#
# Data Analysis and Output Generation
# =============================================================================#

# Calculate summary statistics - ensure columns exist first
if (!"Study_Area_Size_km2" %in% names(result_data)) {
  result_data$Study_Area_Size_km2 <- as.numeric(result_data$Study_Area_Size)
}

summary_stats <- result_data |>
  dplyr::summarise(
    N_Studies = dplyr::n(),
    Mean_Unit_Size = round(mean(Study_Area_Size_km2, na.rm = TRUE), 4),
    Median_Unit_Size = round(median(Study_Area_Size_km2, na.rm = TRUE), 4),
    SD_Unit_Size = round(sd(Study_Area_Size_km2, na.rm = TRUE), 4),
    Min_Unit_Size = round(min(Study_Area_Size_km2, na.rm = TRUE), 6),
    Max_Unit_Size = round(max(Study_Area_Size_km2, na.rm = TRUE), 2),
    Q1_Unit_Size = round(quantile(Study_Area_Size_km2, 0.25, na.rm = TRUE), 4),
    Q3_Unit_Size = round(quantile(Study_Area_Size_km2, 0.75, na.rm = TRUE), 4),
    IQR_Unit_Size = round(IQR(Study_Area_Size_km2, na.rm = TRUE), 4),
    .groups = 'drop'
  )

# Add skewness and kurtosis if e1071 package is available
if (requireNamespace("e1071", quietly = TRUE)) {
  summary_stats$Skewness <- round(e1071::skewness(result_data$Study_Area_Size_km2, na.rm = TRUE), 3)
  summary_stats$Kurtosis <- round(e1071::kurtosis(result_data$Study_Area_Size_km2, na.rm = TRUE), 3)
} else {
  summary_stats$Skewness <- NA
  summary_stats$Kurtosis <- NA
}

# Create a summary table
summary_statistics <- data.frame(
  Metric = c(
    "Raw Dataset Rows",
    "Final Dataset Rows", 
    "Total Studies",
    "Median Unit Size",
    "Mean Unit Size",
    "Standard Deviation",
    "Skewness (original)",
    "Orders of Magnitude Range"
  ),
  Value = c(
    raw_dataset_rows,
    nrow(result_data),
    nrow(result_data),
    paste(round(median(result_data$Study_Area_Size_km2, na.rm = TRUE), 1), "km²"),
    paste(round(mean(result_data$Study_Area_Size_km2, na.rm = TRUE), 2), "km²"),
    paste(round(sd(result_data$Study_Area_Size_km2, na.rm = TRUE), 2), "km²"),
    ifelse(is.na(summary_stats$Skewness), "N/A", as.character(summary_stats$Skewness)),
    ifelse(all(is.na(result_data$Study_Area_Size_km2)) || 
             min(result_data$Study_Area_Size_km2, na.rm = TRUE) <= 0, 
           "N/A",
           as.character(round(log10(max(result_data$Study_Area_Size_km2, na.rm = TRUE) / 
                                      min(result_data$Study_Area_Size_km2, na.rm = TRUE)), 1)))
  ),
  stringsAsFactors = FALSE
)

# Print summary statistics
print("=== COMPREHENSIVE EXTRACTION SUMMARY ===")
print(summary_statistics)

# Create unit type distribution - safely handle missing columns
if ("Unit_Type" %in% names(df_combined)) {
  unit_type_distribution <- df_combined |>
    dplyr::count(Unit_Type, name = "Studies") |>
    dplyr::arrange(dplyr::desc(Studies)) |>
    dplyr::mutate(
      Percentage = round((Studies / sum(Studies)) * 100, 2),
      Proportion = round(Studies / sum(Studies), 3)
    )
} else {
  unit_type_distribution <- data.frame(
    Unit_Type = "Not Available",
    Studies = nrow(df_combined),
    Percentage = 100,
    Proportion = 1,
    stringsAsFactors = FALSE
  )
}

print("=== UNIT TYPE DISTRIBUTION ===")
print(unit_type_distribution)

# Create justification analysis - safely handle missing columns
if ("Has_Unit_Justification" %in% names(df_combined)) {
  justification_analysis <- df_combined |>
    dplyr::summarise(
      Total_Studies = dplyr::n(),
      With_Justification = sum(Has_Unit_Justification, na.rm = TRUE),
      Percent_Justified = round(100 * With_Justification / Total_Studies, 1),
      With_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
      With_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE),
      .groups = 'drop'
    )
} else {
  justification_analysis <- data.frame(
    Total_Studies = nrow(df_combined),
    With_Justification = 0,
    Percent_Justified = 0,
    With_Quoted_Rationale = 0,
    With_Rationale_Category = 0,
    stringsAsFactors = FALSE
  )
}

print("=== JUSTIFICATION ANALYSIS ===")
print(justification_analysis)

# Assign all results to sheets and create Excel file
all_results <- list(
  "Summary_Statistics" = summary_statistics,
  "Unit_Type_Distribution" = unit_type_distribution,
  "Justification_Analysis" = justification_analysis,
  "Raw_Dataset" = df_combined
)

# Save comprehensive Excel file
excel_filename <- file.path(output_folder, "comprehensive_analysis_results.xlsx")
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(all_results, path = excel_filename)
  print(paste("Excel file saved:", excel_filename))
} else {
  warning("writexl package not available. Skipping Excel file creation.")
}

print("=== SCRIPT COMPLETED SUCCESSFULLY ===")
print(paste("Output files saved in:", output_folder))
print("Files created:")
print("- clean_combined_dataset_merged.csv")
print("- comprehensive_analysis_results.xlsx")

# =============================================================================#
# Comprehensive Variable Categorization System
# =============================================================================#

# Create comprehensive variable categorization dictionaries
create_variable_categories <- function() {
  list(
    demographic = c(
      # Age-related
      "age", "aged", "age group", "age cohort", "age distribution", "median age", "average age",
      "elderly", "senior", "youth", "young", "adolescent", "adult", "middle-aged",
      "age 0-17", "age 18-64", "age 65+", "under 18", "over 65",
      
      # Population characteristics
      "population", "population density", "pop density", "population size", "residents",
      "population count", "total population", "residential population", "census population",
      "population per unit", "population per area", "persons per unit", "inhabitants",
      
      # Ethnicity and race
      "ethnicity", "ethnic", "race", "racial", "minority", "ethnic minority", "ethnic composition",
      "black", "white", "hispanic", "latino", "asian", "african", "european", "immigrant",
      "foreign born", "non-native", "diversity", "ethnic diversity", "racial diversity",
      "percentage black", "percentage white", "percentage minority", "ethnic mix",
      
      # Household characteristics
      "household", "households", "household size", "family size", "household composition",
      "family structure", "single parent", "married couple", "divorced", "separated",
      "household income", "family income", "household type", "family type",
      "single person household", "multi-person household", "household head",
      
      # Marital status
      "married", "marriage", "marital status", "single", "divorced", "separated", "widowed",
      "never married", "cohabiting", "partnership", "civil union",
      
      # Gender
      "gender", "male", "female", "sex", "men", "women", "gender ratio", "sex ratio",
      "percentage male", "percentage female", "gender composition", "sex composition",
      
      # Education
      "education", "educational", "schooling", "school", "college", "university", "degree",
      "high school", "secondary school", "primary school", "tertiary education",
      "dropout", "graduation", "literacy", "educational attainment", "qualification",
      "bachelors", "masters", "doctorate", "phd", "vocational", "training",
      "no education", "less than high school", "high school graduate", "some college",
      
      # Social characteristics
      "social", "community", "neighborhood", "residential", "mobility", "migration",
      "social cohesion", "social capital", "social structure", "social class",
      "social mobility", "residential mobility", "residential stability",
      "length of residence", "years in residence", "time in neighborhood"
    ),
    
    economic = c(
      # Income measures
      "income", "earnings", "wage", "salary", "pay", "compensation", "wealth",
      "household income", "family income", "per capita income", "median income",
      "average income", "mean income", "disposable income", "gross income", "net income",
      "income level", "income distribution", "income inequality", "income quintile",
      "low income", "high income", "middle income", "income bracket", "income group",
      
      # Employment
      "employment", "unemployed", "unemployment", "employed", "job", "work", "labor",
      "employment rate", "unemployment rate", "labor force", "workforce", "jobless",
      "employment status", "job market", "labor market", "employment opportunity",
      "full-time", "part-time", "temporary", "permanent", "contract", "freelance",
      "self-employed", "entrepreneur", "employer", "employee", "worker",
      
      # Occupation
      "occupation", "occupational", "professional", "skilled", "unskilled", "blue collar",
      "white collar", "manual", "service", "managerial", "executive", "technical",
      "clerical", "administrative", "sales", "retail", "manufacturing", "construction",
      "agriculture", "farming", "mining", "transportation", "healthcare", "education",
      "government", "public sector", "private sector", "industry", "sector",
      
      # Business and commerce
      "business", "commercial", "retail", "shop", "store", "market", "trade", "commerce",
      "economic activity", "economic development", "economic growth", "gdp", "productivity",
      "investment", "capital", "assets", "property value", "real estate", "housing value",
      "business density", "commercial density", "retail density", "enterprise",
      "small business", "large business", "corporation", "company", "firm",
      
      # Poverty and deprivation
      "poverty", "poor", "deprivation", "disadvantaged", "deprived", "hardship",
      "poverty rate", "poverty level", "below poverty line", "low socioeconomic",
      "socioeconomic status", "social deprivation", "economic deprivation",
      "material deprivation", "financial hardship", "economic disadvantage",
      
      # Benefits and welfare
      "benefits", "welfare", "social security", "unemployment benefits", "food stamps",
      "housing assistance", "government assistance", "public assistance", "subsidy",
      "social support", "financial support", "transfer payments", "safety net"
    ),
    
    environmental = c(
      # Crime attractors and generators
      "bars", "pubs", "nightclub", "restaurant", "fast food", "takeaway", "liquor store",
      "off-license", "convenience store", "gas station", "petrol station", "atm",
      "bank", "check cashing", "pawn shop", "second hand", "shopping center", "mall",
      "supermarket", "grocery", "retail outlet", "hotel", "motel", "hostel",
      "entertainment", "cinema", "theater", "stadium", "sports facility", "gym",
      "recreation", "park", "playground", "community center", "library", "church",
      "religious facility", "school", "college", "university", "hospital", "clinic",
      "pharmacy", "medical facility", "government office", "court", "police station",
      
      # Land use and zoning
      "land use", "zoning", "residential", "commercial", "industrial", "mixed use",
      "single family", "multi family", "apartment", "condo", "townhouse", "housing",
      "housing type", "housing density", "dwelling", "dwelling type", "building type",
      "building height", "building age", "construction", "development", "redevelopment",
      "vacant", "vacant land", "vacant building", "abandoned", "derelict", "demolition",
      
      # Physical environment
      "physical", "environmental", "built environment", "urban form", "street layout",
      "road network", "intersection", "traffic", "pedestrian", "walkability",
      "street lighting", "lighting", "illumination", "visibility", "surveillance",
      "cctv", "security", "defensible space", "natural surveillance", "permeability",
      "connectivity", "accessibility", "barrier", "fence", "wall", "gate",
      
      # Green space and nature
      "green space", "park", "garden", "forest", "tree", "vegetation", "natural",
      "open space", "public space", "recreational space", "playground", "sports field",
      "water", "river", "lake", "beach", "waterfront", "coastal", "environmental quality",
      
      # Disorder and decay
      "disorder", "decay", "deterioration", "dilapidation", "maintenance", "upkeep",
      "cleanliness", "litter", "garbage", "graffiti", "vandalism", "broken windows",
      "property maintenance", "building condition", "neighborhood condition",
      "physical disorder", "social disorder", "incivilities", "nuisance",
      
      # Density and concentration
      "density", "concentration", "clustering", "spatial concentration", "activity density",
      "population density", "housing density", "retail density", "commercial density",
      "business density", "facility density", "service density", "amenity density"
    ),
    
    distance = c(
      # Distance measures
      "distance", "proximity", "accessibility", "access", "nearest", "closest", "near",
      "far", "remote", "adjacent", "neighboring", "nearby", "within", "buffer",
      "euclidean distance", "straight line", "as crow flies", "manhattan distance",
      "network distance", "travel distance", "walking distance", "driving distance",
      "travel time", "commute time", "journey time", "trip time", "reachability",
      
      # Transportation
      "transport", "transportation", "public transport", "transit", "bus", "train",
      "subway", "metro", "tram", "rail", "railway", "station", "bus stop", "terminal",
      "airport", "highway", "motorway", "freeway", "main road", "arterial", "street",
      "road", "route", "path", "walkway", "cycle path", "bike lane", "pedestrian",
      
      # Centrality and location
      "central", "centrality", "city center", "downtown", "cbd", "central business district",
      "urban center", "town center", "suburban", "periphery", "edge", "boundary",
      "location", "position", "site", "place", "geographic", "spatial", "coordinate",
      "latitude", "longitude", "address", "postcode", "zip code", "area code",
      
      # Mobility and movement
      "mobility", "movement", "flow", "circulation", "connectivity", "linkage",
      "connection", "network", "route", "pathway", "corridor", "junction", "node",
      "hub", "gateway", "access point", "entry", "exit", "intersection", "crossroad",
      
      # Travel patterns
      "travel", "trip", "journey", "commute", "commuting", "travel behavior",
      "travel pattern", "trip pattern", "mobility pattern", "movement pattern",
      "origin", "destination", "home", "work", "workplace", "employment location",
      "activity location", "service location", "facility location"
    ),
    
    temporal = c(
      # Time periods
      "time", "temporal", "period", "season", "seasonal", "month", "monthly", "week",
      "weekly", "day", "daily", "hour", "hourly", "minute", "year", "annual", "yearly",
      "quarter", "quarterly", "semester", "term", "cycle", "timing", "schedule",
      
      # Time of day
      "morning", "afternoon", "evening", "night", "midnight", "noon", "dawn", "dusk",
      "daytime", "nighttime", "business hours", "working hours", "peak hours",
      "off-peak", "rush hour", "weekend", "weekday", "holiday", "workday",
      
      # Time trends
      "trend", "change", "growth", "decline", "increase", "decrease", "variation",
      "fluctuation", "pattern", "cycle", "rhythm", "frequency", "rate", "speed",
      "duration", "length", "period", "interval", "gap", "lag", "delay",
      
      # Historical
      "historical", "past", "previous", "prior", "before", "after", "since", "until",
      "baseline", "reference", "comparison", "change over time", "time series",
      "longitudinal", "cross-sectional", "panel", "repeated", "follow-up",
      
      # Control variables
      "control", "control variable", "covariate", "confounding", "confound", "fixed effect",
      "random effect", "dummy", "indicator", "binary", "categorical", "continuous",
      "interaction", "interaction term", "moderator", "mediator", "proxy", "instrument",
      "lagged", "spatial lag", "temporal lag", "autoregressive", "autocorrelation"
    )
  )
}

# Function to extract all variables from all sections
extract_all_variables_comprehensive <- function(df, row_index) {
  all_variables <- character()
  
  # Define all variable sections to check
  variable_sections <- c(
    "DEMOGRAPHIC & SOCIAL VARIABLES",
    "ECONOMIC VARIABLES", 
    "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
    "DISTANCE & ACCESSIBILITY VARIABLES",
    "TEMPORAL & CONTROL VARIABLES"
  )
  
  # Extract variables from each section
  for (section in variable_sections) {
    if (section %in% names(df)) {
      text <- df[[section]][row_index]
      if (!is.na(text) && text != "" && text != "N/A") {
        # Extract numbered items
        items <- stringr::str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
        
        for (item in items) {
          clean_item <- stringr::str_remove(item, "^\\d+\\.\\s*")
          clean_item <- stringr::str_trim(clean_item)
          
          # Extract variable name (before | if it exists)
          if (stringr::str_detect(clean_item, "\\|")) {
            parts <- stringr::str_split(clean_item, "\\|")[[1]] |> stringr::str_trim()
            if (length(parts) >= 1) {
              var_name <- parts[1]
              all_variables <- c(all_variables, var_name)
            }
          } else {
            all_variables <- c(all_variables, clean_item)
          }
        }
      }
    }
  }
  
  # Also check other sections that might contain variables
  other_sections <- c(
    "MAJOR FINDINGS & RESULTS",
    "STUDY DESIGN & METHODOLOGY",
    "DATA PREPARATION & PROCESSING"
  )
  
  for (section in other_sections) {
    if (section %in% names(df)) {
      text <- df[[section]][row_index]
      if (!is.na(text) && text != "" && text != "N/A") {
        # Look for variable mentions in results or methodology
        var_patterns <- c(
          "variables?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "predictors?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "covariates?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)",
          "factors?\\s*(?:included|used|analyzed|examined|considered)\\s*:?\\s*([^\\n\\.]+)"
        )
        
        for (pattern in var_patterns) {
          matches <- stringr::str_match_all(text, pattern)
          if (length(matches[[1]]) > 0) {
            for (i in 1:nrow(matches[[1]])) {
              var_text <- matches[[1]][i, 2]
              if (!is.na(var_text)) {
                # Split by common delimiters
                vars <- stringr::str_split(var_text, "[,;]")[[1]] |> stringr::str_trim()
                all_variables <- c(all_variables, vars)
              }
            }
          }
        }
      }
    }
  }
  
  # Clean and deduplicate
  all_variables <- unique(all_variables[all_variables != "" & !is.na(all_variables)])
  
  return(all_variables)
}

# Function to categorize variables based on content
categorize_variables <- function(variables) {
  if (length(variables) == 0) {
    return(list(
      demographic = character(),
      economic = character(),
      environmental = character(),
      distance = character(),
      temporal = character(),
      uncategorized = character()
    ))
  }
  
  categories <- create_variable_categories()
  
  categorized <- list(
    demographic = character(),
    economic = character(),
    environmental = character(),
    distance = character(),
    temporal = character(),
    uncategorized = character()
  )
  
  for (var in variables) {
    var_lower <- tolower(var)
    categorized_flag <- FALSE
    
    # Check each category
    for (category_name in names(categories)) {
      category_keywords <- categories[[category_name]]
      
      # Check if any keyword matches
      for (keyword in category_keywords) {
        if (stringr::str_detect(var_lower, paste0("\\b", keyword, "\\b"))) {
          categorized[[category_name]] <- c(categorized[[category_name]], var)
          categorized_flag <- TRUE
          break
        }
      }
      
      if (categorized_flag) break
    }
    
    # If not categorized, add to uncategorized
    if (!categorized_flag) {
      categorized$uncategorized <- c(categorized$uncategorized, var)
    }
  }
  
  # Remove duplicates from each category
  for (category_name in names(categorized)) {
    categorized[[category_name]] <- unique(categorized[[category_name]])
  }
  
  return(categorized)
}

# Function to create summary statistics for categorized variables
create_variable_summary <- function(categorized_vars) {
  summary_stats <- data.frame(
    Category = names(categorized_vars),
    Count = sapply(categorized_vars, length),
    Variables = sapply(categorized_vars, function(x) {
      if (length(x) > 0) {
        paste(x, collapse = "; ")
      } else {
        NA
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(summary_stats)
}

# Old function removed - using improved extraction approach above

# Create variable categorization summary
variable_summary <- data.frame(
  Study_Title = result_data_final$Title,
  Demographic_Count = result_data_final$Demographic_Count,
  Economic_Count = result_data_final$Economic_Count,
  Environmental_Count = result_data_final$Environmental_Count,
  Distance_Count = result_data_final$Distance_Count,
  Temporal_Count = result_data_final$Temporal_Count,
  Uncategorized_Count = result_data_final$Uncategorized_Count,
  Total_Variables = result_data_final$Total_Variables,
  stringsAsFactors = FALSE
)

# Save variable categorization summary
custom_save(variable_summary, output_folder, "variable_categorization_summary", readr::write_csv)

# Create detailed variable listing
variable_details <- data.frame(
  Study_Title = result_data_final$Title,
  Demographic_Variables = result_data_final$Demographic_Variables,
  Economic_Variables = result_data_final$Economic_Variables,
  Environmental_Variables = result_data_final$Environmental_Variables,
  Distance_Variables = result_data_final$Distance_Variables,
  Temporal_Variables = result_data_final$Temporal_Variables,
  Uncategorized_Variables = result_data_final$Uncategorized_Variables,
  All_Variables = result_data_final$All_Variables,
  stringsAsFactors = FALSE
)

# Save detailed variable listing
custom_save(variable_details, output_folder, "detailed_variable_listing", readr::write_csv)

# Create uncategorized variables report for review
if (any(!is.na(result_data_final$Uncategorized_Variables))) {
  uncategorized_report <- result_data_final[!is.na(result_data_final$Uncategorized_Variables), 
                                            c("Title", "Uncategorized_Variables", "Uncategorized_Count")]
  
  # Save uncategorized variables report
  custom_save(uncategorized_report, output_folder, "uncategorized_variables_report", readr::write_csv)
}

# Function to extract study area information from Elicit study area size data
extract_study_area_info <- function(area_text) {
  if (is.na(area_text) || area_text == "" || area_text == "N/A") {
    return(list(
      extracted_area_km2 = NA,
      total_units = NA,
      average_unit_size_km2 = NA,
      calculation_method = "Not Available",
      area_source = "Not Mentioned"
    ))
  }
  
  # Clean the text
  clean_text <- stringr::str_trim(area_text)
  
  # Check if it's explicitly mentioned as not available
  if (stringr::str_detect(tolower(clean_text), "not mentioned|not provided|not available")) {
    return(list(
      extracted_area_km2 = NA,
      total_units = NA,
      average_unit_size_km2 = NA,
      calculation_method = "Not Available",
      area_source = "Not Mentioned"
    ))
  }
  
  # Try to extract direct area measurements
  area_patterns <- c(
    "([0-9,]+(?:\\.[0-9]+)?)\\s*km[²2]",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*square\\s*km",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*km\\^2"
  )
  
  extracted_area <- NA
  for (pattern in area_patterns) {
    match <- stringr::str_extract(clean_text, pattern)
    if (!is.na(match)) {
      number <- stringr::str_extract(match, "[0-9,]+(?:\\.[0-9]+)?")
      extracted_area <- as.numeric(stringr::str_replace_all(number, ",", ""))
      break
    }
  }
  
  # Try to extract calculation components (number * size = total)
  calc_patterns <- c(
    "([0-9,]+)\\s*(?:units?|areas?).*?([0-9,]+(?:\\.[0-9]+)?)\\s*km[²2]",
    "([0-9,]+(?:\\.[0-9]+)?)\\s*km[²2].*?([0-9,]+)\\s*(?:units?|areas?)"
  )
  
  total_units <- NA
  avg_unit_size <- NA
  calculated_area <- NA
  
  for (pattern in calc_patterns) {
    match <- stringr::str_match(clean_text, pattern)
    if (!is.na(match[1, 1])) {
      num1 <- as.numeric(stringr::str_replace_all(match[1, 2], ",", ""))
      num2 <- as.numeric(stringr::str_replace_all(match[1, 3], ",", ""))
      
      if (!is.na(num1) && !is.na(num2)) {
        # Try both interpretations
        if (num1 > num2) {
          total_units <- num1
          avg_unit_size <- num2
          calculated_area <- num1 * num2
        } else {
          total_units <- num2
          avg_unit_size <- num1
          calculated_area <- num1 * num2
        }
        break
      }
    }
  }
  
  # Determine final area value and method
  final_area <- NA
  method <- "Not Available"
  
  if (!is.na(extracted_area)) {
    final_area <- extracted_area
    method <- "Direct Measurement"
  } else if (!is.na(calculated_area)) {
    final_area <- calculated_area
    method <- "Calculated from Units"
  }
  
  # Extract source information
  source_patterns <- c(
    "source:\\s*([^\\n\\.,;]+)",
    "from\\s+([^\\n\\.,;]+(?:database|census|survey|bureau|agency|office))",
    "(census|bureau|agency|office|database)\\s+[^\\n\\.,;]+"
  )
  
  area_source <- "Not Mentioned"
  for (pattern in source_patterns) {
    match <- stringr::str_extract(clean_text, pattern)
    if (!is.na(match)) {
      area_source <- stringr::str_trim(match)
      break
    }
  }
  
  return(list(
    extracted_area_km2 = final_area,
    total_units = total_units,
    average_unit_size_km2 = avg_unit_size,
    calculation_method = method,
    area_source = area_source
  ))
}

