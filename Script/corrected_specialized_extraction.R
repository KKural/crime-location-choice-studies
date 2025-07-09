# Corrected Specialized Prompts Extraction - Exact Copy of Working Script
# This is based on the successful specialized_prompts_extraction.R

library(dplyr)
library(stringr)
library(readr)

# Read the data
df <- read_csv("Output/clean_combined_dataset.csv")

cat("Starting corrected extraction based on working specialized prompts script...\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")

# Function to extract a specific field from text using multiple patterns
extract_field <- function(text, field_name) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(NA)
  }
  
  # Try multiple patterns to extract the field - EXACT SAME AS WORKING SCRIPT
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
      # Clean up the result
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

# Function to extract numbered list items (for variables) - EXACT SAME AS WORKING SCRIPT
extract_numbered_items <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(list())
  }
  
  # Find numbered items (1. Variable | Description | Unit | Source)
  items <- str_extract_all(text, "\\d+\\.[^\\n\\r]+")[[1]]
  
  result <- list()
  for (item in items) {
    # Remove the number prefix
    clean_item <- str_remove(item, "^\\d+\\.\\s*")
    clean_item <- str_trim(clean_item)
    
    if (str_detect(clean_item, "\\|")) {
      # Split by pipes
      parts <- str_split(clean_item, "\\|")[[1]] %>% str_trim()
      if (length(parts) >= 2) {
        var_name <- parts[1]
        var_name <- str_replace_all(var_name, "[^A-Za-z0-9_\\s]", "_")
        var_name <- str_replace_all(var_name, "\\s+", "_")
        var_name <- str_remove_all(var_name, "_+$")
        
        if (length(parts) == 4) {
          # Format: Variable | Description | Unit | Source
          result[[paste0(var_name, "_Description")]] <- parts[2]
          result[[paste0(var_name, "_Unit")]] <- parts[3]
          result[[paste0(var_name, "_Source")]] <- parts[4]
        } else {
          # Other formats
          result[[paste0(var_name, "_Info")]] <- paste(parts[-1], collapse=" | ")
        }
        result[[var_name]] <- var_name  # Store the variable name itself
      }
    } else {
      # Simple item without pipes
      var_name <- str_replace_all(clean_item, "[^A-Za-z0-9_\\s]", "_")
      var_name <- str_replace_all(var_name, "\\s+", "_")
      var_name <- str_remove_all(var_name, "_+$")
      result[[var_name]] <- clean_item
    }
  }
  
  return(result)
}

# Initialize result dataframe with Title
result_data <- data.frame(Title = df$Title, stringsAsFactors = FALSE)

# PROMPT 1: BASIC STUDY IDENTIFICATION - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Basic Study Identification...\n")
for (i in 1:nrow(df)) {
  text <- df$`BASIC STUDY IDENTIFICATION`[i]
  if (!is.na(text)) {
    result_data$Year[i] <- extract_field(text, "Year")
    result_data$Authors[i] <- extract_field(text, "Authors")
    result_data$Journal[i] <- extract_field(text, "Journal")
    result_data$DOI[i] <- extract_field(text, "DOI")
    result_data$Filename[i] <- extract_field(text, "Filename")
  }
}

# PROMPT 2: TEMPORAL SCOPE & DATA SOURCES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Temporal Scope & Data Sources...\n")
for (i in 1:nrow(df)) {
  text <- df$`TEMPORAL SCOPE & DATA SOURCES`[i]
  if (!is.na(text)) {
    result_data$Study_Period[i] <- extract_field(text, "Study Period")
    result_data$Data_Collection_Period[i] <- extract_field(text, "Data Collection Period")
    result_data$Data_Sources[i] <- extract_field(text, "Data Sources")
    result_data$Data_Availability[i] <- extract_field(text, "Data Availability")
  }
}

# PROMPT 3: SPATIAL UNITS - DESCRIPTION & JUSTIFICATION - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Spatial Units Description & Justification...\n")
for (i in 1:nrow(df)) {
  text <- df$`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`[i]
  if (!is.na(text)) {
    result_data$SUoA_Type[i] <- extract_field(text, "SUoA Type")
    result_data$SUoA_Size[i] <- extract_field(text, "SUoA Size")
    result_data$SUoA_Description[i] <- extract_field(text, "SUoA Description")
    result_data$Number_of_Units[i] <- extract_field(text, "Number of Units")
    result_data$Population_per_Unit[i] <- extract_field(text, "Population per Unit")
    result_data$Quoted_Rationale[i] <- extract_field(text, "Quoted Rationale")
    result_data$Rationale_Category[i] <- extract_field(text, "Rationale Category")
    result_data$Justification_Summary[i] <- extract_field(text, "Justification Summary")
  }
}

# PROMPT 4: STUDY CONTEXT & GEOGRAPHY - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Study Context & Geography...\n")
for (i in 1:nrow(df)) {
  text <- df$`STUDY CONTEXT & GEOGRAPHY`[i]
  if (!is.na(text)) {
    result_data$Country[i] <- extract_field(text, "Country")
    result_data$City_Region[i] <- extract_field(text, "City/Region")
    result_data$Study_Area_Size[i] <- extract_field(text, "Study Area Size")
    result_data$Study_Area_Description[i] <- extract_field(text, "Study Area Description")
    result_data$Crime_Type[i] <- extract_field(text, "Crime Type")
    result_data$Crime_Types_All[i] <- extract_field(text, "Crime Types \\(All\\)")
  }
}

# PROMPT 5: SAMPLING & CHOICE SETS - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Sampling & Choice Sets...\n")
for (i in 1:nrow(df)) {
  text <- df$`SAMPLING & CHOICE SETS`[i]
  if (!is.na(text)) {
    result_data$Sampling_Approach[i] <- extract_field(text, "Sampling Approach")
    result_data$Sample_Size[i] <- extract_field(text, "Sample Size")
    result_data$Number_of_Crimes_Analyzed[i] <- extract_field(text, "Number of Crimes Analyzed")
    result_data$Number_of_Offenders[i] <- extract_field(text, "Number of Offenders")
    result_data$Choice_Set_Definition[i] <- extract_field(text, "Choice Set Definition")
    result_data$Alternative_Selection[i] <- extract_field(text, "Alternative Selection")
    result_data$Sample_Restrictions[i] <- extract_field(text, "Sample Restrictions")
  }
}

# PROMPT 6: THEORETICAL FRAMEWORK & OBJECTIVES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Theoretical Framework & Objectives...\n")
for (i in 1:nrow(df)) {
  text <- df$`THEORETICAL FRAMEWORK & OBJECTIVES`[i]
  if (!is.na(text)) {
    result_data$Theoretical_Framework[i] <- extract_field(text, "Theoretical Framework")
    result_data$Research_Objectives[i] <- extract_field(text, "Research Objectives")
    result_data$Literature_Gap[i] <- extract_field(text, "Literature Gap")
    result_data$Study_Motivation[i] <- extract_field(text, "Study Motivation")
  }
}

# PROMPT 7: STUDY DESIGN & METHODOLOGY - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Study Design & Methodology...\n")
for (i in 1:nrow(df)) {
  text <- df$`STUDY DESIGN & METHODOLOGY`[i]
  if (!is.na(text)) {
    result_data$Study_Design[i] <- extract_field(text, "Study Design")
    result_data$Discrete_Choice_Model[i] <- extract_field(text, "Discrete Choice Model")
    result_data$Model_Specification[i] <- extract_field(text, "Model Specification")
    result_data$Software_Used[i] <- extract_field(text, "Software Used")
    result_data$Estimation_Method[i] <- extract_field(text, "Estimation Method")
    result_data$Model_Extensions[i] <- extract_field(text, "Model Extensions")
  }
}

# PROMPT 8: DATA PREPARATION & PROCESSING - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Data Preparation & Processing...\n")
for (i in 1:nrow(df)) {
  text <- df$`DATA PREPARATION & PROCESSING`[i]
  if (!is.na(text)) {
    result_data$Data_Cleaning[i] <- extract_field(text, "Data Cleaning")
    result_data$Variable_Construction[i] <- extract_field(text, "Variable Construction")
    result_data$Missing_Data_Handling[i] <- extract_field(text, "Missing Data Handling")
    result_data$Data_Integration[i] <- extract_field(text, "Data Integration")
    result_data$Quality_Control[i] <- extract_field(text, "Quality Control")
  }
}

# Variable extraction - EXACT SAME APPROACH AS WORKING SCRIPT
all_demo_vars <- list()
all_econ_vars <- list()
all_env_vars <- list()
all_dist_vars <- list()
all_temp_vars <- list()

# PROMPT 9: DEMOGRAPHIC & SOCIAL VARIABLES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Demographic & Social Variables...\n")
for (i in 1:nrow(df)) {
  text <- df$`DEMOGRAPHIC & SOCIAL VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    all_demo_vars <- c(all_demo_vars, vars)
  }
}

# PROMPT 10: ECONOMIC VARIABLES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Economic Variables...\n")
for (i in 1:nrow(df)) {
  text <- df$`ECONOMIC VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    all_econ_vars <- c(all_econ_vars, vars)
  }
}

# PROMPT 11: ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Environmental & Crime Attractor Variables...\n")
for (i in 1:nrow(df)) {
  text <- df$`ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    all_env_vars <- c(all_env_vars, vars)
  }
}

# PROMPT 12: DISTANCE & ACCESSIBILITY VARIABLES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Distance & Accessibility Variables...\n")
for (i in 1:nrow(df)) {
  text <- df$`DISTANCE & ACCESSIBILITY VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    all_dist_vars <- c(all_dist_vars, vars)
  }
}

# PROMPT 13: TEMPORAL & CONTROL VARIABLES - EXACT SAME AS WORKING SCRIPT
cat("Extracting: Temporal & Control Variables...\n")
for (i in 1:nrow(df)) {
  text <- df$`TEMPORAL & CONTROL VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    all_temp_vars <- c(all_temp_vars, vars)
  }
}

# Create columns for unique variables - EXACT SAME AS WORKING SCRIPT
unique_demo_vars <- unique(names(all_demo_vars))
unique_econ_vars <- unique(names(all_econ_vars))
unique_env_vars <- unique(names(all_env_vars))
unique_dist_vars <- unique(names(all_dist_vars))
unique_temp_vars <- unique(names(all_temp_vars))

cat("Found", length(unique_demo_vars), "unique demographic variables\n")
cat("Found", length(unique_econ_vars), "unique economic variables\n")
cat("Found", length(unique_env_vars), "unique environmental variables\n")
cat("Found", length(unique_dist_vars), "unique distance variables\n")
cat("Found", length(unique_temp_vars), "unique temporal variables\n")

# Add variable columns and fill them - EXACT SAME AS WORKING SCRIPT
for (var in unique_demo_vars) {
  result_data[[paste0("DEMO_", var)]] <- NA
}
for (var in unique_econ_vars) {
  result_data[[paste0("ECON_", var)]] <- NA
}
for (var in unique_env_vars) {
  result_data[[paste0("ENV_", var)]] <- NA
}
for (var in unique_dist_vars) {
  result_data[[paste0("DIST_", var)]] <- NA
}
for (var in unique_temp_vars) {
  result_data[[paste0("TEMP_", var)]] <- NA
}

# Fill variable data - EXACT SAME AS WORKING SCRIPT
cat("Filling variable data...\n")
for (i in 1:nrow(df)) {
  # Demographic variables
  text <- df$`DEMOGRAPHIC & SOCIAL VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    for (var_name in names(vars)) {
      col_name <- paste0("DEMO_", var_name)
      if (col_name %in% names(result_data)) {
        result_data[[col_name]][i] <- vars[[var_name]]
      }
    }
  }
  
  # Economic variables
  text <- df$`ECONOMIC VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    for (var_name in names(vars)) {
      col_name <- paste0("ECON_", var_name)
      if (col_name %in% names(result_data)) {
        result_data[[col_name]][i] <- vars[[var_name]]
      }
    }
  }
  
  # Environmental variables
  text <- df$`ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    for (var_name in names(vars)) {
      col_name <- paste0("ENV_", var_name)
      if (col_name %in% names(result_data)) {
        result_data[[col_name]][i] <- vars[[var_name]]
      }
    }
  }
  
  # Distance variables
  text <- df$`DISTANCE & ACCESSIBILITY VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    for (var_name in names(vars)) {
      col_name <- paste0("DIST_", var_name)
      if (col_name %in% names(result_data)) {
        result_data[[col_name]][i] <- vars[[var_name]]
      }
    }
  }
  
  # Temporal variables
  text <- df$`TEMPORAL & CONTROL VARIABLES`[i]
  if (!is.na(text)) {
    vars <- extract_numbered_items(text)
    for (var_name in names(vars)) {
      col_name <- paste0("TEMP_", var_name)
      if (col_name %in% names(result_data)) {
        result_data[[col_name]][i] <- vars[[var_name]]
      }
    }
  }
}

# Continue with remaining prompts - EXACT SAME AS WORKING SCRIPT

# PROMPT 14: MODEL FIT & PERFORMANCE METRICS
cat("Extracting: Model Fit & Performance Metrics...\n")
for (i in 1:nrow(df)) {
  text <- df$`MODEL FIT & PERFORMANCE METRICS`[i]
  if (!is.na(text)) {
    result_data$Model_Performance[i] <- extract_field(text, "Model Performance")
    result_data$Information_Criteria[i] <- extract_field(text, "Information Criteria")
    result_data$Goodness_of_Fit_Tests[i] <- extract_field(text, "Goodness-of-Fit Tests")
    result_data$Model_Comparison[i] <- extract_field(text, "Model Comparison")
    result_data$Sample_Size_Effects[i] <- extract_field(text, "Sample Size Effects")
    result_data$Convergence_Issues[i] <- extract_field(text, "Convergence Issues")
  }
}

# PROMPT 15: MAJOR FINDINGS & RESULTS
cat("Extracting: Major Findings & Results...\n")
for (i in 1:nrow(df)) {
  text <- df$`MAJOR FINDINGS & RESULTS`[i]
  if (!is.na(text)) {
    result_data$Main_Results[i] <- extract_field(text, "Main Results")
    result_data$Significant_Predictors[i] <- extract_field(text, "Significant Predictors")
    result_data$Effect_Directions[i] <- extract_field(text, "Effect Directions")
    result_data$Effect_Magnitudes[i] <- extract_field(text, "Effect Magnitudes")
    result_data$Surprising_Findings[i] <- extract_field(text, "Surprising Findings")
    result_data$Robustness_Checks[i] <- extract_field(text, "Robustness Checks")
  }
}

# PROMPT 16: SCALE EFFECTS & SPATIAL FINDINGS
cat("Extracting: Scale Effects & Spatial Findings...\n")
for (i in 1:nrow(df)) {
  text <- df$`SCALE EFFECTS & SPATIAL FINDINGS`[i]
  if (!is.na(text)) {
    result_data$Scale_Effects[i] <- extract_field(text, "Scale Effects")
    result_data$Scale_Sensitivity[i] <- extract_field(text, "Scale Sensitivity")
    result_data$Spatial_Autocorrelation[i] <- extract_field(text, "Spatial Autocorrelation")
    result_data$Scale_Recommendations[i] <- extract_field(text, "Scale Recommendations")
    result_data$Scale_Limitations[i] <- extract_field(text, "Scale Limitations")
    result_data$Cross_Scale_Comparisons[i] <- extract_field(text, "Cross-Scale Comparisons")
  }
}

# PROMPT 17: DATA LIMITATIONS & METHODOLOGICAL ISSUES
cat("Extracting: Data Limitations & Methodological Issues...\n")
for (i in 1:nrow(df)) {
  text <- df$`DATA LIMITATIONS & METHODOLOGICAL ISSUES`[i]
  if (!is.na(text)) {
    result_data$Data_Quality_Issues[i] <- extract_field(text, "Data Quality Issues")
    result_data$Missing_Data[i] <- extract_field(text, "Missing Data")
    result_data$Data_Source_Limitations[i] <- extract_field(text, "Data Source Limitations")
    result_data$Measurement_Issues[i] <- extract_field(text, "Measurement Issues")
    result_data$Temporal_Limitations[i] <- extract_field(text, "Temporal Limitations")
    result_data$Geographic_Limitations[i] <- extract_field(text, "Geographic Limitations")
    result_data$Model_Limitations[i] <- extract_field(text, "Model Limitations")
    result_data$Analytical_Constraints[i] <- extract_field(text, "Analytical Constraints")
  }
}

# PROMPT 18: GENERALIZABILITY & COMPARATIVE LIMITATIONS
cat("Extracting: Generalizability & Comparative Limitations...\n")
for (i in 1:nrow(df)) {
  text <- df$`GENERALIZABILITY & COMPARATIVE LIMITATIONS`[i]
  if (!is.na(text)) {
    result_data$Sample_Limitations[i] <- extract_field(text, "Sample Limitations")
    result_data$Causal_Inference[i] <- extract_field(text, "Causal Inference")
    result_data$Generalizability[i] <- extract_field(text, "Generalizability")
    result_data$Comparative_Limitations[i] <- extract_field(text, "Comparative Limitations")
    result_data$Context_Specificity[i] <- extract_field(text, "Context Specificity")
    result_data$Population_Constraints[i] <- extract_field(text, "Population Constraints")
  }
}

# PROMPT 19: IMPLICATIONS & FUTURE DIRECTIONS
cat("Extracting: Implications & Future Directions...\n")
for (i in 1:nrow(df)) {
  text <- df$`IMPLICATIONS & FUTURE DIRECTIONS`[i]
  if (!is.na(text)) {
    result_data$Theoretical_Contributions[i] <- extract_field(text, "Theoretical Contributions")
    result_data$Policy_Implications[i] <- extract_field(text, "Policy Implications")
    result_data$Crime_Prevention_Implications[i] <- extract_field(text, "Crime Prevention Implications")
    result_data$Urban_Planning_Implications[i] <- extract_field(text, "Urban Planning Implications")
    result_data$Policy_Recommendations[i] <- extract_field(text, "Policy Recommendations")
    result_data$Future_Research_Directions[i] <- extract_field(text, "Future Research Directions")
    result_data$Spatial_Scale_Recommendations[i] <- extract_field(text, "Spatial Scale Recommendations")
    result_data$Data_Collection_Suggestions[i] <- extract_field(text, "Data Collection Suggestions")
    result_data$Methodological_Improvements[i] <- extract_field(text, "Methodological Improvements")
    result_data$Broader_Societal_Implications[i] <- extract_field(text, "Broader Societal Implications")
    result_data$Interdisciplinary_Connections[i] <- extract_field(text, "Interdisciplinary Connections")
  }
}

# Save the result
write_csv(result_data, "Output/corrected_specialized_extraction.csv")

cat("\n=== CORRECTED EXTRACTION SUMMARY ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Extracted dataset:", nrow(result_data), "rows x", ncol(result_data), "columns\n")
cat("Output saved to: Output/corrected_specialized_extraction.csv\n")

# Show summary of key extracted fields
key_fields <- c("Title", "Year", "Authors", "Country", "Study_Period", "Crime_Type", 
               "Discrete_Choice_Model", "Sample_Size", "Main_Results")

cat("\n=== KEY FIELD EXTRACTION SUMMARY ===\n")
for (field in key_fields) {
  if (field %in% names(result_data)) {
    non_na_count <- sum(!is.na(result_data[[field]]))
    cat(sprintf("- %s: %d/%d rows (%.1f%%)\n", 
                field, non_na_count, nrow(result_data), 
                (non_na_count/nrow(result_data))*100))
  }
}

cat("\nCorrected extraction based on successful specialized prompts approach completed!\n")
cat("This script uses the EXACT SAME approach as the working specialized_prompts_extraction.R\n")
