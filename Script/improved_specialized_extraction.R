# IMPROVED Specialized Prompts Extraction Script
# Enhanced version with better pattern matching, data cleaning, and organization

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Read the data
df <- read_csv("Output/clean_combined_dataset.csv")

cat("=== IMPROVED SPECIALIZED PROMPTS EXTRACTION ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")

# Enhanced function to extract fields with multiple patterns and better cleaning
extract_field_enhanced <- function(text, field_name) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(NA)
  }
  
  # Multiple patterns to handle various formatting
  patterns <- c(
    # Standard markdown format: **Field:** Value
    paste0("\\*\\*", field_name, ":\\*\\*\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-\\s*\\*\\*|\\s*$)"),
    # Dash format: - **Field:** Value
    paste0("-\\s*\\*\\*", field_name, ":\\*\\*\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-\\s*\\*\\*|\\s*$)"),
    # Simple dash format: - Field: Value
    paste0("-\\s*", field_name, ":\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-|\\s*$)"),
    # No dash format: Field: Value
    paste0("^", field_name, ":\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*$)"),
    # Flexible format with optional markdown
    paste0("\\*?\\*?", field_name, "\\s*:\\s*\\*?\\*?\\s*([^\\n\\r-]+?)(?=\\s*\\n|\\s*-|\\s*$)")
  )
  
  for (pattern in patterns) {
    matches <- str_match_all(text, pattern)[[1]]
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        result <- str_trim(matches[i, 2])
        # Enhanced cleaning
        result <- str_remove_all(result, "\\*\\*")
        result <- str_remove_all(result, "\\*")
        result <- str_remove(result, "^-\\s*")
        result <- str_remove(result, "\\s*\\)$")
        result <- str_remove(result, "\\s*\\]$")
        result <- str_trim(result)
        
        # Check for valid content
        if (result != "" && 
            !str_detect(tolower(result), "^not\\s+(mentioned|explicitly|available|specified|stated)") &&
            !str_detect(result, "^\\[.*\\]$") &&
            nchar(result) > 1) {
          return(result)
        }
      }
    }
  }
  
  return(NA)
}

# Enhanced function to extract numbered variables with better parsing
extract_variables_enhanced <- function(text) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(list())
  }
  
  result <- list()
  
  # Enhanced pattern for numbered items
  # Pattern 1: "1. Variable | Description | Unit | Source"
  pipe_pattern <- "\\d+\\.\\s*([^|\\n\\r]+?)\\s*\\|\\s*([^|\\n\\r]+?)\\s*\\|\\s*([^|\\n\\r]+?)\\s*\\|\\s*([^\\n\\r]+)"
  pipe_matches <- str_match_all(text, pipe_pattern)[[1]]
  
  if (nrow(pipe_matches) > 0) {
    for (i in 1:nrow(pipe_matches)) {
      var_name <- str_trim(pipe_matches[i, 2])
      description <- str_trim(pipe_matches[i, 3])
      unit <- str_trim(pipe_matches[i, 4])
      source <- str_trim(pipe_matches[i, 5])
      
      # Clean variable name for column naming
      clean_var_name <- str_replace_all(var_name, "[^A-Za-z0-9\\s]", "_")
      clean_var_name <- str_replace_all(clean_var_name, "\\s+", "_")
      clean_var_name <- str_remove_all(clean_var_name, "_+$")
      clean_var_name <- str_remove_all(clean_var_name, "^_+")
      
      if (clean_var_name != "" && nchar(clean_var_name) > 1) {
        result[[clean_var_name]] <- var_name
        result[[paste0(clean_var_name, "_Description")]] <- description
        result[[paste0(clean_var_name, "_Unit")]] <- unit
        result[[paste0(clean_var_name, "_Source")]] <- source
      }
    }
  }
  
  # Pattern 2: "1. Variable | Description"
  two_pipe_pattern <- "\\d+\\.\\s*([^|\\n\\r]+?)\\s*\\|\\s*([^\\n\\r]+)"
  two_pipe_matches <- str_match_all(text, two_pipe_pattern)[[1]]
  
  if (nrow(two_pipe_matches) > 0) {
    for (i in 1:nrow(two_pipe_matches)) {
      var_name <- str_trim(two_pipe_matches[i, 2])
      description <- str_trim(two_pipe_matches[i, 3])
      
      clean_var_name <- str_replace_all(var_name, "[^A-Za-z0-9\\s]", "_")
      clean_var_name <- str_replace_all(clean_var_name, "\\s+", "_")
      clean_var_name <- str_remove_all(clean_var_name, "_+$")
      clean_var_name <- str_remove_all(clean_var_name, "^_+")
      
      if (clean_var_name != "" && nchar(clean_var_name) > 1) {
        result[[clean_var_name]] <- var_name
        result[[paste0(clean_var_name, "_Info")]] <- description
      }
    }
  }
  
  # Pattern 3: Simple numbered items "1. Variable"
  simple_pattern <- "\\d+\\.\\s*([^\\n\\r]+)"
  simple_matches <- str_match_all(text, simple_pattern)[[1]]
  
  if (nrow(simple_matches) > 0 && nrow(pipe_matches) == 0 && nrow(two_pipe_matches) == 0) {
    for (i in 1:nrow(simple_matches)) {
      var_content <- str_trim(simple_matches[i, 2])
      
      # Skip if it looks like it has pipes (already handled above)
      if (str_detect(var_content, "\\|")) next
      
      clean_var_name <- str_replace_all(var_content, "[^A-Za-z0-9\\s]", "_")
      clean_var_name <- str_replace_all(clean_var_name, "\\s+", "_")
      clean_var_name <- str_remove_all(clean_var_name, "_+$")
      clean_var_name <- str_remove_all(clean_var_name, "^_+")
      clean_var_name <- str_sub(clean_var_name, 1, 50)  # Limit length
      
      if (clean_var_name != "" && nchar(clean_var_name) > 1) {
        result[[clean_var_name]] <- var_content
      }
    }
  }
  
  return(result)
}

# Initialize result dataframe
result_data <- data.frame(Title = df$Title, stringsAsFactors = FALSE)

# CORE FIELDS EXTRACTION with progress tracking
core_fields <- list(
  # Prompt 1: Basic Study Identification
  "BASIC STUDY IDENTIFICATION" = c("Year", "Authors", "Journal", "DOI", "Filename"),
  
  # Prompt 2: Temporal Scope & Data Sources  
  "TEMPORAL SCOPE & DATA SOURCES" = c("Study Period", "Data Collection Period", "Data Sources", "Data Availability"),
  
  # Prompt 3: Spatial Units
  "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION" = c("SUoA Type", "SUoA Size", "SUoA Description", "Number of Units", 
                                                   "Population per Unit", "Quoted Rationale", "Rationale Category", 
                                                   "Justification Summary"),
  
  # Prompt 4: Study Context & Geography
  "STUDY CONTEXT & GEOGRAPHY" = c("Country", "City/Region", "Study Area Size", "Study Area Description", 
                                 "Crime Type", "Crime Types \\(All\\)"),
  
  # Prompt 5: Sampling & Choice Sets
  "SAMPLING & CHOICE SETS" = c("Sampling Approach", "Sample Size", "Number of Crimes Analyzed", "Number of Offenders",
                              "Choice Set Definition", "Alternative Selection", "Sample Restrictions"),
  
  # Prompt 6: Theoretical Framework & Objectives
  "THEORETICAL FRAMEWORK & OBJECTIVES" = c("Theoretical Framework", "Research Objectives", "Literature Gap", "Study Motivation"),
  
  # Prompt 7: Study Design & Methodology
  "STUDY DESIGN & METHODOLOGY" = c("Study Design", "Discrete Choice Model", "Model Specification", "Software Used",
                                  "Estimation Method", "Model Extensions"),
  
  # Prompt 8: Data Preparation & Processing
  "DATA PREPARATION & PROCESSING" = c("Data Cleaning", "Variable Construction", "Missing Data Handling", 
                                     "Data Integration", "Quality Control"),
  
  # Prompt 14: Model Fit & Performance
  "MODEL FIT & PERFORMANCE METRICS" = c("Model Performance", "Information Criteria", "Goodness-of-Fit Tests",
                                       "Model Comparison", "Sample Size Effects", "Convergence Issues"),
  
  # Prompt 15: Major Findings & Results
  "MAJOR FINDINGS & RESULTS" = c("Main Results", "Significant Predictors", "Effect Directions", "Effect Magnitudes",
                                "Surprising Findings", "Robustness Checks"),
  
  # Prompt 16: Scale Effects & Spatial Findings
  "SCALE EFFECTS & SPATIAL FINDINGS" = c("Scale Effects", "Scale Sensitivity", "Spatial Autocorrelation",
                                        "Scale Recommendations", "Scale Limitations", "Cross-Scale Comparisons"),
  
  # Prompt 17: Data Limitations & Methodological Issues
  "DATA LIMITATIONS & METHODOLOGICAL ISSUES" = c("Data Quality Issues", "Missing Data", "Data Source Limitations",
                                                 "Measurement Issues", "Temporal Limitations", "Geographic Limitations",
                                                 "Model Limitations", "Analytical Constraints"),
  
  # Prompt 18: Generalizability & Comparative Limitations
  "GENERALIZABILITY & COMPARATIVE LIMITATIONS" = c("Sample Limitations", "Causal Inference", "Generalizability",
                                                   "Comparative Limitations", "Context Specificity", "Population Constraints"),
  
  # Prompt 19: Implications & Future Directions
  "IMPLICATIONS & FUTURE DIRECTIONS" = c("Theoretical Contributions", "Policy Implications", "Crime Prevention Implications",
                                        "Urban Planning Implications", "Policy Recommendations", "Future Research Directions",
                                        "Spatial Scale Recommendations", "Data Collection Suggestions", 
                                        "Methodological Improvements", "Broader Societal Implications", "Interdisciplinary Connections")
)

# Extract core fields
cat("\n=== EXTRACTING CORE FIELDS ===\n")
for (column_name in names(core_fields)) {
  if (column_name %in% names(df)) {
    cat("Processing:", column_name, "\n")
    
    for (field in core_fields[[column_name]]) {
      clean_field_name <- str_replace_all(field, "[^A-Za-z0-9]", "_")
      clean_field_name <- str_remove_all(clean_field_name, "_+$")
      
      for (i in 1:nrow(df)) {
        text <- df[[column_name]][i]
        if (!is.na(text)) {
          extracted_value <- extract_field_enhanced(text, field)
          result_data[[clean_field_name]][i] <- extracted_value
        }
      }
    }
  }
}

# VARIABLES EXTRACTION with enhanced categorization
cat("\n=== EXTRACTING VARIABLES BY CATEGORY ===\n")

variable_columns <- c(
  "DEMOGRAPHIC & SOCIAL VARIABLES",
  "ECONOMIC VARIABLES", 
  "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES",
  "DISTANCE & ACCESSIBILITY VARIABLES",
  "TEMPORAL & CONTROL VARIABLES"
)

variable_prefixes <- c("DEMO", "ECON", "ENV", "DIST", "TEMP")

# First pass: collect all unique variables
all_variables <- list()
for (i in 1:length(variable_columns)) {
  col_name <- variable_columns[i]
  prefix <- variable_prefixes[i]
  
  if (col_name %in% names(df)) {
    cat("Collecting variables from:", col_name, "\n")
    
    all_vars <- list()
    for (row in 1:nrow(df)) {
      text <- df[[col_name]][row]
      if (!is.na(text)) {
        vars <- extract_variables_enhanced(text)
        all_vars <- c(all_vars, vars)
      }
    }
    
    all_variables[[prefix]] <- unique(names(all_vars))
    cat("  Found", length(unique(names(all_vars))), "unique variables\n")
  }
}

# Create columns for all variables
cat("\n=== CREATING VARIABLE COLUMNS ===\n")
for (prefix in names(all_variables)) {
  for (var_name in all_variables[[prefix]]) {
    col_name <- paste0(prefix, "_", var_name)
    result_data[[col_name]] <- NA
  }
  cat("Created", length(all_variables[[prefix]]), "columns for", prefix, "variables\n")
}

# Second pass: fill variable data
cat("\n=== FILLING VARIABLE DATA ===\n")
for (i in 1:length(variable_columns)) {
  col_name <- variable_columns[i]
  prefix <- variable_prefixes[i]
  
  if (col_name %in% names(df)) {
    cat("Filling data for:", col_name, "\n")
    
    for (row in 1:nrow(df)) {
      text <- df[[col_name]][row]
      if (!is.na(text)) {
        vars <- extract_variables_enhanced(text)
        
        for (var_name in names(vars)) {
          col_name_full <- paste0(prefix, "_", var_name)
          if (col_name_full %in% names(result_data)) {
            result_data[[col_name_full]][row] <- vars[[var_name]]
          }
        }
      }
    }
  }
}

# FINAL DATA CLEANING AND ORGANIZATION
cat("\n=== FINAL DATA CLEANING ===\n")

# Clean all character columns
for (col in names(result_data)) {
  if (is.character(result_data[[col]])) {
    result_data[[col]] <- str_trim(result_data[[col]])
    result_data[[col]][result_data[[col]] == ""] <- NA
    result_data[[col]][str_detect(tolower(result_data[[col]]), "^not\\s+(mentioned|explicitly|available|specified|stated)")] <- NA
  }
}

# Organize columns logically
core_cols <- c("Title", "Year", "Authors", "Journal", "DOI", "Filename", "Country", "City_Region", 
               "Study_Period", "Data_Collection_Period", "Crime_Type", "Study_Area_Size",
               "SUoA_Type", "SUoA_Size", "Sample_Size", "Number_of_Crimes_Analyzed", 
               "Discrete_Choice_Model", "Software_Used")

demo_cols <- names(result_data)[str_detect(names(result_data), "^DEMO_")]
econ_cols <- names(result_data)[str_detect(names(result_data), "^ECON_")]
env_cols <- names(result_data)[str_detect(names(result_data), "^ENV_")]
dist_cols <- names(result_data)[str_detect(names(result_data), "^DIST_")]
temp_cols <- names(result_data)[str_detect(names(result_data), "^TEMP_")]
other_cols <- setdiff(names(result_data), c(core_cols, demo_cols, econ_cols, env_cols, dist_cols, temp_cols))

# Reorder columns
final_column_order <- c(
  intersect(core_cols, names(result_data)),
  sort(other_cols),
  sort(demo_cols),
  sort(econ_cols), 
  sort(env_cols),
  sort(dist_cols),
  sort(temp_cols)
)

result_data <- result_data[, final_column_order]

# Save results
write_csv(result_data, "Output/improved_specialized_extraction.csv")

# Generate summary report
cat("\n=== EXTRACTION SUMMARY REPORT ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Extracted dataset:", nrow(result_data), "rows x", ncol(result_data), "columns\n")
cat("Output saved to: Output/improved_specialized_extraction.csv\n")

# Field extraction success rates
cat("\n=== CORE FIELD EXTRACTION RATES ===\n")
key_fields <- c("Title", "Year", "Authors", "Country", "Study_Period", "Crime_Type", 
               "Discrete_Choice_Model", "Sample_Size", "Main_Results", "SUoA_Type")

for (field in key_fields) {
  if (field %in% names(result_data)) {
    non_na_count <- sum(!is.na(result_data[[field]]))
    success_rate <- round((non_na_count/nrow(result_data))*100, 1)
    cat(sprintf("%-25s: %2d/%d (%5.1f%%)\n", field, non_na_count, nrow(result_data), success_rate))
  }
}

# Variable extraction summary
cat("\n=== VARIABLE EXTRACTION SUMMARY ===\n")
var_summary <- data.frame(
  Category = c("Demographic", "Economic", "Environmental", "Distance", "Temporal"),
  Columns = c(length(demo_cols), length(econ_cols), length(env_cols), length(dist_cols), length(temp_cols))
)
print(var_summary)

cat("\nTotal variable columns:", sum(var_summary$Columns), "\n")
cat("Total columns:", ncol(result_data), "\n")
cat("\n=== IMPROVED EXTRACTION COMPLETED ===\n")
