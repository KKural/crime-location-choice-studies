# Enhanced Specialized Prompts Extraction - Following Successful Format
# Based on the working specialized_prompts_extraction.R with variable counting

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# Read the clean combined dataset
df <- read_csv("Output/clean_combined_dataset.csv")

cat("=== ENHANCED SPECIALIZED PROMPTS EXTRACTION ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")

# Initialize result dataframe with Title column
result_data <- data.frame(
  Title = df$Title,
  stringsAsFactors = FALSE
)

# Field extraction function (same as successful version)
extract_field_enhanced <- function(text, field_name) {
  if (is.na(text) || text == "") return(NA)
  
  # Create flexible pattern for field name
  escaped_field <- str_replace_all(field_name, "[^A-Za-z0-9]", "\\\\.")
  
  patterns <- c(
    # Bold markdown format: **Field:** Value
    paste0("\\*\\*", escaped_field, "\\*\\*\\s*:?\\s*([^\\n\\*]+)"),
    # List format: - Field: Value
    paste0("-\\s*", escaped_field, "\\s*:?\\s*([^\\n\\-]+)"),
    # Numbered format: 1. Field: Value
    paste0("\\d+\\.\\s*", escaped_field, "\\s*:?\\s*([^\\n\\d]+)"),
    # Simple colon format: Field: Value
    paste0("\\b", escaped_field, "\\s*:+\\s*([^\\n\\|]+?)(?=\\||\\n|$)"),
    # Pipe format: Field | Value
    paste0("\\b", escaped_field, "\\s*\\|\\s*([^\\n\\|]+)")
  )
  
  for (pattern in patterns) {
    match_result <- str_match(text, regex(pattern, ignore_case = TRUE))
    if (!is.na(match_result[1,1]) && ncol(match_result) > 1) {
      extracted <- match_result[1,2]
      if (!is.na(extracted)) {
        # Clean the extracted value
        cleaned <- str_trim(extracted)
        cleaned <- str_remove_all(cleaned, "^[\\*\\-\\d\\.\\s]+")
        cleaned <- str_remove_all(cleaned, "[\\*]{2,}")
        if (nchar(cleaned) > 0 && cleaned != field_name) {
          return(cleaned)
        }
      }
    }
  }
  return(NA)
}

# Variable collection function (simplified from successful version)
collect_variables_from_column <- function(column_data, category_prefix) {
  all_variables <- character()
  
  for (text in column_data) {
    if (is.na(text) || text == "") next
    
    # Enhanced patterns for variable extraction
    patterns <- c(
      # **Variable:** Description | Unit | Source
      "\\*\\*([^\\*\\|\\n]+?)\\*\\*\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
      # - Variable: Description | Unit | Source  
      "-\\s*([^:\\|\\n]+?)\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
      # Variable | Description | Unit | Source
      "([^\\|\\n]+?)\\s*\\|\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
      # Numbered lists
      "\\d+\\.\\s*([^:\\|\\n]+?)\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?"
    )
    
    for (pattern in patterns) {
      matches <- str_match_all(text, regex(pattern, ignore_case = TRUE))[[1]]
      
      if (nrow(matches) > 0) {
        for (i in 1:nrow(matches)) {
          var_name <- str_trim(matches[i, 2])
          var_desc <- if(ncol(matches) > 2 && !is.na(matches[i, 3])) str_trim(matches[i, 3]) else ""
          var_unit <- if(ncol(matches) > 3 && !is.na(matches[i, 4])) str_trim(matches[i, 4]) else ""
          var_source <- if(ncol(matches) > 4 && !is.na(matches[i, 5])) str_trim(matches[i, 5]) else ""
          
          # Clean variable name
          var_name <- str_remove_all(var_name, "[\\*\\-\\d\\.\\s]*$")
          var_name <- str_remove_all(var_name, "^[\\*\\-\\d\\.\\s]+")
          var_name <- str_trim(var_name)
          
          if (nchar(var_name) > 2 && !str_detect(var_name, "^(and|or|the|a|an|is|are|was|were|will|would|could|should)$")) {
            # Create clean variable name
            clean_name <- str_replace_all(var_name, "[^A-Za-z0-9]", "_")
            clean_name <- str_remove_all(clean_name, "_+$")
            clean_name <- str_remove_all(clean_name, "^_+")
            
            full_var_name <- paste0(category_prefix, "_", clean_name)
            all_variables <- c(all_variables, full_var_name)
            
            # Also store metadata variations
            if (nchar(var_desc) > 0) {
              all_variables <- c(all_variables, paste0(full_var_name, "_Description"))
            }
            if (nchar(var_unit) > 0) {
              all_variables <- c(all_variables, paste0(full_var_name, "_Unit"))  
            }
            if (nchar(var_source) > 0) {
              all_variables <- c(all_variables, paste0(full_var_name, "_Source"))
            }
            if (nchar(var_desc) > 0) {
              all_variables <- c(all_variables, paste0(full_var_name, "_Info"))
            }
          }
        }
      }
    }
  }
  
  return(unique(all_variables))
}

# Core fields mapping (exactly like successful version)
core_fields <- list(
  # Prompt 1: Basic Study Identification
  "BASIC STUDY IDENTIFICATION" = c("Title", "Year", "Authors", "Journal", "DOI", "Filename"),
  
  # Prompt 2: Temporal Scope & Data Sources  
  "TEMPORAL SCOPE & DATA SOURCES" = c("Country", "City_Region", "Study_Period", "Data_Collection_Period", 
                                     "Crime_Type", "Data_Sources"),
  
  # Prompt 3: Spatial Units
  "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION" = c("Study_Area_Size", "SUoA_Type", "SUoA_Size", "SUoA_Description",
                                                   "Population_per_Unit", "Number_of_Units", "Quoted_Rationale", 
                                                   "Rationale_Category", "Justification_Summary"),
  
  # Prompt 4: Study Context & Geography
  "STUDY CONTEXT & GEOGRAPHY" = c("Study_Area_Description", "Geographic_Limitations", "Population_Constraints"),
  
  # Prompt 5: Sampling & Choice Sets
  "SAMPLING & CHOICE SETS" = c("Sample_Size", "Number_of_Crimes_Analyzed", "Number_of_Offenders", 
                              "Sampling_Approach", "Choice_Set_Definition", "Sample_Restrictions", 
                              "Sample_Limitations"),
  
  # Prompt 6: Theoretical Framework
  "THEORETICAL FRAMEWORK & OBJECTIVES" = c("Theoretical_Framework", "Research_Objectives", "Literature_Gap", 
                                         "Study_Motivation"),
  
  # Prompt 7: Study Design & Methodology
  "STUDY DESIGN & METHODOLOGY" = c("Study_Design", "Discrete_Choice_Model", "Estimation_Method", 
                                 "Software_Used", "Alternative_Selection"),
  
  # Prompt 8: Data Preparation
  "DATA PREPARATION & PROCESSING" = c("Data_Cleaning", "Data_Integration", "Missing_Data_Handling", 
                                    "Variable_Construction", "Quality_Control"),
  
  # Prompt 9: Model Fit & Performance
  "MODEL FIT & PERFORMANCE METRICS" = c("Model_Performance", "Goodness_of_Fit_Tests", "Information_Criteria", 
                                      "Model_Comparison", "Robustness_Checks"),
  
  # Prompt 10: Major Findings
  "MAJOR FINDINGS & RESULTS" = c("Main_Results", "Significant_Predictors", "Effect_Directions", 
                                "Effect_Magnitudes", "Surprising_Findings"),
  
  # Prompt 11: Scale Effects
  "SCALE EFFECTS & SPATIAL FINDINGS" = c("Scale_Effects", "Cross_Scale_Comparisons", "Scale_Sensitivity", 
                                       "Sample_Size_Effects", "Spatial_Autocorrelation"),
  
  # Prompt 12: Data Limitations
  "DATA LIMITATIONS & METHODOLOGICAL ISSUES" = c("Data_Limitations", "Data_Quality_Issues", "Data_Source_Limitations", 
                                                "Data_Availability", "Measurement_Issues", "Missing_Data"),
  
  # Prompt 13: Generalizability
  "GENERALIZABILITY & COMPARATIVE LIMITATIONS" = c("Generalizability", "Context_Specificity", "Comparative_Limitations", 
                                                  "Geographic_Limitations", "Temporal_Limitations", "Scale_Limitations"),
  
  # Prompt 14: Implications & Future Directions
  "IMPLICATIONS & FUTURE DIRECTIONS" = c("Theoretical_Contributions", "Policy_Implications", "Crime_Prevention_Implications",
                                       "Urban_Planning_Implications", "Policy_Recommendations", "Future_Research_Directions",
                                       "Spatial_Scale_Recommendations", "Data_Collection_Suggestions", 
                                       "Methodological_Improvements", "Broader_Societal_Implications", 
                                       "Interdisciplinary_Connections")
)

# Extract core fields (exactly like successful version)
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

# VARIABLES EXTRACTION (exactly like successful version)
cat("\n=== EXTRACTING VARIABLES BY CATEGORY ===\n")

variable_categories <- list(
  "DEMOGRAPHIC & SOCIAL VARIABLES" = "DEMO",
  "ECONOMIC VARIABLES" = "ECON", 
  "ENVIRONMENTAL & CRIME ATTRACTOR VARIABLES" = "ENV",
  "DISTANCE & ACCESSIBILITY VARIABLES" = "DIST",
  "TEMPORAL & CONTROL VARIABLES" = "TEMP"
)

# Initialize tracking variables
category_stats <- data.frame(
  Category = character(),
  Variables_Found = integer(),
  Columns_Created = integer(),
  stringsAsFactors = FALSE
)

all_unique_variables <- list()

for (category_name in names(variable_categories)) {
  prefix <- variable_categories[[category_name]]
  cat("Collecting variables from:", category_name, "\n")
  
  if (category_name %in% names(df)) {
    unique_vars <- collect_variables_from_column(df[[category_name]], prefix)
    all_unique_variables[[prefix]] <- unique_vars
    
    # Count base variables (without _Description, _Unit, _Source, _Info suffixes)
    base_vars <- unique_vars[!str_detect(unique_vars, "_(Description|Unit|Source|Info)$")]
    
    cat("  Found", length(unique_vars), "unique variables\n")
    
    # Add to tracking
    category_stats <- rbind(category_stats, data.frame(
      Category = category_name,
      Variables_Found = length(unique_vars),
      Columns_Created = length(unique_vars),
      stringsAsFactors = FALSE
    ))
  }
}

# Create variable columns (like successful version)
cat("\n=== CREATING VARIABLE COLUMNS ===\n")
for (prefix in names(all_unique_variables)) {
  unique_vars <- all_unique_variables[[prefix]]
  
  for (var_name in unique_vars) {
    # Initialize column with NA values
    result_data[[var_name]] <- rep(NA, nrow(df))
  }
  
  cat("Created", length(unique_vars), "columns for", prefix, "variables\n")
}

# Fill variable data (like successful version)
cat("\n=== FILLING VARIABLE DATA ===\n")
for (category_name in names(variable_categories)) {
  prefix <- variable_categories[[category_name]]
  cat("Filling data for:", category_name, "\n")
  
  if (category_name %in% names(df)) {
    for (i in 1:nrow(df)) {
      text <- df[[category_name]][i]
      if (!is.na(text)) {
        # Enhanced patterns for variable extraction and filling
        patterns <- c(
          # **Variable:** Description | Unit | Source
          "\\*\\*([^\\*\\|\\n]+?)\\*\\*\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
          # - Variable: Description | Unit | Source  
          "-\\s*([^:\\|\\n]+?)\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
          # Variable | Description | Unit | Source
          "([^\\|\\n]+?)\\s*\\|\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?",
          # Numbered lists
          "\\d+\\.\\s*([^:\\|\\n]+?)\\s*:?\\s*([^\\|\\n]*?)(?:\\s*\\|\\s*([^\\|\\n]*?))?(?:\\s*\\|\\s*([^\\|\\n]*?))?"
        )
        
        for (pattern in patterns) {
          matches <- str_match_all(text, regex(pattern, ignore_case = TRUE))[[1]]
          
          if (nrow(matches) > 0) {
            for (j in 1:nrow(matches)) {
              var_name <- str_trim(matches[j, 2])
              var_desc <- if(ncol(matches) > 2 && !is.na(matches[j, 3])) str_trim(matches[j, 3]) else ""
              var_unit <- if(ncol(matches) > 3 && !is.na(matches[j, 4])) str_trim(matches[j, 4]) else ""
              var_source <- if(ncol(matches) > 4 && !is.na(matches[j, 5])) str_trim(matches[j, 5]) else ""
              
              # Clean variable name
              var_name <- str_remove_all(var_name, "[\\*\\-\\d\\.\\s]*$")
              var_name <- str_remove_all(var_name, "^[\\*\\-\\d\\.\\s]+")
              var_name <- str_trim(var_name)
              
              if (nchar(var_name) > 2) {
                # Create clean variable name
                clean_name <- str_replace_all(var_name, "[^A-Za-z0-9]", "_")
                clean_name <- str_remove_all(clean_name, "_+$")
                clean_name <- str_remove_all(clean_name, "^_+")
                
                full_var_name <- paste0(prefix, "_", clean_name)
                
                # Fill main variable column
                if (full_var_name %in% names(result_data)) {
                  if (nchar(var_desc) > 0) {
                    result_data[[full_var_name]][i] <- var_desc
                  } else {
                    result_data[[full_var_name]][i] <- var_name
                  }
                }
                
                # Fill metadata columns
                if (nchar(var_desc) > 0 && paste0(full_var_name, "_Description") %in% names(result_data)) {
                  result_data[[paste0(full_var_name, "_Description")]][i] <- var_desc
                }
                if (nchar(var_unit) > 0 && paste0(full_var_name, "_Unit") %in% names(result_data)) {
                  result_data[[paste0(full_var_name, "_Unit")]][i] <- var_unit
                }
                if (nchar(var_source) > 0 && paste0(full_var_name, "_Source") %in% names(result_data)) {
                  result_data[[paste0(full_var_name, "_Source")]][i] <- var_source
                }
                if (nchar(var_desc) > 0 && paste0(full_var_name, "_Info") %in% names(result_data)) {
                  result_data[[paste0(full_var_name, "_Info")]][i] <- var_desc
                }
              }
            }
          }
        }
      }
    }
  }
}

# Final data cleaning (exactly like successful version)
cat("\n=== FINAL DATA CLEANING ===\n")
for (col in names(result_data)) {
  if (is.character(result_data[[col]])) {
    result_data[[col]] <- str_trim(result_data[[col]])
    result_data[[col]][result_data[[col]] == ""] <- NA
    result_data[[col]][str_detect(tolower(result_data[[col]]), "^not\\s+(mentioned|explicitly|available|specified|stated)")] <- NA
  }
}

# Organize columns logically (exactly like successful version)
core_cols <- names(result_data)[!str_detect(names(result_data), "^(DEMO|ECON|ENV|DIST|TEMP)_")]
demo_cols <- names(result_data)[str_detect(names(result_data), "^DEMO_")]
econ_cols <- names(result_data)[str_detect(names(result_data), "^ECON_")]
env_cols <- names(result_data)[str_detect(names(result_data), "^ENV_")]
dist_cols <- names(result_data)[str_detect(names(result_data), "^DIST_")]
temp_cols <- names(result_data)[str_detect(names(result_data), "^TEMP_")]

final_column_order <- c(
  core_cols,
  sort(demo_cols),
  sort(econ_cols), 
  sort(env_cols),
  sort(dist_cols),
  sort(temp_cols)
)

result_data <- result_data[, final_column_order]

# Save results
write_csv(result_data, "Output/enhanced_specialized_extraction.csv")

# Generate comprehensive summary report with detailed variable counting
cat("\n=== COMPREHENSIVE EXTRACTION SUMMARY REPORT ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Enhanced dataset:", nrow(result_data), "rows x", ncol(result_data), "columns\n")
cat("Output saved to: Output/enhanced_specialized_extraction.csv\n")

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

# Calculate detailed variable statistics
cat("\n=== DETAILED VARIABLE EXTRACTION SUMMARY ===\n")

# Recalculate category statistics
demo_total <- length(demo_cols)
econ_total <- length(econ_cols)
env_total <- length(env_cols)
dist_total <- length(dist_cols)
temp_total <- length(temp_cols)

# Count base variables (without metadata suffixes)
demo_base <- length(demo_cols[!str_detect(demo_cols, "_(Description|Unit|Source|Info)$")])
econ_base <- length(econ_cols[!str_detect(econ_cols, "_(Description|Unit|Source|Info)$")])
env_base <- length(env_cols[!str_detect(env_cols, "_(Description|Unit|Source|Info)$")])
dist_base <- length(dist_cols[!str_detect(dist_cols, "_(Description|Unit|Source|Info)$")])
temp_base <- length(temp_cols[!str_detect(temp_cols, "_(Description|Unit|Source|Info)$")])

# Create summary table
enhanced_stats <- data.frame(
  Category = c("Demographic", "Economic", "Environmental", "Distance", "Temporal"),
  Base_Variables = c(demo_base, econ_base, env_base, dist_base, temp_base),
  Total_Columns = c(demo_total, econ_total, env_total, dist_total, temp_total),
  Metadata_Columns = c(demo_total - demo_base, econ_total - econ_base, 
                      env_total - env_base, dist_total - dist_base, temp_total - temp_base)
)

print(enhanced_stats)

# Calculate grand totals
total_base_variables <- sum(enhanced_stats$Base_Variables)
total_variable_columns <- sum(enhanced_stats$Total_Columns)
total_metadata_columns <- sum(enhanced_stats$Metadata_Columns)
total_core_columns <- length(core_cols)

cat("\n=== FINAL COMPREHENSIVE TOTALS ===\n")
cat("Total base variables found:", total_base_variables, "\n")
cat("Total variable columns (with metadata):", total_variable_columns, "\n")
cat("Total metadata columns:", total_metadata_columns, "\n")
cat("Total core columns:", total_core_columns, "\n")
cat("GRAND TOTAL DATASET COLUMNS:", ncol(result_data), "\n")

# Breakdown by category with percentages
cat("\n=== VARIABLE BREAKDOWN BY CATEGORY ===\n")
cat(sprintf("Demographic (DEMO_): %d base variables, %d total columns (%.1f%%)\n", 
           demo_base, demo_total, (demo_total/ncol(result_data))*100))
cat(sprintf("Economic (ECON_): %d base variables, %d total columns (%.1f%%)\n", 
           econ_base, econ_total, (econ_total/ncol(result_data))*100))
cat(sprintf("Environmental (ENV_): %d base variables, %d total columns (%.1f%%)\n", 
           env_base, env_total, (env_total/ncol(result_data))*100))
cat(sprintf("Distance (DIST_): %d base variables, %d total columns (%.1f%%)\n", 
           dist_base, dist_total, (dist_total/ncol(result_data))*100))
cat(sprintf("Temporal (TEMP_): %d base variables, %d total columns (%.1f%%)\n", 
           temp_base, temp_total, (temp_total/ncol(result_data))*100))

# Sample variables from each category
cat("\n=== SAMPLE VARIABLES BY CATEGORY ===\n")
if (length(demo_cols) > 0) {
  cat("Sample demographic variables:\n")
  cat(paste(head(demo_cols[!str_detect(demo_cols, "_(Description|Unit|Source|Info)$")], 5), collapse = ", "), "\n")
}
if (length(econ_cols) > 0) {
  cat("Sample economic variables:\n")
  cat(paste(head(econ_cols[!str_detect(econ_cols, "_(Description|Unit|Source|Info)$")], 5), collapse = ", "), "\n")
}
if (length(env_cols) > 0) {
  cat("Sample environmental variables:\n")
  cat(paste(head(env_cols[!str_detect(env_cols, "_(Description|Unit|Source|Info)$")], 5), collapse = ", "), "\n")
}
if (length(dist_cols) > 0) {
  cat("Sample distance variables:\n")
  cat(paste(head(dist_cols[!str_detect(dist_cols, "_(Description|Unit|Source|Info)$")], 5), collapse = ", "), "\n")
}
if (length(temp_cols) > 0) {
  cat("Sample temporal variables:\n")
  cat(paste(head(temp_cols[!str_detect(temp_cols, "_(Description|Unit|Source|Info)$")], 5), collapse = ", "), "\n")
}

cat("\n=== ENHANCED SPECIALIZED EXTRACTION COMPLETED ===\n")
cat("Dataset ready for systematic review analysis!\n")
cat("Total studies processed:", nrow(result_data), "\n")
cat("Total variables extracted:", total_base_variables, "\n")
cat("Total columns created:", ncol(result_data), "\n")
