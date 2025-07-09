# COMPREHENSIVE GROUPED EXTRACTION - ALL DATA AND VARIABLES
# Extracts ALL information from each column, not just variables

library(dplyr)
library(stringr)
library(readr)

# Read the data
df <- read_csv("Output/clean_combined_dataset.csv")

cat("Starting COMPREHENSIVE grouped extraction with ALL data...\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")

# Enhanced function to extract ALL field information from text
extract_all_fields <- function(text, field_names) {
  if (is.na(text) || text == "" || text == "N/A") {
    return(setNames(rep(NA, length(field_names)), field_names))
  }
  
  results <- setNames(rep(NA, length(field_names)), field_names)
  
  for (field_name in field_names) {
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
          results[field_name] <- result
          break
        }
      }
    }
  }
  
  return(results)
}

# Function to extract variables from text and return them as a single string with count
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

# Extract ALL core fields comprehensively
cat("Extracting ALL core fields comprehensively...\n")

for (i in 1:nrow(df)) {
  # 1. BASIC STUDY IDENTIFICATION - Extract ALL fields
  text <- df$`BASIC STUDY IDENTIFICATION`[i]
  if (!is.na(text)) {
    basic_fields <- extract_all_fields(text, c("Year", "Authors", "Journal", "DOI", "Filename"))
    result_data$Year[i] <- basic_fields["Year"]
    result_data$Authors[i] <- basic_fields["Authors"]
    result_data$Journal[i] <- basic_fields["Journal"]
    result_data$DOI[i] <- basic_fields["DOI"]
    result_data$Filename[i] <- basic_fields["Filename"]
  }
  
  # 2. TEMPORAL SCOPE & DATA SOURCES - Extract ALL fields
  text <- df$`TEMPORAL SCOPE & DATA SOURCES`[i]
  if (!is.na(text)) {
    temporal_fields <- extract_all_fields(text, c("Study Period", "Data Collection Period", "Data Sources", "Data Availability"))
    result_data$Study_Period[i] <- temporal_fields["Study Period"]
    result_data$Data_Collection_Period[i] <- temporal_fields["Data Collection Period"]
    result_data$Data_Sources[i] <- temporal_fields["Data Sources"]
    result_data$Data_Availability[i] <- temporal_fields["Data Availability"]
  }
  
  # 3. SPATIAL UNITS - Extract ALL fields including rationale
  text <- df$`SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`[i]
  if (!is.na(text)) {
    spatial_fields <- extract_all_fields(text, c("SUoA Type", "SUoA Size", "SUoA Description", "Number of Units", 
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
  
  # 4. STUDY CONTEXT & GEOGRAPHY - Extract ALL fields
  text <- df$`STUDY CONTEXT & GEOGRAPHY`[i]
  if (!is.na(text)) {
    context_fields <- extract_all_fields(text, c("Country", "City/Region", "Study Area Size", "Study Area Description", 
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
  
  # 5. SAMPLING & CHOICE SETS - Extract ALL fields
  text <- df$`SAMPLING & CHOICE SETS`[i]
  if (!is.na(text)) {
    sampling_fields <- extract_all_fields(text, c("Sample Size", "Number of Crimes Analyzed", "Number of Offenders", 
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
  
  # 6. THEORETICAL FRAMEWORK & OBJECTIVES - Extract ALL fields
  text <- df$`THEORETICAL FRAMEWORK & OBJECTIVES`[i]
  if (!is.na(text)) {
    theory_fields <- extract_all_fields(text, c("Theoretical Framework", "Research Objectives", "Literature Gap", 
                                                "Study Motivation"))
    result_data$Theoretical_Framework[i] <- theory_fields["Theoretical Framework"]
    result_data$Research_Objectives[i] <- theory_fields["Research Objectives"]
    result_data$Literature_Gap[i] <- theory_fields["Literature Gap"]
    result_data$Study_Motivation[i] <- theory_fields["Study Motivation"]
  }
  
  # 7. STUDY DESIGN & METHODOLOGY - Extract ALL fields
  text <- df$`STUDY DESIGN & METHODOLOGY`[i]
  if (!is.na(text)) {
    method_fields <- extract_all_fields(text, c("Study Design", "Discrete Choice Model", "Model Specification", 
                                                "Software Used", "Estimation Method", "Model Extensions"))
    result_data$Study_Design[i] <- method_fields["Study Design"]
    result_data$Discrete_Choice_Model[i] <- method_fields["Discrete Choice Model"]
    result_data$Model_Specification[i] <- method_fields["Model Specification"]
    result_data$Software_Used[i] <- method_fields["Software Used"]
    result_data$Estimation_Method[i] <- method_fields["Estimation Method"]
    result_data$Model_Extensions[i] <- method_fields["Model Extensions"]
  }
  
  # 8. DATA PREPARATION & PROCESSING - Extract ALL fields
  text <- df$`DATA PREPARATION & PROCESSING`[i]
  if (!is.na(text)) {
    data_fields <- extract_all_fields(text, c("Data Cleaning", "Variable Construction", "Missing Data Handling", 
                                              "Data Integration", "Quality Control"))
    result_data$Data_Cleaning[i] <- data_fields["Data Cleaning"]
    result_data$Variable_Construction[i] <- data_fields["Variable Construction"]
    result_data$Missing_Data_Handling[i] <- data_fields["Missing Data Handling"]
    result_data$Data_Integration[i] <- data_fields["Data Integration"]
    result_data$Quality_Control[i] <- data_fields["Quality Control"]
  }
  
  # 9. MODEL FIT & PERFORMANCE METRICS - Extract ALL fields
  text <- df$`MODEL FIT & PERFORMANCE METRICS`[i]
  if (!is.na(text)) {
    performance_fields <- extract_all_fields(text, c("Model Performance", "Information Criteria", "Goodness-of-Fit Tests", 
                                                     "Model Comparison", "Sample Size Effects", "Robustness Checks"))
    result_data$Model_Performance[i] <- performance_fields["Model Performance"]
    result_data$Information_Criteria[i] <- performance_fields["Information Criteria"]
    result_data$Goodness_of_Fit_Tests[i] <- performance_fields["Goodness-of-Fit Tests"]
    result_data$Model_Comparison[i] <- performance_fields["Model Comparison"]
    result_data$Sample_Size_Effects[i] <- performance_fields["Sample Size Effects"]
    result_data$Robustness_Checks[i] <- performance_fields["Robustness Checks"]
  }
  
  # 10. MAJOR FINDINGS & RESULTS - Extract ALL fields
  text <- df$`MAJOR FINDINGS & RESULTS`[i]
  if (!is.na(text)) {
    results_fields <- extract_all_fields(text, c("Main Results", "Significant Predictors", "Effect Directions", 
                                                 "Effect Magnitudes", "Surprising Findings"))
    result_data$Main_Results[i] <- results_fields["Main Results"]
    result_data$Significant_Predictors[i] <- results_fields["Significant Predictors"]
    result_data$Effect_Directions[i] <- results_fields["Effect Directions"]
    result_data$Effect_Magnitudes[i] <- results_fields["Effect Magnitudes"]
    result_data$Surprising_Findings[i] <- results_fields["Surprising Findings"]
  }
  
  # 11. SCALE EFFECTS & SPATIAL FINDINGS - Extract ALL fields
  text <- df$`SCALE EFFECTS & SPATIAL FINDINGS`[i]
  if (!is.na(text)) {
    scale_fields <- extract_all_fields(text, c("Scale Effects", "Scale Sensitivity", "Spatial Autocorrelation", 
                                               "Scale Recommendations", "Scale Limitations", "Cross-Scale Comparisons"))
    result_data$Scale_Effects[i] <- scale_fields["Scale Effects"]
    result_data$Scale_Sensitivity[i] <- scale_fields["Scale Sensitivity"]
    result_data$Spatial_Autocorrelation[i] <- scale_fields["Spatial Autocorrelation"]
    result_data$Scale_Recommendations[i] <- scale_fields["Scale Recommendations"]
    result_data$Scale_Limitations[i] <- scale_fields["Scale Limitations"]
    result_data$Cross_Scale_Comparisons[i] <- scale_fields["Cross-Scale Comparisons"]
  }
  
  # 12. DATA LIMITATIONS & METHODOLOGICAL ISSUES - Extract ALL fields
  text <- df$`DATA LIMITATIONS & METHODOLOGICAL ISSUES`[i]
  if (!is.na(text)) {
    limitations_fields <- extract_all_fields(text, c("Data Quality Issues", "Missing Data", "Data Source Limitations", 
                                                     "Measurement Issues", "Temporal Limitations", "Data Limitations"))
    result_data$Data_Quality_Issues[i] <- limitations_fields["Data Quality Issues"]
    result_data$Missing_Data_Issues[i] <- limitations_fields["Missing Data"]
    result_data$Data_Source_Limitations[i] <- limitations_fields["Data Source Limitations"]
    result_data$Measurement_Issues[i] <- limitations_fields["Measurement Issues"]
    result_data$Temporal_Limitations[i] <- limitations_fields["Temporal Limitations"]
    result_data$Data_Limitations[i] <- limitations_fields["Data Limitations"]
  }
  
  # 13. GENERALIZABILITY & COMPARATIVE LIMITATIONS - Extract ALL fields
  text <- df$`GENERALIZABILITY & COMPARATIVE LIMITATIONS`[i]
  if (!is.na(text)) {
    general_fields <- extract_all_fields(text, c("Generalizability", "Context Specificity", "Comparative Limitations"))
    result_data$Generalizability[i] <- general_fields["Generalizability"]
    result_data$Context_Specificity[i] <- general_fields["Context Specificity"]
    result_data$Comparative_Limitations[i] <- general_fields["Comparative Limitations"]
  }
  
  # 14. IMPLICATIONS & FUTURE DIRECTIONS - Extract ALL fields
  text <- df$`IMPLICATIONS & FUTURE DIRECTIONS`[i]
  if (!is.na(text)) {
    implications_fields <- extract_all_fields(text, c("Theoretical Contributions", "Policy Implications", "Crime Prevention Implications",
                                                      "Urban Planning Implications", "Policy Recommendations", "Future Research Directions",
                                                      "Spatial Scale Recommendations", "Data Collection Suggestions", 
                                                      "Methodological Improvements", "Broader Societal Implications", 
                                                      "Interdisciplinary Connections"))
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

# NOW Extract grouped variables with counts
cat("Extracting grouped variables comprehensively...\n")

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

# Save the comprehensive result
write_csv(result_data, "Output/comprehensive_grouped_extraction.csv")

# Generate comprehensive summary report
cat("\n=== COMPREHENSIVE GROUPED EXTRACTION SUMMARY ===\n")
cat("Original dataset:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Comprehensive dataset:", nrow(result_data), "rows x", ncol(result_data), "columns\n")
cat("Output saved to: Output/comprehensive_grouped_extraction.csv\n")

# Show extraction success rates for key fields
cat("\n=== COMPREHENSIVE FIELD EXTRACTION SUMMARY ===\n")
key_fields <- c("Title", "Year", "Authors", "Country", "Study_Period", "Crime_Type", 
               "Discrete_Choice_Model", "Sample_Size", "Main_Results", "SUoA_Type",
               "Quoted_Rationale", "Rationale_Category", "Justification_Summary",
               "Theoretical_Framework", "Research_Objectives", "Scale_Effects")

for (field in key_fields) {
  if (field %in% names(result_data)) {
    non_na_count <- sum(!is.na(result_data[[field]]))
    success_rate <- round((non_na_count/nrow(result_data))*100, 1)
    cat(sprintf("%-30s: %2d/%d (%5.1f%%)\n", field, non_na_count, nrow(result_data), success_rate))
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

cat("\nCOMPREHENSIVE extraction completed!\n")
cat("ALL data fields and variables extracted with grouping and counts.\n")
