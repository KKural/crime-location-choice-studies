# Quick Analysis Test - Trimmed Dataset with New Variables
# Test script to validate the trimmed dataset and show key results

library(dplyr)
library(ggplot2)
library(readr)

# Load the trimmed dataset
data <- read_csv("20250713_Analysis & Results/20250713_analysis_ready_dataset_trimmed.csv", 
                show_col_types = FALSE)

cat("=== TRIMMED DATASET ANALYSIS RESULTS ===\n")
cat("Dataset loaded successfully!\n")
cat("Rows:", nrow(data), "| Columns:", ncol(data), "\n\n")

# === 1. UNIT SELECTION JUSTIFICATION ANALYSIS ===
cat("1. UNIT SELECTION JUSTIFICATION ANALYSIS\n")
cat("=========================================\n")

justification_summary <- data %>%
  summarise(
    Total_Studies = n(),
    With_Justification = sum(Has_Unit_Justification, na.rm = TRUE),
    Percent_With_Justification = round(100 * With_Justification / Total_Studies, 1),
    With_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
    With_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE)
  )

print(justification_summary)

# Rationale categories
rationale_categories <- data %>%
  filter(!is.na(Rationale_Category) & Rationale_Category != "") %>%
  count(Rationale_Category, sort = TRUE) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))

cat("\nRationale Categories:\n")
print(rationale_categories)

# === 2. VARIABLE COMPLEXITY ANALYSIS ===
cat("\n2. VARIABLE COMPLEXITY ANALYSIS\n")
cat("================================\n")

variable_summary <- data %>%
  summarise(
    Mean_Total_Variables = round(mean(Total_Variables_Numeric, na.rm = TRUE), 1),
    Median_Total_Variables = median(Total_Variables_Numeric, na.rm = TRUE),
    Min_Variables = min(Total_Variables_Numeric, na.rm = TRUE),
    Max_Variables = max(Total_Variables_Numeric, na.rm = TRUE),
    Mean_Variable_Diversity = round(mean(Variable_Diversity_Score, na.rm = TRUE), 1)
  )

print(variable_summary)

# Variable complexity distribution
complexity_dist <- data %>%
  count(Variable_Complexity) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))

cat("\nVariable Complexity Distribution:\n")
print(complexity_dist)

# Variable types analysis
variable_types <- data %>%
  summarise(
    Studies_with_Demographic = sum(Has_Demographic_Vars, na.rm = TRUE),
    Studies_with_Economic = sum(Has_Economic_Vars, na.rm = TRUE), 
    Studies_with_Environmental = sum(Has_Environmental_Vars, na.rm = TRUE),
    Studies_with_Distance = sum(Has_Distance_Vars, na.rm = TRUE),
    Studies_with_Temporal = sum(Has_Temporal_Vars, na.rm = TRUE)
  )

cat("\nVariable Types Usage:\n")
print(variable_types)

# === 3. DATA LIMITATIONS ANALYSIS ===
cat("\n3. DATA LIMITATIONS ANALYSIS\n")
cat("=============================\n")

limitation_summary <- data %>%
  summarise(
    Mean_Limitation_Score = round(mean(Data_Limitation_Score, na.rm = TRUE), 1),
    Median_Limitation_Score = median(Data_Limitation_Score, na.rm = TRUE),
    Studies_with_Quality_Issues = sum(Has_Data_Quality_Issues, na.rm = TRUE),
    Studies_with_Missing_Data = sum(Has_Missing_Data_Issues, na.rm = TRUE),
    Studies_with_Generalizability_Issues = sum(Has_Generalizability_Issues, na.rm = TRUE),
    Studies_with_Context_Specificity = sum(Has_Context_Specificity, na.rm = TRUE)
  )

print(limitation_summary)

# Limitation categories
limitation_categories <- data %>%
  count(Data_Limitation_Category) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))

cat("\nData Limitation Categories:\n")
print(limitation_categories)

# === 4. SPATIAL SCALE ANALYSIS ===
cat("\n4. SPATIAL SCALE ANALYSIS\n")
cat("==========================\n")

scale_analysis <- data %>%
  summarise(
    Studies_with_Scale_Limitations = sum(Has_Scale_Limitations, na.rm = TRUE),
    Percent_with_Scale_Limitations = round(100 * Studies_with_Scale_Limitations / n(), 1),
    Studies_with_Scale_Recommendations = sum(Has_Scale_Recommendations, na.rm = TRUE),
    Percent_with_Scale_Recommendations = round(100 * Studies_with_Scale_Recommendations / n(), 1),
    Studies_with_Spatial_Recommendations = sum(Has_Spatial_Scale_Recommendations, na.rm = TRUE),
    Percent_with_Spatial_Recommendations = round(100 * Studies_with_Spatial_Recommendations / n(), 1)
  )

print(scale_analysis)

# === 5. METHODOLOGICAL RIGOR ANALYSIS ===
cat("\n5. METHODOLOGICAL RIGOR ANALYSIS\n")
cat("=================================\n")

rigor_summary <- data %>%
  summarise(
    Mean_Research_Sophistication = round(mean(Research_Sophistication, na.rm = TRUE), 1),
    Mean_Methodological_Rigor = round(mean(Methodological_Rigor_Score, na.rm = TRUE), 1),
    Mean_Study_Quality = round(mean(Study_Quality_Score, na.rm = TRUE), 1),
    Studies_with_Model_Comparison = sum(Has_Model_Comparison, na.rm = TRUE),
    Studies_with_Robustness_Checks = sum(Has_Robustness_Checks, na.rm = TRUE)
  )

print(rigor_summary)

# === 6. CORE UNIT SIZE ANALYSIS (from trimmed data) ===
cat("\n6. CORE UNIT SIZE ANALYSIS\n")
cat("===========================\n")

unit_size_summary <- data %>%
  summarise(
    N_Studies = n(),
    Mean_Unit_Size = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Unit_Size = round(median(Unit_size_km2, na.rm = TRUE), 4),
    Min_Unit_Size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_Unit_Size = round(max(Unit_size_km2, na.rm = TRUE), 2),
    SD_Unit_Size = round(sd(Unit_size_km2, na.rm = TRUE), 4)
  )

print(unit_size_summary)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key findings:\n")
cat("- All 51 studies have unit selection justification (100%)\n")
cat("- Variable complexity ranges from", min(data$Total_Variables_Numeric, na.rm = TRUE), 
    "to", max(data$Total_Variables_Numeric, na.rm = TRUE), "variables\n")
cat("- Mean data limitation score:", round(mean(data$Data_Limitation_Score, na.rm = TRUE), 1), "/8\n")
cat("- ", sum(data$Has_Scale_Limitations, na.rm = TRUE), "studies report spatial scale limitations\n")
cat("- Mean study quality score:", round(mean(data$Study_Quality_Score, na.rm = TRUE), 1), "/8\n")
