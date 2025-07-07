# Research Questions Analysis Script
# Analyzing SUoA (Spatial Unit of Analysis) characteristics across studies

library(tidyverse)
library(stringr)
library(here)
library(readr)

# Load the merged dataset
data <- read_csv(here("Output", "merged_comprehensive_unit_sizes.csv"), show_col_types = FALSE)

# Convert Publication_Year to numeric for analysis
data <- data %>%
  mutate(Publication_Year = as.numeric(Publication_Year))

# Create country/jurisdiction groupings from citation and title
data <- data %>%
  mutate(
    # Extract country from citation and title
    text_to_search = paste(Citation, Title_of_the_study),
    Jurisdiction = case_when(
      str_detect(text_to_search, "Netherlands|Dutch|The Hague|Amsterdam") ~ "Netherlands",
      str_detect(text_to_search, "Australia|Australian|Brisbane|Perth|Melbourne|Sydney") ~ "Australia", 
      str_detect(text_to_search, "Belgium|Belgian|Flanders|Brussels|Ghent") ~ "Belgium",
      str_detect(text_to_search, "United Kingdom|UK|Britain|British|London|England") ~ "United Kingdom",
      str_detect(text_to_search, "United States|USA|US|American|Chicago|New York|California") ~ "United States",
      str_detect(text_to_search, "Canada|Canadian|Toronto|Vancouver") ~ "Canada",
      str_detect(text_to_search, "China|Chinese|Beijing|Shanghai|ZG city") ~ "China",
      str_detect(text_to_search, "Japan|Japanese|Tokyo") ~ "Japan",
      str_detect(text_to_search, "Germany|German|Berlin") ~ "Germany",
      str_detect(text_to_search, "France|French|Paris") ~ "France",
      str_detect(text_to_search, "Italy|Italian|Rome") ~ "Italy",
      str_detect(text_to_search, "Spain|Spanish|Madrid") ~ "Spain",
      TRUE ~ "Other"
    ),
    # Create Anglo-Saxon vs Other grouping
    Anglo_Saxon = case_when(
      Jurisdiction %in% c("United States", "United Kingdom", "Canada", "Australia") ~ "Anglo-Saxon",
      TRUE ~ "Other"
    )
  )

# Extract crime type from title (simplified approach)
data <- data %>%
  mutate(
    Crime_Type = case_when(
      str_detect(tolower(Title_of_the_study), "burglary") ~ "Burglary",
      str_detect(tolower(Title_of_the_study), "robbery") ~ "Robbery", 
      str_detect(tolower(Title_of_the_study), "theft") ~ "Theft",
      str_detect(tolower(Title_of_the_study), "graffiti") ~ "Graffiti",
      str_detect(tolower(Title_of_the_study), "drug") ~ "Drug Crime",
      str_detect(tolower(Title_of_the_study), "assault") ~ "Assault",
      str_detect(tolower(Title_of_the_study), "crime") ~ "General Crime",
      TRUE ~ "Other"
    )
  )

cat("=== RESEARCH QUESTIONS ANALYSIS ===\n\n")

# ==================================================================================
# RQ1: What is the distribution of the size of SUoA across the N selected studies?
# ==================================================================================

cat("1. DISTRIBUTION OF SUoA SIZE ACROSS STUDIES\n")
cat("============================================\n")

# Summary statistics
summary_stats <- summary(data$Unit_size_km2)
cat("Summary Statistics of Unit Size (km²):\n")
print(summary_stats)

cat("\nPercentiles:\n")
percentiles <- quantile(data$Unit_size_km2, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
print(percentiles)

cat("\nDistribution by Size Group:\n")
size_group_dist <- table(data$Size_group)
print(size_group_dist)
cat("Proportions:\n")
print(prop.table(size_group_dist))

cat("\n")

# ==================================================================================
# RQ2: Does the size of the SUoA change over calendar time?
# ==================================================================================

cat("2. SUoA SIZE CHANGE OVER TIME\n")
cat("=============================\n")

# Correlation between year and unit size
correlation_year_size <- cor(data$Publication_Year, data$Unit_size_km2, use = "complete.obs")
cat("Correlation between Publication Year and Unit Size (km²):", round(correlation_year_size, 4), "\n")

# Regression analysis
time_model <- lm(log(Unit_size_km2) ~ Publication_Year, data = data)
cat("\nRegression: log(Unit_size_km2) ~ Publication_Year\n")
cat("Coefficient for Publication_Year:", round(coef(time_model)[2], 6), "\n")
cat("P-value:", round(summary(time_model)$coefficients[2,4], 4), "\n")
cat("R-squared:", round(summary(time_model)$r.squared, 4), "\n")

# Mean unit size by decade
data$Decade <- floor(data$Publication_Year / 10) * 10
decade_summary <- data %>%
  group_by(Decade) %>%
  summarise(
    n = n(),
    mean_size_km2 = mean(Unit_size_km2, na.rm = TRUE),
    median_size_km2 = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  )
cat("\nMean Unit Size by Decade:\n")
print(decade_summary)

cat("\n")

# ==================================================================================
# RQ3: Does the size of the SUoA vary by jurisdiction?
# ==================================================================================

cat("3. SUoA SIZE BY JURISDICTION\n")
cat("============================\n")

# Summary by jurisdiction
jurisdiction_summary <- data %>%
  group_by(Jurisdiction) %>%
  summarise(
    n = n(),
    mean_size_km2 = mean(Unit_size_km2, na.rm = TRUE),
    median_size_km2 = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(mean_size_km2)

cat("Unit Size by Jurisdiction:\n")
print(jurisdiction_summary)

# Anglo-Saxon vs Other comparison
anglo_comparison <- data %>%
  group_by(Anglo_Saxon) %>%
  summarise(
    n = n(),
    mean_size_km2 = mean(Unit_size_km2, na.rm = TRUE),
    median_size_km2 = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nAnglo-Saxon vs Other Countries:\n")
print(anglo_comparison)

# Statistical test (only if we have both groups)
if(length(unique(data$Anglo_Saxon)) > 1) {
  anglo_test <- t.test(Unit_size_km2 ~ Anglo_Saxon, data = data)
  cat("\nT-test (Anglo-Saxon vs Other):\n")
  cat("P-value:", round(anglo_test$p.value, 4), "\n")
} else {
  cat("\nNote: Only one jurisdiction group found, cannot perform t-test\n")
}

cat("\n")

# ==================================================================================
# RQ4: Does the size of the SUoA vary by crime type studied?
# ==================================================================================

cat("4. SUoA SIZE BY CRIME TYPE\n")
cat("==========================\n")

crime_summary <- data %>%
  group_by(Crime_Type) %>%
  summarise(
    n = n(),
    mean_size_km2 = mean(Unit_size_km2, na.rm = TRUE),
    median_size_km2 = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(mean_size_km2)

cat("Unit Size by Crime Type:\n")
print(crime_summary)

cat("\n")

# ==================================================================================
# RQ5: Does the size of the SUoA vary by size of the total study area?
# ==================================================================================

cat("5. SUoA SIZE vs TOTAL STUDY AREA\n")
cat("================================\n")

# Calculate total study area (approximate)
data <- data %>%
  mutate(
    No_of_units_numeric = as.numeric(str_replace_all(No_of_units, "[^0-9.]", "")),
    Total_study_area_km2 = Unit_size_km2 * No_of_units_numeric
  )

# Correlation between total study area and unit size
correlation_areas <- cor(data$Total_study_area_km2, data$Unit_size_km2, use = "complete.obs")
cat("Correlation between Total Study Area and Unit Size:", round(correlation_areas, 4), "\n")

# Regression
area_model <- lm(log(Unit_size_km2) ~ log(Total_study_area_km2), data = data)
cat("\nRegression: log(Unit_size_km2) ~ log(Total_study_area_km2)\n")
cat("Coefficient:", round(coef(area_model)[2], 4), "\n")
cat("P-value:", round(summary(area_model)$coefficients[2,4], 4), "\n")
cat("R-squared:", round(summary(area_model)$r.squared, 4), "\n")

cat("\n")

# ==================================================================================
# RQ6: Is the size of the unit of analysis related to discrete choice methods used?
# ==================================================================================

cat("6. SUoA SIZE vs METHODOLOGICAL CHOICES\n")
cat("======================================\n")

# Extract method information from titles/text (simplified)
data <- data %>%
  mutate(
    Has_Logit = str_detect(tolower(Title_of_the_study), "logit|choice"),
    Has_Sampling = str_detect(tolower(Title_of_the_study), "sampl"),
    Has_Mixed_Model = str_detect(tolower(Title_of_the_study), "mixed|multilevel|hierarchical")
  )

# Method vs unit size
method_summary <- data %>%
  group_by(Has_Logit) %>%
  summarise(
    n = n(),
    mean_size_km2 = mean(Unit_size_km2, na.rm = TRUE),
    median_size_km2 = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  )

cat("Unit Size by Method (Logit/Choice models):\n")
print(method_summary)

cat("\n")

# ==================================================================================
# RQ7: Is the size of the SUoA related to the type of independent variables included?
# ==================================================================================

cat("7. SUoA SIZE vs VARIABLE TYPES\n")
cat("==============================\n")

# Correlation between unit size and number of variables by category
variable_correlations <- data %>%
  select(Unit_size_km2, total_variables_count, distance_access, infrastructure, 
         economic, land_use, social_behavioral, environmental, demographic, temporal_control) %>%
  cor(use = "complete.obs")

cat("Correlations between Unit Size and Variable Counts:\n")
print(round(variable_correlations[1, -1], 3))

# Variable density (variables per km²)
data <- data %>%
  mutate(Variable_density = total_variables_count / Unit_size_km2)

cat("\nVariable Density Statistics:\n")
print(summary(data$Variable_density))

# Regression: total variables ~ unit size
var_model <- lm(total_variables_count ~ log(Unit_size_km2), data = data)
cat("\nRegression: total_variables_count ~ log(Unit_size_km2)\n")
cat("Coefficient:", round(coef(var_model)[2], 4), "\n")
cat("P-value:", round(summary(var_model)$coefficients[2,4], 4), "\n")
cat("R-squared:", round(summary(var_model)$r.squared, 4), "\n")

cat("\n")

# ==================================================================================
# SUMMARY OF FINDINGS
# ==================================================================================

cat("SUMMARY OF KEY FINDINGS\n")
cat("=======================\n")

cat("1. Unit sizes range from", round(min(data$Unit_size_km2, na.rm = TRUE), 6), 
    "to", round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²\n")

cat("2. Temporal trend: Unit sizes are", 
    ifelse(correlation_year_size < 0, "DECREASING", "INCREASING"), 
    "over time (r =", round(correlation_year_size, 3), ")\n")

cat("3. Jurisdiction: Anglo-Saxon countries have", 
    ifelse(anglo_comparison$mean_size_km2[1] < anglo_comparison$mean_size_km2[2], "SMALLER", "LARGER"),
    "average unit sizes\n")

cat("4. Total study area correlation: r =", round(correlation_areas, 3), "\n")

cat("5. Variable count correlation: r =", round(variable_correlations[1, "total_variables_count"], 3), "\n")

cat("\nAnalysis complete!\n")
