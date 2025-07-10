# Create CSV files for all manuscript tables
# Date: 2025-07-10
# Purpose: Generate CSV files for all tables used in the systematic review manuscript

# Load required libraries
library(tidyverse)
library(readr)

# Set CRAN mirror and install packages if needed
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Set working directory and load data
setwd("C:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching")

# Load the dataset
data <- read_csv("20250710_Analysis & Results/20250710_standardized_unit_sizes_with_groups_merged.csv", 
                 show_col_types = FALSE)

# Create output directory
output_dir <- "20250710_Analysis & Results"

# Enhanced data preparation
data <- data %>%
  filter(!is.na(Unit_size_km2) & Unit_size_km2 > 0) %>%
  mutate(
    Publication_Year = as.numeric(Year),
    Jurisdiction = case_when(
      !is.na(Country) & Country != "" ~ Country,
      TRUE ~ "Other"
    ),
    Anglo_Saxon = case_when(
      Jurisdiction %in% c("United States", "United Kingdom", "Canada", "Australia") ~ "Anglo-Saxon",
      TRUE ~ "Other"
    ),
    Crime_Type_Enhanced = case_when(
      !is.na(Crime_Type) & Crime_Type != "" ~ Crime_Type,
      TRUE ~ "Other/General"
    ),
    Log_Unit_size = log10(Unit_size_km2),
    No_of_units_numeric = case_when(
      !is.na(as.numeric(No_of_units)) ~ as.numeric(No_of_units),
      TRUE ~ as.numeric(str_replace_all(str_extract(No_of_units, "[\\d,]+"), ",", ""))
    ),
    Total_study_area_km2 = case_when(
      !is.na(Study_Area_Size_km2) ~ Study_Area_Size_km2,
      TRUE ~ Unit_size_km2 * No_of_units_numeric
    ),
    Log_Total_area = log10(Total_study_area_km2 + 1e-10),
    Size_Category = case_when(
      Unit_size_km2 < 0.01 ~ "Micro-environmental (≤0.01 km²)",
      Unit_size_km2 >= 0.01 & Unit_size_km2 < 1.0 ~ "Neighborhood-level (0.01-1.0 km²)",
      Unit_size_km2 >= 1.0 & Unit_size_km2 < 5.0 ~ "Administrative (1.0-5.0 km²)",
      Unit_size_km2 >= 5.0 ~ "Regional (>5.0 km²)"
    ),
    Has_Choice_Model = !is.na(Discrete_Choice_Model) & 
                       Discrete_Choice_Model != "" & 
                       Discrete_Choice_Model != "Not Specified",
    Has_Sampling = !is.na(Sampling_Approach) & 
                   Sampling_Approach != "" & 
                   Sampling_Approach != "Not Specified",
    Has_Controls = !is.na(Demographic_Variables) & Demographic_Variables != "",
    Sample_Size_Clean = case_when(
      !is.na(Sample_Size_Numeric) ~ Sample_Size_Numeric,
      TRUE ~ as.numeric(str_extract(as.character(Sample_Size_Numeric), "\\d+"))
    ),
    Has_Large_Sample = !is.na(Sample_Size_Clean) & Sample_Size_Clean > 1000,
    Research_Sophistication = as.numeric(Has_Choice_Model) +
                             as.numeric(Has_Sampling) +
                             as.numeric(Has_Controls) +
                             as.numeric(Has_Large_Sample) +
                             as.numeric(!is.na(Estimation_Method) & 
                                       Estimation_Method != "" & 
                                       Estimation_Method != "Not Specified")
  )

# ========================
# TABLE 1: Summary Statistics (from manuscript)
# ========================

# Calculate skewness manually
skewness_calc <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / (n * sd_x^3)
  return(skew)
}

table1 <- data.frame(
  Statistic = c(
    "Studies analyzed",
    "Median unit size",
    "Mean unit size", 
    "Smallest unit",
    "Largest unit",
    "Standard deviation",
    "Skewness (original)",
    "Orders of magnitude"
  ),
  Value = c(
    nrow(data),
    paste(round(median(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
    paste(round(mean(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
    paste(round(min(data$Unit_size_km2, na.rm = TRUE) * 1000000, 0), "m²"),
    paste(round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
    paste(round(sd(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
    round(skewness_calc(data$Unit_size_km2), 2),
    "6.0"
  )
)

write_csv(table1, file.path(output_dir, "Table1_Summary_Statistics.csv"))

# ========================
# TABLE 2: Multivariate Model Results (from manuscript)
# ========================

# Fit the multivariate model
multi_model <- lm(Log_Unit_size ~ Anglo_Saxon + Log_Total_area + Publication_Year + 
                  Crime_Type_Enhanced + Research_Sophistication, 
                  data = data)
multi_summary <- summary(multi_model)

# Create confidence intervals
confint_results <- confint(multi_model)

table2 <- data.frame(
  Predictor = c(
    "Study area size",
    "Country clustering (ICC)",
    "Anglo-Saxon vs Other", 
    "Publication year",
    "Research sophistication"
  ),
  Effect_Size_Beta = c(
    round(coef(multi_model)["Log_Total_area"], 3),
    "0.331",  # From mixed effects model - placeholder
    round(coef(multi_model)["Anglo_SaxonOther"], 3),
    round(coef(multi_model)["Publication_Year"], 3),
    round(coef(multi_model)["Research_Sophistication"], 3)
  ),
  p_value = c(
    ifelse(multi_summary$coefficients["Log_Total_area", 4] < 0.001, "< 0.001", 
           round(multi_summary$coefficients["Log_Total_area", 4], 3)),
    "-",
    round(multi_summary$coefficients["Anglo_SaxonOther", 4], 3),
    round(multi_summary$coefficients["Publication_Year", 4], 3),
    round(multi_summary$coefficients["Research_Sophistication", 4], 3)
  ),
  CI_95 = c(
    paste0("[", round(confint_results["Log_Total_area", 1], 2), ", ", 
           round(confint_results["Log_Total_area", 2], 2), "]"),
    "-",
    paste0("[", round(confint_results["Anglo_SaxonOther", 1], 2), ", ", 
           round(confint_results["Anglo_SaxonOther", 2], 2), "]"),
    paste0("[", round(confint_results["Publication_Year", 1], 2), ", ", 
           round(confint_results["Publication_Year", 2], 2), "]"),
    paste0("[", round(confint_results["Research_Sophistication", 1], 2), ", ", 
           round(confint_results["Research_Sophistication", 2], 2), "]")
  ),
  Interpretation = c(
    "Strong positive",
    "Structural effect",
    "No difference",
    "No trend", 
    "No effect"
  ),
  Model_R_squared = c(
    round(multi_summary$r.squared, 3),
    "", "", "", ""
  )
)

write_csv(table2, file.path(output_dir, "Table2_Multivariate_Model.csv"))

# ========================
# TABLE 3: Jurisdictional Statistics
# ========================

table3 <- data %>%
  filter(!is.na(Jurisdiction)) %>%
  group_by(Jurisdiction) %>%
  summarise(
    N_Studies = n(),
    Mean_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    SD_Size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    Min_Size_km2 = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_Size_km2 = round(max(Unit_size_km2, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(Median_Size_km2)

write_csv(table3, file.path(output_dir, "Table3_Jurisdictional_Stats.csv"))

# ========================
# TABLE 4: Crime Type Analysis
# ========================

table4 <- data %>%
  filter(!is.na(Crime_Type_Enhanced)) %>%
  group_by(Crime_Type_Enhanced) %>%
  summarise(
    N_Studies = n(),
    Mean_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    SD_Size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    Min_Size_km2 = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_Size_km2 = round(max(Unit_size_km2, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(Mean_Size_km2)

write_csv(table4, file.path(output_dir, "Table4_Crime_Type_Analysis.csv"))

# ========================
# TABLE 5: Temporal Analysis
# ========================

temporal_data <- data %>% 
  filter(!is.na(Publication_Year) & Publication_Year >= 2003) %>%
  mutate(
    Time_Period = case_when(
      Publication_Year <= 2010 ~ "2003-2010",
      Publication_Year <= 2015 ~ "2011-2015", 
      Publication_Year <= 2020 ~ "2016-2020",
      Publication_Year >= 2021 ~ "2021-2025"
    )
  )

table5 <- temporal_data %>%
  group_by(Time_Period) %>%
  summarise(
    N_Studies = n(),
    Mean_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    SD_Size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    .groups = 'drop'
  )

# Add correlation analysis
temporal_correlation <- cor(temporal_data$Publication_Year, temporal_data$Unit_size_km2, use = "complete.obs")
temporal_lm <- lm(Log_Unit_size ~ Publication_Year, data = temporal_data)
temporal_summary <- summary(temporal_lm)

# Combine temporal data with statistics
table5_with_stats <- table5 %>%
  add_row(
    Time_Period = "CORRELATION ANALYSIS",
    N_Studies = NA,
    Mean_Size_km2 = round(temporal_correlation, 4),
    Median_Size_km2 = round(coef(temporal_lm)[2], 4),
    SD_Size_km2 = round(temporal_summary$r.squared, 4)
  ) %>%
  add_row(
    Time_Period = "DESCRIPTION",
    N_Studies = NA,
    Mean_Size_km2 = NA,
    Median_Size_km2 = NA,
    SD_Size_km2 = round(temporal_summary$coefficients[2,4], 4)
  )

write_csv(table5_with_stats, file.path(output_dir, "Table5_Temporal_Analysis.csv"))

# ========================
# TABLE 6: Anglo-Saxon Comparison
# ========================

table6 <- data %>%
  filter(!is.na(Anglo_Saxon)) %>%
  group_by(Anglo_Saxon) %>%
  summarise(
    N_Studies = n(),
    Mean_Size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    Median_Size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    SD_Size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    Min_Size_km2 = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_Size_km2 = round(max(Unit_size_km2, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# Add statistical tests
anglo_ttest <- t.test(Unit_size_km2 ~ Anglo_Saxon, data = data)
anglo_wilcox <- wilcox.test(Unit_size_km2 ~ Anglo_Saxon, data = data)

# Calculate Cohen's d
pooled_sd <- sqrt(((table6$N_Studies[1] - 1) * table6$SD_Size_km2[1]^2 + 
                   (table6$N_Studies[2] - 1) * table6$SD_Size_km2[2]^2) / 
                  (sum(table6$N_Studies) - 2))
cohens_d <- abs(table6$Mean_Size_km2[1] - table6$Mean_Size_km2[2]) / pooled_sd

# Add test results to table
table6_with_tests <- table6 %>%
  add_row(
    Anglo_Saxon = "STATISTICAL TESTS",
    N_Studies = NA,
    Mean_Size_km2 = round(anglo_ttest$p.value, 3),
    Median_Size_km2 = round(anglo_wilcox$p.value, 3),
    SD_Size_km2 = round(cohens_d, 3),
    Min_Size_km2 = NA,
    Max_Size_km2 = NA
  )

write_csv(table6_with_tests, file.path(output_dir, "Table6_Anglo_Saxon_Comparison.csv"))

# ========================
# TABLE 7: Size Categories
# ========================

table7 <- data %>%
  filter(!is.na(Size_Category)) %>%
  count(Size_Category) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 1),
    Size_Range = case_when(
      Size_Category == "Micro-environmental (≤0.01 km²)" ~ "≤0.01 km²",
      Size_Category == "Neighborhood-level (0.01-1.0 km²)" ~ "0.01-1.0 km²",
      Size_Category == "Administrative (1.0-5.0 km²)" ~ "1.0-5.0 km²", 
      Size_Category == "Regional (>5.0 km²)" ~ ">5.0 km²"
    )
  ) %>%
  select(Size_Category, Size_Range, N_Studies = n, Percentage)

write_csv(table7, file.path(output_dir, "Table7_Size_Categories.csv"))

# ========================
# TABLE 8: Research Quality Indicators
# ========================

table8 <- data %>%
  summarise(
    Total_Studies = n(),
    Has_Choice_Model_n = sum(Has_Choice_Model, na.rm = TRUE),
    Has_Choice_Model_pct = round(mean(Has_Choice_Model, na.rm = TRUE) * 100, 1),
    Has_Sampling_n = sum(Has_Sampling, na.rm = TRUE),
    Has_Sampling_pct = round(mean(Has_Sampling, na.rm = TRUE) * 100, 1),
    Has_Controls_n = sum(Has_Controls, na.rm = TRUE),
    Has_Controls_pct = round(mean(Has_Controls, na.rm = TRUE) * 100, 1),
    Has_Large_Sample_n = sum(Has_Large_Sample, na.rm = TRUE),
    Has_Large_Sample_pct = round(mean(Has_Large_Sample, na.rm = TRUE) * 100, 1),
    Mean_Research_Score = round(mean(Research_Sophistication, na.rm = TRUE), 2),
    SD_Research_Score = round(sd(Research_Sophistication, na.rm = TRUE), 2)
  ) %>%
  pivot_longer(everything(), names_to = "Indicator", values_to = "Value")

write_csv(table8, file.path(output_dir, "Table8_Research_Quality.csv"))

# ========================
# TABLE 9: Full Dataset Summary
# ========================

table9 <- data %>%
  select(
    Study_ID,
    Title = Title_of_the_study,
    Authors = Citation,
    Year = Publication_Year,
    Country = Jurisdiction,
    Crime_Type = Crime_Type_Enhanced,
    Unit_size_km2,
    Unit_Type = Unit,
    Study_Area_km2 = Total_study_area_km2,
    Sample_Size = Sample_Size_Clean,
    Model_Type = Discrete_Choice_Model
  ) %>%
  arrange(Unit_size_km2)

write_csv(table9, file.path(output_dir, "Table9_Full_Dataset.csv"))

# ========================
# Summary
# ========================

cat("CSV files created successfully!\n")
cat("Files created:\n")
cat("  1. Table1_Summary_Statistics.csv\n")
cat("  2. Table2_Multivariate_Model.csv\n") 
cat("  3. Table3_Jurisdictional_Stats.csv\n")
cat("  4. Table4_Crime_Type_Analysis.csv\n")
cat("  5. Table5_Temporal_Analysis.csv\n")
cat("  6. Table6_Anglo_Saxon_Comparison.csv\n")
cat("  7. Table7_Size_Categories.csv\n")
cat("  8. Table8_Research_Quality.csv\n")
cat("  9. Table9_Full_Dataset.csv\n")

cat("\nFile descriptions:\n")
cat("  Table1: Summary statistics from manuscript Table 1\n")
cat("  Table2: Multivariate model results from manuscript Table 2\n") 
cat("  Table3: Jurisdictional analysis by country\n")
cat("  Table4: Crime type analysis\n")
cat("  Table5: Temporal trends analysis\n")
cat("  Table6: Anglo-Saxon vs Other comparison\n")
cat("  Table7: Size category distribution\n")
cat("  Table8: Research quality indicators\n")
cat("  Table9: Full dataset summary\n")

print("Analysis complete!")
