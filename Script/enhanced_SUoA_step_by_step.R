# Enhanced SUoA Analysis - Step by Step Approach (DEPRECATED)
# Research Questions on Spatial Unit of Analysis characteristics across crime location choice studies
# 
# NOTE: This file has been deprecated in favor of enhanced_SUoA_complete.R
# which contains all research questions in a single, optimized script.
#
# For the complete analysis, please use: enhanced_SUoA_complete.R

library(tidyverse)
library(here)

cat("=======================================================================\n")
cat("ENHANCED SPATIAL UNIT OF ANALYSIS (SUoA) - STEP-BY-STEP APPROACH\n")
cat("=======================================================================\n\n")

cat("⚠️  NOTE: This file has been deprecated.\n")
cat("   Please use 'enhanced_SUoA_complete.R' for the full analysis.\n")
cat("   This file contained all 7 research questions but is now\n")
cat("   superseded by the complete, optimized version.\n\n")

cat("To run the complete analysis:\n")
cat("   source(here('Script', 'enhanced_SUoA_complete.R'))\n\n")

cat("The complete script includes:\n")
cat("• All 7 research questions in one efficient run\n")
cat("• Advanced statistical methods (mixed-effects, interactions)\n")
cat("• Comprehensive visualizations and summary\n")
cat("• Execution time tracking\n")
cat("• Enhanced manuscript-ready outputs\n\n")

# Automatically source the complete analysis if user wants
response <- readline(prompt = "Would you like to run the complete analysis now? (y/n): ")

if(tolower(response) %in% c("y", "yes")) {
  cat("\nLaunching complete analysis...\n")
  source(here("Script", "enhanced_SUoA_complete.R"))
} else {
  cat("\nTo run later, use: source(here('Script', 'enhanced_SUoA_complete.R'))\n")
}

# Load the merged dataset for reference
cat("Loading data for reference...\n")
data <- read_csv(here("Output", "merged_comprehensive_unit_sizes.csv"), show_col_types = FALSE)

# Data preparation (simplified)
data <- data %>%
  mutate(
    Publication_Year = as.numeric(Publication_Year),
    Publication_Year = ifelse(is.na(Publication_Year) & !is.na(year), 
                             as.numeric(year), Publication_Year),
    Jurisdiction = case_when(
      !is.na(country) & country != "" ~ country,
      TRUE ~ "Other"
    ),
    Anglo_Saxon = case_when(
      Jurisdiction %in% c("United States", "United Kingdom", "Canada", "Australia") ~ "Anglo-Saxon",
      TRUE ~ "Other"
    ),
    Crime_Type = case_when(
      !is.na(crime_types_all) & crime_types_all != "" ~ crime_types_all,
      TRUE ~ "Other"
    ),
    No_of_units_numeric = as.numeric(str_replace_all(No_of_units, "[^0-9.]", "")),
    Total_study_area_km2 = Unit_size_km2 * No_of_units_numeric,
    Log_Unit_size = log10(Unit_size_km2),
    Log_Total_area = log10(Total_study_area_km2 + 1e-10)
  )

cat("Dataset prepared with", nrow(data), "studies\n\n")

cat("=======================================================================\n")
cat("STEP-BY-STEP ANALYSIS STRUCTURE:\n")
cat("=======================================================================\n\n")

cat("The analysis covers 7 research questions:\n")
cat("• RQ1: Distribution of SUoA sizes across studies\n")
cat("• RQ2: Temporal changes in SUoA size over time\n")
cat("• RQ3: Jurisdictional differences in SUoA size\n")
cat("• RQ4: Crime type variations in SUoA size\n")
cat("• RQ5: Relationship between SUoA and total study area\n")
cat("• RQ6: Connection between SUoA size and discrete choice methods\n")
cat("• RQ7: Relationship between SUoA size and independent variables\n\n")

cat("For the complete analysis with all research questions,\n")
cat("please run: enhanced_SUoA_complete.R\n\n")

cat("This step-by-step file now serves as a reference for the data preparation\n")
cat("and provides an overview of the research structure.\n\n")
