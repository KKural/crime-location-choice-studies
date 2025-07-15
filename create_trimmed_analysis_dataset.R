# Create Trimmed Analysis Dataset
# This script creates a clean, analysis-ready dataset containing only the variables 
# actually used in the spatial unit analysis, including unit selection justification

library(dplyr)
library(readr)
library(stringr)

# Read the full merged dataset
full_data <- read_csv("20250713_Analysis & Results/20250713_standardized_unit_sizes_with_groups_merged.csv")

# Define the variables used in the analysis
# Core variables from the original data
source_vars <- c(
  # Identifiers and basic info
  "Study_ID",
  "Citation", 
  "Year",
  "Title_of_the_study",
  "DOI",
  "Journal",
  "Volume", 
  "Issue",
  "ISSN",
  "match_type",
  
  # Unit size and characteristics (main dependent variable)
  "Unit_size_km2",
  "Size_of_the_unit",
  "Unit",
  "Size_group",
  "Name_of_the_unit",
  "No_of_units",
  "No_of_incidents",
  "Inferred_size",
  
  # Geographic/jurisdictional variables
  "Country",
  "City_Region",
  "Study_Area_Size_km2",
  "Population_per_Unit_Numeric",
  
  # Crime type and study characteristics
  "Crime_Type_Standardized",
  "Crime_Type",
  "Number_of_Crimes_Numeric",
  "Number_of_Offenders_Numeric",
  "Data_Collection_Period",
  "Study_Period",
  
  # Methodological variables used for Research_Sophistication score
  "Discrete_Choice_Model",
  "Sampling_Approach", 
  "Estimation_Method",
  "Study_Design",
  "Choice_Set_Definition",
  "Alternative_Selection",
  
  # Model quality and robustness indicators
  "Model_Comparison_Status",
  "Sample_Size_Effects_Status",
  "Robustness_Checks_Status",
  "Scale_Recommendations_Status",
  "Scale_Limitations_Status", 
  "Cross_Scale_Comparisons_Status",
  
  # Unit selection justification variables (key addition)
  "Justification_Summary",
  "Quoted_Rationale", 
  "Rationale_Category",
  
  # Data sources and results
  "Data_Sources",
  "Data_Availability",
  "Significant_Predictors",
  "Effect_Directions",
  
  # Spatial scale and recommendations
  "Spatial_Scale_Recommendations",
  "Data_Collection_Suggestions", 
  "Methodological_Improvements",
  
  # Variable counts and complexity (IMPORTANT for analysis)
  "Total_Variables",
  "Demographic_Variables",
  "Demographic_Count",
  "Economic_Variables",
  "Economic_Count", 
  "Environmental_Variables",
  "Environmental_Count",
  "Distance_Variables",
  "Distance_Count",
  "Temporal_Variables",
  "Temporal_Count",
  
  # Data limitations (comprehensive set)
  "Data_Quality_Issues",
  "Missing_Data_Issues",
  "Data_Source_Limitations",
  "Measurement_Issues",
  "Temporal_Limitations",
  "Generalizability",
  "Context_Specificity",
  "Comparative_Limitations"
)

# Create the trimmed dataset
trimmed_data <- full_data %>%
  # Select only the variables we need
  select(all_of(intersect(source_vars, names(full_data)))) %>%
  # Filter to valid unit size data (same filter as main analysis)
  filter(!is.na(Unit_size_km2) & Unit_size_km2 > 0) %>%
  # Add computed variables that are created in the main analysis
  mutate(
    # Publication year
    Publication_Year = as.numeric(Year),
    
    # Jurisdiction classification
    Jurisdiction = case_when(
      !is.na(Country) & Country != "" ~ Country,
      str_detect(tolower(Citation), "united states|usa|us\\b") ~ "United States",
      str_detect(tolower(Citation), "united kingdom|uk\\b|britain") ~ "United Kingdom", 
      str_detect(tolower(Citation), "canada") ~ "Canada",
      str_detect(tolower(Citation), "australia") ~ "Australia",
      str_detect(tolower(Citation), "netherlands") ~ "Netherlands",
      str_detect(tolower(Citation), "china") ~ "China",
      TRUE ~ "Other"
    ),
    
    # Anglo-Saxon grouping
    Anglo_Saxon = case_when(
      Jurisdiction %in% c("United States", "United Kingdom", "Canada", "Australia") ~ "Anglo-Saxon",
      TRUE ~ "Other"
    ),
    
    # Crime type (using standardized version)
    Crime_Type_Clean = case_when(
      !is.na(Crime_Type_Standardized) & 
      Crime_Type_Standardized != "" & 
      Crime_Type_Standardized != "Not Specified" ~ Crime_Type_Standardized,
      TRUE ~ "Not Specified"
    ),
    
    # Unit type classification
    Unit_Type = case_when(
      str_detect(Name_of_the_unit, "Block") ~ "Block-based",
      str_detect(Name_of_the_unit, "Census_tract") ~ "Census Tract",
      str_detect(Name_of_the_unit, "Census_output|Statistical_Area|LSOA|Super_Output") ~ "Statistical Area",
      str_detect(Name_of_the_unit, "Neighborhood|Community") ~ "Neighborhood", 
      str_detect(Name_of_the_unit, "Residential_property") ~ "Property-level",
      str_detect(Name_of_the_unit, "Street_segment") ~ "Street-based",
      str_detect(Name_of_the_unit, "Grid_cell") ~ "Grid Cell",
      str_detect(Name_of_the_unit, "Ward|Statistical_districts") ~ "Administrative District",
      str_detect(Name_of_the_unit, "Postal_code") ~ "Postal Code",
      TRUE ~ "Other"
    ),
    
    # Time period grouping
    Time_Period = case_when(
      Publication_Year <= 2010 ~ "2003-2010",
      Publication_Year <= 2015 ~ "2011-2015", 
      Publication_Year <= 2020 ~ "2016-2020",
      Publication_Year >= 2021 ~ "2021-2025",
      TRUE ~ "Unknown"
    ),
    
    # Log transformation
    Log_Unit_size = log10(pmax(Unit_size_km2, 1e-9)),
    
    # Methodological indicators for Research_Sophistication
    Has_Choice_Model = !is.na(Discrete_Choice_Model) & 
                      Discrete_Choice_Model != "" & 
                      Discrete_Choice_Model != "Not Specified",
    
    Has_Sampling = !is.na(Sampling_Approach) & 
                  Sampling_Approach != "" & 
                  Sampling_Approach != "Not Specified",
                  
    Has_Controls = !is.na(Demographic_Variables) & Demographic_Variables != "",
    
    Has_Estimation_Method = !is.na(Estimation_Method) & 
                           Estimation_Method != "" & 
                           Estimation_Method != "Not Specified",
    
    # Research sophistication score (0-4 scale)
    Research_Sophistication = as.numeric(Has_Choice_Model) +
                             as.numeric(Has_Sampling) +
                             as.numeric(Has_Controls) +
                             as.numeric(Has_Estimation_Method),
    
    # Unit selection justification flag
    Has_Unit_Justification = !is.na(Justification_Summary) & 
                            Justification_Summary != "" &
                            Justification_Summary != "Not Specified",
    
    # Variable complexity analysis
    Total_Variables_Numeric = as.numeric(Total_Variables),
    Demographic_Count_Numeric = as.numeric(Demographic_Count),
    Economic_Count_Numeric = as.numeric(Economic_Count),
    Environmental_Count_Numeric = as.numeric(Environmental_Count),
    Distance_Count_Numeric = as.numeric(Distance_Count),
    Temporal_Count_Numeric = as.numeric(Temporal_Count),
    
    # Variable complexity categories
    Variable_Complexity = case_when(
      Total_Variables_Numeric <= 10 ~ "Low (≤10)",
      Total_Variables_Numeric <= 20 ~ "Medium (11-20)",
      Total_Variables_Numeric <= 30 ~ "High (21-30)",
      Total_Variables_Numeric > 30 ~ "Very High (>30)",
      TRUE ~ "Unknown"
    ),
    
    # Has different types of variables
    Has_Demographic_Vars = !is.na(Demographic_Count_Numeric) & Demographic_Count_Numeric > 0,
    Has_Economic_Vars = !is.na(Economic_Count_Numeric) & Economic_Count_Numeric > 0,
    Has_Environmental_Vars = !is.na(Environmental_Count_Numeric) & Environmental_Count_Numeric > 0,
    Has_Distance_Vars = !is.na(Distance_Count_Numeric) & Distance_Count_Numeric > 0,
    Has_Temporal_Vars = !is.na(Temporal_Count_Numeric) & Temporal_Count_Numeric > 0,
    
    # Variable diversity score (how many different types of variables)
    Variable_Diversity_Score = as.numeric(Has_Demographic_Vars) +
                              as.numeric(Has_Economic_Vars) +
                              as.numeric(Has_Environmental_Vars) +
                              as.numeric(Has_Distance_Vars) +
                              as.numeric(Has_Temporal_Vars),
    
    # Scale limitations and recommendations flags
    Has_Scale_Limitations = !is.na(Scale_Limitations_Status) & 
                           Scale_Limitations_Status == "Specified",
    
    Has_Scale_Recommendations = !is.na(Scale_Recommendations_Status) & 
                               Scale_Recommendations_Status == "Specified",
    
    Has_Spatial_Scale_Recommendations = !is.na(Spatial_Scale_Recommendations) & 
                                       Spatial_Scale_Recommendations != "" &
                                       Spatial_Scale_Recommendations != "Not mentioned.",
    
    # Data limitation indicators
    Has_Data_Quality_Issues = !is.na(Data_Quality_Issues) & 
                             Data_Quality_Issues != "" &
                             Data_Quality_Issues != "NA",
    
    Has_Missing_Data_Issues = !is.na(Missing_Data_Issues) & 
                             Missing_Data_Issues != "" &
                             Missing_Data_Issues != "NA",
    
    Has_Data_Source_Limitations = !is.na(Data_Source_Limitations) & 
                                 Data_Source_Limitations != "" &
                                 Data_Source_Limitations != "NA",
    
    Has_Measurement_Issues = !is.na(Measurement_Issues) & 
                            Measurement_Issues != "" &
                            Measurement_Issues != "NA",
    
    Has_Temporal_Limitations = !is.na(Temporal_Limitations) & 
                              Temporal_Limitations != "" &
                              Temporal_Limitations != "NA",
    
    Has_Generalizability_Issues = !is.na(Generalizability) & 
                                 Generalizability != "" &
                                 Generalizability != "NA",
    
    Has_Context_Specificity = !is.na(Context_Specificity) & 
                             Context_Specificity != "" &
                             Context_Specificity != "NA",
    
    Has_Comparative_Limitations = !is.na(Comparative_Limitations) & 
                                 Comparative_Limitations != "" &
                                 Comparative_Limitations != "NA",
    
    # Overall data limitation score (0-8 scale)
    Data_Limitation_Score = as.numeric(Has_Data_Quality_Issues) +
                           as.numeric(Has_Missing_Data_Issues) +
                           as.numeric(Has_Data_Source_Limitations) +
                           as.numeric(Has_Measurement_Issues) +
                           as.numeric(Has_Temporal_Limitations) +
                           as.numeric(Has_Generalizability_Issues) +
                           as.numeric(Has_Context_Specificity) +
                           as.numeric(Has_Comparative_Limitations),
    
    # Data limitation category
    Data_Limitation_Category = case_when(
      Data_Limitation_Score == 0 ~ "No Limitations Reported",
      Data_Limitation_Score <= 2 ~ "Few Limitations (1-2)",
      Data_Limitation_Score <= 4 ~ "Moderate Limitations (3-4)",
      Data_Limitation_Score <= 6 ~ "Many Limitations (5-6)",
      Data_Limitation_Score > 6 ~ "Extensive Limitations (>6)",
      TRUE ~ "Unknown"
    ),
    
    # Methodological robustness indicators
    Has_Model_Comparison = !is.na(Model_Comparison_Status) & 
                          Model_Comparison_Status == "Specified",
    
    Has_Sample_Size_Effects = !is.na(Sample_Size_Effects_Status) & 
                             Sample_Size_Effects_Status == "Specified",
    
    Has_Robustness_Checks = !is.na(Robustness_Checks_Status) & 
                           Robustness_Checks_Status == "Specified",
    
    Has_Cross_Scale_Comparisons = !is.na(Cross_Scale_Comparisons_Status) & 
                                 Cross_Scale_Comparisons_Status == "Specified",
    
    # Methodological rigor score (0-4 scale)
    Methodological_Rigor_Score = as.numeric(Has_Model_Comparison) +
                                as.numeric(Has_Sample_Size_Effects) +
                                as.numeric(Has_Robustness_Checks) +
                                as.numeric(Has_Cross_Scale_Comparisons),
    
    # Study quality indicator (combining sophistication and rigor)
    Study_Quality_Score = Research_Sophistication + Methodological_Rigor_Score
  ) %>%
  # Remove rows with invalid log transformations
  filter(!is.infinite(Log_Unit_size) & !is.na(Log_Unit_size)) %>%
  # Arrange by Study_ID for consistency
  arrange(Study_ID)

# Print summary of the trimmed dataset
cat("Trimmed Analysis Dataset Summary:\n")
cat("================================\n")
cat("Original dataset rows:", nrow(full_data), "\n")
cat("Trimmed dataset rows:", nrow(trimmed_data), "\n")
cat("Number of variables:", ncol(trimmed_data), "\n\n")

cat("Key variables included:\n")
cat("- Core: Unit_size_km2, Log_Unit_size, Publication_Year, Total_Variables\n") 
cat("- Geographic: Jurisdiction, Anglo_Saxon, Country, City_Region\n")
cat("- Crime: Crime_Type_Clean, Crime_Type_Standardized\n")
cat("- Unit: Unit_Type, Name_of_the_unit, No_of_units\n")
cat("- Methods: Research_Sophistication, Methodological_Rigor_Score, Study_Quality_Score\n")
cat("- Justification: Justification_Summary, Quoted_Rationale, Rationale_Category\n")
cat("- Variable Complexity: Total_Variables_Numeric, Variable_Complexity, Variable_Diversity_Score\n")
cat("- Variable Types: Demographic_Count, Economic_Count, Environmental_Count, Distance_Count, Temporal_Count\n")
cat("- Scale Analysis: Scale_Limitations_Status, Scale_Recommendations_Status, Spatial_Scale_Recommendations\n")
cat("- Data Limitations: Data_Limitation_Score, Data_Limitation_Category, and 8 specific limitation types\n")
cat("- Robustness: Has_Model_Comparison, Has_Robustness_Checks, Has_Sample_Size_Effects, etc.\n\n")

# Check unit justification coverage
justification_summary <- trimmed_data %>%
  summarise(
    Total_Studies = n(),
    Has_Justification_Summary = sum(Has_Unit_Justification, na.rm = TRUE),
    Percent_With_Justification = round(100 * Has_Justification_Summary / Total_Studies, 1),
    Has_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
    Has_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE),
    
    # Variable complexity analysis
    Mean_Total_Variables = round(mean(Total_Variables_Numeric, na.rm = TRUE), 1),
    Median_Total_Variables = round(median(Total_Variables_Numeric, na.rm = TRUE), 1),
    Min_Total_Variables = min(Total_Variables_Numeric, na.rm = TRUE),
    Max_Total_Variables = max(Total_Variables_Numeric, na.rm = TRUE),
    Mean_Variable_Diversity = round(mean(Variable_Diversity_Score, na.rm = TRUE), 1),
    
    # Scale analysis
    With_Scale_Limitations = sum(Has_Scale_Limitations, na.rm = TRUE),
    Percent_With_Scale_Limitations = round(100 * With_Scale_Limitations / Total_Studies, 1),
    With_Scale_Recommendations = sum(Has_Scale_Recommendations, na.rm = TRUE),
    Percent_With_Scale_Recommendations = round(100 * With_Scale_Recommendations / Total_Studies, 1),
    With_Spatial_Scale_Recs = sum(Has_Spatial_Scale_Recommendations, na.rm = TRUE),
    Percent_With_Spatial_Scale_Recs = round(100 * With_Spatial_Scale_Recs / Total_Studies, 1),
    
    # Data limitations analysis
    Mean_Data_Limitation_Score = round(mean(Data_Limitation_Score, na.rm = TRUE), 1),
    Median_Data_Limitation_Score = median(Data_Limitation_Score, na.rm = TRUE),
    With_Data_Quality_Issues = sum(Has_Data_Quality_Issues, na.rm = TRUE),
    With_Generalizability_Issues = sum(Has_Generalizability_Issues, na.rm = TRUE),
    With_Context_Specificity = sum(Has_Context_Specificity, na.rm = TRUE),
    
    # Methodological rigor
    Mean_Research_Sophistication = round(mean(Research_Sophistication, na.rm = TRUE), 1),
    Mean_Methodological_Rigor = round(mean(Methodological_Rigor_Score, na.rm = TRUE), 1),
    Mean_Study_Quality = round(mean(Study_Quality_Score, na.rm = TRUE), 1)
  )

cat("Unit Selection Justification Coverage:\n")
print(justification_summary)
cat("\n")

# Save the trimmed dataset
output_file <- "20250713_Analysis & Results/20250713_analysis_ready_dataset_trimmed.csv"
write_csv(trimmed_data, output_file)
cat("Trimmed analysis dataset saved to:", output_file, "\n")

# Also create a version with only the essential analysis variables (for correlation matrix etc.)
essential_vars <- c(
  # Core identification and unit size
  "Study_ID", "Citation", "Publication_Year", "Unit_size_km2", "Log_Unit_size",
  "Total_Variables", "Total_Variables_Numeric",
  
  # Geographic and methodological
  "Jurisdiction", "Anglo_Saxon", "Crime_Type_Clean", "Unit_Type", 
  "Research_Sophistication", "Time_Period", 
  
  # Unit selection justification (key focus)
  "Has_Unit_Justification", "Justification_Summary", "Quoted_Rationale", "Rationale_Category",
  
  # Variable complexity analysis
  "Variable_Complexity", "Variable_Diversity_Score",
  "Demographic_Count_Numeric", "Economic_Count_Numeric", "Environmental_Count_Numeric",
  "Distance_Count_Numeric", "Temporal_Count_Numeric",
  "Has_Demographic_Vars", "Has_Economic_Vars", "Has_Environmental_Vars",
  "Has_Distance_Vars", "Has_Temporal_Vars",
  
  # Data limitations (comprehensive)
  "Data_Limitation_Score", "Data_Limitation_Category",
  "Has_Data_Quality_Issues", "Has_Missing_Data_Issues", "Has_Data_Source_Limitations",
  "Has_Measurement_Issues", "Has_Temporal_Limitations", "Has_Generalizability_Issues",
  "Has_Context_Specificity", "Has_Comparative_Limitations",
  
  # Spatial scale recommendations and limitations
  "Has_Scale_Limitations", "Has_Scale_Recommendations", "Has_Spatial_Scale_Recommendations",
  "Scale_Limitations_Status", "Scale_Recommendations_Status", "Spatial_Scale_Recommendations",
  
  # Methodological rigor
  "Methodological_Rigor_Score", "Study_Quality_Score",
  "Has_Model_Comparison", "Has_Sample_Size_Effects", "Has_Robustness_Checks", "Has_Cross_Scale_Comparisons",
  
  # Raw limitation text for qualitative analysis
  "Data_Quality_Issues", "Missing_Data_Issues", "Data_Source_Limitations",
  "Measurement_Issues", "Temporal_Limitations", "Generalizability", 
  "Context_Specificity", "Comparative_Limitations"
)

essential_data <- trimmed_data %>%
  select(all_of(essential_vars))

essential_output_file <- "20250713_Analysis & Results/20250713_analysis_ready_dataset_essential.csv"
write_csv(essential_data, essential_output_file)
cat("Essential analysis dataset saved to:", essential_output_file, "\n")

# Print variable list for verification
cat("\nFinal variable list in trimmed dataset:\n")
cat(paste(names(trimmed_data), collapse = ", "), "\n")
