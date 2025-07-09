# ──────────────────────────────────────────────────────────
# CLEAN EXTRACTION USING REGEX APPROACH (Fixed for actual data)
# ──────────────────────────────────────────────────────────
library(readr)    # read_csv
library(dplyr)    # mutate, select
library(stringr)  # regex helpers
library(tidyr)    # separate_wider_regex

# Load the actual clean dataset
df_raw <- read_csv("Output/clean_combined_dataset.csv", show_col_types = FALSE)

# ---- Inspect the actual column names
cat("Actual column names:\n")
print(names(df_raw))

# Use the correct column names from your data
df <- df_raw %>% 
  mutate(
    # Combine key columns for extraction
    full_text = paste(
      `BASIC STUDY IDENTIFICATION`,
      `TEMPORAL SCOPE & DATA SOURCES`,
      `SPATIAL UNITS - DESCRIPTION & JUSTIFICATION`,
      `STUDY CONTEXT & GEOGRAPHY`,
      `SAMPLING & CHOICE SETS`,
      `THEORETICAL FRAMEWORK & OBJECTIVES`,
      `STUDY DESIGN & METHODOLOGY`,
      `DATA PREPARATION & PROCESSING`,
      `MAJOR FINDINGS & RESULTS`,
      sep = "\n"
    )
  )

# ──────────────────────────────────────────────────────────
# 2. HELPER FUNCTIONS (regex wrappers) - VECTORIZED
# ──────────────────────────────────────────────────────────
# Matches markdown style: **Year:** 2013
grab_bold <- function(txt, label){
  pattern <- paste0("(?i)\\*\\*\\s*", label, "\\s*\\*\\*\\s*[:]?\\s*([^\\r\\n]+)")
  matches <- str_match(txt, pattern)
  result <- str_trim(matches[,2])
  return(result)
}

# Matches dash style: - SUoA Type: Census tract
grab_dash <- function(txt, label){
  pattern <- paste0("(?i)-\\s*", label, "\\s*[:]\\s*([^\\r\\n]+)")
  matches <- str_match(txt, pattern)
  result <- str_trim(matches[,2])
  return(result)
}

# Matches simple colon pattern: Label: Value
grab_colon <- function(txt, label){
  pattern <- paste0("(?i)", label, "\\s*[:]\\s*([^\\r\\n]+)")
  matches <- str_match(txt, pattern)
  result <- str_trim(matches[,2])
  return(result)
}

# ──────────────────────────────────────────────────────────
# 3. EXTRACT FIELDS WITH MULTIPLE PATTERN ATTEMPTS
# ──────────────────────────────────────────────────────────
df_parsed <- df %>% 
  mutate(
    # Basic Study Information (try both ** and - patterns)
    Year = coalesce(
      grab_bold(full_text, "Year"),
      grab_dash(full_text, "Year"),
      grab_colon(full_text, "Year")
    ),
    Authors = coalesce(
      grab_bold(full_text, "Authors"),
      grab_dash(full_text, "Authors"),
      grab_colon(full_text, "Authors")
    ),
    Journal = coalesce(
      grab_bold(full_text, "Journal"),
      grab_dash(full_text, "Journal"),
      grab_colon(full_text, "Journal")
    ),
    DOI = coalesce(
      grab_bold(full_text, "DOI"),
      grab_dash(full_text, "DOI"),
      grab_colon(full_text, "DOI")
    ),
    
    # Temporal Information
    Study_Period = coalesce(
      grab_bold(full_text, "Study Period"),
      grab_dash(full_text, "Study Period"),
      grab_colon(full_text, "Study Period")
    ),
    Data_Collection_Period = coalesce(
      grab_bold(full_text, "Data Collection Period"),
      grab_dash(full_text, "Data Collection Period"),
      grab_colon(full_text, "Data Collection Period")
    ),
    Data_Sources = coalesce(
      grab_bold(full_text, "Data Sources"),
      grab_dash(full_text, "Data Sources"),
      grab_colon(full_text, "Data Sources")
    ),
    
    # Spatial Units Information
    SUoA_Type = coalesce(
      grab_dash(full_text, "SUoA Type"),
      grab_colon(full_text, "SUoA Type")
    ),
    SUoA_Size = coalesce(
      grab_dash(full_text, "SUoA Size"),
      grab_colon(full_text, "SUoA Size")
    ),
    SUoA_Description = coalesce(
      grab_dash(full_text, "SUoA Description"),
      grab_colon(full_text, "SUoA Description")
    ),
    Number_of_Units = coalesce(
      grab_dash(full_text, "Number of Units"),
      grab_colon(full_text, "Number of Units")
    ),
    Population_per_Unit = coalesce(
      grab_dash(full_text, "Population per Unit"),
      grab_colon(full_text, "Population per Unit")
    ),
    Quoted_Rationale = coalesce(
      grab_dash(full_text, "Quoted Rationale"),
      grab_colon(full_text, "Quoted Rationale")
    ),
    Rationale_Category = coalesce(
      grab_dash(full_text, "Rationale Category"),
      grab_colon(full_text, "Rationale Category")
    ),
    
    # Geographic Information
    Country = coalesce(
      grab_bold(full_text, "Country"),
      grab_dash(full_text, "Country"),
      grab_colon(full_text, "Country")
    ),
    City_Region = coalesce(
      grab_bold(full_text, "City/Region"),
      grab_dash(full_text, "City/Region"),
      grab_colon(full_text, "City/Region")
    ),
    Study_Area_Size = coalesce(
      grab_bold(full_text, "Study Area Size"),
      grab_dash(full_text, "Study Area Size"),
      grab_colon(full_text, "Study Area Size")
    ),
    Crime_Type = coalesce(
      grab_bold(full_text, "Crime Type"),
      grab_dash(full_text, "Crime Type"),
      grab_colon(full_text, "Crime Type")
    ),
    
    # Sampling Information
    Sampling_Approach = coalesce(
      grab_bold(full_text, "Sampling Approach"),
      grab_dash(full_text, "Sampling Approach"),
      grab_colon(full_text, "Sampling Approach")
    ),
    Sample_Size = coalesce(
      grab_bold(full_text, "Sample Size"),
      grab_dash(full_text, "Sample Size"),
      grab_colon(full_text, "Sample Size")
    ),
    Number_of_Crimes_Analyzed = coalesce(
      grab_bold(full_text, "Number of Crimes Analyzed"),
      grab_dash(full_text, "Number of Crimes Analyzed"),
      grab_colon(full_text, "Number of Crimes Analyzed")
    ),
    
    # Methodology
    Study_Design = coalesce(
      grab_dash(full_text, "Study Design"),
      grab_colon(full_text, "Study Design")
    ),
    Discrete_Choice_Model = coalesce(
      grab_dash(full_text, "Discrete Choice Model"),
      grab_colon(full_text, "Discrete Choice Model")
    ),
    Software_Used = coalesce(
      grab_dash(full_text, "Software Used"),
      grab_colon(full_text, "Software Used")
    ),
    
    # Key Findings
    Main_Results = coalesce(
      grab_bold(full_text, "Main Results"),
      grab_dash(full_text, "Main Results"),
      grab_colon(full_text, "Main Results")
    ),
    Significant_Predictors = coalesce(
      grab_bold(full_text, "Significant Predictors"),
      grab_dash(full_text, "Significant Predictors"),
      grab_colon(full_text, "Significant Predictors")
    )
  )

# ──────────────────────────────────────────────────────────
# 4. CLEAN UP EXTRACTED DATA
# ──────────────────────────────────────────────────────────
df_parsed <- df_parsed %>%
  mutate(
    # Remove any remaining ** symbols
    across(Year:Significant_Predictors, ~str_remove_all(.x, "\\*\\*")),
    # Clean up extra whitespace
    across(Year:Significant_Predictors, ~str_trim(.x)),
    # Convert empty strings to NA
    across(Year:Significant_Predictors, ~ifelse(.x == "" | .x == "Not mentioned" | .x == "Not explicitly mentioned", NA_character_, .x))
  )

# ──────────────────────────────────────────────────────────
# 5. OPTIONAL — EXTRACT YEAR NUMBERS FROM STUDY PERIOD
# ──────────────────────────────────────────────────────────
df_parsed <- df_parsed %>%
  mutate(
    # Extract first year from study period
    Study_Start_Year = str_extract(Study_Period, "\\d{4}"),
    # Extract last year if there's a range
    Study_End_Year = str_extract(Study_Period, "\\d{4}$")
  )

# ──────────────────────────────────────────────────────────
# 6. SELECT KEY COLUMNS AND SAVE
# ──────────────────────────────────────────────────────────
df_final <- df_parsed %>%
  select(
    Title,
    Year, Authors, Journal, DOI,
    Study_Period, Study_Start_Year, Study_End_Year,
    Data_Collection_Period, Data_Sources,
    Country, City_Region, Study_Area_Size, Crime_Type,
    SUoA_Type, SUoA_Size, Number_of_Units, Population_per_Unit,
    Quoted_Rationale, Rationale_Category,
    Sampling_Approach, Sample_Size, Number_of_Crimes_Analyzed,
    Study_Design, Discrete_Choice_Model, Software_Used,
    Main_Results, Significant_Predictors
  )

# Save the clean extracted dataset
write_csv(df_final, "Output/regex_extracted_dataset.csv")

# ──────────────────────────────────────────────────────────
# 7. QUICK INSPECTION & SUMMARY
# ──────────────────────────────────────────────────────────
cat("\n=== EXTRACTION RESULTS ===\n")
cat("Total articles processed:", nrow(df_final), "\n")
cat("Total fields extracted:", ncol(df_final) - 1, "\n")

# Check extraction success rates
extraction_rates <- df_final %>%
  select(-Title) %>%
  summarise_all(~sum(!is.na(.x))) %>%
  pivot_longer(everything(), names_to = "Field", values_to = "Count") %>%
  mutate(Success_Rate = round((Count / nrow(df_final)) * 100, 1)) %>%
  arrange(desc(Success_Rate))

cat("\n=== FIELD EXTRACTION SUCCESS RATES ===\n")
print(extraction_rates)

cat("\n=== SAMPLE EXTRACTED DATA ===\n")
sample_data <- df_final %>% 
  select(Title, Year, Authors, Country, SUoA_Type, Number_of_Units) %>% 
  head(5)
print(sample_data)

cat("\nDataset saved as: Output/regex_extracted_dataset.csv\n")
