library(dplyr)
library(readr)
library(stringr)
library(tibble)

# === LOAD AND CLEAN ===
elicit_df <- read_csv("Data/Elicit - Extract.csv", show_col_types = FALSE) %>%
  rename(core_infor = `core infor`) %>%
  mutate(core_infor = str_replace_all(core_infor, "\r\n|\r", "\n"))

# === FIELD EXTRACTORS ===
extract_field <- function(text, field_label) {
  pattern <- paste0("\\*\\*", field_label, ":\\*\\*\\s*(.*?)\\s*(\\n|$)")
  match <- str_match(text, pattern)
  ifelse(is.na(match[,2]), NA, str_trim(match[,2]))
}
extract_fields <- function(df, field) {
  sapply(df$core_infor, function(txt) extract_field(txt, field))
}

# === VARIABLE BLOCK EXTRACTOR ===
extract_variable_block <- function(text) {
  pattern <- "## COMPLETE INDIVIDUAL VARIABLE LIST\\n(.*?)\\n\\n## VARIABLE SUMMARY COUNTS"
  match <- str_match(text, pattern)
  if (!is.na(match[2])) return(match[2]) else return(NA)
}

# === CATEGORY VARIABLE EXTRACTOR ===
extract_variables <- function(text, section_title) {
  if (is.na(text)) return(NA)
  pattern <- paste0("- \\*\\*", section_title, ":\\*\\*\\n((?:\\s+- .+\\n?)*)")
  match <- str_match(text, pattern)
  if (is.na(match[,2])) return(NA)
  vars <- str_extract_all(match[,2], "\\s+-\\s+.+")[[1]]
  vars_clean <- str_trim(str_replace(vars, "^-\\s+", ""))
  return(vars_clean)
}

# === VARIABLE CATEGORIES ===
categories <- c(
  "DEMOGRAPHIC VARIABLES", "ECONOMIC VARIABLES", "LAND USE VARIABLES",
  "INFRASTRUCTURE VARIABLES", "DISTANCE/ACCESSIBILITY VARIABLES",
  "CRIME OPPORTUNITY VARIABLES", "SOCIAL/BEHAVIORAL VARIABLES", "TEMPORAL/CONTROL VARIABLES"
)

# === ADD ALL FIELDS + VARIABLE BLOCK TO DATA ===
elicit_df_parsed <- elicit_df %>%
  mutate(
    Title = extract_fields(., "Title"),
    Year = extract_fields(., "Year"),
    Authors = extract_fields(., "Authors"),
    Country = extract_fields(., "Country"),
    City = extract_fields(., "City/Region"),
    Study_Area_Size = extract_fields(., "Study Area Size"),
    Crime_Type = extract_fields(., "Crime Type"),
    Study_Period = extract_fields(., "Study Period"),
    SUoA_Type = extract_fields(., "SUoA Type"),
    Study_Design = extract_fields(., "Study Design"),
    Stat_Method = extract_fields(., "Statistical Method"),
    Model_Type = extract_fields(., "Model Type"),
    Software_Used = extract_fields(., "Software Used"),
    Sample_Size = extract_fields(., "Sample Size"),
    Main_Results = extract_fields(., "Main Results"),
    Significant_Predictors = extract_fields(., "Significant Predictors"),
    Model_Performance = extract_fields(., "Model Performance"),
    Extraction_Confidence = extract_fields(., "Extraction Confidence"),
    variable_block = sapply(core_infor, extract_variable_block)
  )

# === ADD VARIABLES AND COUNTS IN A LOOP ===
for (cat in categories) {
  elicit_df_parsed[[cat]] <- sapply(elicit_df_parsed$variable_block, function(text) {
    vars <- extract_variables(text, cat)
    if (is.na(vars) || length(vars) == 0) return(NA)
    paste(vars, collapse = "; ")
  })
  
  elicit_df_parsed[[paste0(cat, "_COUNT")]] <- sapply(elicit_df_parsed[[cat]], function(vars_str) {
    if (is.na(vars_str)) return(0)
    length(str_split(vars_str, ";")[[1]])
  })
}

# === EXPORT AND VIEW ===
View(elicit_df_parsed)
write_csv(elicit_df_parsed, "Data/elicit_with_metadata_and_variables.csv")
