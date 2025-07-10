# Extract supporting evidence for manuscript integration
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# Read the complete evidence dataset
data <- read_csv('20250710_Analysis & Results/20250710_standardized_unit_sizes_with_complete_evidence.csv', show_col_types = FALSE)

cat("Loaded", nrow(data), "studies with evidence\n")

# Define key evidence categories for manuscript integration
evidence_categories <- list(
  spatial_justification = 'Supporting quotes for "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION"',
  theoretical_framework = 'Supporting quotes for "THEORETICAL FRAMEWORK & OBJECTIVES"',
  data_constraints = 'Supporting quotes for "DATA LIMITATIONS & METHODOLOGICAL ISSUES"',
  scale_effects = 'Supporting quotes for "SCALE EFFECTS & SPATIAL FINDINGS"',
  methodology = 'Supporting quotes for "STUDY DESIGN & METHODOLOGY"',
  study_context = 'Supporting quotes for "STUDY CONTEXT & GEOGRAPHY"'
)

# Function to clean and extract meaningful quotes
extract_quotes <- function(quote_text) {
  if (is.na(quote_text) || quote_text == "" || quote_text == "-") {
    return(NULL)
  }
  
  # Split by bullet points or line breaks
  quotes <- str_split(quote_text, "\\n|\\s*-\\s*")[[1]]
  quotes <- quotes[quotes != "" & !str_detect(quotes, "^\\s*$")]
  
  # Clean quotes - remove extra quotes and trim
  quotes <- str_remove_all(quotes, '^["\'\\s]+|["\'\\s]+$')
  quotes <- str_trim(quotes)
  
  # Filter out very short or non-meaningful quotes
  quotes <- quotes[nchar(quotes) > 20]
  
  return(quotes)
}

# Extract evidence by theme
evidence_by_theme <- list()

for (theme in names(evidence_categories)) {
  col_name <- evidence_categories[[theme]]
  
  cat("\n=== EXTRACTING:", theme, "===\n")
  
  theme_evidence <- data %>%
    filter(!is.na(get(col_name)) & get(col_name) != "" & get(col_name) != "-") %>%
    select(Citation, Name_of_the_unit, Crime_Type, Unit_size_km2, Country, all_of(col_name)) %>%
    mutate(
      quotes = map(get(col_name), extract_quotes),
      num_quotes = map_int(quotes, length)
    ) %>%
    filter(num_quotes > 0) %>%
    arrange(Unit_size_km2)
  
  evidence_by_theme[[theme]] <- theme_evidence
  cat("Found", nrow(theme_evidence), "studies with", theme, "evidence\n")
  
  # Show top examples
  if (nrow(theme_evidence) > 0) {
    cat("Examples:\n")
    for (i in 1:min(3, nrow(theme_evidence))) {
      study <- theme_evidence[i, ]
      cat("- ", study$Citation, " (", study$Name_of_the_unit, ", ", study$Crime_Type, ")\n")
      if (length(study$quotes[[1]]) > 0) {
        quote <- study$quotes[[1]][1]
        cat("  Quote: \"", substr(quote, 1, 100), "...\"\n")
      }
    }
  }
}

# Create targeted extracts for specific manuscript sections

# 1. MICRO-ENVIRONMENTAL vs NEIGHBORHOOD SCALE EXAMPLES
scale_examples <- data %>%
  filter(!is.na(get(evidence_categories$spatial_justification))) %>%
  mutate(
    scale_category = case_when(
      Unit_size_km2 <= 0.01 ~ "Micro-environmental",
      Unit_size_km2 <= 1.0 ~ "Neighborhood",
      TRUE ~ "Administrative"
    )
  ) %>%
  select(Citation, Crime_Type, Name_of_the_unit, Unit_size_km2, scale_category, 
         all_of(evidence_categories$spatial_justification)) %>%
  arrange(Unit_size_km2)

cat("\n=== SCALE CATEGORY EXAMPLES ===\n")
for (scale in c("Micro-environmental", "Neighborhood", "Administrative")) {
  examples <- scale_examples %>% filter(scale_category == scale)
  cat("\n", scale, "scale (", nrow(examples), "studies):\n")
  
  if (nrow(examples) > 0) {
    for (i in 1:min(2, nrow(examples))) {
      study <- examples[i, ]
      cat("- ", study$Citation, " (", study$Unit_size_km2, " km², ", study$Crime_Type, ")\n")
    }
  }
}

# 2. THEORETICAL SOPHISTICATION EXAMPLES
theoretical_examples <- evidence_by_theme$theoretical_framework %>%
  head(5) %>%
  select(Citation, Crime_Type, quotes)

cat("\n=== THEORETICAL SOPHISTICATION EXAMPLES ===\n")
for (i in 1:nrow(theoretical_examples)) {
  study <- theoretical_examples[i, ]
  cat("- ", study$Citation, " (", study$Crime_Type, ")\n")
  if (length(study$quotes[[1]]) > 0) {
    cat("  Theory: \"", substr(study$quotes[[1]][1], 1, 120), "...\"\n")
  }
}

# 3. DATA CONSTRAINT EXAMPLES
constraint_examples <- evidence_by_theme$data_constraints %>%
  head(5) %>%
  select(Citation, Country, quotes)

cat("\n=== DATA CONSTRAINT EXAMPLES ===\n")
for (i in 1:nrow(constraint_examples)) {
  study <- constraint_examples[i, ]
  cat("- ", study$Citation, " (", study$Country, ")\n")
  if (length(study$quotes[[1]]) > 0) {
    cat("  Constraint: \"", substr(study$quotes[[1]][1], 1, 120), "...\"\n")
  }
}

# Save evidence extracts for manuscript integration
write_csv(scale_examples, "20250710_Analysis & Results/scale_justification_examples.csv")

for (theme in names(evidence_by_theme)) {
  filename <- paste0("20250710_Analysis & Results/", theme, "_evidence.csv")
  write_csv(evidence_by_theme[[theme]], filename)
}

cat("\n=== EVIDENCE EXTRACTION COMPLETE ===\n")
cat("Evidence files saved for manuscript integration\n")
cat("Ready to enhance manuscript with specific citations and quotes\n")
