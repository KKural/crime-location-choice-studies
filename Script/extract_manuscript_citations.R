# Extract and format supporting evidence for manuscript integration
library(dplyr)
library(readr)
library(stringr)

# Read the complete evidence dataset
evidence_data <- read_csv('20250710_Analysis & Results/20250710_standardized_unit_sizes_with_complete_evidence.csv', show_col_types = FALSE)

cat("Loaded dataset with", nrow(evidence_data), "studies\n")
cat("Studies with evidence:", sum(evidence_data$Has_Supporting_Evidence, na.rm = TRUE), "\n")

# Function to clean and format quotes
format_quote <- function(quote_text, citation, max_length = 200) {
  if (is.na(quote_text) || quote_text == "" || quote_text == "-") {
    return(NULL)
  }
  
  # Clean the quote
  clean_quote <- str_trim(quote_text)
  clean_quote <- str_remove_all(clean_quote, '^[-•\\s]+')  # Remove leading bullets/dashes
  
  # Truncate if too long
  if (nchar(clean_quote) > max_length) {
    clean_quote <- paste0(str_sub(clean_quote, 1, max_length), "...")
  }
  
  # Format with citation
  paste0('"', clean_quote, '" (', citation, ')')
}

# Extract key evidence categories for manuscript
spatial_units_col <- 'Supporting quotes for "SPATIAL UNITS - DESCRIPTION & JUSTIFICATION"'
scale_effects_col <- 'Supporting quotes for "SCALE EFFECTS & SPATIAL FINDINGS"'
limitations_col <- 'Supporting quotes for "DATA LIMITATIONS & METHODOLOGICAL ISSUES"'
theoretical_col <- 'Supporting quotes for "THEORETICAL FRAMEWORK & OBJECTIVES"'

# 1. SPATIAL UNITS JUSTIFICATION QUOTES (for Methods/Results sections)
cat("\n=== SPATIAL UNITS JUSTIFICATION QUOTES ===\n")
spatial_evidence <- evidence_data %>%
  filter(!is.na(.data[[spatial_units_col]]) & .data[[spatial_units_col]] != "" & .data[[spatial_units_col]] != "-") %>%
  select(Citation, Name_of_the_unit, Crime_Type, Unit_size_km2, all_of(spatial_units_col)) %>%
  arrange(Unit_size_km2)

if (nrow(spatial_evidence) > 0) {
  cat("Examples of spatial unit justifications by size:\n\n")
  
  # Micro-environmental (< 0.01 km²)
  micro <- spatial_evidence %>% filter(Unit_size_km2 < 0.01)
  if (nrow(micro) > 0) {
    cat("MICRO-ENVIRONMENTAL UNITS (< 0.01 km²):\n")
    for (i in 1:min(3, nrow(micro))) {
      quote <- format_quote(micro[[spatial_units_col]][i], micro$Citation[i])
      if (!is.null(quote)) {
        cat("- ", quote, "\n")
      }
    }
    cat("\n")
  }
  
  # Neighborhood-level (0.01-1.0 km²)
  neighborhood <- spatial_evidence %>% filter(Unit_size_km2 >= 0.01 & Unit_size_km2 <= 1.0)
  if (nrow(neighborhood) > 0) {
    cat("NEIGHBORHOOD-LEVEL UNITS (0.01-1.0 km²):\n")
    for (i in 1:min(3, nrow(neighborhood))) {
      quote <- format_quote(neighborhood[[spatial_units_col]][i], neighborhood$Citation[i])
      if (!is.null(quote)) {
        cat("- ", quote, "\n")
      }
    }
    cat("\n")
  }
  
  # Administrative (> 1.0 km²)
  admin <- spatial_evidence %>% filter(Unit_size_km2 > 1.0)
  if (nrow(admin) > 0) {
    cat("ADMINISTRATIVE UNITS (> 1.0 km²):\n")
    for (i in 1:min(3, nrow(admin))) {
      quote <- format_quote(admin[[spatial_units_col]][i], admin$Citation[i])
      if (!is.null(quote)) {
        cat("- ", quote, "\n")
      }
    }
    cat("\n")
  }
}

# 2. SCALE EFFECTS QUOTES (for Discussion section)
cat("\n=== SCALE EFFECTS & SPATIAL FINDINGS ===\n")
scale_evidence <- evidence_data %>%
  filter(!is.na(.data[[scale_effects_col]]) & .data[[scale_effects_col]] != "" & .data[[scale_effects_col]] != "-") %>%
  select(Citation, Name_of_the_unit, Crime_Type, all_of(scale_effects_col))

if (nrow(scale_evidence) > 0) {
  cat("Examples for Discussion section:\n\n")
  for (i in 1:min(5, nrow(scale_evidence))) {
    quote <- format_quote(scale_evidence[[scale_effects_col]][i], scale_evidence$Citation[i], max_length = 300)
    if (!is.null(quote)) {
      cat("- ", quote, "\n\n")
    }
  }
}

# 3. DATA LIMITATIONS (for Limitations section)
cat("\n=== DATA LIMITATIONS & METHODOLOGICAL ISSUES ===\n")
limitations_evidence <- evidence_data %>%
  filter(!is.na(.data[[limitations_col]]) & .data[[limitations_col]] != "" & .data[[limitations_col]] != "-") %>%
  select(Citation, Name_of_the_unit, Crime_Type, all_of(limitations_col))

if (nrow(limitations_evidence) > 0) {
  cat("Examples for Limitations section:\n\n")
  for (i in 1:min(5, nrow(limitations_evidence))) {
    quote <- format_quote(limitations_evidence[[limitations_col]][i], limitations_evidence$Citation[i], max_length = 250)
    if (!is.null(quote)) {
      cat("- ", quote, "\n\n")
    }
  }
}

# 4. THEORETICAL FRAMEWORK (for Introduction/Discussion)
cat("\n=== THEORETICAL FRAMEWORK & OBJECTIVES ===\n")
theoretical_evidence <- evidence_data %>%
  filter(!is.na(.data[[theoretical_col]]) & .data[[theoretical_col]] != "" & .data[[theoretical_col]] != "-") %>%
  select(Citation, Name_of_the_unit, Crime_Type, all_of(theoretical_col))

if (nrow(theoretical_evidence) > 0) {
  cat("Examples for Introduction/Discussion:\n\n")
  for (i in 1:min(5, nrow(theoretical_evidence))) {
    quote <- format_quote(theoretical_evidence[[theoretical_col]][i], theoretical_evidence$Citation[i], max_length = 300)
    if (!is.null(quote)) {
      cat("- ", quote, "\n\n")
    }
  }
}

# 5. CREATE REFERENCE LOOKUP TABLE
cat("\n=== CREATING REFERENCE LOOKUP TABLE ===\n")
reference_table <- evidence_data %>%
  filter(Has_Supporting_Evidence) %>%
  select(Citation, Title_of_the_study, Year.x, Country, Crime_Type, Name_of_the_unit, Unit_size_km2) %>%
  arrange(Citation)

write_csv(reference_table, '20250710_Analysis & Results/manuscript_reference_table.csv')
cat("Reference table saved to: manuscript_reference_table.csv\n")

# 6. SUMMARY STATISTICS FOR MANUSCRIPT
cat("\n=== SUMMARY FOR MANUSCRIPT ===\n")
cat("Total studies analyzed:", nrow(evidence_data), "\n")
cat("Studies with supporting evidence:", sum(evidence_data$Has_Supporting_Evidence, na.rm = TRUE), "\n")
cat("Coverage percentage:", round(100 * sum(evidence_data$Has_Supporting_Evidence, na.rm = TRUE) / nrow(evidence_data), 1), "%\n")

# Count by evidence type
evidence_counts <- evidence_data %>%
  summarise(
    spatial_units = sum(!is.na(.data[[spatial_units_col]]) & .data[[spatial_units_col]] != "" & .data[[spatial_units_col]] != "-"),
    scale_effects = sum(!is.na(.data[[scale_effects_col]]) & .data[[scale_effects_col]] != "" & .data[[scale_effects_col]] != "-"),
    limitations = sum(!is.na(.data[[limitations_col]]) & .data[[limitations_col]] != "" & .data[[limitations_col]] != "-"),
    theoretical = sum(!is.na(.data[[theoretical_col]]) & .data[[theoretical_col]] != "" & .data[[theoretical_col]] != "-")
  )

cat("\nEvidence availability by type:\n")
cat("- Spatial units justification:", evidence_counts$spatial_units, "studies\n")
cat("- Scale effects findings:", evidence_counts$scale_effects, "studies\n")
cat("- Data limitations:", evidence_counts$limitations, "studies\n")
cat("- Theoretical framework:", evidence_counts$theoretical, "studies\n")

cat("\nScript complete! Use the quotes above to enhance your manuscript.\n")
