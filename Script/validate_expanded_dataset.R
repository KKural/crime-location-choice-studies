# Comprehensive validation of the expanded dataset
library(readr)
library(dplyr)

# Read the expanded dataset
expanded_data <- read_csv("Output/simple_expanded_dataset.csv")

cat("=== EXPANDED DATASET VALIDATION REPORT ===\n\n")

# Overall statistics
cat("Dataset Overview:\n")
cat("- Total articles:", nrow(expanded_data), "\n")
cat("- Total columns:", ncol(expanded_data), "\n")
cat("- Original columns:", 20, "\n")
cat("- Extracted columns:", ncol(expanded_data) - 1, "\n\n")

# Check basic study identification fields
basic_cols <- grep("BASIC_STUDY_IDENTIFICATION_", names(expanded_data), value = TRUE)
cat("Basic Study Identification Fields (", length(basic_cols), " fields):\n")
for(col in basic_cols) {
  non_empty <- sum(!is.na(expanded_data[[col]]) & expanded_data[[col]] != "")
  cat("- ", gsub("BASIC_STUDY_IDENTIFICATION_", "", col), ": ", non_empty, "/", nrow(expanded_data), " articles\n")
}
cat("\n")

# Check temporal scope fields
temporal_cols <- grep("TEMPORAL_SCOPE_DATA_SOURCES_", names(expanded_data), value = TRUE)
cat("Temporal Scope & Data Sources Fields (", length(temporal_cols), " fields):\n")
for(col in head(temporal_cols, 10)) {  # Show first 10
  non_empty <- sum(!is.na(expanded_data[[col]]) & expanded_data[[col]] != "")
  cat("- ", gsub("TEMPORAL_SCOPE_DATA_SOURCES_", "", col), ": ", non_empty, "/", nrow(expanded_data), " articles\n")
}
if(length(temporal_cols) > 10) cat("... and", length(temporal_cols) - 10, "more fields\n")
cat("\n")

# Check spatial units fields
spatial_cols <- grep("SPATIAL_UNITS_DESCRIPTION_JUSTIFICATION_", names(expanded_data), value = TRUE)
cat("Spatial Units Fields (", length(spatial_cols), " fields):\n")
for(col in head(spatial_cols, 10)) {  # Show first 10
  non_empty <- sum(!is.na(expanded_data[[col]]) & expanded_data[[col]] != "")
  cat("- ", gsub("SPATIAL_UNITS_DESCRIPTION_JUSTIFICATION_", "", col), ": ", non_empty, "/", nrow(expanded_data), " articles\n")
}
if(length(spatial_cols) > 10) cat("... and", length(spatial_cols) - 10, "more fields\n")
cat("\n")

# Sample data quality check
cat("Sample Data Quality Check:\n")
cat("First 3 articles - Basic Information:\n")
for(i in 1:min(3, nrow(expanded_data))) {
  cat("\nArticle", i, ":\n")
  cat("- Title:", expanded_data$BASIC_STUDY_IDENTIFICATION_Title[i], "\n")
  cat("- Year:", expanded_data$BASIC_STUDY_IDENTIFICATION_Year[i], "\n")
  cat("- Authors:", substr(expanded_data$BASIC_STUDY_IDENTIFICATION_Authors[i], 1, 60), "...\n")
}

# Check for all major field categories
field_categories <- c(
  "BASIC_STUDY_IDENTIFICATION_",
  "TEMPORAL_SCOPE_DATA_SOURCES_",
  "SPATIAL_UNITS_DESCRIPTION_JUSTIFICATION_",
  "STUDY_CONTEXT_GEOGRAPHY_",
  "SAMPLING_CHOICE_SETS_",
  "THEORETICAL_FRAMEWORK_OBJECTIVES_",
  "STUDY_DESIGN_METHODOLOGY_",
  "DATA_PREPARATION_PROCESSING_",
  "DEMOGRAPHIC_SOCIAL_VARIABLES_",
  "ECONOMIC_VARIABLES_",
  "TEMPORAL_CONTROL_VARIABLES_",
  "MODEL_FIT_PERFORMANCE_METRICS_",
  "MAJOR_FINDINGS_RESULTS_",
  "ENVIRONMENTAL_CRIME_ATTRACTOR_VARIABLES_",
  "DISTANCE_ACCESSIBILITY_VARIABLES_",
  "SCALE_EFFECTS_SPATIAL_FINDINGS_",
  "DATA_LIMITATIONS_METHODOLOGICAL_ISSUES_",
  "GENERALIZABILITY_COMPARATIVE_LIMITATIONS_",
  "IMPLICATIONS_FUTURE_DIRECTIONS_"
)

cat("\n\nField Category Coverage:\n")
for(category in field_categories) {
  cols_in_category <- grep(category, names(expanded_data), value = TRUE)
  if(length(cols_in_category) > 0) {
    # Calculate coverage (how many articles have data in at least one field of this category)
    coverage <- 0
    for(i in 1:nrow(expanded_data)) {
      has_data <- any(!is.na(expanded_data[i, cols_in_category]) & expanded_data[i, cols_in_category] != "")
      if(has_data) coverage <- coverage + 1
    }
    cat("- ", gsub("_$", "", category), ": ", length(cols_in_category), " fields, ", coverage, "/", nrow(expanded_data), " articles have data\n")
  }
}

cat("\n=== VALIDATION COMPLETE ===\n")
