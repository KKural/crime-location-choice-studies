# Check which Elicit files contain which supporting evidence columns
library(dplyr)
library(readr)
library(stringr)

# List of Elicit files
elicit_files <- c(
  "Data/Elicit - New table - BASIC STUDY IDENTIFICATION TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - DESCRIPTION _ JUSTIFICATION STUDY CONTEXT _ G.csv",
  "Data/Elicit - New table - THEORETICAL FRAMEWORK _ OBJECTIVES STUDY DESIGN _ METHODOLOGY DATA PREPARATION _ PROCESSING DEMOGRAPHIC _ SOCIAL VARIA.csv",
  "Data/Elicit - New table - TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS MAJOR FINDINGS _ RESULTS ENVIRONMENTAL _ CRIME ATTRACTOR .csv",
  "Data/Elicit - New table - SCALE EFFECTS _ SPATIAL FINDINGS DATA LIMITATIONS _ METHODOLOGICAL ISSUES GENERALIZABILITY _ COMPARATIVE LIMITATIONS I.csv"
)

# Check each file for supporting quote columns
for (file_path in elicit_files) {
  if (file.exists(file_path)) {
    cat("\n=== FILE:", basename(file_path), "===\n")
    
    tryCatch({
      file_data <- read_csv(file_path, show_col_types = FALSE)
      
      # Find supporting quote columns
      quote_cols <- names(file_data)[str_detect(names(file_data), "Supporting quotes")]
      cat("Supporting quote columns found:", length(quote_cols), "\n")
      
      if (length(quote_cols) > 0) {
        for (col in quote_cols) {
          # Count non-empty quotes
          non_empty <- sum(!is.na(file_data[[col]]) & file_data[[col]] != '' & file_data[[col]] != '-', na.rm = TRUE)
          cat("  -", col, ":", non_empty, "studies\n")
        }
      }
      
      # Find reasoning columns
      reasoning_cols <- names(file_data)[str_detect(names(file_data), "Reasoning")]
      cat("Reasoning columns found:", length(reasoning_cols), "\n")
      
    }, error = function(e) {
      cat("Error reading file:", e$message, "\n")
    })
  }
}

cat("\n=== SUMMARY ===\n")
cat("The issue is that different types of supporting evidence are in different CSV files.\n")
cat("You need to ensure all 4 Elicit CSV files are being processed and merged correctly.\n")
