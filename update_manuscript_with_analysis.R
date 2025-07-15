# Update Manuscript with Analysis Results
# This script runs the analysis and updates the manuscript with the latest results

# Function to handle errors
handle_error <- function(step_name, error_message) {
  cat("\n\n=============================================\n")
  cat("ERROR in", step_name, "\n")
  cat(error_message, "\n")
  cat("=============================================\n\n")
  stop("Process halted. Please fix the error and try again.")
}

# Check for required packages
cat("Checking required packages...\n")
required_packages <- c("readxl", "writexl", "dplyr", "ggplot2", "rmarkdown", "flextable", "tidyverse", "knitr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  handle_error("Package Check", paste("Missing required packages:", paste(missing_packages, collapse = ", "), 
                                    "\nPlease install them with: install.packages(c('", 
                                    paste(missing_packages, collapse = "', '"), "'))"))
} else {
  cat("All required packages are installed.\n")
}

# Get the current date in YYYYMMDD format
current_date <- format(Sys.Date(), "%Y%m%d")
cat("Running analysis for date:", current_date, "\n")

# Step 1: Run the analysis script to generate the latest results
cat("Running main analysis script...\n")
tryCatch({
  source("20250714_new_csv_analysis_clean.R")
}, error = function(e) {
  handle_error("Analysis Script", paste("Error running analysis script:", e$message))
})

# Step 2: Check that the Excel file was created
expected_path <- file.path(paste0(current_date, "_Analysis & Results"), 
                          paste0(current_date, "_Comprehensive_Manuscript_Tables.xlsx"))

if (!file.exists(expected_path)) {
  handle_error("File Check", paste("Analysis did not create the expected Excel file at:", expected_path))
} else {
  cat("Analysis complete! Excel file created at:", expected_path, "\n")
}

# Step 3: Render the R Markdown document to create the manuscript
cat("\nRendering manuscript with latest analysis results...\n")
output_file <- paste0(current_date, "_Systematic_Review_Manuscript.docx")

tryCatch({
  rmarkdown::render("Systematic_Review_Manuscript.Rmd", 
                   output_format = "word_document",
                   output_file = output_file)
  
  cat("\n=============================================\n")
  cat("SUCCESS! Manuscript updated with latest analysis results!\n")
  cat("=============================================\n")
  cat("Output file:", output_file, "\n")
  cat("You can now open this file to view your updated manuscript.\n")
  
}, error = function(e) {
  handle_error("Manuscript Rendering", paste("Error rendering R Markdown:", e$message, 
                                            "\nCheck the Rmd file for syntax errors."))
})
