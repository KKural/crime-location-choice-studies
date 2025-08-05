# Test improved highlighting on one study to check accuracy
library(reticulate)
library(here)

# Set working directory
setwd(here())

# Source the Python highlighting functions
source("pdf_quote_highlighter_r_integration.R")

# Test on one study
csv_file <- "../20250730_Analysis & Results/20250730_combined_dataset.csv"
pdf_folder <- "../Review_articles"
output_folder <- "../20250730_Analysis & Results/Test_Improved_Highlighting"

# Create output directory
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

cat("Testing improved highlighting algorithm on Study 1...\n")

# Test just the first study
result <- highlight_single_study(
  csv_file = csv_file,
  pdf_folder = pdf_folder,
  output_folder = output_folder,
  study_index = 1,  # First study
  threshold = 85    # Higher threshold for better precision
)

cat("Test result:", result, "\n")
cat("Check the output in:", output_folder, "\n")
