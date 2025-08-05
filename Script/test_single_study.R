# Test PDF highlighting with just ONE study
# This will be much faster to see results

# Load the integration script
source("pdf_quote_highlighter_r_integration.R")

# Test with just 1 study (should take 1-2 minutes max)
cat("Testing PDF highlighting with 1 study...\n")
result <- test_pdf_highlighting(
  csv_file = "../20250730_Analysis & Results/20250730_combined_dataset.csv",
  pdf_folder = "../Review_articles",
  num_studies = 1  # Just ONE study
)

# Print results
if (result$success) {
  cat("\n✅ SUCCESS! PDF highlighting completed!\n")
  cat("Studies processed:", result$summary$total_studies, "\n")
  cat("Total quotes found:", result$summary$total_quotes, "\n")
  cat("Quotes highlighted:", result$summary$total_highlighted, "\n")
  cat("Success rate:", paste0(round(result$summary$success_rate, 1), "%"), "\n")
  cat("Output folder:", result$output_folder, "\n")
} else {
  cat("\n❌ Failed:", result$error, "\n")
}
