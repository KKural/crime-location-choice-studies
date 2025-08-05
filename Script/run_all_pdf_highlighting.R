# Run PDF highlighting for ALL 50 studies
# This will take about 12-15 hours, so run overnight or during the day while working on other things

# Load the integration script
source("pdf_quote_highlighter_r_integration.R")

# Dynamic date handling like other scripts
today_date <- format(Sys.Date(), "%Y%m%d")

cat("Starting PDF highlighting for ALL 50 studies...\n")
cat("This will take approximately 12-15 hours.\n")
cat("You can safely close R and it will continue running.\n")
cat("Progress will be logged to pdf_highlighting.log\n\n")
cat(paste("Using today's date:", today_date, "\n\n"))

# Run the full highlighting process with dynamic paths
result <- highlight_quotes_in_pdfs(
  csv_file = paste0("../", today_date, "_Analysis & Results/", today_date, "_combined_dataset.csv"),
  pdf_folder = "../Review_articles",
  output_folder = paste0("../", today_date, "_Analysis & Results/Highlighted_PDFs"),
  threshold = 80  # Good balance of accuracy vs coverage
)

# Print final results
if (result$success) {
  cat("\n🎉 SUCCESS! ALL PDF highlighting completed!\n")
  cat("===========================================\n")
  cat("Studies processed:", result$summary$total_studies, "\n")
  cat("Studies with highlights:", result$summary$successful_studies, "\n")
  cat("Total quotes found:", result$summary$total_quotes, "\n")
  cat("Quotes highlighted:", result$summary$total_highlighted, "\n")
  cat("Overall success rate:", paste0(round(result$summary$success_rate, 1), "%"), "\n")
  cat("Output folder:", result$output_folder, "\n")
  cat("===========================================\n")
  
  # Save completion timestamp
  writeLines(paste("PDF highlighting completed at:", Sys.time()), 
             "pdf_highlighting_completed.txt")
  
} else {
  cat("\n❌ Process failed:", result$error, "\n")
  cat("Check the log file: pdf_highlighting.log\n")
}
