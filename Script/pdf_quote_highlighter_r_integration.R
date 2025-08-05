# PDF Quote Highlighter - R Integration Script
# ==============================================
# This script provides R functions to highlight supporting quotes in PDF files
# using the Python PDF highlighter script.

# Required R packages
if (!require(reticulate)) install.packages("reticulate")
if (!require(here)) install.packages("here")

# Setup Python environment and check dependencies
setup_pdf_highlighter <- function() {
  cat("Setting up PDF Quote Highlighter...\n")
  
  # Check if Python is available
  if (!py_available()) {
    stop("Python is not available. Please install Python and try again.")
  }
  
  # Check/install required Python packages
  required_packages <- c("pymupdf", "pandas", "rapidfuzz", "pathlib")
  
  for (package in required_packages) {
    if (!py_module_available(package)) {
      cat(paste("Installing Python package:", package, "\n"))
      py_install(package)
    }
  }
  
  cat("PDF Quote Highlighter setup complete!\n")
}

# Function to run the PDF highlighter
highlight_quotes_in_pdfs <- function(csv_file, pdf_folder, output_folder = NULL, threshold = 80) {
  # Set default output folder if not provided
  if (is.null(output_folder)) {
    output_folder <- paste0(pdf_folder, "_highlighted")
  }
  
  # Get the path to the Python script
  script_path <- here::here("Script", "pdf_quote_highlighter.py")
  
  if (!file.exists(script_path)) {
    stop("PDF highlighter Python script not found at: ", script_path)
  }
  
  if (!file.exists(csv_file)) {
    stop("CSV file not found at: ", csv_file)
  }
  
  if (!dir.exists(pdf_folder)) {
    stop("PDF folder not found at: ", pdf_folder)
  }
  
  cat("Starting PDF quote highlighting...\n")
  cat("CSV file:", csv_file, "\n")
  cat("PDF folder:", pdf_folder, "\n")
  cat("Output folder:", output_folder, "\n")
  cat("Similarity threshold:", threshold, "\n\n")
  
  # Run the Python script
  python_cmd <- paste(
    "python", 
    shQuote(script_path),
    "--csv_file", shQuote(csv_file),
    "--pdf_folder", shQuote(pdf_folder),
    "--output_folder", shQuote(output_folder),
    "--threshold", threshold
  )
  
  # Execute the command
  result <- system(python_cmd, intern = FALSE)
  
  if (result == 0) {
    cat("\n✓ PDF highlighting completed successfully!\n")
    
    # Read and display the summary report
    report_file <- file.path(output_folder, "highlighting_report.csv")
    if (file.exists(report_file)) {
      cat("\nHighlighting Summary:\n")
      cat("====================\n")
      
      report <- read.csv(report_file, stringsAsFactors = FALSE)
      
      total_studies <- nrow(report)
      successful_studies <- sum(report$Highlighted_Quotes > 0, na.rm = TRUE)
      total_quotes <- sum(report$Total_Quotes, na.rm = TRUE)
      total_highlighted <- sum(report$Highlighted_Quotes, na.rm = TRUE)
      success_rate <- if (total_quotes > 0) round(total_highlighted / total_quotes * 100, 1) else 0
      
      cat("Total studies processed:", total_studies, "\n")
      cat("Studies with highlights:", successful_studies, "\n")
      cat("Total quotes:", total_quotes, "\n")
      cat("Successfully highlighted:", total_highlighted, "\n")
      cat("Overall success rate:", paste0(success_rate, "%"), "\n")
      
      # Show top successful studies
      if (nrow(report) > 0) {
        cat("\nTop Results:\n")
        report_sorted <- report[order(-report$Success_Rate), ]
        top_results <- head(report_sorted, 5)
        
        for (i in 1:nrow(top_results)) {
          row <- top_results[i, ]
          cat(sprintf("%d. %s - %d/%d quotes (%.1f%%)\n", 
                     i, 
                     substr(row$Study_Title, 1, 50), 
                     row$Highlighted_Quotes, 
                     row$Total_Quotes, 
                     row$Success_Rate))
        }
      }
      
      cat("\nDetailed reports saved to:", output_folder, "\n")
      return(list(
        success = TRUE,
        output_folder = output_folder,
        summary = list(
          total_studies = total_studies,
          successful_studies = successful_studies,
          total_quotes = total_quotes,
          total_highlighted = total_highlighted,
          success_rate = success_rate
        ),
        report_data = report
      ))
    }
  } else {
    cat("\n✗ PDF highlighting failed!\n")
    cat("Check the log file for details: pdf_highlighting.log\n")
    return(list(success = FALSE, error = "Python script execution failed"))
  }
}

# Function to create a test run with a small subset
test_pdf_highlighting <- function(csv_file, pdf_folder, num_studies = 3) {
  cat("Running PDF highlighting test with", num_studies, "studies...\n")
  
  # Read the CSV and select first few studies
  df <- read.csv(csv_file, stringsAsFactors = FALSE)
  test_df <- head(df, num_studies)
  
  # Create test CSV file
  test_csv <- "test_combined_dataset.csv"
  write.csv(test_df, test_csv, row.names = FALSE)
  
  # Run highlighting on test data
  result <- highlight_quotes_in_pdfs(
    csv_file = test_csv,
    pdf_folder = pdf_folder,
    output_folder = "Test_Highlighted_PDFs"
  )
  
  # Clean up test file
  if (file.exists(test_csv)) {
    file.remove(test_csv)
  }
  
  return(result)
}

# Function to analyze quote extraction results
analyze_quote_extraction <- function(csv_file) {
  cat("Analyzing quote extraction from dataset...\n")
  
  df <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Find all supporting quote columns
  quote_cols <- grep("Supporting_quotes_for", names(df), value = TRUE)
  
  cat("Found", length(quote_cols), "supporting quote columns:\n")
  
  quote_analysis <- data.frame(
    Variable = character(),
    Total_Studies = integer(),
    Studies_with_Quotes = integer(),
    Avg_Quote_Length = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (col in quote_cols) {
    variable_name <- gsub("Supporting_quotes_for_", "", col)
    variable_name <- gsub("_", " ", variable_name)
    
    # Count non-empty quotes
    non_empty <- df[[col]][!is.na(df[[col]]) & df[[col]] != "" & !df[[col]] %in% c("-", "N/A", "n/a", "NA")]
    
    # Calculate average quote length
    avg_length <- if (length(non_empty) > 0) round(mean(nchar(non_empty)), 1) else 0
    
    quote_analysis <- rbind(quote_analysis, data.frame(
      Variable = variable_name,
      Total_Studies = nrow(df),
      Studies_with_Quotes = length(non_empty),
      Avg_Quote_Length = avg_length,
      stringsAsFactors = FALSE
    ))
    
    cat(sprintf("  %s: %d/%d studies have quotes (avg length: %.1f chars)\n", 
               variable_name, length(non_empty), nrow(df), avg_length))
  }
  
  return(quote_analysis)
}

# Main function to integrate with existing workflow
run_pdf_quote_highlighting <- function(combined_dataset_path = NULL, pdf_folder = "Review_articles") {
  cat("=== PDF Quote Highlighting Workflow ===\n\n")
  
  # If no dataset path provided, look for the most recent one
  if (is.null(combined_dataset_path)) {
    # Look for combined dataset in analysis results folders
    pattern <- "\\d{8}_Analysis & Results"
    folders <- list.dirs(".", recursive = FALSE)
    analysis_folders <- folders[grepl(pattern, basename(folders))]
    
    if (length(analysis_folders) > 0) {
      # Get most recent folder
      latest_folder <- analysis_folders[order(basename(analysis_folders), decreasing = TRUE)][1]
      
      # Look for combined dataset
      csv_files <- list.files(latest_folder, pattern = "combined_dataset\\.csv", full.names = TRUE)
      if (length(csv_files) > 0) {
        combined_dataset_path <- csv_files[1]
        cat("Found combined dataset:", combined_dataset_path, "\n")
      }
    }
  }
  
  if (is.null(combined_dataset_path) || !file.exists(combined_dataset_path)) {
    stop("Combined dataset not found. Please provide the path to your combined_dataset.csv file.")
  }
  
  # Setup the highlighter
  setup_pdf_highlighter()
  
  # Analyze quote extraction
  cat("\n1. Analyzing quote extraction...\n")
  quote_analysis <- analyze_quote_extraction(combined_dataset_path)
  
  # Ask user if they want to proceed
  cat("\n2. Ready to highlight PDFs. Do you want to:")
  cat("\n   a) Run a test with 3 studies first")
  cat("\n   b) Process all studies")
  cat("\n   c) Cancel")
  
  choice <- readline("\nEnter your choice (a/b/c): ")
  
  if (tolower(choice) == "a") {
    cat("\nRunning test highlighting...\n")
    result <- test_pdf_highlighting(combined_dataset_path, pdf_folder)
  } else if (tolower(choice) == "b") {
    cat("\nRunning full highlighting...\n")
    result <- highlight_quotes_in_pdfs(combined_dataset_path, pdf_folder)
  } else {
    cat("Operation cancelled.\n")
    return(NULL)
  }
  
  return(result)
}

# Example usage:
# 1. Setup (run once)
# setup_pdf_highlighter()
#
# 2. Run highlighting
# result <- run_pdf_quote_highlighting()
#
# 3. Or specify paths manually
# result <- highlight_quotes_in_pdfs(
#   csv_file = "20250730_Analysis & Results/20250730_combined_dataset.csv",
#   pdf_folder = "Review_articles",
#   output_folder = "Review_articles_highlighted"
# )
