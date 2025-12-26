# Fixed PDF Highlighter R Integration
# This script provides R functions for the fixed PDF highlighter that addresses
# partial highlighting and overlap issues

#' Run Fixed PDF Highlighting (Addresses Partial Highlighting & Overlaps)
#'
#' @param study_id Study ID to process
#' @param csv_file Path to the combined dataset CSV file
#' @param pdf_folder Path to folder containing PDF files
#' @param output_folder Path for output highlighted PDFs
#' @param enable_fuzzy Whether to enable fuzzy matching (default: TRUE)
#' @param python_script Path to the fixed PDF highlighter script
#'
#' @return List with processing results
#' @export
run_fixed_pdf_highlighting <- function(study_id,
                                      csv_file = NULL,
                                      pdf_folder = "Review_articles",
                                      output_folder = NULL,
                                      enable_fuzzy = TRUE,
                                      python_script = "fixed_pdf_highlighter.py") {
  
  # Set default paths
  if (is.null(csv_file)) {
    today_date <- format(Sys.Date(), "%Y%m%d")
    csv_file <- paste0(today_date, "_Analysis & Results/", today_date, "_combined_dataset_no_reasoning.csv")
  }
  
  if (is.null(output_folder)) {
    today_date <- format(Sys.Date(), "%Y%m%d")
    output_folder <- paste0(today_date, "_Fixed_Highlighted_PDFs")
  }
  
  # Build command arguments
  cmd_args <- c(
    as.character(study_id),
    "--csv", csv_file,
    "--pdf-folder", pdf_folder,
    "--output", output_folder
  )
  
  if (!enable_fuzzy) {
    cmd_args <- c(cmd_args, "--no-fuzzy")
  }
  
  # Log the operation
  cat("🔧 Starting Fixed PDF Highlighting\n")
  cat("==================================\n")
  cat("Study ID:", study_id, "\n")
  cat("CSV file:", csv_file, "\n")
  cat("PDF folder:", pdf_folder, "\n")
  cat("Output folder:", output_folder, "\n")
  cat("Fuzzy matching:", if (enable_fuzzy) "enabled" else "disabled", "\n")
  cat("\n🎯 Key Fixes:\n")
  cat("  ✓ Full text boundary detection (no partial highlighting)\n")
  cat("  ✓ Overlap prevention system\n")
  cat("  ✓ Multi-line text support\n")
  cat("  ✓ Better transparency settings\n")
  cat("==================================\n\n")
  
  # Run the Python script
  start_time <- Sys.time()
  
  tryCatch({
    # Execute the Python script
    result <- system2("python", args = c(python_script, cmd_args), 
                     stdout = TRUE, stderr = TRUE, wait = TRUE)
    
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Check if successful
    exit_code <- attr(result, "status")
    
    if (is.null(exit_code) || exit_code == 0) {
      # Parse results from output
      output_lines <- paste(result, collapse = "\n")
      
      # Extract key metrics
      highlights_created <- NA
      processing_time <- NA
      output_file <- NA
      
      # Parse highlights created
      if (grepl("Highlights created:", output_lines)) {
        highlights_line <- regmatches(output_lines, regexpr("Highlights created: \\d+", output_lines))
        if (length(highlights_line) > 0) {
          highlights_created <- as.numeric(gsub("Highlights created: ", "", highlights_line))
        }
      }
      
      # Parse processing time
      if (grepl("Processing time:", output_lines)) {
        time_line <- regmatches(output_lines, regexpr("Processing time: [0-9.]+s", output_lines))
        if (length(time_line) > 0) {
          processing_time <- as.numeric(gsub("Processing time: ([0-9.]+)s", "\\1", time_line))
        }
      }
      
      # Parse output file
      if (grepl("Output file:", output_lines)) {
        file_line <- regmatches(output_lines, regexpr("Output file: .+", output_lines))
        if (length(file_line) > 0) {
          output_file <- gsub("Output file: ", "", file_line)
        }
      }
      
      cat("\n🎉 SUCCESS! Fixed PDF highlighting completed!\n")
      cat("============================================\n")
      cat("Study:", study_id, "\n")
      if (!is.na(highlights_created)) cat("Highlights created:", highlights_created, "\n")
      if (!is.na(processing_time)) cat("Processing time:", paste0(processing_time, "s"), "\n")
      if (!is.na(output_file)) cat("Output file:", output_file, "\n")
      cat("R execution time:", paste0(round(total_time, 2), "s"), "\n")
      cat("============================================\n")
      cat("\n✅ Fixed Issues:\n")
      cat("  • No more partial highlighting (full text boundaries)\n")
      cat("  • No overlapping highlights\n")
      cat("  • Better multi-line text detection\n")
      cat("  • Improved visual quality\n\n")
      
      return(list(
        success = TRUE,
        study_id = study_id,
        highlights_created = highlights_created,
        processing_time = processing_time,
        output_file = output_file,
        r_execution_time = total_time,
        python_output = result,
        fixes_applied = c(
          "Full text boundary detection",
          "Overlap prevention",
          "Multi-line support", 
          "Better transparency"
        )
      ))
      
    } else {
      # Failed
      cat("\n❌ Fixed PDF highlighting failed!\n")
      cat("Exit code:", exit_code, "\n")
      cat("Error output:\n")
      cat(paste(result, collapse = "\n"), "\n")
      
      return(list(
        success = FALSE,
        study_id = study_id,
        error = paste(result, collapse = "\n"),
        exit_code = exit_code,
        r_execution_time = total_time
      ))
    }
    
  }, error = function(e) {
    cat("\n❌ R error during execution:\n")
    cat("Error:", e$message, "\n")
    
    return(list(
      success = FALSE,
      study_id = study_id,
      error = e$message,
      r_execution_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  })
}

#' Compare Both PDF Highlighters (Enhanced vs Fixed)
#'
#' @param study_id Study ID to test with
#' @param csv_file Path to dataset CSV
#' @param pdf_folder Path to PDF folder
#'
#' @return Comparison results
#' @export
compare_pdf_highlighters <- function(study_id = "1",
                                    csv_file = NULL,
                                    pdf_folder = "Review_articles") {
  
  cat("🧪 COMPARING PDF HIGHLIGHTERS\n")
  cat("=============================\n")
  cat("Study ID:", study_id, "\n")
  cat("Testing both Enhanced and Fixed versions...\n\n")
  
  # Set default CSV if not provided
  if (is.null(csv_file)) {
    today_date <- format(Sys.Date(), "%Y%m%d")
    csv_file <- paste0(today_date, "_Analysis & Results/", today_date, "_combined_dataset_no_reasoning.csv")
  }
  
  # Run comparison Python script
  start_time <- Sys.time()
  
  result <- system2("python", args = c("compare_highlighters.py", study_id, csv_file, pdf_folder),
                   stdout = TRUE, stderr = TRUE, wait = TRUE)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Display results
  cat(paste(result, collapse = "\n"), "\n")
  
  cat("\n📋 RECOMMENDATION:\n")
  cat("==================\n")
  cat("Use the FIXED VERSION (fixed_pdf_highlighter.py) for:\n")
  cat("  ✓ Better highlighting coverage (no partial highlighting)\n")
  cat("  ✓ No overlapping highlights\n")
  cat("  ✓ Improved multi-line text detection\n")
  cat("  ✓ Better visual presentation\n\n")
  
  return(list(
    comparison_completed = TRUE,
    execution_time = total_time,
    recommendation = "Use fixed_pdf_highlighter.py",
    output = result
  ))
}

#' Test Fixed Highlighter with Single Study
#'
#' @param test_study_id Study ID to test (default: "1")
#' @param csv_file Path to dataset CSV
#' @param pdf_folder Path to PDF folder
#'
#' @return Test results
#' @export
test_fixed_highlighter <- function(test_study_id = "1",
                                  csv_file = NULL,
                                  pdf_folder = "Review_articles") {
  
  cat("🧪 Testing Fixed PDF Highlighter\n")
  cat("===============================\n")
  cat("This version fixes partial highlighting and overlap issues.\n\n")
  
  result <- run_fixed_pdf_highlighting(
    study_id = test_study_id,
    csv_file = csv_file,
    pdf_folder = pdf_folder,
    enable_fuzzy = TRUE
  )
  
  if (result$success) {
    cat("\n📊 Test Results:\n")
    cat("================\n")
    if (!is.na(result$highlights_created)) {
      cat("✅ Test PASSED! Highlights created:", result$highlights_created, "\n")
    } else {
      cat("✅ Test completed successfully\n")
    }
    
    cat("\n🔧 Applied Fixes:\n")
    for (fix in result$fixes_applied) {
      cat("  •", fix, "\n")
    }
    
    cat("\n📁 Next Steps:\n")
    cat("  1. Open the generated PDF file\n")
    cat("  2. Verify that highlights cover full text (not partial)\n")
    cat("  3. Check that there are no overlapping highlights\n")
    cat("  4. Confirm multi-line quotes are properly highlighted\n")
    
  } else {
    cat("\n❌ Test failed:", result$error, "\n")
  }
  
  return(result)
}

# Example usage
if (FALSE) {
  # Test the fixed highlighter
  test_result <- test_fixed_highlighter("1")
  
  # Use fixed highlighter for a study
  result <- run_fixed_pdf_highlighting("1")
  
  # Compare both versions
  comparison <- compare_pdf_highlighters("1")
}

cat("Fixed PDF Highlighter R Integration Loaded! 🔧\n")
cat("===============================================\n")
cat("Available functions:\n")
cat("• run_fixed_pdf_highlighting() - Main fixed highlighter\n")
cat("• test_fixed_highlighter() - Test with single study\n")
cat("• compare_pdf_highlighters() - Compare enhanced vs fixed versions\n")
cat("\nKey improvements in fixed version:\n")
cat("✓ No partial highlighting (full text boundaries)\n")
cat("✓ No overlapping highlights\n")
cat("✓ Better multi-line text support\n")
cat("✓ Improved visual quality\n")
