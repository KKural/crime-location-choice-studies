# Enhanced PDF Highlighter R Integration
# This script provides R functions to work with the enhanced Python PDF highlighter

#' Run Enhanced PDF Highlighting for Studies
#'
#' @param study_ids Character vector of study IDs to process, or "all" for all studies
#' @param csv_file Path to the combined dataset CSV file
#' @param pdf_folder Path to folder containing PDF files
#' @param output_folder Path for output highlighted PDFs
#' @param max_workers Number of parallel workers (default: 4)
#' @param enable_fuzzy Whether to enable fuzzy matching (default: TRUE)
#' @param python_script Path to the enhanced PDF highlighter script
#'
#' @return List with processing results and summary statistics
#' @export
run_enhanced_pdf_highlighting <- function(study_ids = "all",
                                         csv_file = NULL,
                                         pdf_folder = "Review_articles",
                                         output_folder = NULL,
                                         max_workers = 4,
                                         enable_fuzzy = TRUE,
                                         python_script = "enhanced_pdf_highlighter.py") {
  
  # Set default paths based on current date
  if (is.null(csv_file)) {
    today_date <- format(Sys.Date(), "%Y%m%d")
    csv_file <- paste0(today_date, "_Analysis & Results/", today_date, "_combined_dataset_no_reasoning.csv")
  }
  
  if (is.null(output_folder)) {
    today_date <- format(Sys.Date(), "%Y%m%d")
    output_folder <- paste0(today_date, "_Enhanced_Highlighted_PDFs")
  }
  
  # Prepare study IDs argument
  if (length(study_ids) == 1 && study_ids == "all") {
    study_ids_arg <- "all"
  } else {
    study_ids_arg <- paste(study_ids, collapse = ",")
  }
  
  # Build command
  cmd_args <- c(
    study_ids_arg,
    "--csv", csv_file,
    "--pdf-folder", pdf_folder,
    "--output", output_folder,
    "--workers", as.character(max_workers)
  )
  
  if (!enable_fuzzy) {
    cmd_args <- c(cmd_args, "--no-fuzzy")
  }
  
  # Log the operation
  cat("Starting Enhanced PDF Highlighting\n")
  cat("==================================\n")
  cat("Study IDs:", if (study_ids_arg == "all") "all studies" else study_ids_arg, "\n")
  cat("CSV file:", csv_file, "\n")
  cat("PDF folder:", pdf_folder, "\n")
  cat("Output folder:", output_folder, "\n")
  cat("Workers:", max_workers, "\n")
  cat("Fuzzy matching:", if (enable_fuzzy) "enabled" else "disabled", "\n")
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
      # Success - try to read the results file
      results_file <- file.path(output_folder, "processing_results.json")
      
      if (file.exists(results_file)) {
        # Read detailed results
        results_json <- jsonlite::fromJSON(results_file)
        
        cat("\n🎉 SUCCESS! Enhanced PDF highlighting completed!\n")
        cat("===============================================\n")
        cat("Studies processed:", results_json$summary$total_studies, "\n")
        cat("Successful:", results_json$summary$successful_studies, "\n")
        cat("Success rate:", paste0(round(results_json$summary$success_rate, 1), "%"), "\n")
        cat("Total highlights:", results_json$summary$total_highlights_created, "\n")
        cat("Total quotes processed:", results_json$summary$total_quotes_processed, "\n")
        cat("Processing time:", paste0(round(results_json$summary$total_processing_time, 2), "s"), "\n")
        cat("Output folder:", output_folder, "\n")
        cat("Results file:", results_file, "\n")
        cat("===============================================\n")
        
        return(list(
          success = TRUE,
          summary = results_json$summary,
          study_results = results_json$study_results,
          output_folder = output_folder,
          results_file = results_file,
          r_processing_time = total_time,
          python_output = result
        ))
        
      } else {
        cat("\n✅ Process completed successfully, but results file not found.\n")
        cat("Output folder:", output_folder, "\n")
        
        return(list(
          success = TRUE,
          summary = NULL,
          output_folder = output_folder,
          r_processing_time = total_time,
          python_output = result
        ))
      }
      
    } else {
      # Failed
      cat("\n❌ Enhanced PDF highlighting failed!\n")
      cat("Exit code:", exit_code, "\n")
      cat("Error output:\n")
      cat(paste(result, collapse = "\n"), "\n")
      
      return(list(
        success = FALSE,
        error = paste(result, collapse = "\n"),
        exit_code = exit_code,
        r_processing_time = total_time
      ))
    }
    
  }, error = function(e) {
    cat("\n❌ R error during execution:\n")
    cat("Error:", e$message, "\n")
    
    return(list(
      success = FALSE,
      error = e$message,
      r_processing_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  })
}

#' Highlight Quotes for Specific Studies (Wrapper Function)
#'
#' @param study_ids Vector of study IDs to process
#' @param csv_file Path to the dataset CSV file
#' @param pdf_folder Path to PDF folder
#' @param output_folder Output folder (optional, will use date-based default)
#' @param fast_mode If TRUE, disables fuzzy matching for faster processing
#' @param parallel_workers Number of parallel workers
#'
#' @return Processing results
#' @export
highlight_studies_enhanced <- function(study_ids,
                                      csv_file = NULL,
                                      pdf_folder = "Review_articles",
                                      output_folder = NULL,
                                      fast_mode = FALSE,
                                      parallel_workers = 4) {
  
  return(run_enhanced_pdf_highlighting(
    study_ids = study_ids,
    csv_file = csv_file,
    pdf_folder = pdf_folder,
    output_folder = output_folder,
    max_workers = parallel_workers,
    enable_fuzzy = !fast_mode
  ))
}

#' Process All Studies with Enhanced Highlighting
#'
#' @param csv_file Path to the dataset CSV file
#' @param pdf_folder Path to PDF folder
#' @param output_folder Output folder (optional)
#' @param max_workers Number of parallel workers
#' @param enable_fuzzy Whether to use fuzzy matching
#'
#' @return Processing results
#' @export
highlight_all_studies_enhanced <- function(csv_file = NULL,
                                          pdf_folder = "Review_articles",
                                          output_folder = NULL,
                                          max_workers = 4,
                                          enable_fuzzy = TRUE) {
  
  cat("🚀 Starting enhanced highlighting for ALL studies!\n")
  cat("This may take some time depending on the number of studies.\n\n")
  
  return(run_enhanced_pdf_highlighting(
    study_ids = "all",
    csv_file = csv_file,
    pdf_folder = pdf_folder,
    output_folder = output_folder,
    max_workers = max_workers,
    enable_fuzzy = enable_fuzzy
  ))
}

#' Quick Test of Enhanced Highlighter
#'
#' @param test_study_id Study ID to test with (default: "1")
#' @param csv_file Path to dataset CSV
#' @param pdf_folder Path to PDF folder
#'
#' @return Test results
#' @export
test_enhanced_highlighter <- function(test_study_id = "1",
                                     csv_file = NULL,
                                     pdf_folder = "Review_articles") {
  
  cat("🧪 Testing Enhanced PDF Highlighter with Study", test_study_id, "\n")
  cat("================================================\n")
  
  today_date <- format(Sys.Date(), "%Y%m%d")
  test_output <- paste0(today_date, "_Test_Enhanced_Highlighting")
  
  result <- run_enhanced_pdf_highlighting(
    study_ids = test_study_id,
    csv_file = csv_file,
    pdf_folder = pdf_folder,
    output_folder = test_output,
    max_workers = 1,  # Single worker for testing
    enable_fuzzy = TRUE
  )
  
  if (result$success && !is.null(result$summary)) {
    cat("\n📊 Test Results Summary:\n")
    cat("Success rate:", paste0(result$summary$success_rate, "%"), "\n")
    cat("Highlights created:", result$summary$total_highlights_created, "\n")
    cat("Processing time:", paste0(round(result$summary$total_processing_time, 2), "s"), "\n")
    
    if (result$summary$success_rate >= 80) {
      cat("\n✅ Test PASSED! Highlighter is working well.\n")
    } else {
      cat("\n⚠️ Test shows room for improvement.\n")
    }
  }
  
  return(result)
}

# Example usage functions
if (FALSE) {
  # Example 1: Test with Study 1
  test_result <- test_enhanced_highlighter("1")
  
  # Example 2: Process specific studies
  result <- highlight_studies_enhanced(
    study_ids = c("1", "2", "3"),
    fast_mode = FALSE,  # Enable fuzzy matching
    parallel_workers = 2
  )
  
  # Example 3: Process all studies (be careful - this takes time!)
  all_results <- highlight_all_studies_enhanced(
    max_workers = 4,
    enable_fuzzy = TRUE
  )
  
  # Example 4: Custom processing with specific paths
  custom_result <- run_enhanced_pdf_highlighting(
    study_ids = c("1", "5", "10"),
    csv_file = "my_data.csv",
    pdf_folder = "my_pdfs/",
    output_folder = "my_results/",
    max_workers = 3,
    enable_fuzzy = FALSE  # Fast mode
  )
}

cat("Enhanced PDF Highlighter R Integration Loaded! 🚀\n")
cat("Available functions:\n")
cat("- run_enhanced_pdf_highlighting(): Main function\n")
cat("- highlight_studies_enhanced(): Process specific studies\n")
cat("- highlight_all_studies_enhanced(): Process all studies\n")
cat("- test_enhanced_highlighter(): Quick test function\n")
cat("\nUse help() for detailed documentation.\n")
