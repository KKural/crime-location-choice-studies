# Load Analysis Results Helper Function
# This function loads all tables from the Excel file and makes them available

load_analysis_results <- function(excel_path = NULL) {
  tryCatch({
    # If no path provided, construct using current date
    if (is.null(excel_path)) {
      current_date <- format(Sys.Date(), "%Y%m%d")
      excel_path <- paste0(current_date, "_Analysis & Results/", 
                          current_date, "_Comprehensive_Manuscript_Tables.xlsx")
    }
    
    # Check if the file exists
    if (!file.exists(excel_path)) {
      stop("Analysis results file not found: ", excel_path, 
           "\nRun the analysis script first or check the file path.")
    }
    
    # Get all sheet names
    cat("Reading Excel sheets from", excel_path, "\n")
    sheets <- readxl::excel_sheets(excel_path)
    
    if (length(sheets) == 0) {
      stop("No sheets found in Excel file: ", excel_path)
    }
    
    # Load each sheet into a list
    results <- list()
    sheet_count <- 0
    
    for (sheet in sheets) {
      # Clean sheet name to create a valid R variable
      var_name <- gsub("\\W", "_", sheet)
      
      # Read the sheet with error handling
      tryCatch({
        results[[var_name]] <- readxl::read_excel(excel_path, sheet = sheet)
        sheet_count <- sheet_count + 1
      }, error = function(e) {
        warning("Error reading sheet '", sheet, "': ", e$message)
      })
    }
    
    # Check for required tables
    required_tables <- c(
      "Table1_Summary_Statistics", 
      "Table4_RQ3_Country_Analysis", 
      "Table6_RQ5_Variable_Complexity",
      "Table8_Statistical_Models",
      "Raw_Summary_Statistics"
    )
    
    # Convert to the format used in the results list (with underscores instead of special chars)
    required_tables_clean <- gsub("\\W", "_", required_tables)
    
    # Check if all required tables are present
    missing_tables <- required_tables_clean[!required_tables_clean %in% names(results)]
    
    if (length(missing_tables) > 0) {
      warning("Missing required tables: ", paste(missing_tables, collapse = ", "),
              "\nThis may cause errors when rendering the manuscript.")
    }
    
    # Print summary of loaded data
    cat("Successfully loaded", sheet_count, "of", length(sheets), "tables from", excel_path, "\n")
    cat("Available tables:", paste(names(results), collapse = ", "), "\n")
    
    if (length(names(results)) < length(sheets)) {
      warning("Some sheets could not be loaded. Check warning messages above.")
    }
    
    # Return the list of tables
    return(results)
    
  }, error = function(e) {
    cat("ERROR: Failed to load analysis results:", e$message, "\n")
    # Return an empty list as fallback
    return(list())
  })
}

# Example usage:
# analysis_data <- load_analysis_results()
# summary_stats <- analysis_data$Table1_Summary_Statistics
