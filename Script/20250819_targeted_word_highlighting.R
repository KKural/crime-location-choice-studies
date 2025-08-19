# ==============================================================================
# TARGETED WORD HIGHLIGHTING SCRIPT
# Purpose: Highlight specific extracted words/phrases in PDFs for verification
# Date: August 19, 2025
# ==============================================================================

# Load required libraries
library(tidyverse)
library(pdftools)
library(stringr)
library(here)

# Set up environment
current_date <- format(Sys.Date(), "%Y%m%d")
cat("Starting targeted word highlighting process...\n")
cat("Date:", current_date, "\n")

# Create output folder
output_folder <- paste0(current_date, "_PDF_Highlighting")
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("Created output folder:", output_folder, "\n")
}

# ==============================================================================
# STEP 1: LOAD AND PREPARE DATA
# ==============================================================================

# Load the combined dataset
data_file <- paste0(current_date, "_Analysis & Results/", current_date, "_combined_dataset.csv")

if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

cat("Loading data from:", data_file, "\n")
df_combined <- read_csv(data_file, show_col_types = FALSE)

cat("Dataset loaded:", nrow(df_combined), "rows,", ncol(df_combined), "columns\n")

# ==============================================================================
# STEP 2: IDENTIFY TARGET COLUMNS FOR HIGHLIGHTING
# ==============================================================================

# Get all column names
all_columns <- names(df_combined)

# Filter out supporting quotes and reasoning columns
main_columns <- all_columns[!grepl("Supporting_quotes_for_|Reasoning_for_", all_columns)]

cat("Main data columns identified:", length(main_columns), "columns\n")
cat("Columns to process:", paste(head(main_columns, 10), collapse = ", "), "...\n")

# ==============================================================================
# STEP 3: EXTRACT UNIQUE VALUES FOR HIGHLIGHTING
# ==============================================================================

# Function to clean and prepare search terms
clean_search_terms <- function(values) {
  # Remove NA, empty strings, and clean up
  values <- values[!is.na(values) & values != "" & values != "Not specified"]
  
  # Split comma-separated values and clean
  all_terms <- c()
  for (val in values) {
    if (is.character(val)) {
      # Split by common separators
      split_vals <- unlist(strsplit(val, "[,;|&]"))
      # Clean each term
      clean_vals <- str_trim(split_vals)
      clean_vals <- clean_vals[clean_vals != "" & nchar(clean_vals) > 2]
      all_terms <- c(all_terms, clean_vals)
    }
  }
  
  # Return unique terms
  unique(all_terms)
}

# Extract search terms by category
cat("Extracting search terms by category...\n")

search_terms_list <- list(
  geographic = list(
    columns = c("Country", "City", "Spatial_Unit_Name"),
    color = "yellow",
    terms = c()
  ),
  quantitative = list(
    columns = c("Unit_Size", "Number_of_Units", "Total_Study_Area_Size", 
                "Crime_Incidents", "Number_of_Variables", "Average_Population_per_Unit"),
    color = "lightblue", 
    terms = c()
  ),
  methods = list(
    columns = c("Model_Type", "Software_Used", "Spatial_Aggregation"),
    color = "lightgreen",
    terms = c()
  ),
  crime_related = list(
    columns = c("Crime_Type", "Crime_Type_Group", "Data_Sources"),
    color = "orange",
    terms = c()
  ),
  statistical = list(
    columns = c("Model_Fit_Statistics", "Coefficients", "Confidence_Intervals", 
                "Effect_Sizes", "Independent_Variables"),
    color = "plum",
    terms = c()
  ),
  other = list(
    columns = setdiff(main_columns, c("Study_ID", "Title", "Citation", "Year", "Verified_by_Kural", "Verified_by_Stephanie")),
    color = "lightcoral",
    terms = c()
  )
)

# Extract terms for each category
for (category in names(search_terms_list)) {
  cat("Processing category:", category, "\n")
  category_columns <- intersect(search_terms_list[[category]]$columns, names(df_combined))
  
  all_category_terms <- c()
  for (col in category_columns) {
    col_terms <- clean_search_terms(df_combined[[col]])
    all_category_terms <- c(all_category_terms, col_terms)
  }
  
  search_terms_list[[category]]$terms <- unique(all_category_terms)
  cat("  Found", length(search_terms_list[[category]]$terms), "unique terms\n")
}

# ==============================================================================
# STEP 4: CREATE COMPREHENSIVE SEARCH TERM LIST
# ==============================================================================

# Combine all terms with their categories and colors
all_search_terms <- data.frame(
  term = character(),
  category = character(), 
  color = character(),
  stringsAsFactors = FALSE
)

for (category in names(search_terms_list)) {
  if (length(search_terms_list[[category]]$terms) > 0) {
    category_df <- data.frame(
      term = search_terms_list[[category]]$terms,
      category = category,
      color = search_terms_list[[category]]$color,
      stringsAsFactors = FALSE
    )
    all_search_terms <- rbind(all_search_terms, category_df)
  }
}

# Remove duplicates (keep first occurrence)
all_search_terms <- all_search_terms[!duplicated(all_search_terms$term), ]

# Sort by length (longest first) to avoid partial matching issues
all_search_terms <- all_search_terms[order(-nchar(all_search_terms$term)), ]

cat("Total unique search terms:", nrow(all_search_terms), "\n")

# Save search terms list
write_csv(all_search_terms, file.path(output_folder, paste0(current_date, "_search_terms_list.csv")))

# ==============================================================================
# STEP 5: FIND PDF FILES
# ==============================================================================

# Look for PDF files in the Review_articles folder
pdf_folder <- "Review_articles"

if (!dir.exists(pdf_folder)) {
  stop("Review_articles folder not found!")
}

pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)

if (length(pdf_files) == 0) {
  stop("No PDF files found in the Review_articles folder!")
}

cat("Found", length(pdf_files), "PDF files to process\n")

# ==============================================================================
# STEP 6: HIGHLIGHTING FUNCTION
# ==============================================================================

# Function to create highlighted HTML from text
create_highlighted_html <- function(text, search_terms_df, study_title = "Unknown") {
  
  highlighted_text <- text
  highlighting_log <- data.frame(
    term = character(),
    category = character(), 
    found = logical(),
    first_occurrence_only = logical(),
    stringsAsFactors = FALSE
  )
  
  # Track which terms have been highlighted (first occurrence only)
  already_highlighted <- character()
  
  for (i in 1:nrow(search_terms_df)) {
    term <- search_terms_df$term[i]
    category <- search_terms_df$category[i]
    color <- search_terms_df$color[i]
    
    # Skip if already highlighted
    if (term %in% already_highlighted) {
      highlighting_log <- rbind(highlighting_log, data.frame(
        term = term,
        category = category,
        found = TRUE,
        first_occurrence_only = FALSE
      ))
      next
    }
    
    # Create regex pattern (case insensitive, word boundaries where appropriate)
    if (grepl("^[0-9]", term) || grepl("[0-9]", term)) {
      # For numbers, don't require word boundaries
      pattern <- regex(paste0("\\b", escapeRegex(term), "\\b"), ignore_case = TRUE)
    } else {
      # For words, use word boundaries
      pattern <- regex(paste0("\\b", escapeRegex(term), "\\b"), ignore_case = TRUE)
    }
    
    # Check if term exists
    if (str_detect(highlighted_text, pattern)) {
      # Highlight first occurrence only
      highlighted_text <- str_replace(highlighted_text, pattern, 
                                    paste0('<mark style="background-color: ', color, '; font-weight: bold;">', 
                                          term, '</mark>'))
      
      already_highlighted <- c(already_highlighted, term)
      
      highlighting_log <- rbind(highlighting_log, data.frame(
        term = term,
        category = category,
        found = TRUE,
        first_occurrence_only = TRUE
      ))
    } else {
      highlighting_log <- rbind(highlighting_log, data.frame(
        term = term,
        category = category, 
        found = FALSE,
        first_occurrence_only = FALSE
      ))
    }
  }
  
  return(list(
    html = highlighted_text,
    log = highlighting_log
  ))
}

# Function to escape regex special characters
escapeRegex <- function(string) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

# ==============================================================================
# STEP 7: PROCESS PDF FILES
# ==============================================================================

cat("Starting PDF processing...\n")

# Create results tracking
results_summary <- data.frame(
  pdf_file = character(),
  total_terms_found = integer(),
  geographic_terms = integer(),
  quantitative_terms = integer(), 
  methods_terms = integer(),
  crime_terms = integer(),
  statistical_terms = integer(),
  other_terms = integer(),
  processing_status = character(),
  stringsAsFactors = FALSE
)

# Process each PDF
for (i in seq_along(pdf_files)) {
  pdf_file <- pdf_files[i]
  cat("\nProcessing PDF", i, "of", length(pdf_files), ":", basename(pdf_file), "\n")
  
  tryCatch({
    # Extract text from PDF
    pdf_text <- pdf_text(pdf_file)
    full_text <- paste(pdf_text, collapse = " ")
    
    # Create study title from filename
    study_title <- str_remove(basename(pdf_file), "\\.pdf$")
    
    # Apply highlighting
    highlight_result <- create_highlighted_html(full_text, all_search_terms, study_title)
    
    # Count terms by category
    log_summary <- highlight_result$log %>%
      filter(found == TRUE & first_occurrence_only == TRUE) %>%
      count(category, name = "count")
    
    # Create summary row
    summary_row <- data.frame(
      pdf_file = basename(pdf_file),
      total_terms_found = sum(log_summary$count),
      geographic_terms = ifelse("geographic" %in% log_summary$category, 
                               log_summary$count[log_summary$category == "geographic"], 0),
      quantitative_terms = ifelse("quantitative" %in% log_summary$category,
                                 log_summary$count[log_summary$category == "quantitative"], 0),
      methods_terms = ifelse("methods" %in% log_summary$category,
                            log_summary$count[log_summary$category == "methods"], 0),
      crime_terms = ifelse("crime_related" %in% log_summary$category,
                          log_summary$count[log_summary$category == "crime_related"], 0),
      statistical_terms = ifelse("statistical" %in% log_summary$category,
                                log_summary$count[log_summary$category == "statistical"], 0),
      other_terms = ifelse("other" %in% log_summary$category,
                          log_summary$count[log_summary$category == "other"], 0),
      processing_status = "Success"
    )
    
    results_summary <- rbind(results_summary, summary_row)
    
    # Save highlighted HTML
    html_content <- paste0(
      '<!DOCTYPE html>
      <html>
      <head>
        <title>Highlighted: ', study_title, '</title>
        <style>
          body { font-family: Arial, sans-serif; line-height: 1.6; margin: 40px; }
          .header { background: #f0f0f0; padding: 20px; margin-bottom: 20px; }
          .legend { background: #fff; border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; }
          .legend-item { display: inline-block; margin-right: 20px; }
          .content { white-space: pre-wrap; }
        </style>
      </head>
      <body>
        <div class="header">
          <h1>Highlighted PDF: ', study_title, '</h1>
          <p>Processed on: ', Sys.Date(), '</p>
          <p>Total highlighted terms: ', sum(log_summary$count), '</p>
        </div>
        
        <div class="legend">
          <h3>Legend:</h3>
          <div class="legend-item"><mark style="background-color: yellow;">Geographic</mark></div>
          <div class="legend-item"><mark style="background-color: lightblue;">Quantitative</mark></div>
          <div class="legend-item"><mark style="background-color: lightgreen;">Methods</mark></div>
          <div class="legend-item"><mark style="background-color: orange;">Crime-related</mark></div>
          <div class="legend-item"><mark style="background-color: plum;">Statistical</mark></div>
          <div class="legend-item"><mark style="background-color: lightcoral;">Other</mark></div>
        </div>
        
        <div class="content">',
      highlight_result$html,
      '</div>
      </body>
      </html>'
    )
    
    # Save HTML file
    html_filename <- file.path(output_folder, paste0(str_remove(basename(pdf_file), "\\.pdf$"), "_highlighted.html"))
    writeLines(html_content, html_filename)
    
    # Save detailed log
    log_filename <- file.path(output_folder, paste0(str_remove(basename(pdf_file), "\\.pdf$"), "_highlighting_log.csv"))
    write_csv(highlight_result$log, log_filename)
    
    cat("  Highlighted", sum(log_summary$count), "terms\n")
    
  }, error = function(e) {
    cat("  Error processing:", e$message, "\n")
    error_row <- data.frame(
      pdf_file = basename(pdf_file),
      total_terms_found = 0,
      geographic_terms = 0,
      quantitative_terms = 0,
      methods_terms = 0,
      crime_terms = 0,
      statistical_terms = 0,
      other_terms = 0,
      processing_status = paste("Error:", e$message)
    )
    results_summary <<- rbind(results_summary, error_row)
  })
}

# ==============================================================================
# STEP 8: SAVE RESULTS AND SUMMARY
# ==============================================================================

# Save results summary
write_csv(results_summary, file.path(output_folder, paste0(current_date, "_highlighting_summary.csv")))

# Create final report
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("TARGETED WORD HIGHLIGHTING COMPLETED\n")
cat(paste(rep("=", 80), collapse=""), "\n")
cat("Total PDFs processed:", nrow(results_summary), "\n")
cat("Successful:", sum(results_summary$processing_status == "Success"), "\n")
cat("Errors:", sum(results_summary$processing_status != "Success"), "\n")
cat("Total terms highlighted:", sum(results_summary$total_terms_found), "\n")
cat("\nOutput folder:", output_folder, "\n")

# Show top highlighted terms by category
cat("\nHighlighting breakdown:\n")
cat("Geographic terms:", sum(results_summary$geographic_terms), "\n")
cat("Quantitative terms:", sum(results_summary$quantitative_terms), "\n") 
cat("Methods terms:", sum(results_summary$methods_terms), "\n")
cat("Crime-related terms:", sum(results_summary$crime_terms), "\n")
cat("Statistical terms:", sum(results_summary$statistical_terms), "\n")
cat("Other terms:", sum(results_summary$other_terms), "\n")

cat("\nFiles generated:\n")
cat("- Search terms list:", paste0(current_date, "_search_terms_list.csv"), "\n")
cat("- Summary report:", paste0(current_date, "_highlighting_summary.csv"), "\n")
cat("- Individual HTML files for each PDF\n")
cat("- Individual highlighting logs for each PDF\n")

cat("\nHighlighting complete! Check the", output_folder, "folder for results.\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
