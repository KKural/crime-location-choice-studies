# Extract Model Results Tables and Create Combined Word Document
# This script extracts model/predictor tables from each study and combines them into one document

library(officer)  # For creating Word documents
library(flextable)  # For creating tables in Word
library(dplyr)
library(pdftools)  # For reading PDF content

# Load the dataset
cat("Loading dataset...\n")
df <- readr::read_csv("../20250730_Analysis & Results/20250730_combined_dataset.csv", show_col_types = FALSE)

# Create output folder for the combined document in the main results directory
output_folder <- "../20250730_Analysis & Results"
combined_doc_path <- file.path(output_folder, "Combined_Model_Results_Tables.docx")

# Function to find and extract model information from PDF text
extract_model_info_from_pdf <- function(pdf_path, study_title) {
  
  cat("  Looking for model information in PDF...\n")
  
  tryCatch({
    # Extract text from PDF with error handling
    pdf_text <- tryCatch({
      pdf_text(pdf_path)
    }, error = function(e) {
      cat("    ⚠ PDF reading error, trying alternative method...\n")
      return(NULL)
    })
    
    if (is.null(pdf_text)) {
      return(NULL)
    }
    
    # Clean and combine text from all pages
    full_text <- paste(pdf_text, collapse = " ")
    
    # Remove problematic characters that cause XML errors
    full_text <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]", "", full_text, perl = TRUE)
    full_text <- iconv(full_text, to = "UTF-8", sub = "")
    
    # Search for common model result patterns
    patterns <- list(
      "Table.*[Mm]odel.*[Rr]esult" = "Model Results Table",
      "Table.*[Cc]oefficient" = "Coefficients Table", 
      "Table.*[Rr]egression" = "Regression Table",
      "Table.*[Pp]redictor" = "Predictors Table",
      "Table.*[Ee]stimate" = "Estimates Table",
      "Table.*[Oo]dds.*[Rr]atio" = "Odds Ratios Table",
      "[Mm]odel.*[Ff]it.*[Ss]tatistic" = "Model Fit Statistics",
      "[Ii]ndependent.*[Vv]ariable" = "Independent Variables"
    )
    
    found_content <- character()
    
    for (pattern in names(patterns)) {
      matches <- gregexpr(pattern, full_text, ignore.case = TRUE)
      if (matches[[1]][1] != -1) {
        table_name <- patterns[[pattern]]
        
        # Try to extract surrounding context (this is a simplified approach)
        for (match_pos in matches[[1]]) {
          # Get some context around the match
          start_pos <- max(1, match_pos - 500)
          end_pos <- min(nchar(full_text), match_pos + 1500)
          context <- substr(full_text, start_pos, end_pos)
          
          # Clean up the text and remove problematic characters
          context <- gsub("\\s+", " ", context)
          context <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]", "", context, perl = TRUE)
          context <- iconv(context, to = "UTF-8", sub = "")
          context <- trimws(context)
          
          if (nchar(context) > 100) {  # Only keep substantial content
            # Escape any remaining problematic characters for Word
            context <- gsub("[<>&]", "", context)
            found_content <- c(found_content, paste0("**", table_name, "**\n", context, "\n\n"))
          }
        }
      }
    }
    
    if (length(found_content) > 0) {
      cat("    ✓ Found", length(found_content), "model-related sections\n")
      # Clean the final content
      final_content <- paste(found_content, collapse = "\n")
      final_content <- iconv(final_content, to = "UTF-8", sub = "")
      return(final_content)
    } else {
      cat("    ⚠ No clear model sections found\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("    ✗ Error extracting PDF content:", e$message, "\n")
    return(NULL)
  })
}

# Function to find PDF file for a study
find_pdf_file <- function(title, pdf_folder = "../Review_articles") {
  
  # Clean title for matching
  clean_title <- gsub("[^A-Za-z0-9 ]", "", title)
  clean_title <- gsub(" +", " ", clean_title)
  title_words <- strsplit(tolower(clean_title), " ")[[1]]
  title_words <- title_words[nchar(title_words) > 2]  # Only significant words
  
  if (!dir.exists(pdf_folder)) {
    return(NULL)
  }
  
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  best_match <- NULL
  best_score <- 0
  
  for (pdf_file in pdf_files) {
    pdf_name <- tolower(basename(pdf_file))
    
    # Count matching words
    matches <- sum(sapply(title_words, function(word) grepl(word, pdf_name)))
    score <- matches / length(title_words)
    
    if (score > best_score && score > 0.3) {  # At least 30% word match
      best_score <- score
      best_match <- pdf_file
    }
  }
  
  return(best_match)
}

# Create the combined Word document
cat("Creating combined model results document...\n")
combined_doc <- read_docx()

# Add title page
combined_doc <- combined_doc %>%
  body_add_par("Model Results Tables", style = "heading 1") %>%
  body_add_par("Combined Predictor and Coefficient Tables from All Studies", style = "heading 2") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste("Total studies processed:", nrow(df)), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_break()

# Process all studies
sections_found <- 0
studies_with_content <- 0

for (i in 1:nrow(df)) {
  study_title <- df$Title[i]
  cat("Processing study", i, "of", nrow(df), ":", substr(study_title, 1, 60), "...\n")
  
  tryCatch({
    # Find PDF file for this study
    pdf_file <- find_pdf_file(study_title)
    
    if (is.null(pdf_file)) {
      cat("  ⚠ No PDF file found\n")
      next
    }
    
    cat("  ✓ Found PDF:", basename(pdf_file), "\n")
    
    # Extract model information from PDF
    model_content <- extract_model_info_from_pdf(pdf_file, study_title)
    
    if (!is.null(model_content) && nchar(model_content) > 100) {
      studies_with_content <- studies_with_content + 1
      sections_found <- sections_found + 1
      
      # Add study as a heading
      combined_doc <- combined_doc %>%
        body_add_par(paste("Study", i, ":", study_title), style = "heading 2") %>%
        body_add_par(paste("Authors:", ifelse(is.na(df$Authors[i]), "Not specified", df$Authors[i])), style = "Normal") %>%
        body_add_par(paste("Year:", ifelse(is.na(df$Year[i]), "Not specified", df$Year[i])), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Model Information Extracted:", style = "heading 3") %>%
        body_add_par(model_content, style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_break()
      
      cat("    ✓ Added model content to document\n")
      
    } else {
      cat("  ⚠ No substantial model content found in PDF\n")
    }
    
  }, error = function(e) {
    cat("  ✗ Error processing study:", e$message, "\n")
  })
}

# Add summary at the end
combined_doc <- combined_doc %>%
  body_add_par("Summary", style = "heading 1") %>%
  body_add_par(paste("Studies processed:", nrow(df)), style = "Normal") %>%
  body_add_par(paste("Studies with model content:", studies_with_content), style = "Normal") %>%
  body_add_par(paste("Total model sections extracted:", sections_found), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Generated by: Automated Model Content Extraction System", style = "Normal")

# Save the combined document
print(combined_doc, target = combined_doc_path)

cat("\n🎉 Model content extraction completed!\n")
cat("=================================\n")
cat("Studies processed:", nrow(df), "\n")
cat("Studies with model content:", studies_with_content, "\n")
cat("Total model sections found:", sections_found, "\n")
cat("Combined document:", combined_doc_path, "\n")
cat("=================================\n")
