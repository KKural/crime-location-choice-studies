# SIMPLIFIED: Merge ALL supporting evidence properly 
library(dplyr)
library(readr)
library(stringr)

# Function to create output folder
make_folder <- function() {
  today <- format(Sys.Date(), "%Y%m%d")
  folder_name <- paste0(today, "_Analysis & Results")
  if (!dir.exists(folder_name)) {
    dir.create(folder_name, recursive = TRUE)
  }
  return(folder_name)
}

output_folder <- make_folder()

# Read the existing merged dataset
master_file <- file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_standardized_unit_sizes_with_groups_merged.csv"))
master <- read_csv(master_file, show_col_types = FALSE)
cat("Master dataset loaded with", nrow(master), "rows\n")

# Function to normalize titles for matching
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%                          
    str_remove_all('[""\'\"*]') %>%            
    str_remove_all('\\([^)]*\\)') %>%          
    str_replace_all('[[:punct:]]', ' ') %>%    
    str_replace_all('\\s+', ' ') %>%           
    str_trim()                                  
}

# Create title matching function
create_matches <- function(master_titles, elicit_titles) {
  master_norm <- normalize_title(master_titles)
  elicit_norm <- normalize_title(elicit_titles)
  
  matches <- data.frame(
    master_idx = integer(),
    elicit_idx = integer(),
    match_type = character(),
    stringsAsFactors = FALSE
  )
  
  # Method 1: Exact match
  for (i in 1:length(master_norm)) {
    exact_matches <- which(elicit_norm == master_norm[i])
    if (length(exact_matches) > 0) {
      matches <- bind_rows(matches, data.frame(
        master_idx = i,
        elicit_idx = exact_matches[1],
        match_type = "exact"
      ))
    }
  }
  
  # Method 2: Fuzzy match for unmatched
  unmatched_master <- setdiff(1:length(master_norm), matches$master_idx)
  for (i in unmatched_master) {
    for (j in 1:length(elicit_norm)) {
      if (j %in% matches$elicit_idx) next  # Skip already matched
      
      # Check if master is contained in elicit
      if (str_detect(elicit_norm[j], str_escape(master_norm[i]))) {
        matches <- bind_rows(matches, data.frame(
          master_idx = i,
          elicit_idx = j,
          match_type = "fuzzy"
        ))
        break
      }
    }
  }
  
  return(matches)
}

# Read and process each Elicit file separately
elicit_files <- c(
  "Data/Elicit - New table - BASIC STUDY IDENTIFICATION TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - DESCRIPTION _ JUSTIFICATION STUDY CONTEXT _ G.csv",
  "Data/Elicit - New table - THEORETICAL FRAMEWORK _ OBJECTIVES STUDY DESIGN _ METHODOLOGY DATA PREPARATION _ PROCESSING DEMOGRAPHIC _ SOCIAL VARIA.csv",
  "Data/Elicit - New table - TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS MAJOR FINDINGS _ RESULTS ENVIRONMENTAL _ CRIME ATTRACTOR .csv",
  "Data/Elicit - New table - SCALE EFFECTS _ SPATIAL FINDINGS DATA LIMITATIONS _ METHODOLOGICAL ISSUES GENERALIZABILITY _ COMPARATIVE LIMITATIONS I.csv"
)

# Start with master dataset
result <- master

for (file_path in elicit_files) {
  if (file.exists(file_path)) {
    cat("\nProcessing:", basename(file_path), "\n")
    
    tryCatch({
      # Read file
      elicit_data <- read_csv(file_path, show_col_types = FALSE)
      cat("  Loaded", nrow(elicit_data), "rows\n")
      
      # Create matches
      matches <- create_matches(master$Title_of_the_study, elicit_data$Title)
      cat("  Found", nrow(matches), "matches\n")
      
      # Find supporting evidence columns
      quote_cols <- names(elicit_data)[str_detect(names(elicit_data), "Supporting quotes")]
      reasoning_cols <- names(elicit_data)[str_detect(names(elicit_data), "Reasoning")]
      cat("  Quote columns:", length(quote_cols), "Reasoning columns:", length(reasoning_cols), "\n")
      
      # Add evidence columns to result
      for (col in c(quote_cols, reasoning_cols)) {
        # Initialize column if not exists
        if (!col %in% names(result)) {
          result[[col]] <- NA_character_
        }
        
        # Fill in matched data
        for (k in 1:nrow(matches)) {
          master_idx <- matches$master_idx[k]
          elicit_idx <- matches$elicit_idx[k]
          
          if (!is.na(elicit_data[[col]][elicit_idx]) && elicit_data[[col]][elicit_idx] != "") {
            result[[col]][master_idx] <- elicit_data[[col]][elicit_idx]
          }
        }
      }
      
    }, error = function(e) {
      cat("Error processing", basename(file_path), ":", e$message, "\n")
    })
  }
}

# Add summary flags
quote_cols_final <- names(result)[str_detect(names(result), "Supporting quotes")]
result$Has_Supporting_Evidence <- rowSums(!is.na(result[quote_cols_final])) > 0

# Save result
output_file <- file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_standardized_unit_sizes_with_complete_evidence.csv"))
write_csv(result, output_file)

# Summary
cat("\n=== FINAL SUMMARY ===\n")
cat("Total studies:", nrow(result), "\n")
cat("Total quote columns:", length(quote_cols_final), "\n")

for (col in quote_cols_final) {
  non_empty <- sum(!is.na(result[[col]]) & result[[col]] != '' & result[[col]] != '-', na.rm = TRUE)
  cat(col, ":", non_empty, "studies\n")
}

studies_with_evidence <- sum(result$Has_Supporting_Evidence)
cat("\nStudies with supporting evidence:", studies_with_evidence, "out of", nrow(result), "\n")
cat("Output saved to:", output_file, "\n")
