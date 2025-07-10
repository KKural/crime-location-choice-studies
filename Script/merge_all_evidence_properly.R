# FIXED: Properly merge ALL supporting evidence from all Elicit files
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
if (!file.exists(master_file)) {
  stop("Master merged dataset not found! Please ensure the merged file exists.")
}

master <- read_csv(master_file, show_col_types = FALSE)
cat("Master dataset loaded with", nrow(master), "rows\n")

# Read Elicit files with supporting evidence
elicit_files <- c(
  "Data/Elicit - New table - BASIC STUDY IDENTIFICATION TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - DESCRIPTION _ JUSTIFICATION STUDY CONTEXT _ G.csv",
  "Data/Elicit - New table - THEORETICAL FRAMEWORK _ OBJECTIVES STUDY DESIGN _ METHODOLOGY DATA PREPARATION _ PROCESSING DEMOGRAPHIC _ SOCIAL VARIA.csv",
  "Data/Elicit - New table - TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS MAJOR FINDINGS _ RESULTS ENVIRONMENTAL _ CRIME ATTRACTOR .csv",
  "Data/Elicit - New table - SCALE EFFECTS _ SPATIAL FINDINGS DATA LIMITATIONS _ METHODOLOGICAL ISSUES GENERALIZABILITY _ COMPARATIVE LIMITATIONS I.csv"
)

# Function to normalize titles for matching (same as in merge script)
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%                          # Convert to lowercase
    str_remove_all('[""\'\"*]') %>%            # Remove quotes and asterisks
    str_remove_all('\\([^)]*\\)') %>%          # Remove parenthetical content
    str_replace_all('[[:punct:]]', ' ') %>%    # Replace punctuation with spaces
    str_replace_all('\\s+', ' ') %>%           # Replace multiple spaces with single space
    str_trim()                                  # Trim whitespace
}

# Create normalized titles for master
master_norm <- master %>%
  mutate(title_norm = normalize_title(Title_of_the_study))

# FIXED APPROACH: Merge each Elicit file separately with master, then combine
combined_evidence <- master

for (file_path in elicit_files) {
  if (file.exists(file_path)) {
    cat("\nProcessing:", basename(file_path), "\n")
    
    tryCatch({
      file_data <- read_csv(file_path, show_col_types = FALSE)
      cat("  Loaded", nrow(file_data), "rows\n")
      
      # Create normalized titles for this file
      file_norm <- file_data %>%
        mutate(title_norm = normalize_title(Title))
      
      # Create matches for this file using the same fuzzy logic
      matches <- data.frame(
        master_title = character(),
        elicit_title = character(),
        match_type = character(),
        stringsAsFactors = FALSE
      )
      
      # Method 1: Exact normalized match
      for (i in 1:nrow(master_norm)) {
        master_title <- master_norm$Title_of_the_study[i]
        master_norm_title <- master_norm$title_norm[i]
        
        exact_match <- file_norm$Title[file_norm$title_norm == master_norm_title]
        if (length(exact_match) > 0) {
          matches <- bind_rows(matches, data.frame(
            master_title = master_title,
            elicit_title = exact_match[1],
            match_type = "exact_normalized"
          ))
        }
      }
      
      # Method 2: Fuzzy match (master contained in elicit)
      for (i in 1:nrow(master_norm)) {
        master_title <- master_norm$Title_of_the_study[i]
        master_norm_title <- master_norm$title_norm[i]
        
        # Skip if already matched
        if (master_title %in% matches$master_title) next
        
        # Look for elicit titles that contain the master title
        fuzzy_match <- file_norm$Title[str_detect(file_norm$title_norm, str_escape(master_norm_title))]
        if (length(fuzzy_match) > 0) {
          matches <- bind_rows(matches, data.frame(
            master_title = master_title,
            elicit_title = fuzzy_match[1],
            match_type = "fuzzy_contained"
          ))
        }
      }
      
      # Method 3: Partial word matching (60% of words match)
      for (i in 1:nrow(master_norm)) {
        master_title <- master_norm$Title_of_the_study[i]
        master_norm_title <- master_norm$title_norm[i]
        
        # Skip if already matched
        if (master_title %in% matches$master_title) next
        
        master_words <- str_split(master_norm_title, "\\s+")[[1]]
        master_words <- master_words[nchar(master_words) > 3]  # Only significant words
        
        if (length(master_words) < 3) next  # Skip very short titles
        
        for (j in 1:nrow(file_norm)) {
          elicit_words <- str_split(file_norm$title_norm[j], "\\s+")[[1]]
          elicit_words <- elicit_words[nchar(elicit_words) > 3]
          
          if (length(elicit_words) < 3) next
          
          # Calculate word overlap
          common_words <- intersect(master_words, elicit_words)
          overlap_ratio <- length(common_words) / length(master_words)
          
          if (overlap_ratio >= 0.6) {  # 60% word overlap
            matches <- bind_rows(matches, data.frame(
              master_title = master_title,
              elicit_title = file_norm$Title[j],
              match_type = sprintf("word_overlap_%.0f%%", overlap_ratio * 100)
            ))
            break
          }
        }
      }
      
      # Method 4: Reverse fuzzy match (elicit contained in master)
      for (j in 1:nrow(file_norm)) {
        elicit_title <- file_norm$Title[j]
        elicit_norm_title <- file_norm$title_norm[j]
        
        # Skip if already matched
        if (elicit_title %in% matches$elicit_title) next
        
        # Look for master titles that contain the elicit title
        reverse_match <- master_norm$Title_of_the_study[str_detect(master_norm$title_norm, str_escape(elicit_norm_title))]
        if (length(reverse_match) > 0) {
          matches <- bind_rows(matches, data.frame(
            master_title = reverse_match[1],
            elicit_title = elicit_title,
            match_type = "reverse_fuzzy"
          ))
        }
      }
      
      cat("  Found", nrow(matches), "matches for this file\n")
      
      # Find supporting quotes and reasoning columns in this file
      quote_cols <- names(file_data)[str_detect(names(file_data), "Supporting quotes")]
      reasoning_cols <- names(file_data)[str_detect(names(file_data), "Reasoning")]
      
      cat("  Quote columns:", length(quote_cols), "Reasoning columns:", length(reasoning_cols), "\n")
      
      if (length(quote_cols) > 0 || length(reasoning_cols) > 0) {
        # Create evidence data with unique suffix for this file
        file_suffix <- str_extract(basename(file_path), "(?<=table - )[^.]+")
        file_suffix <- str_replace_all(file_suffix, "[^A-Za-z0-9]", "_")
        
        evidence_cols <- c("Title", "Authors", "Year", quote_cols, reasoning_cols)
        evidence_data <- file_data %>%
          select(all_of(evidence_cols)) %>%
          distinct(Title, .keep_all = TRUE)
        
        # Merge this file's evidence with the combined dataset
        temp_merge <- combined_evidence %>%
          left_join(matches, by = c("Title_of_the_study" = "master_title")) %>%
          left_join(evidence_data, by = c("elicit_title" = "Title"), suffix = c("", paste0("_", file_suffix)))
        
        # Update combined evidence, keeping existing columns
        for (col in quote_cols) {
          if (col %in% names(temp_merge)) {
            combined_evidence[[col]] <- temp_merge[[col]]
          }
        }
        for (col in reasoning_cols) {
          if (col %in% names(temp_merge)) {
            combined_evidence[[col]] <- temp_merge[[col]]
          }
        }
        
        # Add the match columns from this file if not already present
        if (!"elicit_title" %in% names(combined_evidence) && "elicit_title" %in% names(temp_merge)) {
          combined_evidence$elicit_title <- temp_merge$elicit_title
        }
        if (!"Evidence_Match_Type" %in% names(combined_evidence) && "match_type" %in% names(temp_merge)) {
          combined_evidence$Evidence_Match_Type <- temp_merge$match_type
        }
      }
      
    }, error = function(e) {
      cat("Error processing", file_path, ":", e$message, "\n")
    })
  }
}

# Add final supporting evidence flag
combined_evidence <- combined_evidence %>%
  mutate(
    Has_Supporting_Evidence = !is.na(elicit_title)
  )

# Save the enhanced dataset
output_file <- file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_standardized_unit_sizes_with_complete_evidence.csv"))
write_csv(combined_evidence, output_file)

# Check results
quote_cols_final <- names(combined_evidence)[str_detect(names(combined_evidence), "Supporting quotes")]
cat("\n=== FINAL RESULTS ===\n")
cat("Total supporting quote columns in final dataset:", length(quote_cols_final), "\n")

for (col in quote_cols_final) {
  non_empty <- sum(!is.na(combined_evidence[[col]]) & combined_evidence[[col]] != '' & combined_evidence[[col]] != '-', na.rm = TRUE)
  cat(col, ":", non_empty, "studies with quotes\n")
}

cat("\nOutput saved to:", output_file, "\n")
cat("Dataset now contains", ncol(combined_evidence), "columns\n")
cat("Processing complete!\n")
