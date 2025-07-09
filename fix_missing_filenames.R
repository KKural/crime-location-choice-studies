# Script to Fix Missing Filenames in Elicit CSV Extractions

library(tidyverse)
library(janitor)

# Function to generate filename from paper details
generate_filename <- function(authors, year, title) {
  # Extract first author's last name
  first_author <- str_extract(authors, "^[^,;]+") %>%
    str_extract("[A-Za-z]+$") %>%  # Get last word (surname)
    str_to_title()
  
  # Extract key words from title (remove common words)
  common_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "from", "up", "about", "into", "through", "during", "before", "after", "above", "below", "between", "among", "under", "over", "inside", "outside", "within")
  
  title_words <- title %>%
    str_to_lower() %>%
    str_remove_all("[^a-z0-9\\s]") %>%  # Remove punctuation
    str_split("\\s+") %>%
    unlist() %>%
    .[!. %in% common_words] %>%  # Remove common words
    .[nchar(.) > 2] %>%  # Keep words longer than 2 chars
    head(3) %>%  # Take first 3 meaningful words
    str_to_title()
  
  # Construct filename
  filename <- paste0(
    first_author, "_", 
    year, "_",
    paste(title_words, collapse = "_"),
    ".pdf"
  )
  
  return(filename)
}

# Function to fix filenames in a CSV file
fix_filenames_in_csv <- function(csv_path, output_path = NULL) {
  
  if (!file.exists(csv_path)) {
    cat("Error: File not found:", csv_path, "\n")
    return(NULL)
  }
  
  cat("Processing:", basename(csv_path), "\n")
  
  # Read the CSV
  df <- read_csv(csv_path, locale = locale(encoding = "UTF-8"))
  
  # Check if required columns exist
  required_cols <- c("Title", "Authors", "Year")
  missing_cols <- required_cols[!required_cols %in% names(df)]
  
  if (length(missing_cols) > 0) {
    cat("Error: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  # Check current filename situation
  filename_col <- "Filename"
  if (!filename_col %in% names(df)) {
    cat("Adding Filename column...\n")
    df$Filename <- NA
  }
  
  # Count missing/problematic filenames
  problematic_filenames <- df$Filename %>%
    is.na() | 
    . == "" | 
    . == "Not mentioned" | 
    . == "-" |
    str_detect(., "^untitled", negate = FALSE)
  
  cat("Found", sum(problematic_filenames, na.rm = TRUE), "missing or problematic filenames\n")
  
  # Generate filenames for problematic cases
  if (sum(problematic_filenames, na.rm = TRUE) > 0) {
    cat("Generating new filenames...\n")
    
    for (i in which(problematic_filenames)) {
      if (!is.na(df$Authors[i]) && !is.na(df$Year[i]) && !is.na(df$Title[i])) {
        new_filename <- generate_filename(df$Authors[i], df$Year[i], df$Title[i])
        df$Filename[i] <- new_filename
        cat("  Row", i, ":", new_filename, "\n")
      }
    }
  }
  
  # Check for duplicate filenames and make them unique
  if (any(duplicated(df$Filename))) {
    cat("Fixing duplicate filenames...\n")
    df <- df %>%
      group_by(Filename) %>%
      mutate(
        Filename = if_else(
          n() > 1,
          paste0(str_remove(Filename, "\\.pdf$"), "_", row_number(), ".pdf"),
          Filename
        )
      ) %>%
      ungroup()
  }
  
  # Set output path
  if (is.null(output_path)) {
    output_path <- str_replace(csv_path, "\\.csv$", "_with_filenames.csv")
  }
  
  # Save the fixed CSV
  write_csv(df, output_path)
  cat("Fixed CSV saved to:", output_path, "\n")
  
  # Summary report
  cat("\nSUMMARY:\n")
  cat("- Total rows:", nrow(df), "\n")
  cat("- Rows with filenames:", sum(!is.na(df$Filename) & df$Filename != ""), "\n")
  cat("- Generated filenames:", sum(problematic_filenames, na.rm = TRUE), "\n")
  
  return(df)
}

# Function to process all Elicit CSV files in current directory
fix_all_elicit_csvs <- function(pattern = "Elicit.*\\.csv$") {
  
  csv_files <- list.files(".", pattern = pattern, full.names = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No Elicit CSV files found matching pattern:", pattern, "\n")
    return(NULL)
  }
  
  cat("Found", length(csv_files), "Elicit CSV files:\n")
  for (file in csv_files) {
    cat("  -", basename(file), "\n")
  }
  cat("\n")
  
  # Process each file
  results <- list()
  for (file in csv_files) {
    cat("=" %>% rep(50) %>% paste(collapse=""), "\n")
    results[[file]] <- fix_filenames_in_csv(file)
    cat("\n")
  }
  
  return(results)
}

# Manual filename mapping (if you want to specify exact filenames)
create_manual_filename_mapping <- function() {
  
  # Create a template mapping file
  mapping_template <- tribble(
    ~Title, ~Filename,
    "TARGET CHOICE DURING EXTREME EVENTS: A DISCRETE SPATIAL CHOICE MODEL OF THE 2011 LONDON RIOTS", "Baudains_2013_London_Riots.pdf",
    "Formal Evaluation of the Impact of Barriers and Connectors on Residential Burglars' Macro-Level Offending Location Choices", "Clare_2009_Barriers_Burglars.pdf",
    "a_discrete_spatial_choice_model_of_burglary_target.pdf", "Bernasco_2013_Burglary_Target.pdf"
  )
  
  write_csv(mapping_template, "manual_filename_mapping.csv")
  cat("Manual mapping template created: manual_filename_mapping.csv\n")
  cat("Edit this file to specify exact filenames, then use apply_manual_mapping()\n")
  
  return(mapping_template)
}

# Function to apply manual filename mapping
apply_manual_mapping <- function(csv_path, mapping_path = "manual_filename_mapping.csv") {
  
  if (!file.exists(mapping_path)) {
    cat("Error: Mapping file not found:", mapping_path, "\n")
    cat("Create one using create_manual_filename_mapping()\n")
    return(NULL)
  }
  
  # Read files
  df <- read_csv(csv_path)
  mapping <- read_csv(mapping_path)
  
  # Apply mapping
  df_updated <- df %>%
    left_join(mapping, by = "Title", suffix = c("_old", "_new")) %>%
    mutate(Filename = coalesce(Filename_new, Filename_old)) %>%
    select(-Filename_old, -Filename_new)
  
  # Save updated file
  output_path <- str_replace(csv_path, "\\.csv$", "_mapped_filenames.csv")
  write_csv(df_updated, output_path)
  
  cat("Applied manual mapping. Saved to:", output_path, "\n")
  return(df_updated)
}

# Usage examples:
cat("USAGE EXAMPLES:\n")
cat("1. Fix all Elicit CSV files: fix_all_elicit_csvs()\n")
cat("2. Fix specific file: fix_filenames_in_csv('your_file.csv')\n")
cat("3. Create manual mapping: create_manual_filename_mapping()\n")
cat("4. Apply manual mapping: apply_manual_mapping('your_file.csv')\n")

# If running interactively, you can uncomment and run:
# fix_all_elicit_csvs()
