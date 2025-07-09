# Variable Duplication Detection Script for Elicit Extractions
# This script identifies duplicate variables across different categories

library(tidyverse)
library(janitor)

# Function to extract variables from a category column
extract_variables <- function(text) {
  if (is.na(text) || text == "" || text == "No variables mentioned") {
    return(character(0))
  }
  
  # Split by numbered list items (1., 2., etc.) or bullet points
  lines <- str_split(text, "\\n")[[1]]
  
  # Extract variable names (everything before the first |)
  variables <- lines %>%
    str_trim() %>%
    str_extract("^[0-9]+\\.\\s*([^|]+)") %>%
    str_remove("^[0-9]+\\.\\s*") %>%
    str_trim() %>%
    na.omit() %>%
    as.character()
  
  # Also try alternative format without numbers
  if (length(variables) == 0) {
    variables <- lines %>%
      str_trim() %>%
      str_extract("^([^|]+)\\|") %>%
      str_remove("\\|$") %>%
      str_trim() %>%
      na.omit() %>%
      as.character()
  }
  
  return(variables[variables != ""])
}

# Function to normalize variable names for comparison
normalize_variable_name <- function(var_names) {
  var_names %>%
    tolower() %>%
    str_remove_all("[^a-z0-9\\s]") %>%
    str_replace_all("\\s+", "_") %>%
    str_trim()
}

# Function to check for duplicates across categories
check_duplicates <- function(csv_file_path) {
  
  cat("Analyzing:", basename(csv_file_path), "\n")
  cat("=" %>% rep(50) %>% paste(collapse=""), "\n")
  
  # Read the CSV file
  df <- read_csv(csv_file_path, locale = locale(encoding = "UTF-8"))
  
  # Get variable category columns (exclude supporting quotes, tables, reasoning)
  var_columns <- names(df) %>%
    str_subset("VARIABLES|FIT|EFFECTS|FINDINGS") %>%
    str_subset("Supporting|Reasoning", negate = TRUE)
  
  cat("Variable categories found:\n")
  for (col in var_columns) {
    cat("  -", col, "\n")
  }
  cat("\n")
  
  # Extract all variables by category
  all_variables <- list()
  
  for (col in var_columns) {
    category_vars <- c()
    for (i in 1:nrow(df)) {
      vars <- extract_variables(df[[col]][i])
      category_vars <- c(category_vars, vars)
    }
    all_variables[[col]] <- unique(category_vars[category_vars != ""])
  }
  
  # Check for duplicates
  duplicates_found <- FALSE
  
  # Create a flattened list of all variables with their categories
  var_category_map <- tibble(
    variable = character(),
    category = character(),
    normalized = character()
  )
  
  for (cat_name in names(all_variables)) {
    if (length(all_variables[[cat_name]]) > 0) {
      cat_vars <- tibble(
        variable = all_variables[[cat_name]],
        category = cat_name,
        normalized = normalize_variable_name(all_variables[[cat_name]])
      )
      var_category_map <- bind_rows(var_category_map, cat_vars)
    }
  }
  
  # Find duplicates by normalized name
  duplicate_vars <- var_category_map %>%
    group_by(normalized) %>%
    filter(n() > 1) %>%
    arrange(normalized, category)
  
  if (nrow(duplicate_vars) > 0) {
    duplicates_found <- TRUE
    cat("DUPLICATE VARIABLES FOUND:\n")
    cat("-" %>% rep(30) %>% paste(collapse=""), "\n")
    
    for (norm_var in unique(duplicate_vars$normalized)) {
      cat("\nVariable:", norm_var, "\n")
      dup_entries <- duplicate_vars %>% filter(normalized == norm_var)
      for (i in 1:nrow(dup_entries)) {
        cat("  Category:", dup_entries$category[i], "\n")
        cat("  Original:", dup_entries$variable[i], "\n")
      }
    }
  } else {
    cat("✓ No duplicate variables found!\n")
  }
  
  # Summary statistics
  cat("\nSUMMARY:\n")
  cat("-" %>% rep(20) %>% paste(collapse=""), "\n")
  cat("Total unique variables:", nrow(var_category_map), "\n")
  cat("Categories analyzed:", length(var_columns), "\n")
  cat("Duplicates found:", nrow(duplicate_vars), "\n")
  
  return(list(
    duplicates = duplicate_vars,
    all_variables = var_category_map,
    duplicates_found = duplicates_found
  ))
}

# Function to generate variable frequency report
generate_variable_frequency_report <- function(csv_files) {
  
  all_vars_combined <- tibble(
    variable = character(),
    category = character(),
    normalized = character(),
    file = character()
  )
  
  for (file in csv_files) {
    result <- check_duplicates(file)
    if (nrow(result$all_variables) > 0) {
      file_vars <- result$all_variables %>%
        mutate(file = basename(file))
      all_vars_combined <- bind_rows(all_vars_combined, file_vars)
    }
  }
  
  # Variable frequency across files
  var_frequency <- all_vars_combined %>%
    group_by(normalized) %>%
    summarise(
      frequency = n(),
      categories = paste(unique(category), collapse = "; "),
      files = paste(unique(file), collapse = "; "),
      .groups = "drop"
    ) %>%
    arrange(desc(frequency))
  
  return(var_frequency)
}

# Main execution
if (interactive()) {
  
  # Define CSV files to check
  csv_files <- c(
    "Elicit - New table - ENVIRONMENTAL _ CRIME ATTRACTOR VARIABLES STUDY DESIGN _ METHODOLOGY SAMPLING _ CHOICE SETS DEMOGRAPHIC _ SOCIAL VARIA.csv",
    "Elicit - New table - DISTANCE _ ACCESSIBILITY VARIABLES TEMPORAL _ CONTROL VARIABLES MODEL FIT _ PERFORMANCE METRICS SCALE EFFECTS _ SPATI.csv",
    "Elicit - Extract from 50 papers - BASIC STUDY IDENTIFICATION STUDY CONTEXT _ GEOGRAPHY TEMPORAL SCOPE _ DATA SOURCES SPATIAL UNITS - BASIC .csv"
  )
  
  # Check each file
  results <- list()
  for (file in csv_files) {
    if (file.exists(file)) {
      results[[file]] <- check_duplicates(file)
      cat("\n\n")
    } else {
      cat("File not found:", file, "\n\n")
    }
  }
  
  # Generate combined frequency report
  existing_files <- csv_files[file.exists(csv_files)]
  if (length(existing_files) > 0) {
    cat("COMBINED VARIABLE FREQUENCY REPORT\n")
    cat("=" %>% rep(50) %>% paste(collapse=""), "\n")
    
    freq_report <- generate_variable_frequency_report(existing_files)
    
    # Show most frequent variables (potential duplicates)
    frequent_vars <- freq_report %>%
      filter(frequency > 1) %>%
      head(20)
    
    if (nrow(frequent_vars) > 0) {
      cat("Variables appearing in multiple categories/files:\n")
      print(frequent_vars)
    } else {
      cat("No variables appear in multiple categories/files.\n")
    }
    
    # Save the report
    write_csv(freq_report, "variable_frequency_analysis.csv")
    cat("\nFull report saved to: variable_frequency_analysis.csv\n")
  }
}

cat("Script completed. Use check_duplicates('filename.csv') to analyze specific files.\n")
