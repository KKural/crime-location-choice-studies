# Enhanced Rationale Analysis Script
# This script creates improved rationale category analysis by properly handling multi-category values
# and addresses correlation analysis warnings

# Load required libraries
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(purrr)

# Function to safely calculate correlations (avoiding warnings for zero-variance variables)
safe_correlation <- function(data, vars) {
  # Select only numeric variables and check for variance
  numeric_data <- data %>%
    select(all_of(vars)) %>%
    select_if(is.numeric)
  
  # Remove variables with zero variance or all missing
  valid_vars <- numeric_data %>%
    summarise_all(~var(., na.rm = TRUE)) %>%
    select_if(~!is.na(.) && . > 0) %>%
    names()
  
  if(length(valid_vars) < 2) {
    warning("Insufficient variables with variance for correlation analysis")
    return(NULL)
  }
  
  # Calculate correlation matrix
  cor_matrix <- numeric_data %>%
    select(all_of(valid_vars)) %>%
    cor(use = "pairwise.complete.obs")
  
  return(cor_matrix)
}

# Function to clean and split rationale categories
clean_rationale_categories <- function(rationale_column) {
  # Handle multiple categories separated by " / "
  rationale_column %>%
    # Remove "Category: " prefix if present
    str_replace("^Category: ", "") %>%
    # Split by " / " and create separate rows for each category
    str_split(" / ") %>%
    map(~tibble(rationale_category = .x)) %>%
    bind_rows(.id = "original_row")
}

# Function to process rationale data
process_rationale_data <- function(data) {
  # Check if rationale_new column exists
  if(!"rationale_new" %in% names(data)) {
    warning("rationale_new column not found in data")
    return(data)
  }
  
  # Create expanded rationale dataset
  rationale_expanded <- data %>%
    filter(!is.na(rationale_new) & rationale_new != "" & rationale_new != "Not specified") %>%
    mutate(
      row_id = row_number(),
      # Clean rationale_new column
      rationale_clean = str_replace(rationale_new, "^Category: ", ""),
      rationale_clean = str_trim(rationale_clean)
    ) %>%
    # Split multi-category entries
    separate_rows(rationale_clean, sep = " / ") %>%
    mutate(
      rationale_clean = str_trim(rationale_clean),
      # Standardize category names
      rationale_standardized = case_when(
        str_detect(rationale_clean, "(?i)theory.*method|method.*theory") ~ "Theory-Method",
        str_detect(rationale_clean, "(?i)data.*availability|availability.*data") ~ "Data Availability",
        str_detect(rationale_clean, "(?i)practical.*constraint|constraint.*practical") ~ "Practical Constraint", 
        str_detect(rationale_clean, "(?i)prior.*research|research.*prior") ~ "Prior Research",
        str_detect(rationale_clean, "(?i)admin.*convenience|convenience.*admin") ~ "Administrative Convenience",
        str_detect(rationale_clean, "(?i)scale.*optimization|optimization.*scale") ~ "Scale Optimization",
        TRUE ~ str_to_title(rationale_clean)
      )
    ) %>%
    filter(!is.na(rationale_standardized) & rationale_standardized != "")
  
  return(rationale_expanded)
}

# Function to create comprehensive rationale analysis
analyze_rationale_categories <- function(data, output_folder) {
  
  # Process rationale data
  rationale_data <- process_rationale_data(data)
  
  if(nrow(rationale_data) == 0) {
    warning("No valid rationale data found")
    return(NULL)
  }
  
  # Summary by individual categories
  rationale_summary <- rationale_data %>%
    group_by(rationale_standardized) %>%
    summarise(
      N_studies = n_distinct(row_id),
      Percentage = round(n_distinct(row_id) / n_distinct(rationale_data$row_id) * 100, 1),
      Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
      Median_size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
      .groups = "drop"
    ) %>%
    arrange(desc(N_studies))
  
  # Combination analysis (for multi-category rationales)
  combination_summary <- data %>%
    filter(!is.na(rationale_new) & rationale_new != "" & rationale_new != "Not specified") %>%
    mutate(
      rationale_clean = str_replace(rationale_new, "^Category: ", ""),
      rationale_clean = str_trim(rationale_clean)
    ) %>%
    group_by(rationale_clean) %>%
    summarise(
      N_studies = n(),
      Percentage = round(n() / nrow(.) * 100, 1),
      Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
      .groups = "drop"
    ) %>%
    arrange(desc(N_studies))
  
  # Co-occurrence analysis (which categories appear together)
  if(any(str_detect(data$rationale_new, " / ", na.rm = TRUE))) {
    
    # Get studies with multiple categories
    multi_category_studies <- data %>%
      filter(str_detect(rationale_new, " / ", na.rm = TRUE)) %>%
      mutate(
        rationale_clean = str_replace(rationale_new, "^Category: ", ""),
        categories = str_split(rationale_clean, " / ")
      )
    
    # Create co-occurrence matrix
    if(nrow(multi_category_studies) > 0) {
      category_pairs <- multi_category_studies %>%
        rowwise() %>%
        do({
          cats <- str_trim(.$categories[[1]])
          if(length(cats) > 1) {
            expand.grid(cat1 = cats, cat2 = cats, stringsAsFactors = FALSE) %>%
              filter(cat1 != cat2) %>%
              mutate(
                cat1 = case_when(
                  str_detect(cat1, "(?i)theory.*method|method.*theory") ~ "Theory-Method",
                  str_detect(cat1, "(?i)data.*availability|availability.*data") ~ "Data Availability",
                  str_detect(cat1, "(?i)practical.*constraint|constraint.*practical") ~ "Practical Constraint", 
                  str_detect(cat1, "(?i)prior.*research|research.*prior") ~ "Prior Research",
                  str_detect(cat1, "(?i)admin.*convenience|convenience.*admin") ~ "Administrative Convenience",
                  str_detect(cat1, "(?i)scale.*optimization|optimization.*scale") ~ "Scale Optimization",
                  TRUE ~ str_to_title(cat1)
                ),
                cat2 = case_when(
                  str_detect(cat2, "(?i)theory.*method|method.*theory") ~ "Theory-Method",
                  str_detect(cat2, "(?i)data.*availability|availability.*data") ~ "Data Availability",
                  str_detect(cat2, "(?i)practical.*constraint|constraint.*practical") ~ "Practical Constraint", 
                  str_detect(cat2, "(?i)prior.*research|research.*prior") ~ "Prior Research",
                  str_detect(cat2, "(?i)admin.*convenience|convenience.*admin") ~ "Administrative Convenience",
                  str_detect(cat2, "(?i)scale.*optimization|optimization.*scale") ~ "Scale Optimization",
                  TRUE ~ str_to_title(cat2)
                )
              )
          } else {
            data.frame(cat1 = character(0), cat2 = character(0))
          }
        }) %>%
        ungroup()
      
      co_occurrence_summary <- category_pairs %>%
        count(cat1, cat2, sort = TRUE) %>%
        rename(
          Category_1 = cat1,
          Category_2 = cat2,
          Co_occurrences = n
        )
    } else {
      co_occurrence_summary <- data.frame(
        Category_1 = character(0),
        Category_2 = character(0), 
        Co_occurrences = numeric(0)
      )
    }
  }
  
  # Save all summaries
  if(nrow(rationale_summary) > 0) {
    write_csv(rationale_summary, file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_individual_rationale_categories.csv")))
  }
  
  if(nrow(combination_summary) > 0) {
    write_csv(combination_summary, file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_rationale_combinations.csv")))
  }
  
  if(exists("co_occurrence_summary") && nrow(co_occurrence_summary) > 0) {
    write_csv(co_occurrence_summary, file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_rationale_co_occurrences.csv")))
  }
  
  # Create visualizations
  if(nrow(rationale_summary) > 0) {
    
    # Individual categories plot
    p1 <- ggplot(rationale_summary, aes(x = reorder(rationale_standardized, N_studies), y = N_studies)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_text(aes(label = paste0(N_studies, " (", Percentage, "%)")), 
                hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(
        title = "Rationale Categories (Individual Categories)",
        x = "Rationale Category", 
        y = "Number of Studies"
      ) +
      theme_minimal()
    
    ggsave(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_individual_rationale_categories.png")), 
           p1, width = 12, height = 8, dpi = 300)
  }
  
  if(nrow(combination_summary) > 0) {
    
    # Combination plot (top 10)
    top_combinations <- combination_summary %>% slice_head(n = 10)
    
    p2 <- ggplot(top_combinations, aes(x = reorder(rationale_clean, N_studies), y = N_studies)) +
      geom_col(fill = "darkgreen", alpha = 0.7) +
      geom_text(aes(label = paste0(N_studies, " (", Percentage, "%)")), 
                hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(
        title = "Rationale Category Combinations (Top 10)",
        x = "Rationale Combination", 
        y = "Number of Studies"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
    
    ggsave(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_rationale_combinations.png")), 
           p2, width = 14, height = 8, dpi = 300)
  }
  
  # Return all analyses for further use
  result <- list(
    individual_categories = rationale_summary,
    combinations = combination_summary,
    expanded_data = rationale_data
  )
  
  if(exists("co_occurrence_summary")) {
    result$co_occurrences = co_occurrence_summary
  }
  
  return(result)
}

# Main function to run enhanced analysis
run_enhanced_rationale_analysis <- function(input_file, output_folder) {
  
  # Create output folder if it doesn't exist
  if(!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Load data
  cat("Loading data from:", input_file, "\n")
  data <- read_csv(input_file, show_col_types = FALSE)
  
  cat("Data loaded. Rows:", nrow(data), "Columns:", ncol(data), "\n")
  cat("Rationale_new column present:", "rationale_new" %in% names(data), "\n")
  
  # Run rationale analysis
  cat("Running enhanced rationale analysis...\n")
  rationale_results <- analyze_rationale_categories(data, output_folder)
  
  if(!is.null(rationale_results)) {
    cat("Rationale analysis completed successfully!\n")
    cat("Individual categories found:", nrow(rationale_results$individual_categories), "\n")
    cat("Category combinations found:", nrow(rationale_results$combinations), "\n")
    
    # Print summary
    cat("\nTop 5 Individual Categories:\n")
    print(rationale_results$individual_categories %>% slice_head(n = 5))
    
    cat("\nTop 5 Category Combinations:\n") 
    print(rationale_results$combinations %>% slice_head(n = 5))
    
  } else {
    cat("No rationale data found or analysis failed.\n")
  }
  
  return(rationale_results)
}

# If running this script directly
if(interactive() || length(commandArgs(trailingOnly = TRUE)) > 0) {
  
  # Set file paths
  input_file <- "c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching/20250716_Analysis & Results/20250716_standardized_unit_sizes_with_groups_new.csv"
  output_folder <- "c:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/Urban_Foraging_Archive/Articles_drafts/2024 Snatching/20250716_Analysis & Results"
  
  # Run analysis
  results <- run_enhanced_rationale_analysis(input_file, output_folder)
}
