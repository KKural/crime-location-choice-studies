# Enhanced Variable Extraction and Integration Script
# Based on clean_variable_extraction_integrated.R but with enhanced information capture
# Handles detailed extraction format while maintaining same methodology

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Enhanced extraction function to handle detailed format
extract_enhanced_study_info <- function(text) {
  
  # Initialize result list with enhanced fields
  result <- list(
    # Basic identification
    title = NA_character_,
    year = NA_integer_,
    authors = NA_character_,
    filename = NA_character_,
    journal = NA_character_,
    doi = NA_character_,
    
    # Geographic and temporal context  
    country = NA_character_,
    city_region = NA_character_,
    study_area_size = NA_real_,
    study_area_description = NA_character_,
    crime_type = NA_character_,
    crime_types_all = NA_character_,
    study_period = NA_character_,
    data_sources = NA_character_,
    
    # Spatial unit details
    suoa_type = NA_character_,
    suoa_size = NA_character_,
    suoa_size_km2 = NA_real_,
    suoa_description = NA_character_,
    number_of_units = NA_integer_,
    population_per_unit = NA_character_,
    suoa_justification = NA_character_,
    justification_category = NA_character_,
    justification_quote = NA_character_,
    
    # Methodology
    study_design = NA_character_,
    statistical_method = NA_character_,
    model_type = NA_character_,
    software_used = NA_character_,
    sampling_approach = NA_character_,
    sample_size = NA_integer_,
    choice_set_definition = NA_character_,
    estimation_method = NA_character_,
    
    # Variable counts (6 categories, removed social/behavioral)
    total_variables = NA_integer_,
    demographic_vars = NA_integer_,
    economic_vars = NA_integer_,
    crime_attractors_vars = NA_integer_, # Combined land use/infrastructure/environmental/opportunity
    distance_vars = NA_integer_,
    temporal_vars = NA_integer_,
    other_vars = NA_integer_,
    
    # Results and quality
    main_results = NA_character_,
    significant_predictors = NA_character_,
    model_performance = NA_character_,
    scale_effects = NA_character_,
    variable_quality = NA_character_,
    missing_information = NA_character_,
    extraction_confidence = NA_character_,
    
    # Data limitations (new section)
    data_limitations = NA_character_,
    limitation_quotes = NA_character_,
    limitation_summary = NA_character_,
    spatial_limitations = NA_character_,
    temporal_limitations = NA_character_,
    data_quality_limitations = NA_character_,
    methodological_limitations = NA_character_,
    generalizability_limitations = NA_character_
  )
  
  # Helper function to extract field values
  extract_field <- function(pattern, text, multiline = FALSE) {
    if (multiline) {
      match <- str_extract(text, paste0(pattern, "\\s*:\\s*([\\s\\S]*?)(?=\\n\\w+\\s*:|$)"))
    } else {
      match <- str_extract(text, paste0(pattern, "\\s*:\\s*([^\n]+)"))
    }
    if (!is.na(match)) {
      value <- str_replace(match, paste0("^", pattern, "\\s*:\\s*"), "")
      value <- str_trim(value)
      if (value == "" || value == "NA" || str_detect(value, "^\\[.*\\]$")) {
        return(NA_character_)
      }
      return(value)
    }
    return(NA_character_)
  }
  
  # Extract basic identification
  result$title <- extract_field("Title", text)
  year_text <- extract_field("Year", text)
  if (!is.na(year_text)) {
    result$year <- as.integer(str_extract(year_text, "\\d{4}"))
  }
  result$authors <- extract_field("Authors", text)
  result$filename <- extract_field("Filename", text)
  # Handle case where filename is not provided
  if (is.na(result$filename) || result$filename == "Not provided" || result$filename == "") {
    result$filename <- "Not provided"
  }
  result$journal <- extract_field("Journal", text)
  result$doi <- extract_field("DOI", text)
  
  # Extract geographic and temporal context (updated field names)
  result$country <- extract_field("Country", text)
  result$city_region <- extract_field("City/Region", text)
  
  # Extract study area size with improved parsing  
  area_text <- extract_field("Study Area Size \\(km²\\)", text)
  if (!is.na(area_text)) {
    # Extract numeric value - should already be in km² in new format
    numeric_match <- str_extract(area_text, "[0-9,\\.]+")
    if (!is.na(numeric_match)) {
      result$study_area_size <- as.numeric(str_replace_all(numeric_match, ",", ""))
    }
  }
  
  result$crime_type <- extract_field("Crime Type\\(s\\)", text)
  result$study_period <- extract_field("Study Period", text)
  result$data_sources <- extract_field("Data Sources", text)
  
  # Extract spatial unit information (updated field names)
  result$suoa_type <- extract_field("Type", text)
  result$suoa_size <- extract_field("Size", text) 
  result$number_of_units <- as.integer(extract_field("#Units", text))
  result$population_per_unit <- extract_field("Pop\\./Unit", text)
  result$suoa_justification <- extract_field("Quoted Rationale", text)
  result$justification_category <- extract_field("Rationale Category", text)
  result$justification_quote <- extract_field("Rationale Summary", text)
  
  # Enhanced SUoA justification processing
  if (!is.na(result$justification_category)) {
    # Check if category indicates "Not specified" and update accordingly
    if (str_detect(tolower(result$justification_category), "not specified|no rationale|unclear")) {
      result$justification_category <- "Not specified"
      # Add flag in summary if needed
      if (!is.na(result$justification_quote) && 
          !str_detect(tolower(result$justification_quote), "no explicit rationale")) {
        result$justification_quote <- paste0(result$justification_quote, " [No explicit rationale provided for SUoA choice]")
      }
    }
  }
  
  
  # Extract methodology (updated field names)
  result$study_design <- extract_field("Design", text)
  result$statistical_method <- extract_field("Stat Method", text)
  result$model_type <- extract_field("Model Type", text)
  result$software_used <- extract_field("Software", text)
  result$sampling_approach <- extract_field("Sampling", text)
  
  # Extract sample size
  sample_text <- extract_field("Sample Size \\(choices\\)", text)
  if (!is.na(sample_text)) {
    result$sample_size <- as.integer(str_extract(sample_text, "\\d+"))
  }
  
  result$choice_set_definition <- extract_field("Choice Set", text)
  
  # Extract variable counts from new compact format: "Demo: [#] | Econ: [#] | ..."
  extract_compact_counts <- function(text) {
    # Look for the VARIABLE COUNTS line (without Social category)
    counts_line <- str_extract(text, "Demo:\\s*\\d+\\s*\\|[^\\n]*")
    
    if (!is.na(counts_line)) {
      # Extract individual counts using regex (6 categories)
      demo <- as.integer(str_extract(counts_line, "Demo:\\s*(\\d+)", group = 1))
      econ <- as.integer(str_extract(counts_line, "Econ:\\s*(\\d+)", group = 1))
      crime_attractors <- as.integer(str_extract(counts_line, "Crime Attractors:\\s*(\\d+)", group = 1))
      distance <- as.integer(str_extract(counts_line, "Distance:\\s*(\\d+)", group = 1))
      temporal <- as.integer(str_extract(counts_line, "Temporal:\\s*(\\d+)", group = 1))
      other <- as.integer(str_extract(counts_line, "Other:\\s*(\\d+)", group = 1))
      
      # Calculate total automatically
      total <- sum(c(demo, econ, crime_attractors, distance, temporal, other), na.rm = TRUE)
      
      return(list(
        total = total, demo = demo, econ = econ, crime_attractors = crime_attractors,
        distance = distance, temporal = temporal, other = other
      ))
    }
    return(list(total = NA, demo = NA, econ = NA, crime_attractors = NA, 
                distance = NA, temporal = NA, other = NA))
  }
  
  # Extract counts and calculate total automatically
  counts <- extract_compact_counts(text)
  result$total_variables <- counts$total  # Calculated automatically
  result$demographic_vars <- counts$demo
  result$economic_vars <- counts$econ
  result$crime_attractors_vars <- counts$crime_attractors
  result$distance_vars <- counts$distance
  result$temporal_vars <- counts$temporal
  result$other_vars <- counts$other
  
  # Extract results and quality information (updated field names)
  result$main_results <- extract_field("Main Results", text, multiline = TRUE)
  result$significant_predictors <- extract_field("Significant Predictors", text, multiline = TRUE)  
  result$model_performance <- extract_field("Model Performance", text)
  result$scale_effects <- extract_field("Scale Effects", text, multiline = TRUE)
  result$variable_quality <- extract_field("Variable Info", text)
  result$missing_information <- extract_field("Missing Info", text, multiline = TRUE)
  result$extraction_confidence <- extract_field("Confidence", text)
  
  # Extract data limitations (new section)
  # Look for the data limitations section in the text
  limitations_pattern <- "\\*\\*Data Limitations:\\*\\*([\\s\\S]*?)(?=\\*\\*Overall Limitation Summary:|$)"
  limitations_match <- str_extract(text, limitations_pattern)
  
  if (!is.na(limitations_match)) {
    # Clean up the extracted limitations text
    limitations_text <- str_replace(limitations_match, "^\\*\\*Data Limitations:\\*\\*\\s*", "")
    result$data_limitations <- str_trim(limitations_text)
    
    # Extract individual quotes and categorize them
    quote_pattern <- "\\[Quote \\d+\\]:\\s*\"([^\"]+)\"[^\\n]*\\nCategory:\\s*([^\\n]+)"
    quotes <- str_extract_all(limitations_text, quote_pattern)[[1]]
    
    if (length(quotes) > 0) {
      # Store all quotes together
      result$limitation_quotes <- paste(quotes, collapse = " | ")
      
      # Categorize limitations by type
      spatial_quotes <- str_extract_all(limitations_text, "Category:\\s*Spatial[^\\n]*\\n(?:Summary:[^\\n]*)?")[[1]]
      temporal_quotes <- str_extract_all(limitations_text, "Category:\\s*Temporal[^\\n]*\\n(?:Summary:[^\\n]*)?")[[1]]
      quality_quotes <- str_extract_all(limitations_text, "Category:\\s*Data quality[^\\n]*\\n(?:Summary:[^\\n]*)?")[[1]]
      method_quotes <- str_extract_all(limitations_text, "Category:\\s*Methodological[^\\n]*\\n(?:Summary:[^\\n]*)?")[[1]]
      general_quotes <- str_extract_all(limitations_text, "Category:\\s*Generalizability[^\\n]*\\n(?:Summary:[^\\n]*)?")[[1]]
      
      result$spatial_limitations <- if(length(spatial_quotes) > 0) paste(spatial_quotes, collapse = " | ") else NA_character_
      result$temporal_limitations <- if(length(temporal_quotes) > 0) paste(temporal_quotes, collapse = " | ") else NA_character_
      result$data_quality_limitations <- if(length(quality_quotes) > 0) paste(quality_quotes, collapse = " | ") else NA_character_
      result$methodological_limitations <- if(length(method_quotes) > 0) paste(method_quotes, collapse = " | ") else NA_character_
      result$generalizability_limitations <- if(length(general_quotes) > 0) paste(general_quotes, collapse = " | ") else NA_character_
    }
  }
  
  # Extract overall limitation summary
  result$limitation_summary <- extract_field("Overall Limitation Summary", text, multiline = TRUE)
  
  return(result)
}

# Enhanced variable extraction function
extract_enhanced_variables <- function(text) {
  
  # Define variable categories to search for (simplified to 6 categories, removed social/behavioral)
  categories <- c(
    "DEMOGRAPHIC",
    "ECONOMIC", 
    "CRIME ATTRACTORS",
    "DISTANCE / ACCESSIBILITY",
    "TEMPORAL / CONTROL",
    "OTHER"
  )
  
  all_variables <- data.frame()
  
  for (category in categories) {
    # Extract section for this category (updated pattern for new format)
    pattern <- paste0(category, "\\s*:?\\s*([\\s\\S]*?)(?=\\n[A-Z /-]+\\s*:|##|$)")
    section_match <- str_extract(text, pattern)
    
    if (!is.na(section_match)) {
      # Extract variable content
      var_content <- str_replace(section_match, paste0("^", category, "\\s*:?\\s*"), "")
      var_content <- str_replace(var_content, "```.*$", "")
      
      # Split into individual variable lines
      var_lines <- str_split(var_content, "\n")[[1]]
      var_lines <- var_lines[str_trim(var_lines) != ""]
      var_lines <- var_lines[!str_detect(var_lines, "^\\[|^Example:|^\\s*$")]
      
      # Process each variable line
      for (line in var_lines) {
        line <- str_trim(line)
        if (line != "" && !str_detect(line, "^-\\s*$")) {
          # Remove leading dash or bullet
          var_name <- str_replace(line, "^-\\s*", "")
          
          # If line contains pipe separators, extract components
          if (str_detect(var_name, "\\|")) {
            parts <- str_split(var_name, "\\|")[[1]]
            var_name <- str_trim(parts[1])
            description <- if(length(parts) > 1) str_trim(parts[2]) else NA_character_
            unit <- if(length(parts) > 2) str_trim(parts[3]) else NA_character_
            source <- if(length(parts) > 3) str_trim(parts[4]) else NA_character_
          } else {
            description <- NA_character_
            unit <- NA_character_
            source <- NA_character_
          }
          
          if (var_name != "" && !str_detect(var_name, "^\\[")) {
            all_variables <- rbind(all_variables, data.frame(
              variable_name = var_name,
              category = str_to_lower(str_replace_all(category, " VARIABLES", "")),
              description = description,
              unit = unit,
              source = source,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  return(all_variables)
}

# Function to process multiple study files
process_enhanced_studies <- function(file_path) {
  
  cat("Reading extraction file:", file_path, "\n")
  
  # Read the file
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  full_text <- read_file(file_path)
  
  # Split by study separators (## STUDY or # Study or similar patterns)
  study_pattern <- "(?=##?\\s*(?:STUDY|Study)\\s+(?:IDENTIFICATION|\\d+))"
  studies <- str_split(full_text, study_pattern, perl = TRUE)[[1]]
  studies <- studies[str_trim(studies) != ""]
  
  cat("Found", length(studies), "potential study sections\n")
  
  all_study_data <- data.frame()
  all_variables_data <- data.frame()
  
  for (i in seq_along(studies)) {
    study_text <- studies[i]
    
    # Skip if study text is too short or doesn't contain key information
    if (nchar(study_text) < 100 || !str_detect(study_text, "Title\\s*:")) {
      next
    }
    
    cat("Processing study", i, "\n")
    
    # Extract study information
    study_info <- extract_enhanced_study_info(study_text)
    study_info$study_id <- i
    
    # Extract variables
    variables <- extract_enhanced_variables(study_text)
    if (nrow(variables) > 0) {
      variables$study_id <- i
      variables$title <- study_info$title
      all_variables_data <- rbind(all_variables_data, variables)
    }
    
    # Convert study_info to data frame
    study_df <- data.frame(study_info, stringsAsFactors = FALSE)
    all_study_data <- rbind(all_study_data, study_df)
  }
  
  cat("Successfully processed", nrow(all_study_data), "studies with", nrow(all_variables_data), "variables\n")
  
  return(list(
    studies = all_study_data,
    variables = all_variables_data
  ))
}

# Function to handle cross-national studies (studies 20, 36, 50)
# These are the same study but with different countries and should be kept as separate entries
handle_cross_national_studies <- function(studies_df, variables_df) {
  
  cat("Checking for cross-national studies (20, 36, 50)...\n")
  
  # Check if we have study 20 which contains data for Netherlands, UK, and Australia
  study_20_exists <- any(studies_df$study_id == 20)
  
  if (study_20_exists) {
    cat("Found cross-national study 20. Creating separate entries for studies 36 and 50...\n")
    
    # Get study 20 data
    study_20 <- studies_df[studies_df$study_id == 20, ]
    
    # Create study 36 (UK portion) if it doesn't exist
    if (!any(studies_df$study_id == 36)) {
      study_36 <- study_20
      study_36$study_id <- 36
      study_36$country <- "United Kingdom"
      study_36$city_region <- "Birmingham"
      studies_df <- rbind(studies_df, study_36)
      
      # Add variables for study 36
      if (nrow(variables_df) > 0) {
        vars_20 <- variables_df[variables_df$study_id == 20, ]
        if (nrow(vars_20) > 0) {
          vars_36 <- vars_20
          vars_36$study_id <- 36
          variables_df <- rbind(variables_df, vars_36)
        }
      }
    }
    
    # Create study 50 (Australia portion) if it doesn't exist  
    if (!any(studies_df$study_id == 50)) {
      study_50 <- study_20
      study_50$study_id <- 50
      study_50$country <- "Australia"
      study_50$city_region <- "Brisbane"
      studies_df <- rbind(studies_df, study_50)
      
      # Add variables for study 50
      if (nrow(variables_df) > 0) {
        vars_20 <- variables_df[variables_df$study_id == 20, ]
        if (nrow(vars_20) > 0) {
          vars_50 <- vars_20
          vars_50$study_id <- 50
          variables_df <- rbind(variables_df, vars_50)
        }
      }
    }
    
    # Update study 20 to reflect Netherlands portion
    studies_df[studies_df$study_id == 20, "country"] <- "Netherlands"
    studies_df[studies_df$study_id == 20, "city_region"] <- "The Hague"
    
    cat("Cross-national study handling complete. Total studies:", nrow(studies_df), "\n")
  }
  
  return(list(
    studies = studies_df,
    variables = variables_df
  ))
}

# Main execution function
main_enhanced_extraction <- function(input_file = NULL, output_dir = "Output") {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Default input file if not specified
  if (is.null(input_file)) {
    input_file <- "Data/enhanced_extraction_results.txt"  # Update with your file
  }
  
  # Process studies
  cat("Starting enhanced variable extraction process...\n")
  results <- process_enhanced_studies(input_file)
  
  # Handle cross-national studies (20, 36, 50) - keep as separate studies with different countries
  cross_national_results <- handle_cross_national_studies(results$studies, results$variables)
  studies_final <- cross_national_results$studies
  variables_final <- cross_national_results$variables
  
  
  # Calculate summary statistics
  summary_stats <- studies_final %>%
    summarise(
      total_studies = n(),
      mean_variables = mean(total_variables, na.rm = TRUE),
      median_suoa_size = median(suoa_size_km2, na.rm = TRUE),
      countries = n_distinct(country, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("\nSummary Statistics:\n")
  print(summary_stats)
  
  # Save results
  write_csv(studies_final, file.path(output_dir, "enhanced_studies_data.csv"))
  write_csv(variables_final, file.path(output_dir, "enhanced_variables_data.csv"))
  write_csv(summary_stats, file.path(output_dir, "enhanced_extraction_summary.csv"))
  
  cat("\nResults saved to:", output_dir, "\n")
  cat("- enhanced_studies_data.csv:", nrow(studies_final), "studies\n")
  cat("- enhanced_variables_data.csv:", nrow(variables_final), "variables\n")
  cat("- enhanced_extraction_summary.csv: summary statistics\n")
  
  return(list(
    studies = studies_final,
    variables = variables_final,
    summary = summary_stats
  ))
}

# Run the enhanced extraction if script is executed directly
if (!interactive()) {
  # You can specify your input file here
  # results <- main_enhanced_extraction("path/to/your/extraction_file.txt")
  cat("Enhanced extraction script loaded. Run main_enhanced_extraction() to process data.\n")
} else {
  cat("Enhanced extraction functions loaded successfully!\n")
  cat("Use main_enhanced_extraction('your_file.txt') to process extraction data.\n")
}
