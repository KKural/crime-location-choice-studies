# =============================================================================#
# Variable Categorization Analysis and Improvement Script
# =============================================================================#
# Purpose: Analyze extracted variables and improve categorization
# Use this script to examine uncategorized variables and update categories
# =============================================================================#

library(dplyr)
library(stringr)
library(readr)

# Load your extracted data
analyze_variable_categorization <- function(data_file_path) {
  # Read the data
  df <- read_csv(data_file_path)
  
  # Analyze uncategorized variables
  uncategorized_vars <- df %>%
    filter(!is.na(Uncategorized_Variables)) %>%
    select(Title, Uncategorized_Variables, Uncategorized_Count)
  
  # Split and analyze all uncategorized variables
  all_uncategorized <- character()
  
  for (i in 1:nrow(uncategorized_vars)) {
    if (!is.na(uncategorized_vars$Uncategorized_Variables[i])) {
      vars <- str_split(uncategorized_vars$Uncategorized_Variables[i], "; ")[[1]]
      all_uncategorized <- c(all_uncategorized, vars)
    }
  }
  
  # Get frequency of uncategorized variables
  var_freq <- table(all_uncategorized)
  var_freq_df <- data.frame(
    Variable = names(var_freq),
    Frequency = as.numeric(var_freq),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Frequency))
  
  cat("Most common uncategorized variables:\n")
  print(head(var_freq_df, 20))
  
  # Analyze variable patterns
  cat("\nVariable patterns analysis:\n")
  
  # Check for demographic patterns
  demo_patterns <- c("population", "age", "race", "ethnic", "household", "income", "education", "employment")
  econ_patterns <- c("economic", "business", "commercial", "retail", "poverty", "wealth", "job", "work")
  env_patterns <- c("land use", "building", "facility", "amenity", "green", "park", "density")
  dist_patterns <- c("distance", "proximity", "access", "transport", "travel", "location")
  temp_patterns <- c("time", "temporal", "season", "day", "hour", "trend", "lag")
  
  for (pattern in demo_patterns) {
    matches <- grep(pattern, all_uncategorized, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Potential demographic variables with '", pattern, "':\n")
      print(unique(matches))
      cat("\n")
    }
  }
  
  for (pattern in econ_patterns) {
    matches <- grep(pattern, all_uncategorized, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Potential economic variables with '", pattern, "':\n")
      print(unique(matches))
      cat("\n")
    }
  }
  
  for (pattern in env_patterns) {
    matches <- grep(pattern, all_uncategorized, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Potential environmental variables with '", pattern, "':\n")
      print(unique(matches))
      cat("\n")
    }
  }
  
  for (pattern in dist_patterns) {
    matches <- grep(pattern, all_uncategorized, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Potential distance variables with '", pattern, "':\n")
      print(unique(matches))
      cat("\n")
    }
  }
  
  for (pattern in temp_patterns) {
    matches <- grep(pattern, all_uncategorized, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      cat("Potential temporal variables with '", pattern, "':\n")
      print(unique(matches))
      cat("\n")
    }
  }
  
  return(list(
    uncategorized_summary = uncategorized_vars,
    variable_frequencies = var_freq_df,
    all_uncategorized = all_uncategorized
  ))
}

# Function to suggest improvements to categorization
suggest_categorization_improvements <- function(analysis_results) {
  cat("Suggested improvements for variable categorization:\n\n")
  
  # Analyze the most common uncategorized variables
  common_vars <- analysis_results$variable_frequencies %>%
    filter(Frequency >= 2) %>%
    arrange(desc(Frequency))
  
  for (i in 1:min(nrow(common_vars), 20)) {
    var <- common_vars$Variable[i]
    freq <- common_vars$Frequency[i]
    
    cat("Variable: '", var, "' (appears ", freq, " times)\n")
    
    # Suggest category based on keywords
    var_lower <- tolower(var)
    
    suggested_category <- "Unknown"
    
    if (str_detect(var_lower, "population|age|race|ethnic|household|family|gender|education|social|resident|demographic")) {
      suggested_category <- "Demographic"
    } else if (str_detect(var_lower, "income|employment|economic|business|commercial|retail|poverty|wealth|job|work|unemploy|salary|wage|occupation")) {
      suggested_category <- "Economic"
    } else if (str_detect(var_lower, "land use|building|facility|amenity|green|park|density|zoning|residential|commercial|industrial|environment|physical")) {
      suggested_category <- "Environmental"
    } else if (str_detect(var_lower, "distance|proximity|access|transport|travel|location|nearest|close|far|central|peripheral|mobility")) {
      suggested_category <- "Distance"
    } else if (str_detect(var_lower, "time|temporal|season|day|hour|trend|lag|control|dummy|fixed|random|interaction")) {
      suggested_category <- "Temporal"
    }
    
    cat("  Suggested category: ", suggested_category, "\n")
    cat("  Add to category keywords: '", var_lower, "'\n\n")
  }
}

# Function to create comprehensive variable dictionary
create_variable_dictionary <- function(analysis_results) {
  # Create a comprehensive dictionary of all variables found
  all_vars <- analysis_results$all_uncategorized
  
  # Create a data frame with variable suggestions
  dictionary <- data.frame(
    Variable = all_vars,
    Suggested_Category = NA,
    Confidence = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(dictionary)) {
    var <- dictionary$Variable[i]
    var_lower <- tolower(var)
    
    # Score each category
    demo_score <- sum(str_count(var_lower, "population|age|race|ethnic|household|family|gender|education|social|resident|demographic"))
    econ_score <- sum(str_count(var_lower, "income|employment|economic|business|commercial|retail|poverty|wealth|job|work|unemploy|salary|wage|occupation"))
    env_score <- sum(str_count(var_lower, "land use|building|facility|amenity|green|park|density|zoning|residential|commercial|industrial|environment|physical"))
    dist_score <- sum(str_count(var_lower, "distance|proximity|access|transport|travel|location|nearest|close|far|central|peripheral|mobility"))
    temp_score <- sum(str_count(var_lower, "time|temporal|season|day|hour|trend|lag|control|dummy|fixed|random|interaction"))
    
    scores <- c(demo_score, econ_score, env_score, dist_score, temp_score)
    categories <- c("Demographic", "Economic", "Environmental", "Distance", "Temporal")
    
    max_score <- max(scores)
    if (max_score > 0) {
      dictionary$Suggested_Category[i] <- categories[which.max(scores)]
      dictionary$Confidence[i] <- if (max_score >= 2) "High" else "Medium"
    } else {
      dictionary$Suggested_Category[i] <- "Unknown"
      dictionary$Confidence[i] <- "Low"
    }
  }
  
  return(dictionary)
}

# Example usage:
# Run this after your main extraction script
# analysis_results <- analyze_variable_categorization("path/to/your/detailed_variable_listing.csv")
# suggest_categorization_improvements(analysis_results)
# dictionary <- create_variable_dictionary(analysis_results)
# write_csv(dictionary, "variable_categorization_dictionary.csv")
