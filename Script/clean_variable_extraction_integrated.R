# Load required libraries
library(tidyverse)
library(stringr)
library(here)
library(readr)

# Function to standardize country names
standardize_country <- function(country) {
  if (is.na(country)) return(NA)
  
  country_clean <- str_trim(tolower(country))
  
  # Handle multiple countries (cross-national studies)
  if (str_detect(country_clean, ";")) {
    # For cross-national studies, return as is but standardized
    countries <- str_split(country_clean, ";")[[1]]
    countries <- str_trim(countries)
    standardized <- sapply(countries, function(x) {
      switch(x,
        "usa" = "United States",
        "united states" = "United States",
        "us" = "United States",
        "america" = "United States",
        "uk" = "United Kingdom", 
        "united kingdom" = "United Kingdom",
        "britain" = "United Kingdom",
        "great britain" = "United Kingdom",
        "england" = "United Kingdom",
        "netherlands" = "Netherlands",
        "holland" = "Netherlands",
        "australia" = "Australia",
        "belgium" = "Belgium",
        "china" = "China",
        "japan" = "Japan",
        "new zealand" = "New Zealand",
        "india" = "India",
        "northern ireland" = "Northern Ireland",
        str_to_title(x)  # Default: capitalize first letter
      )
    })
    return(paste(standardized, collapse = "; "))
  }
  
  # Single country standardization
  standardized <- switch(country_clean,
    "usa" = "United States",
    "united states" = "United States", 
    "us" = "United States",
    "america" = "United States",
    "uk" = "United Kingdom",
    "united kingdom" = "United Kingdom", 
    "britain" = "United Kingdom",
    "great britain" = "United Kingdom",
    "england" = "United Kingdom",
    "netherlands" = "Netherlands",
    "holland" = "Netherlands",
    "australia" = "Australia",
    "belgium" = "Belgium", 
    "china" = "China",
    "japan" = "Japan",
    "new zealand" = "New Zealand",
    "india" = "India",
    "northern ireland" = "Northern Ireland",
    str_to_title(country_clean)  # Default: capitalize first letter
  )
  
  return(standardized)
}

# Function to standardize city names
standardize_city <- function(city) {
  if (is.na(city)) return(NA)
  
  city_clean <- str_trim(tolower(city))
  
  # Handle multiple cities (cross-national studies)
  if (str_detect(city_clean, ";")) {
    cities <- str_split(city_clean, ";")[[1]]
    cities <- str_trim(cities)
    standardized <- sapply(cities, function(x) {
      switch(x,
        "the hague" = "The Hague",
        "hague" = "The Hague",
        "den haag" = "The Hague",
        "chicago" = "Chicago",
        "birmingham" = "Birmingham", 
        "london" = "London",
        "brisbane" = "Brisbane",
        "amsterdam" = "Amsterdam",
        "ghent" = "Ghent",
        "gent" = "Ghent",
        "perth" = "Perth",
        "belfast" = "Belfast",
        "york" = "York",
        "richmond" = "Richmond",
        "tampa" = "Tampa",
        "baltimore" = "Baltimore",
        "sendai" = "Sendai",
        "chennai" = "Chennai",
        "madras" = "Chennai",
        "zg city" = "ZG City",
        "zg" = "ZG City",
        "east flanders" = "East Flanders",
        "oost-vlaanderen" = "East Flanders",
        "west midlands" = "West Midlands",
        "dorset" = "Dorset",
        "greater the hague" = "Greater The Hague",
        "greater the hague area" = "Greater The Hague",
        "greater the hague region" = "Greater The Hague",
        "the hague area" = "Greater The Hague",
        "the hague and north-holland" = "The Hague and North-Holland",
        "entire netherlands" = "Entire Netherlands",
        "perth, western australia" = "Perth",
        "high wycombe, beaconsfield, marlow in buckinghamshire" = "High Wycombe area",
        "baltimore city, maryland" = "Baltimore",
        "richmond, va" = "Richmond",
        "tampa, florida" = "Tampa",
        str_to_title(x)  # Default: capitalize properly
      )
    })
    return(paste(standardized, collapse = "; "))
  }
  
  # Single city standardization
  standardized <- switch(city_clean,
    "the hague" = "The Hague",
    "hague" = "The Hague", 
    "den haag" = "The Hague",
    "chicago" = "Chicago",
    "birmingham" = "Birmingham",
    "london" = "London", 
    "brisbane" = "Brisbane",
    "amsterdam" = "Amsterdam",
    "ghent" = "Ghent",
    "gent" = "Ghent",
    "perth" = "Perth", 
    "belfast" = "Belfast",
    "york" = "York",
    "richmond" = "Richmond",
    "tampa" = "Tampa",
    "baltimore" = "Baltimore",
    "sendai" = "Sendai",
    "chennai" = "Chennai", 
    "madras" = "Chennai",
    "zg city" = "ZG City",
    "zg" = "ZG City",
    "east flanders" = "East Flanders",
    "oost-vlaanderen" = "East Flanders",
    "west midlands" = "West Midlands", 
    "dorset" = "Dorset",
    "greater the hague" = "Greater The Hague",
    "greater the hague area" = "Greater The Hague", 
    "greater the hague region" = "Greater The Hague",
    "the hague area" = "Greater The Hague",
    "the hague and north-holland" = "The Hague and North-Holland",
    "entire netherlands" = "Entire Netherlands",
    "perth, western australia" = "Perth",
    "high wycombe, beaconsfield, marlow in buckinghamshire" = "High Wycombe area",
    "baltimore city, maryland" = "Baltimore", 
    "richmond, va" = "Richmond",
    "tampa, florida" = "Tampa",
    str_to_title(city_clean)  # Default: capitalize properly
  )
  
  return(standardized)
}

# Function to classify a variable based on keywords
classify_variable_by_keywords <- function(variable_name) {
  variable_lower <- tolower(variable_name)
  
  # Infrastructure/Physical Environment
  if (str_detect(variable_lower, "construction type|garage|central heating|air conditioning|built surface area|house type|building type|dwelling type|residential units|building height|high-rise|floor area|property type|housing type|apartment|detached|semi-detached|terraced|condominium|single-family|multi-family|fence|wall|barrier|road|river|access control|gated")) {
    return("infrastructure")
  }
  
  # Distance/Access/Proximity
  if (str_detect(variable_lower, "distance|proximity|accessibility|ease of escape|escape route|centrality|network centrality|highway|ramp|connector|train|transport|transportation|access|travel time|commute")) {
    return("distance_access")
  }
  
  # Demographic
  if (str_detect(variable_lower, "population|age|gender|sex|ethnic|race|diversity|indigenous|household|family|single-person|composition|structure|migrants|foreign|immigration|birth country|hukou|residential mobility|mobility|churn|transience")) {
    return("demographic")
  }
  
  # Economic/Socioeconomic
  if (str_detect(variable_lower, "income|wealth|affluence|poverty|deprivation|seifa|unemployment|employment|rent|housing value|property value|repayment|price|socioeconomic|economic|financial|imd|index of multiple deprivation")) {
    return("economic")
  }
  
  # Land Use/Commercial Activity
  if (str_detect(variable_lower, "retail|commercial|business|shops|stores|restaurants|bars|hotels|banks|atm|schools|hospitals|health care|cultural|sport|leisure|entertainment|mixed land-use|land use|zoning")) {
    return("land_use")
  }
  
  # Social/Behavioral/Crime Environment
  if (str_detect(variable_lower, "collective efficacy|social|surveillance|surveillability|guardianship|crime|offense|prior|previous|victimization|disorder|unrest|contagion|repeat|recency|criminal activity|offender")) {
    return("social_behavioral")
  }
  
  # Temporal
  if (str_detect(variable_lower, "time|temporal|day|night|hour|season|duration|period|timing|when|schedule")) {
    return("temporal_control")
  }
  
  # Environmental/Physical Features
  if (str_detect(variable_lower, "trees|vegetation|grass|plant|green|park|natural|weather|lighting|visibility|cover|concealment|window|sidewalk|street")) {
    return("environmental")
  }
  
  # Default to other if no match
  return("other")
}

# Function to extract variables from the core information text
extract_variables_from_text <- function(text) {
  # Initialize empty lists for each variable category
  demographics <- c()
  economic <- c()
  land_use <- c()
  infrastructure <- c()
  distance_access <- c()
  social_behavioral <- c()
  environmental <- c()
  temporal_control <- c()
  other_vars <- c()
  
  # Also keep track of all variables found for reclassification
  all_variables_found <- c()
  
  # Split text into lines and clean carriage returns
  lines <- str_split(text, "\n")[[1]]
  lines <- str_replace_all(lines, "\r", "")  # Remove carriage returns
  
  # Find variable sections - look for multiple patterns
  current_category <- ""
  in_variable_section <- FALSE
  
  for (line in lines) {
    # Check if we're in any variable list section
    if (str_detect(line, "## COMPLETE INDIVIDUAL VARIABLE LIST|## VARIABLES|Variable List|Streetscape variables:|Crime attractors, generators, and detractors:|Socioeconomic features:")) {
      in_variable_section <- TRUE
      next
    }
    
    # Check for other variable section indicators
    if (str_detect(line, "VARIABLE LIST|\\*\\*DEMOGRAPHIC VARIABLES|\\*\\*ECONOMIC VARIABLES|\\*\\*LAND USE VARIABLES|\\*\\*INFRASTRUCTURE VARIABLES|\\*\\*DISTANCE|\\*\\*SOCIAL|\\*\\*ENVIRONMENTAL|\\*\\*TEMPORAL|\\*\\*OTHER")) {
      in_variable_section <- TRUE
    }
    
    # Stop if we reach certain sections
    if (str_detect(line, "## VARIABLE SUMMARY COUNTS|## KEY FINDINGS|## DATA QUALITY")) {
      in_variable_section <- FALSE
      current_category <- ""
    }
    
    # Process lines both in and out of variable sections for flexibility
    # Don't restrict to just in_variable_section
    
    # Check for category headers (including study-specific categories)
    if (str_detect(line, "\\*\\*DEMOGRAPHIC VARIABLES|Demographic Variables:|DEMOGRAPHIC VARIABLES:")) {
      current_category <- "demographic"
    } else if (str_detect(line, "\\*\\*ECONOMIC VARIABLES|Economic Variables:|ECONOMIC VARIABLES:")) {
      current_category <- "economic"
    } else if (str_detect(line, "\\*\\*LAND USE VARIABLES|Land Use Variables:|LAND USE VARIABLES:")) {
      current_category <- "land_use"
    } else if (str_detect(line, "\\*\\*INFRASTRUCTURE VARIABLES|Infrastructure Variables:|INFRASTRUCTURE VARIABLES:")) {
      current_category <- "infrastructure"
    } else if (str_detect(line, "\\*\\*DISTANCE/ACCESSIBILITY VARIABLES|Distance/Accessibility variables:|DISTANCE/ACCESSIBILITY VARIABLES:|\\*\\*DISTANCE")) {
      current_category <- "distance_access"
    } else if (str_detect(line, "\\*\\*CRIME OPPORTUNITY VARIABLES|Crime Opportunity Variables:|CRIME OPPORTUNITY VARIABLES:|\\*\\*CRIME OPPORTUNITY")) {
      current_category <- "crime_opportunity"
    } else if (str_detect(line, "\\*\\*SOCIAL/BEHAVIORAL VARIABLES|Social/Behavioral Variables:|SOCIAL/BEHAVIORAL VARIABLES:|\\*\\*SOCIAL|Socioeconomic features:")) {
      current_category <- "social_behavioral"
    } else if (str_detect(line, "\\*\\*ENVIRONMENTAL VARIABLES|Environmental Variables:|ENVIRONMENTAL VARIABLES:")) {
      current_category <- "environmental"
    } else if (str_detect(line, "\\*\\*TEMPORAL/CONTROL VARIABLES|Temporal/Control Variables:|TEMPORAL/CONTROL VARIABLES:|\\*\\*TEMPORAL")) {
      current_category <- "temporal_control"
    } else if (str_detect(line, "\\*\\*OTHER VARIABLES|Other Variables:|OTHER VARIABLES:")) {
      current_category <- "other"
    } else if (str_detect(line, "\\*\\*Streetscape variables:|Streetscape variables:")) {
      current_category <- "environmental"  # Map streetscape to environmental
    } else if (str_detect(line, "\\*\\*Distance/Accessibility variables:|Distance/Accessibility variables:")) {
      current_category <- "distance_access"
    }
    
    # Extract variable names from different formats
    var_name <- ""
    
    # Format 1: Lines that start with "  - " (standard format)
    if (str_detect(line, "^\\s*-\\s+") && current_category != "" && 
        !str_detect(line, "\\*\\*") && 
        !str_detect(line, "Total Independent|Variables:|Main Results|Significant Predictors|Model Performance|Scale Effects|Variable Information|Missing Information|Extraction Confidence")) {
      
      var_name <- str_replace(line, "^\\s*-\\s+", "")
      var_name <- str_replace(var_name, ":.*$", "") # Remove description after colon
      var_name <- str_trim(var_name)
    }
    
    # Format 2: Numbered list format "1. Variable name | description"
    else if (str_detect(line, "^\\s*\\d+\\.\\s+") && current_category != "" &&
             !str_detect(line, "Not specified")) {
      
      var_name <- str_replace(line, "^\\s*\\d+\\.\\s+", "")
      var_name <- str_replace(var_name, "\\s*\\|.*$", "") # Remove description after |
      var_name <- str_trim(var_name)
    }
    
    # Format 3: Simple list with different indentation
    else if (str_detect(line, "^\\s{2,8}[^-*#\\d]") && current_category != "" &&
             !str_detect(line, "\\*\\*") &&
             !str_detect(line, "Not specified|Not mentioned") &&
             !str_detect(line, "Total Independent|Variables:|Main Results|Significant Predictors|Model Performance")) {
      
      var_name <- str_trim(line)
      var_name <- str_replace(var_name, ":.*$", "") # Remove description after colon
    }
    
    # Format 4: Handle lines like "- **TEMPORAL/CONTROL VARIABLES:** Timing of crime within the week and day"
    else if (str_detect(line, "^\\s*-\\s+\\*\\*[A-Z/\\s]+VARIABLES\\*\\*:") && 
             !str_detect(line, "Not specified|Not mentioned")) {
      
      # Extract the variable after the colon
      var_name <- str_extract(line, "(?<=:).*")
      var_name <- str_trim(var_name)
      
      # Determine category from the line itself
      if (str_detect(line, "DEMOGRAPHIC")) {
        current_category <- "demographic"
      } else if (str_detect(line, "ECONOMIC")) {
        current_category <- "economic"
      } else if (str_detect(line, "LAND USE")) {
        current_category <- "land_use"
      } else if (str_detect(line, "INFRASTRUCTURE")) {
        current_category <- "infrastructure"
      } else if (str_detect(line, "DISTANCE|ACCESSIBILITY")) {
        current_category <- "distance_access"
      } else if (str_detect(line, "SOCIAL|BEHAVIORAL")) {
        current_category <- "social_behavioral"
      } else if (str_detect(line, "ENVIRONMENTAL")) {
        current_category <- "environmental"
      } else if (str_detect(line, "TEMPORAL|CONTROL")) {
        current_category <- "temporal_control"
      } else if (str_detect(line, "OTHER")) {
        current_category <- "other"
      }
    }
    
    # Format 5: Handle direct variable listings after category headers (new format)
    else if (current_category != "" && str_detect(line, "^\\s*[A-Z]") && 
             !str_detect(line, "\\*\\*") &&
             !str_detect(line, "Not specified|Not mentioned|Total Independent|Variables:|Main Results")) {
      
      var_name <- str_trim(line)
      var_name <- str_replace(var_name, ":.*$", "") # Remove description after colon
    }
    
    # Format 6: Handle variables in lines starting with specific patterns
    else if (str_detect(line, "^\\s*[A-Za-z].*\\|.*\\|") && current_category != "" &&
             !str_detect(line, "Not specified|Not mentioned")) {
      
      # Extract variable name (before first |)
      var_name <- str_extract(line, "^[^|]+")
      var_name <- str_trim(var_name)
    }
    
    # Clean and validate variable name
    if (nchar(var_name) >= 2 && !str_detect(var_name, "^\\s*$") && 
        !str_detect(var_name, "Not specified|Not mentioned|Variables:|VARIABLES")) {
      
      # Add to the list of all variables found for later reclassification
      all_variables_found <- c(all_variables_found, var_name)
      
      # Also temporarily store in original category for verification
      if (current_category == "crime_opportunity") {
        # Store crime opportunity variables separately for now
        all_variables_found <- c(all_variables_found, paste0("ORIG_CRIME_OPP:", var_name))
      }
    }
  }
  
  # After going through all sections, also check the broader text for any missed variables
  all_text <- paste(lines, collapse = " ")
  
  # Look for additional variable mentions in the broader text if we found few variables
  total_vars_found <- length(all_variables_found)
  
  if (total_vars_found < 3) {
    # If very few variables found, try extracting from quoted text and broader sections
    
    # Extract variables from specific patterns in text
    if (str_detect(all_text, "Number of people on the street|Percent fence|Percent wall|Percent window|Percent grass|Percent plant|Percent sidewalk")) {
      all_variables_found <- c(all_variables_found, "Number of people on the street", "Percent fence", "Percent wall", "Percent window", "Percent grass", "Percent plant", "Percent sidewalk")
    }
    
    if (str_detect(all_text, "ATM and banks|Bus stops|Subway stations|Schools|Hospitals|Guard stations")) {
      all_variables_found <- c(all_variables_found, "ATM and banks", "Bus stops", "Subway stations", "Schools", "Hospitals", "Guard stations")
    }
    
    if (str_detect(all_text, "Percent migrants|Percent low-rent houses")) {
      all_variables_found <- c(all_variables_found, "Percent migrants", "Percent low-rent houses")
    }
    
    if (str_detect(all_text, "Proximity|distance from offender|Distance")) {
      all_variables_found <- c(all_variables_found, "Proximity")
    }
    
    # Look for variables mentioned in model results or findings
    if (str_detect(all_text, "Mean housing repayment|affluence")) {
      all_variables_found <- c(all_variables_found, "Mean housing repayment")
    }
    
    if (str_detect(all_text, "Proportion of single-family dwellings|target accessibility")) {
      all_variables_found <- c(all_variables_found, "Proportion of single-family dwellings")
    }
    
    if (str_detect(all_text, "Proximity to city center|Brisbane CBD")) {
      all_variables_found <- c(all_variables_found, "Proximity to city center")
    }
    
    if (str_detect(all_text, "Number of households|residential dwellings")) {
      all_variables_found <- c(all_variables_found, "Number of households")
    }
    
    if (str_detect(all_text, "Residential mobility|social cohesion")) {
      all_variables_found <- c(all_variables_found, "Residential mobility")
    }
    
    # General fallback patterns
    if (str_detect(all_text, "timing|temporal|time|day|week|hour") && length(temporal_control) == 0) {
      all_variables_found <- c(all_variables_found, "Temporal variables")
    }
    if (str_detect(all_text, "distance|proximity|accessibility") && length(distance_access) == 0) {
      all_variables_found <- c(all_variables_found, "Distance/accessibility variables")
    }
    if (str_detect(all_text, "demographic|age|sex|gender|ethnicity|population") && length(demographics) == 0) {
      all_variables_found <- c(all_variables_found, "Demographic variables")
    }
    if (str_detect(all_text, "economic|income|employment|deprivation|affluence") && length(economic) == 0) {
      all_variables_found <- c(all_variables_found, "Economic variables")
    }
  }
  
  # Now reclassify ALL variables using keyword-based classification
  # Reset all category lists
  demographics <- c()
  economic <- c()
  land_use <- c()
  infrastructure <- c()
  distance_access <- c()
  social_behavioral <- c()
  environmental <- c()
  temporal_control <- c()
  other_vars <- c()
  
  # Reclassify each variable using keywords
  for (var in all_variables_found) {
    if (nchar(var) >= 2) {
      # Clean up variable name and remove any prefixes
      clean_var <- str_replace(var, "^ORIG_CRIME_OPP:", "")
      
      category <- classify_variable_by_keywords(clean_var)
      
      if (category == "demographic") {
        demographics <- c(demographics, clean_var)
      } else if (category == "economic") {
        economic <- c(economic, clean_var)
      } else if (category == "land_use") {
        land_use <- c(land_use, clean_var)
      } else if (category == "infrastructure") {
        infrastructure <- c(infrastructure, clean_var)
      } else if (category == "distance_access") {
        distance_access <- c(distance_access, clean_var)
      } else if (category == "social_behavioral") {
        social_behavioral <- c(social_behavioral, clean_var)
      } else if (category == "environmental") {
        environmental <- c(environmental, clean_var)
      } else if (category == "temporal_control") {
        temporal_control <- c(temporal_control, clean_var)
      } else {
        other_vars <- c(other_vars, clean_var)
      }
    }
  }
  
  # Remove duplicates from each category
  demographics <- unique(demographics)
  economic <- unique(economic)
  land_use <- unique(land_use)
  infrastructure <- unique(infrastructure)
  distance_access <- unique(distance_access)
  social_behavioral <- unique(social_behavioral)
  environmental <- unique(environmental)
  temporal_control <- unique(temporal_control)
  other_vars <- unique(other_vars)
  
  # Return a list with all variables
  return(list(
    demographic = demographics,
    economic = economic,
    land_use = land_use,
    infrastructure = infrastructure,
    distance_access = distance_access,
    social_behavioral = social_behavioral,
    environmental = environmental,
    temporal_control = temporal_control,
    other = other_vars
  ))
}

# Function to extract study metadata including methodological information
extract_study_metadata <- function(text) {
  # Helper function to extract field value with more robust patterns
  extract_field <- function(text, field_name) {
    # Try multiple patterns to handle variations in formatting
    patterns <- c(
      paste0("\\*\\*", field_name, ":\\*\\*\\s*([^\n\r]+)"),
      paste0("\\*\\*", field_name, ":\\*\\*([^\n\r]+)"),
      paste0("\\*\\*", field_name, ":\\s*\\*\\*\\s*([^\n\r]+)")
    )
    
    for (pattern in patterns) {
      match <- str_match(text, pattern)
      if (!is.na(match[1, 2])) {
        return(str_trim(match[1, 2]))
      }
    }
    return(NA)
  }
  
  # Extract key metadata using more robust patterns
  title <- extract_field(text, "Title")
  year <- extract_field(text, "Year")
  country <- extract_field(text, "Country")
  city <- extract_field(text, "City/Region")
  study_area_size <- extract_field(text, "Study Area Size")
  study_area_description <- extract_field(text, "Study Area Description")
  crime_type <- extract_field(text, "Crime Type")
  crime_types_all <- extract_field(text, "Crime Types \\(All\\)")
  study_period <- extract_field(text, "Study Period")
  data_sources <- extract_field(text, "Data Sources")
  
  # SUoA Information
  suoa_type <- extract_field(text, "SUoA Type")
  suoa_size <- extract_field(text, "SUoA Size")
  suoa_description <- extract_field(text, "SUoA Description")
  num_units <- extract_field(text, "Number of Units")
  population_per_unit <- extract_field(text, "Population per Unit")
  suoa_justification <- extract_field(text, "Justification for SUoA Choice")
  
  # Methodology Information
  study_design <- extract_field(text, "Study Design")
  statistical_method <- extract_field(text, "Statistical Method")
  model_type <- extract_field(text, "Model Type")
  software_used <- extract_field(text, "Software Used")
  sampling_approach <- extract_field(text, "Sampling Approach")
  sample_size <- extract_field(text, "Sample Size")
  choice_set_definition <- extract_field(text, "Choice Set Definition")
  estimation_method <- extract_field(text, "Estimation Method")
  
  # Key Findings
  main_results <- extract_field(text, "Main Results")
  significant_predictors <- extract_field(text, "Significant Predictors")
  model_performance <- extract_field(text, "Model Performance")
  scale_effects <- extract_field(text, "Scale Effects")
  
  # Data Quality
  variable_info_quality <- extract_field(text, "Variable Information Quality")
  missing_information <- extract_field(text, "Missing Information")
  extraction_confidence <- extract_field(text, "Extraction Confidence")
  
  # Clean extracted values function
  clean_field <- function(field) {
    if (is.na(field)) return(NA)
    cleaned <- str_trim(str_replace_all(field, "\\*", ""))
    # Remove common placeholder patterns
    if (cleaned == "" || 
        cleaned == "[Not specified]" || 
        cleaned == "[Not mentioned]" ||
        cleaned == "Not specified" ||
        cleaned == "Not mentioned" ||
        str_detect(cleaned, "^\\[Not .+\\]$")) {
      return(NA)
    }
    return(cleaned)
  }
  
  return(list(
    # Basic Study Information
    title = clean_field(title),
    year = clean_field(year),
        
    # Study Context
    country = standardize_country(clean_field(country)),
    city = standardize_city(clean_field(city)),
    study_area_size = clean_field(study_area_size),
    study_area_description = clean_field(study_area_description),
    crime_type = clean_field(crime_type),
    crime_types_all = clean_field(crime_types_all),
    study_period = clean_field(study_period),
    data_sources = clean_field(data_sources),
    
    # SUoA Information
    suoa_type = clean_field(suoa_type),
    suoa_size = clean_field(suoa_size),
    suoa_description = clean_field(suoa_description),
    num_units = clean_field(num_units),
    population_per_unit = clean_field(population_per_unit),
    suoa_justification = clean_field(suoa_justification),
    
    # Methodology
    study_design = clean_field(study_design),
    statistical_method = clean_field(statistical_method),
    model_type = clean_field(model_type),
    software_used = clean_field(software_used),
    sampling_approach = clean_field(sampling_approach),
    sample_size = clean_field(sample_size),
    choice_set_definition = clean_field(choice_set_definition),
    estimation_method = clean_field(estimation_method),
    
    # Key Findings
    main_results = clean_field(main_results),
    significant_predictors = clean_field(significant_predictors),
    model_performance = clean_field(model_performance),
    scale_effects = clean_field(scale_effects),
    
    # Data Quality
    variable_info_quality = clean_field(variable_info_quality),
    missing_information = clean_field(missing_information),
    extraction_confidence = clean_field(extraction_confidence)
  ))
}

# Load the CSV file
elicit_data <- read_csv(here("Data", "Elicit - Extract.csv"), show_col_types = FALSE)

# Create empty dataframes for results
studies_metadata <- data.frame()
variables_long <- data.frame()

# Process each study
for (i in 1:nrow(elicit_data)) {
  # Skip if core_infor is missing
  if (is.na(elicit_data$`core infor`[i])) {
    next
  }
  
  # Extract metadata
  metadata <- extract_study_metadata(elicit_data$`core infor`[i])
  
  # Extract study ID from filename (e.g., "14_target_choice..." -> 14)
  filename_study_id <- as.numeric(str_extract(elicit_data$Filename[i], "^\\d+"))
  if (is.na(filename_study_id)) {
    filename_study_id <- i  # Fallback to row number if extraction fails
  }
  
  # Handle cross-national study (Study 20 contains data for studies 20, 36, and 50)
  # Check if this is study 20 (cross-national comparison study)
  if (filename_study_id == 20) {
    # Study 20 contains data for Netherlands; United Kingdom; Australia
    # We'll create three separate entries for studies 20, 36, and 50
    
    # Study 20: Netherlands portion
    metadata$country <- standardize_country("Netherlands")
    metadata$city <- standardize_city("The Hague")
  }
  
  # Create metadata row with all extracted information
  study_row <- data.frame(
    study_id = filename_study_id,
    filename = elicit_data$Filename[i],
    
    # Basic Study Information
    title = metadata$title,
    year = metadata$year,
    
    
    # Study Context
    country = metadata$country,
    city = metadata$city,
    study_area_size = metadata$study_area_size,
    study_area_description = metadata$study_area_description,
    crime_type = metadata$crime_type,
    crime_types_all = metadata$crime_types_all,
    study_period = metadata$study_period,
    data_sources = metadata$data_sources,
    
    # SUoA Information
    suoa_type = metadata$suoa_type,
    suoa_size = metadata$suoa_size,
    suoa_description = metadata$suoa_description,
    num_units = metadata$num_units,
    population_per_unit = metadata$population_per_unit,
    suoa_justification = metadata$suoa_justification,
    
    # Methodology
    study_design = metadata$study_design,
    statistical_method = metadata$statistical_method,
    model_type = metadata$model_type,
    software_used = metadata$software_used,
    sampling_approach = metadata$sampling_approach,
    sample_size = metadata$sample_size,
    choice_set_definition = metadata$choice_set_definition,
    estimation_method = metadata$estimation_method,
    
    # Key Findings
    main_results = metadata$main_results,
    significant_predictors = metadata$significant_predictors,
    model_performance = metadata$model_performance,
    scale_effects = metadata$scale_effects,
    
    # Data Quality
    variable_info_quality = metadata$variable_info_quality,
    missing_information = metadata$missing_information,
    extraction_confidence = metadata$extraction_confidence,
    
    stringsAsFactors = FALSE
  )
  
  # Add to metadata dataframe
  studies_metadata <- rbind(studies_metadata, study_row)
  
  # Special handling for cross-national study 20: create additional entries for studies 36 and 50
  if (filename_study_id == 20) {
    # Create entry for study 36 (UK portion)
    study_row_36 <- study_row
    study_row_36$study_id <- 36
    study_row_36$country <- standardize_country("United Kingdom")
    study_row_36$city <- standardize_city("Birmingham")
    studies_metadata <- rbind(studies_metadata, study_row_36)
    
    # Create entry for study 50 (Australia portion)
    study_row_50 <- study_row
    study_row_50$study_id <- 50
    study_row_50$country <- standardize_country("Australia")
    study_row_50$city <- standardize_city("Brisbane")
    studies_metadata <- rbind(studies_metadata, study_row_50)
  }
  
  # Extract variables
  variables <- extract_variables_from_text(elicit_data$`core infor`[i])
  
  # Convert variables to long format
  for (category in names(variables)) {
    if (length(variables[[category]]) > 0) {
      var_rows <- data.frame(
        study_id = filename_study_id,
        variable_category = category,
        variable_name = variables[[category]],
        stringsAsFactors = FALSE
      )
      variables_long <- rbind(variables_long, var_rows)
      
      # Special handling for cross-national study 20: also add variables for studies 36 and 50
      if (filename_study_id == 20) {
        # Add same variables for study 36 (UK portion)
        var_rows_36 <- var_rows
        var_rows_36$study_id <- 36
        variables_long <- rbind(variables_long, var_rows_36)
        
        # Add same variables for study 50 (Australia portion)
        var_rows_50 <- var_rows
        var_rows_50$study_id <- 50
        variables_long <- rbind(variables_long, var_rows_50)
      }
    }
  }
}

# Create summary statistics
# Variable category counts per study
var_counts <- variables_long %>%
  group_by(study_id, variable_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = variable_category, values_from = count, values_fill = 0)

# Overall variable frequency
var_frequency <- variables_long %>%
  group_by(variable_category) %>%
  summarise(
    total_variables = n(),
    unique_variables = n_distinct(variable_name),
    studies_using = n_distinct(study_id)
  )

# Most common variables
common_variables <- variables_long %>%
  group_by(variable_name, variable_category) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  arrange(desc(frequency))

# Create separate frequency tables for methodological analysis
study_design_freq <- studies_metadata %>%
  filter(!is.na(study_design)) %>%
  count(study_design, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

statistical_method_freq <- studies_metadata %>%
  filter(!is.na(statistical_method)) %>%
  count(statistical_method, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

model_type_freq <- studies_metadata %>%
  filter(!is.na(model_type)) %>%
  count(model_type, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

software_freq <- studies_metadata %>%
  filter(!is.na(software_used)) %>%
  count(software_used, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

estimation_method_freq <- studies_metadata %>%
  filter(!is.na(estimation_method)) %>%
  count(estimation_method, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# Sampling approach analysis
sampling_freq <- studies_metadata %>%
  filter(!is.na(sampling_approach)) %>%
  count(sampling_approach, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# SUoA type analysis
suoa_type_freq <- studies_metadata %>%
  filter(!is.na(suoa_type)) %>%
  count(suoa_type, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# Crime type analysis
crime_type_freq <- studies_metadata %>%
  filter(!is.na(crime_type)) %>%
  count(crime_type, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# Temporal analysis by year and methodology
temporal_methods <- studies_metadata %>%
  filter(!is.na(year) & !is.na(statistical_method)) %>%
  count(year, statistical_method) %>%
  arrange(year)

# Create output folder if it doesn't exist
if (!dir.exists(here("Output"))) {
  dir.create(here("Output"), recursive = TRUE)
}

# Create comprehensive combined dataset
# 1. Combine metadata with variable counts
combined_metadata <- studies_metadata %>%
  left_join(var_counts, by = "study_id")

# 2. Create a summary of all variables per study as a single text field
variables_summary <- variables_long %>%
  group_by(study_id) %>%
  summarise(
    all_variables = paste(variable_name, collapse = "; "),
    total_variables_count = n(),
    .groups = "drop"
  )

# 3. Create category-specific variable lists
variables_by_category <- variables_long %>%
  group_by(study_id, variable_category) %>%
  summarise(variables = paste(variable_name, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = variable_category, 
              values_from = variables, 
              names_prefix = "vars_",
              values_fill = "")

# 4. Combine all data into one comprehensive dataset
comprehensive_dataset <- combined_metadata %>%
  left_join(variables_summary, by = "study_id") %>%
  left_join(variables_by_category, by = "study_id")

# Fill NA values with 0 for count columns and "" for variable lists
count_cols <- names(comprehensive_dataset)[str_detect(names(comprehensive_dataset), "^(demographic|economic|land_use|infrastructure|distance_access|social_behavioral|environmental|temporal_control|other)$")]
var_cols <- names(comprehensive_dataset)[str_detect(names(comprehensive_dataset), "^vars_")]

comprehensive_dataset <- comprehensive_dataset %>%
  mutate(across(all_of(count_cols), ~replace_na(., 0))) %>%
  mutate(across(all_of(var_cols), ~replace_na(., "")))

# Save all datasets
write_csv(comprehensive_dataset, here("Output", "comprehensive_studies_dataset.csv"))
write_csv(studies_metadata, here("Output", "studies_metadata.csv"))
write_csv(variables_long, here("Output", "variables_extracted.csv"))
write_csv(var_counts, here("Output", "variable_counts_by_study.csv"))
write_csv(var_frequency, here("Output", "variable_frequency_summary.csv"))
write_csv(common_variables, here("Output", "common_variables.csv"))

# Save methodological summaries
write_csv(study_design_freq, here("Output", "study_design_frequency.csv"))
write_csv(statistical_method_freq, here("Output", "statistical_method_frequency.csv"))
write_csv(model_type_freq, here("Output", "model_type_frequency.csv"))
write_csv(software_freq, here("Output", "software_frequency.csv"))
write_csv(estimation_method_freq, here("Output", "estimation_method_frequency.csv"))
write_csv(sampling_freq, here("Output", "sampling_approach_frequency.csv"))
write_csv(suoa_type_freq, here("Output", "suoa_type_frequency.csv"))
write_csv(crime_type_freq, here("Output", "crime_type_frequency.csv"))
write_csv(temporal_methods, here("Output", "temporal_methods_analysis.csv"))

# Print summary statistics
cat("===========================================\n")
cat("EXTRACTION SUMMARY\n")
cat("===========================================\n")
cat("Total studies processed:", nrow(studies_metadata), "\n")
cat("Total variables extracted:", nrow(variables_long), "\n")
cat("Studies with methodology info:", sum(!is.na(studies_metadata$statistical_method)), "\n")
cat("Studies with complete SUoA info:", sum(!is.na(studies_metadata$suoa_type) & !is.na(studies_metadata$num_units)), "\n")

cat("\n===========================================\n")
cat("TOP METHODOLOGICAL APPROACHES\n")
cat("===========================================\n")
if(nrow(statistical_method_freq) > 0) {
  cat("Statistical Methods:\n")
  print(head(statistical_method_freq, 5))
}

if(nrow(model_type_freq) > 0) {
  cat("\nModel Types:\n")
  print(head(model_type_freq, 5))
}

if(nrow(software_freq) > 0) {
  cat("\nSoftware Used:\n")
  print(head(software_freq, 5))
}
