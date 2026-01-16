# Comprehensive Spatial Unit Analysis - New CSV Version
# Updated to work with 20250714_standardized_unit_sizes_with_groups_new.csv
#
# INTEGRATION WITH MANUSCRIPT:
# This analysis script generates tables and figures that are automatically used by the Rmd manuscript.
# When you update this script and run it, a new Excel file will be created that contains all the
# tables. To update the manuscript with these new results:
# 

# Function to create a folder with a date argument
make_folder <- function(date = Sys.Date(), subfolder = NULL) {
  # Convert the provided date to "YYYYMMDD" format
  date_string <- format(date, "%Y%m%d")
  
  # Create the main folder name with the date
  main_folder_name <- paste0(date_string, "_Analysis & Results")
  
  # If a subfolder is specified, append it to the main folder path
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)  # Create nested folders if necessary
  }
  
  return(full_folder_path)  # Return the folder path to use later
}

# Create a function to save output with date
custom_save <- function(data, folder_name, file_description, save_function, file_extension = ".csv", ...) {
  # Current date in YYYYMMDD format
  current_date <- format(Sys.Date(), "%Y%m%d")
  
  # Ensure file description has the correct extension
  if (!grepl(paste0("\\", file_extension, "$"), file_description)) {
    file_description <- paste0(file_description, file_extension)
  }
  
  # Create the file name using the date and the file description
  file_name <- paste0(current_date, "_", file_description)
  
  # Define the path for the output file
  file_path <- here::here(folder_name, file_name)
  
  # Use the provided save function
  save_function(data, file_path, ...)
}

# Function to save as png
ggsave_png <- function(ggp, output_folder, file_description, width = 8, height = 6, dpi = 1200) {
  current_date <- format(Sys.Date(), "%Y%m%d")
  file_name <- paste0(current_date, "_", file_description, ".png")
  file_path <- here::here(output_folder, file_name)
  ggplot2::ggsave(
    filename = file_name,
    device = "png",
    plot = ggp,
    path = output_folder,
    width = width,
    height = height,
    dpi = dpi,
    limitsize = TRUE
  )
}

# Input and output setup-----------------------------

# Create the output folder using custom function
output_folder <- make_folder()

# read 2026014_working.xlsx from data folder
df_raw <- readxl::read_excel("Data/2026014_working.xlsx")

# Identify columns to remove (containing 'supporting' or 'reasoning')
columns_to_remove <- grep("supporting|reasoning|Supporting|Reasoning", 
                          names(df_raw), 
                          value = TRUE, 
                          ignore.case = TRUE)

# Remove supporting and reasoning columns
df_clean <- df_raw |> dplyr::select(-all_of(columns_to_remove))

# Convert numeric columns to proper types
df_clean <- df_clean |>
  dplyr::mutate(
    Number_of_Units = as.numeric(Number_of_Units),
    Number_of_Data_Sources = as.numeric(Number_of_Data_Sources),
    Crime_Incidents = as.numeric(Crime_Incidents)
  )

# Display cleaning summary
ncol(df_raw)
length(columns_to_remove)
ncol(df_clean)
nrow(df_clean)
names(df_clean)

# Clean Title column ----
df_clean <- df_clean |>
  dplyr::mutate(
    Title = Title |>
      stringr::str_trim() |>                          # Remove leading/trailing whitespace
      stringr::str_replace_all("\\s+", " ") |>        # Replace multiple spaces with single
      stringr::str_remove("\\s*\\*+\\s*$") |>         # Remove asterisks at end
      stringr::str_to_title()                          # Convert to title case
  )

df_clean$Title

table(df_clean$Country)

df_clean$City

# Clean City column ----
df_clean <- df_clean |>
  dplyr::mutate(
    City_Clean = dplyr::case_when(
      # Nationwide or national level studies
      stringr::str_detect(City, stringr::regex("nationwide|national level", ignore_case = TRUE)) ~ "Nationwide",
      # Multiple cities (contains semicolon)
      stringr::str_detect(City, ";") ~ "Multiple cities",
      # Regions/provinces (East Flanders, West Midlands, etc.)
      stringr::str_detect(City, stringr::regex("East Flandres|East Flanders|West Midlands|Dorset", ignore_case = TRUE)) ~ "Multiple cities",
      # Not mentioned
      stringr::str_detect(City, stringr::regex("not mentioned", ignore_case = TRUE)) ~ "Nationwide",
      # Single city - clean it
      TRUE ~ City |>
        stringr::str_trim() |>
        stringr::str_replace_all("\\s+", " ") |>
        stringr::str_replace("City$", "") |>           # Remove "City" at end
        stringr::str_replace("city$", "") |>           # Remove "city" at end  
        stringr::str_trim()
    )
  ) |>
  dplyr::relocate(City_Clean, .after = City)

df_clean$City_Clean

df_clean$Total_Study_Area

# Clean Total_Study_Area column ----
df_clean <- df_clean |>
  dplyr::mutate(
    Total_Study_Area_Approx_in_sqkm = dplyr::case_when(
      # Handle "Not specified" cases
      is.na(Total_Study_Area) | 
        stringr::str_detect(Total_Study_Area, stringr::regex("not specified", ignore_case = TRUE)) ~ NA_real_,
      # Clean numeric values
      TRUE ~ Total_Study_Area |>
        stringr::str_remove("~") |>                    # Remove approximately symbol
        stringr::str_remove_all(",") |>                # Remove commas
        stringr::str_trim() |>                         # Trim whitespace
        as.numeric()                                   # Convert to numeric
    )
  ) |>
  dplyr::relocate(Total_Study_Area_Approx_in_sqkm, .after = Area_Unit)

df_clean$Total_Study_Area_Approx_in_sqkm


# Clean Spatial_Unit_Name column ----
df_clean <- df_clean |>
  dplyr::mutate(
    Spatial_Unit_Name_Clean = Spatial_Unit_Name |>
      # Remove line breaks and trim
      stringr::str_remove_all("\\r\\n|\\n|\\r") |>
      stringr::str_trim() |>
      # Standardize neighborhoods (British vs American spelling)
      stringr::str_replace_all(stringr::regex("Neighbourhood", ignore_case = TRUE), "Neighborhood") |>
      # Standardize plural forms
      stringr::str_replace(stringr::regex("^Street segment$", ignore_case = TRUE), "Street segments") |>
      stringr::str_replace(stringr::regex("^Block group$", ignore_case = TRUE), "Block groups") |>
      stringr::str_replace(stringr::regex("^Census block$", ignore_case = TRUE), "Census blocks") |>
      stringr::str_replace(stringr::regex("^Grid cell$", ignore_case = TRUE), "Grid cells") |>
      stringr::str_replace(stringr::regex("^Residential unit$", ignore_case = TRUE), "Residential units") |>
      # Simplify complex descriptions - keep main term
      stringr::str_replace("Statistical Area 2 \\(SA2s\\)/neighborhoods", "Statistical Area 2 (SA2)") |>
      stringr::str_replace("Output Areas \\(referred to as Neighbourhoods\\)", "Output Areas") |>
      # Standardize Census terms
      stringr::str_replace("Census Lower Level Super Output Area \\(LSOA\\)", "Lower Super Output Areas (LSOA)") |>
      stringr::str_replace("Census Lower Super Output Area \\(LSOA\\)", "Lower Super Output Areas (LSOA)") |>
      stringr::str_replace("Lower Super Output Areas \\(LSOAs\\)", "Lower Super Output Areas (LSOA)") |>
      stringr::str_replace("Census Statistical Area 2 \\(SA2\\)", "Statistical Area 2 (SA2)") |>
      stringr::str_replace("Census Statistical Area 2", "Statistical Area 2 (SA2)") |>
      # Final trim
      stringr::str_trim()
  ) |>
  dplyr::relocate(Spatial_Unit_Name_Clean, .after = Spatial_Unit_Name)

df_clean$Spatial_Unit_Name_Clean

df_clean$Unit_Size
df_clean$unit

# Clean unit column ----
df_clean <- df_clean |>
  dplyr::mutate(
    unit = dplyr::case_when(
      is.na(unit) | stringr::str_trim(unit) == "" ~ NA_character_,
      TRUE ~ stringr::str_trim(unit)
    )
  )

df_clean$unit

# Clean Unit_Size and convert to sq km ----
df_clean <- df_clean |>
  dplyr::mutate(
    Unit_Size_Clean = dplyr::case_when(
      # Handle "Not specified" cases
      is.na(Unit_Size) | 
        stringr::str_detect(Unit_Size, stringr::regex("not specified", ignore_case = TRUE)) ~ NA_real_,
      
      # Handle grid dimensions (e.g., "200 x 200", "170 x 191", "140 x 140")
      stringr::str_detect(Unit_Size, "\\d+\\s*x\\s*\\d+") ~ {
        dims <- stringr::str_extract_all(Unit_Size, "\\d+")
        round(as.numeric(sapply(dims, function(x) as.numeric(x[1]) * as.numeric(x[2]))), 3)  # Area in square meters
      },
      
      # Clean numeric values (remove commas)
      TRUE ~ round(as.numeric(stringr::str_remove_all(stringr::str_trim(Unit_Size), ",")), 3)
    ),
    
    # Convert all to square kilometers
    Unit_Size_SqKm = dplyr::case_when(
      is.na(Unit_Size_Clean) ~ NA_real_,
      # Grid cells are in square meters, convert to sq km
      stringr::str_detect(Unit_Size, "\\d+\\s*x\\s*\\d+") ~ round(Unit_Size_Clean / 1000000, 3),
      # Use unit column for conversion
      unit == "square meters" ~ round(Unit_Size_Clean / 1000000, 3),
      unit == "square miles" ~ round(Unit_Size_Clean * 2.58999, 3),
      unit == "square kilometers" ~ round(Unit_Size_Clean, 3),
      # Default: assume square kilometers
      TRUE ~ round(Unit_Size_Clean, 3)
    )
  ) |>
  dplyr::relocate(Unit_Size_Clean, unit, Unit_Size_SqKm, .after = Unit_Size)

df_clean$unit
df_clean$Unit_Size_SqKm
names(df_clean)

# Clean Rationale_Category column ----
df_clean <- df_clean |>
  dplyr::mutate(
    Rationale_Category_Clean = Rationale_Category |>
      # Fix typo: "heory" → "Theory"
      stringr::str_replace("^heory", "Theory") |>
      # Remove trailing periods
      stringr::str_remove("\\.$") |>
      # Trim whitespace
      stringr::str_trim() |>
      # Standardize capitalization
      stringr::str_to_title() |>
      # Remove " and " and replace with comma for splitting
      stringr::str_replace_all(" And ", ", ") |>
      # Split, sort in standard order, and rejoin with semicolon
      purrr::map_chr(function(x) {
        # Split by comma
        items <- stringr::str_split(x, ",\\s*")[[1]] |> stringr::str_trim()
        # Define standard order
        order_ref <- c("Theory", "Previous Research", "Method", "Data Availability", "Administrative Convenience")
        # Sort items according to standard order
        sorted_items <- items[order(match(items, order_ref))]
        # Join with semicolon
        paste(sorted_items, collapse = "; ")
      })
  ) |>
  dplyr::relocate(Rationale_Category_Clean, .after = Rationale_Category)

df_clean$Rationale_Category_Clean

df_clean$Crime_Type

# Clean Crime_Type column ----
df_clean <- df_clean |>
  dplyr::mutate(
    Crime_Type_Clean = Crime_Type |>
      # Remove parenthetical abbreviations like (TFP), (TFV)
      stringr::str_remove_all("\\s*\\([A-Z]+\\)") |>
      # Standardize spelling: offences → offenses
      stringr::str_replace_all("offences", "offenses") |>
      # Simplify "any-crime-type" variations
      stringr::str_replace("any-crime-type.*", "Multiple Crime Types") |>
      # Standardize to title case
      stringr::str_to_title() |>
      # Clean up specific terms
      stringr::str_replace_all("Non-Residential", "Non-residential") |>
      stringr::str_replace_all("Extra-Familial", "Extra-familial") |>
      # Trim whitespace
      stringr::str_trim(),
    
    # Create Crime_Category (grouping by crime type)
    Crime_Category = dplyr::case_when(
      # If multiple crime types, keep as multiple
      Crime_Type_Group == "Multiple crime types" ~ "Multiple Crime Types",
      
      # For single crime types, categorize:
      # Burglary (all types)
      stringr::str_detect(Crime_Type_Clean, stringr::regex("burglary|burglaries", ignore_case = TRUE)) ~ "Burglary",
      
      # Robbery (including snatching, pickpocketing)
      stringr::str_detect(Crime_Type_Clean, stringr::regex("robbery|snatching|pickpocketing", ignore_case = TRUE)) ~ "Robbery",
      
      # Theft (all types except snatching/pickpocketing which are robbery)
      stringr::str_detect(Crime_Type_Clean, stringr::regex("theft", ignore_case = TRUE)) ~ "Theft",
      
      # Assault/Violence
      stringr::str_detect(Crime_Type_Clean, stringr::regex("assault|violence", ignore_case = TRUE)) ~ "Assault/Violence",
      
      # Drug offenses
      stringr::str_detect(Crime_Type_Clean, stringr::regex("drug", ignore_case = TRUE)) ~ "Drug Offenses",
      
      # Vandalism/Graffiti
      stringr::str_detect(Crime_Type_Clean, stringr::regex("vandalism|graffiti", ignore_case = TRUE)) ~ "Vandalism",
      
      # Shooting/Bombing
      stringr::str_detect(Crime_Type_Clean, stringr::regex("shooting|bombing", ignore_case = TRUE)) ~ "Violence",
      
      # Other specific crimes
      TRUE ~ "Other"
    )
  ) |>
  dplyr::relocate(Crime_Type_Clean, Crime_Category, .after = Crime_Type_Group)

df_clean$Crime_Type_Clean
df_clean$Crime_Category

df_clean$Crime_Type_Group

df_clean$Independent_Variables

# Clean and format Independent Variables ----
# IMPORTANT: Do NOT change or recount Number_of_Variables - it is already verified.
# This step ONLY creates Independent_Variables_Clean for display/categorization purposes.
df_clean <- df_clean |>
  dplyr::mutate(
    # Format variables with sequential numbering based on user's verified count
    Independent_Variables_Clean = purrr::map2_chr(Independent_Variables, Number_of_Variables, function(vars, n_vars) {
      if (is.na(vars) || stringr::str_trim(vars) == "" || is.na(n_vars) || n_vars == 0) return(NA_character_)
      
      # Try splitting by line breaks first
      var_list <- stringr::str_split(vars, "\\r\\n|\\r|\\n")[[1]] |>
        stringr::str_trim()
      
      # If only one item after line break split, try splitting by semicolons
      if (length(var_list) == 1) {
        var_list <- stringr::str_split(vars, ";")[[1]] |>
          stringr::str_trim()
      }
      
      # Clean each variable
      var_list <- var_list |>
        stringr::str_remove("^\\d+\\.\\s*") |>
        stringr::str_replace_all("\\s+", " ") |>
        stringr::str_trim() |>
        purrr::keep(~ nchar(.x) > 0)
      
      # Use ONLY the first n_vars items (user's verified count)
      var_list <- var_list[1:min(n_vars, length(var_list))]
      
      # Add sequential numbering
      numbered <- paste0(seq_along(var_list), ". ", var_list)
      
      # Join with semicolons
      paste(numbered, collapse = "; ")
    })
  ) |>
  dplyr::relocate(Independent_Variables_Clean, .after = Independent_Variables)

# Create empty columns for variable categorization ----
df_clean <- df_clean |>
  dplyr::mutate(
    Demo_var = NA_character_,
    Demo_count = 0L,
    Env_var = NA_character_,
    Env_count = 0L,
    Econ_var = NA_character_,
    Econ_count = 0L,
    Dist_var = NA_character_,
    Dist_count = 0L,
    Temp_var = NA_character_,
    Temp_count = 0L,
    Other_var = NA_character_,
    Other_count = 0L
  ) |>
  dplyr::relocate(Demo_var, Demo_count, Env_var, Env_count,
                  Econ_var, Econ_count, Dist_var, Dist_count,
                  Temp_var, Temp_count, Other_var, Other_count,
                  .after = Number_of_Variables)

# Manual categorization of variables ----

# Study 1 (Row 1)
df_clean$Env_var[1] <- "1. Construction type (Terraced / Semi-detached / Detached); 2. Number of floors (Ground floor / 1 or more); 3. Rooftop living floor (Yes / No); 4. Garage present (Yes / No); 5. Central heating/AC (Yes / No); 6. Built surface area (1,000 m²)"
df_clean$Env_count[1] <- 6L
df_clean$Dist_var[1] <- "1. Distance to house"
df_clean$Dist_count[1] <- 1L

# Study 2 (Row 2)
df_clean$Temp_var[2] <- "1. Current area of residence (collapsed: long-term vs. short-term residence); 2. Former area of residence (collapsed: combinations of duration and recency)"
df_clean$Temp_count[2] <- 2L
df_clean$Dist_var[2] <- "1. Distance to current area of residence; 2. Distance to former area of residence"
df_clean$Dist_count[2] <- 2L
df_clean$Demo_var[2] <- "1. Population size; 2. Percentage of single-person households; 3. Level of urbanization (Address density)"
df_clean$Demo_count[2] <- 3L
df_clean$Econ_var[2] <- "1. Poverty level (Average value of residential property)"
df_clean$Econ_count[2] <- 1L

# Study 3 (Row 3)
df_clean$Temp_var[3] <- "1. Prior offending history (collapsed conceptual variable representing the interaction of temporal similarity and crime type similarity); 2. Offender's current or former residence status"
df_clean$Temp_count[3] <- 2L
df_clean$Demo_var[3] <- "1. Proportion of non-Western residents; 2. Proportion of single-person households; 3. Population density"
df_clean$Demo_count[3] <- 3L
df_clean$Env_var[3] <- "1. Retail business; 2. Hotels, restaurants, and bars; 3. Schools; 4. Health-care facility; 5. Cultural facility; 6. Sports and leisure facility; 7. Number of employees"
df_clean$Env_count[3] <- 7L
df_clean$Dist_var[3] <- "1. Distance from current residential area"
df_clean$Dist_count[3] <- 1L

# Study 4 (Row 4)
df_clean$Temp_var[4] <- "1. Time spent in location (collapsed from three binary indicators of duration in activity space); 2. Prior crime location and proximity (collapsed from six binary indicators of repeat and near-repeat locations)"
df_clean$Temp_count[4] <- 2L
df_clean$Dist_var[4] <- "1. Proximity to activity space (collapsed from five binary indicators of spatial contiguity order)"
df_clean$Dist_count[4] <- 1L
df_clean$Env_var[4] <- "1. Catering business presence; 2. Retail business presence; 3. School presence"
df_clean$Env_count[4] <- 3L

# Study 5 (Row 5)
df_clean$Env_var[5] <- "1. Malls and supermarkets; 2. Grocers; 3. Terminal markets; 4. Bars and clubs; 5. Cybercafés; 6. Sports stadiums; 7. High schools; 8. ATMs and banks; 9. Carparks; 10. Bus stops; 11. Transportation hubs; 12. Subway stations; 13. Density of road network; 14. Surveillance cameras; 15. Ambient population"
df_clean$Env_count[5] <- 15L
df_clean$Demo_var[5] <- "1. Proportion of migrants; 2. Proportion of youngsters; 3. Socioeconomic heterogeneity"
df_clean$Demo_count[5] <- 3L
df_clean$Dist_var[5] <- "1. Log distance"
df_clean$Dist_count[5] <- 1L

# Study 6 (Row 6)
df_clean$Temp_var[6] <- "1. Time interval between two robberies"
df_clean$Temp_count[6] <- 1L
df_clean$Dist_var[6] <- "1. Distance of journey to prior Robbery; 2. Distance of journey to subsequent Robbery (Distance of JSR)"
df_clean$Dist_count[6] <- 2L
df_clean$Env_var[6] <- "1. Transportation hubs; 2. Subway stations; 3. Bus stops; 4. Bars and clubs; 5. Cybercafés; 6. Malls and supermarkets; 7. High schools; 8. Surveillance cameras; 9. Daily human mobility"
df_clean$Env_count[6] <- 9L
df_clean$Demo_var[6] <- "1. Socioeconomic heterogeneity"
df_clean$Demo_count[6] <- 1L
df_clean$Other_var[6] <- "1. Not arrested in the act of prior Robbery"
df_clean$Other_count[6] <- 1L

# Study 7 (Row 7)
df_clean$Temp_var[7] <- "1. Residential area offender (Current); 2. Residential area family; 3. Former residential area offender; 4. Former crime location"
df_clean$Temp_count[7] <- 4L
df_clean$Dist_var[7] <- "1. Distance from residence of offender"
df_clean$Dist_count[7] <- 1L
df_clean$Env_var[7] <- "1. Number of bars/restaurants/hotels; 2. Neighbourhood disorder; 3. Retail businesses; 4. Schools; 5. Health-care facilities; 6. Cultural facilities; 7. Sport and leisure facilities; 8. Number of employees"
df_clean$Env_count[7] <- 8L
df_clean$Demo_var[7] <- "1. Collective efficacy; 2. Population density; 3. Proportion non-western residents; 4. Proportion single-person households"
df_clean$Demo_count[7] <- 4L

# Study 8 (Row 8)
df_clean$Temp_var[8] <- "1. Recency and type of prior offense (combined interaction of time intervals and same/different crime type); 2. Number of prior offenses (Frequency); 3. Current residence duration; 4. Former residence history (combined duration and recency of move)"
df_clean$Temp_count[8] <- 4L
df_clean$Dist_var[8] <- "1. Distance to prior offense (3 years); 2. Distance to current residence (km)"
df_clean$Dist_count[8] <- 2L
df_clean$Demo_var[8] <- "1. Population density; 2. Proportion of single-person households; 3. Proportion of non-Western residents"
df_clean$Demo_count[8] <- 3L
df_clean$Env_var[8] <- "1. Number of employees; 2. Retail businesses; 3. Hotels, restaurants, and bars; 4. Schools; 5. Health-care facilities; 6. Cultural facilities; 7. Sport and leisure facilities"
df_clean$Env_count[8] <- 7L

# Study 9 (Row 9)
df_clean$Econ_var[9] <- "1. Residential real estate value (Affluence)"
df_clean$Econ_count[9] <- 1L
df_clean$Env_var[9] <- "1. Residential mobility (Community stability)"
df_clean$Env_count[9] <- 1L
df_clean$Demo_var[9] <- "1. Proportion of single-family dwellings (Target accessibility); 2. Number of households (Target availability)"
df_clean$Demo_count[9] <- 2L
df_clean$Dist_var[9] <- "1. Proximity to offender's residence; 2. Proximity to city center"
df_clean$Dist_count[9] <- 2L

# Study 10 (Row 10) - Same as Row 9
df_clean$Econ_var[10] <- "1. Residential real estate value (Affluence)"
df_clean$Econ_count[10] <- 1L
df_clean$Env_var[10] <- "1. Residential mobility (Community stability)"
df_clean$Env_count[10] <- 1L
df_clean$Demo_var[10] <- "1. Proportion of single-family dwellings (Target accessibility); 2. Number of households (Target availability)"
df_clean$Demo_count[10] <- 2L
df_clean$Dist_var[10] <- "1. Proximity to offender's residence; 2. Proximity to city center"
df_clean$Dist_count[10] <- 2L

# Study 11 (Row 11) - Same as Row 9
df_clean$Econ_var[11] <- "1. Residential real estate value (Affluence)"
df_clean$Econ_count[11] <- 1L
df_clean$Env_var[11] <- "1. Residential mobility (Community stability)"
df_clean$Env_count[11] <- 1L
df_clean$Demo_var[11] <- "1. Proportion of single-family dwellings (Target accessibility); 2. Number of households (Target availability)"
df_clean$Demo_count[11] <- 2L
df_clean$Dist_var[11] <- "1. Proximity to offender's residence; 2. Proximity to city center"
df_clean$Dist_count[11] <- 2L

# Study 12 (Row 12)
df_clean$Env_var[12] <- "1. Rivers; 2. Major roads with access control; 3. Major roads without access control; 4. Proportion of houses built after 2000; 5. Proportion of high-rise building"
df_clean$Env_count[12] <- 5L
df_clean$Demo_var[12] <- "1. Social barriers (Offender Hukou status and community majority Hukou status combinations); 2. Number of households (logged)"
df_clean$Demo_count[12] <- 2L
df_clean$Dist_var[12] <- "1. Distance from home"
df_clean$Dist_count[12] <- 1L
df_clean$Econ_var[12] <- "1. Average rent price"
df_clean$Econ_count[12] <- 1L

# Study 13 (Row 13)
df_clean$Temp_var[13] <- "1. Current home location; 2. Previous home location; 3. Previous crime location"
df_clean$Temp_count[13] <- 3L
df_clean$Demo_var[13] <- "1. Density of residents; 2. Number of one-person households; 3. Percentage of non-Western residents"
df_clean$Demo_count[13] <- 3L
df_clean$Env_var[13] <- "1. Number of employees; 2. Retail businesses; 3. Hotel, restaurants and bars; 4. Schools; 5. Healthcare facilities; 6. Cultural facilities; 7. Sport facilities"
df_clean$Env_count[13] <- 7L

# Study 14 (Row 14)
df_clean$Econ_var[14] <- "1. Affluence"
df_clean$Econ_count[14] <- 1L
df_clean$Env_var[14] <- "1. Physical accessibility; 2. Own neighbourhood"
df_clean$Env_count[14] <- 2L
df_clean$Demo_var[14] <- "1. Social disorganisation; 2. Number of properties"
df_clean$Demo_count[14] <- 2L
df_clean$Dist_var[14] <- "1. Proximity home neighbourhood; 2. Proximity city center"
df_clean$Dist_count[14] <- 2L

# Study 15 (Row 15)
df_clean$Env_var[15] <- "1. Primary schools; 2. Middle schools; 3. Hospitals; 4. Basic stores; 5. Markets; 6. Supermarkets; 7. Restaurants; 8. Cinemas; 9. Bars; 10. Banks; 11. Subway stations; 12. Bus stops; 13. Relative Mobility flow (ln)"
df_clean$Env_count[15] <- 13L
df_clean$Demo_var[15] <- "1. Nonlocal proportion"
df_clean$Demo_count[15] <- 1L
df_clean$Dist_var[15] <- "1. Distance (ln(km))"
df_clean$Dist_count[15] <- 1L

# Study 16 (Row 16)
df_clean$Env_var[16] <- "1. Spatial hierarchy term (log(k̂))"
df_clean$Env_count[16] <- 1L
df_clean$Dist_var[16] <- "1. Distance to nearest federal highway (u30); 2. Proximity variable u13; 3. Proximity variable u18; 4. Proximity variable u19"
df_clean$Dist_count[16] <- 4L
df_clean$Demo_var[16] <- "1. Population density (u66); 2. Demographic variable u38; 3. Demographic variable u46; 4. Demographic variable u52; 5. Demographic variable u53; 6. Demographic variable u58; 7. Demographic variable u62"
df_clean$Demo_count[16] <- 7L

# Study 17 (Row 17)
df_clean$Env_var[17] <- "1. Target neighborhood concentrated disadvantage status; 2. Spatial knowledge (Mobility flow); 3. Physical disorder (Observed or Perceived measures); 4. Proportion of buildings built post-2000; 5. Proportion of high-rise buildings (9+ floors); 6. Number of subway stations; 7. Number of bus stops; 8. Number of parks; 9. Number of entertainment venues"
df_clean$Env_count[17] <- 9L
df_clean$Demo_var[17] <- "1. Population size (log)"
df_clean$Demo_count[17] <- 1L
df_clean$Dist_var[17] <- "1. Euclidean distance (log)"
df_clean$Dist_count[17] <- 1L
df_clean$Other_var[17] <- "1. Interaction: Target disadvantage × Spatial knowledge; 2. Interaction: Target disadvantage × Physical disorder; 3. Interaction: Spatial knowledge × Physical disorder; 4. Interaction: Target disadvantage × Spatial knowledge × Physical disorder"
df_clean$Other_count[17] <- 4L

# Study 18 (Row 18)
df_clean$Dist_var[18] <- "1. Euclidean distance (log)"
df_clean$Dist_count[18] <- 1L
df_clean$Demo_var[18] <- "1. Migrants proportion; 2. Young adults proportion"
df_clean$Demo_count[18] <- 2L
df_clean$Env_var[18] <- "1. Bars and clubs; 2. Cybercafés; 3. Schools; 4. Transportation hubs; 5. Wholesale markets; 6. Malls and supermarkets; 7. Bus stops; 8. Subway stations; 9. Car parks"
df_clean$Env_count[18] <- 9L

# Study 19 (Row 19)
df_clean$Env_var[19] <- "1. Malls and supermarkets; 2. Grocers; 3. Wholesale markets; 4. Bars and clubs; 5. Sports stadiums; 6. High schools; 7. ATMs and banks; 8. Carparks; 9. Bus stops; 10. Transportation hubs; 11. Subway stations; 12. Density of road network; 13. Daily human mobility"
df_clean$Env_count[19] <- 13L
df_clean$Demo_var[19] <- "1. Proportion of migrants; 2. Proportion of youngsters; 3. Socioeconomic heterogeneity"
df_clean$Demo_count[19] <- 3L
df_clean$Dist_var[19] <- "1. Log distance"
df_clean$Dist_count[19] <- 1L
df_clean$Other_var[19] <- "1. Detection rate"
df_clean$Other_count[19] <- 1L

# Study 20 (Row 20)
df_clean$Env_var[20] <- "1. Bars and clubs; 2. Restaurants, food stands, etc.; 3. Barbers and beauty salons; 4. Liquor stores; 5. Grocery stores; 6. General merchandise stores; 7. Gas stations; 8. Laundromats; 9. Currency exchange and pawnshops; 10. Drug-related incidents; 11. Prostitution-related incidents; 12. Gambling-related incidents; 13. Main streets; 14. El stations; 15. High schools"
df_clean$Env_count[20] <- 15L
df_clean$Demo_var[20] <- "1. Total population; 2. Ethnic majority (Offender-block racial/ethnic concordance)"
df_clean$Demo_count[20] <- 2L
df_clean$Dist_var[20] <- "1. Proximity (Negative log distance)"
df_clean$Dist_count[20] <- 1L

# Study 21 (Row 21)
df_clean$Env_var[21] <- "1. SWEBER (Spatially weighted burglar exposition rate); 2. Residential mobility"
df_clean$Env_count[21] <- 2L
df_clean$Dist_var[21] <- "1. Proximity to Central Business District (CBD)"
df_clean$Dist_count[21] <- 1L
df_clean$Demo_var[21] <- "1. Ethnic heterogeneity; 2. Home ownership"
df_clean$Demo_count[21] <- 2L
df_clean$Econ_var[21] <- "1. Average value of residential units"
df_clean$Econ_count[21] <- 1L

# Study 22 (Row 22)
df_clean$Temp_var[22] <- "1. Current area of residence; 2. Former area of residence"
df_clean$Temp_count[22] <- 2L
df_clean$Dist_var[22] <- "1. Distance (km); 2. Distance to nearest highway ramp"
df_clean$Dist_count[22] <- 2L
df_clean$Env_var[22] <- "1. Total area covered by retail businesses"
df_clean$Env_count[22] <- 1L
df_clean$Econ_var[22] <- "1. Average real estate property value"
df_clean$Econ_count[22] <- 1L
df_clean$Demo_var[22] <- "1. Number of residents (population size); 2. Population density; 3. Population percentage aged 15–25; 4. Population percentage born abroad; 5. Population percentage single"
df_clean$Demo_count[22] <- 5L

# Study 23 (Row 23)
df_clean$Temp_var[23] <- "1. Combined reliability and relevance of spatial knowledge (Categorical variable combining familiarity factors and activity similarity)"
df_clean$Temp_count[23] <- 1L
df_clean$Dist_var[23] <- "1. Distance to nearest activity node (Spatial proximity predictor)"
df_clean$Dist_count[23] <- 1L
df_clean$Env_var[23] <- "1. Crime opportunity (Target distribution/density control)"
df_clean$Env_count[23] <- 1L

# Study 24 (Row 24)
df_clean$Temp_var[24] <- "1. Residential area of parents (Current/Former/Never); 2. Residential area of siblings (Current/Former/Never); 3. Residential area of children (Current/Former/Never); 4. Residential area of offender (Current/Former/Never); 5. Previous crime location (binary)"
df_clean$Temp_count[24] <- 5L
df_clean$Dist_var[24] <- "1. Euclidean distance"
df_clean$Dist_count[24] <- 1L
df_clean$Demo_var[24] <- "1. Population density; 2. Percentage of residents with a non-Western background; 3. Percentage of single-person households"
df_clean$Demo_count[24] <- 3L
df_clean$Env_var[24] <- "1. Number of employees; 2. Number of retail stores; 3. Number of hotels, restaurants, and bars; 4. Number of schools; 5. Number of health-care facilities; 6. Number of cultural facilities; 7. Number of sport and leisure facilities"
df_clean$Env_count[24] <- 7L

# Study 25 (Row 25)
df_clean$Env_var[25] <- "1. River barrier (Boolean); 2. Road barrier (Boolean); 3. Train connector (Boolean); 4. Residential transience (5-year)"
df_clean$Env_count[25] <- 4L
df_clean$Dist_var[25] <- "1. Distance to offender home (moderated by offender age)"
df_clean$Dist_count[25] <- 1L
df_clean$Econ_var[25] <- "1. Suburb income (weekly category); 2. SEIFA index (Advantage–Disadvantage)"
df_clean$Econ_count[25] <- 2L
df_clean$Demo_var[25] <- "1. Percentage of rental properties; 2. Ethnic heterogeneity (percentage of Indigenous residents moderated by offender ethnicity); 3. Number of residential units"
df_clean$Demo_count[25] <- 3L

# Study 26 (Row 26)
df_clean$Env_var[26] <- "1. Bars and clubs (focal); 2. Fast-food restaurants (focal); 3. Barbers and beauty salons (focal); 4. Liquor stores (focal); 5. Grocers (focal); 6. General merchandise stores (focal); 7. Gas stations (focal); 8. Laundromats (focal); 9. Pawn shops and check-cashing services (focal); 10. Drug-related incidents (focal); 11. Prostitution-related incidents (focal); 12. Illegal gambling incidents (focal); 13. Presence of a main street (focal); 14. Presence of an El station (focal); 15. Presence of a high school (focal); 16. Spatial lag: Bars and clubs; 17. Spatial lag: Fast-food restaurants; 18. Spatial lag: Barbers and beauty salons; 19. Spatial lag: Liquor stores; 20. Spatial lag: Grocers; 21. Spatial lag: General merchandise stores; 22. Spatial lag: Gas stations; 23. Spatial lag: Laundromats; 24. Spatial lag: Pawn shops and check-cashing services; 25. Spatial lag: Drug-related incidents; 26. Spatial lag: Prostitution-related incidents; 27. Spatial lag: Illegal gambling incidents; 28. Spatial lag: Presence of a main street; 29. Spatial lag: Presence of an El station; 30. Spatial lag: Presence of a high school"
df_clean$Env_count[26] <- 30L
df_clean$Demo_var[26] <- "1. Total population (focal); 2. Spatial lag: Total population"
df_clean$Demo_count[26] <- 2L
df_clean$Dist_var[26] <- "1. Distance to target block (km)"
df_clean$Dist_count[26] <- 1L
df_clean$Other_var[26] <- "1. Racial/ethnic composition of target block population (Majority type interacted with offender race)"
df_clean$Other_count[26] <- 1L

# Study 27 (Row 27)
df_clean$Env_var[27] <- "1. Area; 2. Segment type; 3. Betweenness; 4. Bars; 5. Night shops; 6. Night clubs; 7. Restaurants; 8. Shops; 9. Schools; 10. Residences; 11. Trees along the street"
df_clean$Env_count[27] <- 11L

# Study 28 (Row 28)
df_clean$Demo_var[28] <- "1. Ethnic heterogeneity; 2. Percentage of single-family dwellings; 3. Number of residential units"
df_clean$Demo_count[28] <- 3L
df_clean$Dist_var[28] <- "1. Proximity to the offender's home; 2. Proximity to the city centre"
df_clean$Dist_count[28] <- 2L
df_clean$Econ_var[28] <- "1. Affluence (average value of residential real estate)"
df_clean$Econ_count[28] <- 1L
df_clean$Env_var[28] <- "1. Residential mobility"
df_clean$Env_count[28] <- 1L
df_clean$Other_var[28] <- "1. Interaction term for minor vs. adult burglars; 2. Interaction term for native vs. non-native burglars"
df_clean$Other_count[28] <- 2L

# Study 29 (Row 29)
df_clean$Env_var[29] <- "1. Number of people on the street; 2. Percent wall; 3. Percent fence; 4. Percent window; 5. Percent grass; 6. Percent plant; 7. Percent sidewalk; 8. ATM and banks; 9. Bus stops; 10. Subway stations; 11. Schools; 12. Hospitals; 13. Guard stations"
df_clean$Env_count[29] <- 13L
df_clean$Dist_var[29] <- "1. Proximity (to offender's home)"
df_clean$Dist_count[29] <- 1L
df_clean$Demo_var[29] <- "1. Percent migrants"
df_clean$Demo_count[29] <- 1L
df_clean$Econ_var[29] <- "1. Percent low-rent houses"
df_clean$Econ_count[29] <- 1L

# Study 30 (Row 30)
df_clean$Temp_var[30] <- "1. Prior burglary in same LSOA; 2. Prior burglary in same MSOA"
df_clean$Temp_count[30] <- 2L
df_clean$Dist_var[30] <- "1. Distance to home; 2. Distance to city center"
df_clean$Dist_count[30] <- 2L
df_clean$Env_var[30] <- "1. Bus stations; 2. Train stations; 3. Population turnover"
df_clean$Env_count[30] <- 3L
df_clean$Demo_var[30] <- "1. Number of households; 2. Single-family residences; 3. Ethnic diversity"
df_clean$Demo_count[30] <- 3L
df_clean$Econ_var[30] <- "1. Mean house price"
df_clean$Econ_count[30] <- 1L

# Study 31 (Row 31)
df_clean$Dist_var[31] <- "1. Log-distance to offender home"
df_clean$Dist_count[31] <- 1L
df_clean$Temp_var[31] <- "1. Prior offending in ward (dichotomy)"
df_clean$Temp_count[31] <- 1L
df_clean$Env_var[31] <- "1. Ward surface area; 2. Retail stores; 3. Transit stations; 4. Mosques; 5. Temples; 6. Churches; 7. Education institutions; 8. School and college; 9. Personal care businesses; 10. Hospitals; 11. Marriage halls; 12. Jewelry shops; 13. Textile stores; 14. Parks; 15. Recreation facilities; 16. Restaurants; 17. Government offices"
df_clean$Env_count[31] <- 17L
df_clean$Demo_var[31] <- "1. Ward population"
df_clean$Demo_count[31] <- 1L

# Study 32 (Row 32)
df_clean$Dist_var[32] <- "1. Distance from the offender's home"
df_clean$Dist_count[32] <- 1L
df_clean$Temp_var[32] <- "1. Prior burglary in neighborhood"
df_clean$Temp_count[32] <- 1L
df_clean$Env_var[32] <- "1. Building construction type"
df_clean$Env_count[32] <- 1L
df_clean$Demo_var[32] <- "1. Residential density; 2. Proportion non-Belgian residents; 3. Proportion rental units; 4. Number of occupants; 5. Number of residences inside the building; 6. Rental status"
df_clean$Demo_count[32] <- 6L
df_clean$Econ_var[32] <- "1. Median income"
df_clean$Econ_count[32] <- 1L

# Study 33 (Row 33)
df_clean$Dist_var[33] <- "1. Proximity (to offender home)"
df_clean$Dist_count[33] <- 1L
df_clean$Env_var[33] <- "1. Spatial competition factor; 2. Number of properties"
df_clean$Env_count[33] <- 2L
df_clean$Demo_var[33] <- "1. Percent non-native population; 2. Percent population aged 15-25"
df_clean$Demo_count[33] <- 2L
df_clean$Econ_var[33] <- "1. Mean property value"
df_clean$Econ_count[33] <- 1L

# Study 34 (Row 34)
df_clean$Dist_var[34] <- "1. Distance from the offender's home; 2. Distance to the city centre"
df_clean$Dist_count[34] <- 2L
df_clean$Temp_var[34] <- "1. Previous offence location"
df_clean$Temp_count[34] <- 1L
df_clean$Env_var[34] <- "1. Residential churn; 2. Number of potential targets"
df_clean$Env_count[34] <- 2L
df_clean$Demo_var[34] <- "1. Ethnic heterogeneity; 2. Socioeconomic heterogeneity; 3. Offender gender (Class membership predictor); 4. Offender age group (Class membership predictor)"
df_clean$Demo_count[34] <- 4L
df_clean$Econ_var[34] <- "1. Affluence"
df_clean$Econ_count[34] <- 1L
df_clean$Other_var[34] <- "1. Offender prolificacy (Collapsed: 1, 2–3, or 4+ crimes; Class membership predictor)"
df_clean$Other_count[34] <- 1L

# Study 35 (Row 35)
df_clean$Dist_var[35] <- "1. Distance (home-to-target); 2. Distance to city centre"
df_clean$Dist_count[35] <- 2L
df_clean$Env_var[35] <- "1. River (barrier); 2. Major road (connectivity); 3. Military base / police station"
df_clean$Env_count[35] <- 3L

# Study 36 (Row 36)
df_clean$Dist_var[36] <- "1. Proximity to Home; 2. Proximity to Family: immediate; 3. Proximity to Family: intimate partner; 4. Proximity to Family: other relatives; 5. Proximity to School; 6. Proximity to Work; 7. Proximity to Prior offence; 8. Proximity to Prior victim/witness; 9. Proximity to Prior incident; 10. Proximity to Other location"
df_clean$Dist_count[36] <- 10L
df_clean$Env_var[36] <- "1. Opportunity (target availability)"
df_clean$Env_count[36] <- 1L

# Study 37 (Row 37)
df_clean$Dist_var[37] <- "1. Distance; 2. Distance squared"
df_clean$Dist_count[37] <- 2L
df_clean$Demo_var[37] <- "1. Percentage of Black residents; 2. Percentage of Latino residents; 3. Ethnic heterogeneity; 4. Single-parent households"
df_clean$Demo_count[37] <- 4L
df_clean$Env_var[37] <- "1. Residential stability; 2. Percentage of occupied housing"
df_clean$Env_count[37] <- 2L
df_clean$Econ_var[37] <- "1. Concentrated disadvantage"
df_clean$Econ_count[37] <- 1L
df_clean$Other_var[37] <- "1. Relative influence (interaction)"
df_clean$Other_count[37] <- 1L

# Study 38 (Row 38)
df_clean$Env_var[38] <- "1. Surveillability (Index); 2. Accessibility (Index); 3. Ease of escape (Index); 4. Business presence; 5. Mixed land-use; 6. House type"
df_clean$Env_count[38] <- 6L
df_clean$Econ_var[38] <- "1. Wealth (Index)"
df_clean$Econ_count[38] <- 1L
df_clean$Other_var[38] <- "1. Interaction (Surveillability × Collective efficacy)"
df_clean$Other_count[38] <- 1L

# Study 39 (Row 39)
df_clean$Temp_var[39] <- "1. Activity space overlap (time-sensitive)"
df_clean$Temp_count[39] <- 1L

# Study 40 (Row 40)
df_clean$Dist_var[40] <- "1. Idiosyncratic Farness (10 min)"
df_clean$Dist_count[40] <- 1L
df_clean$Env_var[40] <- "1. Idiosyncratic Betweenness (10%); 2. Local Pedestrian (overall) Betweenness (10%); 3. Nonlocal Pedestrian (overall) Betweenness (10%); 4. Vehicular (overall) Betweenness (10%); 5. Residential Turnover (%); 6. Number of Dwellings"
df_clean$Env_count[40] <- 6L
df_clean$Demo_var[40] <- "1. Ethnic Heterogeneity (%); 2. Socioeconomic Heterogeneity (%)"
df_clean$Demo_count[40] <- 2L
df_clean$Econ_var[40] <- "1. Affluence (£10,000s)"
df_clean$Econ_count[40] <- 1L

# Study 41 (Row 41)
df_clean$Env_var[41] <- "1. Number of bars; 2. Number of schools; 3. Number of transit stops; 4. Number of shopping outlets; 5. Auto-network density; 6. Pedestrian-network density; 7. Percent turnover"
df_clean$Env_count[41] <- 7L
df_clean$Demo_var[41] <- "1. Racial/ethnic heterogeneity; 2. Population density; 3. Percent poverty"
df_clean$Demo_count[41] <- 3L
df_clean$Other_var[41] <- "1. Solo crime rate; 2. Group crime spatial lag"
df_clean$Other_count[41] <- 2L

# Study 42 (Row 42)
df_clean$Dist_var[42] <- "1. Distance to highway (D.HIGHWAY); 2. Distance to hospital (D.HOSPITAL)"
df_clean$Dist_count[42] <- 2L
df_clean$Demo_var[42] <- "1. Family density per unit area (FAM.DENSITY)"
df_clean$Demo_count[42] <- 1L
df_clean$Econ_var[42] <- "1. Personal care expenditure per household (P.CARE.PH)"
df_clean$Econ_count[42] <- 1L

# Study 43 (Row 43)
df_clean$Dist_var[43] <- "1. Origin-destination distance (Home-to-offense distance); 2. Distance to city center"
df_clean$Dist_count[43] <- 2L
df_clean$Env_var[43] <- "1. Number of schools; 2. Underground station presence; 3. Retail floor space; 4. River Thames barrier; 5. Population churn"
df_clean$Env_count[43] <- 5L
df_clean$Demo_var[43] <- "1. Ethnic diversity; 2. Population density"
df_clean$Demo_count[43] <- 2L
df_clean$Econ_var[43] <- "1. Deprivation (Index of Multiple Deprivation)"
df_clean$Econ_count[43] <- 1L
df_clean$Temp_var[43] <- "1. Recent riot activity (Previous 24 hours)"
df_clean$Temp_count[43] <- 1L

# Study 44 (Row 44)
df_clean$Dist_var[44] <- "1. Proximity to city center; 2. Proximity (Distance to home)"
df_clean$Dist_count[44] <- 2L
df_clean$Demo_var[44] <- "1. Proportion of single-family dwellings; 2. Number of households"
df_clean$Demo_count[44] <- 2L
df_clean$Env_var[44] <- "1. Residential mobility"
df_clean$Env_count[44] <- 1L
df_clean$Econ_var[44] <- "1. Mean housing repayment (Affluence)"
df_clean$Econ_count[44] <- 1L

# Study 45 (Row 45)
df_clean$Dist_var[45] <- "1. Distance to offender's home; 2. Distance to city center; 3. Adjacency"
df_clean$Dist_count[45] <- 3L
df_clean$Env_var[45] <- "1. Presence of schools; 2. Presence of train stations; 3. Connectivity (major roads); 4. Population turnover; 5. Number of car parks; 6. Number of cars and vans"
df_clean$Env_count[45] <- 6L
df_clean$Demo_var[45] <- "1. Socioeconomic heterogeneity"
df_clean$Demo_count[45] <- 1L

# Study 46 (Row 46)
df_clean$Dist_var[46] <- "1. Distance from offender's home"
df_clean$Dist_count[46] <- 1L
df_clean$Env_var[46] <- "1. Crime opportunity"
df_clean$Env_count[46] <- 1L

# Study 47 (Row 47)
df_clean$Dist_var[47] <- "1. Distance from offender home"
df_clean$Dist_count[47] <- 1L
df_clean$Demo_var[47] <- "1. Population density; 2. Percentage of young adults"
df_clean$Demo_count[47] <- 2L

# Study 48 (Row 48)
df_clean$Dist_var[48] <- "1. Distance to offender's home"
df_clean$Dist_count[48] <- 1L
df_clean$Demo_var[48] <- "1. Population density; 2. Percentage non-Western; 3. Percentage singles"
df_clean$Demo_count[48] <- 3L
df_clean$Env_var[48] <- "1. Retail businesses"
df_clean$Env_count[48] <- 1L
df_clean$Temp_var[48] <- "1. Past burglary data (3 years)"
df_clean$Temp_count[48] <- 1L

# Study 49 (Row 49)
df_clean$Dist_var[49] <- "1. Distance from home to target; 2. Distance to city center"
df_clean$Dist_count[49] <- 2L
df_clean$Demo_var[49] <- "1. Ethnic heterogeneity; 2. Percentage single-parent families; 3. Percentage unemployed"
df_clean$Demo_count[49] <- 3L
df_clean$Env_var[49] <- "1. Residential stability; 2. Percentage vacant housing"
df_clean$Env_count[49] <- 2L
df_clean$Econ_var[49] <- "1. Gini coefficient; 2. Disadvantage factor (latent)"
df_clean$Econ_count[49] <- 2L

# Study 50 (Row 50)
df_clean$Env_var[50] <- "1. Red Light District presence; 2. Tippelzone presence; 3. Capacity for sex work; 4. Number of windows/doors; 5. Window/door density; 6. Business establishments (count); 7. Business type diversity; 8. Day visitor inflow; 9. Night visitor inflow; 10. Visitor diversity; 11. Bicycle flow day; 12. Bicycle flow night; 13. Car flow day; 14. Car flow night; 15. Pedestrian flow day; 16. Pedestrian flow night; 17. Total people count day; 18. Total people count night; 19. Active natural surveillance day; 20. Active natural surveillance night; 21. Passive natural surveillance day; 22. Passive natural surveillance night; 23. Physical disorder; 24. Social disorder; 25. Commercial presence; 26. Entertainment/nightlife presence; 27. Tourism presence; 28. Red-light activity presence; 29. Drug trade presence; 30. Street prostitution presence; 31. Homeless presence; 32. Youth gathering presence; 33. Disorder index; 34. Crime generator index; 35. Crime attractor index; 36. Crime enabler index; 37. Overall crime facilitator score; 38. Street segment length"
df_clean$Env_count[50] <- 38L

# Study 51 (Row 51)
df_clean$Dist_var[51] <- "1. Distance to home; 2. Distance to gang territory centroid"
df_clean$Dist_count[51] <- 2L
df_clean$Demo_var[51] <- "1. Percent Black; 2. Percent Latino; 3. Residential instability"
df_clean$Demo_count[51] <- 3L
df_clean$Env_var[51] <- "1. Concentrated disadvantage; 2. Immigrant concentration; 3. Bars; 4. Retail stores; 5. Public transit stops"
df_clean$Env_count[51] <- 5L
df_clean$Other_var[51] <- "1. Racial composition interaction (offender race × neighborhood composition)"
df_clean$Other_count[51] <- 1L

# Save cleaned dataset for manual review and categorization
custom_save(df_clean, output_folder, "cleaned_for_categorization", openxlsx::write.xlsx, file_extension = ".xlsx")
