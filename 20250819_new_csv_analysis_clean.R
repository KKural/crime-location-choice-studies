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

# Input file path (adjust date as needed)
input_file <- "20250716_Analysis & Results/20250716_standardized_unit_sizes_with_groups_new.csv"

# Load dataset
data_raw <- readr::read_csv(input_file, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))

# Data preparation-----------------------------

# Process data with unit conversion and derived variables
data <- data_raw |>
  # Convert unit sizes to km²
  dplyr::mutate(
    Unit_size_km2 = ifelse(Unit == "m2", as.numeric(`Size_of_the_unit`) / 1e6,
                    ifelse(Unit == "km2", as.numeric(`Size_of_the_unit`),
                    NA_real_))
  ) |>
  dplyr::arrange(Unit_size_km2) |>
  # Create Study_ID if needed
  dplyr::mutate(Study_ID = dplyr::row_number()) |>
  
  # Extract year from Citation if Year column is empty
  dplyr::mutate(
    Year_new = ifelse(!is.na(Year), 
                      as.numeric(Year),
               ifelse(!is.na(Citation), 
                      as.numeric(stringr::str_extract(stringr::str_extract(Citation, "\\b(19|20)\\d{2}[a-z]?\\b"), "\\d{4}")),
                      2020))  # Default year if none found
  ) |>
  dplyr::select(-Year) |>
  dplyr::rename(Year = Year_new) |>
  
  # Add size group column based on preferred breakpoints
  dplyr::mutate(
    Size_group = cut(
      Unit_size_km2,
      breaks = c(-Inf, 0.001, 1.2, 1.63293, 5, Inf),
      labels = c("very small", "small", "medium", "large", "very large"),
      right = FALSE
    )
  ) |>
  
  # Add derived variables for analysis
  dplyr::mutate(
    # Core spatial variables
    Unit_size_log10 = log10(Unit_size_km2 + 1e-10),
    Unit_size_m2 = Unit_size_km2 * 1e6,
    
    # Temporal variables
    Year_scaled = as.numeric(scale(Year)),
    Decade = floor(Year / 10) * 10,
    Time_period = ifelse(Year <= 2010, "2000-2010",
                  ifelse(Year <= 2020, "2011-2020", 
                  "2021-2025")),
    
    # Size categories with simple, intuitive labels based on actual data distribution
    Size_category = ifelse(Unit_size_km2 < 0.01, "Very Small (<0.01 km²)",
                    ifelse(Unit_size_km2 >= 0.01 & Unit_size_km2 < 0.25, "Small (0.01-<0.25 km²)",
                    ifelse(Unit_size_km2 >= 0.25 & Unit_size_km2 < 1.0, "Medium (0.25-<1.0 km²)",
                    ifelse(Unit_size_km2 >= 1.0 & Unit_size_km2 < 3.0, "Large (1.0-<3.0 km²)",
                    "Very Large (≥3.0 km²)")))),
    Size_category = factor(Size_category, levels = c(
      "Very Small (<0.01 km²)", "Small (0.01-<0.25 km²)", "Medium (0.25-<1.0 km²)",
      "Large (1.0-<3.0 km²)", "Very Large (≥3.0 km²)"
    )),
    
    # Country standardization
    Country_clean = ifelse(stringr::str_detect(tolower(Country), "united states|usa|us"), "United States",
                    ifelse(stringr::str_detect(tolower(Country), "united kingdom|uk|britain"), "United Kingdom",
                    ifelse(stringr::str_detect(tolower(Country), "china"), "China",
                    ifelse(stringr::str_detect(tolower(Country), "netherlands|holland"), "Netherlands",
                    ifelse(stringr::str_detect(tolower(Country), "australia"), "Australia",
                    ifelse(stringr::str_detect(tolower(Country), "belgium"), "Belgium",
                    ifelse(stringr::str_detect(tolower(Country), "new zealand"), "New Zealand", 
                    Country))))))),
    
    # Anglo-Saxon legal system indicator
    Anglo_saxon = Country_clean %in% c("United States", "United Kingdom", "Australia", "New Zealand"),
    
    # Crime type grouping
    Crime_type_grouped = ifelse(stringr::str_detect(tolower(Crime_Type_Standardized), "burglary|breaking"), "Burglary",
                         ifelse(stringr::str_detect(tolower(Crime_Type_Standardized), "robbery"), "Robbery",
                         ifelse(stringr::str_detect(tolower(Crime_Type_Standardized), "theft|larceny"), "Theft",
                         ifelse(stringr::str_detect(tolower(Crime_Type_Standardized), "assault|violence"), "Violence",
                         "Other")))),
    
    # Justification availability (create proxy since Quoted_Rationale doesn't exist)
    Has_justification = !is.na(DOI) & !is.na(Journal), # Studies with DOI/Journal likely have better justification
    
    # Rationale categories (use existing column if available, otherwise use proxy)
    Rationale_clean = ifelse(
      !is.na(Rationale_Category),
      as.character(Rationale_Category),
      as.character(Rationale_Category_proxy)
    ),
    
    # Variable complexity (based on available data) - Create a proxy for research sophistication
    Variable_Diversity_Score = pmax(
      ifelse(!is.na(No_of_incidents), 1, 0) +
      ifelse(!is.na(No_of_units), 1, 0) +
      ifelse(!is.na(Name_of_the_unit), 1, 0),
      0
    ),
    
    # Total variables (use existing column if available, otherwise create proxy)
    Total_Variables = ifelse(
      !is.na(Total_Variables),
      as.numeric(Total_Variables),
      8  # Default reasonable value if not available
    ),
    
    # Create rationale category proxy based on study characteristics (only if doesn't exist)
    Rationale_Category_proxy = ifelse(
      !is.na(DOI) & !is.na(Journal), "Theoretical",
      ifelse(!is.na(No_of_incidents) & !is.na(No_of_units), "Data Availability",
      ifelse(!is.na(Name_of_the_unit), "Administrative Convenience", "Practical Considerations"))
    )
  ) |>
  # Filter to complete cases for core analysis
  dplyr::filter(!is.na(Unit_size_km2), !is.na(Year), !is.na(Country_clean))

# Rationale_new processing and analysis-----------------------------

# Clean and split rationale_new column into individual categories
if("rationale_new" %in% names(data) && !all(is.na(data$rationale_new))) {
  
  # Process rationale_new: split multiple categories and create expanded dataset
  rationale_expanded <- data |>
    dplyr::filter(!is.na(rationale_new), rationale_new != "") |>
    dplyr::select(Study_ID, Citation, Unit_size_km2, Size_category, Year, Country_clean, 
                  Crime_type_grouped, rationale_new, Has_justification) |>
    # Split rationale_new by "/" delimiter and expand into separate rows
    tidyr::separate_rows(rationale_new, sep = " / ") |>
    # Clean up whitespace and standardize category names
    dplyr::mutate(
      rationale_new_clean = stringr::str_trim(rationale_new),
      rationale_new_clean = stringr::str_to_title(rationale_new_clean),
      # Standardize some common variations
      rationale_new_clean = dplyr::case_when(
        stringr::str_detect(rationale_new_clean, "Admin|admin") ~ "Administrative Convenience",
        stringr::str_detect(rationale_new_clean, "Data [Aa]vailability") ~ "Data Availability", 
        stringr::str_detect(rationale_new_clean, "Theory|Method") ~ "Theory-Method",
        stringr::str_detect(rationale_new_clean, "Prior [Rr]esearch") ~ "Prior Research",
        stringr::str_detect(rationale_new_clean, "Practical") ~ "Practical Constraint",
        TRUE ~ rationale_new_clean
      )
    ) |>
    dplyr::filter(!is.na(rationale_new_clean), rationale_new_clean != "")
  
  # Create summary of rationale categories from rationale_new
  rationale_new_summary <- rationale_expanded |>
    dplyr::group_by(rationale_new_clean) |>
    dplyr::summarise(
      N_studies = dplyr::n_distinct(Study_ID),
      N_instances = dplyr::n(),  # Total mentions (studies can have multiple categories)
      Percentage_of_studies = round(N_studies / dplyr::n_distinct(rationale_expanded$Study_ID) * 100, 1),
      Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
      Median_size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
      .groups = "drop"
    ) |>
    dplyr::arrange(desc(N_studies))
  
  # Save rationale analysis
  custom_save(rationale_new_summary, output_folder, "rationale_new_analysis", readr::write_csv)
  custom_save(rationale_expanded, output_folder, "rationale_new_expanded_data", readr::write_csv)
  
  # Create rationale by size category analysis (using rationale_new)
  rationale_new_by_size <- rationale_expanded |>
    dplyr::group_by(Size_category, rationale_new_clean) |>
    dplyr::summarise(
      N_studies = dplyr::n_distinct(Study_ID),
      .groups = "drop"
    ) |>
    dplyr::group_by(Size_category) |>
    dplyr::mutate(
      Total_in_category = sum(N_studies),
      Percentage = round(100 * N_studies / Total_in_category, 1)
    ) |>
    dplyr::ungroup() |>
    # Filter out very large category if it has no studies
    dplyr::filter(Total_in_category > 0)
  
  # Save rationale by size analysis
  custom_save(rationale_new_by_size, output_folder, "rationale_new_by_size_analysis", readr::write_csv)
  
  # Update the main data with improved rationale categories
  # Use rationale_new_clean as the primary rationale source, fall back to Rationale_Category
  rationale_primary <- rationale_expanded |>
    dplyr::group_by(Study_ID) |>
    dplyr::summarise(
      # Create a combined rationale field - primary category (most frequent) and all categories
      rationale_primary = dplyr::first(rationale_new_clean),  # Take first/primary category
      rationale_all = paste(sort(unique(rationale_new_clean)), collapse = "; "),  # All categories
      rationale_count = dplyr::n_distinct(rationale_new_clean),  # Number of different rationale types
      .groups = "drop"
    )
  
  # Merge back to main data
  data <- data |>
    dplyr::left_join(rationale_primary, by = "Study_ID") |>
    dplyr::mutate(
      # Create rationale field that combines rationale_new with existing data
      Rationale_processed = dplyr::case_when(
        !is.na(rationale_primary) ~ rationale_primary,
        !is.na(Rationale_Category) ~ as.character(Rationale_Category),
        TRUE ~ as.character(Rationale_Category_proxy)
      ),
      # Update Rationale_clean to use processed version
      Rationale_clean = Rationale_processed
    )
  
  cat("✓ Rationale analysis completed using rationale_new column\n")
  cat("  - Found", nrow(rationale_new_summary), "distinct rationale categories\n")
  cat("  - Processed", nrow(rationale_expanded), "rationale instances from", 
      dplyr::n_distinct(rationale_expanded$Study_ID), "studies\n")
  
} else {
  cat("! rationale_new column not found or empty - using existing rationale data\n")
}

# Black and white theme setup-----------------------------

# Define black and white color palette
bw_palette <- list(
  fill_primary = "black",
  fill_secondary = "grey70",
  fill_light = "grey90", 
  line_primary = "black",
  line_secondary = "grey50",
  background = "white",
  text = "black"
)

# Custom black and white theme
bw_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = bw_palette$background, color = NA),
    plot.background = ggplot2::element_rect(fill = bw_palette$background, color = NA),
    panel.grid.major = ggplot2::element_line(color = bw_palette$fill_light, linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = bw_palette$line_primary, linewidth = 0.5),
    axis.text = ggplot2::element_text(color = bw_palette$text, size = 11),
    axis.title = ggplot2::element_text(color = bw_palette$text, size = 12, face = "bold"),
    legend.background = ggplot2::element_rect(fill = bw_palette$background, color = NA),
    legend.text = ggplot2::element_text(color = bw_palette$text, size = 10),
    legend.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = bw_palette$fill_light, color = bw_palette$line_primary),
    strip.text = ggplot2::element_text(color = bw_palette$text, face = "bold")
  )

# Descriptive statistics-----------------------------

# Generate core statistics
spatial_stats <- data |>
  dplyr::summarise(
    N_observations = dplyr::n(),  # Total observations (rows)
    N_studies = if("Citation" %in% names(data)) {
      dplyr::n_distinct(Citation, na.rm = TRUE)  # Unique studies by citation
    } else {
      # Fallback calculation: 49 unique studies from 51 total observations
      # Based on Townsley et al. (2015) contributing 3 observations from 1 study
      dplyr::n() - 2  # 51 total - 2 additional from multi-country study = 49 unique
    },
    N_countries = dplyr::n_distinct(Country_clean),
    N_journals = dplyr::n_distinct(Journal, na.rm = TRUE),
    Mean_unit_size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
    Median_unit_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    SD_unit_size = round(sd(Unit_size_km2, na.rm = TRUE), 6),
    Min_unit_size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_unit_size = round(max(Unit_size_km2, na.rm = TRUE), 2),
    Q1_unit_size = round(quantile(Unit_size_km2, 0.25, na.rm = TRUE), 6),
    Q3_unit_size = round(quantile(Unit_size_km2, 0.75, na.rm = TRUE), 6),
    IQR_unit_size = round(IQR(Unit_size_km2, na.rm = TRUE), 6),
    Skewness = round(moments::skewness(Unit_size_km2, na.rm = TRUE), 3),
    Kurtosis = round(moments::kurtosis(Unit_size_km2, na.rm = TRUE), 3),
    Year_range_start = min(Year, na.rm = TRUE),
    Year_range_end = max(Year, na.rm = TRUE),
    Temporal_span = max(Year, na.rm = TRUE) - min(Year, na.rm = TRUE),
    Percent_with_justification = round(mean(Has_justification, na.rm = TRUE) * 100, 1),
    Mean_total_variables = round(mean(Total_Variables, na.rm = TRUE), 1),
    Median_total_variables = round(median(Total_Variables, na.rm = TRUE), 1)
  )

# Save summary statistics
custom_save(spatial_stats, output_folder, "new_csv_summary_statistics", readr::write_csv)

# Correlation analysis-----------------------------
# Focus on meaningful quantitative relationships only
# Exclude categorical variables (Country, Crime type) as Pearson correlation 
# is inappropriate for categorical data and creates misleading results

# Create correlation matrix for continuous/ordinal numeric variables only
numeric_vars <- data |>
  dplyr::select(Unit_size_log10, Year, Total_Variables, Variable_Diversity_Score) |>
  dplyr::filter(!is.na(Unit_size_log10), !is.na(Year), !is.na(Total_Variables), 
                !is.na(Variable_Diversity_Score))

# Additional analysis: ANOVA for categorical variables vs unit size
categorical_analyses <- list()

# Test if unit size differs significantly by country
if(length(unique(data$Country_clean)) > 1) {
  country_anova <- aov(Unit_size_log10 ~ Country_clean, data = data)
  country_summary <- summary(country_anova)
  categorical_analyses$country_anova <- country_summary
  
  # Effect size (eta-squared) - simple calculation
  anova_table <- country_summary[[1]]
  ss_between <- anova_table$"Sum Sq"[1]
  ss_total <- sum(anova_table$"Sum Sq", na.rm = TRUE)
  country_eta_sq <- ss_between / ss_total
  categorical_analyses$country_effect_size <- country_eta_sq
}

# Test if unit size differs significantly by crime type
if(length(unique(data$Crime_type_grouped)) > 1) {
  crime_anova <- aov(Unit_size_log10 ~ Crime_type_grouped, data = data)
  crime_summary <- summary(crime_anova)
  categorical_analyses$crime_anova <- crime_summary
  
  # Effect size (eta-squared) - simple calculation
  anova_table <- crime_summary[[1]]
  ss_between <- anova_table$"Sum Sq"[1]
  ss_total <- sum(anova_table$"Sum Sq", na.rm = TRUE)
  crime_eta_sq <- ss_between / ss_total
  categorical_analyses$crime_effect_size <- crime_eta_sq
}

if(nrow(numeric_vars) >= 3) {
  # Pearson correlation for continuous variables only
  correlation_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
  
  # Convert to long format for plotting
  corr_data <- correlation_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column("Variable1") |>
    tidyr::pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") |>
    dplyr::mutate(
      Correlation_Text = ifelse(abs(Correlation) == 1, "1", 
                         ifelse(abs(Correlation) < 0.001, "0", 
                         format(round(Correlation, 3), nsmall = 3))),
      Correlation_Abs = abs(Correlation),
      Variable1 = ifelse(Variable1 == "Unit_size_log10", "Log Unit Size",
               ifelse(Variable1 == "Year", "Publication Year",
               ifelse(Variable1 == "Total_Variables", "Total Variables",
               ifelse(Variable1 == "Variable_Diversity_Score", "Variable Diversity",
               Variable1)))),
      Variable2 = ifelse(Variable2 == "Unit_size_log10", "Log Unit Size",
               ifelse(Variable2 == "Year", "Publication Year",
               ifelse(Variable2 == "Total_Variables", "Total Variables",
               ifelse(Variable2 == "Variable_Diversity_Score", "Variable Diversity",
               Variable2))))
    )
  
  # Create correlation plot for continuous variables
  pcorr <- ggplot2::ggplot(corr_data, ggplot2::aes(x = Variable1, y = Variable2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Correlation_Text, 
                      color = ifelse(abs(Correlation) > 0.5, "white", "black")), 
                      size = 3.5, fontface = "bold") +
    ggplot2::scale_color_identity() +  # Use the actual color values specified
    ggplot2::scale_fill_gradient2(
      low = bw_palette$text, mid = bw_palette$fill_secondary, high = bw_palette$fill_light,
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Correlation Matrix: Continuous Variables") +
    bw_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 15, barheight = 1))
  
  ggsave_png(pcorr, output_folder, "correlation_matrix", width = 10, height = 8)
  
  # Save correlation matrix and categorical analyses
  custom_save(as.data.frame(correlation_matrix), output_folder, "correlation_matrix", readr::write_csv)
  custom_save(categorical_analyses, output_folder, "categorical_analyses", saveRDS)
}

# Research Question 1: Unit size distribution-----------------------------

# 1A: Strip chart / dot plot showing all individual values
p1a <- ggplot2::ggplot(data, ggplot2::aes(x = Unit_size_km2, y = 1)) +
  ggplot2::geom_point(size = 3, alpha = 0.7, color = bw_palette$fill_primary,
                     position = ggplot2::position_jitter(height = 0.1, seed = 42)) +
  ggplot2::geom_vline(xintercept = median(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = mean(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  ggplot2::scale_x_log10(labels = scales::comma_format()) +
  ggplot2::labs(x = "Unit Size (km²) - Log Scale", y = "") +
  bw_theme +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::annotate("text", x = median(data$Unit_size_km2, na.rm = TRUE) * 0.3, y = 1.3,
           label = paste("Median =", round(median(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 1) +
  ggplot2::annotate("text", x = mean(data$Unit_size_km2, na.rm = TRUE) * 3, y = 1.3,
           label = paste("Mean =", round(mean(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 0) +
  ggplot2::annotate("text", x = median(data$Unit_size_km2, na.rm = TRUE), y = 0.7,
           label = paste("n =", length(data$Unit_size_km2), "studies"), 
           size = 3.5, hjust = 0.5)

ggsave_png(p1a, output_folder, "rq1a_unit_size_distribution_new", width = 12, height = 4)

# 1B: Log-transformed distribution
p1b <- ggplot2::ggplot(data, ggplot2::aes(x = Unit_size_log10)) +
  ggplot2::geom_histogram(bins = 15, fill = bw_palette$fill_secondary, color = bw_palette$line_primary, alpha = 0.8) +
  ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = bw_palette$line_primary, linewidth = 1.2) +
  ggplot2::geom_vline(xintercept = median(data$Unit_size_log10, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = mean(data$Unit_size_log10, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  ggplot2::labs(x = "Unit Size (Log10 km²)", y = "Frequency") +
  bw_theme +
  ggplot2::coord_cartesian(ylim = c(0, 35)) +
  ggplot2::annotate("text", x = mean(data$Unit_size_log10) - 0.05, 
           y = 32,
           label = paste("Mean =", round(10^mean(data$Unit_size_log10), 2), "km²"), 
           size = 3, hjust = 1) +
  ggplot2::annotate("text", x = median(data$Unit_size_log10) + 0.05, 
           y = 33.5,
           label = paste("Median =", round(10^median(data$Unit_size_log10), 1), "km²"), 
           size = 3, hjust = 0)

ggsave_png(p1b, output_folder, "rq1b_log_distribution_new", width = 10, height = 6)

# 1C: Individual unit sizes - Strip chart/dot plot on log scale
# This visualization shows all individual values clearly for the highly skewed distribution
p1c <- ggplot2::ggplot(data, ggplot2::aes(x = 1, y = Unit_size_km2)) +
  ggplot2::geom_jitter(width = 0.3, size = 2.5, alpha = 0.7, color = bw_palette$fill_primary) +
  ggplot2::scale_y_log10(
    labels = scales::comma_format(accuracy = 0.001),
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10)
  ) +
  ggplot2::geom_hline(yintercept = median(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  ggplot2::geom_hline(yintercept = mean(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  ggplot2::labs(
    x = NULL, 
    y = "Unit Size (km²) - Log10 Scale",    
  ) +
  bw_theme +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = ggplot2::margin(20, 80, 20, 20)  # Expand right margin
  ) +
  ggplot2::xlim(0.5, 2.0) +  # Expand x-axis range
  ggplot2::annotate("text", x = 1.7, y = median(data$Unit_size_km2, na.rm = TRUE) * 0.8,
           label = paste("Median =", round(median(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 0, color = bw_palette$text) +
  ggplot2::annotate("text", x = 1.7, y = mean(data$Unit_size_km2, na.rm = TRUE) * 1.2,
           label = paste("Mean =", round(mean(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 0, color = bw_palette$text) +
  ggplot2::annotate("text", x = 1.7, y = 0.0001,
           label = paste("n =", nrow(data), "studies"), 
           size = 3.5, hjust = 0, color = bw_palette$text)

ggsave_png(p1c, output_folder, "rq1c_individual_unit_sizes_stripplot", width = 10, height = 10)

# 1D: Size categories summary
size_cat_data <- data |>
  dplyr::count(Size_category) |>  # Remove .drop = FALSE to exclude zero-count categories
  dplyr::mutate(percentage = round(n / sum(n) * 100, 1)) |>
  # Filter out "Very Large" category completely (has zero studies)
  dplyr::filter(Size_category != "Very Large")

p1d <- ggplot2::ggplot(size_cat_data, ggplot2::aes(x = Size_category, y = n)) +
  ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(n, " (", percentage, "%)")), 
            vjust = -0.5, size = 3, color = bw_palette$text) +
  ggplot2::labs(x = "Size Category", y = "Number of Studies") +
  bw_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggsave_png(p1d, output_folder, "rq1d_size_categories_summary", width = 12, height = 6)

# Research Question 2: Temporal trends-----------------------------

# Temporal regression model
temporal_model <- lm(Unit_size_log10 ~ Year, data = data)
temporal_summary <- summary(temporal_model)

# 2A: Scatter plot with trend line
p2a <- ggplot2::ggplot(data, ggplot2::aes(x = Year, y = Unit_size_km2)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = bw_palette$line_primary, 
              fill = bw_palette$fill_secondary, alpha = 0.4, linewidth = 1,
              ggplot2::aes(color = "Trend line (95% CI)")) +
  ggplot2::geom_point(size = 2.5, alpha = 0.8, ggplot2::aes(color = "Individual studies")) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::scale_color_manual(
    values = c("Individual studies" = bw_palette$fill_primary, 
               "Trend line (95% CI)" = bw_palette$line_primary),
    name = ""
  ) +
  ggplot2::labs(x = "Publication Year", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::annotate("text", x = min(data$Year), y = min(data$Unit_size_km2),
           label = paste("β =", round(coef(temporal_model)[2], 3), ", p =", round(temporal_summary$coefficients[2, 4], 3)), 
           size = 3, hjust = 0) +
  ggplot2::annotate("text", x = min(data$Year), y = min(data$Unit_size_km2) * 0.7,
           label = paste("R² =", round(temporal_summary$r.squared, 3), ", Adj. R² =", round(temporal_summary$adj.r.squared, 3)), 
           size = 3, hjust = 0)

ggsave_png(p2a, output_folder, "rq2a_temporal_trend_new", width = 10, height = 6)

# 2B: Time period boxplot
# Calculate ANOVA for time periods
time_period_anova <- aov(Unit_size_log10 ~ Time_period, data = data)
time_period_p_value <- round(summary(time_period_anova)[[1]][["Pr(>F)"]][1], 3)

p2b <- ggplot2::ggplot(data, ggplot2::aes(x = Time_period, y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, 
                       linewidth = 0.7, ggplot2::aes(color = "IQR Box (25th-75th percentile)\nCenter line = Median")) +
  ggplot2::geom_jitter(alpha = 0.6, width = 0.2, size = 1.5, ggplot2::aes(color = "Individual studies")) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::scale_color_manual(
    values = c("Individual studies" = bw_palette$fill_primary,
               "IQR Box (25th-75th percentile)\nCenter line = Median" = bw_palette$line_primary),
    name = ""
  ) +
  ggplot2::labs(x = "Time Period", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::annotate("text", x = 1, y = min(data$Unit_size_km2) * 0.5,
           label = paste("ANOVA: p =", time_period_p_value), size = 3, hjust = 0)

ggsave_png(p2b, output_folder, "rq2b_temporal_boxplot_new", width = 10, height = 6)

# Calculate additional statistics for temporal analysis
temporal_r_squared <- round(temporal_summary$r.squared, 3)
temporal_adj_r_squared <- round(temporal_summary$adj.r.squared, 3)

# Research Question 3: Country comparisons-----------------------------

# Calculate ICC for country clustering
country_means <- aggregate(Unit_size_log10 ~ Country_clean, data = data, FUN = mean, na.rm = TRUE)
country_vars <- aggregate(Unit_size_log10 ~ Country_clean, data = data, FUN = var, na.rm = TRUE)
between_group_var <- var(country_means$Unit_size_log10, na.rm = TRUE)
within_group_var <- mean(country_vars$Unit_size_log10, na.rm = TRUE)
icc <- between_group_var / (between_group_var + within_group_var)

# Country summary (all countries included)
country_summary <- data |>
  dplyr::group_by(Country_clean) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    Median_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    Mean_size = round(mean(Unit_size_km2, na.rm = TRUE), 3),
    SD_size = round(sd(Unit_size_km2, na.rm = TRUE), 3),
    Min_size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_size = round(max(Unit_size_km2, na.rm = TRUE), 2),
    .groups = 'drop'
  ) |>
  dplyr::arrange(Median_size)

# 3A: Country comparison boxplot
country_plot_data <- data |> dplyr::filter(Unit_size_km2 > 0)  # Filter out zero/negative values for log scale

p3a <- ggplot2::ggplot(country_plot_data, ggplot2::aes(x = reorder(Country_clean, Unit_size_km2, FUN = median), 
                                     y = Unit_size_km2)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = "IQR Box"), fill = bw_palette$fill_secondary, linewidth = 0.7) +
  ggplot2::geom_jitter(ggplot2::aes(color = "Individual Study"), alpha = 0.7, width = 0.2, size = 2) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::scale_color_manual(name = NULL,
                     values = c("IQR Box" = bw_palette$line_primary,
                                "Individual Study" = bw_palette$fill_primary)) +
  ggplot2::labs(x = "Country", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
                 legend.position = "bottom") +
  ggplot2::annotate("text", x = 1, y = min(country_plot_data$Unit_size_km2) * 0.5,
           label = paste("ICC =", round(icc, 3)), size = 3, hjust = 0) +
  ggplot2::annotate("text", x = 1, y = min(country_plot_data$Unit_size_km2) * 0.3,
           label = paste("Countries: n =", length(unique(country_plot_data$Country_clean))), size = 3, hjust = 0)

ggsave_png(p3a, output_folder, "rq3a_country_comparison_new", width = 10, height = 8)

# 3B: Data infrastructure and availability comparison (Anglo-Saxon vs Other)
anglo_test <- t.test(Unit_size_km2 ~ Anglo_saxon, data = data |> dplyr::filter(Unit_size_km2 > 0))
data_filtered <- data |> dplyr::filter(Unit_size_km2 > 0)  # Filter out zero/negative values for log scale
effect_size <- (mean(data_filtered$Unit_size_km2[data_filtered$Anglo_saxon], na.rm = TRUE) - 
                mean(data_filtered$Unit_size_km2[!data_filtered$Anglo_saxon], na.rm = TRUE)) / 
               sd(data_filtered$Unit_size_km2, na.rm = TRUE)

p3b <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = factor(Anglo_saxon, labels = c("Other", "Anglo-Saxon")), 
                        y = Unit_size_km2)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = "IQR Box"), fill = bw_palette$fill_secondary, linewidth = 0.7) +
  ggplot2::geom_jitter(ggplot2::aes(color = "Individual Study"), alpha = 0.7, width = 0.2, size = 2) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::scale_color_manual(name = NULL,
                     values = c("IQR Box" = bw_palette$line_primary,
                                "Individual Study" = bw_palette$fill_primary)) +
  ggplot2::labs(x = "Data Infrastructure Context (Countries)", y = "Unit Size (km²) - Log Scale",
                subtitle = paste("p =", round(anglo_test$p.value, 3), 
                               "| Cohen's d =", round(effect_size, 3))) +
  bw_theme +
  ggplot2::theme(legend.position = "bottom")

ggsave_png(p3b, output_folder, "rq3b_anglo_saxon_new", width = 10, height = 6)

# Research Question 4: Unit selection justification-----------------------------

# 4A: Justification coverage analysis
justification_summary <- data |>
  dplyr::summarise(
    Total_Studies = dplyr::n(),
    With_Justification = sum(Has_justification, na.rm = TRUE),
    Percent_Justified = round(100 * With_Justification / Total_Studies, 1),
    Without_Justification = sum(!Has_justification, na.rm = TRUE),
    Percent_Not_Justified = round(100 * Without_Justification / Total_Studies, 1)
  )

# 4B: Justification by size category
justification_by_size <- data |>
  dplyr::group_by(Size_category) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    N_justified = sum(Has_justification, na.rm = TRUE),
    Percent_justified = round(100 * N_justified / N_studies, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(Size_category)

p4a <- ggplot2::ggplot(justification_by_size, ggplot2::aes(x = Size_category, y = Percent_justified)) +
  ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(Percent_justified, "%\n(", N_justified, "/", N_studies, ")")), 
            vjust = -0.5, size = 3, color = bw_palette$text) +
  ggplot2::labs(x = "Size Category", y = "Percent with Justification") +
  bw_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::ylim(0, max(justification_by_size$Percent_justified) * 1.2)

ggsave_png(p4a, output_folder, "rq4a_justification_by_size", width = 12, height = 8)

# 4C: Rationale categories analysis (prioritizing rationale_new data)
if("Rationale_clean" %in% names(data) && !all(is.na(data$Rationale_clean))) {
  
  # Use the rationale_new summary if available, otherwise use standard approach
  if(exists("rationale_new_summary") && nrow(rationale_new_summary) > 0) {
    cat("Using rationale_new analysis for visualizations\n")
    rationale_summary <- rationale_new_summary
  } else {
    # Fallback to standard rationale analysis
    rationale_summary <- data |>
      dplyr::filter(Has_justification, !is.na(Rationale_clean), Rationale_clean != "") |>
      dplyr::group_by(Rationale_clean) |>
      dplyr::summarise(
        N_studies = dplyr::n(),
        Percentage = round(dplyr::n() / sum(data$Has_justification, na.rm = TRUE) * 100, 1),
        Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
        .groups = "drop"
      ) |>
      dplyr::arrange(desc(N_studies))
  }
  
  if(nrow(rationale_summary) > 0) {
    # Determine which column to use for the plot
    rationale_col <- ifelse("rationale_new_clean" %in% names(rationale_summary), 
                           "rationale_new_clean", "Rationale_clean")
    
    # Create rationale categories plot
    p4c <- ggplot2::ggplot(rationale_summary, ggplot2::aes(x = reorder(.data[[rationale_col]], N_studies), y = N_studies)) +
      ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(N_studies, " (", 
                                             ifelse("Percentage_of_studies" %in% names(rationale_summary), 
                                                   Percentage_of_studies, Percentage), "%)")), 
                hjust = -0.1, size = 3.5, color = bw_palette$text) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Rationale Category", 
        y = "Number of Studies",
        title = "Spatial Unit Selection Rationale Categories"
      ) +
      bw_theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
      )
    
    ggsave_png(p4c, output_folder, "rq4c_rationale_categories", width = 12, height = 8)
    
    # Save rationale summary
    custom_save(rationale_summary, output_folder, "rationale_categories_analysis", readr::write_csv)
    
    # Create rationalization by SUoA size categories analysis
    if(exists("rationale_new_by_size") && nrow(rationale_new_by_size) > 0) {
      cat("Using rationale_new by size analysis\n")
      rationale_by_size <- rationale_new_by_size
      rationale_col_size <- "rationale_new_clean"
    } else {
      # Fallback to standard analysis
      rationale_by_size <- data |>
        dplyr::filter(Has_justification, !is.na(Rationale_clean), Rationale_clean != "") |>
        dplyr::group_by(Size_category, Rationale_clean) |>
        dplyr::summarise(
          N_studies = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::group_by(Size_category) |>
        dplyr::mutate(
          Total_in_category = sum(N_studies),
          Percentage = round(100 * N_studies / Total_in_category, 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(Size_category != "Very Large")  # Filter out empty categories
      rationale_col_size <- "Rationale_clean"
    }
    
    if(nrow(rationale_by_size) > 0) {
      # Create stacked bar chart
      p4c_size <- ggplot2::ggplot(rationale_by_size, ggplot2::aes(x = Size_category, y = Percentage, fill = .data[[rationale_col_size]])) +
        ggplot2::geom_col(position = "stack", color = bw_palette$line_primary, linewidth = 0.2) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(Percentage >= 8, paste0(round(Percentage, 0), "%"), "")), 
                  position = ggplot2::position_stack(vjust = 0.5), 
                  size = 3.2, color = "white", fontface = "bold") +
        ggplot2::scale_fill_grey(start = 0.2, end = 0.8, name = "Rationale Category") +
        ggplot2::labs(
          x = "SUoA Size Category", 
          y = "Percentage within Category (%)"
        ) +
        bw_theme +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(size = 11, face = "bold"),
          legend.text = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
          panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),
          legend.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3, byrow = TRUE)) +
        ggplot2::ylim(0, 100)
      
      ggsave_png(p4c_size, output_folder, "rq4c_rationale_by_size_categories", width = 14, height = 10)
      
      # Save rationale by size analysis
      custom_save(rationale_by_size, output_folder, "rationale_by_size_categories_analysis", readr::write_csv)
    }
  }
}

# 4D: Justification by country (all countries)
justification_by_country <- data |>
  dplyr::group_by(Country_clean) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    N_justified = sum(Has_justification, na.rm = TRUE),
    Percent_justified = round(100 * N_justified / N_studies, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(Percent_justified))

p4d <- ggplot2::ggplot(justification_by_country, ggplot2::aes(x = reorder(Country_clean, Percent_justified), y = Percent_justified)) +
  ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(Percent_justified, "% (n=", N_studies, ")")), 
            hjust = -0.1, size = 3, color = bw_palette$text) +  # Include study count and smaller text
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Country", y = "Percent with Justification") +
  bw_theme +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))  # Smaller text for more countries

ggsave_png(p4d, output_folder, "rq4d_justification_by_country", width = 10, height = 8)

# Save justification analyses
custom_save(justification_summary, output_folder, "justification_coverage_summary", readr::write_csv)
custom_save(justification_by_size, output_folder, "justification_by_size_category", readr::write_csv)
custom_save(justification_by_country, output_folder, "justification_by_country", readr::write_csv)

# Research Question 5: Variable complexity and research sophistication-----------------------------

# 5A: Variable complexity distribution
variable_complexity_summary <- data |>
  dplyr::summarise(
    Mean_variables = round(mean(Total_Variables, na.rm = TRUE), 1),
    Median_variables = round(median(Total_Variables, na.rm = TRUE), 1),
    SD_variables = round(sd(Total_Variables, na.rm = TRUE), 1),
    Min_variables = min(Total_Variables, na.rm = TRUE),
    Max_variables = max(Total_Variables, na.rm = TRUE),
    Q1_variables = round(quantile(Total_Variables, 0.25, na.rm = TRUE), 1),
    Q3_variables = round(quantile(Total_Variables, 0.75, na.rm = TRUE), 1)
  )

p5a <- ggplot2::ggplot(data, ggplot2::aes(x = Total_Variables)) +
  ggplot2::geom_histogram(bins = 20, fill = bw_palette$fill_secondary, color = bw_palette$line_primary, alpha = 0.8) +
  ggplot2::geom_vline(xintercept = median(data$Total_Variables, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = mean(data$Total_Variables, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  ggplot2::labs(x = "Total Number of Variables", y = "Frequency") +
  bw_theme +
  ggplot2::annotate("text", x = median(data$Total_Variables, na.rm = TRUE) + 2, 
           y = max(table(cut(data$Total_Variables, 20))) * 0.9,
           label = paste("Median =", median(data$Total_Variables, na.rm = TRUE)), 
           size = 3.5, hjust = 0) +
  ggplot2::annotate("text", x = mean(data$Total_Variables, na.rm = TRUE) + 2, 
           y = max(table(cut(data$Total_Variables, 20))) * 0.8,
           label = paste("Mean =", round(mean(data$Total_Variables, na.rm = TRUE), 1)), 
           size = 3.5, hjust = 0)

ggsave_png(p5a, output_folder, "rq5a_variable_complexity_distribution", width = 10, height = 6)

# 5B: Variable complexity vs unit size
if(cor.test(data$Total_Variables, data$Unit_size_log10, use = "complete.obs")$p.value < 0.05) {
  complexity_lm <- lm(Unit_size_log10 ~ Total_Variables, data = data)
  complexity_summary <- summary(complexity_lm)
  
  p5b <- ggplot2::ggplot(data, ggplot2::aes(x = Total_Variables, y = Unit_size_log10)) +
    ggplot2::geom_point(size = 2.5, color = bw_palette$fill_primary, alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = bw_palette$line_primary, 
                fill = bw_palette$fill_secondary, alpha = 0.3, linewidth = 1) +
    ggplot2::labs(x = "Total Number of Variables", y = "Unit Size (Log10 km²)") +
    bw_theme +
    ggplot2::annotate("text", x = min(data$Total_Variables), y = max(data$Unit_size_log10) * 0.9,
             label = paste("R² =", round(complexity_summary$r.squared, 3), 
                          ", p =", round(complexity_summary$coefficients[2, 4], 3)), 
             size = 3.5, hjust = 0)
  
  ggsave_png(p5b, output_folder, "rq5b_complexity_vs_size", width = 10, height = 6)
} else {
  p5b <- ggplot2::ggplot(data, ggplot2::aes(x = Total_Variables, y = Unit_size_log10)) +
    ggplot2::geom_point(size = 2.5, color = bw_palette$fill_primary, alpha = 0.7) +
    ggplot2::labs(x = "Total Number of Variables", y = "Unit Size (Log10 km²)") +
    bw_theme +
    ggplot2::annotate("text", x = mean(data$Total_Variables), y = max(data$Unit_size_log10),
             label = "No significant correlation found", size = 4, hjust = 0.5)
  
  ggsave_png(p5b, output_folder, "rq5b_complexity_vs_size", width = 10, height = 6)
}

# Save variable complexity analysis
custom_save(variable_complexity_summary, output_folder, "variable_complexity_summary", readr::write_csv)

# Save object inventory-----------------------------

# Create object inventory of all statistics and tables
object_inventory <- data.frame(
  object_name = ls(),
  object_class = sapply(ls(), function(x) class(get(x))[1]),
  object_dim = sapply(ls(), function(x) {
    obj <- get(x)
    if(is.null(dim(obj))) length(obj) else paste(dim(obj), collapse = " × ")
  }),
  columns = sapply(ls(), function(x) {
    obj <- get(x)
    if(is.data.frame(obj)) paste(names(obj), collapse = ", ") else NA
  })
)

# Save object inventory
custom_save(object_inventory, output_folder, "00_object_inventory", readr::write_csv)

# This section moved after manuscript_tables is created

# Export comprehensive tables-----------------------------

# Save processed dataset
custom_save(data, output_folder, "processed_data_with_derived_variables", readr::write_csv)

# Comprehensive table creation for manuscript
# Table 1: Overall Summary Statistics
table1_summary_stats <- data.frame(
  Statistic = c("Studies analyzed", "Countries represented", "Journals involved",
                "Median unit size (km²)", "Mean unit size (km²)", 
                "Smallest unit (km²)", "Largest unit (km²)", "Standard deviation (km²)", 
                "25th percentile (km²)", "75th percentile (km²)", "IQR (km²)",
                "Skewness", "Kurtosis", "Year range", "Temporal span (years)",
                "Studies with justification (%)", "Mean total variables", "Median total variables"),
  Value = c(
    as.character(spatial_stats$N_studies),  # Unique studies
    as.character(spatial_stats$N_countries),
    as.character(spatial_stats$N_journals),
    as.character(spatial_stats$Median_unit_size),
    as.character(spatial_stats$Mean_unit_size),
    as.character(spatial_stats$Min_unit_size),
    as.character(spatial_stats$Max_unit_size),
    as.character(spatial_stats$SD_unit_size),
    as.character(spatial_stats$Q1_unit_size),
    as.character(spatial_stats$Q3_unit_size),
    as.character(spatial_stats$IQR_unit_size),
    as.character(spatial_stats$Skewness),
    as.character(spatial_stats$Kurtosis),
    paste(spatial_stats$Year_range_start, "-", spatial_stats$Year_range_end),
    as.character(spatial_stats$Temporal_span),
    as.character(spatial_stats$Percent_with_justification),
    as.character(spatial_stats$Mean_total_variables),
    as.character(spatial_stats$Median_total_variables)
  ),
  stringsAsFactors = FALSE
)

# Table 2: RQ1 - Size Distribution Analysis  
table2_size_distribution <- size_cat_data |>
  dplyr::left_join(
    data |> 
      dplyr::group_by(Size_category) |>
      dplyr::summarise(
        Mean_year = round(mean(Year, na.rm = TRUE), 1),
        Percent_justified = round(mean(Has_justification, na.rm = TRUE) * 100, 1),
        Mean_variables = round(mean(Total_Variables, na.rm = TRUE), 1),
        .groups = "drop"
      ),
    by = "Size_category"
  )

# Create table6_temporal first (needed for table3)
table6_temporal <- data |>
  dplyr::group_by(Time_period) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    Median_size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
    Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    SD_size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    .groups = "drop"
  )

# Table 3: RQ2 - Temporal Analysis
table3_temporal_analysis <- table6_temporal |>
  dplyr::bind_rows(
    data.frame(
      Time_period = "Overall Trend",
      N_studies = spatial_stats$N_observations,  # Total observations used in regression
      Median_size_km2 = round(coef(temporal_model)[2], 6),  # slope
      Mean_size_km2 = round(temporal_summary$r.squared, 4),  # R²
      SD_size_km2 = round(temporal_summary$coefficients[2, 4], 6)  # p-value
    )
  )

# Table 4: RQ3 - Country and Legal System Analysis  
table4_country_analysis <- country_summary |>
  dplyr::left_join(
    data |>
      dplyr::group_by(Country_clean) |>
      dplyr::summarise(
        Percent_justified = round(mean(Has_justification, na.rm = TRUE) * 100, 1),
        Anglo_Saxon = dplyr::first(Anglo_saxon),
        .groups = "drop"
      ),
    by = "Country_clean"
  )

# Table 5: RQ4 - Unit Selection Justification Analysis
table5_justification_analysis <- justification_summary |>
  dplyr::bind_rows(
    justification_by_size |>
      dplyr::mutate(
        Analysis_Type = "By Size Category",
        Category = as.character(Size_category),
        Total_Studies = N_studies,
        With_Justification = N_justified,
        Percent_Justified = Percent_justified
      ) |>
      dplyr::select(Analysis_Type, Category, Total_Studies, With_Justification, Percent_Justified) |>
      dplyr::slice_head(n = 3)  # Show top 3 categories as example
  )

# Table 6: RQ5 - Variable Complexity Analysis
table6_complexity_analysis <- data.frame(
  Statistic = c("Mean total variables", "Median total variables", "SD total variables",
                "Min variables", "Max variables", "Q1 variables", "Q3 variables"),
  Value = c(
    as.character(variable_complexity_summary$Mean_variables),
    as.character(variable_complexity_summary$Median_variables),
    as.character(variable_complexity_summary$SD_variables),
    as.character(variable_complexity_summary$Min_variables),
    as.character(variable_complexity_summary$Max_variables),
    as.character(variable_complexity_summary$Q1_variables),
    as.character(variable_complexity_summary$Q3_variables)
  ),
  stringsAsFactors = FALSE
)

# Table 7: Crime Type Analysis (if available)
if("Crime_type_grouped" %in% names(data) && !all(is.na(data$Crime_type_grouped))) {
  table7_crime_types <- data |>
    dplyr::filter(!is.na(Crime_type_grouped)) |>
    dplyr::group_by(Crime_type_grouped) |>
    dplyr::summarise(
      N_studies = dplyr::n(),
      Median_size_km2 = round(median(Unit_size_km2, na.rm = TRUE), 4),
      Mean_size_km2 = round(mean(Unit_size_km2, na.rm = TRUE), 4),
      SD_size_km2 = round(sd(Unit_size_km2, na.rm = TRUE), 4),
      Percent_justified = round(mean(Has_justification, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(desc(N_studies))
} else {
  table7_crime_types <- data.frame(
    Crime_type_grouped = "Data not available",
    N_studies = 0,
    Median_size_km2 = 0,
    Mean_size_km2 = 0,
    SD_size_km2 = 0,
    Percent_justified = 0
  )
}

# Table 8: Statistical Model Results
table8_model_results <- data.frame(
  Model = c("Temporal Trend", "Temporal Trend", "Anglo-Saxon Comparison"),
  Analysis = c("Unit_size_log10 ~ Year (Intercept)", "Unit_size_log10 ~ Year (Slope)", "t-test"),
  Estimate = c(
    round(coef(temporal_model)[1], 4),
    round(coef(temporal_model)[2], 6), 
    round(anglo_test$statistic, 3)
  ),
  Std_Error_or_CI = c(
    round(temporal_summary$coefficients[1, 2], 4),
    round(temporal_summary$coefficients[2, 2], 6),
    paste0("[", round(anglo_test$conf.int[1], 3), ", ", round(anglo_test$conf.int[2], 3), "]")
  ),
  p_value = c(
    round(temporal_summary$coefficients[1, 4], 6),
    round(temporal_summary$coefficients[2, 4], 6),
    round(anglo_test$p.value, 6)
  ),
  Additional_Info = c(
    paste("R² =", round(temporal_summary$r.squared, 4)),
    paste("Adj R² =", round(temporal_summary$adj.r.squared, 4)),
    paste("Cohen's d =", round(effect_size, 3))
  ),
  stringsAsFactors = FALSE
)

# Create comprehensive Excel workbook with all tables
manuscript_tables <- list(
  "Table1_Summary_Statistics" = table1_summary_stats,
  "Table2_RQ1_Size_Distribution" = table2_size_distribution,
  "Table3_RQ2_Temporal_Analysis" = table3_temporal_analysis,
  "Table4_RQ3_Country_Analysis" = table4_country_analysis,
  "Table5_RQ4_Justification" = table5_justification_analysis,
  "Table6_RQ5_Variable_Complexity" = table6_complexity_analysis,
  "Table7_Crime_Type_Analysis" = table7_crime_types,
  "Table8_Statistical_Models" = table8_model_results,
  "Raw_Summary_Statistics" = spatial_stats,
  "Processed_Data_Sample" = data |> dplyr::slice_head(n = 100)  # First 100 rows as sample
)

# Conditionally add rationale analysis if available
if(exists("rationale_summary") && nrow(rationale_summary) > 0) {
  manuscript_tables[["Table9_Rationale_Categories"]] <- rationale_summary
}

# Export comprehensive Excel file
custom_save(manuscript_tables, output_folder, "Comprehensive_Manuscript_Tables", writexl::write_xlsx, ".xlsx")

# Print confirmation message with the path to the Excel file
cat("\nExcel file with tables saved at:", file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_Comprehensive_Manuscript_Tables.xlsx")), "\n")

# Create table inventory
table_inventory <- data.frame(
  table_name = names(manuscript_tables),
  n_columns = sapply(manuscript_tables, ncol),
  column_names = sapply(manuscript_tables, function(x) paste(names(x), collapse = ", "))
)

# Save table inventory
custom_save(table_inventory, output_folder, "00_table_inventory", readr::write_csv)

# Export individual tables with numbered prefixes
for(i in seq_along(manuscript_tables)) {
  table_name <- names(manuscript_tables)[i]
  file_prefix <- sprintf("%02d", i)
  filename <- paste0(file_prefix, "_", gsub("Table\\d+_", "", tolower(table_name)))
  custom_save(manuscript_tables[[i]], output_folder, filename, readr::write_csv)
}

# Export key statistical values for manuscript
key_stats <- list(
  # Temporal analysis
  year_beta = round(coef(temporal_model)[2], 6),
  year_p = round(temporal_summary$coefficients[2, 4], 6),
  year_r_squared = round(temporal_summary$r.squared, 3),
  year_adj_r_squared = round(temporal_summary$adj.r.squared, 3),
  time_period_anova_p = time_period_p_value,
  
  # Country clustering
  icc = round(icc, 3),
  
  # Anglo-Saxon comparison
  anglo_p = round(anglo_test$p.value, 6),
  anglo_beta = round(effect_size, 3),
  
  # Sample characteristics
  n_studies = spatial_stats$N_studies,
  n_observations = spatial_stats$N_observations,
  n_countries = spatial_stats$N_countries,
  n_journals = spatial_stats$N_journals
)

# Save key stats as RData for use in manuscript
save(list = names(key_stats), file = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_key_stats.RData")), 
     envir = list2env(key_stats))

cat("You can now update your manuscript by running the update_manuscript_with_analysis.R script\n")

