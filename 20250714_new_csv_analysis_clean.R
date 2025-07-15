# Comprehensive Spatial Unit Analysis - New CSV Version
# Updated to work with 20250714_standardized_unit_sizes_with_groups_new.csv
#
# INTEGRATION WITH MANUSCRIPT:
# This analysis script generates tables and figures that are automatically used by the Rmd manuscript.
# When you update this script and run it, a new Excel file will be created that contains all the
# tables. To update the manuscript with these new results:
#
# 1. First run this script: source("20250714_new_csv_analysis_clean.R")
# 2. Then run: source("update_manuscript_with_analysis.R")
#
# The manuscript will automatically use the latest analysis results without hardcoding any values.

# Custom functions-----------------------------

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
input_file <- file.path(make_folder(), "20250714_standardized_unit_sizes_with_groups_new.csv")

# Load dataset
data_raw <- readr::read_csv(input_file, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))

# Data preparation-----------------------------

# Process data with unit conversion and derived variables
data <- data_raw |>
  # Convert unit sizes to km²
  dplyr::mutate(
    Unit_size_km2 = dplyr::case_when(
      Unit == "m2" ~ as.numeric(`Size_of_the_unit`) / 1e6,
      Unit == "km2" ~ as.numeric(`Size_of_the_unit`),
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::arrange(Unit_size_km2) |>
  # Create Study_ID if needed
  dplyr::mutate(Study_ID = dplyr::row_number()) |>
  
  # Extract year from Citation if Year column doesn't exist or is empty
  dplyr::mutate(
    Year = dplyr::case_when(
      "Year" %in% names(data_raw) && !all(is.na(data_raw$Year)) ~ as.numeric(data_raw$Year),
      "Citation" %in% names(data_raw) ~ as.numeric(stringr::str_extract(stringr::str_extract(Citation, "\\b(19|20)\\d{2}[a-z]?\\b"), "\\d{4}")),
      TRUE ~ 2020  # Default year if none found
    )
  ) |>
  
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
    Time_period = dplyr::case_when(
      Year <= 2010 ~ "2000-2010",
      Year <= 2020 ~ "2011-2020", 
      TRUE ~ "2021-2025"
    ),
    
    # Size categories
    Size_category = dplyr::case_when(
      Unit_size_km2 < 0.001 ~ "Micro (<0.001 km²)",
      Unit_size_km2 < 0.01 ~ "Very Small (0.001-0.01 km²)",
      Unit_size_km2 < 0.1 ~ "Small (0.01-0.1 km²)",
      Unit_size_km2 < 1 ~ "Medium (0.1-1 km²)",
      Unit_size_km2 < 10 ~ "Large (1-10 km²)",
      TRUE ~ "Very Large (>10 km²)"
    ),
    Size_category = factor(Size_category, levels = c(
      "Micro (<0.001 km²)", "Very Small (0.001-0.01 km²)", "Small (0.01-0.1 km²)",
      "Medium (0.1-1 km²)", "Large (1-10 km²)", "Very Large (>10 km²)"
    )),
    
    # Country standardization
    Country_clean = dplyr::case_when(
      stringr::str_detect(tolower(Country), "united states|usa|us") ~ "United States",
      stringr::str_detect(tolower(Country), "united kingdom|uk|britain") ~ "United Kingdom",
      stringr::str_detect(tolower(Country), "china") ~ "China",
      stringr::str_detect(tolower(Country), "netherlands|holland") ~ "Netherlands",
      stringr::str_detect(tolower(Country), "australia") ~ "Australia",
      stringr::str_detect(tolower(Country), "belgium") ~ "Belgium",
      stringr::str_detect(tolower(Country), "new zealand") ~ "New Zealand",
      TRUE ~ Country
    ),
    
    # Anglo-Saxon legal system indicator
    Anglo_saxon = Country_clean %in% c("United States", "United Kingdom", "Australia", "New Zealand"),
    
    # Crime type grouping
    Crime_type_grouped = dplyr::case_when(
      stringr::str_detect(tolower(Crime_Type_Standardized), "burglary|breaking") ~ "Burglary",
      stringr::str_detect(tolower(Crime_Type_Standardized), "robbery") ~ "Robbery", 
      stringr::str_detect(tolower(Crime_Type_Standardized), "theft|larceny") ~ "Theft",
      stringr::str_detect(tolower(Crime_Type_Standardized), "assault|violence") ~ "Violence",
      TRUE ~ "Other"
    ),
    
    # Justification availability
    Has_justification = !is.na(Quoted_Rationale) & Quoted_Rationale != "",
    
    # Rationale categories
    Rationale_clean = dplyr::case_when(
      stringr::str_detect(tolower(Rationale_Category), "data") ~ "Data Availability",
      stringr::str_detect(tolower(Rationale_Category), "admin") ~ "Administrative Convenience",
      stringr::str_detect(tolower(Rationale_Category), "theory|theoretical") ~ "Theoretical",
      stringr::str_detect(tolower(Rationale_Category), "practical") ~ "Practical Considerations",
      TRUE ~ as.character(Rationale_Category)
    ),
    
    # Variable complexity (with fallback to reasonable default)
    Total_Variables = dplyr::if_else(
      "Total_Variables" %in% names(data_raw) & !is.na(Total_Variables), 
      as.numeric(Total_Variables), 
      10  # Default reasonable value
    )
  ) |>
  # Filter to complete cases for core analysis
  dplyr::filter(!is.na(Unit_size_km2), !is.na(Year), !is.na(Country_clean))

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
    N_studies = dplyr::n(),
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

# Create correlation matrix for numeric variables
numeric_vars <- data |>
  dplyr::select(Unit_size_km2, Unit_size_log10, Year, Total_Variables) |>
  dplyr::filter(!is.na(Unit_size_km2), !is.na(Unit_size_log10), !is.na(Year), !is.na(Total_Variables))

if(nrow(numeric_vars) >= 3) {
  correlation_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
  
  # Convert to long format for plotting
  corr_data <- correlation_matrix |>
    as.data.frame() |>
    tibble::rownames_to_column("Variable1") |>
    tidyr::pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") |>
    dplyr::mutate(
      Correlation_Text = round(Correlation, 3),
      Variable1 = dplyr::case_when(
        Variable1 == "Unit_size_km2" ~ "Unit Size",
        Variable1 == "Unit_size_log10" ~ "Log Unit Size", 
        Variable1 == "Year" ~ "Publication Year",
        Variable1 == "Total_Variables" ~ "Total Variables",
        TRUE ~ Variable1
      ),
      Variable2 = dplyr::case_when(
        Variable2 == "Unit_size_km2" ~ "Unit Size",
        Variable2 == "Unit_size_log10" ~ "Log Unit Size",
        Variable2 == "Year" ~ "Publication Year", 
        Variable2 == "Total_Variables" ~ "Total Variables",
        TRUE ~ Variable2
      )
    )
  
  # Create correlation plot
  pcorr <- ggplot2::ggplot(corr_data, ggplot2::aes(x = Variable1, y = Variable2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Correlation_Text), 
                      color = "black", size = 3.5, fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = bw_palette$text, mid = bw_palette$fill_secondary, high = bw_palette$fill_light,
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Variable Correlation Matrix") +
    bw_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  ggsave_png(pcorr, output_folder, "correlation_matrix", width = 10, height = 8)
  
  # Save correlation matrix
  custom_save(as.data.frame(correlation_matrix), output_folder, "correlation_matrix", readr::write_csv)
}

# Research Question 1: Unit size distribution-----------------------------

# 1A: Basic distribution
p1a <- ggplot2::ggplot(data, ggplot2::aes(x = Unit_size_km2)) +
  ggplot2::geom_histogram(bins = 30, fill = bw_palette$fill_secondary, color = bw_palette$line_primary, alpha = 0.8) +
  ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)), color = bw_palette$line_primary, linewidth = 1.2) +
  ggplot2::geom_vline(xintercept = median(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = mean(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  ggplot2::labs(x = "Unit Size (km²)", y = "Frequency") +
  bw_theme +
  ggplot2::annotate("text", x = median(data$Unit_size_km2) * 0.8, y = max(table(cut(data$Unit_size_km2, 30))) * 0.9,
           label = paste("Median =", round(median(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 1) +
  ggplot2::annotate("text", x = mean(data$Unit_size_km2) * 1.2, y = max(table(cut(data$Unit_size_km2, 30))) * 0.9,
           label = paste("Mean =", round(mean(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 0)

ggsave_png(p1a, output_folder, "rq1a_unit_size_distribution_new", width = 10, height = 6)

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

# 1C: Size categories
size_cat_data <- data |>
  dplyr::count(Size_category, .drop = FALSE) |>
  dplyr::mutate(percentage = round(n / sum(n) * 100, 1))

p1c <- ggplot2::ggplot(size_cat_data, ggplot2::aes(x = Size_category, y = n)) +
  ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(n, " (", percentage, "%)")), 
            vjust = -0.5, size = 3, color = bw_palette$text) +
  ggplot2::labs(x = "Size Category", y = "Number of Studies") +
  bw_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggsave_png(p1c, output_folder, "rq1c_size_categories_new", width = 12, height = 6)

# Research Question 2: Temporal trends-----------------------------

# Temporal regression model
temporal_model <- lm(Unit_size_log10 ~ Year, data = data)
temporal_summary <- summary(temporal_model)

# 2A: Scatter plot with trend line
p2a <- ggplot2::ggplot(data, ggplot2::aes(x = Year, y = Unit_size_km2)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = bw_palette$line_primary, 
              fill = bw_palette$fill_secondary, alpha = 0.4, linewidth = 1) +
  ggplot2::geom_point(size = 2.5, color = bw_palette$fill_primary, alpha = 0.8) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::labs(x = "Publication Year", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::annotate("text", x = min(data$Year), y = min(data$Unit_size_km2) * 2,
           label = "Individual studies", size = 3, hjust = 0) +
  ggplot2::annotate("text", x = min(data$Year), y = min(data$Unit_size_km2) * 1.4,
           label = "Trend line", size = 3, hjust = 0) +
  ggplot2::annotate("text", x = min(data$Year), y = min(data$Unit_size_km2),
           label = paste("β =", round(coef(temporal_model)[2], 3), ", p =", round(temporal_summary$coefficients[2, 4], 3)), 
           size = 3, hjust = 0)

ggsave_png(p2a, output_folder, "rq2a_temporal_trend_new", width = 10, height = 6)

# 2B: Time period boxplot
p2b <- ggplot2::ggplot(data, ggplot2::aes(x = Time_period, y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::geom_jitter(alpha = 0.6, width = 0.2, size = 1.5, color = bw_palette$fill_primary) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::labs(x = "Time Period", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::annotate("text", x = "2000-2010", y = min(data$Unit_size_km2) * 2,
           label = "Box = 25th-75th percentile", size = 3, hjust = 0) +
  ggplot2::annotate("text", x = "2000-2010", y = min(data$Unit_size_km2) * 1.4,
           label = "Center line = Median", size = 3, hjust = 0) +
  ggplot2::annotate("text", x = "2000-2010", y = min(data$Unit_size_km2),
           label = "Points = Individual studies", size = 3, hjust = 0)

ggsave_png(p2b, output_folder, "rq2b_temporal_boxplot_new", width = 10, height = 6)

# Research Question 3: Country comparisons-----------------------------

# Country summary (countries with ≥3 studies)
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
  dplyr::filter(N_studies >= 3) |>
  dplyr::arrange(Median_size)

# 3A: Country comparison boxplot
country_plot_data <- data |>
  dplyr::filter(Country_clean %in% country_summary$Country_clean)

p3a <- ggplot2::ggplot(country_plot_data, ggplot2::aes(x = reorder(Country_clean, Unit_size_km2, FUN = median), 
                                     y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::labs(x = "Country", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 2,
           label = "Box = 25th-75th percentile", size = 3, hjust = 0.5) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 1.4,
           label = "Center line = Median", size = 3, hjust = 0.5) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2),
           label = "Points = Individual studies", size = 3, hjust = 0.5)

ggsave_png(p3a, output_folder, "rq3a_country_comparison_new", width = 10, height = 8)

# 3B: Anglo-Saxon legal system comparison
anglo_test <- t.test(Unit_size_km2 ~ Anglo_saxon, data = data)
effect_size <- (mean(data$Unit_size_km2[data$Anglo_saxon], na.rm = TRUE) - 
                mean(data$Unit_size_km2[!data$Anglo_saxon], na.rm = TRUE)) / 
               sd(data$Unit_size_km2, na.rm = TRUE)

p3b <- ggplot2::ggplot(data, ggplot2::aes(x = factor(Anglo_saxon, labels = c("Other", "Anglo-Saxon")), 
                        y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
  ggplot2::scale_y_log10(labels = scales::comma_format()) +
  ggplot2::labs(x = "Legal System Tradition", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  ggplot2::annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2) * 2,
           label = paste("p =", round(anglo_test$p.value, 3)), size = 3, hjust = 1) +
  ggplot2::annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2) * 1.4,
           label = paste("Cohen's d =", round(effect_size, 3)), size = 3, hjust = 1) +
  ggplot2::annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2),
           label = "Box = Quartile range | Line = Median | Points = Studies", size = 3, hjust = 1)

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

# 4C: Rationale categories analysis (if available)
if("Rationale_clean" %in% names(data) && !all(is.na(data$Rationale_clean))) {
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
  
  if(nrow(rationale_summary) > 0) {
    p4c <- ggplot2::ggplot(rationale_summary, ggplot2::aes(x = reorder(Rationale_clean, N_studies), y = N_studies)) +
      ggplot2::geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(N_studies, " (", Percentage, "%)")), 
                hjust = -0.1, size = 3.5, color = bw_palette$text) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Rationale Category", y = "Number of Studies") +
      bw_theme
    
    ggsave_png(p4c, output_folder, "rq4c_rationale_categories", width = 12, height = 8)
    
    # Save rationale summary
    custom_save(rationale_summary, output_folder, "rationale_categories_analysis", readr::write_csv)
  }
}

# 4D: Justification by country (for countries with ≥3 studies)
justification_by_country <- data |>
  dplyr::filter(Country_clean %in% country_summary$Country_clean) |>
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
  ggplot2::geom_text(ggplot2::aes(label = paste0(Percent_justified, "%")), 
            hjust = -0.1, size = 3.5, color = bw_palette$text) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Country", y = "Percent with Justification") +
  bw_theme

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
    as.character(spatial_stats$N_studies),
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
      N_studies = nrow(data),
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
cat("You can now update your manuscript by running the update_manuscript_with_analysis.R script\n")
