# Comprehensive Spatial Unit Analysis - New CSV Version
# Updated to work with 20250714_standardized_unit_sizes_with_groups_new.csv

# Custom functions-----------------------------

# Function to create a folder with a date argument
make_folder <- function(date = Sys.Date(), subfolder = NULL) {
  # Convert the provided date to "YYYYMMDD" format
  date_string <- format(date, "%Y%m%d")
  
  # Create the main folder name using the date
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
    bg = "white"
  )
  return(file_path)
}

# Color palette for plots (black and white theme)
bw_palette <- list(
  fill_primary = "lightgray",
  fill_secondary = "white", 
  line_primary = "black",
  line_secondary = "darkgray",
  text_primary = "black"
)

# Data loading and preparation-----------------------------

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
    # Unit size standardization (convert Size_of_the_unit based on Unit column)
    Unit_size_km2 = dplyr::case_when(
      Unit == "m2" ~ Size_of_the_unit / 1000000,
      Unit == "km2" ~ Size_of_the_unit,
      Unit == "hectares" ~ Size_of_the_unit / 100,
      TRUE ~ Size_of_the_unit / 1000000  # Default to m2 conversion
    ),
    
    # Log transformation for size
    Log_unit_size = log10(Unit_size_km2 + 1e-6),
    
    # Size categories
    Size_category = factor(dplyr::case_when(
      Unit_size_km2 < 0.001 ~ "Micro (<0.001 km²)",
      Unit_size_km2 >= 0.001 & Unit_size_km2 < 0.01 ~ "Very Small (0.001-0.01 km²)",
      Unit_size_km2 >= 0.01 & Unit_size_km2 < 0.1 ~ "Small (0.01-0.1 km²)",
      Unit_size_km2 >= 0.1 & Unit_size_km2 < 1 ~ "Medium (0.1-1 km²)",
      Unit_size_km2 >= 1 & Unit_size_km2 < 10 ~ "Large (1-10 km²)",
      Unit_size_km2 >= 10 ~ "Very Large (>10 km²)",
      TRUE ~ "Unknown"
    ), levels = c(
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
    )
  ) |>
  # Remove any rows with missing unit sizes
  dplyr::filter(!is.na(Unit_size_km2) & Unit_size_km2 > 0)

# Descriptive statistics-----------------------------

# Overall summary statistics
overall_stats <- data |>
  dplyr::summarise(
    Total_studies = dplyr::n(),
    Mean_size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
    Median_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    SD_size = round(sd(Unit_size_km2, na.rm = TRUE), 6),
    Min_size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_size = round(max(Unit_size_km2, na.rm = TRUE), 3),
    Q1_size = round(quantile(Unit_size_km2, 0.25, na.rm = TRUE), 6),
    Q3_size = round(quantile(Unit_size_km2, 0.75, na.rm = TRUE), 6),
    IQR_size = round(Q3_size - Q1_size, 6),
    Skewness = round(moments::skewness(Unit_size_km2, na.rm = TRUE), 3),
    Kurtosis = round(moments::kurtosis(Unit_size_km2, na.rm = TRUE), 3)
  )

# Size category distribution
size_category_summary <- data |>
  dplyr::count(Size_category, name = "N_studies") |>
  dplyr::mutate(
    Percentage = round(N_studies / sum(N_studies) * 100, 1)
  ) |>
  dplyr::arrange(Size_category)

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

# Crime type summary
crime_type_summary <- data |>
  dplyr::group_by(Crime_type_grouped) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    Median_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    Mean_size = round(mean(Unit_size_km2, na.rm = TRUE), 3),
    SD_size = round(sd(Unit_size_km2, na.rm = TRUE), 3),
    .groups = 'drop'
  ) |>
  dplyr::arrange(dplyr::desc(N_studies))

# Legal system comparison
legal_system_summary <- data |>
  dplyr::group_by(Anglo_saxon) |>
  dplyr::summarise(
    N_studies = dplyr::n(),
    Median_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    Mean_size = round(mean(Unit_size_km2, na.rm = TRUE), 3),
    SD_size = round(sd(Unit_size_km2, na.rm = TRUE), 3),
    .groups = 'drop'
  ) |>
  dplyr::mutate(
    Legal_system = ifelse(Anglo_saxon, "Anglo-Saxon", "Other")
  )

# Visualizations-----------------------------

# RQ1A: Overall distribution histogram
p1a <- ggplot2::ggplot(data, ggplot2::aes(x = Unit_size_km2)) +
  ggplot2::geom_histogram(bins = 25, fill = bw_palette$fill_primary, color = bw_palette$line_primary, 
                         linewidth = 0.3, alpha = 0.8) +
  ggplot2::scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::labs(
    x = "Spatial Unit Size (km², log scale)",
    y = "Number of Studies"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::annotation_logticks(sides = "b", color = bw_palette$line_secondary, size = 0.3)

# RQ1B: Distribution with mean and median lines
mean_val <- mean(data$Unit_size_km2, na.rm = TRUE)
median_val <- median(data$Unit_size_km2, na.rm = TRUE)

p1b <- ggplot2::ggplot(data, ggplot2::aes(x = Unit_size_km2)) +
  ggplot2::geom_histogram(bins = 25, fill = bw_palette$fill_primary, color = bw_palette$line_primary, 
                         linewidth = 0.3, alpha = 0.8) +
  ggplot2::geom_vline(xintercept = mean_val, linetype = "dashed", 
                     color = bw_palette$line_primary, linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = median_val, linetype = "solid", 
                     color = bw_palette$line_primary, linewidth = 0.8) +
  ggplot2::scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::labs(
    x = "Spatial Unit Size (km², log scale)",
    y = "Number of Studies"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::annotation_logticks(sides = "b", color = bw_palette$line_secondary, size = 0.3) +
  ggplot2::annotate("text", x = mean_val * 1.8, y = max(ggplot2::ggplot_build(p1a)$data[[1]]$count) * 0.9,
           label = paste("Mean =", round(mean_val, 3), "km²"), 
           hjust = 0, color = bw_palette$text_primary, size = 3) +
  ggplot2::annotate("text", x = median_val * 1.8, y = max(ggplot2::ggplot_build(p1a)$data[[1]]$count) * 0.8,
           label = paste("Median =", round(median_val, 3), "km²"), 
           hjust = 0, color = bw_palette$text_primary, size = 3)

# RQ2: Size category distribution bar plot
p2 <- ggplot2::ggplot(size_category_summary, ggplot2::aes(x = Size_category, y = N_studies)) +
  ggplot2::geom_bar(stat = "identity", fill = bw_palette$fill_primary, color = bw_palette$line_primary, 
                   linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(N_studies, "\n(", Percentage, "%)")), 
                    vjust = -0.3, color = bw_palette$text_primary, size = 3, fontface = "bold") +
  ggplot2::labs(
    x = "Spatial Unit Size Category",
    y = "Number of Studies"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, color = bw_palette$text_primary, size = 9),
    axis.text.y = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    panel.grid.major.y = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))

# RQ3A: Country comparison boxplot (countries with ≥3 studies)
country_plot_data <- data |>
  dplyr::filter(Country_clean %in% country_summary$Country_clean)

p3a <- ggplot2::ggplot(country_plot_data, ggplot2::aes(x = reorder(Country_clean, Unit_size_km2, FUN = median), 
                                     y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::labs(
    x = "Country",
    y = "Spatial Unit Size (km², log scale)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, color = bw_palette$text_primary, size = 9),
    axis.text.y = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    panel.grid.major = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::annotation_logticks(sides = "l", color = bw_palette$line_secondary, size = 0.3) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 2,
           label = paste("n =", nrow(country_plot_data), "studies"), 
           color = bw_palette$text_primary, size = 3) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 1.4,
           label = paste("Countries with ≥3 studies"), 
           color = bw_palette$text_primary, size = 3) +
  ggplot2::annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2),
           label = "Ordered by median size", 
           color = bw_palette$text_primary, size = 3)

# RQ3B: Crime type comparison boxplot
p3b <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(Crime_type_grouped, Unit_size_km2, FUN = median), 
                                         y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::labs(
    x = "Crime Type",
    y = "Spatial Unit Size (km², log scale)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, color = bw_palette$text_primary, size = 9),
    axis.text.y = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    panel.grid.major = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::annotation_logticks(sides = "l", color = bw_palette$line_secondary, size = 0.3) +
  ggplot2::annotate("text", x = 2.5, y = min(data$Unit_size_km2),
           label = "Ordered by median size", 
           color = bw_palette$text_primary, size = 3)

# RQ4: Legal system comparison boxplot
p4 <- ggplot2::ggplot(data, ggplot2::aes(x = ifelse(Anglo_saxon, "Anglo-Saxon", "Other"), 
                                        y = Unit_size_km2)) +
  ggplot2::geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  ggplot2::scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ggplot2::labs(
    x = "Legal System",
    y = "Spatial Unit Size (km², log scale)"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(color = bw_palette$text_primary, size = 10),
    axis.title = ggplot2::element_text(color = bw_palette$text_primary, size = 11, face = "bold"),
    panel.grid.major = ggplot2::element_line(color = "lightgray", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  ) +
  ggplot2::annotation_logticks(sides = "l", color = bw_palette$line_secondary, size = 0.3)

# Save plots-----------------------------

# Save individual plots as PNG
ggsave_png(p1a, output_folder, "rq1a_unit_size_distribution_new")
ggsave_png(p1b, output_folder, "rq1b_unit_size_distribution_with_stats_new")
ggsave_png(p2, output_folder, "rq2_size_category_distribution_new")
ggsave_png(p3a, output_folder, "rq3a_country_comparison_new")
ggsave_png(p3b, output_folder, "rq3b_crime_type_comparison_new")
ggsave_png(p4, output_folder, "rq4_legal_system_comparison_new")

# Export data tables-----------------------------

# Create a list of all tables for multi-sheet Excel export
all_tables <- list(
  "Overall_Statistics" = overall_stats,
  "Size_Categories" = size_category_summary,
  "Countries_Summary" = country_summary,
  "Crime_Types_Summary" = crime_type_summary,
  "Legal_System_Summary" = legal_system_summary
)

# Export multi-sheet Excel file
current_date <- format(Sys.Date(), "%Y%m%d")
excel_filename <- paste0(current_date, "_new_csv_analysis_results.xlsx")
excel_path <- here::here(output_folder, excel_filename)
writexl::write_xlsx(all_tables, excel_path)

# Also export individual CSV files
custom_save(overall_stats, output_folder, "overall_statistics_new", readr::write_csv)
custom_save(size_category_summary, output_folder, "size_category_summary_new", readr::write_csv)
custom_save(country_summary, output_folder, "country_summary_new", readr::write_csv)
custom_save(crime_type_summary, output_folder, "crime_type_summary_new", readr::write_csv)
custom_save(legal_system_summary, output_folder, "legal_system_summary_new", readr::write_csv)

# Print completion message
message("Analysis completed successfully!")
message(paste("Output folder:", output_folder))
message(paste("Excel file saved as:", excel_filename))
message(paste("Individual PNG files and CSV tables saved to:", output_folder))
