# Comprehensive Spatial Unit Analysis (SOUA) - New CSV Version
# Updated to work with 20250714_standardized_unit_sizes_with_groups_new.csv

# Configuration and parameters-----------------------------

# Core parameters
run_date <- Sys.Date()
base_dir <- here::here()
data_filename <- "20250714_standardized_unit_sizes_with_groups_new.csv"
output_subdir <- paste0(format(run_date, "%Y%m%d"), "_Analysis & Results")

# Size category breakpoints
size_breaks <- c(0, 0.001, 0.01, 0.1, 1, 10, Inf)
size_labels <- c(
  "Micro (< 0.001 km²)",
  "Very Small (0.001-0.01 km²)",
  "Small (0.01-0.1 km²)",
  "Medium (0.1-1 km²)",
  "Large (1-10 km²)",
  "Very Large (≥10 km²)"
)

# Country classification
anglo_saxon_countries <- c("United States", "United Kingdom", "Canada", "Australia", "New Zealand")

# Visualization parameters
grey_palette <- c("#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525", "#1A1A1A", "#000000")
plot_width <- 12
plot_height <- 8
plot_dpi <- 300

# Analysis thresholds
min_sample_size <- 30
min_jurisdictions <- 5
large_sample_threshold <- 1000

# Plot constants
histogram_bins <- 25
density_multiplier <- 0.4
annotation_height_factor <- 0.9

# Package loading and setup-----------------------------

load_packages <- function() {
  # Set CRAN mirror if not already set
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }
  
  # List of required packages
  required_packages <- c(
    "tidyverse", "dplyr", "ggplot2", "readr", "lme4", "performance", 
    "broom", "gridExtra", "scales", "kableExtra", "stringr", "e1071", 
    "here", "writexl", "purrr", "magrittr", "tibble", "tidyr", "moments"
  )
  
  # Install missing packages
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, quiet = TRUE)
  }
}

# Custom functions-----------------------------

# Create unified grey theme for all plots
create_grey_theme <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      axis.text = ggplot2::element_text(color = "black", size = 11),
      axis.title = ggplot2::element_text(color = "black", size = 12, face = "bold"),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.text = ggplot2::element_text(color = "black", size = 10),
      legend.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90", color = "black"),
      strip.text = ggplot2::element_text(color = "black", face = "bold")
    )
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

# Create size categories as a separate function
make_size_categories <- function(data) {
  data %>%
    dplyr::mutate(
      Size_Category = cut(
        Unit_size_km2, 
        breaks = size_breaks, 
        labels = size_labels, 
        include.lowest = TRUE, 
        right = FALSE
      )
    )
}

# Create folder with date
make_folder <- function(date = Sys.Date(), subfolder = NULL) {
  date_string <- format(date, "%Y%m%d")
  main_folder_name <- paste0(date_string, "_Analysis & Results")
  
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)
  }
  
  return(full_folder_path)
}

# Load and prepare the main dataset
prep_data <- function(data_path) {
  # Check file exists
  if (!file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
  }
  
  # Load data with error handling
  tryCatch({
    data <- readr::read_csv(data_path, show_col_types = FALSE)
  }, error = function(e) {
    stop("Failed to read CSV file: ", e$message)
  })
  
  print("Columns in dataset:")
  print(names(data))
  
  # Check required columns exist
  required_cols <- c("Size_of_the_unit", "Unit", "Country")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))
  }
  
  # Map and calculate required fields
  data <- data %>%
    dplyr::mutate(
      # Unit size conversion
      Unit_size_km2 = dplyr::case_when(
        Unit == "m2" ~ Size_of_the_unit / 1000000,
        Unit == "km2" ~ Size_of_the_unit,
        Unit == "hectares" ~ Size_of_the_unit / 100,
        TRUE ~ Size_of_the_unit / 1000000  # Default to m2 conversion
      ),
      
      # Log transformation
      Log_Unit_size = log10(pmax(Unit_size_km2, 1e-10)),
      
      # Country standardization and classification
      Country_clean = dplyr::case_when(
        stringr::str_detect(tolower(Country), "united states|usa|us") ~ "United States",
        stringr::str_detect(tolower(Country), "united kingdom|uk|britain") ~ "United Kingdom",
        stringr::str_detect(tolower(Country), "china") ~ "China",
        stringr::str_detect(tolower(Country), "netherlands|holland") ~ "Netherlands",
        stringr::str_detect(tolower(Country), "australia") ~ "Australia",
        stringr::str_detect(tolower(Country), "belgium") ~ "Belgium",
        stringr::str_detect(tolower(Country), "new zealand") ~ "New Zealand",
        stringr::str_detect(tolower(Country), "canada") ~ "Canada",
        TRUE ~ Country
      ),
      
      # Anglo-Saxon classification
      Anglo_Saxon = ifelse(Country_clean %in% anglo_saxon_countries, "Anglo-Saxon", "Other"),
      
      # Publication year
      Publication_Year = Year,
      
      # Unit type
      Unit_Type = Name_of_the_unit,
      
      # Crime type grouping
      Crime_type_grouped = dplyr::case_when(
        stringr::str_detect(tolower(Crime_Type_Standardized), "burglary|breaking") ~ "Burglary",
        stringr::str_detect(tolower(Crime_Type_Standardized), "robbery") ~ "Robbery", 
        stringr::str_detect(tolower(Crime_Type_Standardized), "theft|larceny") ~ "Theft",
        stringr::str_detect(tolower(Crime_Type_Standardized), "assault|violence") ~ "Violence",
        TRUE ~ "Other"
      ),
      
      # Research sophistication (based on available variables)
      Research_Sophistication = pmax(1, pmin(5, round(Total_Variables / 5))),
      
      # Variable complexity categories
      Variable_Complexity = dplyr::case_when(
        Total_Variables <= 10 ~ "Low",
        Total_Variables <= 20 ~ "Medium",
        TRUE ~ "High"
      ),
      
      # Derived variables for analysis
      Total_Variables_Numeric = Total_Variables,
      Variable_Diversity_Score = pmin(Total_Variables / 10, 3),  # Normalized score
      
      # Justification variables
      Has_Unit_Justification = !is.na(Quoted_Rationale) & Quoted_Rationale != "",
      Has_Rationale_Category = !is.na(Rationale_Category) & Rationale_Category != "",
      
      # Data limitation scoring (simplified)
      Data_Limitation_Score = dplyr::case_when(
        !is.na(Data_Quality_Issues) & Data_Quality_Issues != "" ~ 3,
        !is.na(Missing_Data_Issues) & Missing_Data_Issues != "" ~ 2,
        TRUE ~ 1
      ),
      
      Data_Limitation_Category = dplyr::case_when(
        Data_Limitation_Score >= 3 ~ "High",
        Data_Limitation_Score == 2 ~ "Medium",
        TRUE ~ "Low"
      ),
      
      # Jurisdiction (same as Country_clean)
      Jurisdiction = Country_clean
    )
  
  # Filter and add size categories
  data <- data %>%
    dplyr::filter(!is.na(Unit_size_km2) & Unit_size_km2 > 0 & 
                 !is.infinite(Log_Unit_size) & !is.na(Log_Unit_size)) %>%
    make_size_categories()
  
  print("Data preparation complete. Row count:")
  print(nrow(data))
  
  return(data)
}

# Generate summary statistics
calculate_summary_stats <- function(data) {
  data %>%
    dplyr::summarise(
      N_Studies = dplyr::n(),
      Mean_Unit_Size = round(mean(Unit_size_km2, na.rm = TRUE), 4),
      Median_Unit_Size = round(median(Unit_size_km2, na.rm = TRUE), 4),
      SD_Unit_Size = round(sd(Unit_size_km2, na.rm = TRUE), 4),
      Min_Unit_Size = round(min(Unit_size_km2, na.rm = TRUE), 6),
      Max_Unit_Size = round(max(Unit_size_km2, na.rm = TRUE), 2),
      Q1_Unit_Size = round(quantile(Unit_size_km2, 0.25, na.rm = TRUE), 4),
      Q3_Unit_Size = round(quantile(Unit_size_km2, 0.75, na.rm = TRUE), 4),
      IQR_Unit_Size = round(IQR(Unit_size_km2, na.rm = TRUE), 4),
      Skewness = round(moments::skewness(Unit_size_km2, na.rm = TRUE), 3),
      Kurtosis = round(moments::kurtosis(Unit_size_km2, na.rm = TRUE), 3)
    )
}

# Create distribution plot
create_distribution_plot <- function(data) {
  # Pre-compute histogram for annotation
  hist_data <- hist(data$Log_Unit_size, breaks = histogram_bins, plot = FALSE)
  max_count <- max(hist_data$counts)
  
  # Calculate median and mean values
  median_val <- median(data$Unit_size_km2, na.rm = TRUE)
  mean_val <- mean(data$Unit_size_km2, na.rm = TRUE)
  median_log <- log10(pmax(median_val, 1e-9))
  mean_log <- log10(pmax(mean_val, 1e-9))
  
  ggplot2::ggplot(data, ggplot2::aes(x = Log_Unit_size)) +
    ggplot2::geom_histogram(bins = histogram_bins, fill = "grey60", color = "grey20", alpha = 0.8) +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density) * (nrow(data) * density_multiplier)), 
                         color = "grey20", linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = median_log, 
                       linetype = "dashed", color = "grey20", linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = mean_log, 
                       linetype = "dotted", color = "grey20", linewidth = 1.2) +
    ggplot2::scale_x_continuous(
      name = "Spatial Unit Size (log₁₀ km²)",
      breaks = seq(-5, 2, 1),
      labels = c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1", "10", "100")
    ) +
    ggplot2::labs(y = "Frequency") +
    # Median annotation (left side)
    ggplot2::annotate("text", 
                     x = median_log, 
                     y = max_count * 0.95,
                     label = paste("Median =", round(median_val, 3), "km²"),
                     color = "grey20", size = 3.5, hjust = 1.1, vjust = 1,
                     fontface = "bold") +
    # Mean annotation (right side)
    ggplot2::annotate("text", 
                     x = mean_log, 
                     y = max_count * 0.95,
                     label = paste("Mean =", round(mean_val, 3), "km²"),
                     color = "grey20", size = 3.5, hjust = -0.1, vjust = 1,
                     fontface = "bold") +
    # Legend box (positioned in top-left corner)
    ggplot2::annotate("rect", 
                     xmin = -4.8, xmax = -3.2,
                     ymin = max_count * 0.75, 
                     ymax = max_count * 0.95,
                     fill = "white", color = "grey30", alpha = 0.95, linewidth = 0.5) +
    # Legend text
    ggplot2::annotate("text", 
                     x = -4.0, 
                     y = max_count * 0.90,
                     label = "— — — Median", 
                     color = "grey20", size = 2.8, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = -4.0, 
                     y = max_count * 0.84,
                     label = "· · · · · Mean", 
                     color = "grey20", size = 2.8, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = -4.0, 
                     y = max_count * 0.78,
                     label = "——— Density", 
                     color = "grey20", size = 2.8, hjust = 0.5, fontface = "bold") +
    create_grey_theme()
}

# Create correlation matrix plot
create_correlation_plot <- function(data) {
  # Select only available numeric variables that actually exist in the data
  available_vars <- intersect(c("Publication_Year", "Unit_size_km2", 
                               "Log_Unit_size", "Research_Sophistication", "Total_Variables"), 
                             names(data))
  
  if (length(available_vars) < 2) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "Insufficient numeric variables for correlation matrix", 
                         size = 6, color = "grey40") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void()
    )
  }
  
  # Select only available numeric variables
  numeric_vars <- data %>%
    dplyr::select(dplyr::all_of(available_vars)) %>%
    dplyr::filter(complete.cases(.))
  
  if (nrow(numeric_vars) < 3 || ncol(numeric_vars) < 2) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "Insufficient complete observations for correlation matrix\n(Need at least 3 complete cases)", 
                         size = 6, color = "grey40") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void()
    )
  }
  
  # Rename columns for better display
  numeric_vars <- numeric_vars %>%
    dplyr::rename_with(~ dplyr::case_when(
      .x == "Publication_Year" ~ "Pub Year",
      .x == "Unit_size_km2" ~ "Unit Size",
      .x == "Log_Unit_size" ~ "Log Unit",
      .x == "Research_Sophistication" ~ "Research Score",
      .x == "Total_Variables" ~ "Total Vars",
      TRUE ~ .x
    ))
  
  correlation_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
  
  corr_data <- correlation_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Variable1") %>%
    tidyr::pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation") %>%
    dplyr::mutate(Correlation_Text = round(Correlation, 3))
  
  ggplot2::ggplot(corr_data, ggplot2::aes(x = Variable1, y = Variable2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = Correlation_Text), 
                      color = "white", size = 3, fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = "grey10", mid = "grey50", high = "grey90",
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation\n(Pearson)"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text = ggplot2::element_text(size = 9)
    ) +
    create_grey_theme()
}

# Create temporal trend plot
create_temporal_plot <- function(data) {
  temporal_data <- data %>% 
    dplyr::filter(!is.na(Publication_Year) & Publication_Year >= 2000)
  
  if (nrow(temporal_data) < 5) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "Insufficient data for temporal analysis", 
                         size = 6, color = "grey40") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void()
    )
  }
  
  temporal_lm <- lm(Log_Unit_size ~ Publication_Year, data = temporal_data)
  temporal_summary <- summary(temporal_lm)
  
  ggplot2::ggplot(temporal_data, ggplot2::aes(x = Publication_Year, y = Log_Unit_size)) +
    ggplot2::geom_point(color = "grey40", alpha = 0.6, size = 2.5) +
    ggplot2::geom_smooth(method = "lm", color = "grey20", fill = "grey70", alpha = 0.3) +
    ggplot2::geom_smooth(method = "loess", color = "grey30", linetype = "dashed", se = FALSE) +
    ggplot2::labs(
      x = "Publication Year",
      y = "Log₁₀(Unit Size in km²)"
    ) +
    # Add stats annotation
    ggplot2::annotate("text", 
                     x = min(temporal_data$Publication_Year), 
                     y = max(temporal_data$Log_Unit_size) * 0.9,
                     label = paste("β =", round(coef(temporal_lm)[2], 3), 
                                  ", R² =", round(temporal_summary$r.squared, 3)), 
                     color = "grey10", size = 3.5, hjust = 0, fontface = "bold") +
    create_grey_theme()
}

# Create jurisdictional comparison plot
create_jurisdictional_plot <- function(data) {
  data %>%
    dplyr::filter(!is.na(Anglo_Saxon)) %>%
    ggplot2::ggplot(ggplot2::aes(x = Anglo_Saxon, y = Log_Unit_size)) +
    ggplot2::geom_violin(ggplot2::aes(fill = Anglo_Saxon), alpha = 0.7, width = 0.8, color = "grey20") +
    ggplot2::geom_boxplot(width = 0.3, alpha = 0.9, color = "grey20", outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.5, color = "grey30", size = 2) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
                         fill = "grey10", color = "grey10") +
    ggplot2::scale_fill_manual(values = c("Anglo-Saxon" = "grey70", "Other" = "grey40")) +
    ggplot2::labs(
      x = "Jurisdiction Type",
      y = "Log₁₀(Unit Size in km²)"
    ) +
    ggplot2::theme(legend.position = "none") +
    create_grey_theme()
}

# Unit selection justification analysis-----------------------------

# Analyze unit selection justification coverage and types
analyze_unit_justification <- function(data) {
  # Basic justification coverage
  justification_summary <- data %>%
    dplyr::summarise(
      Total_Studies = dplyr::n(),
      With_Justification = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE),
      Percent_Justified = round(100 * With_Justification / Total_Studies, 1),
      With_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
      With_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE)
    )
  
  # Rationale categories
  rationale_by_category <- data %>%
    dplyr::filter(!is.na(Rationale_Category) & Rationale_Category != "") %>%
    dplyr::count(Rationale_Category, sort = TRUE) %>%
    dplyr::mutate(Percentage = round(100 * n / sum(n), 1))
  
  # Justification by unit type
  justification_by_unit_type <- data %>%
    dplyr::group_by(Unit_Type) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      Avg_Unit_Size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
      Percent_Justified = round(100 * mean(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Avg_Unit_Size)
  
  # Variable complexity and justification
  complexity_justification <- data %>%
    dplyr::group_by(Variable_Complexity) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      Percent_Justified = round(100 * mean(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE), 1),
      Mean_Variables = round(mean(Total_Variables, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  return(list(
    summary = justification_summary,
    by_category = rationale_by_category,
    by_unit_type = justification_by_unit_type,
    by_complexity = complexity_justification
  ))
}

# Create justification analysis plots
create_justification_plots <- function(data, justification_analysis) {
  # Plot 1: Rationale categories
  if (nrow(justification_analysis$by_category) > 0) {
    rationale_plot <- ggplot2::ggplot(justification_analysis$by_category, 
                                     ggplot2::aes(x = reorder(Rationale_Category, n), y = n)) +
      ggplot2::geom_col(fill = "grey60", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(n, " (", Percentage, "%)")), 
                        hjust = -0.1, size = 3.5) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Rationale Category",
        y = "Number of Studies"
      ) +
      create_grey_theme()
  } else {
    rationale_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                       label = "No rationale categories available", 
                       size = 6, color = "grey40") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme_void()
  }
    
  # Plot 2: Justification by unit type  
  unit_type_plot <- ggplot2::ggplot(justification_analysis$by_unit_type,
                                   ggplot2::aes(x = reorder(Unit_Type, Avg_Unit_Size), y = Percent_Justified)) +
    ggplot2::geom_col(fill = "grey50", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Percent_Justified, "%")), 
                      hjust = -0.1, size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Unit Type",
      y = "Percent with Justification"
    ) +
    create_grey_theme()
    
  # Plot 3: Variable complexity and justification
  complexity_plot <- ggplot2::ggplot(justification_analysis$by_complexity,
                                    ggplot2::aes(x = Variable_Complexity, y = Count)) +
    ggplot2::geom_col(fill = "grey40", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, size = 3.5) +
    ggplot2::labs(
      x = "Variable Complexity Category", 
      y = "Number of Studies"
    ) +
    create_grey_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(list(
    rationale = rationale_plot,
    unit_type = unit_type_plot,
    complexity = complexity_plot
  ))
}

# Generate all analysis tables
generate_analysis_tables <- function(data, summary_stats, multivariate_results, justification_analysis) {
  # Table 1: Summary Statistics
  table1_summary_stats <- data.frame(
    Statistic = c("Studies analyzed", "Countries represented", "Median unit size", "Mean unit size", 
                  "Smallest unit", "Largest unit", "Standard deviation", 
                  "Skewness (original)", "Orders of magnitude"),
    Value = c(
      as.character(nrow(data)),
      as.character(dplyr::n_distinct(data$Country_clean)),
      paste(round(median(data$Unit_size_km2, na.rm = TRUE), 4), "km²"),
      paste(round(mean(data$Unit_size_km2, na.rm = TRUE), 4), "km²"),
      paste(round(min(data$Unit_size_km2, na.rm = TRUE), 6), "km²"),
      paste(round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
      paste(round(sd(data$Unit_size_km2, na.rm = TRUE), 4), "km²"),
      as.character(round(moments::skewness(data$Unit_size_km2, na.rm = TRUE), 2)),
      as.character(round(log10(max(data$Unit_size_km
