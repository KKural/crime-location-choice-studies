# ============================================================================
# COMPREHENSIVE SPATIAL UNIT ANALYSIS (SOUA) - NEW CSV VERSION
# ============================================================================
# 
# This script processes the new CSV file format and provides a complete 
# analysis of spatial unit selection in crime location choice studies.
#
# Input: 20250714_standardized_unit_sizes_with_groups_new.csv
# Output: Analysis results and manuscript-ready PNG figures
#
# ============================================================================

# Clear environment and set options
rm(list = ls())
options(scipen = 999)
set.seed(42)

# ============================================================================
# 1. PACKAGE LOADING AND SETUP
# ============================================================================

required_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", 
  "ggplot2", "gridExtra", "corrplot", "viridis", "scales",
  "e1071", "broom",
  "openxlsx", "knitr", "flextable"
)

install_and_load <- function(packages) {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  cat("Package loading completed!\n")
}

install_and_load(required_packages)

# ============================================================================
# 2. CONFIGURATION AND PARAMETERS
# ============================================================================

# Date configuration
today_date <- format(Sys.Date(), "%Y%m%d")
output_dir <- paste0(today_date, "_Analysis & Results")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Input file configuration
input_file <- "20250714_standardized_unit_sizes_with_groups_new.csv"

# Black and white theme for all visualizations
bw_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black", family = "Arial"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black"),
    legend.background = element_rect(fill = "white", color = "black"),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(color = "black", face = "bold")
  )

# Black and white color palette
bw_palette <- list(
  fill_primary = "grey40",
  fill_secondary = "grey70", 
  fill_light = "grey90",
  line_primary = "black",
  line_secondary = "grey50"
)

cat("=== NEW COMPREHENSIVE SPATIAL UNIT ANALYSIS INITIALIZED ===\n")
cat("Input file:", input_file, "\n")
cat("Output directory:", output_dir, "\n")

# ============================================================================
# 3. DATA LOADING AND PROCESSING
# ============================================================================

cat("\n=== LOADING AND PROCESSING NEW DATASET ===\n")

# Load raw data
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

df_raw_data <- read_csv(input_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("✓ Raw dataset loaded successfully\n")
cat("  - Rows:", nrow(df_raw_data), "\n")
cat("  - Columns:", ncol(df_raw_data), "\n")

# Process data following your requested steps
df_data_clean <- df_raw_data %>%
  mutate(
    # Convert unit sizes to km²
    Unit_size_km2 = case_when(
      Unit == "m2" ~ as.numeric(`Size_of_the_unit`) / 1e6,
      Unit == "km2" ~ as.numeric(`Size_of_the_unit`),
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(Unit_size_km2) %>%
  # Create Study_ID for sorting
  mutate(Study_ID = row_number())

# Add size group column based on preferred breakpoints
size_breaks <- c(-Inf, 0.001, 1.2, 1.63293, 5, Inf)
size_labels <- c("very small", "small", "medium", "large", "very large")

# Add size group column and extract publication year from Citation column
df_data_clean <- df_data_clean %>%
  mutate(
    Size_group = cut(
      Unit_size_km2,
      breaks = size_breaks,
      labels = size_labels,
      right = FALSE
    ),
    # Extract 4-digit year from citation format
    Year = as.numeric(str_extract(str_extract(Citation, "\\b(19|20)\\d{2}[a-z]?\\b"), "\\d{4}"))
  )

# Create analysis dataset with all needed columns
data <- df_data_clean %>%
  mutate(
    # Core spatial variables
    Unit_size_log10 = log10(Unit_size_km2 + 1e-10),
    Unit_size_m2 = Unit_size_km2 * 1e6,
    
    # Temporal variables
    Year_scaled = as.numeric(scale(Year)),
    Decade = floor(Year / 10) * 10,
    Time_period = case_when(
      Year >= 2021 ~ "2021-2025",
      Year >= 2016 ~ "2016-2020",
      Year >= 2011 ~ "2011-2015",
      Year >= 2006 ~ "2006-2010",
      TRUE ~ "Before 2006"
    ),
    
    # Size categories for analysis
    Size_category = case_when(
      Unit_size_km2 < 0.1 ~ "Small (<0.1 km²)",
      Unit_size_km2 < 1 ~ "Medium (0.1-1 km²)",
      Unit_size_km2 < 10 ~ "Large (1-10 km²)",
      TRUE ~ "Very Large (>10 km²)"
    ),
    Size_category = factor(Size_category, levels = c(
      "Small (<0.1 km²)", "Medium (0.1-1 km²)", "Large (1-10 km²)", "Very Large (>10 km²)"
    )),
    
    # Country standardization
    Country_clean = str_trim(Country),
    Anglo_saxon = Country_clean %in% c("United States", "United Kingdom", "Australia", "Canada"),
    
    # Crime type standardization
    Crime_type_grouped = case_when(
      str_detect(tolower(Crime_Type_Standardized), "burglary|theft") ~ "Property Crime",
      str_detect(tolower(Crime_Type_Standardized), "robbery|assault|violence") ~ "Violent Crime",
      str_detect(tolower(Crime_Type_Standardized), "drug") ~ "Drug Crime",
      TRUE ~ "Other"
    ),
    
    # Justification analysis
    Has_unit_justification = !is.na(Quoted_Rationale) & str_trim(Quoted_Rationale) != "",
    Rationale_category_clean = case_when(
      is.na(Rationale_Category) ~ "No justification",
      TRUE ~ Rationale_Category
    ),
    
    # Variable complexity (where available)
    Total_Variables = as.numeric(Total_Variables),
    Demographic_Count = as.numeric(Demographic_Count),
    Economic_Count = as.numeric(Economic_Count), 
    Environmental_Count = as.numeric(Environmental_Count),
    Distance_Count = as.numeric(Distance_Count),
    Temporal_Count = as.numeric(Temporal_Count),
    
    # Data quality indicators
    Has_limitations_data = !is.na(Data_Quality_Issues) & Data_Quality_Issues != "",
    Has_scale_recommendations = !is.na(Scale_Recommendations_Status) & 
                               Scale_Recommendations_Status == "Specified",
    Has_scale_limitations = !is.na(Scale_Limitations_Status) & 
                           Scale_Limitations_Status == "Specified"
  ) %>%
  # Remove any rows with missing core variables
  filter(!is.na(Unit_size_km2), !is.na(Year), !is.na(Country_clean))

cat("✓ Data processing completed\n")
cat("  - Final dataset:", nrow(data), "observations\n")
cat("  - Unit size range:", round(min(data$Unit_size_km2, na.rm = TRUE), 4), "to", 
      round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²\n")

# Save the processed dataset
write_csv(data, file.path(output_dir, paste0(today_date, "_processed_standardized_unit_sizes.csv")))

# Print summary statistics
cat("\n=== UNIT SIZE SUMMARY STATISTICS ===\n")
print(summary(data$Unit_size_km2))

cat("\n=== SIZE GROUP DISTRIBUTION ===\n")
print(table(data$Size_group))

cat("\n=== COUNTRY DISTRIBUTION ===\n")
print(table(data$Country_clean))

# ============================================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n=== GENERATING COMPREHENSIVE DESCRIPTIVE STATISTICS ===\n")

spatial_stats <- data %>%
  summarise(
    N_studies = n(),
    N_countries = n_distinct(Country_clean),
    N_journals = n_distinct(Journal, na.rm = TRUE),
    
    # Unit size statistics
    Mean_unit_size = mean(Unit_size_km2, na.rm = TRUE),
    Median_unit_size = median(Unit_size_km2, na.rm = TRUE),
    SD_unit_size = sd(Unit_size_km2, na.rm = TRUE),
    Min_unit_size = min(Unit_size_km2, na.rm = TRUE),
    Max_unit_size = max(Unit_size_km2, na.rm = TRUE),
    Q1_unit_size = quantile(Unit_size_km2, 0.25, na.rm = TRUE),
    Q3_unit_size = quantile(Unit_size_km2, 0.75, na.rm = TRUE),
    IQR_unit_size = IQR(Unit_size_km2, na.rm = TRUE),
    
    # Distribution properties
    Skewness = e1071::skewness(Unit_size_km2, na.rm = TRUE),
    Kurtosis = e1071::kurtosis(Unit_size_km2, na.rm = TRUE),
    
    # Temporal coverage
    Year_range_start = min(Year, na.rm = TRUE),
    Year_range_end = max(Year, na.rm = TRUE),
    Temporal_span = max(Year, na.rm = TRUE) - min(Year, na.rm = TRUE),
    
    # Justification coverage
    Percent_with_justification = 100 * mean(Has_unit_justification, na.rm = TRUE),
    
    # Methodological sophistication
    Mean_total_variables = mean(Total_Variables, na.rm = TRUE),
    Median_total_variables = median(Total_Variables, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("✓ Core descriptive statistics generated\n")
print(spatial_stats)

# Save summary statistics
write_csv(spatial_stats, file.path(output_dir, "comprehensive_summary_statistics.csv"))

# ============================================================================
# 5. RQ1: DISTRIBUTION OF SPATIAL UNIT SIZES
# ============================================================================

cat("\n=== RQ1: DISTRIBUTION OF SPATIAL UNIT SIZES ===\n")

tryCatch({
  # Main distribution histogram
  p1 <- ggplot(data, aes(x = Unit_size_km2)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = bw_palette$fill_primary, 
                   color = "white", linewidth = 0.3, alpha = 0.7) +
    geom_density(color = bw_palette$line_primary, linewidth = 1.2, alpha = 0.8) +
    geom_vline(aes(xintercept = median(Unit_size_km2, na.rm = TRUE)), 
               color = "black", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = mean(Unit_size_km2, na.rm = TRUE)), 
               color = "grey30", linetype = "dotted", linewidth = 1.2) +
    labs(x = "Unit Size (km²)", y = "Density") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    annotate("rect", xmin = max(data$Unit_size_km2, na.rm = TRUE) * 0.6, 
             xmax = max(data$Unit_size_km2, na.rm = TRUE) * 0.95,
             ymin = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.7,
             ymax = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.95,
             fill = "white", color = "black", linewidth = 0.5) +
    annotate("segment", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.62, 
             xend = max(data$Unit_size_km2, na.rm = TRUE) * 0.68,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.85,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.85,
             color = "black", linetype = "dashed", linewidth = 1) +
    annotate("text", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.7, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.85,
             label = "Median", size = 3, hjust = 0) +
    annotate("segment", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.62, 
             xend = max(data$Unit_size_km2, na.rm = TRUE) * 0.68,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.8,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.8,
             color = "grey30", linetype = "dotted", linewidth = 1.2) +
    annotate("text", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.7, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.8,
             label = "Mean", size = 3, hjust = 0) +
    annotate("segment", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.62, 
             xend = max(data$Unit_size_km2, na.rm = TRUE) * 0.68,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.75,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.75,
             color = bw_palette$line_primary, linewidth = 1.2) +
    annotate("text", x = max(data$Unit_size_km2, na.rm = TRUE) * 0.7, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_km2)) + geom_density())$data[[1]]$y) * 0.75,
             label = "Density curve", size = 3, hjust = 0)
  
  ggsave(file.path(output_dir, "rq1a_unit_size_distribution.png"), 
         plot = p1, width = 10, height = 6, dpi = 300)
  
  # Log-transformed distribution
  p2 <- ggplot(data, aes(x = Unit_size_log10)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = bw_palette$fill_secondary, 
                   color = "white", linewidth = 0.3, alpha = 0.7) +
    geom_density(color = bw_palette$line_primary, linewidth = 1.2, alpha = 0.8) +
    geom_vline(aes(xintercept = median(Unit_size_log10, na.rm = TRUE)), 
               color = "black", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = mean(Unit_size_log10, na.rm = TRUE)), 
               color = "grey30", linetype = "dotted", linewidth = 1.2) +
    labs(x = "Log₁₀(Unit Size)", y = "Density") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    annotate("rect", xmin = max(data$Unit_size_log10, na.rm = TRUE) * 0.65, 
             xmax = max(data$Unit_size_log10, na.rm = TRUE) * 0.98,
             ymin = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.65,
             ymax = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.95,
             fill = "white", color = "black", linewidth = 0.5) +
    annotate("segment", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.67, 
             xend = max(data$Unit_size_log10, na.rm = TRUE) * 0.73,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.9,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.9,
             color = "black", linetype = "dashed", linewidth = 1) +
    annotate("text", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.75, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.9,
             label = "Median", size = 3, hjust = 0) +
    annotate("segment", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.67, 
             xend = max(data$Unit_size_log10, na.rm = TRUE) * 0.73,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.84,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.84,
             color = "grey30", linetype = "dotted", linewidth = 1.2) +
    annotate("text", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.75, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.84,
             label = "Mean", size = 3, hjust = 0) +
    annotate("segment", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.67, 
             xend = max(data$Unit_size_log10, na.rm = TRUE) * 0.73,
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.78,
             yend = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.78,
             color = bw_palette$line_primary, linewidth = 1.2) +
    annotate("text", x = max(data$Unit_size_log10, na.rm = TRUE) * 0.75, 
             y = max(ggplot_build(ggplot(data, aes(x = Unit_size_log10)) + geom_density())$data[[1]]$y) * 0.78,
             label = "Density curve", size = 3, hjust = 0)
  
  ggsave(file.path(output_dir, "rq1b_log_distribution.png"), 
         plot = p2, width = 10, height = 6, dpi = 300)
  
  # Size category distribution
  size_cat_data <- data %>%
    count(Size_category) %>%
    mutate(Percentage = 100 * n / sum(n))
  
  p3 <- ggplot(size_cat_data, aes(x = Size_category, y = n)) +
    geom_col(fill = bw_palette$fill_primary, color = bw_palette$line_primary, linewidth = 0.5) +
    geom_text(aes(label = paste0(n, "\n(", round(Percentage, 1), "%)")), 
              vjust = -0.3, size = 3.5, color = "black") +
    labs(x = "Spatial Unit Size Category", y = "Number of Studies") +
    bw_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "rq1c_size_categories.png"), 
         plot = p3, width = 12, height = 6, dpi = 300)
  
  cat("✓ RQ1 distribution plots saved as individual files\n")
  
}, error = function(e) {
  cat("✗ Error creating distribution plots:", e$message, "\n")
})

# ============================================================================
# 6. RQ2: TEMPORAL TRENDS
# ============================================================================

cat("\n=== RQ2: TEMPORAL TRENDS IN SPATIAL UNIT SELECTION ===\n")

temporal_data <- data %>%
  filter(!is.na(Year), !is.na(Unit_size_km2)) %>%
  arrange(Year)

# Simple linear regression for temporal trend
temporal_model <- lm(Unit_size_log10 ~ Year, data = temporal_data)
temporal_summary <- summary(temporal_model)

cat("Temporal trend analysis:\n")
cat("  - Coefficient (β):", round(coef(temporal_model)[2], 4), "\n")
cat("  - P-value:", round(temporal_summary$coefficients[2, 4], 4), "\n")
cat("  - R²:", round(temporal_summary$r.squared, 4), "\n")

tryCatch({
  # Temporal scatter plot with trend line
  p1 <- ggplot(temporal_data, aes(x = Year, y = Unit_size_km2)) +
    geom_point(size = 2.5, color = bw_palette$fill_primary, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = bw_palette$line_primary, 
                fill = bw_palette$fill_light, alpha = 0.3, linewidth = 1) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(x = "Publication Year", y = "Unit Size (km²) - Log Scale") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    annotate("rect", xmin = min(temporal_data$Year) + 1, 
             xmax = min(temporal_data$Year) + 8,
             ymin = max(temporal_data$Unit_size_km2) * 0.3,
             ymax = max(temporal_data$Unit_size_km2) * 0.8,
             fill = "white", color = "black", linewidth = 0.5) +
    annotate("point", x = min(temporal_data$Year) + 2, 
             y = max(temporal_data$Unit_size_km2) * 0.6,
             size = 2.5, color = bw_palette$fill_primary, alpha = 0.8) +
    annotate("text", x = min(temporal_data$Year) + 2.5, 
             y = max(temporal_data$Unit_size_km2) * 0.6,
             label = "Individual studies", size = 3, hjust = 0) +
    annotate("segment", x = min(temporal_data$Year) + 1.5, 
             xend = min(temporal_data$Year) + 2.5,
             y = max(temporal_data$Unit_size_km2) * 0.5,
             yend = max(temporal_data$Unit_size_km2) * 0.5,
             color = bw_palette$line_primary, linewidth = 1) +
    annotate("text", x = min(temporal_data$Year) + 2.7, 
             y = max(temporal_data$Unit_size_km2) * 0.5,
             label = "Trend line", size = 3, hjust = 0) +
    annotate("text", x = min(temporal_data$Year) + 1.5, 
             y = max(temporal_data$Unit_size_km2) * 0.4,
             label = paste("β =", round(coef(temporal_model)[2], 3)), 
             size = 3, hjust = 0) +
    annotate("text", x = min(temporal_data$Year) + 1.5, 
             y = max(temporal_data$Unit_size_km2) * 0.35,
             label = paste("p =", round(temporal_summary$coefficients[2, 4], 3)), 
             size = 3, hjust = 0)
  
  ggsave(file.path(output_dir, "rq2a_temporal_trend_scatter.png"), 
         plot = p1, width = 10, height = 6, dpi = 300)
  
  # Temporal boxplot by time periods
  p2 <- ggplot(temporal_data, aes(x = Time_period, y = Unit_size_km2)) +
    geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
    geom_jitter(alpha = 0.6, width = 0.2, size = 1.5, color = bw_palette$fill_primary) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(x = "Time Period", y = "Unit Size (km²) - Log Scale") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "rq2b_temporal_boxplot.png"), 
         plot = p2, width = 10, height = 6, dpi = 300)
  
  cat("✓ RQ2 temporal trend plots saved as individual files\n")
  
}, error = function(e) {
  cat("✗ Error creating temporal plots:", e$message, "\n")
})

# ============================================================================
# 7. RQ3: CROSS-NATIONAL VARIATION
# ============================================================================

cat("\n=== RQ3: CROSS-NATIONAL VARIATION IN SPATIAL UNIT SELECTION ===\n")

# Country-level analysis
min_group_size <- 3
country_data <- data %>%
  filter(!is.na(Country_clean), !is.na(Unit_size_km2)) %>%
  group_by(Country_clean) %>%
  filter(n() >= min_group_size) %>%
  summarise(
    N_studies = n(),
    Median_size = median(Unit_size_km2),
    Mean_size = mean(Unit_size_km2),
    SD_size = sd(Unit_size_km2),
    Min_size = min(Unit_size_km2),
    Max_size = max(Unit_size_km2),
    .groups = "drop"
  ) %>%
  arrange(Median_size)

cat("Country-level summary (countries with ≥", min_group_size, "studies):\n")
print(country_data)

# Anglo-Saxon vs Other comparison
anglo_comparison <- data %>%
  filter(!is.na(Anglo_saxon), !is.na(Unit_size_km2)) %>%
  group_by(Anglo_saxon) %>%
  summarise(
    N = n(),
    Mean_size = mean(Unit_size_km2),
    Median_size = median(Unit_size_km2),
    SD_size = sd(Unit_size_km2),
    .groups = "drop"
  )

# T-test for Anglo-Saxon difference
anglo_test <- t.test(Unit_size_log10 ~ Anglo_saxon, data = data)
cat("\nAnglo-Saxon vs Other legal systems:\n")
cat("  - T-test p-value:", round(anglo_test$p.value, 4), "\n")

tryCatch({
  # Country comparison plot
  country_plot_data <- data %>%
    filter(Country_clean %in% country_data$Country_clean)
  
  p1 <- ggplot(country_plot_data, aes(x = reorder(Country_clean, Unit_size_km2, median), 
                                     y = Unit_size_km2)) +
    geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
    geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(x = "Country", y = "Unit Size (km²) - Log Scale") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "rq3a_country_comparison.png"), 
         plot = p1, width = 10, height = 8, dpi = 300)
  
  # Anglo-Saxon comparison
  p2 <- ggplot(data, aes(x = factor(Anglo_saxon, labels = c("Other", "Anglo-Saxon")), 
                        y = Unit_size_km2)) +
    geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
    geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(x = "Legal System Tradition", y = "Unit Size (km²) - Log Scale") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    annotate("text", x = 1.5, y = max(data$Unit_size_km2) * 0.5,
             label = paste("p =", round(anglo_test$p.value, 3)), 
             size = 4, hjust = 0.5)
  
  ggsave(file.path(output_dir, "rq3b_anglo_saxon_comparison.png"), 
         plot = p2, width = 10, height = 6, dpi = 300)
  
  cat("✓ RQ3 jurisdictional comparison plots saved as individual files\n")
  
}, error = function(e) {
  cat("✗ Error creating jurisdictional plots:", e$message, "\n")
})

# Save country summary
write_csv(country_data, file.path(output_dir, "country_level_summary.csv"))

# ============================================================================
# 8. RQ4: CRIME-TYPE SPECIFICITY
# ============================================================================

cat("\n=== RQ4: CRIME-TYPE SPECIFICITY IN SPATIAL SCALE SELECTION ===\n")

# Crime type analysis
crime_data <- data %>%
  filter(!is.na(Crime_type_grouped), !is.na(Unit_size_km2)) %>%
  group_by(Crime_type_grouped) %>%
  filter(n() >= 2) %>%
  summarise(
    N_studies = n(),
    Median_size = median(Unit_size_km2),
    Mean_size = mean(Unit_size_km2),
    SD_size = sd(Unit_size_km2),
    .groups = "drop"
  ) %>%
  arrange(Median_size)

cat("Crime type summary:\n")
print(crime_data)

tryCatch({
  # Crime type comparison
  crime_plot_data <- data %>%
    filter(Crime_type_grouped %in% crime_data$Crime_type_grouped)
  
  p1 <- ggplot(crime_plot_data, aes(x = reorder(Crime_type_grouped, Unit_size_km2, median), 
                                   y = Unit_size_km2)) +
    geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
    geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
    scale_y_log10(labels = scales::comma_format()) +
    labs(x = "Crime Type", y = "Unit Size (km²) - Log Scale") +
    bw_theme +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, "rq4_crime_type_specificity.png"), 
         plot = p1, width = 10, height = 6, dpi = 300)
  
  cat("✓ RQ4 crime type specificity plot saved\n")
  
}, error = function(e) {
  cat("✗ Error creating crime type plot:", e$message, "\n")
})

# ============================================================================
# 9. RQ5: UNIT SELECTION JUSTIFICATION PRACTICES
# ============================================================================

cat("\n=== RQ5: UNIT SELECTION JUSTIFICATION PRACTICES ===\n")

# Justification analysis
justification_data <- data %>%
  filter(!is.na(Rationale_category_clean)) %>%
  count(Rationale_category_clean) %>%
  mutate(Percentage = 100 * n / sum(n)) %>%
  arrange(desc(n))

cat("Justification categories:\n")
print(justification_data)

tryCatch({
  # Rationale categories
  p1 <- ggplot(justification_data, aes(x = reorder(Rationale_category_clean, n), y = n)) +
    geom_col(fill = bw_palette$fill_primary, color = bw_palette$line_primary, linewidth = 0.5) +
    geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
              hjust = -0.1, size = 3, color = "black") +
    labs(x = "Rationale Category", y = "Number of Studies") +
    bw_theme +
    coord_flip()
  
  ggsave(file.path(output_dir, "rq5a_rationale_categories.png"), 
         plot = p1, width = 10, height = 6, dpi = 300)
  
  # Justification by crime type
  if (nrow(crime_data) > 1) {
    justification_by_crime <- data %>%
      filter(!is.na(Crime_type_grouped), !is.na(Has_unit_justification)) %>%
      group_by(Crime_type_grouped) %>%
      summarise(
        N = n(),
        Percent_justified = 100 * mean(Has_unit_justification),
        .groups = "drop"
      ) %>%
      filter(N >= 2)
    
    p2 <- ggplot(justification_by_crime, aes(x = reorder(Crime_type_grouped, Percent_justified), 
                                            y = Percent_justified)) +
      geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
      geom_text(aes(label = paste0(round(Percent_justified, 1), "%\n(n=", N, ")")), 
                vjust = -0.3, size = 3, color = "black") +
      labs(x = "Crime Type", y = "Percentage with Justification") +
      bw_theme +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 100)
    
    ggsave(file.path(output_dir, "rq5b_justification_by_type.png"), 
           plot = p2, width = 10, height = 6, dpi = 300)
  }
  
  cat("✓ RQ5 justification analysis plots saved\n")
  
}, error = function(e) {
  cat("✗ Error creating justification plots:", e$message, "\n")
})

# ============================================================================
# 10. SAVE COMPREHENSIVE RESULTS
# ============================================================================

cat("\n=== SAVING COMPREHENSIVE RESULTS ===\n")

# Create comprehensive Excel output
tryCatch({
  wb <- createWorkbook()
  
  # Summary statistics
  addWorksheet(wb, "Summary_Statistics")
  writeData(wb, "Summary_Statistics", spatial_stats)
  
  # Country data
  if (nrow(country_data) > 0) {
    addWorksheet(wb, "Country_Analysis")
    writeData(wb, "Country_Analysis", country_data)
  }
  
  # Size distribution
  size_dist <- data %>%
    count(Size_category) %>%
    mutate(Percentage = 100 * n / sum(n))
  addWorksheet(wb, "Size_Distribution")
  writeData(wb, "Size_Distribution", size_dist)
  
  # Justification data
  addWorksheet(wb, "Justification_Analysis")
  writeData(wb, "Justification_Analysis", justification_data)
  
  # Raw processed data
  addWorksheet(wb, "Processed_Data")
  writeData(wb, "Processed_Data", data)
  
  saveWorkbook(wb, file.path(output_dir, "comprehensive_soua_analysis_results.xlsx"), overwrite = TRUE)
  
  cat("✓ Excel results file saved\n")
  
}, error = function(e) {
  cat("✗ Error saving Excel file:", e$message, "\n")
})

# ============================================================================
# 11. FINAL SUMMARY
# ============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("✓ All analyses completed successfully\n")
cat("✓ Individual PNG files created for each research question\n")
cat("✓ All legend titles removed from figures\n")
cat("✓ Results saved in:", output_dir, "\n")

# Final data summary
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat("Total studies analyzed:", nrow(data), "\n")
cat("Countries represented:", n_distinct(data$Country_clean), "\n")
cat("Year range:", min(data$Year, na.rm = TRUE), "-", max(data$Year, na.rm = TRUE), "\n")
cat("Unit size range:", round(min(data$Unit_size_km2, na.rm = TRUE), 4), "to", 
    round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²\n")
cat("Studies with justification:", sum(data$Has_unit_justification, na.rm = TRUE), 
    "(", round(100 * mean(data$Has_unit_justification, na.rm = TRUE), 1), "%)\n")

cat("\n=== ANALYSIS COMPLETED SUCCESSFULLY ===\n")
