# Comprehensive spatial unit analysis - new CSV version ----------------
# Updated to work with 20250714_standardized_unit_sizes_with_groups_new.csv

# Load required libraries
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(corrplot)
  library(moments)  # For skewness and kurtosis
  library(openxlsx)
})

# Input and output paths
input_file <- "20250714_Analysis & Results/20250714_standardized_unit_sizes_with_groups_new.csv"
output_dir <- "20250714_Analysis & Results"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load dataset
data_raw <- read_csv(input_file, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Process data with unit conversion and derived variables
data <- data_raw %>%
  # Convert unit sizes to km²
  mutate(
    Unit_size_km2 = case_when(
      Unit == "m2" ~ as.numeric(`Size_of_the_unit`) / 1e6,
      Unit == "km2" ~ as.numeric(`Size_of_the_unit`),
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(Unit_size_km2) %>%
  # Create Study_ID if needed
  mutate(Study_ID = row_number()) %>%
  
  # Extract year from Citation if Year column doesn't exist or is empty
  mutate(
    Year = if("Year" %in% names(.) && !all(is.na(Year))) {
      as.numeric(Year)
    } else {
      as.numeric(str_extract(str_extract(Citation, "\\b(19|20)\\d{2}[a-z]?\\b"), "\\d{4}"))
    }
  ) %>%
  
  # Add size group column based on preferred breakpoints
  mutate(
    Size_group = cut(
      Unit_size_km2,
      breaks = c(-Inf, 0.001, 1.2, 1.63293, 5, Inf),
      labels = c("very small", "small", "medium", "large", "very large"),
      right = FALSE
    )
  ) %>%
  
  # Add derived variables for analysis
  mutate(
    # Core spatial variables
    Unit_size_log10 = log10(Unit_size_km2 + 1e-10),
    Unit_size_m2 = Unit_size_km2 * 1e6,
    
    # Temporal variables
    Year_scaled = as.numeric(scale(Year)),
    Decade = floor(Year / 10) * 10,
    Time_period = case_when(
      Year <= 2010 ~ "2000-2010",
      Year <= 2020 ~ "2011-2020", 
      TRUE ~ "2021-2025"
    ),
    
    # Size categories
    Size_category = case_when(
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
    Country_clean = case_when(
      str_detect(tolower(Country), "united states|usa|us") ~ "United States",
      str_detect(tolower(Country), "united kingdom|uk|britain") ~ "United Kingdom",
      str_detect(tolower(Country), "china") ~ "China",
      str_detect(tolower(Country), "netherlands|holland") ~ "Netherlands",
      str_detect(tolower(Country), "australia") ~ "Australia",
      str_detect(tolower(Country), "belgium") ~ "Belgium",
      str_detect(tolower(Country), "new zealand") ~ "New Zealand",
      TRUE ~ Country
    ),
    
    # Anglo-Saxon legal system indicator
    Anglo_saxon = Country_clean %in% c("United States", "United Kingdom", "Australia", "New Zealand"),
    
    # Crime type grouping
    Crime_type_grouped = case_when(
      str_detect(tolower(Crime_Type_Standardized), "burglary|breaking") ~ "Burglary",
      str_detect(tolower(Crime_Type_Standardized), "robbery") ~ "Robbery", 
      str_detect(tolower(Crime_Type_Standardized), "theft|larceny") ~ "Theft",
      str_detect(tolower(Crime_Type_Standardized), "assault|violence") ~ "Violence",
      TRUE ~ "Other"
    ),
    
    # Justification availability
    Has_justification = !is.na(Quoted_Rationale) & Quoted_Rationale != "",
    
    # Rationale categories
    Rationale_clean = case_when(
      str_detect(tolower(Rationale_Category), "data") ~ "Data Availability",
      str_detect(tolower(Rationale_Category), "admin") ~ "Administrative Convenience",
      str_detect(tolower(Rationale_Category), "theory|theoretical") ~ "Theoretical",
      str_detect(tolower(Rationale_Category), "practical") ~ "Practical Considerations",
      TRUE ~ as.character(Rationale_Category)
    ),
    
    # Variable complexity (use existing counts or create basic ones)
    Total_Variables = if("Total_Variables" %in% names(.)) {
      as.numeric(Total_Variables)
    } else {
      # Create a basic count if not available
      rowSums(select(., contains("_Count")), na.rm = TRUE)
    }
  ) %>%
  # Filter to complete cases for core analysis
  filter(!is.na(Unit_size_km2), !is.na(Year), !is.na(Country_clean))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

# Core statistics

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
bw_theme <- theme_classic() +
  theme(
    panel.background = element_rect(fill = bw_palette$background, color = NA),
    plot.background = element_rect(fill = bw_palette$background, color = NA),
    panel.grid.major = element_line(color = bw_palette$fill_light, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = bw_palette$line_primary, linewidth = 0.5),
    axis.text = element_text(color = bw_palette$text, size = 11),
    axis.title = element_text(color = bw_palette$text, size = 12, face = "bold"),
    legend.background = element_rect(fill = bw_palette$background, color = NA),
    legend.text = element_text(color = bw_palette$text, size = 10),
    legend.title = element_blank(),  # Remove legend titles
    strip.background = element_rect(fill = bw_palette$fill_light, color = bw_palette$line_primary),
    strip.text = element_text(color = bw_palette$text, face = "bold")
  )

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n=== GENERATING DESCRIPTIVE STATISTICS ===\n")

# ============================================================================
# BLACK AND WHITE THEME SETUP
# ============================================================================

# Define black and white color palette
  summarise(
    N_studies = n(),
    N_countries = n_distinct(Country_clean),
    N_journals = n_distinct(Journal, na.rm = TRUE),
    Mean_unit_size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
    Median_unit_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    SD_unit_size = round(sd(Unit_size_km2, na.rm = TRUE), 6),
    Min_unit_size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_unit_size = round(max(Unit_size_km2, na.rm = TRUE), 2),
    Q1_unit_size = round(quantile(Unit_size_km2, 0.25, na.rm = TRUE), 6),
    Q3_unit_size = round(quantile(Unit_size_km2, 0.75, na.rm = TRUE), 6),
    IQR_unit_size = round(IQR(Unit_size_km2, na.rm = TRUE), 6),
    Skewness = round(skewness(Unit_size_km2, na.rm = TRUE), 3),
    Kurtosis = round(kurtosis(Unit_size_km2, na.rm = TRUE), 3),
    Year_range_start = min(Year, na.rm = TRUE),
    Year_range_end = max(Year, na.rm = TRUE),
    Temporal_span = max(Year, na.rm = TRUE) - min(Year, na.rm = TRUE),
    Percent_with_justification = round(mean(Has_justification, na.rm = TRUE) * 100, 1),
    Mean_total_variables = round(mean(Total_Variables, na.rm = TRUE), 1),
    Median_total_variables = round(median(Total_Variables, na.rm = TRUE), 1)
  )

cat("✓ Core descriptive statistics generated\n")
print(spatial_stats  )

cat("✓ Core descriptive statistics generated\n")
print(spatial_stats)

# Save summary statistics
write_csv(spatial_stats, file.path(output_dir, "new_csv_summary_statistics.csv"))

# ============================================================================
# RESEARCH QUESTION 1: UNIT SIZE DISTRIBUTION
# ============================================================================

cat("\n=== RQ1: DISTRIBUTION OF SPATIAL UNIT SIZES ===\n")

cat("\n=== RQ1: DISTRIBUTION OF SPATIAL UNIT SIZES ===\n")

# 1A: Basic distribution
p1a <- ggplot(data, aes(x = Unit_size_km2)) +
  geom_histogram(bins = 30, fill = bw_palette$fill_secondary, color = bw_palette$line_primary, alpha = 0.8) +
  geom_density(aes(y = after_stat(count)), color = bw_palette$line_primary, linewidth = 1.2) +
  geom_vline(xintercept = median(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(data$Unit_size_km2, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  labs(x = "Unit Size (km²)", y = "Frequency") +
  bw_theme +
  # Add direct labels next to lines
  annotate("text", x = median(data$Unit_size_km2) * 0.8, y = max(table(cut(data$Unit_size_km2, 30))) * 0.9,
           label = paste("Median =", round(median(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 1) +
  annotate("text", x = mean(data$Unit_size_km2) * 1.2, y = max(table(cut(data$Unit_size_km2, 30))) * 0.9,
           label = paste("Mean =", round(mean(data$Unit_size_km2, na.rm = TRUE), 3), "km²"), 
           size = 3.5, hjust = 0)

ggsave(file.path(output_dir, "rq1a_unit_size_distribution_new.png"), 
       plot = p1a, width = 10, height = 6, dpi = 300)

# 1B: Log-transformed distribution
p1b <- ggplot(data, aes(x = Unit_size_log10)) +
  # Use fewer bins to make bars taller
  geom_histogram(bins = 15, fill = bw_palette$fill_secondary, color = bw_palette$line_primary, alpha = 0.8) +
  geom_density(aes(y = after_stat(count)), color = bw_palette$line_primary, linewidth = 1.2) +
  geom_vline(xintercept = median(data$Unit_size_log10, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(data$Unit_size_log10, na.rm = TRUE), 
             color = bw_palette$line_primary, linetype = "solid", linewidth = 1) +
  labs(x = "Unit Size (Log10 km²)", y = "Frequency") +
  bw_theme +
  # Calculate histogram for y-axis scaling
  coord_cartesian(ylim = c(0, 35)) +  # Set y-axis limit to make room for labels
  # Place labels outside histogram area at the top, close to lines
  annotate("text", x = mean(data$Unit_size_log10) - 0.05, 
           y = 32,  # Position near top of plot
           label = paste("Mean =", round(10^mean(data$Unit_size_log10), 2), "km²"), 
           size = 3, hjust = 1) +
  annotate("text", x = median(data$Unit_size_log10) + 0.05, 
           y = 33.5,  # Position slightly higher to avoid density curve
           label = paste("Median =", round(10^median(data$Unit_size_log10), 1), "km²"), 
           size = 3, hjust = 0)

ggsave(file.path(output_dir, "rq1b_log_distribution_new.png"), 
       plot = p1b, width = 10, height = 6, dpi = 300)

# 1C: Size categories
size_cat_data <- data %>%
  count(Size_category, .drop = FALSE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

p1c <- ggplot(size_cat_data, aes(x = Size_category, y = n)) +
  geom_col(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.5) +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")), 
            vjust = -0.5, size = 3, color = bw_palette$text) +
  labs(x = "Size Category", y = "Number of Studies") +
  bw_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "rq1c_size_categories_new.png"), 
       plot = p1c, width = 12, height = 6, dpi = 300)

cat("✓ RQ1 distribution plots saved\n")

# ============================================================================
# RESEARCH QUESTION 2: TEMPORAL TRENDS
# ============================================================================

cat("\n=== RQ2: TEMPORAL TRENDS ===\n")

# Temporal model
temporal_model <- lm(Unit_size_log10 ~ Year, data = data)
temporal_summary <- summary(temporal_model)

cat("Temporal trend analysis:\n")
cat("  - Coefficient (β):", round(coef(temporal_model)[2], 4), "\n")
cat("  - P-value:", round(temporal_summary$coefficients[2, 4], 4), "\n")
cat("  - R²:", round(temporal_summary$r.squared, 4), "\n")

# 2A: Scatter plot with trend
p2a <- ggplot(data, aes(x = Year, y = Unit_size_km2)) +
  geom_smooth(method = "lm", se = TRUE, color = bw_palette$line_primary, 
              fill = bw_palette$fill_secondary, alpha = 0.4, linewidth = 1) +
  geom_point(size = 2.5, color = bw_palette$fill_primary, alpha = 0.8) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(x = "Publication Year", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  # Add legend at bottom left with spacing
  annotate("text", x = min(data$Year), y = min(data$Unit_size_km2) * 2,
           label = "Individual studies", size = 3, hjust = 0) +
  annotate("text", x = min(data$Year), y = min(data$Unit_size_km2) * 1.4,
           label = "Trend line", size = 3, hjust = 0) +
  annotate("text", x = min(data$Year), y = min(data$Unit_size_km2),
           label = paste("β =", round(coef(temporal_model)[2], 3), ", p =", round(temporal_summary$coefficients[2, 4], 3)), 
           size = 3, hjust = 0)

ggsave(file.path(output_dir, "rq2a_temporal_trend_new.png"), 
       plot = p2a, width = 10, height = 6, dpi = 300)

# 2B: Time period boxplot
p2b <- ggplot(data, aes(x = Time_period, y = Unit_size_km2)) +
  geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  geom_jitter(alpha = 0.6, width = 0.2, size = 1.5, color = bw_palette$fill_primary) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(x = "Time Period", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  # Add legend at bottom left with spacing
  annotate("text", x = "2000-2010", y = min(data$Unit_size_km2) * 2,
           label = "Box = 25th-75th percentile", size = 3, hjust = 0) +
  annotate("text", x = "2000-2010", y = min(data$Unit_size_km2) * 1.4,
           label = "Center line = Median", size = 3, hjust = 0) +
  annotate("text", x = "2000-2010", y = min(data$Unit_size_km2),
           label = "Points = Individual studies", size = 3, hjust = 0)

ggsave(file.path(output_dir, "rq2b_temporal_boxplot_new.png"), 
       plot = p2b, width = 10, height = 6, dpi = 300)

cat("✓ RQ2 temporal plots saved\n")

# ============================================================================
# RESEARCH QUESTION 3: COUNTRY COMPARISONS
# ============================================================================

cat("\n=== RQ3: COUNTRY COMPARISONS ===\n")

# Country summary (countries with ≥3 studies)
country_summary <- data %>%
  group_by(Country_clean) %>%
  summarise(
    N_studies = n(),
    Median_size = round(median(Unit_size_km2, na.rm = TRUE), 6),
    Mean_size = round(mean(Unit_size_km2, na.rm = TRUE), 3),
    SD_size = round(sd(Unit_size_km2, na.rm = TRUE), 3),
    Min_size = round(min(Unit_size_km2, na.rm = TRUE), 6),
    Max_size = round(max(Unit_size_km2, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(N_studies >= 3) %>%
  arrange(Median_size)

cat("Country-level summary (countries with ≥3 studies):\n")
print(country_summary)

# 3A: Country comparison
country_plot_data <- data %>%
  filter(Country_clean %in% country_summary$Country_clean)

p3a <- ggplot(country_plot_data, aes(x = reorder(Country_clean, Unit_size_km2, FUN = median), 
                                     y = Unit_size_km2)) +
  geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(x = "Country", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add legend at bottom center with spacing
  annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 2,
           label = "Box = 25th-75th percentile", size = 3, hjust = 0.5) +
  annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2) * 1.4,
           label = "Center line = Median", size = 3, hjust = 0.5) +
  annotate("text", x = length(unique(country_plot_data$Country_clean))/2 - 1, 
           y = min(country_plot_data$Unit_size_km2),
           label = "Points = Individual studies", size = 3, hjust = 0.5)

ggsave(file.path(output_dir, "rq3a_country_comparison_new.png"), 
       plot = p3a, width = 10, height = 8, dpi = 300)

# 3B: Anglo-Saxon comparison
anglo_test <- t.test(Unit_size_km2 ~ Anglo_saxon, data = data)
effect_size <- (mean(data$Unit_size_km2[data$Anglo_saxon], na.rm = TRUE) - 
                mean(data$Unit_size_km2[!data$Anglo_saxon], na.rm = TRUE)) / 
               sd(data$Unit_size_km2, na.rm = TRUE)

cat("Anglo-Saxon vs Other legal systems:\n")
cat("  - T-test p-value:", round(anglo_test$p.value, 4), "\n")
cat("  - Effect size (Cohen's d):", round(effect_size, 3), "\n")

p3b <- ggplot(data, aes(x = factor(Anglo_saxon, labels = c("Other", "Anglo-Saxon")), 
                        y = Unit_size_km2)) +
  geom_boxplot(fill = bw_palette$fill_secondary, color = bw_palette$line_primary, linewidth = 0.7) +
  geom_jitter(alpha = 0.7, width = 0.2, size = 2, color = bw_palette$fill_primary) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(x = "Legal System Tradition", y = "Unit Size (km²) - Log Scale") +
  bw_theme +
  # Add legend at bottom right with spacing
  annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2) * 2,
           label = paste("p =", round(anglo_test$p.value, 3)), size = 3, hjust = 1) +
  annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2) * 1.4,
           label = paste("Cohen's d =", round(effect_size, 3)), size = 3, hjust = 1) +
  annotate("text", x = "Anglo-Saxon", y = min(data$Unit_size_km2),
           label = "Box = Quartile range | Line = Median | Points = Studies", size = 3, hjust = 1)

ggsave(file.path(output_dir, "rq3b_anglo_saxon_new.png"), 
       plot = p3b, width = 10, height = 6, dpi = 300)

cat("✓ RQ3 country comparison plots saved\n")

# ============================================================================
# SUMMARY AND EXPORT
# ============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All plots saved to:", output_dir, "\n")
cat("- No legend titles included\n")
cat("- Black and white theme applied\n")
cat("- Individual PNG files created for each research question\n")

# Create a comprehensive results Excel file
wb <- createWorkbook()

# Add summary statistics sheet
addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", spatial_stats)

# Add country summary
addWorksheet(wb, "Country_Summary") 
writeData(wb, "Country_Summary", country_summary)

# Add the processed dataset
addWorksheet(wb, "Processed_Data")
writeData(wb, "Processed_Data", data)

# Save Excel file
saveWorkbook(wb, file.path(output_dir, "new_csv_analysis_results.xlsx"), overwrite = TRUE)

cat("✓ Excel results file saved: new_csv_analysis_results.xlsx\n")
cat("✓ Analysis completed successfully!\n")
