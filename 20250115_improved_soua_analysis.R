# Improved Size of Unit Analysis (SoUA) with Grey-scale Visualizations
# Date: 2025-01-15
# Author: Enhanced analysis for Size of Unit Analysis

# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)
library(kableExtra)
library(scales)
library(gridExtra)
library(viridis)
library(RColorBrewer)

# Set grey color palette for consistent styling
grey_palette <- c("#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525")
theme_grey_minimal <- theme_minimal() +
  theme(
    panel.grid = element_line(color = "grey90", size = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "grey20"),
    axis.text = element_text(color = "grey40"),
    plot.title = element_text(color = "grey10", face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Read the merged dataset
data <- read_csv("20250709_Analysis & Results/20250709_standardized_unit_sizes_with_groups_merged.csv")

cat("Dataset loaded successfully with", nrow(data), "studies\n")

# =============================================================================
# DATA PREPARATION AND SUMMARY STATISTICS
# =============================================================================

# Create clean data for analysis
analysis_data <- data %>%
  filter(!is.na(Unit_size_km2) & Unit_size_km2 > 0) %>%
  mutate(
    log_unit_size = log10(Unit_size_km2),
    size_category = case_when(
      Unit_size_km2 < 0.001 ~ "Micro (< 0.001 km²)",
      Unit_size_km2 >= 0.001 & Unit_size_km2 < 0.01 ~ "Very Small (0.001-0.01 km²)",
      Unit_size_km2 >= 0.01 & Unit_size_km2 < 0.1 ~ "Small (0.01-0.1 km²)",
      Unit_size_km2 >= 0.1 & Unit_size_km2 < 1 ~ "Medium (0.1-1 km²)",
      Unit_size_km2 >= 1 & Unit_size_km2 < 10 ~ "Large (1-10 km²)",
      Unit_size_km2 >= 10 ~ "Very Large (≥10 km²)"
    ),
    size_category = factor(size_category, levels = c(
      "Micro (< 0.001 km²)",
      "Very Small (0.001-0.01 km²)",
      "Small (0.01-0.1 km²)",
      "Medium (0.1-1 km²)",
      "Large (1-10 km²)",
      "Very Large (≥10 km²)"
    )),
    # Simplify crime types for analysis
    crime_type_simple = case_when(
      grepl("Burglary", Crime_Type, ignore.case = TRUE) ~ "Burglary",
      grepl("Robbery", Crime_Type, ignore.case = TRUE) ~ "Robbery",
      grepl("Theft", Crime_Type, ignore.case = TRUE) ~ "Theft",
      grepl("Multiple", Crime_Type, ignore.case = TRUE) ~ "Multiple Types",
      TRUE ~ "Other"
    ),
    # Extract publication year from citation
    pub_year = as.numeric(stringr::str_extract(Year_period_data_from, "\\d{4}")),
    # Clean unit type names
    unit_type_clean = case_when(
      grepl("Block", Unit_Type, ignore.case = TRUE) ~ "Block-based",
      grepl("Tract", Unit_Type, ignore.case = TRUE) ~ "Tract-based",
      grepl("Area", Unit_Type, ignore.case = TRUE) ~ "Area-based",
      grepl("Neighborhood", Unit_Type, ignore.case = TRUE) ~ "Neighborhood",
      grepl("Property", Unit_Type, ignore.case = TRUE) ~ "Property-level",
      grepl("Street", Unit_Type, ignore.case = TRUE) ~ "Street-based",
      grepl("Grid", Unit_Type, ignore.case = TRUE) ~ "Grid-based",
      grepl("Ward", Unit_Type, ignore.case = TRUE) ~ "Ward-based",
      grepl("Postal", Unit_Type, ignore.case = TRUE) ~ "Postal code",
      TRUE ~ "Other"
    )
  )

cat("Analysis data prepared with", nrow(analysis_data), "valid studies\n")

# Summary statistics table
summary_stats <- analysis_data %>%
  summarise(
    `Number of Studies` = n(),
    `Mean Unit Size (km²)` = round(mean(Unit_size_km2, na.rm = TRUE), 4),
    `Median Unit Size (km²)` = round(median(Unit_size_km2, na.rm = TRUE), 4),
    `Min Unit Size (km²)` = round(min(Unit_size_km2, na.rm = TRUE), 6),
    `Max Unit Size (km²)` = round(max(Unit_size_km2, na.rm = TRUE), 2),
    `Standard Deviation` = round(sd(Unit_size_km2, na.rm = TRUE), 4),
    `Q1 (km²)` = round(quantile(Unit_size_km2, 0.25, na.rm = TRUE), 4),
    `Q3 (km²)` = round(quantile(Unit_size_km2, 0.75, na.rm = TRUE), 4)
  )

print("Summary Statistics:")
print(summary_stats)

# =============================================================================
# VISUALIZATION 1: DISTRIBUTION OF UNIT SIZES
# =============================================================================

# Create histogram with log scale
p1 <- ggplot(analysis_data, aes(x = log_unit_size)) +
  geom_histogram(bins = 20, fill = "grey60", color = "grey20", alpha = 0.8) +
  geom_vline(xintercept = log10(median(analysis_data$Unit_size_km2)), 
             linetype = "dashed", color = "grey20", size = 1) +
  scale_x_continuous(
    name = "Unit Size (log₁₀ km²)",
    breaks = seq(-4, 1, 1),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10")
  ) +
  labs(
    title = "Distribution of Spatial Unit Sizes in Crime Location Choice Studies",
    subtitle = "Histogram showing the frequency distribution of unit sizes (n=51 studies)",
    y = "Number of Studies",
    caption = "Dashed line indicates median unit size"
  ) +
  theme_grey_minimal +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# Save the plot
ggsave("20250115_Analysis & Results/unit_size_distribution.png", p1, 
       width = 10, height = 6, dpi = 300, bg = "white")

# =============================================================================
# VISUALIZATION 2: UNIT SIZES BY CRIME TYPE
# =============================================================================

p2 <- ggplot(analysis_data, aes(x = crime_type_simple, y = Unit_size_km2)) +
  geom_boxplot(fill = "grey70", color = "grey20", alpha = 0.8, outlier.color = "grey40") +
  scale_y_log10(
    name = "Unit Size (km²)",
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10")
  ) +
  labs(
    title = "Spatial Unit Sizes by Crime Type",
    subtitle = "Box plots showing the distribution of unit sizes across different crime types",
    x = "Crime Type",
    caption = "Y-axis on logarithmic scale"
  ) +
  theme_grey_minimal +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave("20250115_Analysis & Results/unit_size_by_crime_type.png", p2, 
       width = 10, height = 6, dpi = 300, bg = "white")

# =============================================================================
# VISUALIZATION 3: UNIT SIZES BY UNIT TYPE
# =============================================================================

p3 <- ggplot(analysis_data, aes(x = reorder(unit_type_clean, Unit_size_km2, median), 
                               y = Unit_size_km2)) +
  geom_boxplot(fill = "grey70", color = "grey20", alpha = 0.8, outlier.color = "grey40") +
  scale_y_log10(
    name = "Unit Size (km²)",
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10")
  ) +
  labs(
    title = "Spatial Unit Sizes by Unit Type",
    subtitle = "Box plots showing unit sizes across different spatial unit types (ordered by median)",
    x = "Unit Type",
    caption = "Y-axis on logarithmic scale; unit types ordered by median size"
  ) +
  theme_grey_minimal +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

ggsave("20250115_Analysis & Results/unit_size_by_unit_type.png", p3, 
       width = 12, height = 6, dpi = 300, bg = "white")

# =============================================================================
# VISUALIZATION 4: TEMPORAL TRENDS
# =============================================================================

# Filter data with valid publication years
temporal_data <- analysis_data %>%
  filter(!is.na(pub_year) & pub_year >= 2000 & pub_year <= 2025)

if(nrow(temporal_data) > 10) {
  p4 <- ggplot(temporal_data, aes(x = pub_year, y = Unit_size_km2)) +
    geom_point(color = "grey40", alpha = 0.7, size = 2) +
    geom_smooth(method = "loess", color = "grey20", fill = "grey80", alpha = 0.3) +
    scale_y_log10(
      name = "Unit Size (km²)",
      breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10),
      labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10")
    ) +
    scale_x_continuous(
      name = "Publication Year",
      breaks = seq(2000, 2025, 5)
    ) +
    labs(
      title = "Temporal Trends in Spatial Unit Sizes",
      subtitle = "Unit sizes over time with smoothed trend line",
      caption = "Y-axis on logarithmic scale; grey band shows 95% confidence interval"
    ) +
    theme_grey_minimal +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  
  ggsave("20250115_Analysis & Results/temporal_trends.png", p4, 
         width = 10, height = 6, dpi = 300, bg = "white")
}

# =============================================================================
# VISUALIZATION 5: SIZE CATEGORIES FREQUENCY
# =============================================================================

size_freq <- analysis_data %>%
  count(size_category) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(n, " (", round(percentage, 1), "%)")
  )

p5 <- ggplot(size_freq, aes(x = size_category, y = n)) +
  geom_col(fill = "grey60", color = "grey20", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.5, color = "grey20", size = 3) +
  labs(
    title = "Frequency of Studies by Unit Size Category",
    subtitle = "Number and percentage of studies in each size category",
    x = "Unit Size Category",
    y = "Number of Studies"
  ) +
  theme_grey_minimal +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  ) +
  ylim(0, max(size_freq$n) * 1.1)

ggsave("20250115_Analysis & Results/size_categories_frequency.png", p5, 
       width = 10, height = 6, dpi = 300, bg = "white")

# =============================================================================
# TABLE 1: SUMMARY BY CRIME TYPE
# =============================================================================

crime_summary <- analysis_data %>%
  group_by(crime_type_simple) %>%
  summarise(
    `Number of Studies` = n(),
    `Mean Size (km²)` = round(mean(Unit_size_km2), 4),
    `Median Size (km²)` = round(median(Unit_size_km2), 4),
    `Min Size (km²)` = round(min(Unit_size_km2), 6),
    `Max Size (km²)` = round(max(Unit_size_km2), 2),
    `Standard Deviation` = round(sd(Unit_size_km2), 4),
    .groups = 'drop'
  ) %>%
  arrange(desc(`Number of Studies`))

# =============================================================================
# TABLE 2: SUMMARY BY UNIT TYPE
# =============================================================================

unit_type_summary <- analysis_data %>%
  group_by(unit_type_clean) %>%
  summarise(
    `Number of Studies` = n(),
    `Mean Size (km²)` = round(mean(Unit_size_km2), 4),
    `Median Size (km²)` = round(median(Unit_size_km2), 4),
    `Min Size (km²)` = round(min(Unit_size_km2), 6),
    `Max Size (km²)` = round(max(Unit_size_km2), 2),
    `Standard Deviation` = round(sd(Unit_size_km2), 4),
    .groups = 'drop'
  ) %>%
  arrange(desc(`Number of Studies`))

# =============================================================================
# TABLE 3: SIZE CATEGORIES DETAILED
# =============================================================================

size_category_detailed <- analysis_data %>%
  group_by(size_category) %>%
  summarise(
    `Number of Studies` = n(),
    `Percentage` = round(n() / nrow(analysis_data) * 100, 1),
    `Representative Studies` = paste(head(substr(Title_of_the_study, 1, 40), 3), collapse = "; "),
    .groups = 'drop'
  )

# =============================================================================
# SAVE TABLES AS CSV
# =============================================================================

write_csv(crime_summary, "20250115_Analysis & Results/summary_by_crime_type.csv")
write_csv(unit_type_summary, "20250115_Analysis & Results/summary_by_unit_type.csv")
write_csv(size_category_detailed, "20250115_Analysis & Results/size_categories_detailed.csv")
write_csv(summary_stats, "20250115_Analysis & Results/overall_summary_statistics.csv")

# =============================================================================
# GENERATE COMBINED VISUALIZATION PANEL
# =============================================================================

# Create a combined panel with multiple visualizations
combined_plot <- grid.arrange(
  p1, p2, p5, 
  ncol = 1, 
  heights = c(1, 1, 1)
)

# Save combined plot
ggsave("20250115_Analysis & Results/combined_soua_analysis.png", combined_plot, 
       width = 12, height = 16, dpi = 300, bg = "white")

# =============================================================================
# STATISTICAL ANALYSIS
# =============================================================================

# Test for differences between crime types
if(length(unique(analysis_data$crime_type_simple)) > 1) {
  kruskal_test_crime <- kruskal.test(Unit_size_km2 ~ crime_type_simple, data = analysis_data)
  cat("\nKruskal-Wallis test for differences between crime types:\n")
  print(kruskal_test_crime)
}

# Test for differences between unit types
if(length(unique(analysis_data$unit_type_clean)) > 1) {
  kruskal_test_unit <- kruskal.test(Unit_size_km2 ~ unit_type_clean, data = analysis_data)
  cat("\nKruskal-Wallis test for differences between unit types:\n")
  print(kruskal_test_unit)
}

# Correlation with temporal trends if enough data
if(nrow(temporal_data) > 10) {
  correlation_time <- cor.test(temporal_data$pub_year, log10(temporal_data$Unit_size_km2), 
                              method = "spearman")
  cat("\nSpearman correlation between publication year and log unit size:\n")
  print(correlation_time)
}

# =============================================================================
# PRINT SUMMARY TABLES
# =============================================================================

cat("\n" + rep("=", 80) + "\n")
cat("SIZE OF UNIT ANALYSIS (SoUA) - IMPROVED RESULTS\n")
cat(rep("=", 80) + "\n\n")

cat("OVERALL SUMMARY STATISTICS:\n")
print(summary_stats)

cat("\n\nSUMMARY BY CRIME TYPE:\n")
print(crime_summary)

cat("\n\nSUMMARY BY UNIT TYPE:\n")
print(unit_type_summary)

cat("\n\nSIZE CATEGORIES:\n")
print(size_category_detailed)

cat("\n\nFiles saved to '20250115_Analysis & Results/' directory:\n")
cat("- unit_size_distribution.png\n")
cat("- unit_size_by_crime_type.png\n")
cat("- unit_size_by_unit_type.png")
if(nrow(temporal_data) > 10) cat("\n- temporal_trends.png")
cat("\n- size_categories_frequency.png\n")
cat("- combined_soua_analysis.png\n")
cat("- summary_by_crime_type.csv\n")
cat("- summary_by_unit_type.csv\n")
cat("- size_categories_detailed.csv\n")
cat("- overall_summary_statistics.csv\n")

cat("\nAnalysis completed successfully!\n")
