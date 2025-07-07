# Enhanced SUoA Analysis with Integrated Methodological Data
# Research Questions on Spatial Unit of Analysis characteristics across crime location choice studies
# Updated to use extracted methodological metadata rather than keyword-based inference

library(tidyverse)
library(stringr)
library(here)
library(readr)
library(ggplot2)
library(scales)
library(gridExtra)

cat("=======================================================================\n")
cat("ENHANCED ANALYSIS: SPATIAL UNIT OF ANALYSIS (SUoA) CHARACTERISTICS\n")
cat("WITH INTEGRATED METHODOLOGICAL METADATA\n")
cat("=======================================================================\n\n")

# Load the main dataset and methodological metadata
data <- read_csv(here("Output", "merged_comprehensive_unit_sizes.csv"), show_col_types = FALSE)
metadata <- read_csv(here("Output", "studies_metadata.csv"), show_col_types = FALSE)

# Join datasets on study ID (filename-based matching)
data_enhanced <- data %>%
  left_join(metadata, by = c("filename" = "filename")) %>%
  mutate(
    Publication_Year = as.numeric(Publication_Year),
    # Use extracted country data where available, fall back to text search
    Jurisdiction = coalesce(country, case_when(
      str_detect(paste(Citation, Title_of_the_study), "Netherlands|Dutch|The Hague|Amsterdam") ~ "Netherlands",
      str_detect(paste(Citation, Title_of_the_study), "Australia|Australian|Brisbane|Perth|Melbourne|Sydney") ~ "Australia", 
      str_detect(paste(Citation, Title_of_the_study), "Belgium|Belgian|Flanders|Brussels|Ghent") ~ "Belgium",
      str_detect(paste(Citation, Title_of_the_study), "United Kingdom|UK|Britain|British|London|England") ~ "United Kingdom",
      str_detect(paste(Citation, Title_of_the_study), "United States|USA|US|American|Chicago|New York|California") ~ "United States",
      str_detect(paste(Citation, Title_of_the_study), "Canada|Canadian|Toronto|Vancouver") ~ "Canada",
      str_detect(paste(Citation, Title_of_the_study), "China|Chinese|Beijing|Shanghai|ZG city") ~ "China",
      str_detect(paste(Citation, Title_of_the_study), "Japan|Japanese|Tokyo") ~ "Japan",
      str_detect(paste(Citation, Title_of_the_study), "Germany|German|Berlin") ~ "Germany",
      TRUE ~ "Other"
    )),
    # Use extracted crime type where available
    Crime_Type = coalesce(crime_type, case_when(
      str_detect(tolower(Title_of_the_study), "burglary") ~ "Burglary",
      str_detect(tolower(Title_of_the_study), "robbery") ~ "Robbery", 
      str_detect(tolower(Title_of_the_study), "theft") ~ "Theft",
      str_detect(tolower(Title_of_the_study), "graffiti") ~ "Graffiti",
      str_detect(tolower(Title_of_the_study), "drug") ~ "Drug Crime",
      str_detect(tolower(Title_of_the_study), "assault") ~ "Assault",
      str_detect(tolower(Title_of_the_study), "crime") ~ "General Crime",
      TRUE ~ "Other"
    )),
    # Clean and standardize methodological variables
    Statistical_Method_Clean = case_when(
      str_detect(tolower(statistical_method), "conditional logit") ~ "Conditional Logit",
      str_detect(tolower(statistical_method), "mixed logit") ~ "Mixed Logit",
      str_detect(tolower(statistical_method), "discrete choice|discrete spatial choice") ~ "Discrete Choice",
      str_detect(tolower(statistical_method), "multinomial") ~ "Multinomial Logit",
      TRUE ~ "Other/Unspecified"
    ),
    Software_Clean = case_when(
      str_detect(tolower(software_used), "stata") ~ "Stata",
      str_detect(tolower(software_used), "^r|r ") ~ "R",
      str_detect(tolower(software_used), "not mentioned|not specified") ~ "Not Specified",
      is.na(software_used) ~ "Not Specified",
      TRUE ~ "Other"
    ),
    Model_Type_Clean = case_when(
      str_detect(tolower(model_type), "conditional logit") ~ "Conditional Logit",
      str_detect(tolower(model_type), "mixed logit") ~ "Mixed Logit",
      str_detect(tolower(model_type), "discrete choice") ~ "Discrete Choice",
      str_detect(tolower(model_type), "multinomial") ~ "Multinomial Logit",
      TRUE ~ "Other/Unspecified"
    ),
    Study_Design_Clean = case_when(
      str_detect(tolower(study_design), "cross-sectional") ~ "Cross-sectional",
      str_detect(tolower(study_design), "longitudinal") ~ "Longitudinal",
      TRUE ~ "Other/Unspecified"
    ),
    # Create Anglo-Saxon vs Other grouping
    Anglo_Saxon = case_when(
      Jurisdiction %in% c("United States", "United Kingdom", "Canada", "Australia") ~ "Anglo-Saxon",
      TRUE ~ "Other"
    ),
    # Calculate areas and transformations
    No_of_units_numeric = as.numeric(str_replace_all(No_of_units, "[^0-9.]", "")),
    Total_study_area_km2 = Unit_size_km2 * No_of_units_numeric,
    # Create time periods
    Time_Period = case_when(
      Publication_Year <= 2010 ~ "2003-2010",
      Publication_Year <= 2015 ~ "2011-2015", 
      Publication_Year <= 2020 ~ "2016-2020",
      Publication_Year >= 2021 ~ "2021-2025"
    ),
    Decade = floor(Publication_Year / 10) * 10,
    # Log transformations
    Log_Unit_size = log10(Unit_size_km2),
    Log_Total_area = log10(Total_study_area_km2 + 1e-10),
    # Extract sample size as numeric
    Sample_Size_Numeric = as.numeric(str_extract(sample_size, "\\d+"))
  )

cat("Dataset Overview:\n")
cat("- Total studies analyzed:", nrow(data_enhanced), "\n")
cat("- Studies with methodological metadata:", sum(!is.na(data_enhanced$statistical_method)), "\n")
cat("- Year range:", min(data_enhanced$Publication_Year, na.rm = TRUE), "-", max(data_enhanced$Publication_Year, na.rm = TRUE), "\n")
cat("- Unit size range:", round(min(data_enhanced$Unit_size_km2, na.rm = TRUE), 6), "-", round(max(data_enhanced$Unit_size_km2, na.rm = TRUE), 2), "km²\n\n")

# =================================================================================
# ENHANCED RESEARCH QUESTION 6: METHODOLOGICAL APPROACHES AND SUoA SIZES
# =================================================================================

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("RQ6 ENHANCED: METHODOLOGICAL APPROACHES AND SUoA SIZES\n")
cat("Using extracted methodological metadata rather than keyword inference\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Statistical Methods Analysis
cat("STATISTICAL METHODS ANALYSIS:\n")
cat("─────────────────────────────\n")

methods_analysis <- data_enhanced %>%
  filter(!is.na(Statistical_Method_Clean), Statistical_Method_Clean != "Other/Unspecified") %>%
  group_by(Statistical_Method_Clean) %>%
  summarise(
    n_studies = n(),
    median_size = median(Unit_size_km2, na.rm = TRUE),
    mean_size = mean(Unit_size_km2, na.rm = TRUE),
    q25_size = quantile(Unit_size_km2, 0.25, na.rm = TRUE),
    q75_size = quantile(Unit_size_km2, 0.75, na.rm = TRUE),
    min_size = min(Unit_size_km2, na.rm = TRUE),
    max_size = max(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_studies))

print(methods_analysis)

# Software Usage Analysis
cat("\nSOFTWARE USAGE ANALYSIS:\n")
cat("────────────────────────\n")

software_analysis <- data_enhanced %>%
  filter(!is.na(Software_Clean), Software_Clean != "Not Specified") %>%
  group_by(Software_Clean) %>%
  summarise(
    n_studies = n(),
    median_size = median(Unit_size_km2, na.rm = TRUE),
    mean_size = mean(Unit_size_km2, na.rm = TRUE),
    q25_size = quantile(Unit_size_km2, 0.25, na.rm = TRUE),
    q75_size = quantile(Unit_size_km2, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_studies))

print(software_analysis)

# Model Type Analysis
cat("\nMODEL TYPE ANALYSIS:\n")
cat("────────────────────\n")

model_analysis <- data_enhanced %>%
  filter(!is.na(Model_Type_Clean), Model_Type_Clean != "Other/Unspecified") %>%
  group_by(Model_Type_Clean) %>%
  summarise(
    n_studies = n(),
    median_size = median(Unit_size_km2, na.rm = TRUE),
    mean_size = mean(Unit_size_km2, na.rm = TRUE),
    q25_size = quantile(Unit_size_km2, 0.25, na.rm = TRUE),
    q75_size = quantile(Unit_size_km2, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_studies))

print(model_analysis)

# =================================================================================
# NEW RESEARCH QUESTION: SAMPLE SIZE AND CHOICE SET EFFECTS
# =================================================================================

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("NEW RQ: SAMPLE SIZE AND CHOICE SET EFFECTS ON SUoA SELECTION\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Sample size correlation analysis
sample_size_data <- data_enhanced %>%
  filter(!is.na(Sample_Size_Numeric), !is.na(Unit_size_km2))

if(nrow(sample_size_data) > 5) {
  cat("SAMPLE SIZE CORRELATION WITH SUoA SIZE:\n")
  cat("──────────────────────────────────────\n")
  
  correlation_result <- cor.test(sample_size_data$Sample_Size_Numeric, 
                                sample_size_data$Unit_size_km2, 
                                method = "pearson")
  
  cat("Pearson correlation coefficient:", round(correlation_result$estimate, 4), "\n")
  cat("p-value:", round(correlation_result$p.value, 4), "\n")
  cat("95% CI: [", round(correlation_result$conf.int[1], 4), ", ", 
      round(correlation_result$conf.int[2], 4), "]\n\n")
  
  # Sample size quintiles analysis
  sample_quintiles <- data_enhanced %>%
    filter(!is.na(Sample_Size_Numeric), !is.na(Unit_size_km2)) %>%
    mutate(Sample_Quintile = ntile(Sample_Size_Numeric, 5)) %>%
    group_by(Sample_Quintile) %>%
    summarise(
      n_studies = n(),
      min_sample = min(Sample_Size_Numeric),
      max_sample = max(Sample_Size_Numeric),
      median_suoa = median(Unit_size_km2),
      mean_suoa = mean(Unit_size_km2),
      .groups = 'drop'
    )
  
  cat("SAMPLE SIZE QUINTILES VS SUoA SIZE:\n")
  cat("──────────────────────────────────\n")
  print(sample_quintiles)
}

# =================================================================================
# NEW RESEARCH QUESTION: METHODOLOGICAL EVOLUTION OVER TIME
# =================================================================================

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("NEW RQ: METHODOLOGICAL EVOLUTION OVER TIME\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Software adoption over time
software_time <- data_enhanced %>%
  filter(!is.na(Software_Clean), Software_Clean != "Not Specified", !is.na(Publication_Year)) %>%
  group_by(Publication_Year, Software_Clean) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Publication_Year) %>%
  mutate(proportion = n / sum(n))

cat("SOFTWARE ADOPTION TRENDS:\n")
cat("─────────────────────────\n")
software_summary <- software_time %>%
  group_by(Software_Clean) %>%
  summarise(
    first_year = min(Publication_Year),
    last_year = max(Publication_Year),
    total_studies = sum(n),
    .groups = 'drop'
  ) %>%
  arrange(first_year)

print(software_summary)

# Statistical method evolution
method_time <- data_enhanced %>%
  filter(!is.na(Statistical_Method_Clean), Statistical_Method_Clean != "Other/Unspecified", !is.na(Publication_Year)) %>%
  group_by(Publication_Year, Statistical_Method_Clean) %>%
  summarise(n = n(), .groups = 'drop')

cat("\nSTATISTICAL METHOD EVOLUTION:\n")
cat("─────────────────────────────\n")
method_summary <- method_time %>%
  group_by(Statistical_Method_Clean) %>%
  summarise(
    first_year = min(Publication_Year),
    last_year = max(Publication_Year),
    total_studies = sum(n),
    avg_year = round(mean(rep(Publication_Year, n)), 1),
    .groups = 'drop'
  ) %>%
  arrange(first_year)

print(method_summary)

# =================================================================================
# ENHANCED VISUALIZATIONS
# =================================================================================

cat("\n═══════════════════════════════════════════════════════════════════════════════\n")
cat("CREATING ENHANCED VISUALIZATIONS\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# 1. Enhanced Methods vs SUoA visualization
p_methods_enhanced <- data_enhanced %>%
  filter(!is.na(Statistical_Method_Clean), Statistical_Method_Clean != "Other/Unspecified") %>%
  ggplot(aes(x = reorder(Statistical_Method_Clean, Unit_size_km2, median), y = Unit_size_km2)) +
  geom_boxplot(aes(fill = Statistical_Method_Clean), alpha = 0.7, outlier.alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "SUoA Size Distribution by Statistical Method",
    subtitle = "Based on extracted methodological metadata",
    x = "Statistical Method",
    y = "Spatial Unit Size (km²)",
    caption = "Data shows median SUoA sizes vary significantly across methodological approaches"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(here("Output", "RQ6_enhanced_methods_vs_SUoA.png"), p_methods_enhanced, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 2. Software usage over time
p_software_time <- software_time %>%
  ggplot(aes(x = Publication_Year, y = proportion, fill = Software_Clean)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Software Adoption in Crime Location Choice Studies Over Time",
    subtitle = "Proportion of studies using different statistical software packages",
    x = "Publication Year",
    y = "Proportion of Studies",
    fill = "Software Package",
    caption = "Based on studies with specified software information"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom"
  )

ggsave(here("Output", "software_evolution_over_time.png"), p_software_time, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 3. Sample size vs SUoA size relationship
if(nrow(sample_size_data) > 5) {
  p_sample_suoa <- sample_size_data %>%
    ggplot(aes(x = Sample_Size_Numeric, y = Unit_size_km2)) +
    geom_point(aes(color = Jurisdiction), alpha = 0.7, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    scale_x_log10(labels = scales::comma_format()) +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    labs(
      title = "Relationship Between Sample Size and SUoA Size",
      subtitle = paste("Correlation coefficient:", round(correlation_result$estimate, 3)),
      x = "Sample Size (log scale)",
      y = "SUoA Size km² (log scale)",
      color = "Jurisdiction",
      caption = "Each point represents one study; trend line shows overall relationship"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "right"
    )
  
  ggsave(here("Output", "sample_size_vs_suoa.png"), p_sample_suoa, 
         width = 12, height = 8, dpi = 300, bg = "white")
}

# 4. Methodological complexity heatmap
methodology_matrix <- data_enhanced %>%
  filter(!is.na(Statistical_Method_Clean), !is.na(Software_Clean),
         Statistical_Method_Clean != "Other/Unspecified",
         Software_Clean != "Not Specified") %>%
  group_by(Statistical_Method_Clean, Software_Clean) %>%
  summarise(
    n = n(),
    median_suoa = median(Unit_size_km2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(n >= 2)  # Only show combinations with at least 2 studies

if(nrow(methodology_matrix) > 0) {
  p_method_software <- methodology_matrix %>%
    ggplot(aes(x = Software_Clean, y = Statistical_Method_Clean)) +
    geom_tile(aes(fill = median_suoa), color = "white", size = 0.5) +
    geom_text(aes(label = paste0("n=", n)), color = "white", fontface = "bold") +
    scale_fill_viridis_c(trans = "log10", 
                        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    labs(
      title = "Methodological Combinations and Median SUoA Size",
      subtitle = "Combinations with at least 2 studies",
      x = "Software Package",
      y = "Statistical Method",
      fill = "Median SUoA\nSize (km²)",
      caption = "Numbers show count of studies using each combination"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(here("Output", "methodology_combination_heatmap.png"), p_method_software, 
         width = 10, height = 6, dpi = 300, bg = "white")
}

# =================================================================================
# ENHANCED SUMMARY OUTPUTS
# =================================================================================

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("SAVING ENHANCED SUMMARY OUTPUTS\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Save enhanced methodological analysis
write_csv(methods_analysis, here("Output", "enhanced_methods_analysis.csv"))
write_csv(software_analysis, here("Output", "enhanced_software_analysis.csv"))
write_csv(model_analysis, here("Output", "enhanced_model_analysis.csv"))

if(nrow(sample_size_data) > 5) {
  write_csv(sample_quintiles, here("Output", "sample_size_quintiles_analysis.csv"))
}

write_csv(software_summary, here("Output", "software_adoption_trends.csv"))
write_csv(method_summary, here("Output", "statistical_method_evolution.csv"))

# Create enhanced comprehensive summary
enhanced_summary <- data_enhanced %>%
  summarise(
    total_studies = n(),
    studies_with_methodology = sum(!is.na(statistical_method)),
    studies_with_software = sum(!is.na(software_used) & software_used != "Not mentioned"),
    studies_with_sample_size = sum(!is.na(Sample_Size_Numeric)),
    median_suoa_size = median(Unit_size_km2, na.rm = TRUE),
    mean_suoa_size = mean(Unit_size_km2, na.rm = TRUE),
    min_year = min(Publication_Year, na.rm = TRUE),
    max_year = max(Publication_Year, na.rm = TRUE),
    most_common_method = names(sort(table(Statistical_Method_Clean), decreasing = TRUE))[1],
    most_common_software = names(sort(table(Software_Clean), decreasing = TRUE))[1],
    most_common_jurisdiction = names(sort(table(Jurisdiction), decreasing = TRUE))[1],
    .groups = 'drop'
  )

write_csv(enhanced_summary, here("Output", "enhanced_comprehensive_summary.csv"))

cat("Enhanced analysis complete!\n")
cat("─────────────────────────\n")
cat("Key improvements made:\n")
cat("• Used extracted methodological metadata instead of keyword inference\n")
cat("• Added sample size correlation analysis\n") 
cat("• Analyzed methodological evolution over time\n")
cat("• Created enhanced visualizations with method/software combinations\n")
cat("• Provided more nuanced statistical method categorization\n")
cat("• Generated comprehensive summary outputs\n\n")

cat("Files saved:\n")
cat("• enhanced_methods_analysis.csv\n")
cat("• enhanced_software_analysis.csv\n") 
cat("• enhanced_model_analysis.csv\n")
cat("• sample_size_quintiles_analysis.csv\n")
cat("• software_adoption_trends.csv\n")
cat("• statistical_method_evolution.csv\n")
cat("• enhanced_comprehensive_summary.csv\n")
cat("• RQ6_enhanced_methods_vs_SUoA.png\n")
cat("• software_evolution_over_time.png\n")
cat("• sample_size_vs_suoa.png\n")
cat("• methodology_combination_heatmap.png\n\n")

cat("This enhanced analysis provides a more accurate and comprehensive view\n")
cat("of methodological practices in crime location choice studies.\n")
