# Enhanced SUoA Analysis Using 20260106_working.csv
# This script implements the priority analyses from ENHANCED_ANALYSIS_PLAN_20260106.md
# Created: 2026-01-06

# Setup ----
library(tidyverse)
library(readr)
library(here)
library(lme4)        # For mixed-effects models
library(ggplot2)
library(patchwork)   # For combining plots
library(scales)
library(knitr)
library(kableExtra)

# Create output folder
current_date <- format(Sys.Date(), "%Y%m%d")
output_folder <- here(paste0(current_date, "_Enhanced_Analysis"))
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Helper function to save outputs
save_output <- function(data, filename, subfolder = NULL) {
  path <- if (!is.null(subfolder)) {
    dir.create(file.path(output_folder, subfolder), showWarnings = FALSE, recursive = TRUE)
    file.path(output_folder, subfolder, paste0(current_date, "_", filename))
  } else {
    file.path(output_folder, paste0(current_date, "_", filename))
  }
  write_csv(data, path)
  cat("✓ Saved:", filename, "\n")
}

# Save plot function
save_plot <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  path <- file.path(output_folder, paste0(current_date, "_", filename))
  ggsave(path, plot, width = width, height = height, dpi = dpi)
  cat("✓ Saved plot:", filename, "\n")
}

# Data Loading ----
cat("Loading data from Data/20260106_working.csv...\n")
data_raw <- read_csv(here("Data", "20260106_working.csv"), 
                     show_col_types = FALSE,
                     locale = locale(encoding = "UTF-8"))

cat("  - Loaded", nrow(data_raw), "studies\n")
cat("  - Found", ncol(data_raw), "columns\n\n")

# Data Preparation ----
cat("Preparing data...\n")

data <- data_raw %>%
  mutate(
    # Parse unit size - handle different formats
    Unit_Size_numeric = case_when(
      # If already numeric
      !is.na(as.numeric(Unit_Size)) ~ as.numeric(Unit_Size),
      # Extract first number if contains text
      str_detect(Unit_Size, "\\d+") ~ as.numeric(str_extract(Unit_Size, "\\d+\\.?\\d*")),
      TRUE ~ NA_real_
    ),
    
    # Detect unit type from Unit_Size column
    Unit_Type_detected = case_when(
      str_detect(tolower(Unit_Size), "km") ~ "km2",
      str_detect(tolower(Unit_Size), "m2|m²|meter") ~ "m2",
      str_detect(tolower(Unit_Size), "x") ~ "m2",  # grid cells like "200x200"
      TRUE ~ "km2"  # default assumption
    ),
    
    # Convert to km²
    Unit_Size_km2 = case_when(
      Unit_Type_detected == "m2" ~ Unit_Size_numeric / 1e6,
      Unit_Type_detected == "km2" ~ Unit_Size_numeric,
      TRUE ~ NA_real_
    ),
    
    # Log transform for analysis
    Unit_Size_log10 = log10(Unit_Size_km2 + 1e-10),
    
    # Ensure Year is numeric
    Year_numeric = as.numeric(Year),
    
    # Size categories
    Size_Category = cut(
      Unit_Size_km2,
      breaks = c(-Inf, 0.01, 0.25, 1.0, 3.0, Inf),
      labels = c("Very Small (<0.01 km²)", "Small (0.01-<0.25 km²)", 
                 "Medium (0.25-<1.0 km²)", "Large (1.0-<3.0 km²)", 
                 "Very Large (≥3.0 km²)"),
      right = FALSE
    ),
    
    # Time periods
    Time_Period = cut(
      Year_numeric,
      breaks = c(2000, 2010, 2020, 2026),
      labels = c("2000-2010", "2011-2020", "2021-2025"),
      right = TRUE
    ),
    
    # Country standardization
    Country_clean = case_when(
      str_detect(tolower(Country), "united states|usa|us") ~ "United States",
      str_detect(tolower(Country), "united kingdom|uk|britain") ~ "United Kingdom",
      str_detect(tolower(Country), "china") ~ "China",
      str_detect(tolower(Country), "netherlands|dutch") ~ "Netherlands",
      str_detect(tolower(Country), "australia") ~ "Australia",
      str_detect(tolower(Country), "belgium") ~ "Belgium",
      str_detect(tolower(Country), "new zealand") ~ "New Zealand",
      str_detect(tolower(Country), "canada") ~ "Canada",
      str_detect(tolower(Country), "japan") ~ "Japan",
      TRUE ~ Country
    ),
    
    # Crime type grouping
    Crime_Type_Group_clean = case_when(
      str_detect(tolower(Crime_Type_Group), "burglary|breaking") ~ "Burglary",
      str_detect(tolower(Crime_Type_Group), "robbery") ~ "Robbery",
      str_detect(tolower(Crime_Type_Group), "theft") ~ "Theft",
      str_detect(tolower(Crime_Type_Group), "assault|violence") ~ "Violence",
      str_detect(tolower(Crime_Type_Group), "single") ~ "Single crime type",
      TRUE ~ "Other"
    ),
    
    # === NEW METHODOLOGICAL VARIABLES ===
    
    # MAUP Discussion (binary and categorical)
    MAUP_binary = case_when(
      is.na(MAUP_Discussion) ~ 0,
      str_detect(tolower(MAUP_Discussion), "no maup|not present|doesn't|does not") ~ 0,
      str_detect(tolower(MAUP_Discussion), "yes|discuss|mention|address|highlight") ~ 1,
      TRUE ~ 0
    ),
    
    MAUP_Quality = case_when(
      MAUP_binary == 0 ~ "None",
      str_detect(tolower(MAUP_Discussion), "brief|mention") ~ "Brief",
      str_detect(tolower(MAUP_Discussion), "discuss|explain|justify") ~ "Moderate",
      str_detect(tolower(MAUP_Discussion), "detail|extensive|thorough") ~ "Detailed",
      TRUE ~ "Brief"
    ),
    
    # Sensitivity Analysis
    Sensitivity_binary = case_when(
      is.na(Sensitivity_Analysis) ~ 0,
      str_detect(tolower(Sensitivity_Analysis), "no |not |doesn't") ~ 0,
      str_detect(tolower(Sensitivity_Analysis), "yes|perform|check|robust|re-estimat") ~ 1,
      TRUE ~ 0
    ),
    
    # Data Limitations
    Has_Data_Limitations = !is.na(Data_Limitations) & 
                          Data_Limitations != "" &
                          !str_detect(tolower(Data_Limitations), "not mentioned|no limitation"),
    
    # Computational Constraints
    Has_Computational_Constraints = `Computational constraints (Yes/No)` == "Yes",
    
    # Data availability constraint
    Smallest_Unit_Data_Availability = `Smallest unit due to data availability (Yes/No)` == "Yes",
    
    # Alternative units mentioned
    Has_Alternative_Units = !is.na(Alternative_Units) & 
                           Alternative_Units != "" &
                           Alternative_Units != "No alternative units discussed.",
    
    # Number of variables
    N_Variables = as.numeric(`Number_of_Variables`),
    
    # Statistical reporting quality
    Has_Model_Fit = !is.na(Model_Fit_Statistics) & Model_Fit_Statistics != "",
    Has_Coefficients = !is.na(Coefficients) & Coefficients != "",
    Has_CI = !is.na(Confidence_Intervals) & Confidence_Intervals != "" &
             !str_detect(tolower(Confidence_Intervals), "not mentioned"),
    Has_Effect_Sizes = !is.na(Effect_Sizes) & Effect_Sizes != "",
    
    # === METHODOLOGICAL SOPHISTICATION INDEX (0-5) ===
    Sophistication_Index = (MAUP_binary + Sensitivity_binary + Has_CI + 
                           Has_Effect_Sizes + Has_Model_Fit),
    
    # Rationale categories
    Rationale_Main = Rationale_Category,
    
    # Data limitation types (content analysis)
    Limit_Privacy = str_detect(tolower(Data_Limitations), "privacy|confidential|protect|gdpr"),
    Limit_Computational = Has_Computational_Constraints,
    Limit_Availability = str_detect(tolower(Data_Limitations), "available|access|obtain|lack"),
    Limit_Aggregation = str_detect(tolower(Data_Limitations), "aggregat|resolution|granular|fine"),
    
    N_Limitation_Types = Limit_Privacy + Limit_Computational + 
                        Limit_Availability + Limit_Aggregation
  ) %>%
  # Filter to complete cases for core variables
  filter(!is.na(Unit_Size_km2), !is.na(Year_numeric), !is.na(Country_clean))

cat("  - Cleaned data:", nrow(data), "studies with complete SUoA information\n")
cat("  - Year range:", min(data$Year_numeric, na.rm = TRUE), "-", 
    max(data$Year_numeric, na.rm = TRUE), "\n")
cat("  - Countries:", n_distinct(data$Country_clean), "\n\n")

# Summary statistics ----
summary_stats <- data %>%
  summarise(
    N_Studies = n(),
    N_Countries = n_distinct(Country_clean),
    Year_Min = min(Year_numeric, na.rm = TRUE),
    Year_Max = max(Year_numeric, na.rm = TRUE),
    
    # SUoA statistics
    Median_Size_km2 = median(Unit_Size_km2, na.rm = TRUE),
    Mean_Size_km2 = mean(Unit_Size_km2, na.rm = TRUE),
    SD_Size_km2 = sd(Unit_Size_km2, na.rm = TRUE),
    Min_Size_km2 = min(Unit_Size_km2, na.rm = TRUE),
    Max_Size_km2 = max(Unit_Size_km2, na.rm = TRUE),
    
    # Methodological sophistication
    Pct_MAUP_Discussion = mean(MAUP_binary, na.rm = TRUE) * 100,
    Pct_Sensitivity_Analysis = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    Pct_Has_CI = mean(Has_CI, na.rm = TRUE) * 100,
    Pct_Has_Effect_Sizes = mean(Has_Effect_Sizes, na.rm = TRUE) * 100,
    Mean_Sophistication_Index = mean(Sophistication_Index, na.rm = TRUE),
    
    # Data limitations
    Pct_Data_Limitations = mean(Has_Data_Limitations, na.rm = TRUE) * 100,
    Pct_Computational_Constraints = mean(Has_Computational_Constraints, na.rm = TRUE) * 100,
    Pct_Alternative_Units = mean(Has_Alternative_Units, na.rm = TRUE) * 100,
    
    # Variables
    Mean_N_Variables = mean(N_Variables, na.rm = TRUE),
    Median_N_Variables = median(N_Variables, na.rm = TRUE)
  )

save_output(summary_stats, "summary_statistics.csv")

# ============================================================================
# ANALYSIS 1: METHODOLOGICAL MATURITY OVER TIME
# ============================================================================
cat("\n=== ANALYSIS 1: Methodological Maturity Over Time ===\n")

# 1.1 Temporal trends in methodological practices
temporal_methods <- data %>%
  group_by(Time_Period) %>%
  summarise(
    N = n(),
    Pct_MAUP = mean(MAUP_binary, na.rm = TRUE) * 100,
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    Pct_CI = mean(Has_CI, na.rm = TRUE) * 100,
    Pct_Effect_Sizes = mean(Has_Effect_Sizes, na.rm = TRUE) * 100,
    Mean_Sophistication = mean(Sophistication_Index, na.rm = TRUE),
    Median_N_Variables = median(N_Variables, na.rm = TRUE),
    Mean_N_Variables = mean(N_Variables, na.rm = TRUE),
    .groups = "drop"
  )

save_output(temporal_methods, "temporal_methodological_practices.csv")

# 1.2 Year-by-year trends
yearly_trends <- data %>%
  group_by(Year_numeric) %>%
  summarise(
    N = n(),
    Pct_MAUP = mean(MAUP_binary, na.rm = TRUE) * 100,
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    Mean_Sophistication = mean(Sophistication_Index, na.rm = TRUE),
    Median_Size_km2 = median(Unit_Size_km2, na.rm = TRUE),
    .groups = "drop"
  )

save_output(yearly_trends, "yearly_methodological_trends.csv")

# 1.3 Statistical test for temporal trends
temporal_model <- lm(Sophistication_Index ~ Year_numeric, data = data)
cat("\nTemporal trend in Sophistication Index:\n")
print(summary(temporal_model))

# ============================================================================
# ANALYSIS 2: MAUP AWARENESS AND SUoA SELECTION
# ============================================================================
cat("\n=== ANALYSIS 2: MAUP Awareness and SUoA Selection ===\n")

# 2.1 SUoA size by MAUP discussion status
maup_size_comparison <- data %>%
  group_by(MAUP_binary) %>%
  summarise(
    N = n(),
    Mean_Size_km2 = mean(Unit_Size_km2, na.rm = TRUE),
    Median_Size_km2 = median(Unit_Size_km2, na.rm = TRUE),
    SD_Size_km2 = sd(Unit_Size_km2, na.rm = TRUE),
    Min_Size_km2 = min(Unit_Size_km2, na.rm = TRUE),
    Max_Size_km2 = max(Unit_Size_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(MAUP_Discussion = ifelse(MAUP_binary == 1, "Discusses MAUP", "No MAUP Discussion"))

save_output(maup_size_comparison, "maup_discussion_by_size.csv")

# 2.2 T-test for size difference
maup_yes <- data %>% filter(MAUP_binary == 1) %>% pull(Unit_Size_log10)
maup_no <- data %>% filter(MAUP_binary == 0) %>% pull(Unit_Size_log10)
maup_ttest <- t.test(maup_yes, maup_no)

cat("\nT-test: SUoA size by MAUP discussion\n")
cat("  MAUP Yes: Mean log10(km²) =", mean(maup_yes, na.rm = TRUE), "\n")
cat("  MAUP No:  Mean log10(km²) =", mean(maup_no, na.rm = TRUE), "\n")
cat("  p-value =", maup_ttest$p.value, "\n")
cat("  Cohen's d =", (mean(maup_yes, na.rm = TRUE) - mean(maup_no, na.rm = TRUE)) / 
    sd(c(maup_yes, maup_no), na.rm = TRUE), "\n")

# 2.3 MAUP discussion by country
maup_by_country <- data %>%
  group_by(Country_clean) %>%
  summarise(
    N = n(),
    N_MAUP = sum(MAUP_binary, na.rm = TRUE),
    Pct_MAUP = mean(MAUP_binary, na.rm = TRUE) * 100,
    N_Sensitivity = sum(Sensitivity_binary, na.rm = TRUE),
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(N >= 3) %>%  # Only countries with 3+ studies
  arrange(desc(Pct_MAUP))

save_output(maup_by_country, "maup_discussion_by_country.csv")

# 2.4 MAUP and alternative units
maup_alternatives <- data %>%
  group_by(MAUP_binary, Has_Alternative_Units) %>%
  summarise(N = n(), .groups = "drop") %>%
  mutate(
    MAUP = ifelse(MAUP_binary == 1, "MAUP Discussed", "No MAUP"),
    Alternatives = ifelse(Has_Alternative_Units, "Has Alternatives", "No Alternatives")
  )

save_output(maup_alternatives, "maup_and_alternative_units.csv")

# ============================================================================
# ANALYSIS 3: DATA LIMITATIONS AND SPATIAL SCALE
# ============================================================================
cat("\n=== ANALYSIS 3: Data Limitations and Spatial Scale ===\n")

# 3.1 Limitation types frequency
limitation_freq <- data %>%
  summarise(
    N_Total = n(),
    N_Any_Limitation = sum(Has_Data_Limitations, na.rm = TRUE),
    Pct_Any_Limitation = mean(Has_Data_Limitations, na.rm = TRUE) * 100,
    N_Privacy = sum(Limit_Privacy, na.rm = TRUE),
    Pct_Privacy = mean(Limit_Privacy, na.rm = TRUE) * 100,
    N_Computational = sum(Limit_Computational, na.rm = TRUE),
    Pct_Computational = mean(Limit_Computational, na.rm = TRUE) * 100,
    N_Availability = sum(Limit_Availability, na.rm = TRUE),
    Pct_Availability = mean(Limit_Availability, na.rm = TRUE) * 100,
    N_Aggregation = sum(Limit_Aggregation, na.rm = TRUE),
    Pct_Aggregation = mean(Limit_Aggregation, na.rm = TRUE) * 100
  )

save_output(limitation_freq, "data_limitation_frequencies.csv")

# 3.2 SUoA size by limitation type
size_by_limitation <- data %>%
  pivot_longer(
    cols = c(Limit_Privacy, Limit_Computational, Limit_Availability, Limit_Aggregation),
    names_to = "Limitation_Type",
    values_to = "Has_Limitation"
  ) %>%
  filter(Has_Limitation) %>%
  group_by(Limitation_Type) %>%
  summarise(
    N = n(),
    Mean_Size_km2 = mean(Unit_Size_km2, na.rm = TRUE),
    Median_Size_km2 = median(Unit_Size_km2, na.rm = TRUE),
    SD_Size_km2 = sd(Unit_Size_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Limitation_Type = str_replace(Limitation_Type, "Limit_", ""))

save_output(size_by_limitation, "size_by_limitation_type.csv")

# 3.3 Computational constraints over time
computational_over_time <- data %>%
  group_by(Time_Period) %>%
  summarise(
    N = n(),
    N_Computational = sum(Has_Computational_Constraints, na.rm = TRUE),
    Pct_Computational = mean(Has_Computational_Constraints, na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_output(computational_over_time, "computational_constraints_over_time.csv")

# 3.4 Limitation types by country
limitations_by_country <- data %>%
  filter(Country_clean %in% c("Netherlands", "United States", "China", "United Kingdom", "Belgium")) %>%
  group_by(Country_clean) %>%
  summarise(
    N = n(),
    Pct_Privacy = mean(Limit_Privacy, na.rm = TRUE) * 100,
    Pct_Computational = mean(Limit_Computational, na.rm = TRUE) * 100,
    Pct_Availability = mean(Limit_Availability, na.rm = TRUE) * 100,
    Pct_Aggregation = mean(Limit_Aggregation, na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_output(limitations_by_country, "limitations_by_country.csv")

# ============================================================================
# ANALYSIS 6: SENSITIVITY ANALYSIS PRACTICES
# ============================================================================
cat("\n=== ANALYSIS 6: Sensitivity Analysis Practices ===\n")

# 6.1 Sensitivity analysis frequency
sensitivity_summary <- data %>%
  summarise(
    N_Total = n(),
    N_Sensitivity = sum(Sensitivity_binary, na.rm = TRUE),
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100
  )

save_output(sensitivity_summary, "sensitivity_analysis_summary.csv")

# 6.2 Sensitivity by time period
sensitivity_temporal <- data %>%
  group_by(Time_Period) %>%
  summarise(
    N = n(),
    N_Sensitivity = sum(Sensitivity_binary, na.rm = TRUE),
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_output(sensitivity_temporal, "sensitivity_by_time_period.csv")

# 6.3 Sensitivity and MAUP together
sensitivity_maup <- data %>%
  group_by(MAUP_binary, Sensitivity_binary) %>%
  summarise(N = n(), .groups = "drop") %>%
  mutate(
    MAUP = ifelse(MAUP_binary == 1, "MAUP Discussed", "No MAUP"),
    Sensitivity = ifelse(Sensitivity_binary == 1, "Has Sensitivity", "No Sensitivity")
  )

save_output(sensitivity_maup, "sensitivity_and_maup_cross_tab.csv")

# 6.4 Sensitivity by size category
sensitivity_by_size <- data %>%
  group_by(Size_Category) %>%
  summarise(
    N = n(),
    N_Sensitivity = sum(Sensitivity_binary, na.rm = TRUE),
    Pct_Sensitivity = mean(Sensitivity_binary, na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_output(sensitivity_by_size, "sensitivity_by_size_category.csv")

# ============================================================================
# ENHANCED RATIONALE ANALYSIS (RQ5)
# ============================================================================
cat("\n=== ENHANCED RATIONALE ANALYSIS ===\n")

# Rationale with limitation cross-tab
rationale_limitation <- data %>%
  filter(!is.na(Rationale_Main)) %>%
  group_by(Rationale_Main) %>%
  summarise(
    N = n(),
    Pct_Has_Limitations = mean(Has_Data_Limitations, na.rm = TRUE) * 100,
    Pct_Privacy = mean(Limit_Privacy, na.rm = TRUE) * 100,
    Pct_Computational = mean(Limit_Computational, na.rm = TRUE) * 100,
    Pct_Availability = mean(Limit_Availability, na.rm = TRUE) * 100,
    Mean_Size_km2 = mean(Unit_Size_km2, na.rm = TRUE),
    Median_Size_km2 = median(Unit_Size_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(N))

save_output(rationale_limitation, "rationale_with_limitations.csv")

# ============================================================================
# VISUALIZATION SECTION
# ============================================================================
cat("\n=== Creating Visualizations ===\n")

# Theme setup
theme_set(theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11),
                legend.position = "bottom"))

# Figure 1: Methodological Evolution Dashboard (4-panel)
p1_size <- ggplot(yearly_trends, aes(x = Year_numeric, y = Median_Size_km2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_y_log10() +
  labs(title = "A) SUoA Size Over Time",
       x = "Year", y = "Median Size (km², log scale)") +
  theme_minimal()

p2_maup <- ggplot(yearly_trends, aes(x = Year_numeric, y = Pct_MAUP)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(title = "B) MAUP Discussion Over Time",
       x = "Year", y = "% Studies Discussing MAUP") +
  theme_minimal()

p3_sensitivity <- ggplot(yearly_trends, aes(x = Year_numeric, y = Pct_Sensitivity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  labs(title = "C) Sensitivity Analysis Over Time",
       x = "Year", y = "% Studies Performing Sensitivity") +
  theme_minimal()

p4_sophistication <- ggplot(yearly_trends, aes(x = Year_numeric, y = Mean_Sophistication)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  ylim(0, 5) +
  labs(title = "D) Methodological Sophistication Over Time",
       x = "Year", y = "Mean Sophistication Index (0-5)") +
  theme_minimal()

fig1_evolution <- (p1_size | p2_maup) / (p3_sensitivity | p4_sophistication) +
  plot_annotation(
    title = "Figure 1: Methodological Evolution in Crime Location Choice Research",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

save_plot(fig1_evolution, "Fig1_Methodological_Evolution_Dashboard.png", 
          width = 14, height = 10)

# Figure 2: MAUP Impact
fig2_maup_size <- data %>%
  filter(!is.na(MAUP_binary)) %>%
  mutate(MAUP_Status = ifelse(MAUP_binary == 1, "Discusses MAUP", "No MAUP Discussion")) %>%
  ggplot(aes(x = MAUP_Status, y = Unit_Size_km2, fill = MAUP_Status)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  scale_fill_manual(values = c("Discusses MAUP" = "#2E7D32", "No MAUP Discussion" = "#C62828")) +
  labs(title = "Figure 2: SUoA Size by MAUP Discussion Status",
       subtitle = paste0("t-test p-value = ", round(maup_ttest$p.value, 4)),
       x = "MAUP Discussion Status",
       y = "Spatial Unit Size (km², log scale)") +
  theme_minimal() +
  theme(legend.position = "none")

save_plot(fig2_maup_size, "Fig2_MAUP_Impact_on_Size.png", width = 8, height = 6)

# Figure 3: Data Limitations Impact
fig3_limitations <- data %>%
  filter(Has_Data_Limitations | Has_Computational_Constraints | 
         Smallest_Unit_Data_Availability) %>%
  mutate(
    Limitation = case_when(
      Has_Computational_Constraints ~ "Computational",
      Limit_Privacy ~ "Privacy",
      Limit_Availability ~ "Data Availability",
      Limit_Aggregation ~ "Aggregation",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = reorder(Limitation, Unit_Size_km2, FUN = median), 
             y = Unit_Size_km2, fill = Limitation)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Figure 3: SUoA Size by Data Limitation Type",
       x = "Limitation Type",
       y = "Spatial Unit Size (km², log scale)") +
  theme_minimal() +
  theme(legend.position = "none")

save_plot(fig3_limitations, "Fig3_Limitations_Impact.png", width = 8, height = 6)

# Figure 4: Temporal trends by time period (bar chart)
fig4_temporal_methods <- temporal_methods %>%
  pivot_longer(cols = c(Pct_MAUP, Pct_Sensitivity, Pct_CI, Pct_Effect_Sizes),
               names_to = "Method", values_to = "Percentage") %>%
  mutate(Method = case_when(
    Method == "Pct_MAUP" ~ "MAUP Discussion",
    Method == "Pct_Sensitivity" ~ "Sensitivity Analysis",
    Method == "Pct_CI" ~ "Confidence Intervals",
    Method == "Pct_Effect_Sizes" ~ "Effect Sizes"
  )) %>%
  ggplot(aes(x = Time_Period, y = Percentage, fill = Method)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Figure 4: Methodological Practices by Time Period",
       x = "Time Period", y = "Percentage of Studies (%)",
       fill = "Methodological Practice") +
  theme_minimal() +
  theme(legend.position = "bottom")

save_plot(fig4_temporal_methods, "Fig4_Methods_By_Period.png", width = 10, height = 6)

# ============================================================================
# SUMMARY REPORT
# ============================================================================
cat("\n=== GENERATING SUMMARY REPORT ===\n")

report <- glue::glue("
ENHANCED SUoA ANALYSIS SUMMARY REPORT
Generated: {Sys.Date()}
Data file: Data/20260106_working.csv

================================================================================
DATASET OVERVIEW
================================================================================
Total studies analyzed: {nrow(data)}
Year range: {min(data$Year_numeric, na.rm=TRUE)} - {max(data$Year_numeric, na.rm=TRUE)}
Countries: {n_distinct(data$Country_clean)}
Crime types: {n_distinct(data$Crime_Type_Group_clean)}

SUoA Size Statistics:
  Median: {round(median(data$Unit_Size_km2, na.rm=TRUE), 4)} km²
  Mean: {round(mean(data$Unit_Size_km2, na.rm=TRUE), 4)} km²
  Range: {round(min(data$Unit_Size_km2, na.rm=TRUE), 6)} - {round(max(data$Unit_Size_km2, na.rm=TRUE), 2)} km²

================================================================================
KEY FINDINGS: METHODOLOGICAL SOPHISTICATION
================================================================================

MAUP Discussion:
  Studies discussing MAUP: {sum(data$MAUP_binary, na.rm=TRUE)} ({round(mean(data$MAUP_binary, na.rm=TRUE)*100, 1)}%)
  
Sensitivity Analysis:
  Studies performing sensitivity: {sum(data$Sensitivity_binary, na.rm=TRUE)} ({round(mean(data$Sensitivity_binary, na.rm=TRUE)*100, 1)}%)
  
Statistical Reporting:
  Confidence Intervals: {sum(data$Has_CI, na.rm=TRUE)} ({round(mean(data$Has_CI, na.rm=TRUE)*100, 1)}%)
  Effect Sizes: {sum(data$Has_Effect_Sizes, na.rm=TRUE)} ({round(mean(data$Has_Effect_Sizes, na.rm=TRUE)*100, 1)}%)
  Model Fit Statistics: {sum(data$Has_Model_Fit, na.rm=TRUE)} ({round(mean(data$Has_Model_Fit, na.rm=TRUE)*100, 1)}%)

Mean Sophistication Index: {round(mean(data$Sophistication_Index, na.rm=TRUE), 2)} / 5

================================================================================
KEY FINDINGS: DATA LIMITATIONS
================================================================================

Any Data Limitations: {sum(data$Has_Data_Limitations, na.rm=TRUE)} ({round(mean(data$Has_Data_Limitations, na.rm=TRUE)*100, 1)}%)

Limitation Types:
  Privacy: {sum(data$Limit_Privacy, na.rm=TRUE)} ({round(mean(data$Limit_Privacy, na.rm=TRUE)*100, 1)}%)
  Computational: {sum(data$Limit_Computational, na.rm=TRUE)} ({round(mean(data$Limit_Computational, na.rm=TRUE)*100, 1)}%)
  Data Availability: {sum(data$Limit_Availability, na.rm=TRUE)} ({round(mean(data$Limit_Availability, na.rm=TRUE)*100, 1)}%)
  Aggregation: {sum(data$Limit_Aggregation, na.rm=TRUE)} ({round(mean(data$Limit_Aggregation, na.rm=TRUE)*100, 1)}%)

Alternative Units Mentioned: {sum(data$Has_Alternative_Units, na.rm=TRUE)} ({round(mean(data$Has_Alternative_Units, na.rm=TRUE)*100, 1)}%)

================================================================================
KEY FINDINGS: TEMPORAL TRENDS
================================================================================

Temporal Trend in Sophistication Index:
  Coefficient: {round(coef(temporal_model)[2], 4)}
  P-value: {round(summary(temporal_model)$coefficients[2,4], 4)}
  R-squared: {round(summary(temporal_model)$r.squared, 4)}

MAUP Discussion by Period:
  2000-2010: {round(filter(temporal_methods, Time_Period=='2000-2010')$Pct_MAUP, 1)}%
  2011-2020: {round(filter(temporal_methods, Time_Period=='2011-2020')$Pct_MAUP, 1)}%
  2021-2025: {round(filter(temporal_methods, Time_Period=='2021-2025')$Pct_MAUP, 1)}%

Sensitivity Analysis by Period:
  2000-2010: {round(filter(temporal_methods, Time_Period=='2000-2010')$Pct_Sensitivity, 1)}%
  2011-2020: {round(filter(temporal_methods, Time_Period=='2011-2020')$Pct_Sensitivity, 1)}%
  2021-2025: {round(filter(temporal_methods, Time_Period=='2021-2025')$Pct_Sensitivity, 1)}%

================================================================================
MAUP AWARENESS ANALYSIS
================================================================================

SUoA Size Comparison:
  MAUP Discussed: Mean = {round(filter(maup_size_comparison, MAUP_binary==1)$Mean_Size_km2, 4)} km²
  No MAUP: Mean = {round(filter(maup_size_comparison, MAUP_binary==0)$Mean_Size_km2, 4)} km²
  
  T-test p-value: {round(maup_ttest$p.value, 4)}
  
MAUP Discussion & Sensitivity Analysis:
  Both: {nrow(filter(data, MAUP_binary==1 & Sensitivity_binary==1))} studies
  MAUP only: {nrow(filter(data, MAUP_binary==1 & Sensitivity_binary==0))} studies
  Sensitivity only: {nrow(filter(data, MAUP_binary==0 & Sensitivity_binary==1))} studies
  Neither: {nrow(filter(data, MAUP_binary==0 & Sensitivity_binary==0))} studies

================================================================================
OUTPUT FILES GENERATED
================================================================================

All files saved to: {output_folder}/

CSV Files:
  - summary_statistics.csv
  - temporal_methodological_practices.csv
  - yearly_methodological_trends.csv
  - maup_discussion_by_size.csv
  - maup_discussion_by_country.csv
  - maup_and_alternative_units.csv
  - data_limitation_frequencies.csv
  - size_by_limitation_type.csv
  - computational_constraints_over_time.csv
  - limitations_by_country.csv
  - sensitivity_analysis_summary.csv
  - sensitivity_by_time_period.csv
  - sensitivity_and_maup_cross_tab.csv
  - sensitivity_by_size_category.csv
  - rationale_with_limitations.csv

Figures:
  - Fig1_Methodological_Evolution_Dashboard.png
  - Fig2_MAUP_Impact_on_Size.png
  - Fig3_Limitations_Impact.png
  - Fig4_Methods_By_Period.png

================================================================================
NEXT STEPS
================================================================================

1. Review all output files in {output_folder}/
2. Examine figures for patterns and insights
3. Conduct follow-up analyses as needed
4. Integrate findings into manuscript
5. Consider additional analyses from ENHANCED_ANALYSIS_PLAN_20260106.md

================================================================================
")

writeLines(report, file.path(output_folder, paste0(current_date, "_ANALYSIS_REPORT.txt")))
cat(report)

cat("\n✓ Enhanced analysis complete!\n")
cat("  All outputs saved to:", output_folder, "\n")
