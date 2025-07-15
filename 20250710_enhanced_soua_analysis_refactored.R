# Size of Unit Analysis (SoUA) - Refactored & Parameterised
# Purpose: Statistical analysis for systematic review of spatial units in crime location choice studies
# Date: 2025-07-12
# Author: Refactored analysis with modular functions and parameterised paths

# =============================================================================
# CONFIGURATION & PARAMETERS
# =============================================================================

# Core parameters.
run_date <- Sys.Date()
base_dir <- here::here()
data_filename <- "20250713_analysis_ready_dataset_trimmed.csv"
data_subdir <- "data"
output_subdir <- paste0(format(run_date, "%Y%m%d"), "_Analysis & Results")

# Data paths - separate input and output directories
data_dir <- "data"  # Raw data folder (stable)
results_dir <- "results"  # Results output folder

# Plot constants
histogram_bins <- 25
density_multiplier <- 0.4
annotation_height_factor <- 0.9

# Size category breakpoints.
size_breaks <- c(0, 0.001, 0.01, 0.1, 1, 10, Inf)
size_labels <- c(
  "Micro (< 0.001 km²)",
  "Very Small (0.001-0.01 km²)",
  "Small (0.01-0.1 km²)",
  "Medium (0.1-1 km²)",
  "Large (1-10 km²)",
  "Very Large (≥10 km²)"
)

# Country classification.
anglo_saxon_countries <- c("United States", "United Kingdom", "Canada", "Australia")

# Visualization parameters.
grey_palette <- c("#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525", "#1A1A1A", "#000000")
plot_width <- 12
plot_height <- 8
plot_dpi <- 300

# Analysis thresholds.
min_sample_size <- 30
min_jurisdictions <- 5
large_sample_threshold <- 1000

# =============================================================================
# PACKAGE LOADING
# =============================================================================

load_packages <- function() {
  # Set CRAN mirror if not already set.
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }
  
  # List of required packages.
  required_packages <- c(
    "tidyverse", "dplyr", "ggplot2", "readr", "lme4", "performance", 
    "broom", "gridExtra", "scales", "kableExtra", "stringr", "e1071", 
    "here", "writexl", "purrr", "magrittr", "tibble", "tidyr"
  )
  
  # Install missing packages without loading them.
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, quiet = TRUE)
  }
  
  # Only attach packages that need it (for %>% operator, etc.)
  # Everything else will use namespace::function() pattern
  library(magrittr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Create unified grey theme for all plots.
create_grey_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      text = ggplot2::element_text(color = "grey20"),
      axis.text = ggplot2::element_text(color = "grey40"),
      plot.title = ggplot2::element_text(color = "grey10", face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "grey30", size = 11),
      axis.title = ggplot2::element_text(color = "grey20", face = "bold"),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.title = ggplot2::element_text(face = "bold", color = "grey20"),
      strip.text = ggplot2::element_text(face = "bold", color = "grey20"),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0, color = "grey50"),
      panel.grid.minor = ggplot2::element_line(linewidth = 0.1, color = "grey95"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "grey90")
    )
}

# Create size categories as a separate function.
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

# Load and prepare the main dataset (simplified for standardized data).
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
  
  # Check required columns
  required_cols <- c("Unit_size_km2")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required columns missing: ", paste(missing_cols, collapse = ", "))
  }
  
  # Map and calculate required fields
  data <- data %>%
    dplyr::mutate(
      Unit_Type = Name_of_the_unit,  # Use Name_of_the_unit for unit type
      Log_Unit_size = log10(Unit_size_km2),
      Anglo_Saxon = ifelse(Country %in% anglo_saxon_countries, "Anglo-Saxon", "Other"),
      Publication_Year = Year,
      Unit_Type = Name_of_the_unit,
      Research_Sophistication = 1,  # Default value since we don't have this
      Variable_Complexity = "Medium",  # Default value
      Total_Variables_Numeric = Total_Variables,
      Variable_Diversity_Score = 1,  # Default value
      Data_Limitation_Score = 1,  # Default value
      Data_Limitation_Category = "Standard",  # Default value
      Jurisdiction = Country  # Use Country as Jurisdiction
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

# Generate summary statistics.
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
      Skewness = round(e1071::skewness(Unit_size_km2, na.rm = TRUE), 3),
      Kurtosis = round(e1071::kurtosis(Unit_size_km2, na.rm = TRUE), 3)
    )
}

# Create distribution plot.
create_distribution_plot <- function(data) {
  # Pre-compute histogram for annotation.
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
    ggplot2::labs(
      y = "Frequency"
    ) +
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

# Create correlation matrix plot.
create_correlation_plot <- function(data) {
  # Select only available numeric variables that actually exist in the data
  available_vars <- intersect(c("Publication_Year", "Unit_size_km2", 
                               "Log_Unit_size", "Research_Sophistication"), 
                             names(data))
  
  if (length(available_vars) < 2) {
    # Create a simple message plot if insufficient variables
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "Insufficient numeric variables for correlation matrix", 
                         size = 6, color = "grey40") +
        ggplot2::labs(
          x = NULL, y = NULL
        ) +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::theme_void()
    )
  }
  
  # Select only available numeric variables
  numeric_vars <- data %>%
    dplyr::select(dplyr::all_of(available_vars)) %>%
    dplyr::filter(complete.cases(.))
  
  # Check if we have enough data for correlation matrix
  if (nrow(numeric_vars) < 3 || ncol(numeric_vars) < 2) {
    # Create a simple message plot if insufficient data
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "Insufficient complete observations for correlation matrix\n(Need at least 3 complete cases)", 
                         size = 6, color = "grey40") +
        ggplot2::labs(
          x = NULL, y = NULL
        ) +
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
    ggplot2::labs(
      x = NULL, y = NULL
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text = ggplot2::element_text(size = 9)
    ) +
    # Add legend explanation as annotation (positioned at top)
    ggplot2::annotate("text", 
                     x = ncol(numeric_vars)/2 + 0.5, y = ncol(numeric_vars) + 0.7,
                     label = "Correlation Matrix (Pearson)",
                     color = "grey20", size = 4, fontface = "bold", hjust = 0.5) +
    ggplot2::annotate("text", 
                     x = ncol(numeric_vars)/2 + 0.5, y = ncol(numeric_vars) + 0.4,
                     label = "Dark = negative | Light = positive",
                     color = "grey40", size = 3.2, hjust = 0.5)
}

# Create temporal trend plot.
create_temporal_plot <- function(data) {
  temporal_data <- data %>% 
    dplyr::filter(!is.na(Publication_Year) & Publication_Year >= 2003)
  
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
    # Add legend box (positioned in bottom left corner)
    ggplot2::annotate("rect", 
                     xmin = 2003, xmax = 2011,
                     ymin = -3.9, ymax = -2.4,
                     fill = "white", color = "grey30", alpha = 0.96, linewidth = 0.6) +
    # Legend title
    ggplot2::annotate("text", 
                     x = 2007, 
                     y = -2.6,
                     label = "Legend:", 
                     color = "grey10", size = 3.2, hjust = 0.5, fontface = "bold") +
    # Legend text with better formatting
    ggplot2::annotate("text", 
                     x = 2007, 
                     y = -2.9,
                     label = "——— Linear trend", 
                     color = "grey20", size = 2.8, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 2007, 
                     y = -3.2,
                     label = "- - - LOESS smoother", 
                     color = "grey30", size = 2.8, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 2007, 
                     y = -3.5,
                     label = "● Data points", 
                     color = "grey40", size = 2.8, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 2007, 
                     y = -3.8,
                     label = "□ Confidence interval", 
                     color = "grey50", size = 2.8, hjust = 0.5, fontface = "bold") +
    create_grey_theme()
}

# Create jurisdictional comparison plot.
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
    # Add compact legend box in bottom-left corner
    ggplot2::annotate("rect", 
                     xmin = 0.6, xmax = 1.35,
                     ymin = -3.9, ymax = -2.5,
                     fill = "white", color = "grey30", alpha = 0.96, linewidth = 0.6) +
    # Legend title
    ggplot2::annotate("text", 
                     x = 0.975, 
                     y = -2.7,
                     label = "Legend:", 
                     color = "grey10", size = 3.0, hjust = 0.5, fontface = "bold") +
    # Legend text - more compact and visible
    ggplot2::annotate("text", 
                     x = 0.975, 
                     y = -2.95,
                     label = "◆ Means", 
                     color = "grey20", size = 2.6, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 0.975, 
                     y = -3.2,
                     label = "□ Quartiles", 
                     color = "grey20", size = 2.6, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 0.975, 
                     y = -3.45,
                     label = "● Data points", 
                     color = "grey30", size = 2.6, hjust = 0.5, fontface = "bold") +
    ggplot2::annotate("text", 
                     x = 0.975, 
                     y = -3.7,
                     label = "Violin: density", 
                     color = "grey40", size = 2.6, hjust = 0.5, fontface = "bold")
}

# Safe coefficient extraction helper.
get_coef_safely <- function(fixed_effects, var_name, column = "Estimate") {
  # Handle flexible factor level names for Anglo_Saxon variable
  if (stringr::str_starts(var_name, "Anglo_Saxon")) {
    possible_names <- paste0("Anglo_Saxon", c("Other", "Anglo-Saxon"))
    var_name <- possible_names[possible_names %in% fixed_effects$Variable][1]
  }
  
  idx <- which(fixed_effects$Variable == var_name)
  if (length(idx) > 0 && !is.na(var_name)) {
    return(fixed_effects[idx, column])
  } else {
    return(NA)
  }
}

# Comprehensive multivariate analysis.
run_multivariate_analysis <- function(data) {
  multivariate_data <- data %>%
    dplyr::filter(!is.na(Log_Unit_size) & !is.na(Publication_Year) &
                 !is.na(Research_Sophistication) & !is.na(Jurisdiction) & !is.na(Anglo_Saxon)) %>%
    dplyr::mutate(
      Publication_Year_centered = Publication_Year - mean(Publication_Year, na.rm = TRUE),
      Research_Sophistication_centered = Research_Sophistication - mean(Research_Sophistication, na.rm = TRUE)
    )
  
  # Check if we have sufficient data for mixed-effects modeling.
  if (nrow(multivariate_data) >= min_sample_size && 
      dplyr::n_distinct(multivariate_data$Jurisdiction) >= min_jurisdictions) {
    
    # Fit comprehensive mixed-effects model focused on spatial unit size
    comprehensive_model <- lme4::lmer(
      Log_Unit_size ~ Anglo_Saxon + 
        Publication_Year_centered + Research_Sophistication_centered + 
        (1|Jurisdiction), 
      data = multivariate_data, 
      REML = FALSE
    )
    
    # Extract model components.
    comprehensive_summary <- summary(comprehensive_model)
    icc_result <- performance::icc(comprehensive_model)
    icc_value <- round(icc_result$ICC_adjusted * 100, 1)
    r2_result <- performance::r2(comprehensive_model)
    model_r2 <- round(r2_result$R2_marginal * 100, 1)
    conditional_r2 <- round(r2_result$R2_conditional * 100, 1)
    
    # Extract coefficients safely.
    fixed_effects <- data.frame(comprehensive_summary$coefficients)
    fixed_effects$Variable <- rownames(fixed_effects)
    
    # Calculate confidence intervals.
    ci_results <- suppressWarnings(confint(comprehensive_model, method = "Wald"))
    
    # Extract coefficients and calculate p-values.
    # area_coef <- get_coef_safely(fixed_effects, "Log_Total_area") # Removed
    anglo_coef <- get_coef_safely(fixed_effects, "Anglo_SaxonOther")
    year_coef <- get_coef_safely(fixed_effects, "Publication_Year_centered")
    sophistication_coef <- get_coef_safely(fixed_effects, "Research_Sophistication_centered")
    
    # Calculate approximate p-values from t-values.
    # area_t <- get_coef_safely(fixed_effects, "Log_Total_area", "t.value") # Removed
    anglo_t <- get_coef_safely(fixed_effects, "Anglo_SaxonOther", "t.value")
    year_t <- get_coef_safely(fixed_effects, "Publication_Year_centered", "t.value")
    sophistication_t <- get_coef_safely(fixed_effects, "Research_Sophistication_centered", "t.value")
    
    # area_p <- ifelse(is.na(area_t), NA, 2 * (1 - pnorm(abs(area_t)))) # Removed
    anglo_p <- ifelse(is.na(anglo_t), NA, 2 * (1 - pnorm(abs(anglo_t))))
    year_p <- ifelse(is.na(year_t), NA, 2 * (1 - pnorm(abs(year_t))))
    sophistication_p <- ifelse(is.na(sophistication_t), NA, 2 * (1 - pnorm(abs(sophistication_t))))
    
    # Create results table.
    comprehensive_results <- data.frame(
      Predictor = c("Country clustering (ICC)", "Anglo-Saxon vs Other", 
                    "Publication year", "Research sophistication"),
      Effect_Size_Beta = c(
        round(icc_value/100, 3),
        round(anglo_coef, 3),
        round(year_coef, 3),
        round(sophistication_coef, 3)
      ),
      p_value = c(
        "-",
        round(anglo_p, 3),
        round(year_p, 3),
        round(sophistication_p, 3)
      ),
      CI_95 = c(
        "-",
        paste0("[", round(ci_results["Anglo_SaxonOther", 1], 2), ", ", 
               round(ci_results["Anglo_SaxonOther", 2], 2), "]"),
        paste0("[", round(ci_results["Publication_Year_centered", 1], 2), ", ", 
               round(ci_results["Publication_Year_centered", 2], 2), "]"),
        paste0("[", round(ci_results["Research_Sophistication_centered", 1], 2), ", ", 
               round(ci_results["Research_Sophistication_centered", 2], 2), "]")
      ),
      Interpretation = c(
        "Structural effect",
        ifelse(anglo_p > 0.05, "No difference", "Significant difference"),
        ifelse(year_p > 0.05, "No trend", "Temporal trend"),
        ifelse(sophistication_p > 0.05, "No effect", "Significant effect")
      ),
      stringsAsFactors = FALSE
    )
    
    return(list(
      results = comprehensive_results,
      success = TRUE,
      model_r2 = model_r2,
      icc_value = icc_value
    ))
    
  } else {
    return(list(
      results = NULL,
      success = FALSE,
      model_r2 = NA,
      icc_value = NA
    ))
  }
}

# =============================================================================
# UNIT SELECTION JUSTIFICATION ANALYSIS
# =============================================================================

# Create justification analysis plots.
create_justification_plots <- function(data, justification_analysis) {
  # Plot 1: Rationale categories
  rationale_plot <- ggplot2::ggplot(justification_analysis$by_category, 
                                   ggplot2::aes(x = reorder(Rationale_Category, n), y = n)) +
    ggplot2::geom_col(fill = "#2E86AB", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(n, " (", Percentage, "%)")), 
                      hjust = -0.1, size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Unit Selection Rationale Categories",
      subtitle = paste("Distribution across", sum(justification_analysis$by_category$n), "studies"),
      x = "Rationale Category",
      y = "Number of Studies"
    ) +
    create_grey_theme()
    
  # Plot 2: Justification by unit type  
  unit_type_plot <- ggplot2::ggplot(justification_analysis$by_unit_type,
                                   ggplot2::aes(x = reorder(Unit_Type, Avg_Unit_Size), y = Percent_Justified)) +
    ggplot2::geom_col(fill = "#A23B72", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Percent_Justified, "%")), 
                      hjust = -0.1, size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Unit Selection Justification by Unit Type",
      subtitle = "Percentage of studies with explicit justification",
      x = "Unit Type",
      y = "Percent with Justification"
    ) +
    create_grey_theme()
    
  # Plot 3: Variable complexity and justification
  complexity_plot <- ggplot2::ggplot(justification_analysis$by_complexity,
                                    ggplot2::aes(x = Variable_Complexity, y = Count)) +
    ggplot2::geom_col(fill = "#F18F01", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, size = 3.5) +
    ggplot2::labs(
      title = "Study Distribution by Variable Complexity",
      subtitle = "Number of studies by total variable count",
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
  
  # Justification by unit type - use Name_of_the_unit instead of Unit_Type
  justification_by_unit_type <- data %>%
    dplyr::group_by(Name_of_the_unit) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      Avg_Unit_Size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
      Percent_Justified = round(100 * mean(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Avg_Unit_Size)
  
  # Variable complexity and justification 
  # Create complexity categories based on Total_Variables
  complexity_justification <- data %>%
    dplyr::mutate(
      Variable_Complexity = dplyr::case_when(
        Total_Variables <= 5 ~ "Low",
        Total_Variables <= 10 ~ "Medium",
        TRUE ~ "High"
      )
    ) %>%
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

# Save plots with consistent parameters.
save_plot <- function(plot_obj, filename, output_dir, width = plot_width, height = plot_height) {
  ggplot2::ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = plot_dpi,
    bg = "white"
  )
}

main_analysis <- function() {
  # Load packages.
  load_packages()
  
  # Set theme globally.
  ggplot2::theme_set(create_grey_theme())
  
  # Setup paths - use yesterday's known working dataset
  data_path <- here::here("20250713_Analysis & Results", "20250713_standardized_unit_sizes_with_groups_merged.csv")
  output_dir <- here::here(output_subdir)
  
  print(paste("Data path:", data_path))
  print(paste("Output directory:", output_dir))
  
  # Create output directory with logging
  print(paste("Creating output directory:", output_dir))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print("Output directory created successfully")
  } else {
    print("Output directory already exists")
  }
  
  # Check if directory is writable
  if (!file.access(output_dir, mode = 2) == 0) {
    stop("Output directory is not writable: ", output_dir)
  }
  
  # Load and prepare data.
  print("Loading and preparing data...")
  data <- prep_data(data_path)
  print(paste("Loaded data with", nrow(data), "rows"))
  
  # Enhanced sanity checks.
  stopifnot("Dataset must not be empty" = nrow(data) > 0)
  stopifnot("Median unit size must be reasonable" = 
            median(data$Unit_size_km2, na.rm = TRUE) > 0.001 & 
            median(data$Unit_size_km2, na.rm = TRUE) < 100)
  stopifnot("Log unit sizes must be finite" = all(!is.infinite(data$Log_Unit_size)))
  
  # Generate summary statistics.
  print("Generating summary statistics...")
  summary_stats <- calculate_summary_stats(data)
  
  # Analyze unit selection justification.
  print("Analyzing unit selection justification...")
  justification_analysis <- analyze_unit_justification(data)
  
  # Create plots with error handling
  print("Creating plots...")
  tryCatch({
    plots <- list(
      distribution = create_distribution_plot(data),
      correlation = create_correlation_plot(data),
      temporal = create_temporal_plot(data),
      jurisdictional = create_jurisdictional_plot(data)
    )
    print("Base plots created successfully")
  }, error = function(e) {
    stop("Error creating base plots: ", e$message)
  })
  
  # Create justification plots with error handling
  print("Creating justification plots...")
  tryCatch({
    justification_plots <- create_justification_plots(data, justification_analysis)
    plots <- c(plots, justification_plots)
    print("Justification plots created successfully")
  }, error = function(e) {
    stop("Error creating justification plots: ", e$message)
  })
  
  # Save plots individually with error handling and logging
  print("Saving plots...")
  
  plot_files <- list(
    distribution = "distribution_analysis.png",
    correlation = "correlation_matrix.png",
    temporal = "temporal_analysis.png",
    jurisdictional = "jurisdictional_analysis.png",
    rationale = "unit_selection_rationale_categories.png",
    unit_type = "justification_by_unit_type.png",
    complexity = "variable_complexity_analysis.png"
  )
  
  for (plot_name in names(plots)) {
    plot_path <- file.path(output_dir, plot_files[[plot_name]])
    print(paste("Saving", plot_files[[plot_name]], "..."))
    tryCatch({
      ggplot2::ggsave(
        filename = plot_path,
        plot = plots[[plot_name]],
        width = plot_width,
        height = plot_height,
        dpi = plot_dpi,
        bg = "white"
      )
      if (file.exists(plot_path)) {
        print(paste("Successfully saved", plot_files[[plot_name]]))
      } else {
        warning(paste("File not created:", plot_files[[plot_name]]))
      }
    }, error = function(e) {
      warning(paste("Error saving", plot_files[[plot_name]], ":", e$message))
    })
  }
  
  # Run multivariate analysis.
  print("Running multivariate analysis...")
  multivariate_results <- run_multivariate_analysis(data)
  
  # Generate and save tables
  print("Generating and saving tables...")
  all_tables <- generate_analysis_tables(data, summary_stats, multivariate_results, justification_analysis)
  
  excel_filename <- file.path(output_dir, "Manuscript_All_Tables.xlsx")
  tryCatch({
    writexl::write_xlsx(all_tables, path = excel_filename)
    print("Excel file saved successfully")
  }, error = function(e) {
    warning("Error saving Excel file:", e$message)
  })
  
  print("Analysis complete!")
  
  return(list(
    data = data,
    summary_stats = summary_stats,
    multivariate_results = multivariate_results,
    tables = all_tables,
    plots = plots,
    justification_analysis = justification_analysis
  ))
}

# =============================================================================
# EXECUTE ANALYSIS
# =============================================================================

# Run the main analysis
if (!exists("TESTING_MODE")) {
  results <- main_analysis()
}

# Generate all analysis tables (simplified version).
generate_analysis_tables <- function(data, summary_stats, multivariate_results, justification_analysis) {
  # Table 1: Summary Statistics
  table1_summary_stats <- data.frame(
    Statistic = c("Studies analyzed", "Median unit size", "Mean unit size", 
                  "Smallest unit", "Largest unit", "Standard deviation", 
                  "Skewness (original)", "Orders of magnitude"),
    Value = c(
      as.character(nrow(data)),
      paste(round(median(data$Unit_size_km2, na.rm = TRUE), 1), "km²"),
      paste(round(mean(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
      paste(round(min(data$Unit_size_km2, na.rm = TRUE), 6), "km²"),
      paste(round(max(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
      paste(round(sd(data$Unit_size_km2, na.rm = TRUE), 2), "km²"),
      as.character(round(e1071::skewness(data$Unit_size_km2, na.rm = TRUE), 2)),
      as.character(round(log10(max(data$Unit_size_km2, na.rm = TRUE) / min(data$Unit_size_km2, na.rm = TRUE)), 1))
    ),
    stringsAsFactors = FALSE
  )
  
  # Table 2: Unit Selection Justification Summary
  table2_justification <- justification_analysis$summary
  
  # Table 3: Justification by Category
  table3_rationale_categories <- justification_analysis$by_category
  
  # Table 4: Justification by Unit Type  
  table4_unit_type <- justification_analysis$by_unit_type
  
  # Table 5: Variable Complexity Analysis
  table5_complexity <- data %>%
    dplyr::group_by(Variable_Complexity) %>%
    dplyr::summarise(
      N_Studies = dplyr::n(),
      Mean_Variables = round(mean(Total_Variables_Numeric, na.rm = TRUE), 1),
      Mean_Diversity = round(mean(Variable_Diversity_Score, na.rm = TRUE), 1),
      Percent_Justified = round(100 * mean(Has_Unit_Justification, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  # Table 6: Data Limitations Analysis
  table6_limitations <- data %>%
    dplyr::group_by(Data_Limitation_Category) %>%
    dplyr::summarise(
      N_Studies = dplyr::n(),
      Mean_Limitation_Score = round(mean(Data_Limitation_Score, na.rm = TRUE), 1),
      .groups = "drop"
    )
  
  # Create list of all tables
  all_tables <- list(
    "Table1_Summary_Statistics" = table1_summary_stats,
    "Table2_Justification_Summary" = table2_justification,
    "Table3_Rationale_Categories" = table3_rationale_categories,
    "Table4_Justification_by_Unit_Type" = table4_unit_type,
    "Table5_Variable_Complexity" = table5_complexity,
    "Table6_Data_Limitations" = table6_limitations
  )
  
  # Add multivariate results if available
  if (multivariate_results$success && !is.null(multivariate_results$results)) {
    all_tables[["Table7_Model_Results"]] <- multivariate_results$results
  }
  
  return(all_tables)
}

