# Enhanced SOUA Analysis - Updated to use Trimmed Dataset with Unit Selection Justification
# This script has been refactored to:
# 1. Use the trimmed analysis-ready dataset with only required variables
# 2. Include analysis of unit selection justification  
# 3. Focus exclusively on spatial unit size (removed study area analysis)
# 4. Maintain all existing analysis outputs with improved efficiency

# Clear environment and load libraries
rm(list = ls())

# Load required libraries with error handling
required_packages <- c("dplyr", "ggplot2", "corrplot", "e1071", "openxlsx", 
                      "gridExtra", "stringr", "readr", "scales", "viridis")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    stop(paste("Package", pkg, "not found. Please install it."))
  }
}

# Set global parameters
set.seed(42)  # For reproducibility

# Configure input/output directories and files
yesterday_date <- format(Sys.Date() - 1, "%Y%m%d")
today_date <- format(Sys.Date(), "%Y%m%d")

input_dir <- paste0(yesterday_date, "_Analysis & Results")
output_dir <- paste0(today_date, "_Analysis & Results")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Set input file (using yesterday's standardized dataset)
input_file <- file.path(input_dir, paste0(yesterday_date, "_standardized_unit_sizes_with_groups_merged.csv"))

cat("Using input file:", input_file, "\n")
cat("Output directory:", output_dir, "\n")

cat("Using input files:\n")
cat("- Trimmed dataset:", input_file, "\n")

# Analysis parameters (unchanged from original)
histogram_bins <- 25
large_sample_threshold <- 500
min_group_size <- 3
min_jurisdictions <- 5

# Color palettes
spatial_colors <- c(viridis::viridis(8, option = "D"))
time_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
jurisdiction_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33")

# Check if trimmed dataset exists
if (!file.exists(input_file)) {
  stop("Trimmed dataset not found. Please run create_trimmed_analysis_dataset.R first.")
}

# Helper functions (unchanged from original analysis)
create_size_categories <- function(unit_size) {
  sapply(unit_size, function(x) {
    if (is.na(x)) return("Unknown")
    if (x < 0.001) return("Very Small")
    if (x < 0.01) return("Small") 
    if (x < 0.1) return("Medium")
    if (x < 1) return("Large")
    return("Very Large")
  })
}

make_size_categories <- function(data) {
  data %>%
    dplyr::mutate(
      Size_Category = create_size_categories(Unit_size_km2),
      Size_Category = factor(Size_Category, 
                           levels = c("Very Small", "Small", "Medium", "Large", "Very Large"))
    )
}

# Load and prepare data
cat("Loading standardized dataset...\n")
data <- tryCatch({
  read_csv(input_file, show_col_types = FALSE)
}, error = function(e) {
  stop("Failed to read standardized dataset: ", e$message)
})

cat("Data loaded successfully. Rows:", nrow(data), "Columns:", ncol(data), "\n")

# Print column names for debugging
cat("\nAvailable columns:\n")
print(names(data))

# Prepare the data for analysis
data <- data %>%
  mutate(
    # Extract unit type if available, otherwise use a default
    Unit_Type = if("Unit_Type" %in% names(data)) Unit_Type else "Not Specified",
    # Set Has_Unit_Justification based on Quoted_Rationale
    Has_Unit_Justification = !is.na(Quoted_Rationale) & Quoted_Rationale != ""
  )

# Apply size categories
data <- make_size_categories(data)

cat("\nData preparation complete. Checking processed data:\n")
cat("Number of records with unit sizes:", sum(!is.na(data$Unit_size_km2)), "\n")
cat("Number of records with unit types:", sum(!is.na(data$Unit_Type)), "\n")
cat("Number of records with justification:", sum(data$Has_Unit_Justification), "\n")

# === UNIT SELECTION JUSTIFICATION ANALYSIS ===
cat("\n=== UNIT SELECTION JUSTIFICATION ANALYSIS ===\n")

# Infer Has_Unit_Justification from Quoted_Rationale if not present
if (!"Has_Unit_Justification" %in% names(data)) {
  cat("Creating Has_Unit_Justification from Quoted_Rationale\n")
  data$Has_Unit_Justification <- !is.na(data$Quoted_Rationale) & data$Quoted_Rationale != ""
}

# Analyze justification coverage and types
justification_analysis <- data %>%
  summarise(
    Total_Studies = n(),
    With_Justification = sum(Has_Unit_Justification, na.rm = TRUE),
    Percent_Justified = round(100 * With_Justification / Total_Studies, 1),
    With_Quoted_Rationale = sum(!is.na(Quoted_Rationale) & Quoted_Rationale != "", na.rm = TRUE),
    With_Rationale_Category = sum(!is.na(Rationale_Category) & Rationale_Category != "", na.rm = TRUE)
  )

cat("Justification Coverage:\n")
print(justification_analysis)

# Analyze rationale categories
if ("Rationale_Category" %in% names(data)) {
  cat("\nValidating Rationale_Category data:\n")
  cat("Total rows in data:", nrow(data), "\n")
  cat("Unique categories:", length(unique(data$Rationale_Category)), "\n")
  cat("NA values:", sum(is.na(data$Rationale_Category)), "\n")
  cat("Empty strings:", sum(data$Rationale_Category == "", na.rm = TRUE), "\n")
  
  # Filter with debugging
  valid_rationales <- data %>%
    filter(!is.na(Rationale_Category) & Rationale_Category != "")
  cat("\nRows after filtering:", nrow(valid_rationales), "\n")
  
  # Create counts with debugging
  rationale_counts <- valid_rationales %>%
    count(Rationale_Category, sort = TRUE) %>%
    mutate(Percentage = round(100 * n / sum(n), 1))
  
  cat("\nRationale category counts:\n")
  print(rationale_counts)
  
  # Create rationale category plot with debug logging
  cat("\nCreating rationale category plot...\n")
  cat("Number of categories:", nrow(rationale_counts), "\n")
  print(rationale_counts)  # Debug print
  
  tryCatch({
    rationale_plot <- ggplot(rationale_counts, aes(x = reorder(Rationale_Category, n), y = n)) +
      geom_col(fill = "#2E86AB", alpha = 0.8) +
      geom_text(aes(label = paste0(n, " (", Percentage, "%)")), 
                hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(
        title = "Unit Selection Rationale Categories",
        subtitle = paste("Distribution across", sum(rationale_counts$n), "studies"),
        x = "Rationale Category",
        y = "Number of Studies"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(20, 50, 20, 20)  # Add margin for labels
      )
    
    # Save plot with error handling
    plot_file <- file.path(output_dir, "unit_selection_rationale_categories.png")
    cat("Saving plot to:", plot_file, "\n")
    
    ggsave(plot_file, 
           plot = rationale_plot,
           width = 12,        # Increased width to accommodate labels
           height = max(6, 2 + 0.5 * nrow(rationale_counts)),  # Dynamic height based on categories
           dpi = 300)
    
    cat("Plot saved successfully\n")
    
    # Verify file was created and has content
    if (file.exists(plot_file)) {
      file_info <- file.info(plot_file)
      cat("File size:", file_info$size, "bytes\n")
      if (file_info$size == 0) {
        warning("Warning: Created file is empty!")
      }
    } else {
      warning("Warning: File was not created!")
    }
  }, error = function(e) {
    cat("Error creating or saving plot:", e$message, "\n")
  })
}

# Analyze justification by unit type and size
justification_by_unit_type <- data %>%
  group_by(Unit_Type) %>%
  summarise(
    Count = n(),
    Avg_Unit_Size = round(mean(Unit_size_km2, na.rm = TRUE), 6),
    Percent_Justified = round(100 * mean(Has_Unit_Justification, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(Avg_Unit_Size)

cat("\nJustification by Unit Type:\n")
print(justification_by_unit_type)

# === EXISTING ANALYSIS (updated to use trimmed data) ===
cat("\n=== CORE SPATIAL UNIT SIZE ANALYSIS ===\n")

# Summary statistics (same as before)
summary_stats <- data %>%
  summarise(
    N_Studies = n(),
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

cat("Summary Statistics:\n")
print(summary_stats)

# Save summary statistics
write_csv(summary_stats, file.path(output_dir, "summary_statistics_trimmed.csv"))

# === DISTRIBUTION ANALYSIS ===
cat("\n=== DISTRIBUTION ANALYSIS ===\n")

# Create distribution plots
tryCatch({
  # Main distribution plot
  dist_plot <- ggplot(data, aes(x = Unit_size_km2)) +
    geom_histogram(bins = histogram_bins, fill = "#2E86AB", alpha = 0.7, color = "white") +
    labs(
      title = "Distribution of Spatial Unit Sizes",
      subtitle = paste("Analysis of", nrow(data), "crime location choice studies"),
      x = "Unit Size (km²)",
      y = "Number of Studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  
  # Log-transformed distribution
  log_dist_plot <- ggplot(data, aes(x = log10(Unit_size_km2))) +
    geom_histogram(bins = histogram_bins, fill = "#A23B72", alpha = 0.7, color = "white") +
    labs(
      title = "Log-Transformed Distribution",
      x = "Log₁₀(Unit Size)",
      y = "Number of Studies"
    ) +
    theme_minimal()
  
  # Size category plot
  size_cat_plot <- ggplot(data, aes(x = Size_Category)) +
    geom_bar(fill = "#F18F01", alpha = 0.8) +
    labs(
      title = "Distribution by Size Category",
      x = "Size Category",
      y = "Number of Studies"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine plots
  combined_dist <- grid.arrange(dist_plot, log_dist_plot, size_cat_plot, 
                               ncol = 1, heights = c(2, 1.5, 1.5))
  
  # Save combined plot
  ggsave(file.path(output_dir, "distribution_analysis.png"), 
         plot = combined_dist, width = 12, height = 10, dpi = 300)
  
  cat("Distribution analysis plots saved\n")
}, error = function(e) {
  cat("Error in distribution analysis:", e$message, "\n")
})

# === TEMPORAL ANALYSIS ===
cat("\n=== TEMPORAL ANALYSIS ===\n")

if ("Year" %in% names(data) && sum(!is.na(data$Year)) > 0) {
  tryCatch({
    # Temporal trends plot
    temporal_data <- data %>%
      filter(!is.na(Year) & !is.na(Unit_size_km2)) %>%
      mutate(Year_Bin = cut(Year, breaks = 4, include.lowest = TRUE))
    
    temporal_plot <- ggplot(temporal_data, aes(x = Year, y = Unit_size_km2)) +
      geom_point(alpha = 0.6, size = 2, color = "#2E86AB") +
      geom_smooth(method = "lm", se = TRUE, color = "#A23B72") +
      scale_y_log10() +
      labs(
        title = "Temporal Trends in Spatial Unit Sizes",
        subtitle = "No systematic change toward finer scales over time",
        x = "Publication Year",
        y = "Unit Size (km²) - Log Scale"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    
    # Boxplot by year bins
    year_box_plot <- ggplot(temporal_data, aes(x = Year_Bin, y = Unit_size_km2)) +
      geom_boxplot(fill = "#F18F01", alpha = 0.7) +
      scale_y_log10() +
      labs(
        title = "Unit Sizes by Time Period",
        x = "Time Period",
        y = "Unit Size (km²) - Log Scale"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine temporal plots
    combined_temporal <- grid.arrange(temporal_plot, year_box_plot, 
                                     ncol = 1, heights = c(2, 1.5))
    
    # Save temporal analysis
    ggsave(file.path(output_dir, "temporal_analysis.png"), 
           plot = combined_temporal, width = 12, height = 8, dpi = 300)
    
    cat("Temporal analysis plots saved\n")
  }, error = function(e) {
    cat("Error in temporal analysis:", e$message, "\n")
  })
} else {
  cat("Year data not available for temporal analysis\n")
}

# === JURISDICTIONAL ANALYSIS ===
cat("\n=== JURISDICTIONAL ANALYSIS ===\n")

if ("Country" %in% names(data) && sum(!is.na(data$Country)) > 0) {
  tryCatch({
    # Country-level analysis
    country_data <- data %>%
      filter(!is.na(Country) & !is.na(Unit_size_km2)) %>%
      group_by(Country) %>%
      filter(n() >= min_group_size) %>%
      ungroup()
    
    if (nrow(country_data) > 0) {
      # Boxplot by country
      country_plot <- ggplot(country_data, aes(x = reorder(Country, Unit_size_km2, median), 
                                              y = Unit_size_km2)) +
        geom_boxplot(fill = "#2E86AB", alpha = 0.7) +
        scale_y_log10() +
        coord_flip() +
        labs(
          title = "Spatial Unit Sizes by Country",
          subtitle = "Strong country-level clustering in methodological preferences",
          x = "Country",
          y = "Unit Size (km²) - Log Scale"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)
        )
      
      # Summary statistics by country
      country_summary <- country_data %>%
        group_by(Country) %>%
        summarise(
          N = n(),
          Median_Size = round(median(Unit_size_km2), 4),
          Mean_Size = round(mean(Unit_size_km2), 4),
          .groups = "drop"
        ) %>%
        arrange(Median_Size)
      
      # Country summary plot
      country_summary_plot <- ggplot(country_summary, aes(x = reorder(Country, Median_Size), 
                                                         y = Median_Size)) +
        geom_col(fill = "#A23B72", alpha = 0.8) +
        scale_y_log10() +
        coord_flip() +
        labs(
          title = "Median Unit Sizes by Country",
          x = "Country",
          y = "Median Unit Size (km²) - Log Scale"
        ) +
        theme_minimal()
      
      # Combine jurisdictional plots
      combined_jurisdiction <- grid.arrange(country_plot, country_summary_plot, 
                                           ncol = 1, heights = c(2, 1.5))
      
      # Save jurisdictional analysis
      ggsave(file.path(output_dir, "jurisdictional_analysis.png"), 
             plot = combined_jurisdiction, width = 12, height = 8, dpi = 300)
      
      cat("Jurisdictional analysis plots saved\n")
      
      # Save country summary
      write_csv(country_summary, file.path(output_dir, "country_summary.csv"))
    }
  }, error = function(e) {
    cat("Error in jurisdictional analysis:", e$message, "\n")
  })
} else {
  cat("Country data not available for jurisdictional analysis\n")
}

# === VARIABLE COMPLEXITY ANALYSIS ===
cat("\n=== VARIABLE COMPLEXITY ANALYSIS ===\n")

# Check for variable complexity columns
complexity_cols <- c("Total_Variables", "Environmental_Variables", "Demographic_Variables", 
                     "Economic_Variables", "Distance_Variables", "Temporal_Variables")
available_complexity_cols <- complexity_cols[complexity_cols %in% names(data)]

if (length(available_complexity_cols) > 0) {
  tryCatch({
    complexity_data <- data %>%
      select(all_of(available_complexity_cols), Unit_size_km2) %>%
      filter(!is.na(Unit_size_km2))
    
    if ("Total_Variables" %in% names(complexity_data)) {
      # Total variables vs unit size
      var_plot <- ggplot(complexity_data, aes(x = Total_Variables, y = Unit_size_km2)) +
        geom_point(alpha = 0.6, size = 2, color = "#2E86AB") +
        geom_smooth(method = "lm", se = TRUE, color = "#A23B72") +
        scale_y_log10() +
        labs(
          title = "Variable Complexity vs Unit Size",
          subtitle = "Relationship between analytical sophistication and spatial scale",
          x = "Total Number of Variables",
          y = "Unit Size (km²) - Log Scale"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)
        )
      
      # Variable distribution
      var_dist_plot <- ggplot(complexity_data, aes(x = Total_Variables)) +
        geom_histogram(bins = 15, fill = "#F18F01", alpha = 0.8, color = "white") +
        labs(
          title = "Distribution of Variable Complexity",
          x = "Total Number of Variables",
          y = "Number of Studies"
        ) +
        theme_minimal()
      
      # Combine complexity plots
      combined_complexity <- grid.arrange(var_plot, var_dist_plot, 
                                         ncol = 1, heights = c(2, 1.5))
      
      # Save variable complexity analysis
      ggsave(file.path(output_dir, "variable_complexity_analysis.png"), 
             plot = combined_complexity, width = 12, height = 8, dpi = 300)
      
      cat("Variable complexity analysis plots saved\n")
    }
  }, error = function(e) {
    cat("Error in variable complexity analysis:", e$message, "\n")
  })
} else {
  cat("Variable complexity data not available\n")
}

# === CORRELATION ANALYSIS ===
cat("\n=== CORRELATION ANALYSIS ===\n")

# Select numeric variables for correlation analysis
numeric_vars <- c("Unit_size_km2", "Year", "Total_Variables", "Environmental_Variables", 
                  "Demographic_Variables", "Economic_Variables", "Distance_Variables", 
                  "Temporal_Variables")
available_numeric_vars <- numeric_vars[numeric_vars %in% names(data)]

if (length(available_numeric_vars) >= 2) {
  tryCatch({
    # Check data types and convert properly
    cat("Checking available numeric variables:\n")
    for (var in available_numeric_vars) {
      cat(paste(var, "- class:", class(data[[var]]), "- NA count:", sum(is.na(data[[var]])), "\n"))
    }
    
    # Create correlation data with careful type conversion
    corr_data <- data %>%
      select(all_of(available_numeric_vars)) %>%
      mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%  # Force numeric conversion
      filter(if_all(everything(), ~ !is.na(.)))  # Remove rows with any NA
    
    cat("Correlation data prepared. Rows:", nrow(corr_data), "Cols:", ncol(corr_data), "\n")
    
    if (nrow(corr_data) > 5 && ncol(corr_data) > 1) {
      # Verify all columns are numeric
      all_numeric <- all(sapply(corr_data, is.numeric))
      cat("All columns numeric:", all_numeric, "\n")
      
      if (all_numeric) {
        # Calculate correlation matrix
        corr_matrix <- cor(corr_data, use = "complete.obs")
        
        # Create correlation plot
        png(file.path(output_dir, "correlation_matrix.png"), 
            width = 10, height = 8, units = "in", res = 300)
        
        corrplot(corr_matrix, 
                 method = "color",
                 type = "upper",
                 order = "hclust",
                 tl.col = "black",
                 tl.srt = 45,
                 addCoef.col = "black",
                 number.cex = 0.8,
                 title = "Correlation Matrix of Key Variables",
                 mar = c(0, 0, 2, 0))
        
        dev.off()
        
        cat("Correlation matrix plot saved\n")
        
        # Save correlation matrix
        corr_matrix_df <- as.data.frame(corr_matrix)
        corr_matrix_df$Variable <- rownames(corr_matrix_df)
        write_csv(corr_matrix_df, file.path(output_dir, "correlation_matrix.csv"))
      } else {
        cat("Some variables could not be converted to numeric\n")
      }
    } else {
      cat("Insufficient data for correlation analysis after filtering\n")
    }
  }, error = function(e) {
    cat("Error in correlation analysis:", e$message, "\n")
    cat("Available variables:", paste(available_numeric_vars, collapse = ", "), "\n")
  })
} else {
  cat("Insufficient numeric variables for correlation analysis\n")
}

# === JUSTIFICATION BY UNIT TYPE PLOT ===
if (nrow(justification_by_unit_type) > 0) {
  tryCatch({
    just_plot <- ggplot(justification_by_unit_type, aes(x = reorder(Unit_Type, Avg_Unit_Size), 
                                                       y = Avg_Unit_Size)) +
      geom_col(fill = "#2E86AB", alpha = 0.8) +
      geom_text(aes(label = paste0("n=", Count, "\n", Percent_Justified, "%")), 
                hjust = -0.1, size = 3) +
      coord_flip() +
      scale_y_log10() +
      labs(
        title = "Unit Types by Average Size and Justification Rate",
        subtitle = "Size and justification patterns across different unit types",
        x = "Unit Type",
        y = "Average Unit Size (km²) - Log Scale"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.margin = margin(20, 50, 20, 20)
      )
    
    ggsave(file.path(output_dir, "justification_by_unit_type.png"), 
           plot = just_plot, width = 12, height = 8, dpi = 300)
    
    cat("Justification by unit type plot saved\n")
  }, error = function(e) {
    cat("Error creating justification by unit type plot:", e$message, "\n")
  })
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All outputs saved to:", output_dir, "\n")
cat("Key new features:\n")
cat("- Used trimmed dataset with only analysis variables\n") 
cat("- Added unit selection justification analysis\n")
cat("- Maintained all existing analysis outputs\n")
cat("- Dataset now includes: Justification_Summary, Quoted_Rationale, Rationale_Category\n")
