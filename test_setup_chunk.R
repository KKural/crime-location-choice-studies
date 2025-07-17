# Test just the setup chunk calculations
library(knitr)
library(flextable)
library(readxl)
library(tidyverse)
library(officer)

# Helper function for safe rounding
safe_round <- function(x, digits = 0) {
  if(is.numeric(x) && !is.na(x) && !is.nan(x) && is.finite(x)) {
    round(x, digits)
  } else {
    0  # Return 0 for non-numeric or problematic values
  }
}

# Helper function for safe numeric conversion
safe_numeric <- function(x, default = 0) {
  result <- suppressWarnings(as.numeric(x))
  if(is.na(result) || !is.finite(result)) {
    return(default)
  }
  return(result)
}

# Helper function for safe percentage calculation
safe_percentage <- function(numerator, denominator, digits = 1, default = 0) {
  if(is.numeric(numerator) && is.numeric(denominator) && 
     !is.na(numerator) && !is.na(denominator) && 
     denominator != 0) {
    safe_round((numerator / denominator) * 100, digits)
  } else {
    default
  }
}

# Load the analysis environment
current_date <- format(Sys.Date(), "%Y%m%d")
env_file <- paste0(current_date, "_analysis_environment.RData")

if (file.exists(env_file)) {
  cat("Loading saved R environment from:", env_file, "\n")
  load(env_file)
} else {
  cat("Running analysis script...\n")
  source("20250714_new_csv_analysis_clean.R")
  save.image(file = env_file)
}

# Test all the problematic calculations
cat("Testing setup chunk calculations...\n")

# Core study statistics
n_studies <- safe_numeric(spatial_stats$N_studies)
n_observations <- safe_numeric(spatial_stats$N_observations)
n_countries <- safe_numeric(spatial_stats$N_countries)
n_journals <- safe_numeric(spatial_stats$N_journals)

cat("Core stats - Studies:", n_studies, "Observations:", n_observations, "\n")

# Unit size statistics
median_unit_size <- paste0(spatial_stats$Median_unit_size, " km²")
mean_unit_size <- paste0(safe_round(safe_numeric(spatial_stats$Mean_unit_size), 3), " km²")
smallest_unit_km2 <- safe_numeric(spatial_stats$Min_unit_size)
smallest_unit_m2 <- safe_round(smallest_unit_km2 * 1000000, 0)

cat("Unit size stats calculated successfully\n")

# Size category percentages
size_category_stats <- data %>%
  count(Size_category, .drop = FALSE) %>%
  mutate(
    total_n = sum(n),
    percentage = safe_percentage(n, total_n, 0)
  )

micro_scale_pct <- safe_numeric(size_category_stats$percentage[size_category_stats$Size_category == "Very Small (<0.01 km²)"], 0)
block_level_pct <- safe_numeric(size_category_stats$percentage[size_category_stats$Size_category == "Small (0.01-<0.25 km²)"], 0)

cat("Size categories calculated successfully\n")

# Country statistics
country_stats <- data %>%
  count(Country_clean, sort = TRUE) %>%
  mutate(
    total_n = sum(n),
    percentage = safe_percentage(n, total_n, 0)
  )

netherlands_studies <- safe_numeric(country_stats$n[country_stats$Country_clean == "Netherlands"], 0)
us_studies <- safe_numeric(country_stats$n[country_stats$Country_clean == "United States"], 0)

cat("Country stats calculated successfully\n")

# Test the tricky calculations
post_2010_pct <- safe_percentage(sum(data$Year > 2010, na.rm = TRUE), nrow(data), 0)
cat("Post-2010 percentage:", post_2010_pct, "\n")

# Test records calculations
initial_records <- 2325  # From analysis script
naive_search_total <- 249
percent_increase <- safe_percentage(initial_records - naive_search_total, naive_search_total, 0)
cat("Percent increase:", percent_increase, "\n")

cat("All setup chunk calculations completed successfully!\n")
