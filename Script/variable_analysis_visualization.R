# Variable Analysis and Visualization Script
# Author: Kuralarasan Kumar
# Date: July 7, 2025

library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(RColorBrewer)

# Load the processed data (run improved_variable_extraction.R first)
print("Loading processed data...")

studies_metadata <- read_csv(here("Output", "studies_metadata.csv"))
variables_long <- read_csv(here("Output", "variables_extracted.csv"))
var_frequency <- read_csv(here("Output", "variable_frequency_summary.csv"))
common_variables <- read_csv(here("Output", "common_variables.csv"))

# Create visualizations

# 1. Variable categories frequency
var_category_plot <- var_frequency %>%
  mutate(variable_category = str_replace_all(variable_category, "_", " "),
         variable_category = str_to_title(variable_category)) %>%
  ggplot(aes(x = reorder(variable_category, total_variables), y = total_variables)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = total_variables), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Distribution of Variable Categories",
    subtitle = "Total number of variables by category across all studies",
    x = "Variable Category",
    y = "Number of Variables"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60")
  )

ggsave(here("Output", "variable_categories_frequency.png"), 
       var_category_plot, width = 10, height = 6, dpi = 300)

# 2. Studies per variable category
studies_per_category <- variables_long %>%
  group_by(variable_category) %>%
  summarise(studies_count = n_distinct(study_id)) %>%
  mutate(variable_category = str_replace_all(variable_category, "_", " "),
         variable_category = str_to_title(variable_category))

studies_category_plot <- studies_per_category %>%
  ggplot(aes(x = reorder(variable_category, studies_count), y = studies_count)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = studies_count), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Number of Studies Using Each Variable Category",
    x = "Variable Category",
    y = "Number of Studies"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(here("Output", "studies_per_category.png"), 
       studies_category_plot, width = 10, height = 6, dpi = 300)

# 3. Most common individual variables (top 20)
top_variables_plot <- common_variables %>%
  slice_head(n = 20) %>%
  mutate(variable_name = str_wrap(variable_name, 40)) %>%
  ggplot(aes(x = reorder(variable_name, frequency), y = frequency)) +
  geom_col(aes(fill = variable_category), alpha = 0.8) +
  geom_text(aes(label = frequency), hjust = -0.1, size = 2.5) +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequently Used Variables",
    x = "Variable Name",
    y = "Frequency (Number of Studies)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(type = "qual", palette = "Set3")

ggsave(here("Output", "top_variables.png"), 
       top_variables_plot, width = 12, height = 8, dpi = 300)

# 4. Crime types analysis
crime_types <- studies_metadata %>%
  filter(!is.na(crime_type) & crime_type != "") %>%
  count(crime_type, sort = TRUE) %>%
  mutate(crime_type = str_wrap(crime_type, 30))

if (nrow(crime_types) > 0) {
  crime_types_plot <- crime_types %>%
    slice_head(n = 15) %>%
    ggplot(aes(x = reorder(crime_type, n), y = n)) +
    geom_col(fill = "orange", alpha = 0.8) +
    geom_text(aes(label = n), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Distribution of Crime Types in Studies",
      x = "Crime Type",
      y = "Number of Studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(here("Output", "crime_types_distribution.png"), 
         crime_types_plot, width = 10, height = 6, dpi = 300)
}

# 5. Spatial units of analysis
suoa_types <- studies_metadata %>%
  filter(!is.na(suoa_type) & suoa_type != "") %>%
  count(suoa_type, sort = TRUE) %>%
  mutate(suoa_type = str_wrap(suoa_type, 30))

if (nrow(suoa_types) > 0) {
  suoa_plot <- suoa_types %>%
    slice_head(n = 15) %>%
    ggplot(aes(x = reorder(suoa_type, n), y = n)) +
    geom_col(fill = "purple", alpha = 0.8) +
    geom_text(aes(label = n), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Spatial Units of Analysis Used in Studies",
      x = "Spatial Unit Type",
      y = "Number of Studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(here("Output", "spatial_units_distribution.png"), 
         suoa_plot, width = 10, height = 6, dpi = 300)
}

# 6. Countries/regions analysis
countries <- studies_metadata %>%
  filter(!is.na(country) & country != "") %>%
  count(country, sort = TRUE)

if (nrow(countries) > 0) {
  countries_plot <- countries %>%
    slice_head(n = 15) %>%
    ggplot(aes(x = reorder(country, n), y = n)) +
    geom_col(fill = "red", alpha = 0.8) +
    geom_text(aes(label = n), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Geographic Distribution of Studies",
      x = "Country",
      y = "Number of Studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  
  ggsave(here("Output", "countries_distribution.png"), 
         countries_plot, width = 10, height = 6, dpi = 300)
}

# 7. Variable complexity by study (number of variables per study)
var_complexity <- variables_long %>%
  group_by(study_id) %>%
  summarise(
    total_variables = n(),
    unique_categories = n_distinct(variable_category)
  ) %>%
  left_join(studies_metadata %>% select(study_id, extracted_title), by = "study_id")

complexity_plot <- var_complexity %>%
  ggplot(aes(x = total_variables, y = unique_categories)) +
  geom_point(alpha = 0.6, size = 2, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Variable Complexity Across Studies",
    subtitle = "Relationship between total variables and category diversity",
    x = "Total Number of Variables",
    y = "Number of Variable Categories"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60")
  )

ggsave(here("Output", "variable_complexity.png"), 
       complexity_plot, width = 10, height = 6, dpi = 300)

# Create summary report
summary_stats <- list(
  total_studies = nrow(studies_metadata),
  total_variables = nrow(variables_long),
  unique_variables = n_distinct(variables_long$variable_name),
  variable_categories = n_distinct(variables_long$variable_category),
  countries = n_distinct(studies_metadata$country, na.rm = TRUE),
  crime_types = n_distinct(studies_metadata$crime_type, na.rm = TRUE)
)

# Save summary report
write_lines(
  c("=== VARIABLE EXTRACTION SUMMARY REPORT ===",
    paste("Date:", Sys.Date()),
    "",
    "=== BASIC STATISTICS ===",
    paste("Total studies processed:", summary_stats$total_studies),
    paste("Total variables extracted:", summary_stats$total_variables),
    paste("Unique variable names:", summary_stats$unique_variables),
    paste("Variable categories:", summary_stats$variable_categories),
    paste("Countries represented:", summary_stats$countries),
    paste("Crime types studied:", summary_stats$crime_types),
    "",
    "=== TOP VARIABLE CATEGORIES ===",
    paste(capture.output(print(var_frequency)), collapse = "\n"),
    "",
    "=== TOP 10 MOST COMMON VARIABLES ===",
    paste(capture.output(print(head(common_variables, 10))), collapse = "\n")
  ),
  here("Output", "summary_report.txt")
)

print("Analysis completed! Check the Output folder for:")
print("- Variable category frequency plot")
print("- Studies per category plot")
print("- Top variables plot")
print("- Crime types distribution plot")
print("- Spatial units distribution plot")
print("- Countries distribution plot")
print("- Variable complexity plot")
print("- Summary report")
