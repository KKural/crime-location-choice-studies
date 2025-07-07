library(tidyverse)
library(stringr)
library(here)
library(readr)

# Load the datasets
comprehensive_data <- read_csv(here("Output", "comprehensive_studies_dataset.csv"), show_col_types = FALSE)
unit_sizes_data <- read_csv(here("Data", "20250707_standardized_unit_sizes_with_groups.csv"), show_col_types = FALSE)

# Perform the left join
merged_data <- unit_sizes_data %>%
  left_join(comprehensive_data, by = c("Study_ID" = "study_id")) %>%
  # Extract publication year from citation
  mutate(Publication_Year = str_extract(Citation, "\\d{4}")) %>%
  # Relocate Publication_Year to be right after Citation
  relocate(Publication_Year, .after = Citation)

# Save the merged dataset (excluding unnecessary columns, but keeping country, city, etc.)
final_merged_data <- merged_data %>%
  select(-filename, -suoa_type, -suoa_size, -num_units, -sample_size)

write_csv(final_merged_data, here("Output", "merged_comprehensive_unit_sizes.csv"))

str(final_merged_data)
final_merged_data$city
final_merged_data$country
