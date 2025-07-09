# Final Cleaning and Optimization of Extracted Data
# Remove ** symbols, handle duplicates, and create clean final dataset

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Read the comprehensive extracted dataset
data <- read_csv("Output/comprehensive_expanded_dataset.csv")

cat("Starting final cleaning process...\n")
cat("Original shape:", nrow(data), "rows x", ncol(data), "columns\n")

# Function to clean values
clean_value <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  
  # Remove ** markdown
  x <- str_remove_all(x, "\\*\\*")
  
  # Clean extra spaces
  x <- str_squish(x)
  
  # Convert empty strings to NA
  if (x == "" || x == "NA") return(NA_character_)
  
  return(x)
}

# Clean all character columns
cat("Cleaning values (removing ** symbols)...\n")
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- sapply(data[[col]], clean_value, USE.NAMES = FALSE)
  }
}

# Identify and remove duplicate columns
cat("Identifying duplicate columns...\n")
column_groups <- list()

for (col in names(data)[-1]) {  # Skip Title column
  # Create a base name by removing extra underscores
  base_name <- str_replace_all(col, "_{2,}", "_") %>% 
               str_remove("_$")
  
  if (is.null(column_groups[[base_name]])) {
    column_groups[[base_name]] <- c()
  }
  column_groups[[base_name]] <- c(column_groups[[base_name]], col)
}

# Keep only the best version of duplicate columns
cat("Resolving duplicate columns...\n")
final_columns <- c("Title")

for (base_name in names(column_groups)) {
  cols <- column_groups[[base_name]]
  
  if (length(cols) == 1) {
    final_columns <- c(final_columns, cols[1])
  } else {
    # Choose the column with most non-NA values
    col_scores <- sapply(cols, function(col) sum(!is.na(data[[col]])))
    best_col <- cols[which.max(col_scores)]
    final_columns <- c(final_columns, best_col)
    
    # Merge data from other columns if they have additional info
    for (other_col in cols[cols != best_col]) {
      for (i in 1:nrow(data)) {
        if (is.na(data[[best_col]][i]) && !is.na(data[[other_col]][i])) {
          data[[best_col]][i] <- data[[other_col]][i]
        }
      }
    }
  }
}

# Create final cleaned dataset
final_data <- data %>% select(all_of(final_columns))

cat("Final cleaning completed!\n")
cat("Final shape:", nrow(final_data), "rows x", ncol(final_data), "columns\n")

# Save the final dataset
write_csv(final_data, "Output/final_expanded_dataset.csv")

# Create summary statistics
cat("\n=== FINAL EXTRACTION SUMMARY ===\n")

# Count by category
category_summary <- tibble(
  Column = names(final_data)[-1]
) %>%
  mutate(
    Category = str_extract(Column, "^[^_]+(?=_[^_]+_)"),
    Category = ifelse(is.na(Category), str_extract(Column, "^[^_]+"), Category)
  ) %>%
  count(Category, sort = TRUE, name = "Column_Count")

print(category_summary)

# Data completeness by category
cat("\n=== DATA COMPLETENESS BY CATEGORY ===\n")
completeness_summary <- final_data %>%
  select(-Title) %>%
  summarise_all(~sum(!is.na(.))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Non_NA_Count") %>%
  mutate(
    Category = str_extract(Column, "^[^_]+(?=_[^_]+_)"),
    Category = ifelse(is.na(Category), str_extract(Column, "^[^_]+"), Category),
    Completeness_Percent = round((Non_NA_Count / nrow(final_data)) * 100, 1)
  ) %>%
  group_by(Category) %>%
  summarise(
    Total_Columns = n(),
    Avg_Completeness = round(mean(Completeness_Percent), 1),
    Min_Completeness = min(Completeness_Percent),
    Max_Completeness = max(Completeness_Percent),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Completeness))

print(completeness_summary)

# Sample the key extracted fields
cat("\n=== SAMPLE EXTRACTED KEY FIELDS ===\n")
key_fields <- c("Title")

# Add some key basic fields if they exist
basic_fields <- names(final_data)[str_detect(names(final_data), "BASIC.*Title|BASIC.*Year|BASIC.*Authors")]
key_fields <- c(key_fields, head(basic_fields, 3))

# Add some geographic fields
geo_fields <- names(final_data)[str_detect(names(final_data), "STUDY.*Country|STUDY.*City")]
key_fields <- c(key_fields, head(geo_fields, 2))

if (length(key_fields) > 1) {
  sample_data <- final_data[1:5, key_fields[key_fields %in% names(final_data)]]
  print(sample_data)
}

cat("\n=== FILES CREATED ===\n")
cat("- Output/comprehensive_expanded_dataset.csv (", ncol(data), " columns - intermediate)\n")
cat("- Output/final_expanded_dataset.csv (", ncol(final_data), " columns - final clean version)\n")
cat("\nExtraction and cleaning process complete!\n")
