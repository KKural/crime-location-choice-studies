library(readr)
library(dplyr)

# List of CSV files to merge
combined_csv_files <- c(
  "Data/Study_Identification_Spatial_Units.csv",
  "Data/Theoretical_Framework_Methodology.csv", 
  "Data/Temporal_Variables_Findings.csv",
  "Data/Scale_Effects_Limitations.csv",
  "Data/study area size.csv"
)

# Function to clean column names (remove supporting/reasoning columns)
clean_column_names <- function(df) {
  main_cols <- colnames(df)[!grepl("tables|DOI|Venue|Citation", colnames(df))]
  return(df[, main_cols])
}

# Function to remove duplicate columns (Authors, Year) from subsequent files
remove_duplicate_cols <- function(df, is_first_file = FALSE) {
  if (is_first_file) {
    return(df)
  } else {
    # Remove Authors and Year columns from subsequent files to avoid duplicates
    cols_to_remove <- c("Authors", "Year")
    remaining_cols <- setdiff(names(df), cols_to_remove)
    return(df[, remaining_cols])
  }
}

# Read and merge all CSV files
df_combined <- NULL

for (i in 1:length(combined_csv_files)) {
  if (file.exists(combined_csv_files[i])) {
    temp_df <- read_csv(combined_csv_files[i], show_col_types = FALSE)
    temp_df <- clean_column_names(temp_df)
    
    if (is.null(df_combined)) {
      # First file becomes the base dataset (keep Authors and Year)
      df_combined <- temp_df
    } else {
      # Remove duplicate columns from subsequent files
      temp_df <- remove_duplicate_cols(temp_df, is_first_file = FALSE)
      # Merge subsequent files by Title
      df_combined <- merge(df_combined, temp_df, by = "Title", all.x = TRUE, all.y = TRUE)
    }
  }
}

# Print column names
print(names(df_combined))

# Save the merged dataset
write_csv(df_combined, "merged_dataset.csv")
