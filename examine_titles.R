library(dplyr)
library(readr)
library(stringr)

# Read both datasets
master <- read_csv("20250709_Analysis & Results/20250709_standardized_unit_sizes_with_groups.csv", show_col_types = FALSE)
clean <- read_csv("20250709_Analysis & Results/20250709_analysis_ready_dataset_clean.csv", show_col_types = FALSE)

# Clean the title column in the clean dataset
clean <- clean %>%
  mutate(Title = str_remove_all(Title, '^"') %>%
                 str_remove_all('"$') %>%
                 str_trim())

cat("=== MASTER DATASET TITLES ===\n")
for(i in 1:nrow(master)) {
  cat(sprintf("%2d: %s\n", i, master$Title_of_the_study[i]))
}

cat("\n=== CLEAN DATASET TITLES ===\n")
for(i in 1:nrow(clean)) {
  cat(sprintf("%2d: %s\n", i, clean$Title[i]))
}

cat("\n=== FINDING UNMATCHED STUDY ===\n")
# Find the unmatched study
unmatched_master_title <- "The Importance of Importance Sampling: Exploring Methods of Sampling from Alternatives in Discrete Choice Models of Crime Location Choice"

cat(sprintf("Unmatched master title: %s\n", unmatched_master_title))

# Look for any title containing "importance" in clean data
importance_matches <- clean$Title[str_detect(str_to_lower(clean$Title), "importance")]
cat(sprintf("Clean titles containing 'importance': %d\n", length(importance_matches)))
if(length(importance_matches) > 0) {
  for(i in 1:length(importance_matches)) {
    cat(sprintf("  %d: %s\n", i, importance_matches[i]))
  }
}

# Look for any title containing "sampling" in clean data
sampling_matches <- clean$Title[str_detect(str_to_lower(clean$Title), "sampling")]
cat(sprintf("\nClean titles containing 'sampling': %d\n", length(sampling_matches)))
if(length(sampling_matches) > 0) {
  for(i in 1:length(sampling_matches)) {
    cat(sprintf("  %d: %s\n", i, sampling_matches[i]))
  }
}
