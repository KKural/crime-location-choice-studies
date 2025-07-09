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

cat("Master titles (first 5):\n")
print(head(master$Title_of_the_study, 5))

cat("\nClean titles (first 5):\n")
print(head(clean$Title, 5))

cat("\nNumber of matching titles:", length(intersect(master$Title_of_the_study, clean$Title)))
cat("\nTotal master titles:", nrow(master))
cat("\nTotal clean titles:", nrow(clean))

# Try fuzzy matching - check if master titles are contained in clean titles
fuzzy_matches <- data.frame(
  master_title = character(),
  clean_title = character(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(master)) {
  master_title <- master$Title_of_the_study[i]
  
  # Look for clean titles that contain the master title (case-insensitive)
  matches <- clean$Title[str_detect(str_to_lower(clean$Title), 
                                   str_to_lower(str_escape(master_title)))]
  
  if (length(matches) > 0) {
    fuzzy_matches <- bind_rows(fuzzy_matches, 
                              data.frame(master_title = master_title,
                                        clean_title = matches[1]))
  }
}

cat("\n\nFuzzy matches found:", nrow(fuzzy_matches))
cat("\nFirst 10 fuzzy matches:\n")
print(head(fuzzy_matches, 10))

# Show unmatched master titles
unmatched_master <- master$Title_of_the_study[!master$Title_of_the_study %in% fuzzy_matches$master_title]
cat("\n\nUnmatched master titles:", length(unmatched_master))
print(head(unmatched_master, 5))
