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

# Function to normalize titles for better matching
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%                          # Convert to lowercase
    str_remove_all('[""\'\"*]') %>%            # Remove quotes and asterisks
    str_remove_all('\\([^)]*\\)') %>%          # Remove parenthetical content
    str_replace_all('[[:punct:]]', ' ') %>%    # Replace punctuation with spaces
    str_replace_all('\\s+', ' ') %>%           # Replace multiple spaces with single space
    str_trim()                                  # Trim whitespace
}

# Create normalized versions for matching
master_norm <- master %>%
  mutate(title_norm = normalize_title(Title_of_the_study))

clean_norm <- clean %>%
  mutate(title_norm = normalize_title(Title))

cat("=== TESTING NORMALIZED MATCHING ===\n")

# Test with a few examples
test_cases <- c(
  "Location Location Location", 
  "A discrete spatial choice model",
  "Graffiti Writers Choose Locations"
)

for (test in test_cases) {
  master_matches <- master_norm$Title_of_the_study[str_detect(master_norm$title_norm, str_to_lower(test))]
  clean_matches <- clean_norm$Title[str_detect(clean_norm$title_norm, str_to_lower(test))]
  
  cat(sprintf("\nTest pattern: '%s'\n", test))
  cat(sprintf("Master matches: %d\n", length(master_matches)))
  if(length(master_matches) > 0) cat(sprintf("  - %s\n", master_matches[1]))
  cat(sprintf("Clean matches: %d\n", length(clean_matches)))
  if(length(clean_matches) > 0) cat(sprintf("  - %s\n", clean_matches[1]))
}

# Create comprehensive matching
matches <- data.frame(
  master_title = character(),
  clean_title = character(),
  match_type = character(),
  stringsAsFactors = FALSE
)

# Method 1: Exact normalized match
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  exact_match <- clean_norm$Title[clean_norm$title_norm == master_norm_title]
  if (length(exact_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = master_title,
      clean_title = exact_match[1],
      match_type = "exact_normalized"
    ))
  }
}

# Method 2: Fuzzy match (master contained in clean)
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  # Skip if already matched
  if (master_title %in% matches$master_title) next
  
  # Look for clean titles that contain the master title
  fuzzy_match <- clean_norm$Title[str_detect(clean_norm$title_norm, str_escape(master_norm_title))]
  if (length(fuzzy_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = master_title,
      clean_title = fuzzy_match[1],
      match_type = "fuzzy_contained"
    ))
  }
}

# Method 3: Partial word matching (80% of words match)
for (i in 1:nrow(master_norm)) {
  master_title <- master_norm$Title_of_the_study[i]
  master_norm_title <- master_norm$title_norm[i]
  
  # Skip if already matched
  if (master_title %in% matches$master_title) next
  
  master_words <- str_split(master_norm_title, "\\s+")[[1]]
  master_words <- master_words[nchar(master_words) > 3]  # Only significant words
  
  if (length(master_words) < 3) next  # Skip very short titles
  
  for (j in 1:nrow(clean_norm)) {
    clean_words <- str_split(clean_norm$title_norm[j], "\\s+")[[1]]
    clean_words <- clean_words[nchar(clean_words) > 3]
    
    if (length(clean_words) < 3) next
    
    # Calculate word overlap
    common_words <- intersect(master_words, clean_words)
    overlap_ratio <- length(common_words) / length(master_words)
    
    if (overlap_ratio >= 0.6) {  # 60% word overlap
      matches <- bind_rows(matches, data.frame(
        master_title = master_title,
        clean_title = clean_norm$Title[j],
        match_type = sprintf("word_overlap_%.0f%%", overlap_ratio * 100)
      ))
      break
    }
  }
}

cat(sprintf("\n=== MATCHING RESULTS ===\n"))
cat(sprintf("Total matches found: %d\n", nrow(matches)))
cat(sprintf("Exact normalized: %d\n", sum(matches$match_type == "exact_normalized")))
cat(sprintf("Fuzzy contained: %d\n", sum(matches$match_type == "fuzzy_contained")))
cat(sprintf("Word overlap: %d\n", sum(str_detect(matches$match_type, "word_overlap"))))

cat("\nFirst 10 matches:\n")
print(head(matches, 10))

# Save the matching results
write_csv(matches, "title_matches.csv")
