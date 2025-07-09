# Merge new columns from clean dataset into master dataset by title
library(dplyr)
library(readr)
library(stringr)

# Read both datasets
master <- read_csv("20250709_Analysis & Results/20250709_standardized_unit_sizes_with_groups.csv")
clean <- read_csv("20250709_Analysis & Results/20250709_analysis_ready_dataset_clean.csv")

# Clean the title column in the clean dataset to match master format
clean <- clean %>%
  mutate(Title = str_remove_all(Title, '^"') %>%  # Remove leading quotes
                 str_remove_all('"$') %>%         # Remove trailing quotes
                 str_trim())                      # Remove extra whitespace

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

# Create comprehensive matching using multiple strategies
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

# Method 3: Partial word matching (60% of words match)
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

# Method 4: Special cases for known study patterns
special_matches <- list(
  # Burglar Target Selection studies
  list(
    master_pattern = "Burglar Target Selection.*\\(NL\\)",
    clean_pattern = "Burglar Target Selection.*Netherlands"
  ),
  list(
    master_pattern = "Burglar Target Selection.*UK\\)",
    clean_pattern = "Burglar Target Selection.*United Kingdom"
  ),
  list(
    master_pattern = "Burglar Target Selection.*AU\\)",
    clean_pattern = "Burglar Target Selection.*Australia"
  )
)

for (special in special_matches) {
  master_candidates <- master$Title_of_the_study[str_detect(master$Title_of_the_study, special$master_pattern)]
  clean_candidates <- clean$Title[str_detect(clean$Title, special$clean_pattern)]
  
  if (length(master_candidates) > 0 && length(clean_candidates) > 0) {
    for (mc in master_candidates) {
      for (cc in clean_candidates) {
        # Skip if already matched
        if (mc %in% matches$master_title) next
        
        matches <- bind_rows(matches, data.frame(
          master_title = mc,
          clean_title = cc,
          match_type = "special_pattern"
        ))
        break
      }
    }
  }
}

# Method 5: Reverse fuzzy match (clean contained in master)
for (j in 1:nrow(clean_norm)) {
  clean_title <- clean_norm$Title[j]
  clean_norm_title <- clean_norm$title_norm[j]
  
  # Skip if already matched
  if (clean_title %in% matches$clean_title) next
  
  # Look for master titles that contain the clean title
  reverse_match <- master_norm$Title_of_the_study[str_detect(master_norm$title_norm, str_escape(clean_norm_title))]
  if (length(reverse_match) > 0) {
    matches <- bind_rows(matches, data.frame(
      master_title = reverse_match[1],
      clean_title = clean_title,
      match_type = "reverse_fuzzy"
    ))
  }
}

cat("Found", nrow(matches), "comprehensive text-based matches\n")

# Merge using the comprehensive matching results
merged <- master %>%
  inner_join(matches, by = c("Title_of_the_study" = "master_title")) %>%
  left_join(clean, by = c("clean_title" = "Title")) %>%
  select(-clean_title)  # Remove the helper column

# Save the merged result
write_csv(merged, "20250709_Analysis & Results/20250709_standardized_unit_sizes_with_groups_merged.csv")
