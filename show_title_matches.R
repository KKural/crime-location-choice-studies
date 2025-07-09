# Script to show only the matched pairs of titles between master and clean datasets
# Load required libraries
library(dplyr)
library(stringr)

# Load the datasets
master_data <- read.csv("20250709_Analysis & Results/20250709_standardized_unit_sizes_with_groups.csv", stringsAsFactors = FALSE)
clean_data <- read.csv("20250709_Analysis & Results/20250709_analysis_ready_dataset_clean.csv", stringsAsFactors = FALSE)

# Function to normalize titles for matching
normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# Create normalized versions
master_data$normalized_title <- normalize_title(master_data$Title_of_the_study)
clean_data$normalized_title <- normalize_title(clean_data$Title)

# Initialize results dataframe
title_matches <- data.frame(
  Master_Title = character(),
  Clean_Title = character(),
  Match_Type = character(),
  stringsAsFactors = FALSE
)

# Track matched studies
matched_master <- c()
matched_clean <- c()

# 1. Exact match on normalized titles
cat("Step 1: Exact matching on normalized titles...\n")
exact_matches <- inner_join(
  master_data %>% select(Title_of_the_study, normalized_title) %>% mutate(master_idx = row_number()),
  clean_data %>% select(Title, normalized_title) %>% mutate(clean_idx = row_number()),
  by = "normalized_title"
)

if (nrow(exact_matches) > 0) {
  title_matches <- rbind(title_matches, data.frame(
    Master_Title = exact_matches$Title_of_the_study,
    Clean_Title = exact_matches$Title,
    Match_Type = "Exact",
    stringsAsFactors = FALSE
  ))
  matched_master <- c(matched_master, exact_matches$master_idx)
  matched_clean <- c(matched_clean, exact_matches$clean_idx)
}

# 2. Fuzzy matching - master title contained in clean title
cat("Step 2: Fuzzy matching (master in clean)...\n")
unmatched_master <- master_data[-matched_master, ]
unmatched_clean <- clean_data[-matched_clean, ]

for (i in 1:nrow(unmatched_master)) {
  master_title <- unmatched_master$normalized_title[i]
  
  # Find clean titles that contain the master title
  matches <- which(str_detect(unmatched_clean$normalized_title, fixed(master_title)))
  
  if (length(matches) > 0) {
    # Take the first match
    match_idx <- matches[1]
    title_matches <- rbind(title_matches, data.frame(
      Master_Title = unmatched_master$title[i],
      Clean_Title = unmatched_clean$title[match_idx],
      Match_Type = "Fuzzy (Master in Clean)",
      stringsAsFactors = FALSE
    ))
    matched_master <- c(matched_master, unmatched_master$master_idx[i])
    matched_clean <- c(matched_clean, unmatched_clean$clean_idx[match_idx])
  }
}

# 3. Word overlap matching
cat("Step 3: Word overlap matching...\n")
unmatched_master <- master_data[!master_data$master_idx %in% matched_master, ]
unmatched_clean <- clean_data[!clean_data$clean_idx %in% matched_clean, ]

get_significant_words <- function(title) {
  words <- str_split(title, "\\s+")[[1]]
  words <- words[nchar(words) > 3]  # Only words longer than 3 characters
  return(words)
}

for (i in 1:nrow(unmatched_master)) {
  master_words <- get_significant_words(unmatched_master$normalized_title[i])
  
  best_match_idx <- NULL
  best_overlap <- 0
  
  for (j in 1:nrow(unmatched_clean)) {
    clean_words <- get_significant_words(unmatched_clean$normalized_title[j])
    
    if (length(master_words) > 0 && length(clean_words) > 0) {
      overlap <- length(intersect(master_words, clean_words))
      overlap_ratio <- overlap / max(length(master_words), length(clean_words))
      
      if (overlap_ratio >= 0.6 && overlap_ratio > best_overlap) {
        best_overlap <- overlap_ratio
        best_match_idx <- j
      }
    }
  }
  
  if (!is.null(best_match_idx)) {
    title_matches <- rbind(title_matches, data.frame(
      Master_Title = unmatched_master$title[i],
      Clean_Title = unmatched_clean$title[best_match_idx],
      Match_Type = paste0("Word Overlap (", round(best_overlap * 100, 1), "%)"),
      stringsAsFactors = FALSE
    ))
    matched_master <- c(matched_master, unmatched_master$master_idx[i])
    matched_clean <- c(matched_clean, unmatched_clean$clean_idx[best_match_idx])
  }
}

# 4. Special pattern matching for AU/NL/UK studies
cat("Step 4: Special pattern matching...\n")
unmatched_master <- master_data[!master_data$master_idx %in% matched_master, ]
unmatched_clean <- clean_data[!clean_data$clean_idx %in% matched_clean, ]

for (i in 1:nrow(unmatched_master)) {
  master_title <- unmatched_master$normalized_title[i]
  
  # Check for AU/NL/UK pattern
  if (str_detect(master_title, "au|nl|uk")) {
    for (j in 1:nrow(unmatched_clean)) {
      clean_title <- unmatched_clean$normalized_title[j]
      
      # Extract the main part before AU/NL/UK
      master_main <- str_extract(master_title, "^[^a]+(?=\\s+(au|nl|uk))")
      if (!is.na(master_main)) {
        master_main <- str_trim(master_main)
        
        if (str_detect(clean_title, fixed(master_main))) {
          title_matches <- rbind(title_matches, data.frame(
            Master_Title = unmatched_master$title[i],
            Clean_Title = unmatched_clean$title[j],
            Match_Type = "Special Pattern (AU/NL/UK)",
            stringsAsFactors = FALSE
          ))
          matched_master <- c(matched_master, unmatched_master$master_idx[i])
          matched_clean <- c(matched_clean, unmatched_clean$clean_idx[j])
          break
        }
      }
    }
  }
}

# 5. Reverse fuzzy matching - clean title contained in master title
cat("Step 5: Reverse fuzzy matching (clean in master)...\n")
unmatched_master <- master_data[!master_data$master_idx %in% matched_master, ]
unmatched_clean <- clean_data[!clean_data$clean_idx %in% matched_clean, ]

for (i in 1:nrow(unmatched_master)) {
  master_title <- unmatched_master$normalized_title[i]
  
  for (j in 1:nrow(unmatched_clean)) {
    clean_title <- unmatched_clean$normalized_title[j]
    
    # Check if clean title is contained in master title
    if (str_detect(master_title, fixed(clean_title))) {
      title_matches <- rbind(title_matches, data.frame(
        Master_Title = unmatched_master$title[i],
        Clean_Title = unmatched_clean$title[j],
        Match_Type = "Reverse Fuzzy (Clean in Master)",
        stringsAsFactors = FALSE
      ))
      matched_master <- c(matched_master, unmatched_master$master_idx[i])
      matched_clean <- c(matched_clean, unmatched_clean$clean_idx[j])
      break
    }
  }
}

# Print summary
cat("\n=== TITLE MATCHING SUMMARY ===\n")
cat("Total matches found:", nrow(title_matches), "\n")
cat("Total studies in master:", nrow(master_data), "\n")
cat("Matching coverage:", round(nrow(title_matches) / nrow(master_data) * 100, 1), "%\n\n")

# Print match type breakdown
match_breakdown <- table(title_matches$Match_Type)
cat("Match type breakdown:\n")
print(match_breakdown)

cat("\n=== MATCHED TITLE PAIRS ===\n")
for (i in 1:nrow(title_matches)) {
  cat(sprintf("%d. [%s]\n", i, title_matches$Match_Type[i]))
  cat(sprintf("   Master: %s\n", title_matches$Master_Title[i]))
  cat(sprintf("   Clean:  %s\n", title_matches$Clean_Title[i]))
  cat("\n")
}

# Save to CSV for reference
write.csv(title_matches, "20250709_Analysis & Results/title_matches.csv", row.names = FALSE)
cat("Title matches saved to: 20250709_Analysis & Results/title_matches.csv\n")
