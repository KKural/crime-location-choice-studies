# ---------------------------------------------
# Script: verify_dois.R
# Purpose: Extract DOIs from a bibliography text file,
#          clean them, query CrossRef, and report which DOIs fail to resolve.
# ---------------------------------------------


# 2. Load libraries
library(rcrossref)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# Install RefManageR if not available (optional, commented out)
# if (!require(RefManageR)) {
#   install.packages("RefManageR")
#   library(RefManageR)
# }

# Function to search for DOI by title and author
search_doi_by_metadata <- function(title, author = NULL, year = NULL) {
  if (is.na(title) || title == "") return(NA)
  
  # Clean the title for search
  clean_title <- str_trim(str_replace_all(title, "[^A-Za-z0-9\\s]", " "))
  
  # Try different search strategies
  search_queries <- c(
    clean_title,
    if (!is.null(author)) paste(clean_title, author),
    if (!is.null(year)) paste(clean_title, year)
  )
  
  for (query in search_queries) {
    if (is.na(query) || query == "") next
    
    tryCatch({
      # Search CrossRef
      search_result <- cr_works(query = query, limit = 5)
      
      if (nrow(search_result$data) > 0) {
        # Look for the best match based on title similarity
        for (i in 1:min(5, nrow(search_result$data))) {
          result_title <- search_result$data$title[i]
          if (!is.na(result_title) && length(result_title) > 0) {
            result_title <- paste(result_title, collapse = " ")
            
            # Simple similarity check
            if (str_detect(tolower(result_title), tolower(str_sub(clean_title, 1, 20)))) {
              return(search_result$data$doi[i])
            }
          }
        }
        # If no good match, return the first result's DOI
        return(search_result$data$doi[1])
      }
    }, error = function(e) {
      # Continue to next search query if this one fails
    })
  }
  
  return(NA)
}

# Function to extract reference metadata from text
extract_reference_metadata <- function(ref_text) {
  # Try to extract year
  year_match <- str_extract(ref_text, "\\b(19|20)\\d{2}\\b")
  
  # Try to extract author (first author before first comma or period)
  author_match <- str_extract(ref_text, "^[^,\\.]+")
  if (!is.na(author_match)) {
    author_match <- str_trim(str_replace_all(author_match, "\\([^)]*\\)", ""))
  }
  
  # Try to extract title (text in quotes or between periods)
  title_patterns <- c(
    '"([^"]+)"',  # Text in quotes
    '\\.\\s*([A-Z][^.]+)\\.',  # Text between periods starting with capital
    '\\s([A-Z][^,]{20,}),'  # Long text starting with capital before comma
  )
  
  title_match <- NA
  for (pattern in title_patterns) {
    match <- str_match(ref_text, pattern)
    if (!is.na(match[1, 2])) {
      title_match <- str_trim(match[1, 2])
      break
    }
  }
  
  return(list(
    title = title_match,
    author = author_match,
    year = year_match
  ))
}

# 3. Read  bibliography into one long string
bib <- readLines("my_bibliography.txt", warn = FALSE) %>%
  paste(collapse = "\n")

# 4. Extract all DOI-like substrings
raw_dois <- str_extract_all(bib, "10\\.\\d{4,9}/[^\\s,;]+") %>%
  unlist() %>%
  unique()

# 5. Clean trailing punctuation (.,;) from each DOI
clean_dois <- gsub("[\\.;,\\)]+$", "", raw_dois)

# 6. Query CrossRef for each DOI and build a results tibble
results <- map_dfr(clean_dois, function(d) {
  res <- try(cr_works(d), silent = TRUE)
  
  # Handle errors or zero-row responses
  if (inherits(res, "try-error") || nrow(res$data) == 0) {
    return(tibble(
      doi       = d,
      status    = "not found",
      title     = NA_character_,
      container = NA_character_
    ))
  }
  
  # Otherwise, extract metadata from the first returned record
  row <- res$data[1, ]
  tibble(
    doi       = d,
    status    = "found",
    title     = if ("title" %in% names(row)) paste(row$title, collapse = " ") else NA_character_,
    container = if ("container-title" %in% names(row)) paste(row$`container-title`, collapse = " ") else NA_character_
  )
})

# 7. Display DOIs that failed to resolve
not_found <- results %>% filter(status == "not found")
if (nrow(not_found) > 0) {
  message("The following DOIs were NOT found on CrossRef:\n")
  print(not_found$doi)
} else {
  message("All DOIs resolved successfully!")
}

# 8. For references without DOIs, try to find DOIs by searching metadata
message("\nSearching for DOIs for references without existing DOIs...")

# Split bibliography into individual references (multiple approaches)
bib_lines <- readLines("my_bibliography.txt", warn = FALSE)

# Method 1: Split by blank lines
individual_refs_method1 <- c()
current_ref <- ""

for (line in bib_lines) {
  if (str_trim(line) == "" && current_ref != "") {
    individual_refs_method1 <- c(individual_refs_method1, current_ref)
    current_ref <- ""
  } else if (str_trim(line) != "") {
    current_ref <- paste(current_ref, line, sep = " ")
  }
}
if (current_ref != "") {
  individual_refs_method1 <- c(individual_refs_method1, current_ref)
}

# Method 2: Split by numbered references (1., 2., etc.)
bib_text <- paste(bib_lines, collapse = "\n")
individual_refs_method2 <- str_split(bib_text, "\\n\\s*\\d+\\.\\s*")[[1]]
individual_refs_method2 <- individual_refs_method2[individual_refs_method2 != ""]

# Method 3: Split by author patterns (assuming references start with author names)
individual_refs_method3 <- str_split(bib_text, "\\n(?=[A-Z][a-z]+,\\s*[A-Z])")[[1]]
individual_refs_method3 <- individual_refs_method3[individual_refs_method3 != ""]

# Choose the method that gives the most reasonable number of references
methods <- list(
  "blank_lines" = individual_refs_method1,
  "numbered" = individual_refs_method2,
  "author_pattern" = individual_refs_method3
)

# Clean all methods
methods <- lapply(methods, function(refs) {
  refs <- str_trim(refs)
  refs[refs != "" & nchar(refs) > 20]  # Remove empty and very short references
})

# Choose the method with most references (likely the correct one)
ref_counts <- sapply(methods, length)
best_method <- names(which.max(ref_counts))
individual_refs <- methods[[best_method]]

message(sprintf("Reference parsing methods found: blank_lines=%d, numbered=%d, author_pattern=%d", 
                ref_counts[1], ref_counts[2], ref_counts[3]))
message(sprintf("Using method '%s' with %d references", best_method, length(individual_refs)))
message(sprintf("Total DOIs found: %d", length(clean_dois)))

# Find references without DOIs by checking if they contain any DOI pattern
refs_without_dois <- c()
for (ref in individual_refs) {
  # Check if this reference contains any DOI pattern (not just the ones we found)
  has_doi_pattern <- str_detect(ref, "10\\.\\d{4,9}/[^\\s,;]+")
  if (!has_doi_pattern) {
    refs_without_dois <- c(refs_without_dois, ref)
  }
}

# Search for DOIs for references without them
if (length(refs_without_dois) > 0) {
  message(sprintf("Found %d references without DOIs. Searching for their DOIs...", length(refs_without_dois)))
  
  new_dois <- map_dfr(refs_without_dois, function(ref) {
    metadata <- extract_reference_metadata(ref)
    found_doi <- search_doi_by_metadata(metadata$title, metadata$author, metadata$year)
    
    tibble(
      reference_text = str_trunc(ref, 100),
      extracted_title = metadata$title,
      extracted_author = metadata$author,
      extracted_year = metadata$year,
      found_doi = found_doi,
      search_status = if (is.na(found_doi)) "no DOI found" else "DOI found"
    )
  })
  
  # Display results for references without DOIs
  message("\nResults for references without existing DOIs:")
  print(new_dois)
  
  # Count successful DOI discoveries
  successful_discoveries <- sum(!is.na(new_dois$found_doi))
  message(sprintf("\nSuccessfully found DOIs for %d out of %d references without DOIs", 
                  successful_discoveries, length(refs_without_dois)))
} else {
  message("All references already have DOIs!")
  new_dois <- tibble()
}

# 9. Create comprehensive results table
message("\n", "="*60)
message("COMPREHENSIVE RESULTS TABLE")
message("="*60)

# Create unified results table
comprehensive_results <- tibble()

# Add references with existing DOIs
if (nrow(results) > 0) {
  existing_doi_results <- results %>%
    mutate(
      reference_type = "Has existing DOI",
      available_doi = doi,
      doi_status = case_when(
        status == "found" ~ "Existing DOI verified",
        status == "not found" ~ "Existing DOI invalid/not found",
        TRUE ~ status
      ),
      reference_title = title,
      found_doi = NA_character_,
      search_status = NA_character_
    ) %>%
    select(reference_type, available_doi, doi_status, reference_title, found_doi, search_status, container)
  
  comprehensive_results <- bind_rows(comprehensive_results, existing_doi_results)
}

# Add references without existing DOIs
if (nrow(new_dois) > 0) {
  no_doi_results <- new_dois %>%
    mutate(
      reference_type = "No existing DOI",
      available_doi = NA_character_,
      doi_status = "No existing DOI",
      reference_title = extracted_title,
      container = NA_character_
    ) %>%
    select(reference_type, available_doi, doi_status, reference_title, found_doi, search_status, container)
  
  comprehensive_results <- bind_rows(comprehensive_results, no_doi_results)
}

# Display comprehensive results
print(comprehensive_results)

# Summary statistics
total_refs <- nrow(comprehensive_results)
refs_with_existing_dois <- sum(comprehensive_results$reference_type == "Has existing DOI", na.rm = TRUE)
refs_without_existing_dois <- sum(comprehensive_results$reference_type == "No existing DOI", na.rm = TRUE)
valid_existing_dois <- sum(comprehensive_results$doi_status == "Existing DOI verified", na.rm = TRUE)
found_new_dois <- sum(!is.na(comprehensive_results$found_doi), na.rm = TRUE)

message("\n" , "="*60)
message("SUMMARY STATISTICS")
message("="*60)
message(sprintf("Total references processed: %d", total_refs))
message(sprintf("References with existing DOIs: %d", refs_with_existing_dois))
message(sprintf("  - Valid existing DOIs: %d", valid_existing_dois))
message(sprintf("  - Invalid existing DOIs: %d", refs_with_existing_dois - valid_existing_dois))
message(sprintf("References without existing DOIs: %d", refs_without_existing_dois))
message(sprintf("  - New DOIs found through search: %d", found_new_dois))
message(sprintf("  - No DOI found: %d", refs_without_existing_dois - found_new_dois))

# 10. (Optional) View the original results for existing DOIs
message("\nOriginal DOI verification results:")
print(results)
