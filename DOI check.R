# ---------------------------------------------
# Script: verify_bib_titles.R
# Purpose: Auto-parse your .txt bibliography, then
#          verify that each DOI’s title matches your local record.
# ---------------------------------------------

# 0. (Optionally) install packages if needed:
# install.packages(c("rcrossref","stringr","dplyr","purrr","tibble","stringdist"))

# 1. Load libraries
library(rcrossref)    # for cr_works()
library(stringr)      # for regex/string ops
library(dplyr)        # for data manipulation
library(purrr)        # for map_dfr()
library(tibble)       # for tribble() / tibble()
library(stringdist)   # for fuzzy string distance

# 2. Read each line of your bibliography
lines <- readLines("my_bibliography.txt", warn = FALSE)

# 3. Parse out DOI and expected title from each line
#    - Title is the first “…” pair
#    - DOI is the substring "doi:..." to end-of-line
bib_df <- tibble(raw = lines) %>%
  filter(str_detect(raw, "doi:|DOI:")) %>%         # only lines with a DOI
  mutate(
    expected_title = str_extract(raw, "(?<=“)[^”]+(?=”)"),      # between the first pair of “ ”
    doi            = str_remove(str_extract(raw, "(?i)doi:.*$"), "(?i)^doi:\\s*")
  ) %>%
  select(doi, expected_title)

# 4. Clean trailing punctuation from DOI
bib_df <- bib_df %>%
  mutate(
    doi = str_trim(doi),
    doi = str_remove(doi, "[\\.;,\\)]+$")
  )

# 5. Query CrossRef for each DOI
crossref_df <- map_dfr(bib_df$doi, function(d) {
  res <- try(cr_works(d), silent = TRUE)
  if (inherits(res, "try-error") || nrow(res$data)==0) {
    tibble(doi = d, status = "not found", crossref_title = NA_character_)
  } else {
    row <- res$data[1, ]
    title <- if ("title" %in% names(row)) paste(row$title, collapse = " ") else NA_character_
    tibble(doi = d, status = "found", crossref_title = title)
  }
})

# 6. Join your expected titles to the CrossRef results
comparison <- bib_df %>%
  left_join(crossref_df, by = "doi") %>%
  
  # 7. Normalize titles for comparison
  mutate(
    expected_norm = expected_title %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish(),
    
    crossref_norm = crossref_title %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish(),
    
    # 8. Exact match?
    exact_match = (!is.na(expected_norm) & !is.na(crossref_norm) &
                     expected_norm == crossref_norm),
    
    # 9. Fuzzy match (Jaro–Winkler distance < 0.10)
    jw_distance = stringdist(expected_norm, crossref_norm, method = "jw"),
    fuzzy_match = ifelse(is.na(jw_distance), FALSE, jw_distance < 0.10)
  )

# 10. Report
cat("\n=== DOIs not found in CrossRef (expected for books) ===\n")
not_found <- comparison %>% filter(status == "not found")
if (nrow(not_found)>0) {
  print(not_found$doi)
} else {
  cat("None\n")
}

cat("\n=== Title mismatches (exact or fuzzy) ===\n")
mismatches <- comparison %>%
  filter(status=="found" & (!exact_match | !fuzzy_match)) %>%
  select(doi, expected_title, crossref_title, exact_match, jw_distance)
if (nrow(mismatches)>0) {
  print(mismatches)
} else {
  cat("All titles match (exactly or within fuzzy threshold).\n")
}

# (Optional) view full comparison
# print(comparison)
