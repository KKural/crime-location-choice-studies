# Load required libraries
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(flextable)
library(officer)

# -----------------------------------------------------------------------------
# STEP 1: Load the dataset
# -----------------------------------------------------------------------------

data <- read_excel(here("Final_data.xlsx"))

# Generate APA in-text citations------------------
generate_in_text_citation <- function(authors, year) {
  author_list <- unlist(strsplit(authors, "; "))  # Split by "; "
  author_list <- sapply(author_list, function(x) str_trim(str_extract(x, "^[^,]+")))
  num_authors <- length(author_list)
  
  if (num_authors == 1) {
    return(paste0("(", author_list[1], ", ", year, ")"))
  } else if (num_authors == 2) {
    return(paste0("(", author_list[1], " & ", author_list[2], ", ", year, ")"))
  } else {
    return(paste0("(", author_list[1], " et al., ", year, ")"))
  }
}

# Apply citation function
data <- data %>%
  mutate(In_Text_Citation = mapply(generate_in_text_citation, Authors, Publication_Year))

# Filter for included studies and process columns -----------------------------
data_included <- data %>%
  filter(Inclusion_Exclusion == "Included") %>%
  mutate(Publication_Year = as.numeric(Publication_Year)) %>%
  arrange(desc(Publication_Year)) %>%
  mutate(S.No. = row_number()) %>%
  select(S.No., everything())

# Merge scenario and population-related variables------------------------------
# Merge corruption scenarios
data_included <- data_included %>%
  mutate(Corruption_Scenarios_Merged = apply(select(., starts_with("Corruption_Scenarios")), 1, function(x) {
    paste(na.omit(unique(x)), collapse = ", ")
  }))

# Merge target population variables
data_included <- data_included %>%
  mutate(Target_Population_Merged = apply(select(., Public_Sector_Actors, Private_Sector_Actors, 
                                                 General_Public, Students, 
                                                 Demographic_Group, Target_Population_Other), 
                                          1, function(x) {
                                            paste(na.omit(unique(x)), collapse = ", ")
                                          }))

# Create summary table for Appendix --------------------------------------------
short_table <- data_included %>%
  select(
    Covidence_ID,
    In_Text_Citation,
    Region,
    Corruption_Scenarios_Merged,
    Settings_Class,
    Target_Population_Merged,
    Techniques_Class,
    Quality_Appraisal_Total_Score
  )

# Format table (APA-style) using flextable--------------------------------------
Table_1 <- short_table %>%
  rename_with(~ str_replace_all(., "_", " ")) %>%
  flextable() %>%
  fit_to_width(max_width = 6.5) %>%
  fontsize(size = 12, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:ncol(short_table), align = "center", part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_bottom(border = officer::fp_border(width = 1), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  set_caption("Table 1. Summary of Included Studies in the Appendix")

Table_1

# Export to Word ---------------------------------------------------------------
save_as_docx(Table_1, path = here("Table_1.docx"))
