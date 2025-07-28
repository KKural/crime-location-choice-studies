# Simple R Markdown Appendix Generator
# This script creates a clean .Rmd file for the appendix

# Load required libraries
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Set up output folder
analysis_date <- Sys.Date()
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_RMarkdown_Appendix")

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

cat("=== R MARKDOWN APPENDIX GENERATOR ===\n")
cat("Output folder:", output_folder, "\n\n")

# -----------------------------------------------------------------------------
# STEP 1: Load and prepare the data
# -----------------------------------------------------------------------------

data <- read_excel(here("Final_data.xlsx"))

# Filter for included studies and add Reference_ID
data_included <- data %>%
  filter(Inclusion_Exclusion == "Included") %>%
  mutate(Publication_Year = as.numeric(Publication_Year)) %>%
  arrange(desc(Publication_Year)) %>%
  mutate(Reference_ID = row_number()) %>%  # Add numbered references
  select(Reference_ID, everything())

# Merge scenario and population variables
data_included <- data_included %>%
  mutate(Corruption_Scenarios_Merged = apply(select(., starts_with("Corruption_Scenarios")), 1, function(x) {
    paste(na.omit(unique(x[x != ""])), collapse = ", ")
  })) %>%
  mutate(Target_Population_Merged = apply(select(., Public_Sector_Actors, Private_Sector_Actors, 
                                                 General_Public, Students, 
                                                 Demographic_Group, Target_Population_Other), 
                                          1, function(x) {
                                            paste(na.omit(unique(x[x != ""])), collapse = ", ")
                                          }))

# Create the appendix table
appendix_table <- data_included %>%
  select(
    Reference_ID,
    Authors,
    Publication_Year,
    Region,
    Corruption_Scenarios_Merged,
    Settings_Class,
    Target_Population_Merged,
    Techniques_Class,
    Quality_Appraisal_Total_Score
  )

cat("Prepared data with", nrow(appendix_table), "included studies\n")

# -----------------------------------------------------------------------------
# STEP 2: Read BibTeX file for references
# -----------------------------------------------------------------------------

# Check if export.bib exists
bib_file <- here("export.bib")
if (file.exists(bib_file)) {
  cat("Reading BibTeX file...\n")
  bib_data <- ReadBib(bib_file, check = FALSE)
  cat("Found", length(bib_data), "entries in BibTeX file\n")
} else {
  cat("Warning: export.bib not found. Will create placeholder references.\n")
  bib_data <- NULL
}

# -----------------------------------------------------------------------------
# STEP 3: Generate R Markdown Content
# -----------------------------------------------------------------------------

# Initialize R Markdown content with YAML header
rmd_content <- c(
  "---",
  "title: 'Appendix: Complete List of Included Studies'",
  "output:",
  "  word_document:",
  "    reference_docx: null",
  "    toc: false",
  "    number_sections: false",
  "  pdf_document:",
  "    toc: false",
  "    number_sections: false",
  "  html_document:",
  "    toc: false",
  "    number_sections: false",
  "date: '`r Sys.Date()`'",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
  "library(knitr)",
  "library(kableExtra)",
  "library(dplyr)",
  "```",
  "",
  "This appendix provides a comprehensive overview of all studies included in the systematic review, along with their key characteristics and complete bibliographic references.",
  "",
  "## Table A1. Summary of Included Studies",
  ""
)

# Function to clean text for markdown (escape special characters)
clean_for_markdown <- function(text) {
  if (is.na(text) || text == "") return("")
  # Escape pipes and other markdown special characters
  text <- str_replace_all(text, "\\|", "&#124;")
  text <- str_replace_all(text, "\\*", "&#42;")
  text <- str_replace_all(text, "_", "&#95;")
  return(text)
}

# Create kable table for R Markdown
table_code <- c(
  "```{r appendix-table}",
  "# Load the data",
  paste0("appendix_data <- read.csv('", csv_file, "')"),
  "",
  "# Create formatted table",
  "appendix_data %>%",
  "  select(",
  "    Reference_ID,",
  "    Authors,", 
  "    Publication_Year,",
  "    Region,",
  "    Corruption_Scenarios_Merged,",
  "    Settings_Class,",
  "    Target_Population_Merged,",
  "    Techniques_Class,",
  "    Quality_Appraisal_Total_Score",
  "  ) %>%",
  "  mutate(",
  "    Reference_ID = paste0('[', Reference_ID, ']'),",
  "    Authors = stringr::str_trunc(Authors, 30),",
  "    Corruption_Scenarios_Merged = stringr::str_trunc(Corruption_Scenarios_Merged, 25),",
  "    Settings_Class = stringr::str_trunc(Settings_Class, 20),",
  "    Target_Population_Merged = stringr::str_trunc(Target_Population_Merged, 25),",
  "    Techniques_Class = stringr::str_trunc(Techniques_Class, 20)",
  "  ) %>%",
  "  kable(",
  "    col.names = c('Ref.', 'Authors', 'Year', 'Region', 'Corruption Scenarios', ",
  "                  'Settings', 'Target Population', 'Techniques', 'Quality'),",
  "    align = c('c', 'l', 'c', 'c', 'c', 'c', 'c', 'c', 'c'),",
  "    caption = 'Summary of Included Studies'",
  "  ) %>%",
  "  kable_styling(",
  "    bootstrap_options = c('striped', 'hover', 'condensed'),",
  "    font_size = 10,",
  "    full_width = TRUE",
  "  ) %>%",
  "  column_spec(1, width = '0.8cm') %>%",
  "  column_spec(2, width = '3cm') %>%",
  "  column_spec(3, width = '1cm') %>%",
  "  column_spec(4:9, width = '2cm')",
  "```",
  ""
)

rmd_content <- c(rmd_content, table_code)

# Add some spacing and notes
rmd_content <- c(
  rmd_content,
  "*Note: Ref. = Reference number corresponding to the bibliography below; Quality ratings: High, Medium, Low based on study design and methodology assessment.*",
  "",
  "\\newpage",  # Page break for Word conversion
  "",
  "## References",
  ""
)

# -----------------------------------------------------------------------------
# STEP 4: Format References (Nature Human Behaviour Style)
# -----------------------------------------------------------------------------

# Function to format authors in Nature Human Behaviour style
format_authors_nhb <- function(authors_string) {
  if (is.na(authors_string) || authors_string == "") return("Unknown Author")
  
  # Split authors by semicolon
  authors_list <- str_split(authors_string, ";")[[1]]
  authors_list <- str_trim(authors_list)
  
  formatted_authors <- sapply(authors_list, function(author) {
    # Split by comma to get Last, First Middle
    parts <- str_split(author, ",")[[1]]
    if (length(parts) >= 2) {
      last_name <- str_trim(parts[1])
      first_names <- str_trim(parts[2])
      
      # Convert first names to initials
      name_parts <- str_split(first_names, "\\s+")[[1]]
      initials <- paste0(substr(name_parts, 1, 1), ".", collapse = " ")
      
      return(paste0(last_name, ", ", initials))
    } else {
      return(str_trim(author))
    }
  })
  
  # Join authors with commas and & for last author
  if (length(formatted_authors) == 1) {
    return(formatted_authors[1])
  } else if (length(formatted_authors) == 2) {
    return(paste(formatted_authors, collapse = " & "))
  } else {
    return(paste0(paste(formatted_authors[-length(formatted_authors)], collapse = ", "), 
                  " & ", formatted_authors[length(formatted_authors)]))
  }
}

# Function to format a single reference in Nature Human Behaviour style
format_reference_nhb <- function(authors, title, journal, year, volume = "", pages = "", doi = "", ref_num) {
  # Format authors
  formatted_authors <- format_authors_nhb(authors)
  
  # Clean and format title
  clean_title <- if (!is.na(title) && title != "") {
    # Remove quotes and clean
    title <- str_replace_all(title, '^"|"$', '')
    paste0(title, ".")
  } else {
    "Unknown title."
  }
  
  # Format journal (italicized)
  formatted_journal <- if (!is.na(journal) && journal != "") {
    paste0("*", journal, "*")
  } else {
    "*Unknown Journal*"
  }
  
  # Format volume and pages
  vol_pages <- ""
  if (!is.na(volume) && volume != "") {
    vol_pages <- paste0("**", volume, "**")
    if (!is.na(pages) && pages != "") {
      vol_pages <- paste0(vol_pages, ", ", pages)
    }
  } else if (!is.na(pages) && pages != "") {
    vol_pages <- pages
  }
  
  # Format year
  formatted_year <- if (!is.na(year) && year != "") {
    paste0("(", year, ")")
  } else {
    "(Year unknown)"
  }
  
  # Format DOI
  doi_part <- if (!is.na(doi) && doi != "") {
    paste0(" https://doi.org/", doi)
  } else {
    ""
  }
  
  # Combine all parts
  reference_parts <- c(
    paste0(ref_num, ". ", formatted_authors),
    clean_title,
    formatted_journal,
    if (vol_pages != "") vol_pages,
    formatted_year
  )
  
  # Join parts and add DOI if available
  reference <- paste(reference_parts[reference_parts != ""], collapse = " ")
  if (doi_part != "") {
    reference <- paste0(reference, doi_part, ".")
  } else {
    reference <- paste0(reference, ".")
  }
  
  return(reference)
}

# Generate references
cat("Generating Nature Human Behaviour style references...\n")

if (!is.null(bib_data) && length(bib_data) > 0) {
  # Use BibTeX data
  for (i in 1:min(nrow(appendix_table), length(bib_data))) {
    entry <- bib_data[[i]]
    
    # Extract information
    authors <- if (!is.null(entry$author)) as.character(entry$author) else appendix_table$Authors[i]
    title <- if (!is.null(entry$title)) as.character(entry$title) else "Unknown title"
    journal <- if (!is.null(entry$journal)) as.character(entry$journal) else "Unknown journal"
    year <- if (!is.null(entry$year)) as.character(entry$year) else appendix_table$Publication_Year[i]
    volume <- if (!is.null(entry$volume)) as.character(entry$volume) else ""
    pages <- if (!is.null(entry$pages)) as.character(entry$pages) else ""
    doi <- if (!is.null(entry$doi)) as.character(entry$doi) else ""
    
    # Format reference
    reference <- format_reference_nhb(authors, title, journal, year, volume, pages, doi, i)
    rmd_content <- c(rmd_content, reference, "")
  }
} else {
  # Use data from Excel file as fallback
  for (i in 1:nrow(appendix_table)) {
    # Create placeholder reference using available data
    reference <- format_reference_nhb(
      appendix_table$Authors[i],
      "Title not available from data source",
      "Journal information not available",
      as.character(appendix_table$Publication_Year[i]),
      "", "", "", i
    )
    rmd_content <- c(rmd_content, reference, "")
  }
}

# -----------------------------------------------------------------------------
# STEP 5: Save files
# -----------------------------------------------------------------------------

# Save table data as CSV first (needed for R Markdown)
csv_file <- file.path(output_folder, "appendix_table_data.csv")
write.csv(appendix_table, csv_file, row.names = FALSE)
cat("Table data saved to:", csv_file, "\n")

# Save R Markdown file
rmd_file <- file.path(output_folder, "appendix.Rmd")
writeLines(rmd_content, rmd_file)
cat("R Markdown appendix saved to:", rmd_file, "\n")

# Create a knitting script for easy rendering
knit_script <- c(
  "# Script to render the appendix R Markdown file",
  "library(rmarkdown)",
  "library(knitr)",
  "library(kableExtra)",
  "",
  "# Set working directory to the output folder",
  paste0("setwd('", output_folder, "')"),
  "",
  "# Render to Word document",
  "rmarkdown::render('appendix.Rmd', output_format = 'word_document')",
  "",
  "# Render to PDF (optional)",
  "# rmarkdown::render('appendix.Rmd', output_format = 'pdf_document')",
  "",
  "# Render to HTML (optional)", 
  "# rmarkdown::render('appendix.Rmd', output_format = 'html_document')",
  "",
  "cat('Appendix rendered successfully!\\n')"
)

knit_script_file <- file.path(output_folder, "render_appendix.R")
writeLines(knit_script, knit_script_file)

# Create alternative simple markdown version (backup)
simple_markdown_content <- c(
  "# Appendix: Complete List of Included Studies",
  "",
  "## Table A1. Summary of Included Studies",
  ""
)

# Create simple markdown table
table_header <- "| Ref. | Authors | Year | Region | Corruption Scenarios | Settings | Target Population | Techniques | Quality |"
table_separator <- "|:----:|:--------|:----:|:------:|:-------------------:|:--------:|:----------------:|:----------:|:-------:|"

simple_markdown_content <- c(simple_markdown_content, table_header, table_separator)

# Add table rows
for (i in 1:nrow(appendix_table)) {
  row_data <- c(
    paste0("[", appendix_table$Reference_ID[i], "]"),
    clean_for_markdown(str_trunc(appendix_table$Authors[i], 30)),
    appendix_table$Publication_Year[i],
    clean_for_markdown(appendix_table$Region[i]),
    clean_for_markdown(str_trunc(appendix_table$Corruption_Scenarios_Merged[i], 25)),
    clean_for_markdown(str_trunc(appendix_table$Settings_Class[i], 20)),
    clean_for_markdown(str_trunc(appendix_table$Target_Population_Merged[i], 25)),
    clean_for_markdown(str_trunc(appendix_table$Techniques_Class[i], 20)),
    clean_for_markdown(appendix_table$Quality_Appraisal_Total_Score[i])
  )
  
  table_row <- paste("|", paste(row_data, collapse = " | "), "|")
  simple_markdown_content <- c(simple_markdown_content, table_row)
}

# Add references to simple markdown
simple_markdown_content <- c(
  simple_markdown_content,
  "",
  "## References",
  ""
)

# Add the same references to simple markdown
if (!is.null(bib_data) && length(bib_data) > 0) {
  for (i in 1:min(nrow(appendix_table), length(bib_data))) {
    entry <- bib_data[[i]]
    authors <- if (!is.null(entry$author)) as.character(entry$author) else appendix_table$Authors[i]
    title <- if (!is.null(entry$title)) as.character(entry$title) else "Unknown title"
    journal <- if (!is.null(entry$journal)) as.character(entry$journal) else "Unknown journal"
    year <- if (!is.null(entry$year)) as.character(entry$year) else appendix_table$Publication_Year[i]
    volume <- if (!is.null(entry$volume)) as.character(entry$volume) else ""
    pages <- if (!is.null(entry$pages)) as.character(entry$pages) else ""
    doi <- if (!is.null(entry$doi)) as.character(entry$doi) else ""
    
    reference <- format_reference_nhb(authors, title, journal, year, volume, pages, doi, i)
    simple_markdown_content <- c(simple_markdown_content, reference, "")
  }
} else {
  for (i in 1:nrow(appendix_table)) {
    reference <- format_reference_nhb(
      appendix_table$Authors[i],
      "Title not available from data source", 
      "Journal information not available",
      as.character(appendix_table$Publication_Year[i]),
      "", "", "", i
    )
    simple_markdown_content <- c(simple_markdown_content, reference, "")
  }
}

# Save simple markdown version
simple_md_file <- file.path(output_folder, "appendix_simple.md")
writeLines(simple_markdown_content, simple_md_file)

# Create summary
summary_content <- c(
  "# Appendix Generation Summary",
  "",
  paste("- Generated on:", Sys.Date()),
  paste("- Total studies included:", nrow(appendix_table)),
  paste("- Reference style: Nature Human Behaviour"),
  paste("- BibTeX file found:", !is.null(bib_data)),
  "",
  "## Generated Files:",
  "1. **appendix.md** - Main appendix file",
  "2. **appendix_word_ready.md** - Word-optimized version",
  "3. **appendix_table_data.csv** - Table data in CSV format",
  "4. **summary.md** - This summary file",
  "",
  "## How to Convert to Word:",
  "1. Use Pandoc: `pandoc appendix_word_ready.md -o appendix.docx`",
  "2. Or open in Word and save as .docx",
  "3. Or use online markdown to Word converters",
  "",
  "## Reference Format:",
  "Nature Human Behaviour style with:",
  "- Numbered references [1], [2], etc.",
  "- Author format: Last, F. M.",
  "- Journal names in italics",
  "- Volume numbers in bold",
  "- DOIs when available"
)

summary_file <- file.path(output_folder, "summary.md")
writeLines(summary_content, summary_file)

cat("\n=== APPENDIX GENERATION COMPLETE ===\n")
cat("Output folder:", output_folder, "\n")
cat("Files created:\n")
cat("1. appendix.md - Main appendix\n")
cat("2. appendix_word_ready.md - Word-optimized version\n") 
cat("3. appendix_table_data.csv - Data file\n")
cat("4. summary.md - Generation summary\n")
cat("\nTo convert to Word document:\n")
cat("- Use: pandoc appendix_word_ready.md -o appendix.docx\n")
cat("- Or open the .md file in Word and save as .docx\n")
