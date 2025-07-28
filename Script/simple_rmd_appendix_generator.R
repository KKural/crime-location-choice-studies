# Simple R Markdown Appendix Generator
# This script creates a clean .Rmd file that can be knitted to Word

# Load required libraries
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Set up output folder
analysis_date <- Sys.Date()
output_folder <- paste0(format(analysis_date, "%Y%m%d"), "_Simple_RMarkdown_Appendix")

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

cat("=== SIMPLE R MARKDOWN APPENDIX GENERATOR ===\n")
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

# Save the data for use in .Rmd
save(appendix_table, file = file.path(output_folder, "appendix_data.RData"))

cat("Prepared data with", nrow(appendix_table), "included studies\n")

# -----------------------------------------------------------------------------
# STEP 2: Create simple R Markdown file
# -----------------------------------------------------------------------------

# Create the .Rmd content
rmd_content <- c(
  "---",
  "title: 'Appendix: Complete List of Included Studies'",
  "output:",
  "  word_document:",
  "    reference_docx: null",
  "    toc: false",
  "    number_sections: false",
  "date: '`r Sys.Date()`'",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
  "library(flextable)",
  "library(dplyr)",
  "library(officer)",
  "",
  "# Load the data",
  "load('appendix_data.RData')",
  "```",
  "",
  "This appendix provides a comprehensive overview of all studies included in the systematic review.",
  "",
  "## Table A1. Summary of Included Studies",
  "",
  "```{r table}",
  "# Create flextable",
  "Table_A1 <- appendix_table %>%",
  "  rename_with(~ gsub('_', ' ', .)) %>%",
  "  flextable() %>%",
  "  fit_to_width(max_width = 6.5) %>%",
  "  fontsize(size = 10, part = 'all') %>%",
  "  font(fontname = 'Times New Roman', part = 'all') %>%",
  "  align(j = 1:2, align = 'left', part = 'all') %>%",
  "  align(j = 3:ncol(appendix_table), align = 'center', part = 'all') %>%",
  "  bold(part = 'header') %>%",
  "  border_remove() %>%",
  "  hline_top(border = fp_border(width = 1), part = 'header') %>%",
  "  hline_bottom(border = fp_border(width = 1), part = 'body') %>%",
  "  padding(padding = 3, part = 'all')",
  "",
  "Table_A1",
  "```",
  "",
  "*Note: Reference ID corresponds to the numbered references below.*",
  "",
  "\\newpage",
  "",
  "## References",
  "",
  "```{r references, results='asis'}",
  "# Generate simple references from the data",
  "for (i in 1:nrow(appendix_table)) {",
  "  # Format author names (simplified)",
  "  authors <- appendix_table$Authors[i]",
  "  year <- appendix_table$Publication_Year[i]",
  "  ",
  "  # Create basic reference format (Nature Human Behaviour style placeholder)",
  "  # You can improve this by adding journal, title, etc. from your BibTeX file",
  "  reference <- paste0(i, '. ', authors, ' (', year, '). [Title and journal information to be added from BibTeX file].')",
  "  ",
  "  cat(reference, '\\n\\n')",
  "}",
  "```",
  "",
  "---",
  "",
  "*References are numbered to correspond with the Reference ID column in Table A1. Complete bibliographic information should be added from the BibTeX file or manually.*"
)

# Save the .Rmd file
rmd_file <- file.path(output_folder, "appendix.Rmd")
writeLines(rmd_content, rmd_file)

cat("R Markdown file created:", rmd_file, "\n")

# -----------------------------------------------------------------------------
# STEP 3: Create simple instructions
# -----------------------------------------------------------------------------

instructions <- c(
  "# Simple Appendix Instructions",
  "",
  "## Files Created:",
  "1. **appendix.Rmd** - Main R Markdown file to knit",
  "2. **appendix_data.RData** - Data file for the .Rmd",
  "3. **instructions.md** - This file",
  "",
  "## How to Use:",
  "1. Open `appendix.Rmd` in RStudio",
  "2. Make sure required packages are installed:",
  "   - `install.packages(c('flextable', 'dplyr', 'officer'))`",
  "3. Click 'Knit' button to generate Word document",
  "4. The output will include:",
  "   - Professional table with flextable formatting",
  "   - Basic numbered references (to be enhanced)",
  "",
  "## Table Features:",
  "- Reference ID [1], [2], [3], etc.",
  "- Clean professional formatting",
  "- Times New Roman font",
  "- Proper column alignment",
  "- Word-ready output",
  "",
  "## To Enhance References:",
  "Replace the references section in the .Rmd file with proper Nature Human Behaviour formatting using your BibTeX data.",
  "",
  paste("Generated on:", Sys.Date())
)

instructions_file <- file.path(output_folder, "instructions.md")
writeLines(instructions, instructions_file)

# Save table data as CSV for reference
csv_file <- file.path(output_folder, "appendix_table_data.csv")
write.csv(appendix_table, csv_file, row.names = FALSE)

cat("Instructions saved:", instructions_file, "\n")
cat("Table data saved:", csv_file, "\n")

cat("\n=== SIMPLE R MARKDOWN APPENDIX READY ===\n")
cat("Files in folder:", output_folder, "\n")
cat("1. appendix.Rmd - MAIN FILE TO KNIT\n")
cat("2. appendix_data.RData - Data for the .Rmd\n")
cat("3. instructions.md - Usage guide\n")
cat("4. appendix_table_data.csv - Raw table data\n")
cat("\nNext steps:\n")
cat("1. Open appendix.Rmd in RStudio\n")
cat("2. Install packages if needed: install.packages(c('flextable', 'dplyr', 'officer'))\n")
cat("3. Click 'Knit' to create Word document\n")
cat("4. Enhance references as needed\n")
