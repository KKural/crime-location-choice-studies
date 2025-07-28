library(here)
library(dplyr)
library(tidyr)
library(stringr)

data <- readxl::read_excel(here::here("Final_data.xlsx"))

# Function to generate APA in-text citations 
generate_in_text_citation <- function(authors, year) {
  author_list <- unlist(strsplit(authors, "; "))  # Split authors by "; "
  num_authors <- length(author_list)
  
  # Extract only last names
  author_list <- sapply(author_list, function(x) str_trim(str_extract(x, "^[^,]+"))) 
  
  if (num_authors == 1) {
    return(paste0("(", author_list[1], ", ", year, ")"))  # One author
  } else if (num_authors == 2) {
    return(paste0("(", author_list[1], " & ", author_list[2], ", ", year, ")"))  # Two authors
  } else {
    return(paste0("(", author_list[1], " et al., ", year, ")"))  # Three or more authors
  }
}

# Apply function to dataset
data <- data %>%
  mutate(In_Text_Citation = mapply(generate_in_text_citation, Authors, Publication_Year))

# View the first few citations
head(data$In_Text_Citation)

# filter only the included items
data_included <- data |>
  dplyr::filter(Inclusion_Exclusion == "Included")

data_included <- data_included %>%
  mutate(Publication_Year = as.numeric(Publication_Year)) %>%  # Convert to numeric
  arrange(desc(Publication_Year)) %>%  # Sort by year (newest first)
  mutate(S.No. = row_number())|>
  select(S.No., everything())


# Create a new column merging all corruption scenario columns
data_included <- data_included %>%
  mutate(Corruption_Scenarios_Merged = apply(select(., starts_with("Corruption_Scenarios")), 1, function(x) {
    paste(na.omit(unique(x)), collapse = ", ")
  }))

head(data_included$Corruption_Scenarios_Merged)

# Create a new column by merging target population-related columns
data_included <- data_included %>%
  mutate(Target_Population_Merged = apply(select(., Public_Sector_Actors, Private_Sector_Actors, 
                                                 General_Public, Students, 
                                                 Demographic_Group, Target_Population_Other), 
                                          1, function(x) {
                                            paste(na.omit(unique(x)), collapse = ", ")
                                          }))

# View the updated dataset
head(data_included$Target_Population_Merged)

# create the short table with columns needed
short_table <- data_included |>
  dplyr::select(Covidence_ID, In_Text_Citation, Region, Corruption_Scenarios_Merged,
                Settings_Class, Target_Population_Merged, Techniques_Class,
                Quality_Appraisal_Total_Score)


library(flextable)
library(dplyr)
library(stringr)
library(officer)

Table_1 <- short_table |>
  dplyr::rename_with(~ str_replace_all(., "_", " ")) |>
  flextable() |>
  fit_to_width(max_width = 6.5) |>
  fontsize(size = 12, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  align(j = 1, align = "left", part = "all") |>
  align(j = 2:ncol(short_table), align = "center", part = "all") |>
  bold(part = "header") |>
  border_remove() |>
  hline_top(border = fp_border(width = 1), part = "header") |>
  hline_bottom(border = fp_border(width = 1), part = "body") |>
  padding(padding = 4, part = "all") |>
  set_caption(caption = "Table 1.")

Table_1
flextable::save_as_docx(Table_1, path = here::here("Table_1.docx"))

write.csv(short_table, "Table_1.csv")

install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)

# Load the Excel file
df <- read_excel("Final_data.xlsx")

# Calculate frequency of each country
country_freq <- df %>%
  group_by(Study_Country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Calculate the percentage for the most predominant country
total_count <- nrow(df)
country_freq <- country_freq %>%
  mutate(percentage = (count / total_count) * 100)

# View the result
country_freq

most_predominant_country <- country_freq %>%
  slice(1)  # Get the first row (most frequent)

most_predominant_country

# Use the filtered and updated dataset
Corruption_Scenarios_Merged_freq <- data_included %>%
  group_by(Corruption_Scenarios_Merged) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = (count / nrow(data_included)) * 100)

# Now, to view the results:
print(Corruption_Scenarios_Merged_freq)


# Frequency and percentage for Settings_Class
settings_class_freq <- df %>%
  group_by(Settings_Class) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = (count / nrow(df)) * 100)

settings_class_freq

# Check that Target_Population_Merged exists in the dataset
if("Target_Population_Merged" %in% colnames(data_included)) {
  # Use the filtered and updated dataset to calculate frequencies
  Target_Population_Merged_freq <- data_included %>%
    group_by(Target_Population_Merged) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(percentage = (count / nrow(data_included)) * 100)
  
  # View the result
  print(Target_Population_Merged_freq)
} else {
  print("Target_Population_Merged column not found in data_included!")
}


# Frequency and percentage for Techniques_Class
techniques_class_freq <- df %>%
  group_by(Techniques_Class) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = (count / nrow(df)) * 100)

techniques_class_freq

# Frequency and percentage for Quality_Appraisal_Total_Score
quality_appraisal_freq <- df %>%
  group_by(Quality_Appraisal_Total_Score) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = (count / nrow(df)) * 100)

quality_appraisal_freq


# Frequency and percentage for Data_Source
data_source_freq <- df %>%
  group_by(Data_Source) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = (count / nrow(df)) * 100)

data_source_freq





