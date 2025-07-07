

# clean the working environment 
rm(list = ls())

# read the Scopus data
scopus <- readr::read_csv(here::here("only_articles/scopus.csv"), col_types = readr::cols(.default = readr::col_character())) |>
  #convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_"))|>
  # convert all the characters to lowercase 
  dplyr::mutate( dplyr::across(where(is.character), tolower))|>
  # combine the start page and end page
  dplyr::mutate(pages = ifelse(!is.na(page_start) & !is.na(page_end), paste(page_start, page_end, sep = "-"), NA))|>
  dplyr::mutate(`source` = "Scopus") |>
  # select the necessary columns 
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`,
                `volume`, `issue`, `pages`, `doi`, `source`)  

# read Google scholar data 
google_scholar <- readr::read_csv(here::here("only_articles/google_scholar.csv"), col_types = readr::cols(.default = readr::col_character()))|>
  #convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_")) |>
  # convert all the characters to lowercase 
  dplyr::mutate( dplyr::across(where(is.character), tolower))|>
  # select the necessary columns 
  dplyr::rename(`issue` = `number`,
                `source_title` = `publication`)|>
  # create columns not present to use it later 
  dplyr::mutate(`source` = "Google scholar",`abstract` = " ",
                `doi` = " ") |>
  # select the necessary columns
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`,
                `volume`, `issue`,`pages`, `doi`,`source`)

# read web of science data 
WOS <- readxl::read_excel(here::here("only_articles/WOS.xls"), col_types = "text")|>
  #convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_"))|>
  # convert all the characters to lowercase 
  dplyr::mutate( dplyr::across(where(is.character), tolower))|>
  # combine the start page and end page
  dplyr::mutate(pages = ifelse(!is.na(start_page) & !is.na(end_page), paste(start_page, end_page, sep = "-"), NA))|>
  # select the necessary columns 
  dplyr::rename(`title` = `article_title`,
                `year` = `publication_year`)|>
  # create columns not present to use it later 
  dplyr::mutate(`source` = "WOS") |>
  # select the necessary columns
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`,
                `volume`, `issue`, `pages`, `doi`,`source`)  

# read Google scholar data 
pro_quest <- readr::read_csv(here::here("only_articles/proQuest.CSV"), col_types = readr::cols(.default = readr::col_character()))|>
  #convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_")) |>
  # convert all the characters to lowercase 
  dplyr::mutate( dplyr::across(where(is.character), tolower))|>
  # select the necessary columns 
  dplyr::rename(`source_title` = `publication`,
                `year` = `pubdate`,
                `authors` = `author`)|>
  dplyr::mutate(pages = ifelse(!is.na(startpage) & !is.na(endpage), paste(startpage, endpage, sep = "-"), NA))|>
  # create columns not present to use it later 
  dplyr::mutate(`source` = "ProQuest") |>
  # select the necessary columns
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`,
                `volume`, `issue`,`pages`, `doi`,`source`)


# bind all rows (N = 1194)
data_all <- dplyr::bind_rows(scopus, google_scholar, WOS, pro_quest)

# count number of articles from each sources
source_summary <- data_all |>
  dplyr::count(source) |>
  dplyr::rename(Number_of_Entries = n)|>
  dplyr::arrange(desc(Number_of_Entries))

source_summary


# Normalize titles to remove punctuation, convert to lowercase, and remove extra spaces
data_all <- data_all |>
  # remove any non text symbols in the title
  dplyr::mutate(title = stringr::str_replace_all(title, "[[:punct:]]", " ") |>
                  stringr::str_squish())


# remove all duplicates based on tile 
data_unique <- data_all |>
  dplyr::distinct(title, .keep_all = TRUE) # n = 744

# add extra columns for inclusion and exclusion
data_unique <- data_unique |> 
  dplyr::mutate(`select_reject_by (Wim/Christophe)` = " ", 
                `select_reject_by (Kural)` = " ",
                `notes` = " ",)|>
  # rearrange the column order 
  dplyr::select(`select_reject_by (Wim/Christophe)`, `select_reject_by (Kural)`,`notes`, everything())

# Save the new dataset with instructions as a CSV
write.csv(data_unique, "data_unique_only_atricels.csv", row.names = FALSE)



# Define your instruction row
instructions_row <- tibble::tibble(
  `S.No` = NA,
  `source` = NA,
  `country` = NA,
  `spatio_temporal` = NA,
  `select_reject_by (Wim/Christophe)` = "SA = selected by abstract, ST = selected by title, SF = selected by full text, RT = rejected by title, RA = rejected by abstract, RF = rejected by full text",
  `select_reject_by (Kural)` = "SA = selected by abstract, ST = selected by title, SF = selected by full text, RT = rejected by title, RA = rejected by abstract, RF = rejected by full text"
)



# write as csv
write.csv(data_with_add_col, "data_to_chekc.csv", )

# read checked file
data_checked <- readr::read_csv(here::here("20240703_data_chekced_Ahmed.csv"))|>
  dplyr::select(-`select_reject_by (Ahmed)`)|>
  dplyr::rename(`select_reject_by (Ahmed)` = `Selection by (SA =selected by abstract, ST= selected by title, SF=selected by full text, RT= rejected by title, RA rejected by abstract, RF=rejected by full text)`)|>
  dplyr::rename(`S.No` = `...1`)|>
  dplyr::select(`S.No`, `source`, `country`, `spatio_temporal`, `select_reject_by (Ahmed)`, `select_reject_by (Kural)`,
                `spatio_temporal`, country, everything())


# recode the SA =selected by abstract and others

data_checked$`select_reject_by (Ahmed)` <- dplyr::recode(data_checked$`select_reject_by (Ahmed)`,
                                                         "SA" = "selected by abstract",
                                                         "ST" = "selected by title", 
                                                         "SF" = "selected by full text", 
                                                         "RT" = "rejected by title", 
                                                         "RA" = "rejected by abstract", 
                                                         "RF" = "rejected by full text")

data_checked$`select_reject_by (Kural)` <- dplyr::recode(data_checked$`select_reject_by (Kural)`,
                                                         "SA" = "selected by abstract",
                                                         "ST" = "selected by title", 
                                                         "SF" = "selected by full text", 
                                                         "RT" = "rejected by title", 
                                                         "RA" = "rejected by abstract", 
                                                         "RF" = "rejected by full text")



