# Workspace set up -------------------------------------------------------------

# We will be using two packages in this script: litsearchr, and igraph
# igraph is a litsearchr dependency, so does not require separate installation

# litsearchr is not on CRAN, so it must be installed with the remotes package
# If you do not have the remotes package, install it with:
#install.packages("remotes")

# If you do not have litsearchr installed, it can be installed with:
remotes::install_github("elizagrames/litsearchr")

#load the library
library(litsearchr)
library(here)

# Defining the scope of the question -------------------------------------------

# As with any review, before we begin we need to define the scope of the 
# question. Let's say we want to do a systematic review on location choice 
# studies in crime

# other options 
# We can use the 'PICO' format to define the question as:
# Population: law enforcement officers
# Intervention: de-escalation
# Comparator: officers who have not undergone training
# Outcome: police violence

# Generating the naive search --------------------------------------------------

# read the RIS fine 
dat.all <- litsearchr::import_results(directory="./20241004_Biblio")


# remove the duplicate articles based on its title
dat <- litsearchr::remove_duplicates(dat.all, field="title", method="exact")

# we can no of dublicates removed 
nrow(dat.all)-nrow(dat)

# To identify possible keywords, we use a knock-off version of the rapid automatic 
# keyword extraction algorithm (RAKE) which is called 'fakerake' in litsearchr 
# (the logic is the same but it doesn't require java)

all_keywords <-
  litsearchr::extract_terms(
    text = paste(dat$title, dat$abstract, dat$keywords),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

# We can look at some of these to get a sense of what they look like with
head(all_keywords, 50)

# Ignore these lines for now, we will come back to them later with explanation
terms_ignored <- c("academic press", "database record", "psycinfo database", "publication abstract",
                    "psycinfo database record", "rights reserved",
                    "copyright holder", "copyright applies", "email articles",
                    "express written", "express written permission",
                    "original published", "original published version",
                    "published version", "written permission", "study examines")

all_keywords <- all_keywords[!all_keywords%in%terms_ignored]

# We now create a document-feature matrix (DFM) out of the articles and the keywords
# In the DFM, each row is an article and each column is one of the potential
# keywords identified in the previous step. If a keyword/phrase appears in
# an article, it is a 1 in the DFM, else it is a 0

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(dat$title, dat$abstract, dat$keywords),
    features = all_keywords
  )

# We can use the DFM to create a keyword co-occurrence network because we know
# which terms appear in the same articles together (i.e. co-occur). 
naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

# The advantage of using a co-occurrence network for keyword selection is that
# we can use properties of networks to identify the most 'important' terms because
# they will have stronger co-occurrence relationships with other terms that
# are also important, whereas relatively  unimportant terms that occur infrequently
# and not in the right context (i.e. not with other good terms) will have low
# scores for importance in the network

# To see what I mean by that, let's plot the network
# This is a big network with a lot of potential terms, so it may take some time
igraph::plot.igraph(naivegraph, 
                    vertex.label.color = "#00000000", # make labels invisible
                    vertex.label.cex = 0.7, # size of labels
                    vertex.size = sqrt(igraph::strength(naivegraph)), # size of nodes
                    vertex.color = "white", # color of nodes
                    vertex.frame.color = "black", 
                    edge.width = 0.25, 
                    edge.color = "black", 
   
                                     edge.arrow.size = 0.25)
# Select a cutoff and finalize search terms ------------------------------------

# What we want to get out of the network are those big nodes in the center that
# are relatively important based on their node strength, which is a measure 
# of the weighted degrees of a node (i.e. how many connections does it have to
# other nodes, and how well connected are those nodes?)

node.strengths <- sort(igraph::strength(naivegraph), decreasing = T)
head(node.strengths, 50)

# Let's plot the node strengths so we get a sense of the shape of the distribution
# and can see which ones are towards the top
par(pty="s", las=1)
plot(sort(node.strengths), ylab="Node strength", pch=19, cex=0.5, axes=F)
axis(1); axis(2) # add axes
labels <- names(sort(node.strengths)) # pull out labels to add
labels[1:(length(labels)-20)] <- "" # we don't want to add all the labels!
text(sort(node.strengths), labels, pos=4, cex=0.5)

# We can now find a cutoff in node strength above which we will manually consider
# terms to use for the systematic review. We certainly don't want to consider
# all  terms, but we also don't want to miss some that may be useful in
# finding relevant literature. We want a cutoff that ignores the long tail of
# unimportant keywords but keeps the best ones

# Sometimes using 80/20 is a nice cutoff because of the Pareto principle
cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .20,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

# This gave us 196 terms to consider using for the search
# If we were cautious about missing terms, we could increase the percent
# in the code above
searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 50)

write.csv(searchterms, "./20241010_crime location choice.csv")

# We can read this back in now and pull out each of the elements of PIO

grouped_terms <- read.csv("./20241005_grouped.csv")
head(grouped_terms)
names(grouped_terms)

pop <- grouped_terms$term[grep("pop", grouped_terms$group)]
int <- grouped_terms$term[grep("int", grouped_terms$group)]
out <- grouped_terms$term[grep("out", grouped_terms$group)]


# At this point, we can revisit our naive terms and see if we want to add
# any of them back into our full search (generally recommended)

pop <- append(pop, c("offen", "criminal"))


# Write Boolean search from the terms ------------------------------------------

# First we need to set up a list that contains all our strings of terms
mysearchterms <- list(pop, int, out)

# litsearchr will separate terms within a concept category (i.e. an element of
# PICO) by 'OR' and then link categories with 'AND' so that the search logic is 
# to have at least one term from each category, but in any combination
my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

# When writing to a plain text file, the extra \ are required to render the 
# * and " properly on some unix-based systems; if copying straight from the 
# console, simply find and replace them in a text editor
# search in the database using the "my_search", save results and RIS and CSV or excel in different folders
my_search


# Checking search against benchmark articles -----------------------------------

# If there has been a previous review on a topic, we may have some idea of what
# articles we expect should be retrieved. Or, we could do some ad-hoc searching
# to find articles that we know are relevant (which was what we did in this case)

# There are probably lots more relevant article, but we will use these three
# as an example of how this process works in litsearchr
gold_standard <-
  c("Where offenders choose to attack: A discrete choice model of robberies in Chicago",
    "Target Selection Models with Preference Variation Between Offenders",
    "Modeling Micro-Level Crime Location Choice: Application of the Discrete Choice Framework to Crime at Places",
    "Location Choice of Snatching Offenders in Chennai City",
    "Burglar Target Selection: A Cross-national Comparison",
    "Biting Once Twice: the Influence of Prior on Subsequent Crime Location Choice")


gold_standard <-
  c("Target Choice During Extreme Events: A Discrete Spatial Choice Model of the 2011 London Riots",
    "Co‐offending and the Choice of Target Areas in Burglary",
    "A Sentimental Journey to Crime: Effects of Residential History on Crime Location Choice",
    "Modeling Micro-Level Crime Location Choice: Application of the Discrete Choice Framework to Crime at Places",
    "Adolescent Offenders’ Current Whereabouts Predict Locations of Their Future Crimes",
    "Where Offenders Choose to Attack: A Discrete Choice Model of Robberies in Chicago",
    "Go where the money is: modeling street robbers’ location choices",
    "Where Do Dealers Solicit Customers and Sell Them Drugs? A Micro-Level Multiple Method Study",
    "Learning Where to Offend: Effects of Past on Future Burglary Locations",
    "Effects of Residential History on Commercial Robbers’ Crime Location Choices",
    "Effects of Attractiveness, Opportunity and Accessibility to Burglars on Residential Burglary Rates of Urban Neighborhoods",
    "How Do Residential Burglars Select Target Areas? A New Approach to the Analysis of Criminal Location Choice",
    "Do Street Robbery Location Choices Vary Over Time of Day or Day of Week? A Test in Chicago",
    "Traveling Alone or Together? Neighborhood Context on Individual and Group Juvenile and Adult Burglary Decisions",
    "Relative Difference and Burglary Location: Can Ecological Characteristics of a Burglar’s Home Neighborhood Predict Offense Location?",
    "Formal Evaluation of the Impact of Barriers and Connectors on Residential Burglars’ Macro-Level Offending Location Choices",
    "Relationships Between Offenders’ Crime Locations and Different Prior Activity Locations as Recorded in Police Data",
    "The Importance of Importance Sampling: Exploring Methods of Sampling from Alternatives in Discrete Choice Models of Crime Location Choice",
    "Modelling Taste Heterogeneity Regarding Offence Location Choices",
    "Role of the Street Network in Burglars’ Spatial Decision-Making",
    "The Usefulness of Past Crime Data as an Attractiveness Index for Residential Burglars",
    "Testing Ecological Theories of Offender Spatial Decision Making Using a Discrete Choice Model",
    "Location Choice of Snatching Offenders in Chennai City",
    "Co-offenders’ Crime Location Choice: Do Co-offending Groups Commit Crimes in Their Shared Awareness Space?",
    "Biting Once, Twice: The Influence of Prior on Subsequent Crime Location Choice",
    "Residential Burglary Target Selection: An Analysis at the Property-Level Using Google Street View",
    "Do Migrant and Native Robbers Target Different Places?",
    "Do Juvenile, Young Adult, and Adult Offenders Target Different Places in the Chinese Context?",
    "Assessing the Influence of Prior on Subsequent Street Robbery Location Choices: A Case Study in ZG City, China",
    "Ambient Population and Surveillance Cameras: The Guardianship Role in Street Robbers’ Crime Location Choice",
    "Modelling the Spatial Decision Making of Terrorists: The Discrete Choice Approach",
    "Awareness × Opportunity: Testing Interactions Between Activity Nodes and Criminal Opportunity in Predicting Crime Location Choice",
    "Family Matters: Effects of Family Members’ Residential Areas on Crime Location Choice",
    "The Influence of Activity Space and Visiting Frequency on Crime Location Choice: Findings from an Online Self-Report Survey",
    "Crime Feeds on Legal Activities: Daily Mobility Flows Help to Explain Thieves’ Target Location Choices",
    "Burglar Target Selection: A Cross-national Comparison",
    "Target Selection Models with Preference Variation Between Offenders",
    "A Time for a Crime: Temporal Aspects of Repeat Offenders’ Crime Location Choices",
    "Right Place, Right Time? Making Crime Pattern Theory Time-Specific",
    "‘Location, Location, Location’: Effects of Neighborhood and House Attributes on Burglars’ Target Selection",
    "A Discrete Spatial Choice Model of Burglary Target Selection at the House-Level",
    "Burglars Blocked by Barriers? The Impact of Physical and Social Barriers on Residential Burglars’ Target Location Choices in China",
    "Investigating the Effect of People on the Street and Streetscape Physical Environment on the Location Choice of Street Theft Crime Offenders Using Street View Images and a Discrete Spatial Choice Model")


# We have litsearchr write a title-only search out of the benchmark articles
title_search <- litsearchr::write_title_search(titles=gold_standard)
title_search

# We then run that search in a few databases to check if the articles are 
# indexed and we would expect our search to retrieve them (if they aren't in a
# database, it would of course not be reasonable to retrieve them). 

# We then run our full search terms in databases we are using for benchmarking
# and export those results, which we can read back in
retrieved_articles <-  litsearchr::import_results(directory="./20241010_result_litsearch")

# And then check the results against our benchmark list to see if they were found
articles_found <- litsearchr::check_recall(true_hits = gold_standard,
                                           retrieved = retrieved_articles$title)

# As expected, the second benchmark article was not found because it isn't
# indexed in Criminal Justice Abstracts, but the other two have matches
articles_found
df_articles_found <- as.data.frame(articles_found) 
write.csv(df_articles_found, "articles_found.csv", )

# read and process the data, 
# read the Scopus data
scopus <- readr::read_csv(here::here("20241010_results_csv/scopus.csv"), col_types = readr::cols(.default = readr::col_character())) |>
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
google_scholar <- readr::read_csv(here::here("20241010_results_csv/google_scholar.csv"), col_types = readr::cols(.default = readr::col_character()))|>
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
WOS <- readxl::read_excel(here::here("20241010_results_csv/WOS.xls"), col_types = "text")|>
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

# read proquest data 
pro_quest <- readr::read_csv(here::here("20241010_results_csv/ProQuest.CSV"), col_types = readr::cols(.default = readr::col_character())) |>
  # Convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_")) |>
  # Convert all the characters to lowercase
  dplyr::mutate(dplyr::across(where(is.character), tolower)) |>
  # Rename columns
  dplyr::rename(`source_title` = `publication`,
                `year` = `pubdate`,
                `authors` = `author`) |>
  # Extract only the year (first 4 characters from the year column)
  dplyr::mutate(year = substr(year, 1, 4)) |>
  # Concatenate startpage and endpage into pages
  dplyr::mutate(pages = ifelse(!is.na(startpage) & !is.na(endpage), paste(startpage, endpage, sep = "-"), NA)) |>
  # Create columns not present to use later
  dplyr::mutate(`source` = "ProQuest") |>
  # Select the necessary columns
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`, `volume`, `issue`, `pages`, `doi`, `source`)

springer <- readr::read_csv(here::here("20241010_results_csv/springer.CSV"), col_types = readr::cols(.default = readr::col_character()))

springer <- readr::read_csv(here::here("20241010_results_csv/springer.CSV"), col_types = readr::cols(.default = readr::col_character())) |>
  # Convert the names to lowercase and replace space with "_"
  dplyr::rename_all(~stringr::str_replace_all(tolower(.), " ", "_")) |>
  # Convert all the characters to lowercase
  dplyr::mutate(dplyr::across(where(is.character), tolower))|>
  # Rename columns
  dplyr::rename(`source_title` = `publication_title`,
                `year` = `publication_year`,
                `title` = `item_title`,
                `volume` = `journal_volume`,
                `issue` = `journal_issue`,
                `doi` = `item_doi`) |>
  # Create columns not present to use later
  dplyr::mutate(`source` = "Springer",
                `pages` = " ",
                `abstract` = " ") |>
  # Select the necessary columns
  dplyr::select(`title`, `abstract`, `year`, `authors`, `source_title`, `volume`, `issue`, `pages`, `doi`, `source`)


# bind all rows (N = 1194)
data_all <- dplyr::bind_rows(WOS,scopus,  pro_quest, springer, google_scholar)

# count number of articles from each sources
source_summary <- data_all |>
  dplyr::count(source) |>
  dplyr::rename(Number_of_Entries = n)|>
  dplyr::arrange(desc(Number_of_Entries))

source_summary


# Normalize titles to remove punctuation, convert to lowercase, and remove extra spaces
data_all <- data_all |>
  # Remove any non-text symbols in the title, then remove extra spaces
  dplyr::mutate(title = stringr::str_replace_all(title, "[[:punct:]]", " ") |> 
                  stringr::str_squish() |> 
                  tolower())

# Remove all duplicates based on the normalized title
data_unique <- data_all |>
  dplyr::distinct(title, .keep_all = TRUE)  # Keeps the first occurrence of duplicates

# add extra columns for inclusion and exclusion
data_unique <- data_unique |> 
  dplyr::mutate(`select_reject_by (Wim/Christophe)` = " ", 
                `select_reject_by (Kural)` = " ",
                `notes` = " ",)|>
  # rearrange the column order 
  dplyr::select(`select_reject_by (Wim/Christophe)`, `select_reject_by (Kural)`,`notes`, everything())

# Save the new dataset with instructions as a CSV
write.csv(data_unique, "20241005_data_unique.csv", row.names = FALSE)
