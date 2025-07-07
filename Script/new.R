library(csvread)
library(readxl)
library(tibble)
library(janitor)
library(sf)
PROJECT    <- "C:/Users/kukumar/OneDrive - UGent/My Projects/Grafitti Project/"
DATAFOLDER <- paste(PROJECT, 
                    "Urban_Foraging_Archive/Research_data/Geo data/",
                    sep="")
WORKFOLDER <- paste(PROJECT, 
                    "Data/Workdata/",
                    sep="")
CrimeEventData <- read_xlsx(paste(DATAFOLDER, 
                                  "20220908_graffiti_data1.xlsx", sep=""))
view(CrimeEventData)
name(CrimeEventData)
head(CrimeEventData, n = 1)
crime_sf <- st_as_sf(crimes, coords = c("lat", "lon"), crs = 4326)
