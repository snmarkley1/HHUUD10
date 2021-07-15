
#########################################################################
#########################################################################
###                                                                   ###
###                    LOAD & CLEAN AIRPORT DATA                      ###
###                                                                   ###
#########################################################################
#########################################################################

## prepare workspace
rm(list = ls())
options(scipen = 999)
options(digits = 6)
getwd()  # should be ~/HIST_HU_URB

## load or install packages
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])
  
  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

## LOAD NEEDED PACKAGSE
packages(tidyverse)
packages(foreign)  # for dbf reading/writing
packages(sf) # for reading gdb


##################################################################
##  STEP 1: LOAD AIRPORT DATA                                   ##
##################################################################

## Import data from FAA (Date: June 28, 2021)
airports_import <- read.delim(
  "https://www.faa.gov/airports/airport_safety/airportdata_5010/menu/nfdcfacilitiesexport.cfm?Region=&District=&State=&County=&City=&Use=&Certification=",
  header = TRUE,
  sep = "\t") %>%
  as_tibble(.) %>%
  print()

# Save locally
#write_csv(airports_import, "tables/airports_raw.csv") 


## Organize for Export into ArcGIS Pro
airports <- airports_import %>%
  # include only airports in Lower 48 + DC
  filter(!State %in% c("AK", "AS", "GU","HI", "MP", "PR", "QM", "QW", "VI", "XL")) %>%
  # filter out heliports, gliderports, etc
  filter(Type == "AIRPORT") %>%
  # keep only relevant columns
  select(LocationID, State, ActivationDate) %>%
  rename(
    LOC_ID = 1,  # match Esri airport attribute name
    STATE = 2,  # keep the state
    ACT_DATE = 3  # date in which the airport was added to the NFDC airport database (https://www.faa.gov/airports/airport_safety/airportdata_5010/media/airport-data-dictionaries.xlsx)
  ) %>%
  # fix up columns
  mutate(
    LOC_ID = str_extract(LOC_ID, "\\w+"),  # keep only alphabetic letters (removes apostrophe)
    ACT_DATE = as.integer(str_extract(ACT_DATE, "\\d+$"))  # keep only ending digits (keeps years, removes day & month)
  ) %>%
  print() # n = 12,539
  

## Save out to tables folder
write.dbf(as.data.frame(airports), "tables/airports_yr.dbf")






