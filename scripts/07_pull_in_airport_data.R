
#########################################################################
#########################################################################
###                                                                   ###
###                    LOAD & CLEAN AIRPORT DATA                      ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/HIST_HU_URB

##################################################################
##  STEP 1: LOAD AIRPORT DATA                                   ##
##################################################################

## !! FAA link from original has BROKEN !!
## To replicate results in HHUUD10, use "airports_yr.dbf" file in "tables" folder
## To produce new results with updated FAA data, skip to Line 66


# Original import (Date: June 28, 2021)
#airports_import <- read.delim(
#  "https://www.faa.gov/airports/airport_safety/airportdata_5010/menu/nfdcfacilitiesexport.cfm?Region=&District=&State=&County=&City=&Use=&Certification=",
#  header = TRUE,
#  sep = "\t") %>%
#  as_tibble(.) %>%
#  print()


## Organize for Export into ArcGIS Pro
#airports <- airports_import %>%
  # # include only airports in Lower 48 + DC
  # filter(!State %in% c("AK", "AS", "GU","HI", "MP", "PR", "QM", "QW", "VI", "XL")) %>%
  # # filter out heliports, gliderports, etc
  # filter(Type == "AIRPORT") %>%
  # # keep only relevant columns
  # select(LocationID, State, ActivationDate) %>%
  # rename(
  #   LOC_ID = 1,  # match Esri airport attribute name
  #   STATE = 2,  # keep the state
  #   ACT_DATE = 3  # date in which the airport was added to the NFDC airport database (https://www.faa.gov/airports/airport_safety/airportdata_5010/media/airport-data-dictionaries.xlsx)
  # ) %>%
  # # fix up columns
  # mutate(
  #   LOC_ID = str_extract(LOC_ID, "\\w+"),  # keep only alphabetic letters (removes apostrophe)
  #   ACT_DATE = as.integer(str_extract(ACT_DATE, "\\d+$"))  # keep only ending digits (keeps years, removes day & month)
  # ) %>%
  # print() # n = 12,539
  

## Save out to tables folder
#write.dbf(as.data.frame(airports), "tables/airports_yr.dbf")


## Inspect original file output
airports_yr <- read.dbf("tables/airports_yr.dbf") %>%
  as_tibble() %>%
  print()


##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
## Import newer data from FAA: pre-downloaded in "tables" folder (Date: Jan. 19, 2022)
##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------

## INSTRUCTIONS: If you want to use the most up-to-date FAA file available, you will need to 
## download it from the FAA site here: https://adip.faa.gov/agis/public/#/airportSearch/advanced.
## Next, you will need to save it locally as a csv file in your tables folder under the name
## "faa_airport_data_new.csv".


### Import table
# airports_import <- read_csv("tables/faa_airport_data_new.csv") %>%
#   print()
# 
# 
# ## Organize for Export into ArcGIS Pro
# airports <- airports_import %>%
#   # include only airports in Lower 48 + DC
#   filter(!`State Id` %in% c("AK", "AS", "GU","HI", "MP", "PR", "QM", "QW", "VI", "XL")) %>%
#   # filter out heliports, gliderports, etc
#   filter(`Facility Type` == "AIRPORT") %>%
#   # keep only relevant columns
#   select(`Loc Id`, `State Id`, `Activation Date`) %>%
#   rename(
#     LOC_ID = 1,  # match Esri airport attribute name
#     STATE = 2,  # keep the state
#     ACT_DATE = 3  # date in which the airport was added to the NFDC airport database (https://www.faa.gov/airports/airport_safety/airportdata_5010/media/airport-data-dictionaries.xlsx)
#   ) %>%
#   # fix up columns
#   mutate(
#     LOC_ID = str_extract(LOC_ID, "\\w+"),  # keep only alphabetic letters (removes apostrophe)
#     ACT_DATE = as.integer(str_extract(ACT_DATE, "\\d+"))  # keep only ending digits (keeps years, removes day & month)
#   ) %>%
#   print() # n = 12,539


## overwrite existing file
#write.dbf(as.data.frame(airports), "tables/airports_yr.dbf")


