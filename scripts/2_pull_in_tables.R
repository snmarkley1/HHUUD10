
#########################################################################
#########################################################################
###                                                                   ###
###        PULL in HISTORICAL HOUSING UNIT (1990-2019) AND            ###
###         YEAR STRUCTURE BUILT (YSB) TABLES (1940-1980)             ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
rm(list = ls())  # clear environment
options(scipen = 999)
options(digits = 6)
getwd()  # should be HIST_HU_URB

## Load or install packages
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])
  
  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

packages(tidyverse)
packages(httr)  # for NHGIS API
packages(jsonlite)  # for NHGIS API
packages(ipumsr)  # for NHGIS tables
packages(foreign)  # write out DBFs (for ArcGIS)

#########################################################
## SET API KEY and EXPLORE NHGIS SHAPEFILES            ##
#########################################################

### NHGIS: https://www.nhgis.org/

## API KEY
# Set personalized API key:  https://account.ipums.org/api_keys
my_key <- "<YOUR KEY HERE>"

#########################################################
## LOAD HU & YSB TABLES                                ##
#########################################################

# Check out available datasets
url <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

# Look at single year (1980) example
url <- "https://api.ipums.org/metadata/nhgis/datasets/1980_STF3/data_tables/NT109A?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, 20L)

# Pull in Year Structure Built (YSB) data by decade at tract level, 1940-80 plus 1990-2019 HU counts in 2010 boundaries (time series)
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '
  {
    "datasets": {
      "1940_tPH_Major": {
        "data_tables": ["NT26"],
        "geog_levels": ["tract"]
      },
      "1950_tPH_Major": {
        "data_tables": ["NT33"],
        "geog_levels": ["tract"]
      },
      "1960_tPH": {
        "data_tables": ["NBT52"],
        "geog_levels": ["tract"]
      },
      "1970_Cnt4H": {
        "data_tables": ["NT8A"],
        "geog_levels": ["tract"]
      },
      "1980_STF3": {
        "data_tables": ["NT109A"],
        "geog_levels": ["tract"]
      },
      "2015_2019_ACS5a": {
        "data_tables": ["B25001"],
        "geog_levels": ["tract"]
      }
    },
    "time_series_tables": {
      "CM7": {
        "years": ["1990","2000","2010"],
        "geog_levels": ["tract"]
        }
      },
    "time_series_table_layout": "time_by_column_layout",
    "data_format": "csv_no_header",
    "breakdown_and_data_type_layout": "single_file"
  }
  '

## Request data extract from NHGIS
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number
my_number

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~90-120 SECONDS for EXTRACT to be PREPARED (check email) !! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Import Extract when it's ready
csv_url <- paste0("https://api.ipums.org/extracts/", my_number, "?product=nhgis&version=v1")
data_extract_status_res <- GET(csv_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links

# Destination file
zip_file <- "temp/NHGIS_tables.zip"

# Download extract to destination file
download.file(
  url = des_df$download_links$table_data, 
  destfile = zip_file, 
  headers = c(Authorization = my_key)
  )

# List extract files in ZIP archive
unzip(zip_file, list = TRUE)

# Read CSV files into data frames
for (i in unique(c(seq(1940, 1980, 10), 2010, 2019))) {
  
  j <- paste0(i, "_tract.csv")

  # read in NHGIS files
  temp <- read_nhgis(zip_file, data_layer = contains(j)) %>% 
    # remove AK, HI, etc.
    filter(!STATE %in% c("Alaska", "Hawaii"))
  
  k <- str_sub(i, 3, 4) # give 2-digit year for data frame name
  assign(paste0("temp", k), temp) # write out new data frames
}


#####################################################
##  CLEAN UP EACH DECADE                           ##
#####################################################

### Getting YSB data from 1940-1980 in original tract geographies
### and HU data for 1990, 2000, 2010, & 2015-19 in 2010 tract geographies.
### NOTE: 1990 YSB data is obtained from the NHGIS crosswalk file in script: 3_xwalk_1990_2010.R)

## 1940
ysb40 <- temp40 %>%
  rename(hu40 = "BU1001") %>%
  select(GISJOIN,hu40)

# save as dbf
write.dbf(as.data.frame(ysb40), "tables/ysb40.dbf")

## 1950
ysb50 <- temp50 %>%
  mutate(
    hu40 = B0Y002 + B0Y003 + B0Y004,  # sum decades prior to 1940
    hu50 = hu40 + B0Y001  # sum pre-1940 count plus 1940s count to get 1950 HU
    ) %>%
  select(GISJOIN, hu40, hu50)

write.dbf(as.data.frame(ysb50), "tables/ysb50.dbf")

## 1960
ysb60 <- temp60 %>%
  mutate(
    hu40 = B7E003,
    hu50 = hu40 + B7E002,
    hu60 = hu50 + B7E001
  ) %>%
  select(GISJOIN, hu40:hu60)

write.dbf(as.data.frame(ysb60), "tables/ysb60.dbf")

## 1970
ysb70 <- temp70 %>%
  mutate(
    hu40 = CZ2006,
    hu50 = hu40 + CZ2005,
    hu60 = hu50 + CZ2004,
    hu70 = hu60 + CZ2003 + CZ2002 + CZ2001
  ) %>%
  select(GISJOIN, hu40:hu70)

write.dbf(as.data.frame(ysb70), "tables/ysb70.dbf")

## 1980
ysb80 <- temp80 %>%
  mutate(
    hu40 = DEQ007,
    hu50 = hu40 + DEQ006,
    hu60 = hu50 + DEQ005,
    hu70 = hu60 + DEQ004,
    hu80 = hu70 + DEQ003 + DEQ002 + DEQ001
  ) %>%
  select(GISJOIN, hu40:hu80)

write.dbf(as.data.frame(ysb80), "tables/ysb80.dbf")

## time series: 1990-2010 (YSB is created in script: 3_xwalk_1990_2010.R)
hu9010 <- temp10 %>%
  mutate(
    hu90 = CM7AA1990,
    hu00 = CM7AA2000,
    hu10 = CM7AA2010
  ) %>%
  select(GISJOIN, hu90:hu10)

write.dbf(as.data.frame(hu9010), "tables/hu9010.dbf")

## 2015-19 ACS
hu19 <- temp19 %>%
  mutate(hu19 = ALZJE001) %>%
  select(GISJOIN, hu19)

write.dbf(as.data.frame(hu19), "tables/hu19")


