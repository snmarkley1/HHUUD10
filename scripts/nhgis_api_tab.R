

## NHGIS API TEST

## prepare workspace
rm(list = ls())
options(scipen = 999)
setwd("C:/Users/scott/Dropbox/urb_proj/tables")

## Load packages
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
packages(httr)  # needed for NHGIS API
packages(jsonlite)  # needed for NHGIS API
packages(ipumsr)  # needed for API unzip

###########################
## GETTING STARTED
###########################

## API KEY
my_key <- "59cba10d8a5da536fc06b59d105a093e6b154940b7a0263ee0891027"


## SHAPEFILES
url <- "https://api.ipums.org/metadata/nhgis/shapefiles?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_1940_tl2008",
   "us_tract_1950_tl2008",
   "us_tract_1960_tl2008",
   "us_tract_1970_tl2008",
   "us_tract_1980_tl2008",
   "us_tract_1990_tl2008",
   "us_blck_grp_2010_tl2010",
   "us_tract_2010_tl2010"
   ],
 "geographic_extents": ["010"]
}
'

mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json")
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number
my_number

## DOWNLOAD SHAPEFILES
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links

## UNZIP
zip_file <- "NHGIS_shp.zip"

# Download extract to destination file
download.file(
  url = des_df$download_links$gis_data[1],
  destfile = zip_file,
  headers = c(Authorization = my_key)
  )
unzip("C:/temp/NHGIS_shp.7z", list = TRUE)


####################################
## LOAD TABLES
####################################

# test case
url <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

# 1980 Example
url <- "https://api.ipums.org/metadata/nhgis/datasets/1980_STF3/data_tables/NT109A?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, 20L)

# Pull in YSB data by decade plus 1990-2019 HU counts in 2010 boundaries
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
      "1990_STF3": {
        "data_tables": ["NH25"],
        "geog_levels": ["blck
        _grp_598"]
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
    "geographic_extents": ["*"],
    "data_format": "csv_no_header",
    "breakdown_and_data_type_layout": "single_file"
  }
  '

# POST DATA EXTRACT on NHGIS website
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number
my_number


## IMPORT EXTRACT when IT'S READY
csv_url <- paste0("https://api.ipums.org/extracts/", my_number, "?product=nhgis&version=v1")
data_extract_status_res <- GET(csv_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links

# Destination file
zip_file <- "NHGIS_tables.zip"

# Download extract to destination file
download.file(des_df$download_links$table_data, zip_file, headers=c(Authorization=my_key))

# List extract files in ZIP archive
unzip(zip_file, list=TRUE)

# Read CSV files into a data frame
for (i in unique(c(seq(1940, 1980, 10), 2010, 2019))) {
  
  j <- paste0(i, "_tract.csv")

  # read in NHGIS files and remove AK, HI, etc.
  temp <- read_nhgis(zip_file, data_layer = contains(j)) %>% 
    filter(!STATE %in% c("Alaska", "Hawaii"))
  
  k <- str_sub(i, 3, 4) # give 2-digit year for data frame name
  assign(paste0("temp", k), temp) # write out new data frames
}

## clean up each decade
ysb40 <- temp40 %>%
  rename(hu40 = "BU1001") %>%
  select(GISJOIN,hu40)

ysb50 <- temp50 %>%
  mutate(
    hu40 = B0Y002 + B0Y003 + B0Y004,
    hu50 = hu40 + B0Y001
    ) %>%
  select(GISJOIN, hu40, hu50)

ysb60 <- temp60 %>%
  mutate(
    hu40 = B7E003,
    hu50 = hu40 + B7E002,
    hu60 = hu50 + B7E001
  ) %>%
  select(GISJOIN, hu40:hu60)

ysb70 <- temp70 %>%
  mutate(
    hu40 = CZ2006,
    hu50 = hu40 + CZ2005,
    hu60 = hu50 + CZ2004,
    hu70 = hu60 + CZ2003 + CZ2002 + CZ2001
  ) %>%
  select(GISJOIN, hu40:hu70)

ysb80 <- temp80 %>%
  mutate(
    hu40 = DEQ007,
    hu50 = hu40 + DEQ006,
    hu60 = hu50 + DEQ005,
    hu70 = hu60 + DEQ004,
    hu80 = hu70 + DEQ003 + DEQ002 + DEQ001
  ) %>%
  select(GISJOIN, hu40:hu80)

hu9010 <- temp10 %>%
  mutate(
    hu90 = CM7AA1990,
    hu00 = CM7AA2000,
    hu10 = CM7AA2010
  ) %>%
  select(GISJOIN, hu90:hu10)

hu19 <- temp19 %>%
  mutate(hu19 = ALZJE001) %>%
  select(GISJOIN, hu19)

#ysb90 <- temp90 %>%
#  mutate(
#    hu40 = EX7008,
#    hu50 = hu40 + EX7007,
#    hu60 = hu50 + EX7006,
#    hu70 = hu60 + EX7005,
#    hu80 = hu70 + EX7004,
#    hu90 = hu80 + EX7003 + EX7002 + EX7001
#  ) %>%
#  select(GISJOIN, hu40:hu90)

## clean up
rm(list = setdiff(ls(), c("ysb40", "ysb50", "ysb60", "ysb70", "ysb80", "hu9010", "hu19","my_key")))

hu9019 <- left_join(hu9010, hu19, by = "GISJOIN")


"time_series_tables": {
  "CM7": {
    "years": ["1990", "2000"],
    "geog_levels": ["tract"]
  }
},
,
"time_series_table_layout": "time_by_column_layout"