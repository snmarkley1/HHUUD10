#########################################################################
#########################################################################
###                                                                   ###
###     PULL in CROSSWALK FILES for 1990 BG HU Data in 2010 BGs       ###
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

## Create temp folder
dir.create("temp")

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

# Look at single year (1980) example
url <- "https://api.ipums.org/metadata/nhgis/datasets/1990_STF3?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
#View(res_df$geog_levels)  # need Block Group parts (blck_grp_598_101)

# Pull in Year Structure Built (YSB) data by decade at tract level, 1940-80 plus 1990-2019 HU counts in 2010 boundaries (time series)
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '
  {
    "datasets": {
      "1990_STF3": {
        "data_tables": ["NH25"],
        "geog_levels": ["blck_grp_598_101"]
      }
    },
    "data_format": "csv_no_header",
    "geographic_extents": ["*"]
    
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

# Read CSV file into a data frame
ysb <- read_nhgis(zip_file, data_layer = contains("_598.csv")) %>%
  filter(!STATE %in% c("Alaska", "Hawaii"))

# Organize ysb
temp90 <- ysb %>%
  select(GISJOIN, EX7001:EX7008) %>%
  mutate(
    hu40 = EX7008,
    hu50 = hu40 + EX7007,
    hu60 = hu50 + EX7006,
    hu70 = hu60 + EX7005,
    hu80 = hu70 + EX7004,
    hu90 = hu80 + EX7003 + EX7002 + EX7001
  ) %>%
  select(-c(EX7001:EX7008)) %>%
  print()


###################################################################
## READ in xwalk Files for BGs and Join w/ ysb90                 ##
###################################################################

## Read in XWALK file
zip_file <- "tables/nhgis_bgp1990_bg2010.zip"  # establish zip_file path
unzip(zip_file, list = TRUE)  # check contents
xwalk_bg <- read_nhgis(zip_file, data_layer = contains("bg2010.csv"))  # read in xwalk file

## BGP 1990 to BG 2010
ysb90 <- xwalk_bg %>%
  select(bgp1990gj, bg2010gj, wt_hu) %>%
  left_join(temp90, by = c("bgp1990gj" = "GISJOIN")) %>%
  mutate(
    hu90 = hu90 * wt_hu,
    hu80 = hu80 * wt_hu,
    hu70 = hu70 * wt_hu,
    hu60 = hu60 * wt_hu,
    hu50 = hu50 * wt_hu,
    hu40 = hu40 * wt_hu
  ) %>%
  group_by(bg2010gj) %>%
  summarize(
    hu40 = sum(hu40),
    hu50 = sum(hu50),
    hu60 = sum(hu60),
    hu70 = sum(hu70),
    hu80 = sum(hu80),
    hu90 = sum(hu90),
  ) %>%
  rename(GISJOIN = "bg2010gj") %>%
  # remove Alaska & Hawaii
  filter(!str_detect(GISJOIN, "G02|G15")) %>%
  print()

## Save as dbf
write.dbf(as.data.frame(ysb90), "tables/ysb90.dbf")

##-------------------------------------------
## Make ysb90 for Tract Level
##-------------------------------------------
ysb90_t <- ysb90 %>%
  mutate(
    GISJOIN = as.character(GISJOIN),
    T_JOIN = str_sub(GISJOIN, 1, 14)
    ) %>%
  group_by(T_JOIN) %>%
  summarize_at(vars(hu40:hu90), sum) %>%
  rename(GISJOIN = T_JOIN) %>%
  print()

# Save as dbf
write.dbf(as.data.frame(ysb90_t), "tables/ysb90_t.dbf")

##------------------------------------------
## Delete temp folder
##------------------------------------------
unlink("temp", recursive = TRUE)


