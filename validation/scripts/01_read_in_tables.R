#########################################################################
#########################################################################
###                                                                   ###
###               PULL in Validation Data (1990-2019)                 ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/HHUUD10/validation

## Create temp folder
dir.create("temp")

## Set up Census key: https://api.census.gov/data/key_signup.html
census_api_key("<YOUR KEY HERE>", overwrite = TRUE, install = TRUE)


#########################################################
## SET API KEY and EXPLORE NHGIS SHAPEFILES            ##
#########################################################

## # Set personalized NHGIS API key:  https://account.ipums.org/api_keys
my_ipums_key <- "<YOUR KEY HERE>"


#########################################################
## LOAD HU & YSB TABLES                                ##
#########################################################

# Check out available datasets
url <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1"
result <- GET(url, add_headers(Authorization = my_ipums_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
#View(res_df)

# Look at single year (1980) example
url <- "https://api.ipums.org/metadata/nhgis/datasets/1980_STF3/data_tables/NT109A?version=v1"
result <- GET(url, add_headers(Authorization = my_ipums_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, 20L)


# Pull in Year Structure Built (YSB) data by decade at tract level, 2000, 2006-10, 2015-19
# Pull in Housing Unit Data at tract level 1990, 2000, 2006-10, 2015-19
# pull in 1990 and 2000 housing units at county level
# pull in 1990 and 2000 housing units in 2010 boundaries (time series data)
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '
  {
    "datasets": {
      "2015_2019_ACS5a": {
        "data_tables": ["B25034","B25001"],
        "geog_levels": ["tract"]
      },
      "2006_2010_ACS5a": {
        "data_tables": ["B25034", "B25001"],
        "geog_levels": ["tract"]
      },
      "2000_SF3a": {
        "data_tables": ["NH034A"],
        "geog_levels": ["tract"]
      },
      "2000_SF1a": {
        "data_tables": ["NH001A"],
        "geog_levels": ["tract", "county"]
      },
      "1990_STF1": {
        "data_tables": ["NH1"],
        "geog_levels": ["tract", "county"]
        }
      },
      "time_series_tables": {
      "CM7": {
        "years": ["1990","2000"],
        "geog_levels": ["tract"]
        }
      },
    "time_series_table_layout": "time_by_column_layout",
    "data_format": "csv_no_header",
    "breakdown_and_data_type_layout": "single_file"
  }
'
  

## All Block Group Data in 'create_shapes.R' file

## Request data extract from NHGIS
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_ipums_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number
my_number


## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~5+ MINUTES for EXTRACT to be PREPARED (check email) !!!!!! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Import Extract when it's ready
csv_url <- paste0("https://api.ipums.org/extracts/", my_number, "?product=nhgis&version=v1")
data_extract_status_res <- GET(csv_url, add_headers(Authorization = my_ipums_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links

# Destination file
zip_file <- "temp/NHGIS_tables.zip"

# Download extract to destination file
download.file(
  url = des_df$download_links$table_data, 
  destfile = zip_file, 
  headers = c(Authorization = my_ipums_key)
)

# List extract files in ZIP archive
unzip(zip_file, list=TRUE)


# Read CSV files into a data frame

ysb1519_table <- read_nhgis(zip_file, data_layer = contains("ds244_20195_2019_tract.csv"))
ysb0610_table <- read_nhgis(zip_file, data_layer = contains("ds176_20105_2010_tract.csv"))

hu00_table <- read_nhgis(zip_file, data_layer = contains("ds146_2000_tract.csv"))
ysb00_table <- read_nhgis(zip_file, data_layer = contains("ds151_2000_tract.csv"))

hu90_table <- read_nhgis(zip_file, data_layer = contains("ds120_1990_tract.csv"))

cntyhu90_table <- read_nhgis(zip_file, data_layer = contains("ds120_1990_county.csv"))
cntyhu00_table <- read_nhgis(zip_file, data_layer = contains("ds146_2000_county.csv"))

ct10ts_table <- read_nhgis(zip_file, data_layer = contains("ts_geog2010_tract.csv"))


###########################################################
##  CLEAN UP data frames
###########################################################

## Get county fips codes for nine sample counties
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390), 
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)


##--------------------------------------------------------------------------------------
# 2015-19

# 2015-19 ACS Housing Units and Year Structure Built, tract
ysb19 <-  ysb1519_table %>%
  select(-c(2:43)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2015_19 = ALZJE001)

ysb19 <- ysb19 %>%
  mutate(
    hu_90 = (AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011), # housing units built before 1989 in tract i in county j
    hu_00 = (AL0DE005 + AL0DE006 + AL0DE007 + AL0DE008 + AL0DE009 + AL0DE010 + AL0DE011)# housing units built before 1999 in tract i in county j
  )

# count hu 90 and hu00 for Hammer estimate

## write
write_csv(ysb19, "tables/tracts1519.csv")

##--------------------------------------------------------------------------------------
# 2006-10

# 2006-2010 ACS Year Structure Built, tract
ysb2010 <- ysb0610_table %>%
  # select(-c(2:36,38:57)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU2006_10 = JRIE001) %>%
  print()

##--------------------------------------------------------------------------------------
# Sum 2000 and 1990 housing units from YSB columns in 2006-10 ACS
ysb10 <- ysb2010 %>%
  mutate(
    HU_1990 = (JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010),      # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
    HU_2000 = (JSDE004+JSDE005+JSDE006+JSDE007+JSDE008+JSDE009+JSDE010), # YSB Built 1939 or earlier + ... + YSB Built 1990-1999
    HU_2006_10 = JSDE001
  ) %>%
  select(GISJOIN, HU2006_10, HU_2000, HU_1990) %>%
  print()

## write
write_csv(ysb10, "tables/tracts0610.csv")


##--------------------------------------------------------------------------------------
## 2000 housing units, tract
tract_hu2000 <- hu00_table %>%
  select(-c(2:7,9:30)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>% 
  rename(HU2000_00 = FKI001)

## 2000 Year Structure Built, tract
tract_ysb2000 <- ysb00_table %>%
  select(-c(2:7,9:31)) %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  mutate(
    HU1990_00 = (GAJ004+GAJ005+GAJ006+GAJ007+GAJ008+GAJ009), # YSB Built 1939 or earlier + ... + YSB Built 1980-1989
  )

# ## join
tracts_2000 <- left_join(tract_hu2000, tract_ysb2000)

## write
write_csv(tracts_2000, "tables/tracts00.csv")

##--------------------------------------------------------------------------------------
## Read in NHGIS Housing Unit and YSB Data, 1990

# 1990 housing units, tract
tracts_hu1990 <- hu90_table %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(HU1990_90 = ESA001) %>%
  select(GISJOIN,HU1990_90)

## write
write_csv(tracts_hu1990, "tables/tracts90.csv")


##--------------------------------------------------------------------------------------
## 1990 and 2000 county
# Read in 1990 county level housing units
county_hu1990 <- cntyhu90_table %>%
  select(-c(2:26)) %>%
  filter(GISJOIN %in% county_fips)

# Read in 2000 county units, read in 
county_hu2000 <- cntyhu00_table %>%
  select(-c(2:29)) %>%
  filter(GISJOIN %in% county_fips)

# join county tbls
county_hu90_00 <- left_join(county_hu1990,county_hu2000, by = "GISJOIN") %>%
  rename(hu1990_cnty = ESA001,
         hu2000_cnty = FKI001)

## write
write_csv(county_hu90_00, "tables/countyhu90_00.csv")

##--------------------------------------------------------------------------------------
## Time Series NHGIS data; 1990 and 2000 in 2010 tracts
ts_2010  <- ct10ts_table %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8)) %>%
  filter(COUNTYA %in% county_fips) %>%
  rename(hu1990_10ts = CM7AA1990,
         hu2000_10ts = CM7AA2000) %>%
  select(GISJOIN, hu1990_10ts, hu2000_10ts)

## write
write_csv(ts_2010, "tables/tracts_ts2010.csv")



#######################################################################################
## Block Group Data, 2015-19 ACS pulled from tidycensus
#######################################################################################

# county & state fips
county_fips<- c("071", "163", "510", "065", "095","439","013", "061", "003")
state_codes <- c("LA", "MI", "MO", "CA", "FL", "TX", "NJ", "OH", "PA")

# tidycensus variables for Year Structure Built, 2015-19 ACS
my_vars <- c("B25034_001","B25034_002","B25034_003","B25034_004","B25034_005",":B25034_006","B25034_007","B25034_008","B25034_009","B25034_010","B25034_011")

# read in block group data
ysb19bg <- map2_dfr(
  state_codes, county_fips,
  ~ get_acs(
    geography = "block group",
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2019,
    survey = "acs5",
    geometry = FALSE,
    output = 'wide'
  )
) %>%
  print()

## clean data table
block_groups_1519 <- ysb19bg %>%
  select(-(ends_with("M"))) %>% # remove MOE columns 
  mutate(county_fips = str_sub(GEOID,1,5), # create a county fips column
         GISJOIN = paste0("G",str_sub(GEOID,1,2),0,str_sub(GEOID,3,5),0,str_sub(GEOID,6,12)), # create GISJOIN to match NHGIS codes
         HU2010 = rowSums(.[,6:12]), # sum of YSB 1939...YSB 2009
         HU2000 = rowSums(.[,7:12]), # sum of YSB 1939...YSB 1999
         HU1990 = rowSums(.[,8:12])) %>% # sum of YSB 1939...YSB 1989
  #filter(county_fips %in% acs_county_fips) %>% # filter dataset to counties used in validation
  rename(HU2015_19 = B25034_001E) %>% # rename 2015-19 ACS Housing Units variable
  select(GISJOIN, HU2015_19, HU2010, HU2000, HU1990) %>% # select final columns for dataset
  print()

## write
write_csv(block_groups_1519, "tables/block_groups_1519.csv")

