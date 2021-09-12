#########################################################################
#########################################################################
###                                                                   ###
###          PULL in Shapes for Validation, 1990 - 2019               ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/HHUUD10/validation

##################################################
## SET API KEY and EXPLORE NHGIS SHAPEFILES     ##
##################################################

### NHGIS: https://www.nhgis.org/

## API KEY
# Set personalized API key:  https://account.ipums.org/api_keys
my_ipums_key <- "<YOUR KEY HERE>"

## EXPLORE SHAPEFILES AVAILABLE from NHGIS
url <- "https://api.ipums.org/metadata/nhgis/shapefiles?version=v1"
result <- GET(url, add_headers(Authorization = my_ipums_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
#View(res_df)

###############################################################
##  GET 1990-2000 TRACT SHAPEFILES FROM NHGIS                ##
###############################################################

## Set URL and JSON call to get 1960-80 tract shapefiles
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_1990_tl2008",
   "us_tract_2000_tl2008"
    ]
}
'

## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_ipums_key), body = mybody_json, encode = "json", verbose())  # "Status" should be "200 OK"
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~30-60 SECONDS for EXTRACT to be PREPARED (check email) !! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Download shapefiles
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_ipums_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## Download extract to zip file
## !! MAKE SURE R is up-to-date !! ##
## !! DOES NOT WORK on EVERY MACHINE: BEST PERFORMANCE on DESKTOPS w/ GOOD SPECS !! ##
zip_file <- "temp/NHGIS_shape.zip"
download.file(
   url = des_df$download_links$gis_data,
   destfile = zip_file,
   headers = c(Authorization = my_ipums_key),
   cacheOK = TRUE
 )

# look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "temp", overwrite = TRUE)


# Unzip shapefiles
for(i in seq(1990, 2000, 10)){
  
  zip_file2 <- paste0(
    "temp/nhgis",
    str_pad(as.character(my_number), 4, pad = "0"),
    "_shape/nhgis",
    str_pad(as.character(my_number), 4, pad = "0"),
    "_shapefile_tl2008_us_tract_",
    i,
    ".zip"
  )
  
  unzip(zip_file2, exdir = "temp")
  
}

##################################################################
##  GET 1990 & 2000 tracts into proper format                   ##
##################################################################

## County FIPS
county_fips <- c("G220071", "G260163", "G290510", "G060065", "G120095","G480439","G340013", "G390061", "G420003")

##--------------------------------------
## 1990 tracts
##--------------------------------------
tracts90_sf <- st_read(
  dsn = "temp",
  layer = "US_tract_1990_conflated"
) %>%
  mutate(CO_JOIN = str_sub(GISJOIN, 1, 7)) %>%
  filter(CO_JOIN %in% county_fips) %>%
  select(GISJOIN, geometry) %>%
  st_transform(crs = "ESRI:102003") %>%
  print()


##--------------------------------------
## 2000 tracts
##--------------------------------------
tracts00_sf <- st_read(
  dsn = "temp",
  layer = "US_tract_2000_conflated"
) %>%
  mutate(CO_JOIN = str_sub(GISJOIN, 1, 7)) %>%
  filter(CO_JOIN %in% county_fips) %>%
  select(GISJOIN, geometry) %>%
  st_transform(crs = "ESRI:102003") %>%
  print()


#########################################################
## Pull in 2010/2019 shapes via tigris                 ##
#########################################################

## Set up county and state codes for ACS data pull
county_fips <- c("071", "163", "510", "065", "095","439","013", "061", "003")
state_codes <- c("LA", "MI", "MO", "CA", "FL", "TX", "NJ", "OH", "PA")


##--------------------------------
##  2010 tracts
##--------------------------------
tracts10_sf <- map2_dfr(
  state_codes, county_fips,
  ~tracts(
    state = .x, 
    county = .y,
    cb = TRUE, 
    year = 2010
  )
) %>%
  mutate(GISJOIN = paste0("G", STATE, "0", COUNTY, "0", TRACT)) %>%
  select(GISJOIN) %>%
  st_transform(crs = "ESRI:102003") %>%
  print()


##--------------------------------
##  2019 block groups
##--------------------------------
bgs19_sf <- map2_dfr(
  state_codes, county_fips,
  ~block_groups(
    state = .x,
    county = .y,
    cb = TRUE,
    year = 2019
  )
) %>%
  mutate(GISJOIN = paste0("G", STATEFP, "0", COUNTYFP, "0", TRACTCE, BLKGRPCE)) %>%
  select(GISJOIN) %>%
  st_transform(crs = "ESRI:102003") %>%
  print()



##--------------------------------
##  Export All
##--------------------------------

# write shapefiles
dir.create("gis_files")

st_write(tracts10_sf, dsn = "gis_files/tracts_2010", layer = "tracts10.shp", driver = "ESRI Shapefile", append = TRUE)
st_write(tracts00_sf, dsn = "gis_files/tracts_2000", layer = "tracts00.shp", driver = "ESRI Shapefile", append = FALSE)
st_write(tracts90_sf, dsn = "gis_files/tracts_1990", layer = "tracts90.shp", driver = "ESRI Shapefile", append = FALSE) #,
st_write(bgs19_sf, dsn = "gis_files/bgroups_2019", layer = "bgs19.shp", driver = "ESRI Shapefile", append = FALSE) # , append = FALSE


## Delete temp folder
unlink("temp", recursive = TRUE)
