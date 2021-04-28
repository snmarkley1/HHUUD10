
#########################################################################
#########################################################################
###                                                                   ###
###           PULL in TRACT/BG SHAPEFILES, 1940-1980, 2010            ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
rm(list = ls())  # clear environment
options(scipen = 999) 
options(digits = 6)
getwd()  # should be ~/HIST_HU_URB

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

##################################################
## SET API KEY and EXPLORE NHGIS SHAPEFILES     ##
##################################################

### NHGIS: https://www.nhgis.org/

## API KEY
# Set personalized API key:  https://account.ipums.org/api_keys
my_key <- "<YOUR KEY HERE>"

## EXPLORE SHAPEFILES AVAILABLE from NHGIS
url <- "https://api.ipums.org/metadata/nhgis/shapefiles?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

###############################################################
##  GET 1940-1980 TRACT SHAPEFILES FROM NHGIS                ##
###############################################################

## Set URL and JSON call to get 1960-80 tract shapefiles
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_1940_tl2008",
   "us_tract_1950_tl2008",
   "us_tract_1960_tl2008",
   "us_tract_1970_tl2008",
   "us_tract_1980_tl2008"
    ]
}
'
## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())  # "Status" should be "200 OK"
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~30-60 SECONDS for EXTRACT to be PREPARED (check email) !! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Download shapefiles
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## Download extract to zip file
## !! MAKE SURE R is up-to-date !! ##
## !! DOES NOT WORK on EVERY MACHINE: BEST PERFORMANCE on DESKTOPS w/ LOTS of RAM !! ##
zip_file <- "temp/NHGIS_shape.zip"
download.file(
  url = des_df$download_links$gis_data,
  destfile = zip_file,
  headers = c(Authorization = my_key)
)

# look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "temp", overwrite = TRUE)

# Unzip shapefiles and put in correct folder (gis_files/tracts)
for(i in seq(1940, 1980, 10)){
  
  zip_file2 <- paste0("temp/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shape/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shapefile_tl2008_us_tract_",
                      i,
                      ".zip")
  
  unzip(zip_file2, exdir = "gis_files/tracts")
  
}

##################################################################################
##  GET 2010 BG SHAPEFILE                                                       ##
##################################################################################

## Set URL and JSON call to get 2010 BG shapefile
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_blck_grp_2010_tl2010"
    ]
}
'
## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())  # "Status" should be "200 OK"
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~30-60 SECONDS for EXTRACT to be PREPARED (check email) !! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Download shapefiles
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## Download extract to zip file
## !! MAKE SURE R is up-to-date !! ##
## !! DOES NOT WORK on EVERY MACHINE: BEST PERFORMANCE on DESKTOPS w/ LOTS of RAM !! ##
zip_file <- "temp/NHGIS_shape.zip"
download.file(
  url = des_df$download_links$gis_data,
  destfile = zip_file,
  headers = c(Authorization = my_key)
)

# look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "temp", overwrite = TRUE)

# Unzip shapefiles and put in correct folder (gis_files/tracts)
zip_file2 <- paste0("temp/nhgis",
                    str_pad(as.character(my_number), 4, pad = "0"),
                    "_shape/nhgis",
                    str_pad(as.character(my_number), 4, pad = "0"),
                    "_shapefile_tl2010_us_blck_grp_2010.zip")
  
unzip(zip_file2, exdir = "gis_files/tracts")


##################################################################################
##  GET 2010 TRACT SHAPEFILE                                                    ##
##################################################################################

## Set URL and JSON call to get 2010 tract shapefile
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_2010_tl2010"
    ]
}
'
## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())  # "Status" should be "200 OK"
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##
## !! NEED to WAIT ~30-60 SECONDS for EXTRACT to be PREPARED (check email) !! ##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ##

## Download shapefiles
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## Download extract to zip file
## !! MAKE SURE R is up-to-date !! ##
## !! DOES NOT WORK on EVERY MACHINE: BEST PERFORMANCE on DESKTOPS w/ LOTS of RAM !! ##
zip_file <- "temp/NHGIS_shape.zip"
download.file(
  url = des_df$download_links$gis_data,
  destfile = zip_file,
  headers = c(Authorization = my_key)
)

# look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "temp", overwrite = TRUE)

# Unzip shapefiles and put in correct folder (gis_files/tracts)
zip_file2 <- paste0("temp/nhgis",
                    str_pad(as.character(my_number), 4, pad = "0"),
                    "_shape/nhgis",
                    str_pad(as.character(my_number), 4, pad = "0"),
                    "_shapefile_tl2010_us_tract_2010.zip")

unzip(zip_file2, exdir = "gis_files/tracts")


