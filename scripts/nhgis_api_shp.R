

## prepare workspace
rm(list = ls())
options(scipen = 999)
# setwd("C:/Users/scott/Dropbox/MappingData/urbanization2")
# setwd("D:/MappingData/urbanization2")
# setwd("C:/Users/scott/Dropbox/urb_proj/shp_work")

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


## Shapefile
packages(tidyverse)
packages(httr)  # needed for NHGIS API
packages(jsonlite)  # needed for NHGIS API
packages(ipumsr)  # needed for API unzip

setwd("C:/Users/scott/Dropbox/urb_proj")

## API KEY
my_key <- "59cba10d8a5da536fc06b59d105a093e6b154940b7a0263ee0891027"




## SHAPEFILES
url <- "https://api.ipums.org/metadata/nhgis/shapefiles?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
View(res_df)

##################################################################################
##  GET 1940-1960 tracts (broken up to allow download.file to work)           ##
##################################################################################

## Set URL and JSON call
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_1940_tl2008",
   "us_tract_1950_tl2008",
   "us_tract_1960_tl2008"
    ]
}
'

## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## DOWNLOAD SHAPEFILES
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## PUT in USABLE FORMAT
zip_file <- "C:/Users/scott/Dropbox/urb_proj/temp/NHGIS_shape.zip"

# Download extract to destination file
download.file(
  url = des_df$download_links$gis_data,
  destfile = zip_file,
  headers = c(Authorization = my_key)
)

# look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "shp_work/test", overwrite = TRUE)

# Unzip shapefiles in zipped file and put in correct folder
for(i in seq(1940, 1960, 10)){
  
  zip_file2 <- paste0("shp_work/test/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shape/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shapefile_tl2008_us_tract_",
                      i,
                      ".zip")
  
  unzip(zip_file2, exdir = "shp_work/test")
  
}

##################################################################################
##  GET 1970 & 1980 tracts (broken up to allow download.file to work)           ##
##################################################################################

## Set URL and JSON call
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '

{
 "shapefiles": [
   "us_tract_1980_tl2008"
    ]
}
'

## Complete extract request
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())  # should say: "Status: 200 OK"
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number  # grab number of call (personalized by user)
my_number

## !! WAIT ~ 30-60 seconds for NHGIS EXTRACT to PROCESS !!  ##

## DOWNLOAD SHAPEFILES
shp_url <- paste0("https://api.ipums.org/extracts/",my_number,"?product=nhgis&version=v1")
data_extract_status_res <- GET(shp_url, add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$download_links  # take a look at the download links

## PUT in USABLE FORMAT
zip_file <- "C:/Users/scott/Desktop/NHGIS_gis.zip"

# Download extract to destination file (limited in size it can download)
download.file(
  url = des_df$download_links$gis_data,
  destfile = zip_file,
  mode = "wb",
  headers = c(Authorization = my_key)
)

# Look at list of files in zipped filed
unzip(zip_file, list = TRUE)

# Initial unzipping of original file
unzip(zip_file, exdir = "shp_work/test", overwrite = TRUE)

# Unzip shapefiles in zipped file and put in correct folder
for(i in seq(1940, 1960, 10)){
  
  zip_file2 <- paste0("shp_work/test/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shape/nhgis",
                      str_pad(as.character(my_number), 4, pad = "0"),
                      "_shapefile_tl2008_us_tract_",
                      i,
                      ".zip")
  
  unzip(zip_file2, exdir = "shp_work/test")
  
}
