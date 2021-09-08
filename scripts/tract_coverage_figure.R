

## Load and organize tract pop

## Prepare workspace
source("scripts/00_preamble.R")


## Get data from NHGIS API

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

# Check out available datasets
url <- "https://api.ipums.org/metadata/nhgis/datasets?version=v1"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
#View(res_df)

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
## !! NEED to WAIT ~3+ MINUTES for EXTRACT to be PREPARED (check email) !!!!!! ##
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
    filter(!STATE %in% c("Alaska", "Hawaii", "Puerto Rico"))
  
  k <- str_sub(i, 3, 4) # give 2-digit year for data frame name
  assign(paste0("temp", k), temp) # write out new data frames
}
