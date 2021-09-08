

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
## LOAD POP TABLES                                     ##
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
        "data_tables": ["NT1"],
        "geog_levels": ["tract"]
      },
      "1950_tPH_Major": {
        "data_tables": ["NT1"],
        "geog_levels": ["tract"]
      },
      "1960_tPH": {
        "data_tables": ["NTSUP2"],
        "geog_levels": ["tract"]
      },
      "1970_Cnt4H": {
        "data_tables": ["NT61"],
        "geog_levels": ["tract"]
      },
      "1980_STF1": {
        "data_tables": ["NT1A"],
        "geog_levels": ["tract"]
      },
      "1990_STF1": {
        "data_tables": ["NP1"],
        "geog_levels": ["tract"]
      }
    },
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
## !! NEED to WAIT ~2 MINUTES for EXTRACT to be PREPARED (check email) !!!!!! ##
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
tract_pop <- NULL
for (i in unique(seq(1940, 1990, 10))) {
  
  j <- paste0(i, "_tract.csv")
  
  # read in NHGIS files
  temp <- read_nhgis(zip_file, data_layer = contains(j)) %>% 
    # remove AK, HI, etc.
    filter(!STATE %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
    select(1, 2, ncol(.)) %>%
    rename(pop = 3) %>%
    group_by(YEAR) %>%
    summarize(pop = sum(pop, na.rm = TRUE))
  
  tract_pop <- bind_rows(tract_pop, temp)
  
  #k <- str_sub(i, 3, 4) # give 2-digit year for data frame name
  #assign(paste0("temp", k), temp) # write out new data frames
}

tract_pop


###########################################################
##  Read in Contiental US pop                            ##
###########################################################

# Pull in Year Structure Built (YSB) data by decade at tract level, 1940-80 plus 1990-2019 HU counts in 2010 boundaries (time series)
url <- "https://api.ipums.org/extracts/?product=nhgis&version=v1"
mybody <- '
  {
    "datasets": {
      "1940_cAge": {
        "data_tables": ["NT1"],
        "geog_levels": ["state"]
      },
      "1950_cAge": {
        "data_tables": ["NT1"],
        "geog_levels": ["state"]
      },
      "1960_cAge1": {
        "data_tables": ["NT1"],
        "geog_levels": ["state"]
      },
      "1970_Cnt1": {
        "data_tables": ["NT1"],
        "geog_levels": ["state"]
      },
      "1980_STF1": {
        "data_tables": ["NT1A"],
        "geog_levels": ["state"]
      },
      "1990_STF1": {
        "data_tables": ["NP1"],
        "geog_levels": ["state"]
      }
    },
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
## !! NEED to WAIT ~2 MINUTES for EXTRACT to be PREPARED (check email) !!!!!! ##
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
us_pop <- NULL
for (i in unique(seq(1940, 1990, 10))) {
  
  j <- paste0(i, "_state.csv")
  
  # read in NHGIS files
  temp <- read_nhgis(zip_file, data_layer = contains(j)) %>% 
    # remove AK, HI, etc.
    filter(!str_detect(STATE, "Alaska|Hawaii|Rico")) %>%
    select(2, 3, ncol(.)) %>%
    rename(pop = 3) %>%
    group_by(YEAR) %>%
    summarize(us_pop = sum(pop, na.rm = TRUE))
  
  us_pop <- bind_rows(us_pop, temp)
  
  #k <- str_sub(i, 3, 4) # give 2-digit year for data frame name
  #assign(paste0("temp", k), temp) # write out new data frames
}

us_pop


##----------------------------------------------------------
##  Join together & make line graph                      
##----------------------------------------------------------

pop <- left_join(tract_pop, us_pop, by = "YEAR") %>%
  mutate(ptract = pop / us_pop * 100) %>%
  #bind_rows(data.frame(YEAR = 1990, pop = 1, us_pop = 1, ptract = 100)) %>%
  print()


###########################################################
##  Get areas                                            ##
###########################################################

gdb <- "D:/HIST_HU_URB/gis_files/database1.gdb"

areas <- NULL
for(i in unique(c(seq(40, 80, 10), 10))){
  
  shp = paste0("t", i)
  
  temp <- st_read(
    dsn = gdb,
    layer = shp
  ) %>%
    as_tibble() %>%
    mutate(
      YEAR = as.integer(paste0("19", i)),
      YEAR = ifelse(YEAR == 1910, 1990, YEAR)
      ) %>%
    select(GISJOIN, YEAR, Shape_Area) %>%
    group_by(YEAR) %>%
    summarize(AREA = sum(Shape_Area))
  
  areas <- bind_rows(areas, temp)
  
}

areas

## fix tract_areas
tract_areas <- areas %>%
  mutate(
    AREA_TOT = subset(AREA, YEAR == 1990),
    parea = AREA / AREA_TOT * 100
    ) %>%
  print()


###########################################################
##  Join all together & plot                             ##
###########################################################

pop_area <- pop[c(1,4)] %>%
  left_join(tract_areas[c(1,4)], by = "YEAR") %>%
  rename(pop = 2, area = 3) %>%
  pivot_longer(
    cols = pop:area,
    names_to = "variable",
    values_to = "percent"
  ) %>%
  print()

## GGPLOT
ggplot(pop_area) +
  geom_line(aes(x = YEAR, y = percent, group = variable, col = variable), lwd = 1.8) +
  geom_point(aes(x = YEAR, y = percent, group = variable, col = variable), size = 3) +
  scale_color_manual(values = c("darkgreen", "chocolate3")) +
  xlab("") +
  ylab("Continental US Covered by Tracts (%)") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
   ) +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
  scale_x_continuous(minor_breaks = NULL) +
  annotate("text", x = 1964, y = 67, angle = 34, label = "Population", size = 5, color = "grey30", fontface = "italic") +
  annotate("text", x = 1963, y = 13.5, angle = 26, label = "Area", size = 5, color = "grey30", fontface = "italic")


