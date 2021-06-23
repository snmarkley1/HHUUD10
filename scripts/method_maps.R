
#########################################################################
#########################################################################
###                                                                   ###
###                        MAKE METHOD MAPS                           ###
###                                                                   ###
#########################################################################
#########################################################################

## prepare workspace
rm(list = ls())
options(scipen = 999)
options(digits = 6)
getwd()  # should be ~/HIST_HU_URB

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


## LOAD PACKAGES
packages(tidyverse)
packages(tmap)
packages(tigris)
packages(mapview)
packages(magick)
packages(sf)

## CREATE FOLDER
dir.create("images")

## LOAD TABLE
hu40_80 <- read_csv("tables/hu40_80.csv")

############################################################
## ATLANTA AREA METHOD MAPS
############################################################

## ESTABLISH GA VARIABLES
st <- "GA"
city = "atl"
data <- hu40_80 %>%
  filter(str_detect(GISJOIN10, "G130063|G130067|G130089|G130097|G130113|G130121|G130135|G130151|G130247"))

## LOAD TRACTS
tracts_import <- tigris::tracts(state = st, cb = TRUE, year = 2010) %>%
  mutate(GISJOIN10 = paste0("G", STATE, "0", COUNTY, "0", TRACT))

## LOAD COUNTIES
counties <- tigris::counties(state = st, cb = TRUE, year = 2010)

## COMBINE TRACTS w/ DATA
tracts <- tracts_import %>%
  left_join(data, by ="GISJOIN10") %>%
  select(-c(GEO_ID:CENSUSAREA, COUNTYFP, STATEFP))

## Check out palette
#tmaptools::palette_explorer()

dir.create(paste0("images/", city))

## CREATE MAP IMAGES
for(i in seq(40, 80, 10)){

  year <- as.integer(paste0("19", i))
  
  if(year == "1940"){
    
    map <- tracts %>%
      filter(yr == year) %>%
      tm_shape() +
      tm_polygons(
        col = "method",
        palette = c("#fb8072", "#bebada", "#ffffb3"),  # only 3 categories
        title = "Interpolation Method",
        border.col = "black"
      ) +
      tm_layout(
        title = as.character(year),
        title.size = 7,
        legend.text.size = 2
      ) +
      tm_legend(
        title.size = 3
      ) +
      tm_shape(counties) +
      tm_borders(
        col = "gray20",
        lwd = 3
      )
    
  } else{
    
    map <- tracts %>%
      filter(yr == year) %>%
      tm_shape() +
      tm_polygons(
        col = "method",
        palette = c("#fb8072", "#bebada", "#b3de69", "#ffffb3"),  # 4 categories
        title = "Interpolation Method",
        border.col = "black"
      ) +
      tm_layout(
        title = as.character(year),
        title.size = 7,
        legend.text.size = 2
      ) +
      tm_legend(
        title.size = 3
      ) +
      tm_shape(counties) +
      tm_borders(
        col = "gray20",
        lwd = 3
      )
    
  }
  
  file_name <- paste0("images/", city, "/", city, i, ".png")
  png(file_name, width = 1200, height = 1100)
  print(map)
  dev.off()
  
}

imgs <- list.files(path = paste0("images/", city), pattern = ".png$")
img_list <- lapply(paste0("images/", city, "/", imgs), image_read)
img_joined <- image_join(img_list)
gif <- image_animate(img_joined, fps = 0.25)

gif
image_write(gif, paste0("images/", city, "/", city, "_method.gif"))




############################################################
## NYC METHOD MAPS
############################################################

## ESTABLISH NY INDICATORS
st = "NY"
city = "nyc"
st_code = "G36"
data <- hu40_80 %>%
  filter(str_detect(GISJOIN10, "G360005|G360047|G360061|G360081|G360085"))

## LOAD TRACTS (NY API BROKEN)
#tracts_import <- tigris::tracts(state = st, cb = TRUE, year = 2010) %>%
#  mutate(GISJOIN10 = paste0("G", STATE, "0", COUNTY, "0", TRACT))

## LOAD TRACTS from ARCGIS FEATURE CLASS INSTEAD
tracts_import <- sf::st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10"
) %>%
  filter(str_detect(GISJOIN10, st_code))

## LOAD COUNTIES
counties <- tigris::counties(state = st, cb = TRUE, year = 2010)

## COMBINE TRACTS w/ DATA
tracts <- tracts_import %>%
  left_join(data, by ="GISJOIN10")

## Check out palette
#tmaptools::palette_explorer()

dir.create(paste0("images/", city))

## CREATE MAP IMAGES
for(i in seq(40, 80, 10)){
  
  year <- as.integer(paste0("19", i))
  
    map <- tracts %>%
      filter(yr == year) %>%
      tm_shape() +
      tm_polygons(
        col = "method",
        palette = c("#fb8072", "#bebada", "#b3de69", "#ffffb3"),  # 4 categories
        title = "Interpolation Method",
        border.col = "black"
      ) +
      tm_layout(
        title = as.character(year),
        title.size = 7,
        legend.text.size = 2
      ) +
      tm_legend(
        title.size = 3
      ) +
      tm_shape(counties) +
      tm_borders(
        col = "gray20",
        lwd = 3
      )
  
  file_name <- paste0("images/", city, "/", city, i, ".png")
  png(file_name, width = 1200, height = 1100)
  print(map)
  dev.off()
  
}

imgs <- list.files(path = paste0("images/", city), pattern = ".png$")
img_list <- lapply(paste0("images/", city, "/", imgs), image_read)
img_joined <- image_join(img_list)
gif <- image_animate(img_joined, fps = 0.25)

gif
image_write(gif, paste0("images/", city, "/", city, "_method.gif"))


############################################################
## LA/OC METHOD MAPS
############################################################

## ESTABLISH LA INDICATORS
st = "CA"
city = "la"
st_code = "G06"
data <- hu40_80 %>%
  filter(str_detect(GISJOIN10, "G060037|G060059") & GISJOIN10 != "G0600370599100")  ## LA minus islands (for display purposes)

## LOAD TRACTS (NY BROKEN)
tracts_import <- tigris::tracts(state = st, cb = TRUE, year = 2010) %>%
  mutate(GISJOIN10 = paste0("G", STATE, "0", COUNTY, "0", TRACT))

## LOAD TRACTS from ARCGIS FEATURE CLASS INSTEAD
#tracts_import <- sf::st_read(
#  dsn = "gis_files/database1.gdb",
#  layer = "t10"
#) %>%
#  filter(str_detect(GISJOIN10, st_code))

## LOAD COUNTIES
counties <- tigris::counties(state = st, cb = TRUE, year = 2010)

## COMBINE TRACTS w/ DATA
tracts <- tracts_import %>%
  left_join(data, by ="GISJOIN10")

## Check out palette
#tmaptools::palette_explorer()

dir.create(paste0("images/", city))

## CREATE MAP IMAGES
for(i in seq(40, 80, 10)){
  
  year <- as.integer(paste0("19", i))
  
  map <- tracts %>%
    filter(yr == year) %>%
    tm_shape() +
    tm_polygons(
      col = "method",
      palette = c("#fb8072", "#bebada", "#b3de69", "#ffffb3"),  # 4 categories
      title = "Interpolation Method",
      border.col = "black"
    ) +
    tm_layout(
      title = as.character(year),
      title.size = 7,
      legend.text.size = 2
    ) +
    tm_legend(
      title.size = 3
    ) +
    tm_shape(counties) +
    tm_borders(
      col = "gray20",
      lwd = 3
    )
  
  file_name <- paste0("images/", city, "/", city, i, ".png")
  png(file_name, width = 1200, height = 1100)
  print(map)
  dev.off()
  
}

imgs <- list.files(path = paste0("images/", city), pattern = ".png$")
img_list <- lapply(paste0("images/", city, "/", imgs), image_read)
img_joined <- image_join(img_list)
gif <- image_animate(img_joined, fps = 0.25)

gif
image_write(gif, paste0("images/", city, "/", city, "_method.gif"))


############################################################
## DETROIT AREA METHOD MAPS
############################################################

## ESTABLISH DETROIT INDICATORS
st = "MI"
city = "det"
st_code = "G26"
data <- hu40_80 %>%
  filter(str_detect(GISJOIN10, "G260099|G260125|G260163"))

## LOAD TRACTS (NY BROKEN)
tracts_import <- tigris::tracts(state = st, cb = TRUE, year = 2010) %>%
  mutate(GISJOIN10 = paste0("G", STATE, "0", COUNTY, "0", TRACT))

## LOAD COUNTIES
counties <- tigris::counties(state = st, cb = TRUE, year = 2010)

## COMBINE TRACTS w/ DATA
tracts <- tracts_import %>%
  left_join(data, by ="GISJOIN10")

## Check out palette
#tmaptools::palette_explorer()

dir.create(paste0("images/", city))

## CREATE MAP IMAGES
for(i in seq(40, 80, 10)){
  
  year <- as.integer(paste0("19", i))
  
  map <- tracts %>%
    filter(yr == year) %>%
    tm_shape() +
    tm_polygons(
      col = "method",
      palette = c("#fb8072", "#bebada", "#b3de69", "#ffffb3"),  # 4 categories
      title = "Interpolation Method",
      border.col = "black"
    ) +
    tm_layout(
      title = as.character(year),
      title.size = 7,
      legend.text.size = 2
    ) +
    tm_legend(
      title.size = 3
    ) +
    tm_shape(counties) +
    tm_borders(
      col = "gray20",
      lwd = 3
    )
  
  file_name <- paste0("images/", city, "/", city, i, ".png")
  png(file_name, width = 1200, height = 1100)
  print(map)
  dev.off()
  
}

imgs <- list.files(path = paste0("images/", city), pattern = ".png$")
img_list <- lapply(paste0("images/", city, "/", imgs), image_read)
img_joined <- image_join(img_list)
gif <- image_animate(img_joined, fps = 0.25)

gif
image_write(gif, paste0("images/", city, "/", city, "_method.gif"))


############################################################
## NEW JERSEY METHOD MAPS
############################################################

## ESTABLISH DETROIT INDICATORS
st = "NJ"
city = "nj"
st_code = "G34"
data <- hu40_80 %>%
  filter(str_detect(GISJOIN10, st_code))

## LOAD TRACTS (NY BROKEN)
tracts_import <- tigris::tracts(state = st, cb = TRUE, year = 2010) %>%
  mutate(GISJOIN10 = paste0("G", STATE, "0", COUNTY, "0", TRACT))

## LOAD COUNTIES
counties <- tigris::counties(state = st, cb = TRUE, year = 2010)

## COMBINE TRACTS w/ DATA
tracts <- tracts_import %>%
  left_join(data, by ="GISJOIN10")

## Check out palette
#tmaptools::palette_explorer()

dir.create(paste0("images/", city))

## CREATE MAP IMAGES
for(i in seq(40, 80, 10)){
  
  year <- as.integer(paste0("19", i))
  
  map <- tracts %>%
    filter(yr == year) %>%
    tm_shape() +
    tm_polygons(
      col = "method",
      palette = c("#fb8072", "#bebada", "#b3de69", "#ffffb3"),  # 4 categories
      title = "Interpolation Method",
      border.col = "black"
    ) +
    tm_layout(
      title = as.character(year),
      title.size = 7,
      legend.text.size = 2
    ) +
    tm_legend(
      title.size = 3
    ) +
    tm_shape(counties) +
    tm_borders(
      col = "gray20",
      lwd = 3
    )
  
  file_name <- paste0("images/", city, "/", city, i, ".png")
  png(file_name, width = 1200, height = 1100)
  print(map)
  dev.off()
  
}

imgs <- list.files(path = paste0("images/", city), pattern = ".png$")
img_list <- lapply(paste0("images/", city, "/", imgs), image_read)
img_joined <- image_join(img_list)
gif <- image_animate(img_joined, fps = 0.25)

gif
image_write(gif, paste0("images/", city, "/", city, "_method.gif"))



