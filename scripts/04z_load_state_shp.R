
#########################################################################
#########################################################################
###                                                                   ###
###      LOAD in STATE SHAPEFILES in CORRECT COORDINATE SYSTEM        ###
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

# Load packages
packages(tidyverse)
packages(tigris)
packages(sf)


##############################################################
###  LOAD in STATE SHAPES from TIGRIS PACKAGE              ###
##############################################################

## BRING in LOWER 48
lower48 <- tigris::states(cb = TRUE, resolution = "500k", class = "sf") %>% 
  # keep only contiguous 48 states
  filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS")) %>% 
  # clean up columns
  select(STUSPS, GEOID, geometry) %>% 
  rename(
    ST = STUSPS, 
    GEOMETRY = geometry
    ) %>%
  print()

## SET COORDINATE SYSTEM (USA Contiguous Albers Equal Area Conic)
lower48 <- st_transform(lower48, crs = st_crs(5070))

## WRITE OUT as SHAPEFILE
st_write(lower48, "gis_files/lower48.shp")

