
#########################################################################
#########################################################################
###                                                                   ###
###                     ORGANIZE NEIGHBORS FILE                       ###
###                                                                   ###
#########################################################################
#########################################################################

##-----------------------------------------------------------------------
## PREPARE WORKSPACE
##-----------------------------------------------------------------------

rm(list = ls())
options(scipen = 999)
options(digits = 6)
getwd()  # should be ~/HIST_HU_URB

## LOAD or INSTALL PACKAGES
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])
  
  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

## LOAD NEEDED PACKAGES
packages(tidyverse)
packages(foreign)  # for dbf reading/writing
packages(sf) # for reading gdb


###############################################################################
##  IMPORT & CLEAN t10_neighbors TABLE                                       ##
###############################################################################

## IMPORT
df <- sf::st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10_neighbors"
) %>%
  as_tibble() %>%
  print()

## CALCULATE % BORDER
neighbors <- df %>%
  group_by(src_GISJOIN) %>%
  mutate(
    src_LENGTH = sum(LENGTH),  # calculate non-coastal perimeter of source tracts
    WEIGHT = LENGTH / src_LENGTH  # calculate percent of non-coastal perimeter shared by neighbor tracts --> weight to be used in future calculations
    ) %>%
  select(src_GISJOIN, nbr_GISJOIN, WEIGHT) %>%
  print()

## SAVE OUT
write_csv(neighbors, "tables/neighbors.csv")








