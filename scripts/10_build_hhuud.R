
#########################################################################
#########################################################################
###                                                                   ###
###           BUILD the HIST. HU. & URB. DATABASE (1st CUT)           ###
###                                                                   ###
#########################################################################
#########################################################################

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

## load needed packages
packages(tidyverse)
packages(tidycensus)  # for getting state/county names/abbrevs.
packages(foreign)  # for dbf reading/writing
packages(sf) # for reading gdb
packages(data.table) # for setnames function in for loop
packages(styler)  # for styling code