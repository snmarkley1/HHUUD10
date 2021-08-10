
#########################################################################
#########################################################################
###                                                                   ###
###                             PREAMBLE                              ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
rm(list = ls())  # clear environment
options(scipen = 999) 
options(digits = 6)
#setwd("D:/HIST_HU_URB")  # change if desired

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
packages(ipumsr)  # for NHGIS tables
packages(foreign)  # write out DBFs (for ArcGIS)
packages(sf)  # for spatial/ArcGIS Pro data
packages(tidycensus)  # for getting state/county names/abbrevs.
packages(data.table) # for setnames function
packages(imputeTS)  # for time series imputation




