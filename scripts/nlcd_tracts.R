
#################################################
#################################################
###                                           ###
###             NLCD 2001 DATA                ###
###                01/30/2021                 ###
###                                           ###
#################################################
#################################################

## prep workspace
rm(list = ls())
options(scipen = 999)
#setwd("C:/Users/scott/Dropbox/urb_proj/tables")

## load libraries
library(tidyverse)
library(foreign)
library(sf)

################################################
###               LOAD DATA                  ###
################################################

setwd("C:/Users/scott/Dropbox/urb_proj/nlcd92")

nlcd92 <- NULL
for(i in seq(1,4,1)){
  
  tab <- paste0("nlcd",i,"_zh2")
  
  temp <- sf::st_read(dsn = "nlcd.gdb",
                      layer = tab) %>%
    as_tibble()
  
  temp1 <- temp %>%
    pivot_longer(cols = c(2:ncol(.)),
                 names_to = "GISJOIN10",
                 values_to = "pclass") %>%
    pivot_wider(names_from = LABEL,
                values_from = pclass) %>%
    rename(water = `1`,
           urban = `2`,
           other = `3`) %>%
    select(GISJOIN10,water:other)
    
  nlcd92 <- bind_rows(nlcd92,temp1) %>%
    arrange(GISJOIN10)
  
}

### fix duplicates from rasterization
nlcd92 <- nlcd92 %>%
  group_by(GISJOIN10) %>%
  summarize_all(funs(sum(.))) %>%
  mutate(pdev = urban / (urban + other)) %>%
  select(GISJOIN10, pdev)

## save
setwd("C:/Users/scott/Dropbox/urb_proj/tables")
write.dbf(as.data.frame(nlcd92), "nlcd92.dbf")

## bring in 01,11
nlcd0111 <- read_csv("nlcd0111.csv")

## join and save
df <- read.dbf("hu9019.dbf") %>%
  as_tibble() %>%
  select(GISJOIN10,GISJOIN15)

nlcd9211 <- df %>%
  left_join(nlcd92, by = "GISJOIN10") %>%
  rename(pdev92 = "pdev") %>%
  left_join(nlcd0111, by = "GISJOIN15")

## inc. LULC forested: ~8,897 92 tracts > pdev than 2011 tracts
## inc. only LULC resid: ~3,787 tracts > pdev than 2011 tracts
## inc. neither LULC cat: ~2,878 tracts > pdev than 2011 tracts (compared to 1,818 2010 > 2011)

## save
#write.dbf(as.data.frame(nlcd9211), "nlcd9211.dbf")

#setwd("C:/Users/scott/Dropbox/urb_proj/tables")
#nlcd9211 <- read.dbf("nlcd9211.dbf") %>%
#  as_tibble()

## correct misidentified strings
nlcd9211a <- nlcd9211 %>%
  mutate(pdev01 = ifelse(str_detect(pdev01,"DIV"),0,as.numeric(as.character(pdev01))),
         pdev11 = ifelse(str_detect(pdev11,"DIV"),0,as.numeric(as.character(pdev11))))

## save
write.dbf(as.data.frame(nlcd9211a),"nlcd9211.dbf")

#####################################################################################################
#####################################################################################################

nlcd01_raw <- read_csv("nlcd/nlcd01_raw.csv")

nlcd01_raw
"G0600830002402"

#### FIX UP to JOIN in ArcGIS
nlcd01 <- nlcd01_raw %>%
  ## replace NAs with 0s
  mutate_at(vars(starts_with("SE")), funs(ifelse(is.na(.), 0, .))) %>%
  ## convert to GISJOIN and GET pdev var: sum(dev1,dev2,dev3,dev4)/(area - water/ice)
  mutate(GISJOIN = paste0("G",
                          str_pad(Geo_STATE, width = 2, side = "left", pad = "0"),
                          "0",
                          str_pad(Geo_COUNTY, width = 3, side = "left", pad = "0"),
                          "0",
                          str_pad(Geo_CT, width = 6, side = "left", pad = "0")),
         tot_area = SE_T001_001,
         water_ice = SE_T001_002 + SE_T001_003,
         dev_land = SE_T001_004 + SE_T001_005 + SE_T001_006 + SE_T001_007,
         land = tot_area - water_ice,
         pdev = dev_land/land * 100) %>%
  select(GISJOIN,pdev) %>%
  ## get rid of AK, HI, and PR
  mutate(ST = substr(GISJOIN,1,3)) %>%
  filter(!ST %in% c("G02","G15","G72")) %>%
  select(-ST)

nlcd01

## 8,897 (pdev92 > pdev11)

## Save as dbf!!
write.dbf(as.data.frame(nlcd01),"nlcd/nlcd01.dbf")

####################################################################################################
####################################################################################################
####################################################################################################
### ------------------------------------  ARCMAP PROCEDURES  ----------------------------------- ###
####################################################################################################
####################################################################################################
####################################################################################################

###################################################
###         IMPORT NEIGHBORS FILE               ###
###################################################

## change wd
setwd("C:/Users/scott/Dropbox/urb_proj/shp_work/post_hu")

## read table
n1 <- sf::st_read(dsn = "dev.gdb",
                         layer = "neighbors1") %>%
  as_tibble()

##############################################
## read shapefile to select eligible tracts
##############################################
sf1 <- sf::st_read(dsn = "dev.gdb",
                   layer = "hu4019_dev3") %>%
  as_tibble()

sf2 <- sf1 %>%
  select(state,county,GISJOIN,urb_yr,pdev)

  
sf2

#######################################
###  JOIN sf2 to neighbors file
#######################################

n2 <- n1 %>%
  rename(bord_len = LENGTH,
         src_len = tracts10_Shp_Len) %>%
  left_join(sf2, by = c("src_GISJOIN" = "GISJOIN")) %>%
  left_join(sf2[3:5], by = c("nbr_GISJOIN" = "GISJOIN")) %>%
  rename(src_urb_yr = "urb_yr.x",
         src_pdev = "pdev.x",
         nbr_urb_yr = "urb_yr.y",
         nbr_pdev = "pdev.y") %>%
  select(state,county,src_GISJOIN,src_urb_yr,src_pdev,src_len,nbr_GISJOIN,nbr_urb_yr,nbr_pdev,bord_len) %>%
  ## filter to include only tracts wtih that are > 50% pdev and urb_yr >= 2010 OR which are surrounded by higher values
  group_by(src_GISJOIN) %>%
  mutate(nbr_max = max(nbr_urb_yr)) %>%
  filter((src_urb_yr >= 2010 & src_pdev >= 50)|
         (nbr_max < src_urb_yr & src_pdev >= 50))

### take a break and assess

n2 %>%
  group_by(src_GISJOIN) %>%
  count()  
## n = 720

## custom function for round_any
round_any <- function(x, accuracy, f=round){f(x / accuracy) * accuracy}

#### pick it back up with calculations
n3 <- n2 %>%
  ## get rid of neighbors that overlap
  filter(!nbr_GISJOIN %in% src_GISJOIN) %>%
  ## group to calculate new lengths
  group_by(src_GISJOIN) %>%
  ## keep coasts in mind for weighted lengths--subtract sum of border lengths from source length
  mutate(src_len2 = sum(bord_len)) %>%
  ungroup() %>%
  mutate(wt1 = bord_len/src_len2,
         urb_yr_wt = wt1*nbr_urb_yr) %>%
  group_by(state,county,src_GISJOIN,src_urb_yr,src_pdev,src_len,src_len2) %>%
  summarize(avg_yr = sum(urb_yr_wt)) %>%
  ## calculate new urban estimates
  mutate(urb_yr2 =
           case_when(
             avg_yr >= 2005 ~ 2010,
             TRUE ~ round_any(avg_yr, 10)  ## round to nearest 10 (year)
           ))

length(unique(n2$src_GISJOIN))   ## n = 720

n3


## identify 4 missing---> surrounded by all 
n4 <- n2 %>%
  filter(nbr_GISJOIN %in% src_GISJOIN) %>%
  anti_join(n3, by = "src_GISJOIN") %>%
  right_join(n3[c(3,9)], by = c("nbr_GISJOIN" = "src_GISJOIN")) %>%
  drop_na() %>%
  group_by(src_GISJOIN) %>%
  mutate(src_len2 = sum(bord_len)) %>%
  ungroup() %>%
  mutate(wt1 = bord_len/src_len2,
         urb_yr_wt = wt1*urb_yr2) %>%
  group_by(state,county,src_GISJOIN,src_urb_yr) %>%
  summarize(avg_yr = sum(urb_yr_wt)) %>%
  mutate(urb_yr3 =
           case_when(
             avg_yr >= 2005 ~ 2010,
             TRUE ~ round_any(avg_yr, 10)
           )) %>%
  select(-avg_yr)

n4  ## n = 0

## bring together
n5 <- n3 %>%
  ungroup() %>%
  select(state,county,src_GISJOIN,src_urb_yr,urb_yr2) %>%
  rename(urb_yr3 = urb_yr2) %>%
  bind_rows(n4) %>%
  rename(GISJOIN = src_GISJOIN,
         urb_yr = src_urb_yr,
         urb_yr2 = urb_yr3) %>%
  arrange(GISJOIN) %>%
  mutate(change = ifelse(urb_yr == urb_yr2, 0, 1))

sum(n5$change)
## 582/720
## +4 in NYC

#### GET n5 GIS-ready
dev_fix <- n5 %>%
  select(GISJOIN,urb_yr2)

## SAVE as DBF
write.dbf(as.data.frame(dev_fix),"C:/Users/scott/Dropbox/urb_proj/tables/nlcd/dev_fix.dbf")

