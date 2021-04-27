
#########################################################################
#########################################################################
###                                                                   ###
###              Intersect 1940-80 Urbanization Script                ###
###                            01/11/2020                             ###
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

###################################################
###   LOAD DATA                                 ###
###################################################

### set working directory
setwd("C:/Users/scott/Dropbox/urb_proj/shp_work")

## read in t40_80 to get raster estimates
t40_80 <- sf::st_read(
  dsn = "urb_max.gdb",
  layer = "t40_80"
) %>%
  as_tibble()

t40_80join <- t40_80 %>%
  # mutate(CO_JOIN = paste0(substr(GISJOIN,1,8))) %>%
  select(GISJOIN, tab40_SUM:tab80_SUM, sqmi40:sqmi80)

## make t40_80join long
rast_est <- NULL
for (i in seq(40, 80, 10)) {
  tab <- paste0("tab", i, "_SUM")
  area <- paste0("sqmi", i)

  temp <- t40_80join %>%
    select(GISJOIN, tab, area) %>%
    mutate(yr = as.numeric(paste0("19", i))) %>%
    rename(
      hu_rast = tab,
      sqmi = area
    )

  rast_est <- bind_rows(rast_est, temp)
}

rast_est

#######################################################
###  LOAD ZEROES                                    ###
#######################################################

### load tracts: needed zeroes corrections (from post-processessing); n = 15
zeroes <- read.dbf("C:/Users/scott/Dropbox/urb_proj/tables/zeroes.dbf") %>%
  tibble() %>%
  mutate(hu = 0)

rast_est0 <- rast_est %>%
  mutate(hu_rast = ifelse(GISJOIN %in% zeroes$GISJOIN, 0, hu_rast))

rast_est <- rast_est0


#######################################################
###  INTERSECTION DATA (AREAL WEIGHTING)            ###
#######################################################

### set working directory
setwd("C:/Users/scott/Dropbox/urb_proj/shp_work")

## import files from ArcMap Intersection
df <- NULL ## create empty data frame
for (i in seq(40, 80, 10)) { ## sequence from 40 to 80 by 10

  ## read in intersection files
  temp <- sf::st_read(
    dsn = "urb40_80.gdb",
    layer = paste0("int", i)
  ) %>%
    as_tibble()

  ## keep only releveant columns
  temp1 <- temp %>%
    select("GISJOIN", "GISJOIN_1", "sqmi", "sqmi_1", "sqmi_int", paste0("hu", i)) %>%
    mutate(
      yr = paste0("19", i), ## add year: needed for rbind
      yr = as.numeric(yr)
    )

  ## change unique hu (by decade) into generic "hu" to allow for rbind
  setnames(temp1, paste0("hu", i), "hu")

  ## create long data frame
  df <- bind_rows(df, temp1)
}

df %>%
  filter(GISJOIN %in% zeroes$GISJOIN) %>%
  arrange(GISJOIN, yr)

## add zeroes
df1 <- df %>%
  left_join(zeroes,
    by = "GISJOIN",
    suffix = c("", "_new")
  ) %>%
  mutate(hu = ifelse(!is.na(hu_new), 0, hu)) %>%
  select(-hu_new)

df1 %>%
  filter(GISJOIN %in% zeroes$GISJOIN) %>%
  arrange(GISJOIN, yr)

## get AW estimates
dr_aw <- df1 %>%
  mutate(
    pcover_t = sqmi_int / sqmi, ## % coverage for target tract (t)
    pcover_s = sqmi_int / sqmi_1, ## % coverage for source tract (s)
    hu_int = pcover_s * hu
  ) %>%
  ## estimated hu in tract t
  group_by(GISJOIN, yr) %>%
  summarize(
    sqmi = median(sqmi),
    pcover_t = sum(pcover_t),
    pcover_s = sum(pcover_s),
    hu_aw = sum(hu_int)
  ) %>%
  arrange(yr, GISJOIN)

## check histogram
dr_aw %>%
  filter(between(pcover_t, 0.9, 0.99)) %>%
  ggplot(aes(x = pcover_t)) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0.9, 0.99, 0.01))

## filter out tract-years with coverage < 0.97
dr_aw <- dr_aw %>%
  filter(pcover_t > 0.97) %>%
  select(GISJOIN, yr, sqmi, hu_aw)

dr_aw ## dasymetrically-refined areal weighting

## get dasym. ref. maximum reabsorption
dr_mr <- rast_est %>%
  select(GISJOIN, yr, sqmi, hu_rast) %>%
  arrange(yr, GISJOIN)


#########################################################################
###  "TARGET DENSITY WEIGHTING"                                      ###
#########################################################################

### set working directory
setwd("C:/Users/scott/Dropbox/urb_proj/shp_work")

df_spec <- NULL ## create empty data frame
for (i in seq(40, 80, 10)) { ## set sequence from 40 to 80

  x <- i + 10 ## get 1990 data for correcting 1980 data

  ## read in intersection files
  temp <- sf::st_read(
    dsn = "tdw.gdb",
    layer = paste0("int", i, "_", x)
  ) %>%
    as_tibble()

  y <- paste0("hu", i)
  y1 <- paste0("hu", i, "_1")

  ## organize to allow for row binding
  temp1 <- temp %>%
    rename(
      hu_1 = y,
      hu_12 = y1
    ) %>%
    select(GISJOIN, sqmi, GISJOIN_1, sqmi_1, hu_1, GISJOIN_12, sqmi_12, hu_12, sqmi_int) %>%
    mutate(yr = as.integer(paste0("19", i)))

  ## row bind into single long dataframe
  df_spec <- bind_rows(df_spec, temp1)
}

df_spec

### add zeroes
df_spec0 <- df_spec %>%
  left_join(zeroes,
    by = "GISJOIN"
  ) %>%
  mutate(
    hu_1 = ifelse(!is.na(hu), 0, hu_1),
    hu_12 = ifelse(!is.na(hu), 0, hu_12)
  ) %>%
  select(-hu)

## check zeroes
df_spec0 %>%
  filter(GISJOIN %in% zeroes$GISJOIN) %>%
  arrange(GISJOIN, yr)

### conduct intersection procedure
df_spec1 <- df_spec0 %>%
  # reomve target year zeroes (precaution)
  filter(!(hu_1 > 0 & hu_12 == 0)) %>%
  ## remove these zeros out of caution
  mutate(
    pcover = sqmi_int / sqmi, ## getting proportion overlay for target tract
    pcover_1 = sqmi_int / sqmi_1, ## getting proportion overlay for yr 1
    pcover_12 = sqmi_int / sqmi_12, ## getting proportion overlay for yr 2
    hu_est_1 = pcover_1 * hu_1, ## getting AW fraction of HU for yr 1
    hu_est_12 = pcover_12 * hu_12, ## getting AW fraction of HU for yr 2
    CO = substr(GISJOIN, 1, 8), ## getting unique county code
    ST = substr(GISJOIN, 1, 3), ## getting state code
    flag =
      ## select problematic zeroes from 1960 census by state/county
    case_when(
      yr == 1960 & hu_1 < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
      yr == 1960 & hu_1 < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CT x 2
      yr == 1960 & GISJOIN_1 %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223") ~ 1, ## OR, TX x 2, PA
      yr == 1950 & hu_12 < 100 & ST %in% c("G25", "G34", "G41", "G44") ~ 1,
      yr == 1950 & hu_12 < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1,
      yr == 1950 & GISJOIN_12 %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(flag == 0) %>%
  select(-flag)

df_spec1

## checking zeroes
df_spec1 %>%
  filter(GISJOIN %in% zeroes$GISJOIN) %>%
  arrange(GISJOIN, yr)


# df_spec1 <- df_spec0 %>%
#  filter(!(hu_1 > 0 & hu_12 == 0)) %>%
#  mutate(pcover1 = sqmi_int/sqmi,
#         pcover2 = sqmi_int/sqmi_1,
#         pcover3 = sqmi_int/sqmi_12) %>%
#  group_by(GISJOIN_12,yr) %>%
#  mutate(x = sqmi_int/sqmi_12*hu_12) %>%
#  ungroup() %>%
#  group_by(GISJOIN_1,yr) %>%
#  mutate(y = sum(x)) %>%
#  ungroup() %>%
#  mutate(z = x/y*(sqmi_int/sqmi_1*hu_1)) %>%
#  group_by(GISJOIN,sqmi,yr) %>%
#  summarize(pcover1 = sum(pcover1),
#            pcover2 = sum(pcover2),
#            pcover3 = sum(pcover3),
#            z = sum(z)) %>%
#  arrange(yr,GISJOIN)

# df_spec1 %>%
#  filter(GISJOIN == "G3600850029102")

## TDW
# df_tdw <- df_spec1 %>%
#  group_by(GISJOIN_1,GISJOIN_12,yr) %>%
#  mutate(hu_sum_1 = sum(hu_est_1),
#         hu_sum_12 = sum(hu_est_12)) %>%
#  ungroup() %>%
#  mutate(hu_fin_12 = hu_est_12/hu_12*hu_1)

# df_tdw %>%
#  filter(str_detect(GISJOIN_12,"G36")) %>%
#  as.data.frame() %>%
#  tail(10)

# df_tdw %>%
#  filter(GISJOIN == "G3600850029102" & yr == 1940) %>%
#  as.data.frame() %>%
#  tail(10)



### grouping by 2010 GISJOIN and summarizing HU estimates
df_spec2 <- df_spec1 %>%
  group_by(GISJOIN, ST, CO, yr) %>%
  summarize(
    sqmi = median(sqmi),
    sqmi_int = sum(sqmi_int), ## add up areas (in sq. mi.) for intersected portion, yr 1, and yr 2
    sqmi_1 = sum(sqmi_1),
    sqmi_12 = sum(sqmi_12),
    pcover_1 = sum(pcover_1), ## add up coverage to determine which tracts to keep/discard
    pcover_12 = sum(pcover_12),
    hu_est_1 = sum(hu_est_1), ## get housing estimates via summation
    hu_est_12 = sum(hu_est_12),
    n = n()
  ) %>%
  mutate(pcover_tot = sqmi_int / sqmi) %>%
  ## get total % coverage
  arrange(yr, GISJOIN) %>%
  filter(pcover_tot > 0.97) ## cutoff chosen using histogram below

df_spec2

## histogram
df_spec2 %>%
  filter(pcover_tot > 0.75 & pcover_tot < 0.99) %>%
  ggplot(aes(x = pcover_tot)) +
  geom_histogram(binwidth = 0.01) ## cutoff = 0.97

### remove 2010 tracts from equation to allow calculations
df_alt <- df_spec1 %>%
  filter(GISJOIN %in% df_spec2$GISJOIN) %>%
  ## keep only atoms with 2010 tracts > 97% covered
  # select(-GISJOIN,-sqmi) %>%   ## remove 2010 tracts
  group_by(yr, GISJOIN_1, sqmi_1, hu_1, GISJOIN_12, sqmi_12, hu_12) %>%
  # hu_sqmi_1,hu_sqmi_12) %>%  ## group by yr 1 and yr 2 intersections only
  summarize(
    sqmi_int = sum(sqmi_int), ## summarize those cases where combinations are required
    pcover = sum(pcover),
    pcover_1 = sum(pcover_1),
    pcover_12 = sum(pcover_12),
    hu_est_1 = sum(hu_est_1),
    hu_est_12 = sum(hu_est_12)
  ) %>%
  arrange(yr, GISJOIN_12)

### take a look at tracts in NY state (worst 1940 cases)
df_alt %>%
  filter(str_detect(GISJOIN_12, "G36"))

# df_alt %>%
#  filter(GISJOIN_12 == "G36008500323" & yr == 1940) %>%
#  as.data.frame() %>%
#  tail(10)

## rural Kansas example
df_alt %>%
  filter(yr == 1980 & str_detect(GISJOIN_1, "G200055"))

### group by pseudo-target year (year 2) to get smaller geog. estimates
# df_alt1 <- df_alt %>%
# group_by(GISJOIN_12,yr) %>%
# mutate(hu_sum_12 = sum(hu_est_12),
#       hu_fin_12 = ifelse(hu_sum_12 > 0, hu_est_12/hu_sum_12*hu_est_1, 0)) %>%
# summarize(pcover_1 = sum(pcover_1),
#          pcover_12 = sum(pcover_12),
#          hu_est_1 = sum(hu_est_1),  ## comparison
#          hu_fin_12 = sum(hu_fin_12)) %>%
# arrange(yr,GISJOIN_12) %>%
# filter(pcover_12 > 0.97 &   ## keeps only fully covered s+10 tracts
#       pcover_1 < 0.97)  ## gets rid of tracts that went large to small & 1-to-1 tracts (no advantage of TDW)


df_alt1 <- df_alt %>%
  # group_by(yr,GISJOIN_12,sqmi_12) %>%  ## grouping by year 2
  # mutate(hu_sum_1 = sum(hu_est_1),
  #       hu_sum_12 = sum(hu_est_12)) %>%
  # ungroup() %>%
  # mutate(hu_fin_12 = hu_est_12/hu_sum_12*hu_1))
  #       #hu_sum_1 = sum(hu_est_1),  ## summing source year estimates in target tract geographies (mutate keeps intersections intact) for % calcs
  #       #hu_sum_12 = sum(hu_est_12),  ## summing pseudo-target tract estimates for source year in pseudo-target tract geographies
  #       t_12 = n()) %>% ## getting number
  group_by(yr, GISJOIN_1, sqmi_1, hu_1) %>% ## now grouping by year 1 to get sums
  mutate(hu_sum_12 = sum(hu_est_12)) %>%
  # t_1 = n()) %>%
  ungroup() %>%
  mutate(hu_fin_12 = ifelse(hu_sum_12 > 0, hu_est_12 / hu_sum_12 * hu_1, 0)) ## determine % of HU in summed tract overlaps
# hu_fin_12 = ifelse(hu_sum_12 > hu_sum_1, hu_est_12, hu_fin_12))  ## if YSB est in Y2 > HU est in Y1, then choose YSB in Y2 (could be caused by messed up AW interp)

### take a look at tracts in NY state (worst 1940 cases)
df_alt1 %>%
  filter(str_detect(GISJOIN_12, "G36"))

## rural Kansas example
df_alt1 %>%
  filter(yr == 1980 & str_detect(GISJOIN_1, "G200055"))


### put into pseudo-target year tracts (s + 10) (suffix = _12)
df_alt2 <- df_alt1 %>%
  group_by(yr, GISJOIN_12) %>%
  ## grouping by year 2 vars
  summarize( # sqmi_int = sum(sqmi_int),  ## summing variables
    # sqmi_1 = sum(sqmi_1),
    pcover_1 = sum(pcover_1),
    pcover_12 = sum(pcover_12),
    hu_est_1 = sum(hu_est_1),
    hu_est_12 = sum(hu_est_12),
    hu_fin_12 = sum(hu_fin_12)
  ) %>%
  ## summing number of year 2 tracts
  arrange(yr, GISJOIN_12) %>%
  filter(pcover_12 > 0.97 & ## keeps only fully covered s+10 tracts
    pcover_1 < 0.97) %>%
  ## gets rid of tracts that went large to small & 1-to-1 tracts (no advantage of TDW)
  select(GISJOIN_12, yr, pcover_1:hu_fin_12)

### clean
# df_alt3 <- df_alt2 %>%
#  select(GISJOIN_12,yr,sqmi_12,hu_12,hu_est_1,hu_est_12,hu_fin_12)

df_alt2
## n = 34,403 (in s+10 tracts)

### clean up workspace
# rm(list = setdiff(ls(),c("df","df1","df_spec","df_spec0","df_alt3","zeroes","rast_est","t40_80join")))


dr_tdw0 <- df_spec0 %>%
  left_join(df_alt2[c(1, 2, 7)], by = c("yr", "GISJOIN_12")) %>%
  ## 1: GISJOIN; 2: yr; 7: hu_fin_12
  filter(!is.na(hu_fin_12)) %>%
  mutate(
    ST = substr(GISJOIN, 1, 3), ## get state ID for later calcs
    CO = substr(GISJOIN, 1, 8), ## get county ID for later calcs
    pcover1 = sqmi_int / sqmi, ## 2010 tract coverage  (most important)
    pcover2 = sqmi_int / sqmi_1, ## first tract coverage
    pcover3 = sqmi_int / sqmi_12, ## second tract coverage
    hu_1_est = hu_1 * pcover2,
    hu_tdw = hu_fin_12 * pcover3,
    ## flag and remove errant zeroes
    flag =
      case_when(
        yr == 1960 & hu_1 < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
        yr == 1960 & hu_1 < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CTx23
        yr == 1960 & GISJOIN_1 %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223") ~ 1, ## RO,TX x 2, PA
        TRUE ~ 0
      )
  ) %>%
  filter(flag == 0) %>%
  select(-flag) %>%
  ## group by 2010 tract geographies
  group_by(GISJOIN, sqmi, yr) %>%
  summarize(
    pcover1 = sum(pcover1),
    pcover2 = sum(pcover2),
    pcover3 = sum(pcover3),
    # hu_1_est = sum(hu_1_est),
    hu_tdw = sum(hu_tdw, na.rm = TRUE)
  ) %>%
  arrange(yr, GISJOIN)

##### join with df_spec0 (been zero-corrected) & join back with original to put in 2010 tracts
# df_alt3 <- df_spec0 %>%
#  left_join(df_alt2[c(1,2,7)],by=c("yr","GISJOIN_12")) %>% ## 1: GISJOIN; 2: yr; 7: hu_fin_12
#  mutate(ST = substr(GISJOIN,1,3),  ## get state ID for later calcs
#         CO = substr(GISJOIN,1,8),  ## get county ID for later calcs
#         pcover1 = sqmi_int/sqmi,  ## 2010 tract coverage  (most important)
#         pcover2 = sqmi_int/sqmi_1,  ## first tract coverage
#         pcover3 = sqmi_int/sqmi_12,  ## second tract coverage
#         hu_1_est = hu_1*pcover2,
#         hu_est = ifelse(!is.na(hu_fin_12), ## if an estimate is there: use DR-TDW (hu_fin_12)
#                         hu_fin_12*pcover3,
#                         NA),    ## if not (is NA), use DR-AW (rest)
#         tdw = ifelse(!is.na(hu_fin_12), 1, 0),  ## for counting purposes
#         ## flag and remove errant zeroes
#         flag =
#           case_when(
#             yr == 1960 & hu_1 < 100 & ST %in% c("G25","G34","G44") ~ 1,  #MA, NJ, RI
#             yr == 1960 & hu_1 < 100 & CO %in% c("G0601130","G5300530","G4900110","G2901650","G0900030","G0900130") ~ 1,  # CA, WA, UT, MO, CTx23
#             yr == 1960 & GISJOIN_1 %in% c("G41005100044", "G48035500028","G48043900006","G42010100223") ~ 1,  ## RO,TX x 2, PA
#             TRUE ~ 0)) %>%
#  filter(flag == 0) %>%
#  select(-flag) %>%
#  ## group by 2010 tract geographies
#  group_by(GISJOIN,sqmi,yr) %>%
#  summarize(pcover1 = sum(pcover1),
#            pcover2 = sum(pcover2),
#            pcover3 = sum(pcover3),
#            hu_1_est = sum(hu_1_est),
#            hu_est = sum(hu_est, na.rm = TRUE),
#            tdw = sum(tdw)) %>%
#  mutate(tdw = ifelse(tdw >= 1, 1, 0)) %>% ## fix tdw
#  arrange(yr, GISJOIN)

## n = 76,591 (# of DR-TDW estimated tracts)

## check
# df_alt3 %>%
#  filter(GISJOIN == "G3600850029102")

## histogram showing percent coverage (couls use 96% instead of 97% but will keep it consistent)
dr_tdw0 %>%
  # filter(between(pcover1,0.85,0.99)) %>%
  ggplot(aes(x = pcover1)) +
  geom_histogram(binwidth = 0.01)

### keep only those with pcover1 > 0.97
dr_tdw <- dr_tdw0 %>%
  filter(pcover1 > 0.97) %>%
  ## must remove these: under 0.97 indicates partially covered 2010 tracts
  select(GISJOIN, yr, sqmi, hu_tdw) %>%
  mutate(hu_tdw = ifelse(GISJOIN %in% zeroes$GISJOIN, 0, hu_tdw))

# df_alt4  ## n = 180,344

## check tdw
# sum(df_alt4$tdw)  ## n = 74,407

dr_tdw ## n = 51,216

rm(dr_tdw0)


###########################################################################
###########################################################################
#####        Loading multi-int files to break up large tracts        ######
#####                 "TARGET DENSITY WEIGHTING"                     ######
#####           using only NHGIS harmonized 1990 data                ######
###########################################################################
###########################################################################

### set working directory
setwd("C:/Users/scott/Dropbox/urb_proj/shp_work")

df_spec <- NULL ## create empty data frame
for (i in seq(40, 80, 10)) { ## set sequence from 40 to 80

  ## read in intersection files
  temp <- sf::st_read(
    dsn = "tdw90.gdb",
    layer = paste0("int", i)
  ) %>%
    as_tibble()

  x <- paste0("hu", i)
  x1 <- paste0("hu", i, "_1")

  ## organize to allow for row binding
  temp1 <- temp %>%
    rename(
      hu = x,
      hu_1 = x1
    ) %>%
    select(GISJOIN, sqmi, hu, GISJOIN_1, sqmi_1, hu_1, sqmi_int) %>%
    mutate(yr = as.integer(paste0("19", i)))

  ## row bind into single long dataframe
  df_spec <- bind_rows(df_spec, temp1)
}

df_spec

### add zeroes
df_spec0 <- df_spec %>%
  left_join(zeroes,
    by = "GISJOIN",
    suffix = c("", "_z")
  ) %>%
  mutate(
    hu = ifelse(!is.na(hu_z), 0, hu),
    hu_1 = ifelse(!is.na(hu_z), 0, hu_1)
  ) %>%
  select(-hu_z) %>%
  rename(
    sqmi_t = sqmi,
    hu_t = hu,
    sqmi_s = sqmi_1,
    hu_s = hu_1
  )

## check zeroes
df_spec0 %>%
  filter(GISJOIN %in% zeroes$GISJOIN)

df_spec0

## clean other strange cases from 1960
df_spec1 <- df_spec0 %>%
  mutate(
    ST = substr(GISJOIN, 1, 3),
    CO = substr(GISJOIN, 1, 8)
  ) %>%
  mutate(
    flag =
      case_when(
        yr == 1960 & hu_s < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
        yr == 1960 & hu_s < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CTx23
        yr == 1960 & GISJOIN_1 %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223") ~ 1, ## OR, TX x 2, PA
        TRUE ~ 0
      )
  ) %>%
  filter(flag == 0) %>%
  select(-flag)

df_spec1

## conduct TDW90
df_spec2 <- df_spec1 %>%
  mutate(
    pcover_t = sqmi_int / sqmi_t,
    pcover_s = sqmi_int / sqmi_s,
    hu_int = hu_t * pcover_t
  ) %>%
  group_by(GISJOIN_1, yr) %>%
  mutate(hu_int_sum = sum(hu_int)) %>%
  ungroup() %>%
  mutate(hu_atom = hu_int / hu_int_sum * hu_s) %>%
  group_by(GISJOIN, yr, sqmi_t) %>%
  summarize(
    pcover_t = sum(pcover_t),
    pcover_s = sum(pcover_s),
    hu_tdw90 = sum(hu_atom)
  ) %>%
  arrange(yr, GISJOIN)

df_spec2

## histogram  (could go with 98%; but will stick with 97% for consistency)
df_spec2 %>%
  filter(between(pcover_t, 0.85, 0.999)) %>%
  ggplot(aes(x = pcover_t)) +
  geom_histogram(binwidth = 0.01)

## fix special cases
dr_tdw90 <- df_spec2 %>%
  filter(pcover_t > 0.97 & ## keeps only fully covered s+10 tracts
    pcover_s < 0.97) %>%
  ## gets rid of tracts that went large to small & 1-to-1 tracts (no advantage of TDW)
  rename(sqmi = sqmi_t) %>%
  select(GISJOIN, yr, sqmi, hu_tdw90)


dr_tdw90 ## n = 109,476

## clean up
rm(list = setdiff(ls(), c("dr_aw", "dr_mr", "dr_tdw", "dr_tdw90", "zeroes")))

################################################################
### -------------------------------------------------------- ###
###   BASIC HAMMER PROCEDURE                                 ###
### -------------------------------------------------------- ###
################################################################

## change working directory:
setwd("C:/Users/scott/Dropbox/urb_proj")

## Import county files (at BG level)
counties <- read_csv("tables/county_tracts.csv") %>%
  mutate(
    CO_CALC = JOIN_CO,
    CO_JOIN = substr(GISJOIN, 1, 8)
  ) %>%
  select(STATE, COUNTY, GISJOIN, CO_CALC, CO_JOIN, hu40co:hu80co)

## fix it up & make long:
co_fix <- NULL
for (i in seq(40, 80, 10)) {
  col_name <- paste0("hu", i, "co")

  temp <- counties %>%
    ## make long
    pivot_longer(
      cols = c(hu40co:hu80co),
      names_to = "yr",
      values_to = "hu_co"
    ) %>%
    filter(yr == col_name) %>%
    group_by(STATE, COUNTY, CO_CALC, CO_JOIN) %>%
    summarize(hu_co = median(hu_co)) %>%
    mutate(yr = as.numeric(paste0("19", i))) %>%
    select(STATE:CO_JOIN, yr, hu_co)


  co_fix <- bind_rows(co_fix, temp) %>%
    unique() %>%
    arrange(CO_CALC, yr)
}

co_fix


## join with dr_mr (raster estimates)
dr_ham <- dr_mr %>%
  mutate(CO_JOIN = substr(GISJOIN, 1, 8)) %>%
  # join with co_fix (remove 3: GISJOIN)
  left_join(co_fix, by = c("CO_JOIN", "yr")) %>%
  # reorganize columns
  select(STATE, COUNTY, CO_CALC, CO_JOIN, GISJOIN, yr, sqmi, hu_co, hu_rast) %>%
  # calculate hammer method on max. reabsorp. estimates
  group_by(CO_CALC, yr) %>%
  mutate(hu_sum = sum(hu_rast)) %>%
  ungroup() %>%
  mutate(hu_ham = hu_rast / hu_sum * hu_co)

dr_ham

## clean up again
rm(list = setdiff(ls(), c("dr_aw", "dr_ham", "dr_mr", "dr_tdw", "dr_tdw90", "co_fix", "zeroes")))

################################################################
### -------------------------------------------------------- ###
###           ORGANIZE INTO SINGLE ESTIMATES FILE            ###
### -------------------------------------------------------- ###
################################################################

dr_aw
dr_ham
dr_mr
dr_tdw
dr_tdw90

## only use sqmi from dr_mr
## join on dr_mr
estimates <- dr_ham %>%
  # left_join(dr_mr, by = c("GISJOIN", "yr")) %>%
  left_join(dr_aw[-3], by = c("GISJOIN", "yr")) %>%
  left_join(dr_tdw[-3], by = c("GISJOIN", "yr")) %>%
  left_join(dr_tdw90[-3], by = c("GISJOIN", "yr")) %>%
  # rename(hu_mr = hu_rast) %>%
  # create single vector estimates (1940: tdw > aw; 1950-80: tdw90 > aw)
  mutate(hu_vect = case_when(
    yr == 1940 & !is.na(hu_tdw) ~ hu_tdw,
    yr == 1940 & is.na(hu_tdw) & !is.na(hu_tdw90) ~ hu_tdw90,
    yr > 1940 & !is.na(hu_tdw90) ~ hu_tdw90,
    TRUE ~ hu_aw
  )) %>%
  # overwrite 1960 tracts with zeroes (precautionary measure)
  mutate(
    ST = substr(GISJOIN, 1, 3),
    CO = substr(GISJOIN, 1, 8),
    flag =
      case_when(
        yr == 1960 & hu_vect < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
        yr == 1960 & hu_vect < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CT x 2
        TRUE ~ 0
      )
  ) %>%
  # assign estimate types (est_type) to track which HU procedure was used for each tract-year
  mutate(
    hu_vect = ifelse(flag == 1, NA, hu_vect),  # calc. hu_vect for modified hammer procedure (below)
    est_type = case_when(
      GISJOIN %in% zeroes$GISJOIN ~ "zero",  # zeroes
      flag == 1 ~ "mr_over",  # max. reabsorption overwrite of 1960 problems (flagged above)
      flag == 0 & yr == 1940 & !is.na(hu_tdw) ~ "tdw",  # 1950 TDW on 1940 tracts
      flag == 0 & yr == 1940 & is.na(hu_tdw) & !is.na(hu_tdw90) ~ "tdw90",  # 1990 TDW on 1940 tracts w/o 1950 YSB data
      flag == 0 & yr > 1940 & !is.na(hu_tdw90) ~ "tdw90",  # 1990 TDW on the rest
      flag == 0 & is.na(hu_vect) ~ "mr",  # max. reabsorption (raster-based estimates)
      TRUE ~ "aw"  # rest are (dasymetrically-refined) areal weighting
    )
  ) %>%
  # create final HU estimates
  mutate(
    hu_est = case_when(
      est_type == "mr_over" ~ hu_ham,
      est_type == "zero" ~ 0,
      est_type == "tdw" ~ hu_tdw,
      est_type == "tdw90" ~ hu_tdw90,
      est_type == "aw" ~ hu_aw,
      est_type == "mr" ~ hu_ham
      )
    ) %>%
  # calculate hu/sqmi
  mutate(hu_sqmi = hu_est/sqmi) %>%
  select(STATE:yr, hu_co, hu_rast, hu_ham, hu_aw:hu_tdw90, hu_vect, hu_est, est_type, hu_sqmi, sqmi, -CO_JOIN) %>%
  arrange(GISJOIN, yr)

estimates

## check groups
estimates %>%
  group_by(est_type) %>%
  count()


################################################################
### -------------------------------------------------------- ###
###               MODIFIED HAMMER PROCEDURE                  ###
### -------------------------------------------------------- ###
################################################################

## Aim to subtract vector estimates from county totals before distributing MR (raster) estimates
## Don't end up using because

## create new DF for Modified Hammer (mod_ham)
#mod_ham <- estimates %>%
#  # group by CO_CALC code and year to calculate sums and totals
#  group_by(CO_CALC, yr) %>%
#  mutate(
#    hu_rast_sum = sum(hu_rast),
#    hu_vect_sum = sum(hu_vect, na.rm = TRUE),
#    n_vect = sum(!is.na(hu_vect)), ## county vector-polygon-interpolated tracts per county-year
#    n_tot = n(),
#    p_vect = n_vect / n_tot * 100
#  ) %>%
  # calculate vector difference (v_diff) of county-level estimates and aggregated vector tract estimates;
  # calculate mod. ham. estimate (is.na(hu_vect) is raster estimated tract-year)
#  mutate(
#    v_diff = hu_co - hu_vect_sum,
#    hu_mh = ifelse(is.na(hu_vect), (hu_rast / hu_rast_sum) * v_diff, NA)
#  ) %>%
#  # fix problem counties for 1960
#  mutate(
#    ST = substr(GISJOIN, 1, 3),
#    CO = substr(GISJOIN, 1, 8),
#    flag =
#      case_when(
#        yr == 1960 & hu_vect < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
#        yr == 1960 & hu_vect < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CT x 2
#        TRUE ~ 0
#      ),
#    hu_mh = ifelse(flag == 1, NA, hu_mh)
#  ) %>%
  # check difference bw original hammer procedure and modified hammer procedure
#  mutate(
#    med = (hu_mh + hu_ham) / 2,
#    p_diff = abs(hu_mh - hu_ham) / med * 100,
#    n_rast = n_tot - n_vect
#  ) %>%
#  # clean up a bit
#  select(-med, -ST, -CO) %>%
#  arrange(-p_diff)

#mod_ham

## create df to visualize difference bw orig. ham. and mod. ham.
#test1 <- mod_ham %>%
#  filter(!is.na(hu_mh) & # inc. only tract-years with mh estimates: excludes tract-yrs w/ vector-based estimates
#    p_vect > 0 & # inc. only tract-years in county-yrs w/ some vector-based estimtes (mh and ham are equivalent for the rest)
#    hu_mh > 0) # inc. only tract-years w/ hu_mh > 0 (anything less is automatically assigned ham val.)

# test1 # n = 21,992

## scatterplot
#test1 %>%
  # mutate(p_rast = 100 - p_vect) %>%
#  ggplot(aes(x = p_vect, y = p_diff)) +
#  geom_point() +
  # geom_smooth(method = "lm") +
#  theme_bw()

## just keep original HAMMER (no replacement)

## clean up
#rm(mod_ham, test1)

##########################################################
###             BRING IN 1990-2010 DATA                ###
##########################################################

# load in data
hu90_19 <- read.dbf("tables/hu9019.dbf") %>%
  as_tibble() %>%
  select(-c(COUNTY, STATE))

## separate hu and sqmi and make long
hu9019_hu <- hu90_19 %>%
  select(-c(sqmi90:sqmi19)) %>%
  pivot_longer(
    cols = c(hu90:hu19),
    names_to = "yr",
    values_to = "hu"
  ) %>%
  mutate(
    yr = substr(yr, 3, 4),
    yr = ifelse(yr == "90", "1990", paste0("20", yr))
  )

hu9019_hu

## now for sqmi
hu9019_sqmi <- hu90_19 %>%
  select(-c(hu90:hu19)) %>%
  pivot_longer(
    cols = c(sqmi90:sqmi19),
    names_to = "yr",
    values_to = "sqmi"
  ) %>%
  mutate(
    yr = substr(yr, 5, 6),
    yr = ifelse(yr == "90", "1990", paste0("20", yr))
  )

hu9019_sqmi

## bring back together
hu9019 <- left_join(hu9019_hu,
  hu9019_sqmi,
  by = c("GISJOIN10", "GISJOIN15", "yr")
) %>%
  mutate(
    yr = as.integer(yr),
    hu_co = NA,
    hu_rast = hu,
    hu_ham = hu,
    hu_aw = hu,
    hu_tdw = hu,
    hu_tdw90 = hu,
    hu_vect = hu,
    hu_est = hu,
    est_type = "nhgis",
    hu_sqmi = hu_est/sqmi
  ) %>%
  select(GISJOIN10:yr, hu_co:hu_sqmi, sqmi)

hu9019

## for joining
join <- hu9019 %>%
  select(GISJOIN10, GISJOIN15) %>%
  filter(!duplicated(GISJOIN10))

## join with estimates
hu4019 <- estimates %>%
  left_join(join, by = c("GISJOIN" = "GISJOIN10")) %>%
  rename(GISJOIN10 = GISJOIN) %>%
  select(GISJOIN10, GISJOIN15, yr:sqmi) %>%
  bind_rows(hu9019) %>%
  ## fill out missing combinations
  complete(
    GISJOIN10, 
    nesting(yr), 
    fill = list(hu_rast = 0, hu_ham = 0, hu_aw = NA, hu_tdw = NA, hu_tdw90 = NA, hu_vect = 0, hu_est = 0, est_type = "zero")) %>%
  # add in 10 tracts missing from 1940-80 data (all zeroes)
  mutate(
    GISJOIN15 = ifelse(is.na(GISJOIN15), GISJOIN10, as.character(GISJOIN15)),
    sqmi = ifelse(sqmi == 0, NA, sqmi)
  ) %>%
  ## n = 10*9 (completely covered by clip feature--e.g., water)
  select(GISJOIN10, GISJOIN15, yr, hu_co:sqmi)

hu4019

## JOIN with state/county

## CORRECT state/county
st_co <- fips_codes %>%
  ## from tidyverse packages
  mutate(CO_JOIN = paste0("G", state_code, "0", county_code, "0")) %>%
  select(state, county, CO_JOIN) %>%
  # fix Shannon County, SD -> Oglala Lakota Co.
  mutate(county = ifelse(str_detect(CO_JOIN, "G460102"), "Oglala Lakota County", county)) %>%
  as_tibble()

# ADD in STATE/COUNTY
hu4019 <- hu4019 %>%
  mutate(CO_JOIN = substr(GISJOIN10, 1, 8)) %>%
  left_join(st_co, by = "CO_JOIN") %>%
  rename(
    ST = state,
    CO = county
  ) %>%
  select(GISJOIN10, GISJOIN15, ST, CO, yr:sqmi)

hu4019

## save
write_csv(hu4019, "tables/hu4019.csv")

## clean up
rm(list = setdiff(ls(), "hu4019"))


##################################################
###  URBANIZATION CALCULATIONS                 ###
##################################################

## MAKE WIDE, CLEAN, and ADD URBANIZATION YEAR COLUMN
hu4019_urb <- hu4019 %>%
  # create columns for cleaner pivot_wider
  mutate(
    yr = str_sub(yr, 3, 4),
    hu = hu_est,
    ) %>%
  # remove excess columns
  select(GISJOIN10:CO, yr, hu, hu_sqmi, sqmi) %>%
  # make wide
  pivot_wider(
    names_from = yr,
    names_sep = "",
    values_from = c(hu, hu_sqmi, sqmi)
  ) %>%
  # repair list designations
  unnest() %>%
  # assign urbanization year 1 (UY1)
  mutate(
    UY1 =
      case_when(
        # using 200 HU/SQMI to define UY1
        hu_sqmi40 >= 200 ~ 1940,
        hu_sqmi50 >= 200 ~ 1950,
        hu_sqmi60 >= 200 ~ 1960,
        hu_sqmi70 >= 200 ~ 1970,
        hu_sqmi80 >= 200 ~ 1980,
        hu_sqmi90 >= 200 ~ 1990,
        hu_sqmi00 >= 200 ~ 2000,
        hu_sqmi10 >= 200 ~ 2010,
        hu_sqmi19 >= 200 ~ 2019,
        TRUE ~ 2035  # use 2035 for neighbor weighting procedure
      )
  ) %>%
  # add GEOID10 for joining with data from Census
  mutate(
    GEOID15 = paste0(
      str_sub(GISJOIN15, 2, 3),
      str_sub(GISJOIN15, 5, 7),
      str_sub(GISJOIN15, 9, nchar(GISJOIN15))
    )
  ) %>%
  # organize
  select(GISJOIN10, GISJOIN15, GEOID15, ST, CO, hu40:UY1)  

glimpse(hu4019_urb)

## save!!
write.dbf(as.data.frame(hu4019_urb), "tables/hu4019_urb.csv")

















