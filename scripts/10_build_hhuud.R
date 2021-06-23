
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

## LOAD NEEDED PACKAGSE
packages(tidyverse)
packages(tidycensus)  # for getting state/county names/abbrevs.
packages(foreign)  # for dbf reading/writing
packages(sf) # for reading gdb
packages(data.table) # for setnames function
packages(imputeTS)  # for time series imputation (STEP 9)

## MAPPING
packages(mapview)
packages(leafsync)


##################################################################
##  STEP 1: LOAD & ORGANIZE MAX. REABSORPTION DATA              ##
##################################################################

## Import and Bind Maximum Reabsorption Data
dr_mr <- NULL  # create empty data frame (dasymetrically-refined maximum reabsorption (DR-MR))
for(i in seq(40, 80, 10)){
  
  # grab layer name
  lyr <- paste0("tab", i)  
  
  # read in tables layers from database1.gdb
  temp <- sf::st_read(
    dsn = "gis_files/database1.gdb",
    layer = lyr
  ) %>%
    as_tibble()
  
  # organize/add year column
  temp1 <- temp %>%
    mutate(yr = as.integer(paste0("19", i))) %>%
    select(GISJOIN10, yr, SUM) %>%
    rename(hu_mr = SUM) %>%
    arrange(GISJOIN10)
  
  # bind rows to save out as single data frame
  dr_mr <- bind_rows(dr_mr, temp1)
  
}

dr_mr

## Clean up
rm(list = setdiff(ls(), c("dr_mr", "packages")))


##############################################################################
##  STEP 2: LOAD & ORGANIZE INTERSECTION DATA (AW, TDW, TDW90)              ##
##############################################################################

## Grab/organize intersection shapefiles
df_import <- NULL
for(i in seq(40, 80, 10)){
  
  ## assign generic variables
  lyr <- paste0("int", i)
  hu <- paste0("hu", i)
  hu_1 <- paste0(hu, "_1")
  SUM_hu <- paste0("SUM_", hu)
  
  ## load in shapefile tables
  temp <- sf::st_read(
    dsn = "gis_files/database1.gdb",
    layer = lyr
  ) %>%
    as_tibble() %>%
    mutate(yr = as.integer(paste0("19", i))) # add year variable
  
  ## correct for 1980 difference (two intersection years instead of three)
  if(i == 80){
    
    temp1 <- temp %>%
      mutate(
        GISJOIN_1 = JOIN_T,
        hu80_1 = SUM_hu80,
        sqmi_1 = SUM_sqmi
        ) %>%
      select(GISJOIN, yr, hu, sqmi, GISJOIN_1, hu_1, sqmi_1, JOIN_T, SUM_hu, SUM_sqmi, sqmi_int)
    
  } else{
    
    temp1 <- temp %>%
      select(GISJOIN, yr, hu, sqmi, GISJOIN_1, hu_1, sqmi_1, JOIN_T, SUM_hu, SUM_sqmi, sqmi_int)
    
  }
  
  ## match column names (data.table package)
  setnames(temp1, c(hu, hu_1, SUM_hu, "SUM_sqmi"), c("hu", "hu_1", "hu_2", "sqmi_2"))
  
  ## write out as single data frame
  df_import <- bind_rows(df_import, temp1) %>%
    arrange(yr, GISJOIN)
  
}


df_import # inspect

## REMOVE MISTAKEN CENSUS ZEROES 
df_int  <- df_import %>%
  # remove source tracts w/ zeroes but pt & t tracts w/o zeroes
  filter(!(hu == 0 & (hu_1 > 0 | hu_2 > 0))) %>%
  print()

### Column name guides
# no suffix: data from source year (indicated in 'yr' column)
# '_1': data from pseudo target year (source year + 10)
# '_2': data from 1990 in target year geographies (2010 tracts)
# 'int': sq. mi. of atom

## Clean up
rm(list = setdiff(ls(), c("df_int", "dr_mr", "packages")))

#########################################################################################
##  STEP 3: ESTABLISH FLAG to ID DATA from 1960 CENSUS to REMOVE                       ##
#########################################################################################

## Make flag data frame
flag <- df_int %>%
  # create county and state indicators
  mutate(
    CO = str_sub(JOIN_T, 1, 8),
    ST = str_sub(JOIN_T, 1, 3)
  ) %>%
  # keep only suspicious tracts from 1960: < 100 HU & in one of the following areas
  filter(
    yr == 1960 & hu < 100 & ST %in% c("G25", "G34", "G44")| # MA, NJ, RI
    yr == 1960 & hu < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130")| # CA, WA, UT, MO, CT x 2
    yr == 1960 & GISJOIN %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223")| ## OR, TX x 2, PA
    yr == 1950 & hu_1 < 100 & ST %in% c("G25", "G34", "G41", "G44")|
    yr == 1950 & hu_1 < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130")|
    yr == 1950 & GISJOIN_1 %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223")
    ) %>%
  # group by GEOID and year
  group_by(JOIN_T, yr) %>%
  summarize() %>%
  print()

#flag %>%
#  group_by(yr) %>%
#  count

# 1950: only needs to be removed in TDW since that uses 1960 data
# 1960: should be removed from AW & TDW90

##############################################################################
##  STEP 4: GENERATE AREAL WEIGHTING  (AW)  ESTIMATES                       ##
##############################################################################

## AREAL WEIGHTING
temp_aw <- df_int %>%
  select(-c(GISJOIN_1:sqmi_1)) %>%
  group_by(GISJOIN, yr, hu, sqmi, JOIN_T, hu_2, sqmi_2) %>%
  summarize(sqmi_int = sum(sqmi_int)) %>%
  # calcuate overlaps
  mutate(
    pcover_t = sqmi_int / sqmi_2, ## % coverage for target tract (t)
    pcover_s = sqmi_int / sqmi, ## % coverage for source tract (s)
    hu_int = pcover_s * hu
  ) %>%
  # estimated hu in tract t
  group_by(JOIN_T, yr) %>%
  summarize(
    sqmi = median(sqmi_2),
    pcover_t = sum(pcover_t),
    pcover_s = sum(pcover_s),
    hu_aw = sum(hu_int)
  ) %>%
  arrange(yr, JOIN_T) %>%
  print()

## check NY special cases
#temp_aw %>%
#  filter(
#    str_detect(JOIN_T, "G360047|G360050|G360061|G360081") &
#      yr >= 1950 &
#      yr <= 1970 &
#      pcover_s > 0.97
#  ) %>%
#  arrange()

## Check histogram
#temp_aw %>%
#  filter(between(pcover_t, 0.9, 0.99)) %>%
#  ggplot(aes(x = pcover_t)) +
#  geom_histogram(binwidth = 0.01) +
#  scale_x_continuous(breaks = seq(0.9, 0.99, 0.01))

## Filter out tract-years with coverage < 0.97
dr_aw <- temp_aw %>%
  filter(
    (pcover_t > 0.97 & pcover_s > 0.97)|  # keep only tracts with > 97% target & source coverage
      # correct for NYC inconsistencies (disregard target zone coverage for years 1950-1970 --> sig. Census redrawing of shoreline tracts
      (str_detect(JOIN_T, "G360047|G360050|G360061|G360081") & yr >= 1950 & yr <= 1970 &pcover_s > 0.97)
    ) %>%
  # organize columns/remove pcover
  select(JOIN_T, yr, hu_aw) %>%
  # remove flagged tracts (1960: year of errors)
  filter(!(yr == 1960 & JOIN_T %in% flag$JOIN_T[flag$yr == 1960])) %>%
  rename(GISJOIN10 = JOIN_T)  # rename to match previous

dr_aw # n = 69,816


## Clean up
rm(list = setdiff(ls(), c("dr_mr", "dr_aw", "df_int", "flag", "packages")))

########################################################################################
## STEP 5: TARGET DENSITY WEIGHTING (TDW)-- 1 DECADE FORWARD                          ##
########################################################################################

## TDW: Calculate HU estimates in pseudo-target (pt) tracts (source [s] tracts + 10 years)
temp_tdw_pt <- df_int %>%
  # remove cases w/ HUs in s tracts and no HUs in corresponding pt tracts (could represent error)
  filter(!(hu >= 1 & hu_1 < 1)) %>%
  # group by s tract-year to get HU sums from pt tracts (see Schroeder 2007)
  group_by(GISJOIN, yr) %>%
  mutate(hu_sum_1 = sum(hu_1 * sqmi_int / sqmi_1)) %>%
  ungroup() %>%
  # calculate TDW HU estimates by atom
  mutate(hu_pt = ifelse(hu_sum_1 > 0, hu_1 * (sqmi_int / sqmi_1) / hu_sum_1 * hu, 0)) %>% 
  # sum TDW HU estimates by pt tract geographies
  group_by(GISJOIN_1, yr) %>%
  summarize(
    hu_pt = sum(hu_pt), # tdw HU estimate 
    pcover_s = sum(sqmi_int/sqmi),  # fraction overlay of s tracts on pt tracts
    pcover_pt = sum(sqmi_int/sqmi_1)  # fraction overlay of pt tracts (some are not fully covered)
    ) %>%
  filter(pcover_pt > 0.97 & pcover_s < 1)  # keep only pt tracts over 97% covered by s tracts and discard cases where tracts grew in size from s to pt

temp_tdw_pt

# Put TDW estimates in target tracts
dr_tdw <- df_int %>%
  # join intersection file to temp_tdw_pt
  left_join(temp_tdw_pt[1:3], by = c("GISJOIN_1", "yr")) %>%
  # remove cases w/o a TDW estimate
  drop_na(hu_pt) %>%
  # remove cases w/ HUs in pt tracts and no HUs in corresponding t tracts
  filter(!(hu_pt >= 1 & hu_2 < 1)) %>%
  # take fraction of hu_pt in atom
  mutate(hu_pt_t = hu_pt * sqmi_int / sqmi_1) %>%
  # group by target tract-years
  group_by(JOIN_T, yr) %>%
  summarize(
    hu_tdw = sum(hu_pt_t),
    pcover_pt = sum(sqmi_int/sqmi_1),
    pcover_t = sum(sqmi_int/sqmi_2)
  ) %>%
  # keep pcover_pt & pcover_t above 97% coverage (keeps only pt tracts that remained same size or grew)
  filter(pcover_pt > 0.97 & pcover_t > 0.97) %>%
  # remove flagged tracts
  filter(!(yr %in% c(1950, 1960) & JOIN_T %in% c(flag$JOIN_T))) %>%
  rename(GISJOIN10 = JOIN_T) %>%
  select(GISJOIN10, yr, hu_tdw)
  
dr_tdw  # n = 66,516


## Clean up
rm(list = setdiff(ls(), c("dr_mr", "dr_aw", "dr_tdw", "df_int", "flag", "packages")))


################################################################################################
###  STEP 6: TARGET DENSITY WEIGHTING (TDW90)-- HARMONIZED 1990 YSB DATA                      ##
################################################################################################

## Remove pseudo-target tracts from df_int --> keep only source and target tracts
temp_tdw <- df_int %>%
  # remove cases w/ HUs in s tracts and no HUs in corresponding t tracts
  filter(!(hu >= 1 & hu_2 < 1)) %>%
  # remove pt tracts by grouping polygons by s & t tracts only
  group_by(yr, GISJOIN, hu, sqmi, JOIN_T, hu_2, sqmi_2) %>%
  # sum atom area
  summarize(sqmi_int = sum(sqmi_int)) %>%
  # make names more intuitive (t = target; s = source)
  rename(
    sqmi_t = sqmi_2,
    hu_t = hu_2,
    sqmi_s = sqmi,
    hu_s = hu
  ) %>%
  print()

## Conduct TDW90 calculations
temp_tdw1 <- temp_tdw %>%
  # calculate area overlaps + target HU estimates for atoms
  mutate(
    pcover_t = sqmi_int / sqmi_t,
    pcover_s = sqmi_int / sqmi_s,
    hu_int = hu_t * pcover_t
  ) %>%
  # calc. 1990 YSB HU sums in source tract geographies
  group_by(GISJOIN, yr) %>%
  mutate(hu_int_sum = sum(hu_int)) %>%
  ungroup() %>%
  # calculate HUs in atoms
  mutate(hu_atom = hu_int / hu_int_sum * hu_s) %>%  # hu_t * sqmi_int / sqmi_t / hu_int_sum * hu_s
  # put in target tract geographies
  group_by(JOIN_T, yr) %>%
  summarize(
    pcover_t = sum(pcover_t),  # target tract coverage
    pcover_s = sum(pcover_s),  # source tract coverage
    hu_tdw90 = sum(hu_atom)  # sum HU counts in atoms
  ) %>%
  arrange(yr, JOIN_T) %>%
  print()

## Histogram  (could go with 98%; but will stick with 97% for consistency)
#temp_tdw1 %>%
#  filter(between(pcover_t, 0.85, 0.999)) %>%
#  ggplot(aes(x = pcover_t)) +
#  geom_histogram(binwidth = 0.01)

## Fix special cases and generate final output
dr_tdw90 <- temp_tdw1 %>%
  # removetracts that went large to small & those from target that were incompletely covered (< 97%)
  filter(pcover_t > 0.97 & pcover_s <= 1) %>%
  # remove special cases (flagged cases from 1960)
  filter(!(yr == 1960 & JOIN_T %in% flag$JOIN_T[flag$yr == 1960])) %>%
  # fix up names and columns
  rename(GISJOIN10 = JOIN_T) %>%
  select(GISJOIN10, yr, hu_tdw90) %>%
  ungroup()

dr_tdw90  # n = 132,342
  
## clean up
rm(list = setdiff(ls(), c("dr_aw", "dr_mr", "dr_tdw", "dr_tdw90", "df_int", "flag", "packages")))


##################################################################################################
###  STEP 7: PROPORTIONALLY ALLOCATE MAX. REABSORP. ESTIMATES USING COUNTY HU COUNTS            ##
##################################################################################################

## LOAD in COUNTY DATA
counties <- read_csv("tables/county_tracts.csv") %>%
  mutate(
    CO_CALC = JOIN_CO,  # includes aggregated counties (border changes)
    CO_JOIN = substr(GISJOIN, 1, 8)  # for joining
  ) %>%
  select(STATE, COUNTY, GISJOIN, CO_CALC, CO_JOIN, hu40co:hu80co) %>%
  print()

## MAKE LONG
counties_long <- NULL
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
  
  
  counties_long <- bind_rows(counties_long, temp) %>%
    unique() %>%
    arrange(CO_CALC, yr)
}

counties_long


## CORRECT MR ESTIMATES w/ PROPORTIONAL ALLOCATION
dr_cmr <- dr_mr %>%
  mutate(CO_JOIN = substr(GISJOIN10, 1, 8)) %>%
  # join with counties_long
  left_join(counties_long, by = c("CO_JOIN", "yr")) %>%
  # reorganize columns
  select(STATE, COUNTY, CO_CALC, CO_JOIN, GISJOIN10, yr, hu_co, hu_mr) %>%
  # calculate prop. alloc. on MR estimates
  group_by(CO_CALC, yr) %>%
  mutate(hu_sum = sum(hu_mr)) %>%
  ungroup() %>%
  # do prop. allocation by dividing mr by county HU sum and multiplying by county HU total
  mutate(hu_cmr = hu_mr / hu_sum * hu_co) %>%
  select(-CO_JOIN, -hu_sum) %>%
  print()


## Clean up
rm(counties, counties_long)


##################################################################################################
###  STEP 8: JOIN ESTIMATES TOGETHER                                                            ##
##################################################################################################

## JOIN ALL
hu40_80 <- dr_cmr %>%
  left_join(dr_aw, by = c("GISJOIN10", "yr")) %>%
  left_join(dr_tdw, by = c("GISJOIN10", "yr")) %>%
  left_join(dr_tdw90, by = c("GISJOIN10", "yr")) %>%
  # add method. Hierarchy: AW > TDW-1 > TDW-90 > MR
  mutate(
    method = case_when(
      !is.na(hu_aw) ~ "AW",
      is.na(hu_aw) & !is.na(hu_tdw) ~ "TDW-1",
      is.na(hu_aw) & is.na(hu_tdw) & !is.na(hu_tdw90) ~ "TDW-90",
      TRUE ~ "CMR"
      ) 
    ) %>%
  # add estimate
  mutate(
    hu_est = case_when(
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90,
      method == "CMR" ~ hu_cmr
    )
  ) %>%
  arrange(GISJOIN10, yr) %>%
  print()

## CHECK DISTRIBUTION of METHODS
hu40_80 %>%
  group_by(method) %>%
  count()

## SAVE OUT
write_csv(hu40_80, "tables/hu40_80.csv")


##################################################################################################
###  STEP 9: ADJUST MR ESTIMATES BASED on VECTOR VALUES                                         ##
##################################################################################################

## read in 1990 data
hu9019_import <- read.dbf("tables/hu9019.dbf") %>%
  as_tibble()

## make long
hu90_19 <- hu9019_import %>%
  rename(
    `1990` = hu90,
    `2000` = hu00,
    `2010` = hu10,
    `2019` = hu19
  ) %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "yr",
    values_to = "hu_est"
  ) %>%
  left_join(hu40_80[c(1, 2, 4)], by = "GISJOIN10") %>%
  mutate(
    yr = as.integer(yr),
    method = "NHGIS"
  ) %>%
  select(-GISJOIN19) %>%
  distinct() %>%
  print()

hu4019_prep <- bind_rows(hu40_80, hu90_19) %>%
  arrange(GISJOIN10, yr) %>%
  print()


## Impute missing
df_impute <- hu4019_prep %>%
  group_by(CO_CALC, yr) %>%
  mutate(
    n_tracts = n(),  # total tracts in a county-year
    p_mr = sum(str_detect(method, "MR")) / n_tracts,  # % of tracts in a county-year that are MR tracts
    vect_sum = sum(hu_est[method != "CMR"]),  # sum of vector-calc. HU in county
    hu_co_minus = hu_co - vect_sum,
    hu_tmr = hu_mr / sum(hu_mr) * hu_co_minus  # tmr = "tract adjusted max. reabsorption
  ) %>%
  ungroup() %>%
  mutate(
    hu_est2 = ifelse(method == "CMR", hu_tmr, hu_est),
    hu_est2 = ifelse(hu_est2 < 0, hu_cmr, hu_est2),  # change any negatives to Modified MR
    hu_diff = abs(hu_est2 - (hu_est2 + hu_est)/2)/((hu_est2 + hu_est)/2) * 100,
    mr_indicator = ifelse(hu_diff > 0, 1, 0),
    hu_est_ts = ifelse(mr_indicator == 1, NA, hu_est2)) %>%
  group_by(GISJOIN10) %>%
  mutate(mr_impute = sum(mr_indicator)) %>%
  filter(mr_impute > 0) %>%
  ungroup() %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 176,661
  



## Conduct TS imputation of HU estimates in CMR tracts where other tracts in their county-years have vector estimates
df_impute1 <- df_impute %>%
  group_by(GISJOIN10) %>%
  mutate(
    #hu_arima = na_kalman(hu_est_ts, model = "auto.arima", smooth = TRUE),
    #hu_struct = na_kalman(hu_est_ts, model = "StructTS"),
    hu_line = imputeTS::na_interpolation(hu_est_ts, option = "linear"),
    #hu_spline = na_interpolation(hu_est_ts, option = "spline"),
    #hu_stine = na_interpolation(hu_est_ts, option = "stine")
    ) %>%
  select(GISJOIN10, yr, mr_indicator, hu_cmr, hu_tmr, hu_line) %>%
  ungroup() %>%
  print()

## 
df_impute2 <- df_impute1 %>%
  # keep only eligible
  filter(mr_indicator == 1) %>%
  mutate(
    hu_diff_cmr = abs(hu_cmr - (hu_cmr + hu_line)/2), # calc. diff. bw CMR est. & linear interp. est.
    hu_diff_tmr = abs(hu_tmr - (hu_tmr + hu_line)/2),  # calc. diff. bw TMR est. & linear interp. est.
    hu_est3 = ifelse(hu_diff_cmr < hu_diff_tmr, hu_cmr, hu_tmr),
    method2 = ifelse(hu_diff_cmr < hu_diff_tmr, "CMR", "TMR")
  ) %>%
  select(GISJOIN10, yr, hu_est3, method2) %>%
  print()  # n = 27,812

df_impute2 %>% group_by(method2) %>%
  count()

## JOIN df_impute2 back with hu4019_prep
hu40_19 <- hu4019_prep %>%
  left_join(df_impute2, by = c("GISJOIN10", "yr")) %>%
  mutate(
    method = ifelse(is.na(method2), method, method2),
    hu_est = ifelse(is.na(hu_est3), hu_est, hu_est3)
  ) %>%
  select(STATE, COUNTY, GISJOIN10, yr, method, hu_est) %>%
  print()

write_csv(hu40_19, "tables/hu40_19.csv")
#write.dbf(as.data.frame(hu40_19), "tables/hu40_19.dbf")
#hu40_19 <- read_csv("tables/hu40_19.csv")

## MAKE WIDE
hu4019_wide <- hu40_19 %>% 
  mutate(yr = str_sub(yr, 3, 4)) %>% 
  select(-STATE, -COUNTY, -method) %>% 
  # group by and summarize to solve problem with Boulder Co., CO
  group_by(GISJOIN10, yr) %>%
  summarize(hu_est = median(hu_est)) %>%
  distinct() %>%
  pivot_wider(
    names_from = yr, 
    names_prefix = "hu", 
    names_sep = "",
    values_from = hu_est,
    values_fill = 0
    ) %>% 
  print()

write.dbf(as.data.frame(hu4019_wide), "tables/hu4019_wide.dbf")


