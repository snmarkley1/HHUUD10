
#########################################################################
#########################################################################
###                                                                   ###
###           BUILD the HIST. HU. & URB. DATABASE (1st CUT)           ###
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
packages(tidycensus)  # for getting state/county names/abbrevs.
packages(foreign)  # for dbf reading/writing
packages(sf) # for reading gdb
packages(data.table) # for setnames function
packages(imputeTS)  # for time series imputation


###############################################################################
##  STEP 1: LOAD & ORGANIZE MAX. REABSORPTION DATA                           ##
###############################################################################

## Import and Bind Maximum Reabsorption Data
mr_import <- NULL  # create empty data frame (dasymetrically-refined maximum reabsorption (DR-MR))
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
    select(GISJOIN, yr, SUM) %>%
    rename(hu_mr = SUM) %>%
    arrange(GISJOIN)
  
  # bind rows to save out as single data frame
  mr_import <- bind_rows(mr_import, temp1)
  
}

# inspect
mr_import  # n = 360,993

###############################################################################
##  STEP 2: IMPORT/CLEAN SPARSE & DROP DATA                                  ##
###############################################################################

##-------------------------------------------------------
## Import and Clean Sparse Data (includes zeroes)
##-------------------------------------------------------

## Import sparse data (USO)
uso_import <- sf::st_read(  # USO: Uncorrected Sparse override
  dsn = "gis_files/database1.gdb",
  layer = "t10_zero"
) %>%
  as_tibble()

## Clean USO data
uso_clean <- uso_import %>%
  select(GISJOIN:hu80) %>%
  # make long
  pivot_longer(
    cols = c(hu40:hu80),
    names_to = "yr",
    values_to = "hu"
  ) %>%
    mutate(
    yr = as.integer(paste0("19", str_extract(yr, "\\d+"))),  # rename yr to match formats
    method = "USO"  # add name of method: CSO --> Uncorrected Sparse override
    ) %>%
  arrange(GISJOIN, yr) %>%
  print()  # n = 12,230


###############################################################################
##  STEP 3: COMBINE overridE ESTIMATES & CALCULATE                          ##
###############################################################################

##-----------------------------------------------
## Combine override Estimates
##-----------------------------------------------

## ADD USO & UDO to SAME DF
if(exists("t90_drop_long")){  # for first iteration of code, this object does not exist
  
  # for later iterations where object "drop" is already established
  df_uso <- bind_rows(uso_clean, t90_drop_long) %>%
    # remove UDO cases for 1990
    filter(yr < 1990) %>%
    # remove duplicates
    group_by_at(vars(-method)) %>%
    filter(n() < 2 | method == "USO") %>%
    #Name hu_uso (includes udo as well)
    rename(hu_uso = hu) %>%
    print()  # n = 13,230
  
} else {
  
  # for first iteration
  df_uso <- uso_clean %>%
    rename(hu_uso = hu) %>%
    print()
  
}

##----------------------------------------------
## LOAD YSB 1990 for HAMMER METHOD
##----------------------------------------------
ysb90 <- read.dbf("tables/ysb90_t.dbf") %>% 
  as_tibble() %>% 
  # Make Long
  pivot_longer(
    cols = hu40:hu90,
    names_to = "yr",
    values_to = "hu_ysb"
  ) %>%
  mutate(yr = as.integer(paste0("19", str_extract(yr, "\\d+")))) %>%
  filter(yr != 1990) %>%
  print()


##-------------------------------------------------------------
## Combine MR & USO HU estimates in single data frame
##-------------------------------------------------------------

## Join & Organize
#mr_join <- mr_import %>%
  # do full_join to grab missing tracts (fully erased from dasymetric refinement)
#  full_join(df_uso, by = c("GISJOIN", "yr")) %>%
  # full join w/ ysb_t
#  full_join(ysb_t, by = c("GISJOIN", "yr")) %>%
#  # create MR with values overridten by USO
#  mutate(
#    #hu_mr2 = ifelse(is.na(hu_uso), hu_mr, hu_uso),  # override UMR estimates w/ USO & USD ones
#    method = ifelse(is.na(method), "UMR", method)   # UMR: Uncorrected Max. Reabsorp.
#  ) %>%
#  print()  # n = 361,340

## Join & Organize
mr_join <- mr_import %>% 
  # Add 1990 YSB Data
  full_join(ysb90, by = c("GISJOIN", "yr")) %>%
  # Add USO data (remove hu count [3] --> same as ysb)
  left_join(df_uso[-3], by = c("GISJOIN", "yr")) %>%
  # Adjust methods
  mutate(method = ifelse(is.na(method), "CMR", method)) %>%
  # Correct some NAs
  mutate(
    hu_mr = ifelse(is.na(hu_mr) & is.na(hu_ysb), 0, hu_mr),   # when MR & YSB are NA --> 0
    hu_ysb = ifelse(is.na(hu_mr) & is.na(hu_ysb), 0, hu_ysb),  # when MR & YSB are NA --> 0
    hu_mr = ifelse(is.na(hu_mr) & !is.na(hu_ysb), hu_ysb, hu_mr),  # when MR is NA & YSB is not NA --> YSB
    hu_ysb = ifelse(is.na(hu_ysb) & hu_mr == 0, 0, hu_ysb),
    hu_ysb = ifelse(is.na(hu_ysb) & method %in% c("USO", "UDO"), 0, hu_ysb),
    hu_mr = ifelse(method %in% c("USO", "UDO"), hu_ysb, hu_mr)
  ) %>%
  print()


##################################################################################################
###  STEP 4: PROPORTIONALLY ALLOCATE MAX. REABSORP. ESTIMATES USING COUNTY HU COUNTS            ##
##################################################################################################

##----------------------------------------------
## LOAD in & ORGANIZE COUNTY DATA
##----------------------------------------------

county_tracts <- read_csv("tables/county_tracts.csv") %>%
  mutate(
    CO_CALC = JOIN_CO,  # includes aggregated counties (border changes)
    #CO_JOIN = substr(GISJOIN, 1, 8)  # for joining
  ) %>%
  select(STATE, COUNTY, GISJOIN, CO_CALC, hu40co:hu80co) %>%
  print()


## MAKE LONG
counties_long <- county_tracts %>%
  # make long
  pivot_longer(
    cols = hu40co:hu80co,
    names_to = "yr",
    values_to = "hu_co"
  ) %>%
  # fix formating for "yr" column
  mutate(yr = as.integer(paste0("19", str_extract(yr, "\\d+")))) %>%
  print()


##-------------------------------------------------------------------
## CORRECT MR ESTIMATES w/ PROPORTIONAL ALLOCATION
##-------------------------------------------------------------------

mr_join2 <- mr_join %>%
  left_join(counties_long, by = c("GISJOIN", "yr")) %>%
  select(STATE, COUNTY, CO_CALC, GISJOIN, yr, hu_co, hu_mr, hu_ysb, method) %>%
  print()

#################
## PROPORTIONAL ALLOCATION
#################

## Correct NAs for hu_ysb & then do prop. alloc.
mr_join3 <- mr_join2 %>%
  group_by(CO_CALC, yr) %>%
  mutate(mr_sum = sum(hu_mr)) %>%
  ungroup() %>%
  mutate(
    hu_mr2 = hu_mr/mr_sum*hu_co,
    hu_ysb = ifelse(is.na(hu_ysb), hu_mr2, hu_ysb),
    na_ysb = ifelse(is.na(hu_ysb), 1, 0)
    ) %>%
  # Now do YSB
  group_by(CO_CALC, yr) %>%
  mutate(ysb_sum = sum(hu_ysb)) %>%
  ungroup() %>%
  mutate(
    hu_ysb2 = hu_ysb/ysb_sum*hu_co,
    hu_ysb = ifelse(na_ysb == 1, NA, hu_ysb)  # revert back to original NA data
    ) %>%
  select(STATE:hu_ysb, hu_mr2, hu_ysb2, method) %>%
  arrange(GISJOIN, yr) %>%
  print()

dr_cmr <- mr_join3

##-------------------------------------------------------------
## Import, Clean, and Join Neighbor File
##-------------------------------------------------------------

## IMPORT NEIGHBORS
#nbr_import <- read_csv("tables/neighbors.csv") %>%
#  print()


## CLEAN & ADJUST NEIGHBOR WEIGHTS to USO/UDO TRACTS
#nbr <- nbr_import %>%
#  # Join Max. Reab. & USO/UDO Tracts to Neighbors file
#  left_join(mr_join3, by = c("src_GISJOIN" = "GISJOIN")) %>%
#  mutate(
#    mr_uso_diff = ifelse(method %in% c("USO", "UDO") & !is.na(hu_ysb), hu_mr2 - hu_ysb2, NA),  # get difference bw hu_ysb and hu_mr for USO/UDO neighbor tracts
#    hu_adj = WEIGHT * mr_uso_diff  # Multiply by weight to get HU adjustment factor
#  ) %>%
#  # Sum hu_add data to neighbor tracts
#  group_by(nbr_GISJOIN, yr) %>%
#  summarize(hu_adj = sum(hu_adj, na.rm = TRUE))  %>%
#  # Fix hu_adj in NA cases
#  mutate(hu_adj = ifelse(is.na(hu_adj), 0, hu_adj)) %>%
#  # Clean up
#  arrange(nbr_GISJOIN, yr) %>%
#  # Change "nbr_GISJOIN" to "GISJOIN"
#  rename(GISJOIN = 1) %>%
#  print()


#dr_cmr <- dr_mr %>%
  # join with counties_long
#  full_join(counties_long, by = c("GISJOIN", "yr")) %>%
  # reorganize columns
#  select(STATE, COUNTY, CO_CALC, GISJOIN, yr, hu_co, hu_uso, hu_mr, hu_mr2, method) %>%
#  # calculate sums on MR & MR2 estimates for proportional allocation
#  group_by(CO_CALC, yr) %>%
#  mutate(
#    #hu_sum = sum(hu_mr, na.rm = TRUE),
#    hu_sum2 = sum(hu_mr2, na.rm = TRUE)
#    ) %>%
#  ungroup() %>%
#  # do prop. alloc. by dividing MR/MR2 by county HU sum/HU sum2 and multiplying by county HU total
#  mutate(
#    #hu_cmr = hu_mr / hu_sum * hu_co,  # Corrected Max. Reabsorp.
#    hu_cmr2 = hu_mr2 / hu_sum2 * hu_co,  # Corrected Max. Reabsorp. 2 --> does USO override
#    flag = ifelse(!is.na(hu_uso) & hu_cmr2 <= hu_uso, 1, 0),  # ID flag to correct for cases where USO/UDO > Corrected MR (shouldn't be possible)
#    ) %>%
#  # correct all MR estimates in counties where some tracts have USOs >= CMR2s or hu_cmr2 are > double hu_uso 
#  group_by(CO_CALC, yr) %>%
#  mutate(
#    flag_sum = sum(flag),  # count up special cases by county
#    co_flag = ifelse(flag_sum > 0, 1, 0),  # ID counties w/ some tracts w/ USO > CMR2
#    hu_remove = ifelse(co_flag == 1, sum(hu_uso, na.rm = TRUE), 0),  # HU count to remove from total during corrected prop. alloc.
#    hu_sum3 = sum(hu_mr2, na.rm = TRUE) - hu_remove  # adjust hu_sum3
#  ) %>%
#  ungroup() %>%
#  # Generate final corrections to special cases
#  mutate(
#    hu_co2 = hu_co - hu_remove,  # county HU minus special cases
#    hu_cmr3 = 
#      case_when(  # calc. new CMR
#        flag == 1 ~ hu_uso,  # replace special tracts w/ original USO value
#        flag == 0 & co_flag == 1 ~ hu_cmr2 / hu_sum3 * hu_co2,  # redistribute HU estimates in those special tracts' counties
#        TRUE ~ hu_cmr2  # keep CMR2 estimates for vast majority of tracts in counties w/ no special cases
#        )
#    ) %>% 
#  # ID different methods
#  mutate(
#    method =
#      case_when(
#        flag == 1 ~ method,  # Keep either UDO or USO for special cases
#        flag == 0 & !is.na(hu_uso) & method == "UDO" ~ "CDO",  # Corrected Drop override
#        flag == 0 & !is.na(hu_uso) & method == "USO" ~ "CSO",  # Corrected Sparse override
#        TRUE ~ "OTHER"  # Everything Else
#      )
#    ) %>%
#  # keep only selected columns
#  select(STATE:CO_CALC, GISJOIN:hu_mr, hu_cmr3, method) %>%
#  # rename to CMR2
#  rename(hu_cmr2 = hu_cmr3) %>%
#  print()  # n = 361,340



## Join neighbor adjustments to MR estimates file
#dr_mr <- mr_join %>%
#  left_join(nbr, by = c("GISJOIN", "yr")) %>%
#  # Adjust hu_mr according to USO/UDO status
#  mutate(
#    hu_mr = 
#      ifelse(
#        method %in% c("USO", "UDO") & !str_detect(GISJOIN, "G370143"),  # Keep USO/UDO estimates in hu_mr2 & Perquimans County, NC (strange case)
#        hu_ysb,  
#        hu_mr + hu_adj  # Add HU adjustment factor to MR estimates 
#      ) 
#  ) %>%
#  select(-hu_adj) %>%
#  arrange(GISJOIN, yr) %>%
#  print()

## Clean up
#rm(list = setdiff(ls(), c("dr_mr", "packages")))


# Look at summary breakdown
dr_cmr %>%
  group_by(method) %>%
  count()


## Clean up
#rm(county_tracts, counties_long)

rm(mr_import, mr_join, mr_join2, mr_join3, uso_import, temp, temp1, counties_long, county_tracts)


##############################################################################
##  STEP 5: LOAD & ORGANIZE INTERSECTION DATA (AW, TDW, TDW90)              ##
##############################################################################

##---------------------------------------------------------
## GRAB / ORGANIZE INTERSECTION SHAPEFILES
##---------------------------------------------------------

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

# Inspect
df_import 


##--------------------------------------------------------------------------------------
## REMOVE MISTAKEN CENSUS ZEROES 
##--------------------------------------------------------------------------------------

df_int  <- df_import %>%
  # HU NAs are zeroes
  mutate(hu = ifelse(is.na(hu), 0, hu)) %>%
  # remove source tracts w/ zeroes but pt & t tracts w/o zeroes
  filter(!(hu == 0 & (hu_1 > 0 | hu_2 > 0))) %>%
  print()  # n = 434,152 atoms

### Column name guides
# no suffix: data from source year (indicated in 'yr' column)
# '_1': data from pseudo target year (source year + 10)
# '_2': data from 1990 in target year geographies (2010 tracts)
# 'int': sq. mi. of atom

## Clean up
rm(list = setdiff(ls(), c("df_int", "dr_mr", "dr_cmr", "packages")))


#########################################################################################
##  STEP 6: ESTABLISH FLAG to ID DATA from 1960 CENSUS to REMOVE                       ##
#########################################################################################

## Make flag data frame
flag <- df_int %>%
  # create county and state indicators
  mutate(
    CO = str_sub(JOIN_T, 1, 8),
    ST = str_sub(JOIN_T, 1, 3)
  ) %>%
  # keep only suspicious tracts from 1960: < 100 HU & in one of the following areas (ID'd from manual inspection)
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

# 1950: only needs to be removed in TDW since that uses 1960 data
# 1960: should be removed from AW & TDW90


##############################################################################
##  STEP 7: GENERATE AREAL WEIGHTING  (AW)  ESTIMATES                       ##
##############################################################################

##---------------------------------------------------------
## AREAL WEIGHTING
##---------------------------------------------------------

temp_aw <- df_int %>%
  # clean up columns
  select(-c(GISJOIN_1:sqmi_1)) %>%
  # group to allow calculation of overlapping areas
  group_by(GISJOIN, yr, hu, sqmi, JOIN_T, hu_2, sqmi_2) %>%
  summarize(sqmi_int = sum(sqmi_int)) %>%
  ungroup() %>%
  # calcuate overlaps
  mutate(
    pcover_t = sqmi_int / sqmi_2, # % coverage for target tract (t)
    pcover_s = sqmi_int / sqmi,  # % coverage for source tract (s)
    hu_int = pcover_s * hu  # calc. intersection
  ) %>%
  # estimated hu in tract t (target zone)
  group_by(JOIN_T, yr) %>%
  summarize(
    sqmi = median(sqmi_2),  # get sqmi
    pcover_t = sum(pcover_t),  # sum % coverage for target tracts
    pcover_s = sum(pcover_s),  # sum % converage for source tracts
    hu_aw = sum(hu_int)  # sum HUs in atom
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

##------------------------------------------------------------
## FILTER OUT TRACT-YEARS W/ COVERAGE < 0.97
##------------------------------------------------------------

dr_aw <- temp_aw %>%
  filter(
    (pcover_t > 0.97 & pcover_s > 0.97)|  # keep only tracts with > 97% target & source coverage
      # correct for NYC inconsistencies (disregard target zone coverage for years 1950-1970 --> sig. Census redrawing of shoreline tracts
      (str_detect(JOIN_T, "G360047|G360050|G360061|G360081") & between(yr, 1950, 1970) & pcover_s > 0.97)
  ) %>%
  # organize columns/remove pcover
  select(JOIN_T, yr, hu_aw) %>%
  # remove flagged tracts (1960: year of errors)
  filter(!(yr == 1960 & JOIN_T %in% flag$JOIN_T[flag$yr == 1960])) %>%
  # rename to match previous
  rename(GISJOIN10 = JOIN_T)  %>%
  print()  # n = 70,370
  

## Clean up
rm(temp_aw)


########################################################################################
## STEP 8: TARGET DENSITY WEIGHTING (TDW)-- 1 DECADE FORWARD                          ##
########################################################################################

##----------------------------------------------------------------------------------------------------
## TDW: CALC. HU ESTIMATES in PSEUDO-TARGET (pt) TRACTS (source [s] tracts + 10 years)
##----------------------------------------------------------------------------------------------------

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
  # keep only pt tracts over 97% covered by s tracts and discard cases where tracts grew in size from s to pt
  filter(
    pcover_pt > 0.97 & pcover_s < 1|
      pcover_s < 1 & pcover_pt > 0.5 & str_detect(GISJOIN_1, "G360047|G360059|G360081|G360103")  # grab strange cases of NY
      )  %>%
  print()  # n = 88,791


##---------------------------------------------------------
## PUT TDW ESTIMATES in TARGET TRACTS
##---------------------------------------------------------

dr_tdw <- df_int %>%
  # join intersection file to temp_tdw_pt
  left_join(temp_tdw_pt[1:3], by = c("GISJOIN_1", "yr")) %>%
  # remove cases w/o a TDW estimate
  drop_na(hu_pt) %>%
  # remove cases w/ HUs in pt tracts and no HUs in corresponding t tracts
  #filter(!(hu_pt >= 1 & hu_2 < 1)) %>%
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
 #select(GISJOIN10, yr, hu_tdw) %>%
  print() # n = 66,504


## Clean up
rm(temp_tdw_pt)


################################################################################################
###  STEP 9: TARGET DENSITY WEIGHTING (TDW90)-- HARMONIZED 1990 YSB DATA                      ##
################################################################################################

##-------------------------------------------------------------------------------------------
## REMOVE PSEUDO-TARGET TRACTS from DF_INT --> KEEP ONLY SOURCE and TARGET TRACTS
##-------------------------------------------------------------------------------------------

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

##-------------------------------------------------------------------
## CONDUCT TDW90 CALCULATIONS
##-------------------------------------------------------------------

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

## Histogram
#temp_tdw1 %>%
#  filter(between(pcover_t, 0.85, 0.999)) %>%
#  ggplot(aes(x = pcover_t)) +
#  geom_histogram(binwidth = 0.01)


##----------------------------------------------------------------
## FIX SPECIAL CASES and GENERATE FINAL OUTPUT
##----------------------------------------------------------------

dr_tdw90 <- temp_tdw1 %>%
  # removetracts that went large to small & those from target that were incompletely covered (< 97%)
  filter(
    pcover_t > 0.97 & pcover_s <= 1|
      (pcover_s <= 1 & pcover_t > 0.5 & str_detect(JOIN_T, "G360047|G360059|G360081|G360103"))  # grab strange cases of NY
      ) %>%
  # remove special cases (flagged cases from 1960)
  filter(!(yr == 1960 & JOIN_T %in% flag$JOIN_T[flag$yr == 1960])) %>%
  # fix up names and columns
  rename(GISJOIN10 = JOIN_T) %>%
  select(GISJOIN10, yr, hu_tdw90) %>%
  ungroup() %>%
  print()  # n = 132,105
  
## clean up
rm(temp_tdw, temp_tdw1)


##################################################################################################
##################################################################################################
###  STEP 10: JOIN ESTIMATES TOGETHER                                                           ##
##################################################################################################
##################################################################################################

## JOIN ALL
hu4080 <- dr_cmr %>%  # MAX. REABS. & SPARSE
  # AREAL WEIGHTING
  left_join(dr_aw, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # TARGET-DENSITY WEIGHTING - SINGLE DECADE
  left_join(dr_tdw, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # TARGET-DENSITY WEIGHTING - 1990
  left_join(dr_tdw90, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # change col names
  rename(
    hu_cmr = hu_mr2,
    hu_ham = hu_ysb2
    ) %>%
  # clean up
  select(-pcover_pt, -pcover_t) %>%
  # Name methods
  mutate(
    method = case_when(
      method == "CMR" & !is.na(hu_aw) ~ "AW",
      method == "CMR" & is.na(hu_aw) & !is.na(hu_tdw) ~ "TDW-1",
      method == "CMR" & is.na(hu_aw) & is.na(hu_tdw) & !is.na(hu_tdw90) ~ "TDW-90",
      TRUE ~ method
    )
  ) %>%
  # Choose HU estimation method
  mutate(
    hu_est_cmr = case_when(
      method %in% c("USO", "UDO") ~ hu_cmr,  # already corrected to include MR + Corrected Sparse override
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90,
      method == "CMR" ~ hu_cmr  # includes Sparse override
      )
  ) %>%
  # Same but for Hammer
  mutate(
    hu_est_ham = case_when(
      method %in% c("USO", "UDO") ~ hu_ham,  # already corrected to include MR + Corrected Sparse override
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90,
      method == "CMR" ~ hu_ham  # includes Sparse override
    )
  ) %>%
  mutate(
    method = ifelse(method == "USO", "CSO", method),
    method = ifelse(method == "UDO", "CDO", method)
    ) %>%
  rename(GISJOIN10 = GISJOIN) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 362,785


## CHECK DISTRIBUTION of METHODS
hu4080 %>%
  group_by(method) %>%
  count() %>%
  arrange(-n)


## SAVE OUT
#write_csv(hu4080, "output/hu4080.csv")

## CLEAN UP
#rm(list = setdiff(ls(), "hu4080"))


##################################################################################################
###  STEP 11: ADJUST MR ESTIMATES BASED on VECTOR VALUES                                        ##
##################################################################################################

##----------------------------------------------
## READ in 1990 DATA & MAKE LONG
##----------------------------------------------

## Read in and inspect
hu9019_import <- read.dbf("tables/hu9019.dbf") %>%
  as_tibble() %>%
  print()


## Make Long
hu9019 <- hu9019_import %>%
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
  left_join(hu4080[c(1, 2, 4)], by = "GISJOIN10") %>%
  mutate(
    yr = as.integer(yr),
    method = "NHGIS"
  ) %>%
  select(-GISJOIN19) %>%
  distinct() %>%
  print()  # n = 290,156


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
## COMBINE 1990-2019 DATA w/ 1940-80 DATA & FILL in MISSING VALUES
##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------

hu4019_prep <- bind_rows(hu4080, hu9019) %>%
  # complete dataset (add missing tract-years)
  complete(yr, nesting(GISJOIN10)) %>%
  mutate(
    hu_est_cmr = ifelse(is.na(hu_est_cmr), hu_est, hu_est_cmr),
    hu_est_ham = ifelse(is.na(hu_est_ham), hu_est, hu_est_ham),
    hu_est = ifelse(is.na(hu_est), hu_est_cmr, hu_est)
  ) %>%
  #select(-hu_est) %>% 
  arrange(GISJOIN10, yr) %>%
  print()  # n = 653,013


##-----------------------------------------------------------------
## IMPUTE MISSING VALUES
##-----------------------------------------------------------------

## Prepare TS imputation
ts_prep <- hu4019_prep %>%
  group_by(CO_CALC, yr) %>%
  mutate(
    n_tracts = n(),  # total tracts in a county-year
    p_mr = sum(str_detect(method, "CMR|CSO|CDO")) / n_tracts,  # % of tracts in a county-year that are MR, SO, or DO tracts
    vect_sum = sum(hu_est[!method %in% c("CMR", "CSO", "CDO")]),  # sum of vector-calc. HU in county
    vect_sum_ham = sum(hu_est_ham[!method %in% c("CMR", "CSO", "CDO")]),  # sum of vector-calc. HU in county
    hu_co_minus = hu_co - vect_sum,
    hu_co_minus_ham = hu_co - vect_sum_ham,
    hu_tmr = hu_cmr / sum(hu_cmr) * hu_co_minus,  # tmr = "tract adjusted max. reabsorption
    hu_tham = hu_ham / sum(hu_ham) * hu_co_minus_ham,
    hu_tmr = ifelse(hu_tmr < 0, 0, hu_tmr),
    hu_tham = ifelse(hu_tham < 0, 0, hu_tham)
  ) %>%
  ungroup() %>%
  mutate(
    hu_est2 = ifelse(method %in% c("CMR", "CSO", "CDO") & hu_tmr >= 0 & !is.na(hu_tmr), hu_tmr, hu_est),  # select tract-imputed values for CMR, CSO, & CDO tracts & remove TMRs that come back negative
    hu_est2_ham = ifelse(method %in% c("CMR", "CSO", "CDO") & hu_tham >= 0 & !is.na(hu_tham), hu_tham, hu_est_ham),
    #hu_est2 = ifelse(hu_tmr < 0, hu_est, hu_est2),  # change any negatives to Modified MR
    #hu_diff = abs((hu_est2) - (hu_est2 + hu_est)/2)/((hu_est2 + hu_est)/2) * 100,  # get HU median difference bw hu_est2 & hu_est
    ts_ind = ifelse(p_mr > 0 & p_mr < 1, 1, 0),  # indicate tracts in counties with at least 1 tract-year containing a CMR, CSO, or CDO tract (as long as not all in county are CMR or CSO)
    hu_ts = ifelse(ts_ind == 1 & method %in% c("CMR", "CSO", "CDO") & hu_tmr >= 0 & !is.na(hu_tmr), NA, hu_est2),  # time series estimator --> do only for CMR,CSO,CDO tracts in county-decade w/ < 100% CMR,CSO,CDO tracts
    hu_ts_ham = ifelse(ts_ind == 1 & method %in% c("CMR", "CSO", "CDO") & hu_tham >= 0 & !is.na(hu_tham), NA, hu_est2_ham),
    ts_ind = ifelse(is.na(hu_ts), 1, 0)  # for determining number of imputations needed by tract across 5 decades
    ) %>%
  group_by(GISJOIN10) %>%
  # ID tracts to impute
  mutate(ts_impute = sum(ts_ind)) %>%
  # reduce dataset to only tracts needing TS imputation
  filter(ts_impute > 0) %>%
  ungroup() %>%
  # replace NAs w/ zeros after 1990 --> needed for next step
  mutate(
    hu_ts = ifelse(yr >= 1990 & is.na(hu_ts), 0, hu_ts),
    hu_ts_ham = ifelse(yr >= 1990 & is.na(hu_ts_ham), 0, hu_ts_ham)
    ) %>%
  arrange(GISJOIN10, yr) %>%
  print() 
  

##------------------------------------------------------------------------------------------------------------------------------------
## CONDUCT TIME SERIES OF HU ESTIMATES IN CMR/CSO/CDO TRACTS WHERE OTHER TRACTS IN THEIR COUNTY-YEARS HAVE VECTOR ESTIMATES
##------------------------------------------------------------------------------------------------------------------------------------

## Do Kalman Smoothing to generate HU estimate from temporal trend --> takes awhile
ts_prep1 <- ts_prep %>%
  group_by(GISJOIN10) %>%
  # for avoiding errors
  mutate(
    sum_chk = sum(hu_ts, na.rm = TRUE),
    sum_chk_ham = sum(hu_ts_ham, na.rm = TRUE)
    ) %>%
  # need to arrange in descending order by year for imputation to work properly
  arrange(-yr) %>%
  # do StructTS imputation
  mutate(
    hu_impute = 
      ifelse(
        sum_chk > 0,  # will spit Error if not specified --> replace zeros w/ hu_est2
        na_kalman(  # replace NAs w/ imputed values
          hu_ts,  # run using hu_ts column
          model = "StructTS",  # specifies model to use
          smooth = TRUE
          ),
        hu_est2
        )
    ) %>%
 # StructTS imputation on Hammer data
  mutate(
    hu_impute_ham =
      ifelse(
        sum_chk_ham > 0,
        na_kalman(
          hu_ts_ham,
          model = "StructTS",
          smooth = TRUE
        ),
        hu_est2_ham
      )
  ) %>%
  # rearrange back
  arrange(GISJOIN10, yr) %>%
  ungroup() %>%
  print()



## Step 2: USE TS to Choose County- or Tract-Based Allocation Method
ts_prep2 <- ts_prep1 %>%
  # keep only eligible tracts --> CMR or CSO tracts in county-years w/ at least some vector-based estimates
  filter(ts_ind == 1) %>%
  mutate(
    hu_diff_cmr = abs(hu_cmr - (hu_cmr + hu_impute)/2), # calc. diff. bw CMR est. & linear interp. est.
    hu_diff_ham = abs(hu_ham - (hu_ham + hu_impute_ham)/2),
    hu_diff_tmr = abs(hu_tmr - (hu_tmr + hu_impute)/2),  # calc. diff. bw TMR est. & linear interp. est.
    hu_diff_tham = abs(hu_tham - (hu_tham + hu_impute_ham)/2),
    hu_est3 = ifelse(hu_diff_cmr < hu_diff_tmr, hu_cmr, hu_tmr),
    hu_est3_ham = ifelse(hu_diff_ham < hu_diff_tham, hu_ham, hu_tham),
    method2 = 
      case_when(  # new method
        hu_diff_cmr < hu_diff_tmr & method == "CMR" ~ "CMR",  # stick w/ CMR when diff bw CMR & TS < diff bw TMR & TS
        hu_diff_cmr < hu_diff_tmr & method == "CSO" ~ "CSO",  # stick w/ CSO when diff bw CSO & TS < diff bw TSO & TS
        hu_diff_cmr < hu_diff_tmr & method == "CDO" ~ "CDO",  # stick w/ CDO when diff bw CDO & TS < diff bw TDO & TS
        hu_diff_cmr >= hu_diff_tmr & method == "CMR" ~ "TMR",  # go w/ TMR
        hu_diff_cmr >= hu_diff_tmr & method == "CSO" ~ "TSO",  # go w/ TSO
        hu_diff_cmr >= hu_diff_tmr & method == "CDO" ~ "TDO"  # go w/ TDO
    ),
    method_ham =
      case_when(  # new method
        hu_diff_ham < hu_diff_tham & method == "CMR" ~ "C-HAM",  # stick w/ CMR when diff bw CMR & TS < diff bw TMR & TS
        hu_diff_ham < hu_diff_tham & method == "CSO" ~ "CSO",  # stick w/ CSO when diff bw CSO & TS < diff bw TSO & TS
        hu_diff_ham < hu_diff_tham & method == "CDO" ~ "CDO",  # stick w/ CDO when diff bw CDO & TS < diff bw TDO & TS
        hu_diff_ham >= hu_diff_tham & method == "CMR" ~ "T-HAM",  # go w/ TMR
        hu_diff_ham >= hu_diff_tham & method == "CSO" ~ "TSO",  # go w/ TSO
        hu_diff_ham >= hu_diff_tham & method == "CDO" ~ "TDO"  # go w/ TDO
      ),
  ) %>%
  # clean up
  select(GISJOIN10, yr, method2, method_ham, hu_cmr, hu_tmr, hu_ham, hu_tham, hu_impute, hu_impute_ham, hu_est3, hu_est3_ham) %>%
  print() 



## Count by type
ts_prep2 %>% group_by(method2) %>%
  count() %>%
  arrange(-n)


##################################################################################################
###  STEP 12: JOIN ts_prep2 BACK w/ hu4019_prep and SAVE                                      ##
##################################################################################################

## JOIN
hu4019_prep2 <- hu4019_prep %>%
  # join hu4019_prep to df_impute2
  left_join(ts_prep2, by = c("GISJOIN10", "yr")) %>%
  # clean up
  mutate(
    method = ifelse(is.na(method2), method, method2),  # replace old method with new one from imputation
    method_ham = ifelse(is.na(method_ham), method, method_ham),
    method_ham = ifelse(method_ham == "CMR", "C-HAM", method_ham),
    hu_est = ifelse(is.na(hu_est3), hu_est, hu_est3),  # replace old HU estimate with new one from imputation
    hu_est_ham = ifelse(is.na(hu_est3_ham), hu_est_ham, hu_est3_ham)
  ) %>%
  # keep only the necessary columns
  select(GISJOIN10, yr, method, method_ham, hu_est, hu_est_ham) %>%
  # Clan up names
  rename(
    YEAR = yr,
    METHOD = method,
    METHOD_HAM = method_ham,
    HU = hu_est,
    HU_HAM = hu_est_ham
  ) %>%
  # remove potential duplicates (from Broomfield Co., CO) %>%
  distinct() %>%
  print()  # n = 652,851 (72,539 tracts estimated)


## CHECK METHOD COUNTS
hu4019_prep2 %>%
  group_by(METHOD) %>%
  count() %>%
  arrange(-n)

hu4019_prep2 %>%
  group_by(METHOD_HAM) %>%
  count() %>%
  arrange(-n)


## CLEAN UP
#rm(list = setdiff(ls(), c("hu4019_prep2", "hu4080")))

## FINISH UP
hu4019_prep3 <- hu4019_prep2 %>%
  left_join(hu4080[c(4,5,10)], by = c("GISJOIN10", "YEAR" = "yr")) %>%
  # Hammer override
  rename(HU_HAM_O = hu_ham) %>%
  mutate(
    HU = ifelse(is.na(HU), 0, HU),  # Checked --> No HUs in these 25 tract-decades (5 tracts)
    HU_HAM = ifelse(is.na(HU_HAM), 0, HU_HAM),  # Checked --> No HUs in these 25 tract-decades (5 tracts)
    HU_HAM_O = ifelse(is.na(HU_HAM_O), HU_HAM, HU_HAM_O),
    HU_HAM_O = ifelse(METHOD_HAM == "T-HAM", HU_HAM, HU_HAM_O)
    ) %>%
  print()

## SAVE !!
#write_csv(hu4019, "output/hu4019.csv")

hu4019 <- hu4019_prep3 #%>%
  #mutate(HU_HAM_O = ifelse(METHOD_HAM == "T-HAM", HU_HAM, HU_HAM_O)) %>%
  #select(-HU_HAM) %>%
  #rename(HU_HAM = HU_HAM_O) %>%
  #print()


##################################################################################################
###  STEP 13: MAKE WIDE & IDENTIFY CASES to CORRECT (BIG 1990 DROP-OFF; 70-80-90 dip)           ##
##################################################################################################

##-------------------------------------------------------
## MAKE WIDE
##-------------------------------------------------------

## ESTIMATES
hu4019_wide_prep <- hu4019_prep3 %>%
  # change up year for widening
  mutate(YEAR = str_sub(YEAR, 3, 4)) %>% 
  # keep only 
  select(-METHOD, -METHOD_HAM, -HU_HAM, -HU_HAM_O) %>% 
  pivot_wider(
    names_from = YEAR, 
    names_prefix = "hu", # set up colnames
    names_sep = "",
    values_from = HU,
    values_fill = NA  # ID incomplete cases
    ) %>% 
  print()  # n = 72,539


## METHODS
hu4019_method <- hu4019_prep3 %>%
  mutate(YEAR = str_sub(YEAR, 3, 4)) %>% 
  # keep only 
  select(-HU, -HU_HAM, -METHOD_HAM, -HU_HAM_O) %>% 
  pivot_wider(
    names_from = YEAR, 
    names_prefix = "m", # set up colnames
    names_sep = "",
    values_from = METHOD,
    values_fill = NA  # ID incomplete cases
  ) %>% 
  select(-c(m90:m19)) %>%
  print()  # n = 72,539

## HAM
hu4019_ham <- hu4019_prep3 %>%
  mutate(
    HU_HAM_O = ifelse(METHOD_HAM == "T-HAM", HU_HAM, HU_HAM_O),  # replace w/ T-HAM
    YEAR = str_sub(YEAR, 3, 4)
    ) %>%
  select(-c(METHOD:HU_HAM)) %>%
  pivot_wider(
    names_from = YEAR, 
    names_prefix = "ham", # set up colnames
    names_sep = "",
    values_from = HU_HAM_O,
    values_fill = NA  # ID incomplete cases
  ) %>%
  select(-c(ham90:ham19)) %>%
  print()


# SAVE
#hu4019_cmr <- left_join(hu4019_wide_prep, hu4019_method, by = "GISJOIN10") %>%
#  print()


hu4019_wide <- hu4019_wide_prep %>%
  left_join(hu4019_method, by = "GISJOIN10") %>%
  left_join(hu4019_ham, by = "GISJOIN10") %>%
  print()

write_csv(hu4019_wide, "output/hu4019_wide.csv")
write_csv(hu4080, "output/hu4080.csv")
write_csv(hu4019, "output/hu4019.csv")

rm(list = setdiff(ls(), c("hu4019", "hu4019_wide", "hu4080")))


##-------------------------------------------------------
## CATCH & CORRECT 1970-80-90 DIP
##-------------------------------------------------------

## Dip occurs when estimation method goes from MR (1940-70) to vector (1980) to NHGIS (1990)

# Grab Dip Candidates
#dip_wide <- hu4019_wide_prep %>% 
#  # Get method
#  left_join(hu4019_method, by = "GISJOIN10") %>%
#  # Select tracts w/ MR est. from 1940-70 but vector-based method for 1980
#  filter(
#    m40 %in% c("CMR", "TMR") & 
#      m50  %in% c("CMR", "TMR") & 
#      m60  %in% c("CMR", "TMR") & 
#      m70  %in% c("CMR", "TMR") & 
#      !m80 %in% c("CMR", "TMR")
#  ) %>%
#  print()


## IMPUTE CORRECTION
#dip <- dip_wide %>%
#  # Clean
#  select(GISJOIN10:hu19) %>%
#  # Make long
#  pivot_longer(
#    cols = hu40:hu19,
#    names_to = "yr",
#    values_to = "hu"
#  ) %>%
#  # Correct year & HU
#  mutate(
#    yr = as.integer(paste0("19", str_extract(yr, "\\d+"))),  # fix year
#    hu = ifelse(yr == 1980, NA, hu)  # fix NA
#  ) %>%
#  # Run Arima to estimate 1980
#  mutate(hu_impute = na_kalman(hu, model = "StructTS", smooth = TRUE)) %>%
#  print()


# Use ARIMA Estimate to Select either CMR2 value or Current Method
#dip_fix <- dip %>%
#  # Join w/ hu4080 to get alt. estimates
#  left_join(hu4080[c(4,5,7,9,11,12,15,16)], by = c("GISJOIN10", "yr")) %>%
#  # Join w/ hu4019_method to get method names
#  left_join(hu4019_method[c(1, 6)], by = "GISJOIN10") %>% 
#  # Keep only 1980
#  filter(yr == 1980) %>%
#  # Calculate differences and grab smaller difference
#  mutate(
#    cmr_diff = abs(hu_cmr2 - (hu_cmr2 + hu_impute)/2),  # CMR diff. from Arima estimate
#    hu_est_diff = abs(hu_est - (hu_est + hu_impute)/2),  # HU diff. from Arima est.
#    hu_new = ifelse(cmr_diff < hu_est_diff | is.na(cmr_diff), hu_cmr2, hu_est),  # grab better estimate
#    method2 = ifelse(cmr_diff < hu_est_diff | is.na(cmr_diff), "CMR-O", m80)  # Rename method CMR-O: override
#  ) %>%
#  # Clean
#  select(GISJOIN10, yr, hu_new, method2) %>%
#  print()


## Join back with hu4019_wide
#hu4019 <- hu4019_prep2 %>%
#  left_join(dip_fix, by = c("GISJOIN10", "YEAR" = "yr")) %>%
#  mutate(
#    HU = ifelse(!is.na(hu_new), hu_new, HU),  # HUs w/ dip corrected HUs
#    HU = ifelse(HU < 0, 0, HU),  # replace negative values w/ zeroes (very few cases, none less than -1)
#    METHOD = ifelse(!is.na(method2), method2, METHOD)  # add in method
#  ) %>%
#  # Clean up
#  select(-hu_new, -method2) %>%
#  print()



##-------------------------------------------------------
## MAKE WIDE
##-------------------------------------------------------

## ESTIMATES
#hu4019_wide <- hu4019 %>%
#  # change up year for widening
#  mutate(YEAR = str_sub(YEAR, 3, 4)) %>% 
#  # keep only 
#  select(-METHOD) %>% 
#  pivot_wider(
#    names_from = YEAR, 
#    names_prefix = "hu", # set up colnames
#    names_sep = "",
#    values_from = HU,
#    values_fill = NA  # ID incomplete cases
#  ) %>% 
#  print()  # n = 72,539

## TEST
test <- hu4019 %>% 
  select(-METHOD, -METHOD_HAM) %>% 
  pivot_longer(
    cols = HU:HU_HAM_O, 
    names_to = "model", 
    values_to = "vals"
    ) %>% 
  group_by(GISJOIN10, YEAR) %>% 
  mutate(
    low = min(vals), 
    med = median(vals),
    mean = mean(vals),
    hi = max(vals)
    ) %>% 
  select(-model, -vals) %>% 
  distinct()


##-------------------------------------------------------------------------------
##-------------------------------------------------------------------------------
##  ID CASES for CORRECTION (Done in First Iteration) ---------------------------
##-------------------------------------------------------------------------------
##-------------------------------------------------------------------------------

## IMPORT URBAN RENEWAL DATA
ur <- read_csv("tables/urb_renewal_tracts.csv") %>%
  print()

## FLAG Special Cases
drop <- hu4019_wide %>%
   filter(
    # ID 3x dropoff from 1970 or 1980 to 1990 OR 2x dropoff from 1970 or 1980 to 1990 & 2000
    ((hu90*3 < hu80 | hu90*3 < hu70) | ((hu90*2 < hu80 | hu90*2 < hu70) & (hu00*2 < hu80 | hu00*2 < hu70))) &  
      # only include tracts w/ substantial HU stock
      (hu70 > 100 | hu80 > 100) &  
      # remove cases where hu90 data is the apparent problem
      !(hu80 > 0.8*hu70 & hu00 > 0.8*hu80) &  
      # make sure tract is not in urban renewal zone
      !GISJOIN10 %in% ur$GISJOIN10 &
      # don't drop tracts w/ consistent declines in HU since 1950 w/ a CMR or TMR-estimated 1940 Or 1950
      !(hu50 > hu60 & hu60 > hu70 & hu70 > hu80 & !(m40 %in% c("CMR", "TMR") | m50 %in% c("CMR", "TMR")))
  ) %>%
  print()  # n = 284

## SAVE OUT to TABLES
#write_csv(drop, "tables/drop.csv")

#G3600610014300  # Central Park
#G1100010006202  # National Mall
#G5100130980100  # Arlington Cemetery
#G5100130980200  # Potomac
#G3600810038302  # Flushing Meadows Corona/Citi Field


## SAVE OUT hu4019_wide & hu4019
#write_csv(hu4019_wide, "output/hu4019_wide.csv")
#write_csv(hu4019, "output/hu4019_wide.csv")


## LOOK at DISTRIBUTION
hu4019 %>%
  group_by(METHOD) %>%
  count() %>%
  arrange(-n)


#########################
## FIX DIPS
#########################

#test <- hu4019 %>%
#  mutate(
#    ind_cmr = ifelse(METHOD %in% c("CMR", "TMR"), 1, 0),
#    ind_aw = ifelse(METHOD == "AW", 1, 0),
#    ind0 = ifelse(YEAR >= 1990 & HU < 1, 1, 0)
#    ) %>%
#  group_by(GISJOIN10) %>%
#  mutate(
#    cmr_sum = sum(ind_cmr),
#    aw_sum = sum(ind_aw),
#    ind0_sum = sum(ind0)
#    ) %>%
#  filter(cmr_sum < 5 & aw_sum < 5 & ind0_sum < 4) %>%
#  select(-c(ind_cmr:ind0_sum)) %>%
#  ungroup() %>%
#  mutate(
#    HU_EST = 
#      ifelse(
#        METHOD %in% c("CMR", "TMR", "AW"),
#        NA,
#        HU
#      )
#  ) %>%
#  print()


#test1 <- test %>%
#  group_by(GISJOIN10) %>%
#  arrange(-YEAR) %>%
#  mutate(HU_IMP = na_kalman(HU_EST, model = "StructTS", smooth = TRUE)) %>%
#  arrange(GISJOIN10, YEAR) %>%
#  print()


#########################
## FIX DIPS
#########################

diff80 <- hu4019_wide %>%
  filter(m40 == m50 & m50 == m60 & m60 == m70 & m70 != m80 & !m40 %in% c("CSO", "TSO", "TDW-90")) %>%
  filter(hu90 > 0 | hu00 > 0 | hu10 > 0 | hu19 > 0) %>%
  print()

correct80 <- hu4019 %>%
  filter(GISJOIN10 %in% diff80$GISJOIN10) %>%
  mutate(HU_TS = ifelse(YEAR <= 1980, NA, HU)) %>%
  group_by(GISJOIN10) %>%
  arrange(-YEAR) %>%
  mutate(hu_impute = na_kalman(HU_TS, model = "StructTS", smooth = TRUE)) %>%
  arrange(GISJOIN10, YEAR) %>%
  print()

correct80

View(diff80)


