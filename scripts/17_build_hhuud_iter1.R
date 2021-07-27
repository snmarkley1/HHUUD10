
#########################################################################
#########################################################################
###                                                                   ###
###           BUILD the HIST. HU. & URB. DATABASE (1st CUT)           ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("D:/HIST_HU_URB/scripts/00_preamble.R")


###############################################################################
###############################################################################
##  STEP 1: LOAD & ORGANIZE MAX. REABSORPTION DATA                           ##
###############################################################################
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
mr_import  # n = 360,773


###############################################################################
###############################################################################
##  STEP 2: IMPORT/CLEAN SPARSE & DROP DATA                                  ##
###############################################################################
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
df_uso <- uso_import %>%
  select(GISJOIN:hu80) %>%
  # make long
  pivot_longer(
    cols = c(hu40:hu80),
    names_to = "yr",
    values_to = "hu"
    ) %>%
    mutate(
    yr = as.integer(paste0("19", str_extract(yr, "\\d+"))),  # rename yr to match formats
    method = "USO"  # add name of method: USO --> Uncorrected Sparse override
    ) %>%
  rename(hu_uso = hu) %>%
  arrange(GISJOIN, yr) %>%
  print()  # n = 4,495


###############################################################################
###############################################################################
##  STEP 3: COMBINE overridE ESTIMATES & CALCULATE                           ##
###############################################################################
###############################################################################

##------------------------------------------------------------
## LOAD YSB 1990 (tracts: ysb90_t) for HAMMER METHOD
##------------------------------------------------------------
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
  print()  # n = 362,695


##------------------------------------------------------------
## Join, Organize, & Override NAs
##------------------------------------------------------------
mr_join <- mr_import %>% 
  # Add 1990 YSB Data
  full_join(ysb90, by = c("GISJOIN", "yr")) %>%
  # Add USO data (remove hu count [3] --> same as ysb)
  left_join(df_uso[-3], by = c("GISJOIN", "yr")) %>%
  # Adjust methods
  mutate(method = ifelse(is.na(method), "CMR", method)) %>%
  # Correct some NAs
  mutate(
    hu_mr = ifelse(is.na(hu_mr) & is.na(hu_ysb), 0, hu_mr),   # when MR & YSB are NA --> 0 (n = 799)
    hu_ysb = ifelse(is.na(hu_mr) & is.na(hu_ysb), 0, hu_ysb),  # when MR & YSB are NA --> 0 (n = 799)
    hu_mr = ifelse(is.na(hu_mr) & !is.na(hu_ysb), hu_ysb, hu_mr),  # when MR is NA & YSB is not NA --> YSB  (n = 1,123)
    hu_ysb = ifelse(is.na(hu_ysb) & method %in% c("USO", "UDO"), 0, hu_ysb),  # when ysb should be zero (overlaps w/ 2 above)  (n = 100)
    hu_ysb = ifelse(is.na(hu_ysb) & hu_mr == 0, 0, hu_ysb),  # when ysb is NA but MR is zero  (n = 760)
    hu_mr = ifelse(method %in% c("USO", "UDO"), hu_ysb, hu_mr)  # replace MRs in sparse tracts w/ ysb  (n = 4,495--overlaps w/ above)
  ) %>%
  print()

# NOTE: 820 tract-years where hu_ysb is NA & hu_mr is > 0 (correct in following steps)

##################################################################################################
##################################################################################################
###  STEP 4: PROPORTIONALLY ALLOCATE MAX. REABSORP. ESTIMATES USING COUNTY HU COUNTS            ##
##################################################################################################
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

dr_cmr <- mr_join %>%
  # join mr_join w/ county data for prop. alloc.
  left_join(counties_long, by = c("GISJOIN", "yr")) %>%
  # organize
  select(STATE, COUNTY, CO_CALC, GISJOIN, yr, hu_co, hu_mr, hu_ysb, method) %>%
  # get tract HU totals by county-years
  group_by(CO_CALC, yr) %>%
  # get sum of tract estimates by county & year for prop. alloc. (MR)
  mutate(mr_sum = sum(hu_mr)) %>%
  ungroup() %>%
  # do prop. alloc. for MR estimates
  mutate(
    hu_mr2 = hu_mr / mr_sum * hu_co,  # get tract hu_mr proportion
    hu_ysb = ifelse(is.na(hu_ysb), hu_mr2, hu_ysb),  # replace ysb NAs w/ hu_mr2 proportion
    na_ysb = ifelse(is.na(hu_ysb), 1, 0)  # mark ysb NAs
    ) %>%
  # Now do YSB
  group_by(CO_CALC, yr) %>%
  # get sum of tract estimates by county & year for prop. alloc. (YSB)
  mutate(ysb_sum = sum(hu_ysb)) %>%
  ungroup() %>%
  # do prop. alloc. for YSB estimates (Hammer method)
  mutate(
    hu_ysb2 = hu_ysb / ysb_sum * hu_co,  # get tract hu_ysb proportion
    hu_ysb = ifelse(na_ysb == 1, NA, hu_ysb)  # revert back to original NA data
    ) %>%
  select(STATE:hu_ysb, hu_mr2, hu_ysb2, method) %>%
  arrange(GISJOIN, yr) %>%
  print()


# Look at summary breakdown
dr_cmr %>%
  group_by(method) %>%
  count()

# method       n
# CMR    358,200
# USO      4,495

## Clean up
rm(mr_import, mr_join, uso_import, temp, temp1, counties_long, county_tracts)


##############################################################################
##############################################################################
##  STEP 5: LOAD & ORGANIZE INTERSECTION DATA (AW, TDW, TDW90)              ##
##############################################################################
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
  hu_12 <- paste0(hu, "_12")
  
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
        GISJOIN_12 = GISJOIN_1,
        hu80_12 = hu80_1,
        sqmi_12 = sqmi_1
        ) %>%
      select(GISJOIN, yr, hu, sqmi, GISJOIN_1, hu_1, sqmi_1, GISJOIN_12, hu_12, sqmi_12, sqmi_int)
    
  } else{
    
    temp1 <- temp %>%
      select(GISJOIN, yr, hu, sqmi, GISJOIN_1, hu_1, sqmi_1, GISJOIN_12, hu_12, sqmi_12, sqmi_int)
    
  }
  
  ## match column names (data.table package)
  setnames(temp1, c(hu, hu_1, hu_12, "sqmi_12", "GISJOIN_12"), c("hu", "hu_1", "hu_2", "sqmi_2", "JOIN_BG"))
  
  ## write out as single data frame
  df_import <- bind_rows(df_import, temp1) %>%
    arrange(yr, GISJOIN)
  
}

# Update & inspect
df_int <- df_import %>%
  print() # n = 859,089 atoms

## Clean up
rm(list = setdiff(ls(), c("df_int", "dr_cmr", "packages", "ysb90")))


#########################################################################################
#########################################################################################
##  STEP 6: ESTABLISH FLAGS to ID DATA to be REMOVED                                   ##
#########################################################################################
#########################################################################################

##----------------------------------------------------
## Make flag data frame for AW
##----------------------------------------------------
flag_aw <- df_int %>%
  # flag suspicious atoms --> NA or false zero in source tracts
  filter(
    (hu < 10 & hu_1 > 10 & hu_2 > 10) |
      is.na(hu)  # remove NAs from source years
  ) %>%
  # group by GEOID and year
  group_by(JOIN_BG, yr) %>%
  summarize() %>%
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  print()  # n = 18,658


##---------------------------------------------------
## Make flag data frame for TDW
##---------------------------------------------------
flag_tdw <- df_int %>%
  # flag suspicious atoms --> NA or false zero in source or pseudo-target tracts
  filter(
    (hu < 10 & hu_1 > 10 & hu_2 > 10) |  # suspicious hu
      (hu > 10 & hu_1 < 10 & hu_2 > 10) |  # suspicious hu_1
      (hu > 10 & hu_1 < 10 & yr == 1980) |  # grab 1980 special problems (hu_1 & hu_2 are same)
      is.na(hu) |  # remove NAs from source years
      is.na(hu_1)  # remove NAs from pseudo-target years
  ) %>%
  # group by GEOID and year
  group_by(JOIN_BG, yr) %>%
  summarize() %>%
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  print()  # n = 35,068


##---------------------------------------------------
## Make flag data frame for TDW90
##---------------------------------------------------
flag_tdw90 <- df_int %>%
  # flag suspicious atoms --> NA or false zero in source or target tracts
  filter(
    (hu < 10 & hu_1 > 10 & hu_2 > 10) |  # suspicious hu
      (hu > 0 & hu_2 == 0) |  # 1990 HUs are zeroes (taken care of in sparse override)
      is.na(hu) |  # remove NAs from source years
      is.na(hu_2)  # remove NAs from target years
  ) %>%
  # group by GEOID and year
  group_by(JOIN_BG, yr) %>%
  summarize() %>%
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  print()  # n = 35,763


##############################################################################
##############################################################################
##  STEP 7: GENERATE AREAL WEIGHTING  (AW)  ESTIMATES                       ##
##############################################################################
##############################################################################

##---------------------------------------------------------
## AREAL WEIGHTING
##---------------------------------------------------------

temp_aw <- df_int %>%
  # clean up columns
  select(-c(GISJOIN_1:sqmi_1)) %>%
  # remove NAs from source years
  #filter(!is.na(hu)) %>%
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  filter(!ID %in% flag_aw$ID) %>%
  # group by original GISJOIN + JOIN_T to allow calculation of overlapping areas
  group_by(GISJOIN, yr, hu, sqmi, JOIN_BG, sqmi_2) %>%
  # get sqmi of atoms
  summarize(sqmi_int = sum(sqmi_int)) %>%
  ungroup() %>%
  # calcuate overlaps
  mutate(
    pcover_t = sqmi_int / sqmi_2, # % coverage for target tract (t)
    pcover_s = sqmi_int / sqmi,  # % coverage for source tract (s)
    hu_int = pcover_s * hu  # calc. intersection
  ) %>%
  # create JOIN_T for grouping into target tracts
  mutate(JOIN_T = str_sub(JOIN_BG, 1, 14)) %>%
  # estimated hu in tract t (target zone)
  group_by(JOIN_T, yr) %>%
  summarize(
    sqmi = sum(sqmi_2),  # get sqmi
    pcover_t = sum(pcover_t),  # sum % coverage for target tracts
    pcover_s = sum(pcover_s),  # sum % converage for source tracts
    hu_aw = sum(hu_int)  # sum HUs in atom
  ) %>%
  arrange(yr, JOIN_T) %>%
  print()

## Check histogram
#temp_aw %>%
#  filter(between(pcover_t, 0.9, 0.999)) %>%
#  ggplot(aes(x = pcover_t)) +
#  geom_histogram(binwidth = 0.01) +
#  scale_x_continuous(breaks = seq(0.9, 0.99, 0.01))

##------------------------------------------------------------
## FILTER OUT TRACT-YEARS W/ COVERAGE < 0.99
##------------------------------------------------------------

dr_aw <- temp_aw %>%
  filter(
    (pcover_t >= 0.99 & pcover_s >= 0.99)|  # keep only tracts with > 99% target & source coverage
      # correct for NYC inconsistencies (disregard target zone coverage for years 1950-1970 --> sig. Census redrawing of shoreline tracts
      (str_detect(JOIN_T, "G360047|G360050|G360061|G360081") & between(yr, 1950, 1970) & pcover_s >= 0.99)
  ) %>%
  # rename to match previous
  rename(GISJOIN10 = JOIN_T)  %>%
  left_join(ysb90, by = c("GISJOIN10" = "GISJOIN", "yr")) %>%
  # catch and remove cases where AW est. is less than raw YSB estimate
  filter(hu_aw >= hu_ysb) %>%
  # organize columns/remove pcover
  select(GISJOIN10, yr, hu_aw) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 55,499
  

## Clean up
rm(temp_aw, flag_aw)


########################################################################################
########################################################################################
## STEP 8: TARGET DENSITY WEIGHTING (TDW)-- 1 DECADE FORWARD                          ##
########################################################################################
########################################################################################

##----------------------------------------------------------------------------------------------------
## TDW: CALC. HU ESTIMATES in PSEUDO-TARGET (pt) TRACTS (source [s] tracts + 10 years)
##----------------------------------------------------------------------------------------------------

temp_tdw_pt <- df_int %>%
  # get unique ID for filter
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  # remove suspicious cases from flagged atoms
  filter(!ID %in% flag_tdw$ID) %>%
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
  # keep only pt tracts over 99% covered by s tracts
  filter(pcover_pt > 0.99)  %>%
  print()  # n = 243,356


##---------------------------------------------------------
## PUT TDW ESTIMATES in TARGET TRACTS
##---------------------------------------------------------

dr_tdw <- df_int %>%
  # join intersection file to temp_tdw_pt
  left_join(temp_tdw_pt[1:3], by = c("GISJOIN_1", "yr")) %>%
  # remove cases w/o a TDW estimate
  drop_na(hu_pt) %>%
  # take fraction of hu_pt in atom
  mutate(hu_pt_t = hu_pt * sqmi_int / sqmi_1) %>%
  # Add tract ID
  mutate(JOIN_T = str_sub(JOIN_BG, 1, 14)) %>%
  # get variables in target tract geographies
  group_by(JOIN_T, yr) %>%
  summarize(
    sqmi = sum(sqmi_2),  # get sqmi
    pcover_pt = sum(sqmi_int/sqmi_1),  # sum % coverage for target tracts
    pcover_t = sum(sqmi_int/sqmi_2),  # sum % converage for source tracts
    hu_tdw = sum(hu_pt_t)  # sum HUs in atom
  ) %>%
  # keep pcover_pt & pcover_t above 99% coverage (keeps only pt tracts that remained same size or grew)
  filter(pcover_pt > 0.99 & pcover_t > 0.99) %>%
  # remove suspicious tracts
  rename(GISJOIN10 = JOIN_T) %>%
  # add YSB data in for filtering
  left_join(ysb90, by = c("GISJOIN10" = "GISJOIN", "yr")) %>%
  # remove cases where TDW estimate is less than YSB estimate
  filter(hu_tdw >= hu_ysb) %>%
  # clean up
  select(GISJOIN10, yr, hu_tdw) %>%
  print() # n = 65,558


## Clean up
rm(temp_tdw_pt, flag_tdw)


################################################################################################
################################################################################################
###  STEP 9: TARGET DENSITY WEIGHTING (TDW90)-- HARMONIZED 1990 YSB DATA                      ##
################################################################################################
################################################################################################

##-------------------------------------------------------------------------------------------
## REMOVE PSEUDO-TARGET TRACTS from DF_INT --> KEEP ONLY SOURCE and TARGET TRACTS
##-------------------------------------------------------------------------------------------

temp_tdw <- df_int %>%
  # create unique ID for filtering
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  filter(!ID %in% flag_tdw90$ID) %>%
  # remove pt tracts by grouping polygons by s & t tracts only
  group_by(yr, GISJOIN, hu, sqmi, JOIN_BG, hu_2, sqmi_2) %>%
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
  #create tract geography
  mutate(JOIN_T = str_sub(JOIN_BG, 1, 14)) %>%
  # put in target tract geographies
  group_by(JOIN_T, yr) %>%
  summarize(
    pcover_t = sum(pcover_t),  # target tract coverage
    pcover_s = sum(pcover_s),  # source tract coverage
    hu_tdw90 = sum(hu_atom)  # sum HU counts in atoms
  ) %>%
  arrange(yr, JOIN_T) %>%
  print()  # n = 182,376


##----------------------------------------------------------------
## FIX SPECIAL CASES and GENERATE FINAL OUTPUT
##----------------------------------------------------------------

dr_tdw90 <- temp_tdw1 %>%
  # remove tracts that went large to small & those from target that were incompletely covered (< 97%)
  filter(pcover_t > 0.99) %>%
  # fix up names and columns
  rename(GISJOIN10 = JOIN_T) %>%
  # Get ysb data for corrections
  left_join(ysb90, by = c("GISJOIN10" = "GISJOIN", "yr")) %>%
  filter(hu_tdw90 >= hu_ysb) %>%
  # organize
  select(GISJOIN10, yr, hu_tdw90) %>%
  ungroup() %>%
  print()  # n = 144,268
  
## clean up
rm(temp_tdw, temp_tdw1, flag_tdw90)


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
  # Name methods
  mutate(
    method = case_when(
      method == "CMR" & !is.na(hu_aw) ~ "AW",
      method == "CMR" & is.na(hu_aw) & !is.na(hu_tdw) ~ "TDW-1",
      method == "CMR" & is.na(hu_aw) & is.na(hu_tdw) & !is.na(hu_tdw90) ~ "TDW-90",
      TRUE ~ method
    )
  ) %>%
  # Choose HU estimation method for CMR
  mutate(
    hu_est_cmr = case_when(
      method %in% c("USO", "UDO") ~ hu_cmr,  # already corrected to include MR + Corrected Sparse override
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90,
      method == "CMR" ~ hu_cmr  # includes Sparse override
      )
  ) %>%
  # Choose HU estimation method for Hammer
  mutate(
    hu_est_ham = case_when(
      method %in% c("USO", "UDO") ~ hu_ham,  # already corrected to include MR + Corrected Sparse override
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90,
      method == "CMR" ~ hu_ham  # includes Sparse override
    )
  ) %>%
  # final cleaning
  mutate(
    method = ifelse(method == "USO", "CSO", method),  # correct method names
    method = ifelse(method == "UDO", "CDO", method),  # correct method names
    hu_est_cmr = ifelse(is.na(hu_est_cmr), 0, hu_est_cmr),  # replace NAs
    hu_est_ham = ifelse(is.na(hu_est_ham), 0, hu_est_ham)  # replace NAs
  ) %>%
  # Organize
  rename(GISJOIN10 = GISJOIN) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 362,695


## CHECK DISTRIBUTION of METHODS
hu4080 %>%
  group_by(method) %>%
  count() %>%
  arrange(-n)

#method      n
# CMR    212,175
# TDW-90  56,930
# AW      55,499
# TDW-1   34,596
# CSO      4,495


## SAVE OUT
write_csv(hu4080, "output/hu4080.csv")

##################################################################################################
##################################################################################################
###  STEP 11: ADJUST MR ESTIMATES BASED on VECTOR VALUES                                        ##
##################################################################################################
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
  print()  # n = 652,851


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
## CONDUCT TIME SERIES OF HU ESTIMATES IN CMR/CSO TRACTS WHERE OTHER TRACTS IN THEIR COUNTY-YEARS HAVE VECTOR ESTIMATES
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


##------------------------------------------------------------------------------
## Step 2: USE TS to Choose County- or Tract-Based Allocation Method
##------------------------------------------------------------------------------

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
  # Clean up names
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


## FINISH UP
hu4019 <- hu4019_prep2 %>%
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


##################################################################################################
##################################################################################################
###  STEP 13: MAKE WIDE & IDENTIFY CASES to CORRECT (BIG 1990 DROP-OFF; 70-80-90 dip)           ##
##################################################################################################
##################################################################################################

##-------------------------------------------------------
## MAKE WIDE
##-------------------------------------------------------

## ESTIMATES
hu4019_wide_prep <- hu4019 %>%
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
hu4019_method <- hu4019 %>%
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
hu4019_ham <- hu4019 %>%
  mutate(
    #HU_HAM_O = ifelse(METHOD_HAM == "T-HAM", HU_HAM, HU_HAM_O),  # replace w/ T-HAM
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

## MAKE WIDE
hu4019_wide <- hu4019_wide_prep %>%
  left_join(hu4019_method, by = "GISJOIN10") %>%
  left_join(hu4019_ham, by = "GISJOIN10") %>%
  print()

##------------------------------------------------------
## SAVE OUT !!
##------------------------------------------------------
write_csv(hu4019_wide, "output/hu4019_wide.csv")
write_csv(hu4080, "output/hu4080.csv")
write_csv(hu4019, "output/hu4019.csv")

## Clear workspace
rm(list = setdiff(ls(), c("hu4019", "hu4019_wide", "hu4080")))

