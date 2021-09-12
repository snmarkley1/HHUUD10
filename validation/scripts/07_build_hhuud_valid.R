
#########################################################################
#########################################################################
###                                                                   ###
###              Running Validation 1: Missing Tract Info             ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## create new directory
dir.create("output")


###############################################################################
###############################################################################
##  LOAD & ORGANIZE MAX. REABSORPTION DATA                                   ##
###############################################################################
###############################################################################

## Import and Bind Maximum Reabsorption Data
mr_import <- NULL  # create empty data frame (dasymetrically-refined maximum reabsorption (DR-MR))
for(i in unique(c("90", "00", "10"))){
  
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
    mutate(
      yr = as.integer(paste0("20", i)),
      yr = ifelse(yr == 2090, 1990, yr)
    ) %>%
    select(GISJOIN, yr, SUM) %>%
    rename(hu_mr = SUM) %>%
    arrange(GISJOIN)
  
  # bind rows to save out as single data frame
  mr_import <- bind_rows(mr_import, temp1)
  
}

# inspect
mr_import  # n = 8,226


###############################################################################
###############################################################################
##  IMPORT/CLEAN SPARSE & DROP DATA                                          ##
###############################################################################
###############################################################################

##-------------------------------------------------------
## Import and Clean Sparse Data (includes zeroes)
##-------------------------------------------------------

## Import sparse data (USO)
uso_import <- sf::st_read(  # USO: Uncorrected Sparse override
  dsn = "gis_files/database1.gdb",
  layer = "t19"
) %>%
  as_tibble() %>%
  filter(h1990_1 < 10) %>%
  print()


## Clean USO data
df_uso <- uso_import %>%
  select(GISJOIN, hm_st90, hm_st00, HU2006_) %>%
  # rename
  rename(
    ham90 = hm_st90,
    ham00 = hm_st00,
    hu10 = HU2006_
  ) %>%
  # make long
  pivot_longer(
    cols = ham90:hu10,
    names_to = "yr",
    values_to = "hu"
  ) %>%
  mutate(
    yr = as.integer(paste0("20", str_extract(yr, "\\d+"))),  # rename yr to match formats
    yr = ifelse(yr == 2090, 1990, yr),
    method = "USO"  # add name of method: USO --> Uncorrected Sparse override
  ) %>%
  rename(hu_uso = hu) %>%
  arrange(GISJOIN, yr) %>%
  print()  # n = 270


###############################################################################
###############################################################################
##  COMBINE overridE ESTIMATES & CALCULATE                                   ##
###############################################################################
###############################################################################

##------------------------------------------------------------
## LOAD YSB 1990
##------------------------------------------------------------
ysb90 <- sf::st_read(  # USO: Uncorrected Sparse override --> really ysb10
  dsn = "gis_files/database1.gdb",
  layer = "bg19"
) %>%
  as_tibble() %>%
  select(GISJOIN, HU1990:HU2015_19) %>%
  pivot_longer(
    cols =  HU1990:HU2015_19,
    names_to = "yr",
    values_to = "hu_ysb"
  ) %>%
  mutate(
    yr = as.integer(str_extract(yr, "\\d+")),
    GISJOIN = str_sub(GISJOIN, 1, 14)
    ) %>%
  group_by(GISJOIN, yr) %>%
  summarize(hu_ysb = sum(hu_ysb)) %>%
  print()  # n = 10,980
  
  
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
  print()  # n = 11,160



##################################################################################################
##################################################################################################
###  PROPORTIONALLY ALLOCATE MAX. REABSORP. ESTIMATES USING COUNTY HU COUNTS                    ##
##################################################################################################
##################################################################################################

##----------------------------------------------
## LOAD in & ORGANIZE COUNTY/STATE DATA
##----------------------------------------------

counties <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t19"
) %>%
  as_tibble() %>%
  mutate(CO_CALC = str_sub(GISJOIN, 1, 8)) %>%
  rename(
    `2000` = h2000_c, 
    `1990` = h1990_c
    ) %>%
  group_by(CO_CALC, `1990`, `2000`) %>%
  summarize(
    `2010` = sum(HU2006_),
    `2015` = sum(HU2015_)
  ) %>%
  pivot_longer(
    cols = `1990`:`2015`,
    names_to = "yr",
    values_to = "hu"
  ) %>%
  mutate(yr = as.integer(yr)) %>%
  print()
  

## Missing data
missing_co <- data.frame(
  STATE = "Florida",
  COUNTY = "Orange",
  GISJOIN = "G1200950990000",
  CO_CALC = "G1200950",
  yr = c(1990, 2000),
  hu_co = c(282686, 361349)
) %>%
  print()


## Add in tract data
counties_long <- read_csv("D:/HHUUD10/tables/county_tracts.csv") %>%
  mutate(
    CO_CALC = JOIN_CO,  # includes aggregated counties (border changes)
    #CO_JOIN = substr(GISJOIN, 1, 8)  # for joining
  ) %>%
  select(STATE, COUNTY, GISJOIN, CO_CALC) %>%
  left_join(counties, by = "CO_CALC") %>%
  drop_na() %>%
  arrange(CO_CALC, yr) %>%
  rename(hu_co = "hu") %>%
  bind_rows(missing_co) %>%
  print()


##-------------------------------------------------------------------
## CORRECT MR ESTIMATES w/ PROPORTIONAL ALLOCATION
##-------------------------------------------------------------------

dr_cmr_prep <- mr_join %>%
  # join mr_join w/ county data for prop. alloc.
  full_join(counties_long, by = c("GISJOIN", "yr")) %>%
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
  distinct() %>%
  print()


## Bring in real Hammer data
t10 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t19"
) %>%
  as_tibble() %>%
  select(GISJOIN, hm_st00, hm_st90) %>%
  pivot_longer(
    cols = hm_st00:hm_st90,
    names_to = "yr",
    values_to = "ham"
  ) %>%
  mutate(yr = ifelse(yr == "hm_st00", 2000, 1990)) %>%
  distinct() %>%
  arrange(GISJOIN, yr) %>%
  print()


## add in these hammer method data
dr_cmr <- dr_cmr_prep %>%
  left_join(t10, by = c("GISJOIN", "yr")) %>%
  print()


# Look at summary breakdown
dr_cmr %>%
  group_by(method) %>%
  count()  # NAs are 2019


## Clean up
rm(dr_cmr_prep, mr_import, mr_join, uso_import, temp, temp1, counties_long, county_tracts, counties, county_1990, county_census, county_census00, states, missing_co)


##############################################################################
##############################################################################
##  LOAD & ORGANIZE INTERSECTION DATA (AW, TDW, TDW90)                      ##
##############################################################################
##############################################################################

##---------------------------------------------------------
## GRAB / ORGANIZE INTERSECTION SHAPEFILES
##---------------------------------------------------------

## 1990
int90 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "int90"
) %>%
  as_tibble() %>%
  mutate(yr = 1990) %>%
  select(GISJOIN, yr, HU1990_90, sqmi, GISJOIN_1, HU1990_00, sqmi_1, GISJOIN_12, HU1990, sqmi_12, sqmi_int) %>%
  rename(
    hu = HU1990_90,
    hu_1 = HU1990_00,
    hu_2 = HU1990,
    JOIN_BG = GISJOIN_12,
    sqmi_2 = sqmi_12
  ) %>%
  print()


## 2000
int00 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "int00"
) %>%
  as_tibble() %>%
  mutate(yr = 2000) %>%
  select(GISJOIN, yr, HU2000_00, sqmi, GISJOIN_1, HU_2000, sqmi_1, GISJOIN_12, HU2000, sqmi_12, sqmi_int) %>%
  rename(
    hu = HU2000_00,
    hu_1 = HU_2000,
    hu_2 = HU2000,
    JOIN_BG = GISJOIN_12,
    sqmi_2 = sqmi_12
  ) %>%
  print()


## 2010
int10 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "int10"
) %>%
  as_tibble() %>%
  mutate(yr = 2010) %>%
  select(GISJOIN, yr, HU2006_10, sqmi, GISJOIN_1, HU2010, sqmi_1, GISJOIN_12, HU2010_1, sqmi_12, sqmi_int) %>%
  rename(
    hu = HU2006_10,
    hu_1 = HU2010,
    hu_2 = HU2010_1,
    JOIN_BG = GISJOIN_12,
    sqmi_2 = sqmi_12
  ) %>%
  print()


## Bind
df_int <- bind_rows(int90, int00, int10) %>%
  print()


## Clean up
rm(list = setdiff(ls(), c("df_int", "dr_cmr", "packages", "ysb90")))



##############################################################################
##############################################################################
##  GENERATE AREAL WEIGHTING  (AW)  ESTIMATES                               ##
##############################################################################
##############################################################################

##---------------------------------------------------------
## AREAL WEIGHTING
##---------------------------------------------------------

temp_aw <- df_int %>%
  # clean up columns
  select(-c(GISJOIN_1:sqmi_1)) %>%
  # remove NAs from source years
  filter(!is.na(hu)) %>%
  mutate(ID = paste0(JOIN_BG, yr)) %>%
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
  arrange(JOIN_T, yr) %>%
  print()


##------------------------------------------------------------
## FILTER OUT TRACT-YEARS W/ COVERAGE < 0.99
##------------------------------------------------------------

dr_aw <- temp_aw %>%
  filter(pcover_t >= 0.99 & pcover_s >= 0.99) %>%
  # rename to match previous
  rename(GISJOIN10 = JOIN_T)  %>%
  left_join(ysb90, by = c("GISJOIN10" = "GISJOIN", "yr")) %>%
  # catch and remove cases where AW est. is less than raw YSB estimate
  filter(hu_aw > 0.9 * hu_ysb) %>%
  # organize columns/remove pcover
  select(GISJOIN10, yr, hu_aw) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 55,499


## Clean up
rm(temp_aw)


########################################################################################
########################################################################################
## TARGET DENSITY WEIGHTING (TDW)-- 1 DECADE FORWARD                                  ##
########################################################################################
########################################################################################

##----------------------------------------------------------------------------------------------------
## TDW: CALC. HU ESTIMATES in PSEUDO-TARGET (pt) TRACTS (source [s] tracts + 10 years)
##----------------------------------------------------------------------------------------------------

temp_tdw_pt <- df_int %>%
  # get unique ID for filter
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  # remove suspicious cases from flagged atoms
  drop_na(hu, hu_1) %>%
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
  print()  # n = 10,333


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
  filter(hu_tdw > 0.9 * hu_ysb) %>%
  # clean up
  select(GISJOIN10, yr, hu_tdw) %>%
  print()


## Clean up
rm(temp_tdw_pt)


################################################################################################
################################################################################################
###  TARGET DENSITY WEIGHTING (TDW90)-- HARMONIZED 1990 YSB DATA                              ##
################################################################################################
################################################################################################

##-------------------------------------------------------------------------------------------
## REMOVE PSEUDO-TARGET TRACTS from DF_INT --> KEEP ONLY SOURCE and TARGET TRACTS
##-------------------------------------------------------------------------------------------
temp_tdw <- df_int %>%
  # create unique ID for filtering
  mutate(ID = paste0(JOIN_BG, yr)) %>%
  drop_na(hu, hu_2) %>%
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
  print()  # n = 5,912


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
  filter(hu_tdw90 > 0.9 * hu_ysb) %>%
  # organize
  select(GISJOIN10, yr, hu_tdw90) %>%
  ungroup() %>%
  print()  # n = 5,074


## clean up
rm(temp_tdw, temp_tdw1)


##################################################################################################
##################################################################################################
###  JOIN ESTIMATES TOGETHER                                                                    ##
##################################################################################################
##################################################################################################

## Bring in real values for 2010/2015
t19 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t19"
) %>%
  as_tibble() %>%
  select(GISJOIN:HU2006_) %>%
  dplyr::rename(
    `2010` = HU2006_,
    `2015` = HU2015_
    ) %>%
  pivot_longer(
    cols = `2010`:`2015`,
    names_to = "yr",
    values_to = "hu_real"
  ) %>%
  mutate(yr = as.integer(yr)) %>%
  print()


## JOIN ALL
hu4080_prep <- dr_cmr %>%  # MAX. REABS. & SPARSE
  # AREAL WEIGHTING
  left_join(dr_aw, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # TARGET-DENSITY WEIGHTING - SINGLE DECADE
  left_join(dr_tdw, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # TARGET-DENSITY WEIGHTING - 1990
  left_join(dr_tdw90, by = c("GISJOIN" = "GISJOIN10", "yr")) %>%
  # change col names
  rename(
    hu_cmr = hu_mr2,
    hu_ham = ham
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
    hu_est_ham = ifelse(is.na(hu_est_ham), 0, hu_est_ham),  # replace NAs
    hu_est_cmr = ifelse(method == "CSO", hu_est_ham, hu_est_cmr)  # fix CSO cases
  ) %>%
  # Organize
  rename(GISJOIN10 = GISJOIN) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 10,968


##-----------------------------------------------------------------------------------------
## MAKE ADJUSTMENTS to SUSPICIOUS CASES (hu_est_cmr < 0.9 * hu_ysb)
##-----------------------------------------------------------------------------------------
hu4080 <- hu4080_prep %>%
  # cases where the HU estimate less than 90% of the YSB value
  mutate(flag = ifelse(hu_est_cmr < 0.9 * hu_ysb, 1, 0)) %>%
  # fix up CMR cases
  mutate(
    hu_new = 
      case_when(
        flag == 1 & method == "CMR" & abs(hu_mr - hu_ysb) < abs(hu_ham - hu_ysb) ~ hu_mr,  # add in MR estimates when it is closer to YSB than HM
        flag == 1 & method == "CMR" & abs(hu_mr - hu_ysb) >= abs(hu_ham - hu_ysb) ~ hu_ham,   # add in HM estmates when is is closer to YSB than MR 
        TRUE ~ hu_est_cmr
      )
  ) %>%
  ## Reallocate CMR values in counties w/ flag changes above: start by getting difference in new HU and old HU
  mutate(diff = hu_new - hu_est_cmr) %>%
  # group by county and year to get total tracts in need of adjustment
  group_by(CO_CALC, yr) %>%
  mutate(
    diff_tot = sum(diff),  # get sum of differences by county & year
    n = n(),  # get total tract number by county and year
    tracts = length(subset(n, method == "CMR" & flag != 1))  # get eligible tract total in need of readjustment (CMR & not flagged)
  ) %>%
  ungroup() %>%
  mutate(
    adj = ifelse(flag != 1 & method == "CMR", diff_tot/tracts, 0),  # calculate adjustment factor for eligible tract-years
    hu_new2 = hu_new - adj,  # make adjustment
    hu_new3 = ifelse(hu_new2 < hu_ysb & flag != 1 & method == "CMR", hu_est_cmr, hu_new2)  # override cases where adjustment goes too far (puts new HU < YSB)
  ) %>%
  ## Clean up
  mutate(hu_est_cmr = hu_new3) %>%
  # change method names
  mutate(
    method = 
      case_when(
        flag == 1 & method == "CMR" & abs(hu_mr - hu_ysb) < abs(hu_ham - hu_ysb) ~ "UMR",  # UMR: Un-adjusted Max. Reabsorption.
        flag == 1 & method == "CMR" & abs(hu_mr - hu_ysb) >= abs(hu_ham - hu_ysb) ~ "CHM",  # County-based Hammer method
        TRUE ~ method
      )
  ) %>%
  select(STATE:hu_est_ham) %>%
  print()


## CHECK DISTRIBUTION of METHODS
hu4080 %>%
  group_by(method) %>%
  count() %>%
  arrange(-n) 


## SAVE OUT
write_csv(hu4080, "output/hu4080.csv")

##################################################################################################
##################################################################################################
###  ADJUST MR ESTIMATES BASED on VECTOR VALUES                                                 ##
##################################################################################################
##################################################################################################

##-----------------------------------------------------------------------------
## COMBINE 1990-2019 DATA w/ 1940-80 DATA & FILL in MISSING VALUES
##-----------------------------------------------------------------------------
hu4019_prep <- hu4080 %>%
  mutate(hu_est = hu_est_cmr) %>%
  print()

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
#reduce dataset to only tracts needing TS imputation
 #filter(ts_impute > 0) %>%
 ungroup() %>%
#replace NAs w/ zeros after 1990 --> needed for next step
 mutate(
   hu_ts = ifelse(yr >= 2010 & is.na(hu_ts), 0, hu_ts),
   hu_ts_ham = ifelse(yr >= 2010 & is.na(hu_ts_ham), 0, hu_ts_ham)
 ) %>%
 arrange(GISJOIN10, yr) %>%
 print()


##################################################################################################
##################################################################################################
###  JOIN ts_prep2 BACK w/ hu4019_prep and SAVE                                                 ##
##################################################################################################
##################################################################################################

## PREP
hu4019_prep2 <- ts_prep %>%
  rename(hu_est_tmr = hu_est2, hu_est_tham = hu_est2_ham) %>%
  select(STATE, COUNTY, GISJOIN10, yr, method, hu_cmr, hu_est_cmr, hu_est_ham, hu_est_tmr, hu_est_tham, hu_ysb, hu_ham, hu_mr) %>%
  # fix NAs for 2010
  mutate_at(vars(hu_ysb:hu_mr), funs(ifelse(is.na(.), hu_est_cmr, .))) %>%
  # get CMR/TMR diff from hu_ham
  mutate(
    cmr_diff = abs(hu_est_cmr - hu_ham),
    tmr_diff = abs(hu_est_tmr - hu_ham),
    hu_est = ifelse(method == "CMR" & cmr_diff > tmr_diff, hu_est_tmr, hu_est_cmr)
    ) %>%
  select(-cmr_diff, -tmr_diff) %>%
  # no 
  mutate(
    ham_diff = abs(hu_est_ham - hu_cmr),
    tham_diff = abs(hu_est_tham - hu_cmr),
    hu_est_h = ifelse(method == "CMR" & ham_diff > tham_diff, hu_est_tham, hu_est_ham)
  ) %>%
  #select(-ham_diff, -tham_diff) %>%
  print()


## Bring in and join comparison data
compare_real <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t19"
) %>%
  as_tibble() %>%
  rename(
    hu19_real = HU2015_,
    hu10_real = HU2006_,
    hu00_real = h2000_1,
    hu90_real = h1990_1,
  ) %>%
  select(GISJOIN, hu90_real, hu00_real, hu10_real, hu19_real) %>% 
  pivot_longer(
    cols = hu90_real:hu19_real,
    names_to = "yr",
    values_to = "HU_REAL"
  ) %>%
  mutate(
    yr = ifelse(yr == "hu90_real", 1990,
                ifelse(yr == "hu00_real", 2000, 
                       ifelse(yr == "hu10_real", 2010, 2019)))
  ) %>%
  distinct() %>%
  print()


## ORGANIZE
hu4019 <- hu4019_prep2 %>%
  #left_join(compare_ham, by = c("GISJOIN10" = "GISJOIN", "yr")) %>%
  left_join(compare_real, by = c("GISJOIN10" = "GISJOIN","yr")) %>%
  rename(YEAR = yr) %>%
  rename_all(toupper) %>%
  mutate(METHOD = ifelse(YEAR %in% c(2010, 2015), "NHGIS", METHOD)) %>%
  mutate(YEAR = ifelse(YEAR == 2015, 2019, YEAR)) %>%
  mutate(HU_REAL = ifelse(is.na(HU_REAL) & YEAR == 2019, HU_EST, HU_REAL)) %>%
  print()


##################################################################################################
##################################################################################################
###  ADD FIPS CODES, ORGANIZE, AND SAVE                                                         ##
##################################################################################################
##################################################################################################

##------------------------------------------------------
## ADD STATE/COUNTY NAMES 
##------------------------------------------------------
state_county <- fips_codes %>%
  mutate(
    COUNTY = str_replace(county, " County", ""),
    CO_JOIN = paste0(
      "G",
      state_code,
      "0",
      county_code
    )
    ) %>%
  rename(STATE = state) %>%
  select(STATE, COUNTY, CO_JOIN) %>%
  as_tibble() %>%
  print()


## finalize validation table
valid_90_10 <- hu4019[-c(1:2)] %>%
  mutate(CO_JOIN = str_sub(GISJOIN10, 1, 7)) %>%
  left_join(state_county, by = "CO_JOIN") %>%
  select(STATE, COUNTY, GISJOIN10, YEAR:HU_REAL) %>%
  mutate(
    TYPE = 
      case_when(
        STATE %in% c("LA", "MO", "MI") ~ "D",  # decline
        STATE %in% c("FL", "CA", "TX") ~ "G",  # growth
        STATE %in% c("NJ", "OH", "PA") ~ "S"  # stable
        )
  ) %>%
  drop_na(HU_CMR) %>%
  print()


##------------------------------------------------------
## SAVE OUT !!
##------------------------------------------------------
write_csv(valid_90_10, "output/valid_90_10.csv")
write_csv(hu4080, "output/hu4080.csv")
write_csv(hu4019, "output/hu4019.csv")

## Clear workspace
rm(list = setdiff(ls(), c("hu4019", "hu4080")))


##################################################################################################
##################################################################################################
###  CREATE VALIDATION TABLES                                                                   ##
##################################################################################################
##################################################################################################

# compare 1990 results and by type (G, D, S) (3) and then total by year (2) and overall (1) --> nine total
valid <- read_csv("output/valid_90_10.csv") %>%
  print()


## Preparing validation tables
test <- valid %>%
  select(-METHOD) %>%
  pivot_longer(
    cols = HU_CMR:HU_EST_H,
     names_to = "method",
     values_to = "hu_est"
   ) %>%
  select(STATE:GISJOIN10, YEAR, TYPE, method, hu_est, HU_REAL) %>%
  rename(actual = HU_REAL) %>%
  mutate(
    hu_est = ifelse(is.na(hu_est), 0, hu_est),
    diff = abs(actual - hu_est),
    pdiff = abs(actual - hu_est) / actual,
    pdiff = ifelse(is.nan(pdiff), 0, pdiff),
    smape_diff = abs(hu_est - actual) / ((hu_est + actual) / 2),
    smape_diff = ifelse(is.nan(smape_diff), 0, smape_diff)
    ) %>%
  ## keep only 1990
  filter(YEAR == 1990) %>%
  print()


## by growth type
df <- test %>%
  group_by(YEAR, TYPE, method) %>%
  summarize(
    MdAPE = median(pdiff, na.rm = T),
    sMAPE= mean(smape_diff, na.rm = T),
    total = n()
  ) %>%
  print()


# by total
df_tot <- test %>%
  group_by(YEAR, method) %>%
  summarize(
    MdAPE = median(pdiff, na.rm = T),
    sMAPE= mean(smape_diff, na.rm = T),
    total = n()
  ) %>%
print()


## Combine
df_fin <- bind_rows(df, df_tot) %>%
  filter(YEAR < 2010) %>%
  mutate(TYPE = ifelse(is.na(TYPE), "T", TYPE)) %>%
  filter(
    method %in% c(
      "HU_EST", 
      "HU_EST_H",
      "HU_CMR",
      "HU_HAM"
      )
    ) %>%
  # reorder
  mutate(method = factor(method, c("HU_EST", "HU_EST_H", "HU_CMR", "HU_HAM"))) %>%
  arrange(YEAR, TYPE, method) %>%
  print()


## SAVE OUT!!
write_csv(df_fin, "output/df_fin.csv")


