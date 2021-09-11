#########################################################################
#########################################################################
###                                                                   ###
###           Running Validation: Compare tp AW & TDW                 ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

##############################################################################
##############################################################################
##  STEP 1: BRING in HAMMER DATA from TAYLOR                                ##
##############################################################################
##############################################################################


## Bring in real hammer data
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
  arrange(GISJOIN, yr) %>%
  distinct() %>%
  print()



##############################################################################
##############################################################################
##  LOAD & ORGANIZE INTERSECTION DATA (AW, TDW, TDW90)                      ##
##############################################################################
##############################################################################

##---------------------------------------------------------
## GRAB / ORGANIZE INTERSECTION SHAPEFILES
##---------------------------------------------------------

## 1990
df_int <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "int_full_90"
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
  arrange(yr, JOIN_T) %>%
  print()


##------------------------------------------------------------
## FILTER OUT TRACT-YEARS W/ COVERAGE < 0.99
##------------------------------------------------------------

dr_aw <- temp_aw %>%
  filter(pcover_t > 0.99 & pcover_s > 0.99) %>%
  # rename to match previous
  rename(GISJOIN10 = JOIN_T)  %>%
  # organize columns/remove pcover
  select(GISJOIN10, yr, hu_aw) %>%
  arrange(GISJOIN10, yr) %>%
  print()


## AW for comparison
dr_aw_comp <- temp_aw %>%
  # rename to match previous
  rename(GISJOIN10 = JOIN_T)  %>%
  # organize columns/remove pcover
  select(GISJOIN10, yr, hu_aw) %>%
  arrange(GISJOIN10, yr) %>%
  print()


## Clean up
rm(temp_aw)


########################################################################################
########################################################################################
##  TARGET DENSITY WEIGHTING (TDW)-- 1 DECADE FORWARD                                 ##
########################################################################################
########################################################################################

##----------------------------------------------------------------------------------------------------
## TDW: CALC. HU ESTIMATES in PSEUDO-TARGET (pt) TRACTS (source [s] tracts + 10 years)
##----------------------------------------------------------------------------------------------------

temp_tdw_pt <- df_int %>%
  # get unique ID for filter
  mutate(ID = paste0(JOIN_BG, yr)) %>%
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
  print() 


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
  # clean up
  select(GISJOIN10, yr, hu_tdw) %>%
  print()


## Clean up
rm(temp_tdw_pt, flag_tdw)


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
  print()


##----------------------------------------------------------------
## FIX SPECIAL CASES and GENERATE FINAL OUTPUT
##----------------------------------------------------------------

dr_tdw90 <- temp_tdw1 %>%
  # fix up names and columns
  rename(GISJOIN10 = JOIN_T) %>%
  # organize
  select(GISJOIN10, yr, hu_tdw90) %>%
  ungroup() %>%
  print()  

## clean up
rm(temp_tdw, temp_tdw1, flag_tdw90)


##################################################################################################
##################################################################################################
###  JOIN ESTIMATES TOGETHER                                                                    ##
##################################################################################################
##################################################################################################

## JOIN ALL
hu4080 <- dr_aw %>%  # MAX. REABS. & SPARSE
  # AREAL WEIGHTING
  full_join(dr_aw_comp, by = c("GISJOIN10", "yr"), suffix = c("", "_comp")) %>%
  # TARGET-DENSITY WEIGHTING - SINGLE DECADE
  full_join(dr_tdw, by = c("GISJOIN10", "yr")) %>%
  # TARGET-DENSITY WEIGHTING - 1990
  full_join(dr_tdw90, by = c("GISJOIN10", "yr")) %>%
  arrange(GISJOIN10, yr) %>%
  ## fix
  mutate(hu_tdw90 = ifelse(is.nan(hu_tdw90), 0, hu_tdw90)) %>%
  # Name methods
  mutate(
    method = 
      case_when(
        !is.na(hu_aw) ~ "AW",
        is.na(hu_aw) & !is.na(hu_tdw) ~ "TDW-1",
        is.na(hu_aw) & is.na(hu_tdw) & !is.na(hu_tdw90) ~ "TDW-90",
    )
  ) %>%
  # Choose HU estimation method for CMR
  mutate(
    hu_est = case_when(
      method == "AW" ~ hu_aw,
      method == "TDW-1" ~ hu_tdw,
      method == "TDW-90" ~ hu_tdw90
    )
  ) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 362,695


## CHECK DISTRIBUTION of METHODS
hu4080 %>%
  group_by(method) %>%
  count() %>%
  arrange(-n)


## SAVE OUT
write_csv(hu4080, "output/hu4080_vect.csv")


##################################################################################################
##################################################################################################
###  Bring in Comparison Data                                                                   ##
##################################################################################################
##################################################################################################

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
  print()


## missing tract
missing_t <-  data.frame(
  GISJOIN10 = "G1200950990000",
  YEAR = c(1990, 2000)
  ) %>%
  print()


## Join comparison data to test data
hu4019 <- hu4080 %>%
  left_join(compare_real, by = c("GISJOIN10" = "GISJOIN","yr")) %>%
  rename(YEAR = yr) %>%
  rename_all(toupper) %>%
  # add in missing tracts
  bind_rows(missing_t) %>%
  mutate_at(vars(HU_AW:HU_TDW90, HU_EST, HU_REAL), funs(ifelse(GISJOIN10 == "G1200950990000", 0, .))) %>%
  distinct() %>%
  print()


##################################################################################################
##################################################################################################
## Bring in Fips codes and Finalize Validation Table                                            ##
##################################################################################################
##################################################################################################

##------------------------------------------------------
## ADD STATE/COUNTY NAMES 
##------------------------------------------------------

# fips codes from tidycensus package
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
valid_90_10 <- hu4019 %>%
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
  # removes duplicate
  drop_na(METHOD) %>%
  print()

##------------------------------------------------------
## SAVE OUT !!
##------------------------------------------------------
write_csv(valid_90_10, "output/valid_90_10_vect.csv")

## Clear workspace
rm(list = setdiff(ls(), c("hu4019", "hu4080")))


##################################################################################################
##################################################################################################
## Create Validation Tables                                                                     ##
##################################################################################################
##################################################################################################

# compare by year (1990, 2000) (2) and by type (G, D, S) (3) and then total by year (2) and overall (1) --> nine total
valid <- read_csv("output/valid_90_10_vect.csv") %>%
  print()

## Prepare
test <- valid %>%
  select(-METHOD) %>%
  pivot_longer(
    cols = HU_AW_COMP:HU_EST,
    names_to = "method",
    values_to = "hu_est"
  ) %>%
  select(STATE:GISJOIN10, YEAR, TYPE, method, hu_est, HU_REAL) %>%
  rename(actual = HU_REAL) %>%
  mutate(
    hu_est = ifelse(is.na(hu_est), 0, hu_est),
    diff = abs(actual - hu_est),
    pdiff = abs(actual - hu_est) / actual,
    smape_diff = abs(hu_est - actual) / ((hu_est + actual) / 2)
  ) %>%
  print()


## by growth type
df <- test %>%
  filter(YEAR == 1990) %>%
  group_by(TYPE, method) %>%
  summarize(
    MdAPE = median(pdiff, na.rm = T),
    sMAPE= mean(smape_diff, na.rm = T),
    total = n()
  ) %>%
print()


# Prepare table by method
df_tot <- test %>%
  filter(YEAR == 1990) %>%
  group_by(method) %>%
  summarize(
    MdAPE = median(pdiff, na.rm = T),
    sMAPE= mean(smape_diff, na.rm = T),
    total = n()
  ) %>%
mutate(TYPE = "T") %>%
print()


## Organize final table
df_fin <- bind_rows(df, df_tot) %>%
  mutate(
    method =
      case_when(
        method == "HU_AW_COMP" ~ "DR_AW",
        method == "HU_TDW90" ~ "DR_TDW",
        TRUE ~ method
      )
  ) %>%
  filter(method %in% c("DR_AW", "DR_TDW", "HU_EST")) %>%
  mutate(method = factor(method, c("HU_EST", "DR_AW", "DR_TDW"))) %>%
  arrange(TYPE, method) %>%
  print()


## SAVE OUT!!
write_csv(df_fin, "output/df_fin_vect.csv")




