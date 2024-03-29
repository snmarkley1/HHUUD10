
#########################################################################
#########################################################################
###                                                                   ###
###                     FINALIZING DATA PRODUCT                       ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Create DATA_DOWNLOAD folder
dir.create("DATA_DOWNLOAD")

## for exporting a SAS file
packages(haven)


###############################################################################
##  STEP 1: LOAD & CLEAN DATA                                                ##
###############################################################################

## LOAD PREP DATA
df <- read_csv("output/HHUUD_prep.csv") %>%
  print()

## LOAD AND PREP TIDYVERSE COUNTY & STATE NAMES
state_county <- fips_codes %>%
  as_tibble() %>%
  # create joinable county code
  mutate(JOIN_CO = paste0(state_code, county_code)) %>%
  # organize
  select(state, county, JOIN_CO) %>%
  # keep only lower 48
  filter(!state %in% c("AK", "AS", "GU", "HI", "MP", "PR", "UM", "VI")) %>%
  print()


###############################################################################
##  STEP 2: JOIN & ADD CENSUS TRACTID                                        ##
###############################################################################

hhuud <- df %>%
  mutate(
    # add Census ID
    GEOID10 =
      paste0(
        str_sub(GISJOIN10, 2, 3),
        str_sub(GISJOIN10, 5, 7),
        str_sub(GISJOIN10, 9, 14)
      ),
    # add County ID
    JOIN_CO = str_sub(GEOID10, 1, 5)
  ) %>%
  # Get state & county names
  left_join(state_county, by = "JOIN_CO") %>%
  # fix names
  rename(
    STATE = state,
    COUNTY = county
  ) %>%
  # remove "County"
  mutate(COUNTY = str_replace(COUNTY, " County", "")) %>%
  # reorder
  select(STATE, COUNTY, GISJOIN10, GEOID10, hu40:sqmi19, pdev92:UY2) %>%
  print()

## SAVE!!
write.dbf(as.data.frame(hhuud), "output/HHUUD.dbf")  # dbf for shapefile
write_csv(hhuud, "DATA_DOWNLOAD/HHUUD10_wide.csv")  # csv
write.dta(hhuud, "DATA_DOWNLOAD/HHUUD10_wide.dta")  # dta (STATA)
write_xpt(hhuud, "DATA_DOWNLOAD/HHUUD10_wide.xpt")  # xpt (SAS)
write_xpt(hhuud, "DATA_DOWNLOAD/HHUUD10_wide.v8xpt") # v8xpt (SAS)


###############################################################################
##  STEP 3: MAKE LONG                                                        ##
###############################################################################

hhuud_long <- hhuud %>%
  # Make HU long
  pivot_longer(
    cols = hu40:hu19,
    names_to = "YEAR",
    values_to = "HU"
  ) %>%
  # Make Sq. mi. long
  pivot_longer(
     cols = sqmi40:sqmi19,
     names_to = "YEAR_SQMI",
     values_to = "SQMI"
   ) %>%
  # organize
  select(STATE:GEOID10, YEAR, HU, SQMI, pdev92:UY2) %>%
  # make distinct column to drop duplicates
  mutate(ID = paste0(GISJOIN10, YEAR)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  select(-ID) %>%
  # fix YEAR column
  mutate(
    YEAR = as.integer(str_extract(YEAR, "\\d+")),
    YEAR = ifelse(between(YEAR, 40, 90), as.integer(paste0("19", YEAR)), as.integer(paste0("20", str_pad(YEAR, 2, "left", "0"))))
    ) %>%
  print()

## SAVE!!
write_csv(hhuud_long, "DATA_DOWNLOAD/HHUUD10_long.csv")  # csv
write.dta(hhuud_long, "DATA_DOWNLOAD/HHUUD10_long.dta")  # dta (STATA)
write_xpt(hhuud_long, "DATA_DOWNLOAD/HHUUD10_long.xpt")  # xpt (SAS)
write_xpt(hhuud_long, "DATA_DOWNLOAD/HHUUD10_long.v8xpt") # v8xpt (SAS)


###############################################################################
##  STEP 4: PREPARE HAMMER DATA for COMPARISON                               ##
###############################################################################

## load t10 to get sqmi
t10_sqmi <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10"
) %>%
  as_tibble() %>%
  # keep only necessary columns
  select(GISJOIN, sqmi) %>%
  print()

## Hammer prep
hammer <- read_csv("output/hu4019_wide.csv") %>%
  # organize
  select(GISJOIN10, ham40:ham80, hu90:hu19) %>%
  # add HU values from Hammer method
  left_join(hhuud[c(3,14:22)], by = "GISJOIN10") %>%
  # add 2010 tract sqmi
  left_join(t10_sqmi, by = c("GISJOIN10" = "GISJOIN")) %>%
  mutate(
    # urbanization by Hammer method using 2010 tract areas (sq. mi.)
    UY_HAM =
      case_when(
        ham40/sqmi > 200 ~ 1940,
        ham50/sqmi > 200 ~ 1950,
        ham60/sqmi > 200 ~ 1960,
        ham70/sqmi > 200 ~ 1970,
        ham80/sqmi > 200 ~ 1980,
        hu90/sqmi > 200 ~ 1990,
        hu00/sqmi > 200 ~ 2000,
        hu10/sqmi > 200 ~ 2010,
        hu19/sqmi > 200 ~ 2019,
        TRUE ~ 2035
      ),
    # urbanization by Hammer method using dasymetrically refined tract areas (sq. mi.)
    UY_HAM_DR = 
      case_when(
        ham40/sqmi40 > 200 ~ 1940,
        ham50/sqmi50 > 200 ~ 1950,
        ham60/sqmi60 > 200 ~ 1960,
        ham70/sqmi70 > 200 ~ 1970,
        ham80/sqmi80 > 200 ~ 1980,
        hu90/sqmi90 > 200 ~ 1990,
        hu00/sqmi00 > 200 ~ 2000,
        hu10/sqmi10 > 200 ~ 2010,
        hu19/sqmi19 > 200 ~ 2019,
        TRUE ~ 2035
      )
  ) %>%
  print()

## SAVE
write.dbf(as.data.frame(hammer), "output/hammer.dbf")

