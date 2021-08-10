
#########################################################################
#########################################################################
###                                                                   ###
###                     URBANIZATION PROCESS                          ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

###############################################################################
##  STEP 1: LOAD HU AND NLCD & SQMI DATA                                     ##
###############################################################################

##--------------------------
## HU DATA
##--------------------------

hu4019 <- read_csv("output/hu4019.csv") %>%
  print()

hu4019_wide <- read_csv("output/hu4019_wide.csv") %>%
  # remove extra columns
  select(GISJOIN10:hu19) %>%
  print()

##--------------------------
## NLCD & SQMI DATA
##--------------------------

t10_join <- sf::st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10_join"
) %>%
  as_tibble() %>%
  print()

# FIX
t10 <- t10_join %>%
  select(GISJOIN, sqmi40:sqmi19, pdev92, pdev01, pdev11) %>%
  # Change NAs to Zeroes
  mutate_at(vars(sqmi40:ncol(.)), funs(ifelse(is.na(.), 0, .))) %>%
  rename(GISJOIN10 = GISJOIN) %>%
  print()


###############################################################################
##  STEP 2: CALC HU / SQ. MI.                                                ##
###############################################################################

## MAKE HU long
hu <- left_join(hu4019_wide, t10, by = "GISJOIN10") %>%
  select(GISJOIN10:hu19) %>%
  pivot_longer(
    cols = hu40:hu19,
    names_to = "yr",
    values_to = "hu"
  ) %>%
  mutate(yr = str_extract(yr, "\\d+")) %>%
  print()

  
## MAKE SQMI LONG
sqmi <- left_join(hu4019_wide, t10, by = "GISJOIN10") %>%
  select(GISJOIN10, sqmi40:sqmi19) %>%
  pivot_longer(
    cols = sqmi40:sqmi19,
    names_to = "yr",
    values_to = "sqmi"
  ) %>%
  mutate(yr = str_extract(yr, "\\d+")) %>%
  print()

## GET HU/SQMI
hu_sqmi <- left_join(hu, sqmi, by = c("GISJOIN10", "yr")) %>%
  mutate(hu_sqmi = hu/sqmi) %>%
  select(-hu, -sqmi) %>%
  #mutate(URB = ifelse(hu_sqmi >= 200, 1, 0)) %>%
  pivot_wider(
    names_from = yr,
    names_prefix = "hu_sqmi",
    names_sep = "",
    values_from = hu_sqmi
  ) %>%
  print()


###############################################################################
##  STEP 3: JOIN BACK TOGETHER AND ESTIMATE UY1                              ##
###############################################################################

urb1 <- hu4019_wide %>%
  # Join to get sqmi & pdev
  left_join(t10, by = "GISJOIN10") %>%
  # join to get hu/sqmi
  left_join(hu_sqmi, by = "GISJOIN10") %>%
  # organize
  select(GISJOIN10:sqmi19, hu_sqmi40:hu_sqmi19, pdev92:pdev11) %>%
  # get UY1
  mutate(
    UY1 = 
      case_when(
        # using 200 HU/Sq. mi. as cutoff
        hu_sqmi40 >= 200 ~ 1940,
        hu_sqmi50 >= 200 ~ 1950,
        hu_sqmi60 >= 200 ~ 1960,
        hu_sqmi70 >= 200 ~ 1970,
        hu_sqmi80 >= 200 ~ 1980,
        hu_sqmi90 >= 200 ~ 1990,
        hu_sqmi00 >= 200 ~ 2000,
        hu_sqmi10 >= 200 ~ 2010,
        hu_sqmi19 >= 200 ~ 2019,
        TRUE ~ 2035  # "non-urban"
      )
  ) %>%
  print()

## GET Distribution
urb1 %>%
  group_by(UY1) %>%
  count()
  

## Clean up
rm(list = setdiff(ls(), c("urb1", "packages")))


###############################################################################
##  STEP 4: GENERATE UY2 --> Account for Non-Res Urban Space                 ##
###############################################################################

## BRING in NEIGHBORS FILE
neighbors <- read_csv("tables/neighbors.csv") %>%
  print()

## custom function for round_any
round_any <- function(x, accuracy, f=round){f(x / accuracy) * accuracy}

## GRAB ONLY NEEDED VARS
u <- urb1 %>%
  select(GISJOIN10, pdev92, UY1) %>%
  print()

## 1992 NLCD
nlcd <- u %>%
  full_join(neighbors, by = c("GISJOIN10" = "src_GISJOIN")) %>%
  filter(pdev92 >= 0.5 & UY1 > 1990) %>%
  # get rid of overlapping candidates --> messes up calculations
  filter(!nbr_GISJOIN %in% GISJOIN10) %>%
  # Join u92 back in but drop pdev
  left_join(u[-2], by = c("nbr_GISJOIN" = "GISJOIN10"), suffix = c("", "_nbr")) %>%
  # Get weighted mean for UY neighbor
  group_by(GISJOIN10) %>%
  summarize(uy_est = weighted.mean(UY1_nbr, WEIGHT)) %>%
  ## Establish UY
  mutate(
    UY2 =
      case_when(
        uy_est > 1990 ~ 1990,  # anything above 2019 goes to 2035
        #between(uy_est, 2015, 2019) ~ 2019, ## set 2019
        TRUE ~ round_any(uy_est, 10)  # round remaining to nearest 10
      )
    ) %>%
  select(-uy_est) %>%
  print()

## Join back in w/ group
urb2 <- urb1 %>%
  left_join(nlcd, by = "GISJOIN10") %>%
  mutate(UY2 = ifelse(is.na(UY2), UY1, UY2)) %>%
  print()


###############################################################################
##  STEP 5: GENERATE UY3 --> Use 2001 NLCD data                              ##
###############################################################################

## Set 2001 NLCD
u <- urb2 %>%
  select(GISJOIN10, pdev01, UY2) %>%
  print()


## 2001 NLCD
nlcd <- u %>%
  full_join(neighbors, by = c("GISJOIN10" = "src_GISJOIN")) %>%
  filter(pdev01 >= 0.5 & UY2 > 2000) %>%
  # get rid of overlapping candidates --> messes up calculations
  filter(!nbr_GISJOIN %in% GISJOIN10) %>%
  # Join u92 back in but drop pdev
  left_join(u[-2], by = c("nbr_GISJOIN" = "GISJOIN10"), suffix = c("", "_nbr")) %>%
  # Get weighted mean for UY neighbor
  group_by(GISJOIN10) %>%
  summarize(uy_est = weighted.mean(UY2_nbr, WEIGHT)) %>%
  ## Establish UY
  mutate(
    UY3 =
      case_when(
        uy_est > 2000 ~ 2000,  # anything above 2019 goes to 2035
        #between(uy_est, 2015, 2019) ~ 2019, ## set 2019
        TRUE ~ round_any(uy_est, 10)  # round remaining to nearest 10
      )
  ) %>%
  select(-uy_est) %>%
  print()


## Join back in w/ group
urb3 <- urb2 %>%
  left_join(nlcd, by = "GISJOIN10") %>%
  mutate(UY3 = ifelse(is.na(UY3), UY2, UY3)) %>%
  print()


###############################################################################
##  STEP 6: GENERATE UY4 --> Use 2011 NLCD data                              ##
###############################################################################

## Set 2011 NLCD
u <- urb3 %>%
  select(GISJOIN10, pdev11, UY3) %>%
  print()


## 2011 NLCD
nlcd <- u %>%
  full_join(neighbors, by = c("GISJOIN10" = "src_GISJOIN")) %>%
  filter(pdev11 >= 0.5 & UY3 > 2010) %>%
  # get rid of overlapping candidates --> messes up calculations
  filter(!nbr_GISJOIN %in% GISJOIN10) %>%
  # Join u92 back in but drop pdev
  left_join(u[-2], by = c("nbr_GISJOIN" = "GISJOIN10"), suffix = c("", "_nbr")) %>%
  # Get weighted mean for UY neighbor
  group_by(GISJOIN10) %>%
  summarize(uy_est = weighted.mean(UY3_nbr, WEIGHT)) %>%
  ## Establish UY
  mutate(
    UY4 =
      case_when(
        uy_est > 2010 ~ 2010,  # anything above 2019 goes to 2035
        #between(uy_est, 2015, 2019) ~ 2019, ## set 2019
        TRUE ~ round_any(uy_est, 10)  # round remaining to nearest 10
      )
  ) %>%
  select(-uy_est) %>%
  print()


## Join back in w/ group
urb4 <- urb3 %>%
  left_join(nlcd, by = "GISJOIN10") %>%
  mutate(UY4 = ifelse(is.na(UY4), UY3, UY4)) %>%
  print()

# Check dist
urb4 %>%
  group_by(UY4) %>%
  count()


###############################################################################
##  STEP 7: GENERATE UY5 --> SMOOTH OUT UY4                                  ##
###############################################################################

u <- urb4 %>%
  select(GISJOIN10, UY4) %>%
  print()

## SMOOTHING
smooth <- u %>%
  full_join(neighbors, by = c("GISJOIN10" = "src_GISJOIN")) %>%
  left_join(u, by = c("nbr_GISJOIN" = "GISJOIN10"), suffix = c("", "_nbr")) %>%
  # ID tracts surrounded by tracts w/ lower UY4s
  group_by(GISJOIN10) %>%
  mutate(UY4_nbr_hi = max(UY4_nbr)) %>%
  # Keep only those
  filter(UY4 > UY4_nbr_hi) %>%
  # Generate UY4_nbr mean
  summarize(uy_est = weighted.mean(UY4_nbr, WEIGHT)) %>%
  # Establish UY
  mutate(
    UY5 =
      case_when(
        uy_est > 2019 ~ 2035,  # anything above 2019 goes to 2035
        between(uy_est, 2015, 2019) ~ 2019, ## set 2019
        TRUE ~ round_any(uy_est, 10)  # round remaining to nearest 10
      )
  ) %>%
  select(-uy_est) %>%
  print()  # n = 731


## Join back in w/ group
urb5 <- urb4 %>%
  left_join(smooth, by = "GISJOIN10") %>%
  # create new, final UY column
  mutate(UY5 = ifelse(is.na(UY5), UY4, UY5)) %>%
  # Create only UY1 & UY2 (from UY5)
  mutate(UY2 = UY5) %>%
  # Clean
  select(-c(UY3:UY5)) %>%
  # manually repair two NYC islands (after manually inspecting 9 candidate islands across US [pdev > 0.5 & UY2 > 1940])
  mutate(
    # 1940 based on surrounding tracts (same as smoothing process above)
    UY2 = ifelse(GISJOIN10 %in% c("G3600610024000", "G3600050000100"), 1940, UY2)
    ) %>%
  print()


##-----------------------------------------------------------
##  SAVE OUT
##-----------------------------------------------------------

write_csv(urb5, "output/HHUUD_prep.csv")

