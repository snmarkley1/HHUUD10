

##########################################################
###        INTERSECTION ORGANIZING/CALCULATING         ###
###         VECTOR-BASED AREAL INTERPOLATION           ###
##########################################################

## quick accounting
df1 %>%
  group_by(GISJOIN, yr) %>%
  summarize(n = n()) %>%
  group_by(yr) %>%
  summarize(n = n())

## ~~TRACT-YEAR COuNTS~~##
## 1940: 12,668
## 1950: 18,859
## 1960: 41,964
## 1970: 53,953
## 1980: 65,124

#### VECTOR-BASED INTERPOLATION ####
df2 <- df1 %>%
  mutate(
    pcover10 = sqmi_int / sqmi, ## calculate overlap proportion of 2010 tracts
    pcover_1 = sqmi_int / sqmi_1, ## calculate overlap proportion of old tracts
    hu_est = hu * pcover_1
  ) %>%
  ## calculate housing unit fraction based on old tract overlap proportion
  group_by(GISJOIN, yr, sqmi) %>%
  ## group by 2010 tract boundaries; sum estimates
  summarize(
    sqmi_1 = sum(sqmi_1),
    pcover10 = sum(pcover10),
    pcover_1 = sum(pcover_1),
    hu_sum = sum(hu),
    hu_est = sum(hu_est),
    tracts = n()
  ) %>%
  select(-sqmi_1) %>%
  arrange(yr, GISJOIN)

df2 ## n = 192,568

## test threshold with histogram of pcover10
df2 %>%
  filter(between(pcover10, 0.75, 0.99)) %>%
  ggplot(aes(x = pcover10)) +
  geom_histogram(binwidth = 0.01) +
  stat_bin(
    binwidth = 0.01, geom = "text", color = "white", aes(label = ..x..),
    position = position_stack(vjust = 0.9)
  ) +
  stat_bin(
    binwidth = 0.01, geom = "text", color = "black", aes(label = ..count..),
    position = position_stack(vjust = 0.8)
  )

## settle on setting cutoff at 0.97 based on examination of histogram
df3 <- df2 %>%
  mutate(pcover10 = ifelse(pcover10 > 1, 1, pcover10)) %>%
  ## correct small rounding error
  filter(pcover10 > 0.97) %>%
  rename(hu_vect = "hu_est") %>%
  select(GISJOIN, yr, hu_vect) %>%
  ## remove errant zeroes from 1960
  mutate(
    ST = substr(GISJOIN, 1, 3),
    CO = substr(GISJOIN, 1, 8),
    flag =
      case_when(
        yr == 1960 & hu_vect < 100 & ST %in% c("G25", "G34", "G44") ~ 1, # MA, NJ, RI
        yr == 1960 & hu_vect < 100 & CO %in% c("G0601130", "G5300530", "G4900110", "G2901650", "G0900030", "G0900130") ~ 1, # CA, WA, UT, MO, CTx23
        yr == 1960 & GISJOIN %in% c("G41005100044", "G48035500028", "G48043900006", "G42010100223") ~ 1, ## OR,TX x 2, PA
        TRUE ~ 0
      )
  ) %>%
  filter(flag == 0) %>%
  select(GISJOIN, yr, hu_vect)

df3 ## n = 180,869 (more bc other one only includes tracts intersecting following year)

#####   join df3 with df_alt5 to get new vector-based estimates
vect_est <- df3 %>%
  left_join(df_alt5, by = c("GISJOIN", "yr")) %>%
  select(GISJOIN, yr, hu_vect, sqmi, hu_est, tdw) %>%
  rename(
    hu_vect1 = hu_vect, ## original estimate
    hu_vect2 = hu_est
  ) %>%
  ## corrected estimate
  mutate(hu_vect2 = ifelse(!is.na(hu_vect2) & tdw == 1, hu_vect2, hu_vect1)) ## correct missing values

vect_est ## n = 180,869 after 0 removal (135? tract-years)

##    cojoin      yr hu_vect hu_supp    n
##    <chr>    <dbl>   <dbl>   <dbl> <int>
## 1 G3600470  1940 760712. 760712.   760
## 2 G3600470  1950 779028. 779028.   760
## 3 G3600470  1960 874922. 874922.   759  (759 is ok--zero HU)
## 4 G3600470  1970 901683. 901683.   760
## 5 G3600470  1980 880870. 880870.   760



#####################################################################
#####################################################################
#### ----------------------------------------------------------- ####
####                MODIFIED GENERAL HAMMER METHOD               ####
#### ----------------------------------------------------------- ####
#####################################################################
#####################################################################

######################################################
####            BRING IN COUNTY DATA              ####
######################################################

## change working directory:
setwd("C:/Users/scott/Dropbox/urb_proj")

## Import county files (at BG level)
counties <- read.dbf("tables/counties50st.dbf") %>%
  as_tibble() %>%
  select(-c(hu40est:hu80est)) %>%
  dplyr::mutate(
    GISJOIN = substr(GISJOIN, 1, 14),
    CO_CALC = JOIN_CO,
    CO_JOIN = substr(GISJOIN, 1, 8),
    hu40co = as.numeric(as.character(hu40co))
  ) %>%
  group_by(STATE, COUNTY, CO_CALC, CO_JOIN, GISJOIN, hu40co, hu50co, hu60co, hu70co) %>%
  dplyr::summarize(hu80co = median(hu80co))

## fix it up:
co_fix <- NULL
for (i in seq(40, 80, 10)) {
  col_name <- paste0("hu", i, "co")
  
  temp <- counties %>%
    ungroup() %>%
    pivot_longer(
      cols = c(hu40co:hu80co),
      names_to = "yr",
      values_to = "hu_co"
    ) %>%
    filter(yr == col_name) %>%
    dplyr::mutate(yr = as.numeric(paste0("19", i)))
  
  co_fix <- bind_rows(co_fix, temp) %>%
    unique()
}

co_fix

##############################################################
##       JOIN TOGETHER rast_est, vect_est, and co_fix       ##
##############################################################

### set up for county-level join
co_fix_co <- co_fix %>%
  group_by(STATE, COUNTY, CO_JOIN, CO_CALC, yr) %>%
  summarize(hu_co = median(hu_co))

## check Brooklyn (Kings Co., NY)
co_fix_co %>%
  filter(CO_CALC == "G3600470")

#    STATE   COUNTY CO_JOIN  CO_CALC     yr  hu_co
#    <fct>   <fct>  <chr>    <fct>    <dbl>  <dbl>
# 1 New York Kings  G3600470 G3600470  1940 762526
# 2 New York Kings  G3600470 G3600470  1950 814134
# 3 New York Kings  G3600470 G3600470  1960 875757
# 4 New York Kings  G3600470 G3600470  1970 902622
# 5 New York Kings  G3600470 G3600470  1980 881367


######################################################
###           look at data to be joined            ###
######################################################

rast_est ## raster process estimates; n = 361,295
vect_est ## vector estimates; n = 180,870
# vect_est1  ## n = 180,870
co_fix_co ## county level data needed for hammer method; n = 15,550

## take out CO_JOIN from co_fix
co_fix <- co_fix %>%
  select(-CO_JOIN)

## join together
join_df <- rast_est %>%
  mutate(CO_JOIN = substr(GISJOIN, 1, 8)) %>%
  left_join(co_fix, by = c("GISJOIN", "yr"), suffix = c("", "_old")) %>%
  left_join(vect_est[-4], by = c("GISJOIN", "yr")) %>%
  ## remove vect_est$sqmi
  select(STATE, COUNTY, GISJOIN, CO_JOIN, CO_CALC, hu_co, sqmi, hu_vect1, hu_vect2, hu_rast, yr) %>%
  arrange(yr, GISJOIN)

join_df

## correct missing data
missing <- join_df %>%
  filter(is.na(STATE)) %>%
  left_join(co_fix_co, by = c("CO_JOIN", "yr"), suffix = c("_old", "")) %>%
  select(!ends_with("_old")) %>%
  select(STATE, COUNTY, GISJOIN, CO_JOIN, CO_CALC, hu_co, sqmi:yr)

## add missing to join_df
join_df2 <- join_df %>%
  filter(!is.na(STATE)) %>%
  bind_rows(missing) %>%
  mutate(
    hu_vect1 = ifelse(GISJOIN %in% zeroes$GISJOIN, 0, hu_vect1),
    hu_vect2 = ifelse(GISJOIN %in% zeroes$GISJOIN, 0, hu_vect2)
  )

join_df2

## save
write.dbf(as.data.frame(join_df2), "tables/join_df2.dbf")

### clean up
rm(list = setdiff(ls(), "join_df2"))

######################################################
######################################################
#### -------------------------------------------- ####
####                HAMMER TIME!!                 ####
#### -------------------------------------------- ####
######################################################
######################################################

### wd: "C:/Users/scott/Dropbox/urb_proj"
join_df2 <- read.dbf("tables/join_df2.dbf") %>%
  as_tibble() %>%
  ## fix NAs -- from tracts with no "successor"--all 1980 plus some w/ gap in tract years (e.g., Bibb Co. GA 1940-50)
  mutate(hu_vect = ifelse(is.na(hu_vect2), hu_vect1, hu_vect2)) %>%
  select(-hu_vect1, -hu_vect2) %>%
  filter(!(GISJOIN == "G0800140031100" & str_detect(COUNTY, "Adams"))) ## get rid of mistaken repeated tract (from Broomfield Co., CO)

join_df2

## hammer method: CO_CALC controls for county boundary changes over the decade
ham <- join_df2 %>%
  group_by(CO_CALC, yr) %>%
  mutate(hu_vect_sum = sum(hu_vect, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(diff_v = hu_co - hu_vect_sum)

ham ## n = 361,305; NAs in hu_vect and hu_supp indicate tract-year has no vector-polygon data

## ham1: filter out vector estimates; dealing only w/tract-years w/raster estimates
ham_rast <- ham %>%
  filter(is.na(hu_vect)) %>%
  group_by(CO_CALC, yr) %>%
  mutate(hu_rast_sum = sum(hu_rast)) %>%
  ungroup() %>%
  mutate(hu_v = hu_rast / hu_rast_sum * diff_v)

ham_rast ## n = 180,409

## ham2: include only vector estimates; dealing only w/tract-years w/vector estimates
ham_vect <- ham %>%
  filter(!is.na(hu_vect)) %>%
  group_by(CO_CALC, yr) %>%
  mutate(hu_rast_sum = sum(hu_rast)) %>%
  ungroup() %>%
  mutate(hu_v = hu_vect)

ham_vect ## n = 180,896

## ham2: bind rows and do group calculations for later steps
ham2 <- bind_rows(ham_rast, ham_vect) %>%
  group_by(CO_CALC, yr) %>%
  mutate(
    co_diff_v = hu_co - sum(hu_v), ## calc. diff. between county-equivalent HU estimates and tract HU estimates
    n_rast = sum(is.na(hu_vect)), ## count raster-interpolated tracts per county-year
    n_vect = sum(!is.na(hu_vect)), ## county vector-polygon-interpolated tracts per county-year
    n_tot = n(),
    p_rast = n_rast / n_tot, ## percent of tracts in county-year derived from raster-based estimates
    p_vect = n_vect / n_tot
  ) %>%
  ## percent of tracts in county-year derived from vector-based estimates
  arrange(yr, GISJOIN)

ham2

## save!!
write_csv(ham2, "tables/ham2.csv")

## visualize p_rast as histogram
ham2 %>%
  group_by(CO_CALC, yr, n_rast, p_rast, n_vect, n_tot) %>%
  summarize(n = n()) %>%
  filter(p_rast < 0.5 & n_tot > 10) %>%
  ggplot(aes(x = p_rast)) +
  geom_histogram(binwidth = 0.01)


## test discrepancies bw hu_v and hu_rast (catch odd tracts usually in hu_vect heavy counties)
test <- ham2 %>%
  filter(is.na(hu_vect) & n_vect >= 1) %>%
  ## those eligible for override (non-vector tract-yr & in county-yr w/vector estimates)
  mutate(
    p_diff = abs(hu_v - hu_rast) / (abs(hu_v + hu_rast) / 2) * 100,
    p_rast = n_rast / n_tot * 100
  ) %>%
  arrange(-p_diff, GISJOIN, yr) %>%
  select(STATE:hu_co, yr, n_vect, n_rast, n_tot, hu_rast, hu_v, p_diff, p_rast)

### --------------- DIAGNOSTIC SCATTERPLOT ------------------- ###
## scatterplot of error against % of counties with rasters
test %>%
  filter(p_diff < 1000) %>% ## 6 cases > 1,000 (removed for viz purposes)
  ggplot(aes(x = p_rast, y = p_diff)) +
  geom_point() +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1000, 50))

## SUMMARY: most w/ p_rast < 0.15 EXCEPT Pierce, Co., WA tracts

## create steps
steps <- data.frame(
  x = c(0, 10, 20, 30, 40, 50),
  y = c(10, 20, 30, 40, 50, 60)
)

## second scatterplot
test %>%
  filter(p_diff < 300) %>%
  ggplot(aes(x = p_rast, y = p_diff)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 300, 20)) +
  scale_y_continuous(breaks = seq(0, 300, 20)) +
  geom_step(data = steps, mapping = aes(x = x, y = y), size = 1.2, col = "gray40") +
  geom_segment(aes(x = 50, xend = 100, y = 60, yend = 60), size = 1.2, linetype = "dashed", col = "gray40") +
  theme_bw()


## give p_diff between original raster-based estimates and hammer-adjusted raster-based estimates
ham3 <- ham2 %>%
  ungroup() %>%
  mutate(p_diff = ifelse(is.na(hu_vect), abs((hu_v - hu_rast) / hu_rast * 100), 0)) %>%
  select(STATE:GISJOIN, CO_CALC, hu_co, yr, sqmi, p_rast, n_rast, n_vect, n_tot, hu_vect, hu_rast, hu_v, p_diff)

#############################################################
###                CHECK ON ODDITIES                      ###
#############################################################

# chk5 <- test %>%
#  filter(p_diff > 60 & p_rast > 50) %>% ## n = 168
#  ## remove all negative values (Pierce Co., WA): those will be automoatically replaced (n = 128)
#  filter(hu_v >= 0)  ## n = 40


############################################################################################
###   REPLACE STRANGE, INEXPLICABLE HAMMER-WEIGHTED CASES with ORIG. RASTER ESTIMATES    ###
############################################################################################

ham4 <- NULL
for (i in seq(1940, 1980, 10)) {
  temp <- ham3 %>%
    filter(yr == i) %>%
    mutate(
      hu_new =
        case_when(
          ### anything negative
          hu_v < 0 ~ 1,
          ### steps to catch most egregious cases
          is.na(hu_vect) & p_rast < .1 & p_diff > 10 ~ 1,
          is.na(hu_vect) & between(p_rast, .1, .2) & p_diff > 20 ~ 1,
          is.na(hu_vect) & between(p_rast, .2, .3) & p_diff > 30 ~ 1,
          is.na(hu_vect) & between(p_rast, .3, .4) & p_diff > 40 ~ 1,
          is.na(hu_vect) & between(p_rast, .4, .5) & p_diff > 50 ~ 1, ## stopped being relevant after 50% marker--except Pierce Co., WA
          ### get final anomalies
          is.na(hu_vect) & p_diff > 10 & yr == 1960 & CO_CALC == "G5300530" ~ 1, ## Pierce Co, WA (err all 0 or 99)
          # is.na(hu_vect) & p_diff > 10 & yr == 1960 & CO_CALC == "G4804510" ~ 1,  ## Tom Green Co, TX  (noticeable errors at > 30)
          # is.na(hu_vect) & p_diff > 10 & yr == 1950 & CO_CALC == "G1301210" ~ 1,  ## Fulton Co, GA (all errors at 48%)  #--taken care of now
          # is.na(hu_vect) & p_diff > 10 & yr == 1950 & CO_CALC == "G5300530" ~ 1,  ## Pierce Co, WA (err ~ 41%)  #--taken care of now
          TRUE ~ 0
        )
    )
  
  ham4 <- bind_rows(ham4, temp)
}

## group check
ham4 %>%
  group_by(hu_new) %>%
  summarize(n = n())
## replacements: 1,639

## save
write.dbf(as.data.frame(ham4), "tables/ham4.dbf")

##################################################################
##################################################################
###                                                            ###
###                  LOAD HAM4 AND COMPLETE                    ###
###                                                            ###
##################################################################
##################################################################

## wd: "C:/Users/scott/Dropbox/urb_proj"
## load last
ham4 <- read.dbf("tables/ham4.dbf") %>%
  as_tibble()

ham4

##  replace strange cases established above (hu_new==1) w/ raster estimates
ham5 <- ham4 %>%
  mutate(
    hu_v = ifelse(hu_new == 1, hu_rast, hu_v),
    hu_sqmi = hu_v / sqmi
  )

ham5

## final, GIS-ready version
fin40_80 <- ham5 %>%
  mutate(
    CO_JOIN = substr(GISJOIN, 1, 8),
    rast_vect = ifelse(is.na(hu_vect), "rast", "vect")
  ) %>%
  rename(hu = hu_v) %>%
  select(STATE:GISJOIN, CO_JOIN, yr, rast_vect, p_rast, n_tot, hu_rast, hu, sqmi, hu_sqmi) %>%
  arrange(GISJOIN, yr)

## SAVE|REMOVE OLD
write.dbf(as.data.frame(fin40_80), "tables/fin40_80.dbf")
# rm(list = setdiff(ls(),"fin40_80"))  ## remove all except fin40_80

## how many rast or vect
fin40_80 %>%
  group_by(rast_vect) %>%
  count()

## rast = 180,409 (49.9%)
## vect = 180,896 (50.1%)

## make 40_80 data wide
fin40_80wide <- fin40_80 %>%
  mutate(yr = substr(yr, 3, 4)) %>%
  select(-c(rast_vect, p_rast, n_tot, hu_rast)) %>%
  pivot_wider(
    names_from = yr,
    names_sep = "",
    values_from = c(hu, sqmi, hu_sqmi)
  )


##################################################
####            LOAD 90_19 tract data         ####
##################################################

## load in data
hu90_19 <- read.dbf("tables/hu9019.dbf") %>%
  as_tibble() %>%
  ## calculate hu/sqmi
  mutate(
    hu_sqmi90 = hu90 / sqmi90,
    hu_sqmi00 = hu00 / sqmi00,
    hu_sqmi10 = hu10 / sqmi10,
    hu_sqmi19 = hu19 / sqmi19
  ) %>%
  select(-c(COUNTY, STATE))


## CORRECT state/county
st_co <- fips_codes %>%
  ## from tidyverse packages
  mutate(CO_JOIN = paste0("G", state_code, "0", county_code, "0")) %>%
  select(state, county, CO_JOIN) %>%
  as_tibble()

### JOIN 40-80 w/ 90-19
hu40_19 <- fin40_80wide %>%
  select(-c(STATE, COUNTY)) %>%
  left_join(hu90_19, by = c("GISJOIN" = "GISJOIN10")) %>%
  rename(GISJOIN10 = "GISJOIN") %>%
  left_join(st_co, by = "CO_JOIN") %>%
  select(GISJOIN10, GISJOIN15, state, county, hu40:hu80, hu90:hu19, sqmi40:sqmi80, sqmi90:sqmi19, hu_sqmi40:hu_sqmi80, hu_sqmi90:hu_sqmi19) %>%
  rename(
    STATE = "state",
    COUNTY = "county"
  )

hu40_19

######################################
###     CALCULATE URBANIZATION     ###
######################################

hu4019_urb <- hu40_19 %>%
  mutate(
    UY1 =
      case_when(
        hu_sqmi40 >= 200 ~ 1940,
        hu_sqmi50 >= 200 ~ 1950,
        hu_sqmi60 >= 200 ~ 1960,
        hu_sqmi70 >= 200 ~ 1970,
        hu_sqmi80 >= 200 ~ 1980,
        hu_sqmi90 >= 200 ~ 1990,
        hu_sqmi00 >= 200 ~ 2000,
        hu_sqmi10 >= 200 ~ 2010,
        hu_sqmi19 >= 200 ~ 2019,
        TRUE ~ 2035
      )
  )

## SAVE!!!
write.dbf(as.data.frame(hu4019_urb), "tables/hu4019_urb.csv")

## ~~~~ NUMBERS ~~~~ ##
## N = 650,349 (total t tracts, 1940-2019) [72,261*9]
## n = 361,305 (total t tracts in need of interpolatoin, 1940-1980) [72,261*5]

## VECTOR
## n = 180,896 (50.1%) total
## n =  76,591 (21.2%) DR-TDW
## n = 104,305 (28.9%) DR-AW

## RASTER
## n = 180,409 (49.9%) total
## n = 179,658 (49.7%) MH-MR
## n =     751 (00.2%) MR override

## ZEROES override
## n = 15 (might need to update)
