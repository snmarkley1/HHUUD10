#########################################################################
#########################################################################
###                                                                   ###
###                Combine Files for Validation                       ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/validation


######################################################################
## Import and organize files to combine
######################################################################


# county fips for NHGIS and Census Data Formats for nine  counties included in validation
county_fips <- c("G2200710", "G2601630", "G2905100",  # decline: Orleans Parish(G2200710), Wayne County (G2601630), St. Louis city (G2905100),
                 "G0600650", "G1200950","G4804390",   # growth: Riverside County(G0600650), Orange County (G1200950), Tarrant County (G4804390),
                 "G3400130", "G3900610", "G4200030")  # stable: Essex County (G3400130), Hamilton County(G3900610), Allegheny County (G4200030)

acs_county_fips <- c("22071", "26163", "29510", "06065", "12095","48439","34013", "39061", "42003")

# State Fips and Abbreviations of nine counties in validation
my_states <- c(22,26,29,06,12,48,34,39,42)
state_codes <- c("LA", "MI", "MO", "CA", "FL", "TX", "NJ", "OH", "PA")


##--------------------------------------------------------------------------------------
## read in tables
tracts1519 <- read_csv("tables/tracts1519.csv")
tracts0610 <- read_csv("tables/tracts0610.csv")
tracts00 <- read_csv("tables/tracts00.csv")
tracts90 <- read_csv("tables/tracts90.csv")
county90_00 <- read_csv("tables/countyhu90_00.csv")
tracts_ts2010 <- read_csv("tables/tracts_ts2010.csv")
bgs_1519 <- read_csv("tables/block_groups_1519.csv")

##--------------------------------------------------------------------------------------
## read in shapefiles
tracts10_sf <- read_sf("gis_files/tracts_2010/tracts10.shp")
tracts00_sf <- read_sf("gis_files/tracts_2000/tracts00.shp")
tracts90_sf <- read_sf("gis_files/tracts_1990/tracts90.shp")
bgs19_sf <- read_sf("gis_files/bgroups_2019/bgs19.shp")


##--------------------------------------------------------------------------------------
## read in tracts to remove
remove_tracts_2000 <- read_csv("tables/tracts00_2_3_stay.csv") %>% ## read in file with 1/2 of tracts to stay and 1/2 to remove (created in buf_2000.R)
  select(GISJOIN, stay)

remove_tracts_1990 <- read_csv("tables/tracts90_1_3_stay.csv") %>% # read in file with 1/4 of tracts to stay and 3/4 to remove (created in buf_1990.R)
  select(GISJOIN, stay)
  

##--------------------------------------------------------------------------------------
# shape 1

# join 2006-10 ACS housing units to 2015-19 ACS census tract data
tracts1519 <- tracts0610 %>%
  select(GISJOIN, HU2006_10) %>%
  left_join(.,tracts1519)

## join time series to data
tracts1519ts <- left_join(tracts1519,tracts_ts2010)

# join county housing unit totals to census tract data
tracts15195s_county <- left_join(tracts1519ts,county90_00, by = c("COUNTYA" = "GISJOIN"))

# Hammer Estimates
tracts_ham_est <- tracts15195s_county %>%
  group_by(COUNTYA) %>%
  mutate(
    j90 = sum(AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # sum of housing units built before 1989 in county j
    ham_est90 = (hu1990_cnty/j90) * hu_90,
    j00 = sum(AL0DE005, AL0DE006, AL0DE007, AL0DE008, AL0DE009, AL0DE010, AL0DE011), # housing units built before 1999 inin county j
    ham_est00 = (hu2000_cnty/j00) * hu_00
  ) %>%
  ungroup() %>%
  select(GISJOIN, HU2015_19, HU2006_10, hu1990_10ts, hu2000_10ts, hu1990_cnty, hu2000_cnty, ham_est90, ham_est00)

# join data for Shapefile 1 to spatial file
final1519_sf <- left_join(tracts10_sf,tracts_ham_est)

# # write shapefile
st_write(final1519_sf, dsn = "gis_files/tracts_2019", layer = "shp1_tracts1519.shp", driver = "ESRI Shapefile", append = TRUE)

rm(tracts1519, tracts15195s_county, tracts1519ts, tracts_ts2010, county90_00, tracts_ham_est)

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 2
# join tract 'stay or remove' indicator to housing unit totals
final00 <- left_join(tracts00, remove_tracts_2000)

final00 <- final00 %>%
  select(GISJOIN, HU2000_00, HU1990_00, stay)

# join spatial
final00_sf <- left_join(tracts00_sf,final00)

# write file
st_write(final00_sf, dsn = "gis_files/tracts_2000", layer = "shp2_tracts00.shp", driver = "ESRI Shapefile", append = FALSE)

rm(remove_tracts_2000, tracts00, tracts00_sf, final00)

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 3
# join tract 'stay or remove' indicator to housing unit totals
final90 <- left_join(tracts90, remove_tracts_1990)

# join spatial
final90_sf <- left_join(tracts90_sf, final90)

# write shapefile
st_write(final90_sf, dsn = "gis_files/tracts_1990", layer = "shp3_tracts90.shp", driver = "ESRI Shapefile", append = FALSE) #,

rm(remove_tracts_1990, tracts90, tracts90_sf, final90)

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 4
# join spatial to data table
final19bg_sf <- left_join(bgs19_sf, bgs_1519, by = "GISJOIN")

# write shapefile
st_write(final19bg_sf, dsn = "gis_files/bgroups_2019", layer = "shp4_bgs19.shp", driver = "ESRI Shapefile", append = FALSE) # , append = FALSE

rm(bgs_1519, bgs19_sf)

##--------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------
# shape 5
# # join tract 'stay or remove' indicator to housing unit totals
# final0610 <- left_join(tracts0610,remove_tracts_2010)

# # join spatial
final0610_sf <- left_join(tracts10_sf, tracts0610)

# write file
st_write(final0610_sf, dsn = "gis_files/tracts_2010", layer = "shp5_tracts10.shp", driver = "ESRI Shapefile", append = TRUE)

rm(tracts0610, tracts10_sf)
