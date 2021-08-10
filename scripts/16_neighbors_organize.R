
#########################################################################
#########################################################################
###                                                                   ###
###                     ORGANIZE NEIGHBORS FILE                       ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")


###############################################################################
##  IMPORT & CLEAN t10_neighbors TABLE                                       ##
###############################################################################

## IMPORT
df <- sf::st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10_neighbors"
) %>%
  as_tibble() %>%
  print()


## CALCULATE % BORDER
neighbors <- df %>%
  group_by(src_GISJOIN) %>%
  mutate(
    src_LENGTH = sum(LENGTH),  # calculate non-coastal perimeter of source tracts
    WEIGHT = LENGTH / src_LENGTH  # calculate percent of non-coastal perimeter shared by neighbor tracts --> weight to be used in future calculations
    ) %>%
  select(src_GISJOIN, nbr_GISJOIN, WEIGHT) %>%
  print()


## SAVE OUT
write_csv(neighbors, "tables/neighbors.csv")


###############################################################################
##  IDENTIFY ISLANDS                                                         ##
###############################################################################

## LOAD TRACTS
t10 <- st_read(
  dsn = "gis_files/database1.gdb",
  layer = "t10_nlcd"
) %>%
  as_tibble() %>%
  print()
  

## Select tracts w/o neighbors
islands <- t10 %>%
  anti_join(neighbors, by = c("GISJOIN" = "src_GISJOIN")) %>%
  select(GISJOIN) %>%
  # id island
  mutate(island = 1) %>%
  # keep only those w/ > 50% pdev
  print()  # n = 20


## SAVE
write.dbf(as.data.frame(islands), "tables/islands.dbf")


