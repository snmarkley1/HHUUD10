#########################################################################
#########################################################################
###                                                                   ###
###         Establish Buffer Tracts for Validation Removal            ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/HHUUD10/validation


################################################################
## Bring in Files for Removal Process (1990 & 2000)
################################################################

##----------------------------------------
## Set counties
##----------------------------------------
county_fips <- c("G2200710", "G2601630", "G2905100",
                 "G0600650", "G1200950","G4804390",
                 "G3400130", "G3900610", "G4200030")


##-----------------------------------------
## Import 1990 and 2000 SFs
##-----------------------------------------

## 1990
tracts90_sf <- read_sf("gis_files/tracts_1990/tracts90.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8))

## 2000
tracts00_sf <- read_sf("gis_files/tracts_2000/tracts00.shp") %>%
  mutate(COUNTYA = str_sub(GISJOIN,1,8))


##------------------------------------------------------------
## Create city centroids for nine validation counties
##------------------------------------------------------------

orleans = st_point(c(569207, -822594))
wayne = st_point(c(1049928, 617021))
stl = st_point(c(497637, 142084))
riverside = st_point(c(-1950000, -197000))
orange = st_point(c(1430000, -890000))
tarrant = st_point(c(-115000, -529192))
essex = st_point(c(1810935, 571906))
hamilton = st_point(c(980710, 244401))
allegheny = st_point(c(1340792, 443919))



############################################################################
############################################################################
## Establish 1/3 & 2/3 to Keep for 1990 & 2000
############################################################################
############################################################################


#####################################################################
##-------------------------------------
##  1990 1/3
##-------------------------------------
#####################################################################

## Orleans (1)
orleans_ctr <- st_sfc(orleans, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G2200710") -> orleans_sf

orleans_buf <- st_buffer(orleans_ctr, 3300)
orleans_buf_intersects <- st_intersects(orleans_buf, orleans_sf)
orleans_sel_sf <- orleans_sf[orleans_buf_intersects[[1]],]

count(orleans_sel_sf)/count(orleans_sf)

plot(st_geometry(orleans_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(orleans_sel_sf), add=T, col="red")
plot(st_geometry(orleans_buf), add=T, lwd = 2)


## Wayne (2)
wayne_ctr <- st_sfc(wayne, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G2601630") -> wayne_sf

wayne_buf <- st_buffer(wayne_ctr, 7800)
wayne_buf_intersects <- st_intersects(wayne_buf, wayne_sf)
wayne_sel_sf <- wayne_sf[wayne_buf_intersects[[1]],]

count(wayne_sel_sf)/count(wayne_sf)

plot(st_geometry(wayne_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(wayne_sel_sf), add=T, col="red")
plot(st_geometry(wayne_buf), add=T, lwd = 2)


## STL (3)
stl_ctr <- st_sfc(stl, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G2905100") -> stl_sf

stl_buf <- st_buffer(stl_ctr, 3100)
stl_buf_intersects <- st_intersects(stl_buf, stl_sf)
stl_sel_sf <- stl_sf[stl_buf_intersects[[1]],]

count(stl_sel_sf)/count(stl_sf)

plot(st_geometry(stl_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(stl_sel_sf), add=T, col="red")
plot(st_geometry(stl_buf), add=T, lwd = 2)


## Riverside (4)#
riverside_ctr <- st_sfc(riverside, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G0600650") -> riverside_sf

riverside_buf <- st_buffer(riverside_ctr, 19500)
riverside_buf_intersects <- st_intersects(riverside_buf, riverside_sf)
riverside_sel_sf <- riverside_sf[riverside_buf_intersects[[1]],]

count(riverside_sel_sf)/count(riverside_sf)

plot(st_geometry(riverside_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(riverside_sel_sf), add=T, col="red")
plot(st_geometry(riverside_buf), add=T, lwd = 2)

# Orange (5)
orange_ctr <- st_sfc(orange, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G1200950") -> orange_sf

orange_buf <- st_buffer(orange_ctr, 5050)
orange_buf_intersects <- st_intersects(orange_buf, orange_sf)
orange_sel_sf <- orange_sf[orange_buf_intersects[[1]],]

count(orange_sel_sf)/count(orange_sf)

plot(st_geometry(orange_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(orange_sel_sf), add=T, col="red")
plot(st_geometry(orange_buf), add=T, lwd = 2)

## Tarrant (6)
tarrant_ctr <- st_sfc(tarrant, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G4804390") -> tarrant_sf

tarrant_buf <- st_buffer(tarrant_ctr, 9400)
tarrant_buf_intersects <- st_intersects(tarrant_buf, tarrant_sf)
tarrant_sel_sf <- tarrant_sf[tarrant_buf_intersects[[1]],]

count(tarrant_sel_sf)/count(tarrant_sf)

plot(st_geometry(tarrant_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(tarrant_sel_sf), add=T, col="red")
plot(st_geometry(tarrant_buf), add=T, lwd = 2)


## Essex (7)
essex_ctr <- st_sfc(essex, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G3400130") -> essex_sf

essex_buf <- st_buffer(essex_ctr, 2700)
essex_buf_intersects <- st_intersects(essex_buf, essex_sf)
essex_sel_sf <- essex_sf[essex_buf_intersects[[1]],]

count(essex_sel_sf)/count(essex_sf)

plot(st_geometry(essex_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(essex_sel_sf), add=T, col="red")
plot(st_geometry(essex_buf), add=T, lwd = 2)

## Allegheny (8)
allegheny_ctr <- st_sfc(allegheny, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G4200030") -> allegheny_sf

allegheny_buf <- st_buffer(allegheny_ctr, 5600)
allegheny_buf_intersects <- st_intersects(allegheny_buf, allegheny_sf)
allegheny_sel_sf <- allegheny_sf[allegheny_buf_intersects[[1]],]

count(allegheny_sel_sf)/count(allegheny_sf)

plot(st_geometry(allegheny_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(allegheny_sel_sf), add=T, col="red")
plot(st_geometry(allegheny_buf), add=T, lwd = 2)

## Hamilton (9)
hamilton_ctr <- st_sfc(hamilton, crs = "ESRI:102003")

tracts90_sf %>%
  filter(COUNTYA == "G3900610") -> hamilton_sf

hamilton_buf <- st_buffer(hamilton_ctr, 4850)
hamilton_buf_intersects <- st_intersects(hamilton_buf, hamilton_sf)
hamilton_sel_sf <- hamilton_sf[hamilton_buf_intersects[[1]],]

count(hamilton_sel_sf)/count(hamilton_sf)

plot(st_geometry(hamilton_sf), border="#aaaaaa", main="1/3 of County Census Tracts")
plot(st_geometry(hamilton_sel_sf), add=T, col="red")
plot(st_geometry(hamilton_buf), add=T, lwd = 2)


## --------------------------------------------------------
## combine all selected tracts, Clean, & Export
##---------------------------------------------------------
all_sel_sf <- bind_rows(allegheny_sel_sf,essex_sel_sf, hamilton_sel_sf, orange_sel_sf, orleans_sel_sf, riverside_sel_sf,
                        stl_sel_sf, tarrant_sel_sf, wayne_sel_sf)


# add a column designate which tracts stay
all_sel_sf <- all_sel_sf %>%
  mutate(
    stay = ("Y")
  ) %>%
  as_tibble() %>%select(-(geometry))


# join back to full tracts
tracts90_all <- left_join(tracts90_sf, all_sel_sf) %>%
  mutate(stay = replace_na(stay,"N"))

# update factors
tracts90_all$stay <- factor(tracts90_all$stay, levels = c('Y','N'))

## Make csv
tracts90_all_csv <- tracts90_all %>%
  as_tibble() %>%
  select(-geometry, -COUNTYA) %>%
  print()

# write csv
write_csv(tracts90_all_csv, "tables/tracts90_1_3_stay.csv")


## clean up environment before moving to 2000 tracts
# remove ._sf
rm(allegheny_sf,essex_sf, hamilton_sf, orange_sf, orleans_sf, riverside_sf,stl_sf, tarrant_sf, wayne_sf)

# remove ._sel_sf
rm(allegheny_sel_sf,essex_sel_sf, hamilton_sel_sf, orange_sel_sf, orleans_sel_sf, riverside_sel_sf,stl_sel_sf, tarrant_sel_sf, wayne_sel_sf)

# remove ._buf
rm(allegheny_buf,essex_buf, hamilton_buf, orange_buf, orleans_buf, riverside_buf,stl_buf, tarrant_buf, wayne_buf)

# remove ._buf_intersects
rm(allegheny_buf_intersects,essex_buf_intersects, hamilton_buf_intersects, orange_buf_intersects, orleans_buf_intersects, riverside_buf_intersects,stl_buf_intersects, tarrant_buf_intersects, wayne_buf_intersects)




#####################################################################
##-------------------------------------
##  2000 2/3
##-------------------------------------
#####################################################################


## Orleans County (New Orleans), Declining county #1
tracts00_sf %>%
  filter(COUNTYA == "G2200710") -> orleans_sf

orleans_buf <- st_buffer(orleans_ctr, 5250)
orleans_buf_intersects <- st_intersects(orleans_buf, orleans_sf)
orleans_sel_sf <- orleans_sf[orleans_buf_intersects[[1]],]

count(orleans_sel_sf)/count(orleans_sf)

plot(st_geometry(orleans_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(orleans_sel_sf), add=T, col="red")
plot(st_geometry(orleans_buf), add=T, lwd = 2)


## Wayne County (Detroit), Declining County #2
tracts00_sf %>%
  filter(COUNTYA == "G2601630") -> wayne_sf

wayne_buf <- st_buffer(wayne_ctr, 15550)
wayne_buf_intersects <- st_intersects(wayne_buf, wayne_sf)
wayne_sel_sf <- wayne_sf[wayne_buf_intersects[[1]],]

count(wayne_sel_sf)/count(wayne_sf)

plot(st_geometry(wayne_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(wayne_sel_sf), add=T, col="red")
plot(st_geometry(wayne_buf), add=T, lwd = 2)


## St. Louis; Declining County #3
tracts00_sf %>%
  filter(COUNTYA == "G2905100") -> stl_sf

stl_buf <- st_buffer(stl_ctr, 5550)
stl_buf_intersects <- st_intersects(stl_buf, stl_sf)
stl_sel_sf <- stl_sf[stl_buf_intersects[[1]],]

count(stl_sel_sf)/count(stl_sf)

plot(st_geometry(stl_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(stl_sel_sf), add=T, col="red")
plot(st_geometry(stl_buf), add=T, lwd = 2)


## Riverside County, CA; Growing County #1
tracts00_sf %>%
  filter(COUNTYA == "G0600650") -> riverside_sf

riverside_buf <- st_buffer(riverside_ctr, 37500)
riverside_buf_intersects <- st_intersects(riverside_buf, riverside_sf)
riverside_sel_sf <- riverside_sf[riverside_buf_intersects[[1]],]

count(riverside_sel_sf)/count(riverside_sf)

plot(st_geometry(riverside_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(riverside_sel_sf), add=T, col="red")
plot(st_geometry(riverside_buf), add=T, lwd = 2)


## Orange County, FL; Growing county #2
tracts00_sf %>%
  filter(COUNTYA == "G1200950") -> orange_sf

orange_buf <- st_buffer(orange_ctr, 11400)
orange_buf_intersects <- st_intersects(orange_buf, orange_sf)
orange_sel_sf <- orange_sf[orange_buf_intersects[[1]],]

count(orange_sel_sf)/count(orange_sf)

plot(st_geometry(orange_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(orange_sel_sf), add=T, col="red")
plot(st_geometry(orange_buf), add=T, lwd = 2)


## Tarrant County, TX (Ft Worth); Growing county #3
tracts00_sf %>%
  filter(COUNTYA == "G4804390") -> tarrant_sf

tarrant_buf <- st_buffer(tarrant_ctr, 15000)
tarrant_buf_intersects <- st_intersects(tarrant_buf, tarrant_sf)
tarrant_sel_sf <- tarrant_sf[tarrant_buf_intersects[[1]],]

count(tarrant_sel_sf)/count(tarrant_sf)

plot(st_geometry(tarrant_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(tarrant_sel_sf), add=T, col="red")
plot(st_geometry(tarrant_buf), add=T, lwd = 2)


## Essex County, NJ; stable county #1
tracts00_sf %>%
  filter(COUNTYA == "G3400130") -> essex_sf

essex_buf <- st_buffer(essex_ctr, 5300)
essex_buf_intersects <- st_intersects(essex_buf, essex_sf)
essex_sel_sf <- essex_sf[essex_buf_intersects[[1]],]

count(essex_sel_sf)/count(essex_sf)

plot(st_geometry(essex_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(essex_sel_sf), add=T, col="red")
plot(st_geometry(essex_buf), add=T, lwd = 2)


## Allegheny County; Stable county #2
tracts00_sf %>%
  filter(COUNTYA == "G4200030") -> allegheny_sf

allegheny_buf <- st_buffer(allegheny_ctr, 12000)
allegheny_buf_intersects <- st_intersects(allegheny_buf, allegheny_sf)
allegheny_sel_sf <- allegheny_sf[allegheny_buf_intersects[[1]],]

count(allegheny_sel_sf)/count(allegheny_sf)

plot(st_geometry(allegheny_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(allegheny_sel_sf), add=T, col="red")
plot(st_geometry(allegheny_buf), add=T, lwd = 2)


## Hamilton County, OH; Stable county #3
tracts00_sf %>%
  filter(COUNTYA == "G3900610") -> hamilton_sf

hamilton_buf <- st_buffer(hamilton_ctr, 9700)
hamilton_buf_intersects <- st_intersects(hamilton_buf, hamilton_sf)
hamilton_sel_sf <- hamilton_sf[hamilton_buf_intersects[[1]],]

count(hamilton_sel_sf)/count(hamilton_sf)

plot(st_geometry(hamilton_sf), border="#aaaaaa", main="2/3 of County Census Tracts")
plot(st_geometry(hamilton_sel_sf), add=T, col="red")
plot(st_geometry(hamilton_buf), add=T, lwd = 2)


##------------------------------------------
## combine all selected tracts
##------------------------------------------

all_sel_sf <- bind_rows(allegheny_sel_sf,essex_sel_sf, hamilton_sel_sf, orange_sel_sf, orleans_sel_sf, riverside_sel_sf,
                        stl_sel_sf, tarrant_sel_sf, wayne_sel_sf)

## create column of selected tracts
all_sel_sf <- all_sel_sf %>%
  mutate(
    stay = ("Y")
  ) %>%
  as_tibble() %>%select(-(geometry))


## join back to all tracts
tracts00_all <- left_join(tracts00_sf, all_sel_sf) %>%
  mutate(stay = replace_na(stay,"N"))

tracts00_all$stay <- factor(tracts00_all$stay, levels = c('Y','N'))

fct_count(tracts00_all$stay)


## Put in csv format
tracts00_all_csv <- tracts00_all %>%
  as_tibble() %>%
  select(-geometry, -COUNTYA) %>%
  print()

## Save out
write_csv(tracts00_all_csv, "tables/tracts00_2_3_stay.csv")

