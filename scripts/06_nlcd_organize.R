#########################################################################
#########################################################################
###                                                                   ###
###               NLCD 1992, 2001, 2011 PROCESSING                    ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")

## Check Workspace
getwd()  # D:/HHUUD10

#########################################################
## IMPORT 2001, 2011 DATA from NHGIS                   ##
#########################################################

## Read in 2001, 2011 NLCD data from NHGIS 

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!! NOTE: Users need to download these files manually as zip files w/ these names !!##
##!!! Instructrions are in the "table" folder's README file !!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
nlcd_import <- ipumsr::read_nhgis("tables/us_tract_2010_nlcd_timevariesbycolumn.zip", data_layer = contains("nlcd.csv"))

## Organize Data
nlcd0111 <- nlcd_import %>%
  # organize/remove columns
  select(
    GISJOIN, # GISJON10
    AREA,  # area in sq. m.
    AREA11_2001,  # area open water 2001
    AREA12_2001,  # area perennial ice/snow 2001
    AREA21_2001,  # area developed open space 2001
    AREA22_2001,  # area developed low intensity 2001
    AREA23_2001,  # area developed medium intensity 2001
    AREA24_2001,  # area developed high intensity 2001
    AREA11_2011,  # area open water 2011
    AREA12_2011,  # area perennial ice/snow 2011
    AREA21_2011,  # area developed open space 2011
    AREA22_2011,  # area developed low intensity 2011
    AREA23_2011,  # area developed medium intensity 2011
    AREA24_2011   # area developed high intensity 2011
    ) %>%
  # replace all NAs with zeros
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  # calculate % of non-water land developed in 2001 & 2011
  mutate(
    pdev01 = rowSums(.[5:8], 1, na.rm = TRUE) / (AREA - AREA11_2001 - AREA12_2001),  # rowSums of urban land uses in 2001 / (total area minus water)
    pdev01_dhi = AREA24_2001 / (AREA - AREA11_2001 - AREA12_2001),  # % Devopled, High Intensity (DHI) 2001
    pdev11 = rowSums(.[11:14], 1, na.rm = TRUE) / (AREA - AREA11_2011 - AREA12_2011), # rowSums of urban land uses in 2011 / (total area minus water)
    pdev11_dhi = AREA24_2011 / (AREA - AREA11_2001 - AREA12_2001)  # % Developed, High Intensity (DHI) 2011
  ) %>%
  # organize for output
  rename(GISJOIN10 = GISJOIN) %>%
  select(GISJOIN10, pdev01, pdev01_dhi, pdev11, pdev11_dhi) %>%
  arrange(GISJOIN10) %>%
  print()

################################################
## LOAD in 1992 NLCD DATA                    ###
################################################

## Import 1992 DATA
nlcd_import <- NULL # designate empty data frame
for(i in seq(1, 4)){
  
  # grab name of table file
  tab <- paste0("nlcd", i, "_zh")  
  
  # read in tables from nlcd92.gdb (colnames are GISJOIN codes)
  temp <- sf::st_read(
    dsn = "gis_files/nlcd92.gdb",
    layer = tab
    ) %>%
    as_tibble()
  
  # make long
  temp1 <- temp %>%
    # turn GISJOIN colnames into rows
    pivot_longer(
      cols = c(2:ncol(.)),
      names_to = "GISJOIN10",
      values_to = "pclass"
      ) %>%
    # turn 4 nlcd classifications into columns
    pivot_wider(
      names_from = LABEL,
      values_from = pclass) %>%
    # change names & organize
    rename(
      water = `1`,
      urban = `2`,
      urb_dhi = `3`,
      other = `4`
      ) %>%
    select(GISJOIN10, water:other)
  
  # bind into single data frame
  nlcd_import <- bind_rows(nlcd_import, temp1) %>%
    arrange(GISJOIN10)
  
}

### Fix duplicates from rasterization
nlcd92 <- nlcd_import %>%
  group_by(GISJOIN10) %>%
  summarize_all(funs(sum(.))) %>%
  # calculate % developed
  mutate(
    pdev92 = (urban + urb_dhi) / (urban + urb_dhi + other),
    pdev92_dhi = urb_dhi / (urban + urb_dhi + other)
    ) %>%
  select(GISJOIN10, pdev92, pdev92_dhi) %>%
  print()


###############################################################
## JOIN & SAVE 1992 & 2001/11 % DEVELOPED TRACT DATA        ###
###############################################################

## Join nlcd92 & 01/11
nlcd9211 <- left_join(nlcd92, nlcd0111, by = "GISJOIN10") %>%
  print()

## SAVE OUT
write_csv(nlcd9211, "tables/nlcd9211.csv")
write.dbf(as.data.frame(nlcd9211), "tables/nlcd9211.dbf")



  





