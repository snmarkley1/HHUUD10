





shp40 <- sf::st_read(dsn = "gis_files/database1.gdb", "t40") %>% filter(str_detect(GISJOIN, "G01"))
shp50 <- sf::st_read(dsn = "gis_files/database1.gdb", "t50") %>% filter(str_detect(GISJOIN, "G01"))
shp10 <- sf::st_read(dsn = "gis_files/database1.gdb", "t10") %>% 
  filter(str_detect(GISJOIN, "G010073")) %>%
  left_join(al, by = c("GISJOIN10" = "JOIN_T"))


sync(mapview(shp50, alpha.regions = 0, lwd = 4) + mapview(shp10, zcol = "pcover_pt", col.regions = viridisLite::turbo, lwd = 0.5) ,
     mapview(shp10, zcol = "pcover_pt", col.regions = viridisLite::turbo, lwd = 0.5) + mapview(shp50, alpha.regions = 0, lwd = 4))





## Calculate vars needed for TDW
temp_tdw <- df_int %>%
  # remove target year zeroes (precaution)
  filter(!(hu > 0 & hu_1 == 0)) %>%
  mutate(
    pcover = sqmi_int / sqmi,  # proportion overlay of source tract-years
    pcover_1 = sqmi_int / sqmi_1,  # prop. overlay of pseudo-target tract-years
    pcover_2 = sqmi_int / sqmi_2,  # prop. overlay of target tract-years
    hu_est = pcover * hu,  # AW fraction of HU for source tract-year in atom
    hu_est_1 = pcover_1 * hu_1,  # AW fraction of HU for pseudo-target tract-year in atom
    hu_est_2 = pcover_2 * hu_2  # AW fraction of HU for target tract-year in atom
  )

temp_tdw

## Group by 2010 JOIN_T and summarize HU estimates
temp_tdw1 <- temp_tdw %>%
  group_by(JOIN_T, yr, pcover_2, hu_est_2, sqmi_2) %>%
  summarize(
    sqmi_int = sum(sqmi_int), ## add up areas (in sq. mi.) for intersected portion, yr 1, and yr 2
    sqmi = sum(sqmi),
    sqmi_1 = sum(sqmi_1),
    pcover = sum(pcover),
    pcover_1 = sum(pcover_1), ## add up coverage to determine which tracts to keep/discard
    hu_est = sum(hu_est),
    hu_est_1 = sum(hu_est_1), ## get housing estimates via summation
    n = n()
  ) %>%
  mutate(pcover_tot = sqmi_int / sqmi_2) %>%
  ## get total % coverage
  arrange(yr, JOIN_T)

temp_tdw1

## Histogram
#temp_tdw1 %>%
#  filter(pcover_tot > 0.75 & pcover_tot < 0.99) %>%
#  ggplot(aes(x = pcover_tot)) +
#  geom_histogram(binwidth = 0.01) ## cutoff = 0.97

## Remove tracts with less than 97% coverage
temp_tdw1 <- temp_tdw1 %>%
  filter(pcover_tot > 0.97)

## Remove target tracts from equation to allow calculations
temp_tdw2 <- temp_tdw %>%
  # keep only atoms with 2010 tracts > 97% covered
  filter(JOIN_T %in% temp_tdw1$JOIN_T) %>%
  # group by source & pseudo-target tracts
  group_by(yr, GISJOIN, sqmi, hu, GISJOIN_1, sqmi_1, hu_1) %>%
  summarize(
    sqmi_int = sum(sqmi_int), # sum those cases where combinations are required
    pcover = sum(pcover),
    pcover_1 = sum(pcover_1),
    pcover_2 = sum(pcover_2),
    hu_est = sum(hu_est),
    hu_est_1 = sum(hu_est_1)
  ) %>%
  arrange(yr, GISJOIN_1)

temp_tdw2

## # Group by source tract-years to get sums
temp_tdw3 <- temp_tdw2 %>%
  # do calculations in source tract geographies
  group_by(yr, GISJOIN, sqmi, hu) %>%
  # sum of pseudo-tract values in source tract geographies
  mutate(hu_sum_1 = sum(hu_est_1)) %>%
  ungroup() %>%
  # determine % of HU in summed tract overlaps
  mutate(hu_fin_1 = ifelse(hu_sum_1 > 0, hu_est_1 / hu_sum_1 * hu, 0)) # tdw in pseudo-target tracts

temp_tdw3

## Put into pseudo-target tract-years (s + 10) (suffix = _1)
temp_tdw4 <- temp_tdw3 %>%
  group_by(yr, GISJOIN_1) %>%
  summarize(
    pcover = sum(pcover),  # source tract coverage in pseudo-target (pt) tracts
    pcover_1 = median(pcover_1),  # get median of pt tract areas
    pcover_2 = sum(pcover_2),  # target tract coverage in pt tracts
    hu_est = sum(hu_est),  
    hu_est_1 = sum(hu_est_1),
    hu_fin_1 = sum(hu_fin_1)
  ) %>%
  arrange(yr, GISJOIN_1) %>%
  filter(
    pcover_1 > 0.97 & # keeps only fully covered s+10 tracts
      pcover < 0.97  # get rid of tracts that went small to large & 1-to-1 tracts (no advantage of TDW)
  ) %>%
  select(GISJOIN_1, yr, pcover:hu_fin_1)

temp_tdw4

## Join original intersect data with temp_tdw4 & put estimates in target tracts
temp_tdw5 <- df_int %>%
  # Join original int imput with temp_tdw4: 1-GISJOIN; 2-yr; 7-hu_fin_12
  left_join(temp_tdw4[c(1, 2, 7)], by = c("yr", "GISJOIN_1")) %>%
  # remove unmatched rows
  filter(!is.na(hu_fin_1)) %>%
  mutate(
    pcover_t = sqmi_int / sqmi_2,  # 2010 tract coverage (target)
    pcover_s = sqmi_int / sqmi,  # source tract coverage
    pcover_pt = sqmi_int / sqmi_1,  # pseudo-target tract coverage (s + 10)
    hu_est = hu * pcover_s,  # hu_est for source tract (hu_1_est)
    hu_tdw = hu_fin_1 * pcover_pt # tdw value
  ) %>%
  # Put into target tracts
  group_by(JOIN_T, sqmi, yr) %>%
  summarize(
    pcover_t = sum(pcover_t),  # target (2010)
    pcover_S = sum(pcover_s),  # source
    pcover_pt = sum(pcover_pt),  # pseudo-tract (s + 10)
    hu_tdw = sum(hu_tdw, na.rm = TRUE)
  ) %>%
  arrange(yr, JOIN_T)

temp_tdw5

## Histogram
#temp_tdw5 %>%
#  filter(between(pcover_t, 0.85, 0.99)) %>%
#  ggplot(aes(x = pcover_t)) +
#  geom_histogram(binwidth = 0.01)

## Final output for TDW
dr_tdw <- temp_tdw5 %>%
  filter(
    pcover_t > 0.97 &  # must remove these: under 0.97 indicates only partially covered 2010 tracts
      pcover_pt > 0.97 &  # pseudo-target tract must have stayed same size or been aggregated
      #hu_tdw > 1 &  # remove zeroes
      !(yr %in% c(1950, 1960) & JOIN_T %in% flag$JOIN_T)  # remove flagged tracts for 1950 & 1960
  ) %>%
  rename(GISJOIN10 = JOIN_T) %>%
  select(GISJOIN10, yr, sqmi, hu_tdw) %>%
  ungroup()

dr_tdw  # n = 35,930