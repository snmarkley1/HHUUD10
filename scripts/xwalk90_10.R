

## Prepare workspace
rm(list = ls())
options(scipen = 999)

## 1990 bgp to 2010 t (ysb)
library(tidyverse)
library(foreign)

setwd("C:/Users/scott/Dropbox/urb_proj/nhgis_xwalk")

xwalk_t <- read_csv("bgp90_t10_xwalk.csv")
xwalk_bg <- read_csv("nhgis_bgp1990_bg2010.csv")
ysb <- read_csv("bgp90_ysb.csv")

names(xwalk_t)
names(xwalk_bg)
names(ysb)

## clean
xwalk90 <- xwalk %>%
  select(bgp1990gj, tr2010gj, wt_hu)

ysb90 <- ysb %>%
  select(GISJOIN, EX7001:EX7008) %>%
  mutate(
    hu90 = EX7001 + EX7002 + EX7003 + EX7004 + EX7005 + EX7006 + EX7007 + EX7008,
    hu80 = EX7004 + EX7005 + EX7006 + EX7007 + EX7008,
    hu70 = EX7005 + EX7006 + EX7007 + EX7008,
    hu60 = EX7006 + EX7007 + EX7008,
    hu50 = EX7007 + EX7008,
    hu40 = EX7008
  ) %>%
  select(-c(EX7001:EX7008))

## do xwalk
t9010_ysb <- xwalk90_t %>%
  left_join(ysb90, by = c("bgp1990gj" = "GISJOIN")) %>%
  mutate(
    hu90 = hu90 * wt_hu,
    hu80 = hu80 * wt_hu,
    hu70 = hu70 * wt_hu,
    hu60 = hu60 * wt_hu,
    hu50 = hu50 * wt_hu,
    hu40 = hu40 * wt_hu
  ) %>%
  group_by(tr2010gj) %>%
  summarize(
    hu90 = sum(hu90),
    hu80 = sum(hu80),
    hu70 = sum(hu70),
    hu60 = sum(hu60),
    hu50 = sum(hu50),
    hu40 = sum(hu40)
  ) %>%
  rename(GISJOIN = "tr2010gj")

t9010_ysb

## save as dbf
write.dbf(as.data.frame(t9010_ysb), "C:/Users/scott/Dropbox/urb_proj/tables/t9010_ysb.dbf")



## BGP to BG
bg9010_ysb <- xwalk_bg %>%
  select(bgp1990gj, bg2010gj, wt_hu) %>%
  left_join(ysb90, by = c("bgp1990gj" = "GISJOIN")) %>%
  mutate(
    hu90 = hu90 * wt_hu,
    hu80 = hu80 * wt_hu,
    hu70 = hu70 * wt_hu,
    hu60 = hu60 * wt_hu,
    hu50 = hu50 * wt_hu,
    hu40 = hu40 * wt_hu
  ) %>%
  group_by(bg2010gj) %>%
  summarize(
    hu90 = sum(hu90),
    hu80 = sum(hu80),
    hu70 = sum(hu70),
    hu60 = sum(hu60),
    hu50 = sum(hu50),
    hu40 = sum(hu40)
  ) %>%
  rename(GISJOIN = "bg2010gj")

bg9010_ysb

## save as dbf
write.dbf(as.data.frame(bg9010_ysb), "C:/Users/scott/Dropbox/urb_proj/tables/bg9010_ysb.dbf")
