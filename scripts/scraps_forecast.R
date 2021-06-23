

packages(forecast)
df_impute3 <- df_impute %>%
  #filter(STATE == "Alabama") %>%
  mutate(yr = ifelse(yr == 2019, 2020, yr)) %>%
  group_by(GISJOIN10) %>%
  mutate(
    pred_code = case_when(
      mr_impute == 5 ~ 5,
      mr_impute == 4 & !is.na(hu_est_ts[yr == 1980]) ~ 4,
      mr_impute == 3 & !is.na(hu_est_ts[yr == 1980]) & !is.na(hu_est_ts[yr == 1970]) ~ 3,
      mr_impute == 2 & !is.na(hu_est_ts[yr == 1980]) & !is.na(hu_est_ts[yr == 1970]) & !is.na(hu_est_ts[yr == 1960]) ~ 2,
      mr_impute == 1 & is.na(hu_est_ts[yr == 1940]) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  filter(pred_code > 0) %>%
  arrange(GISJOIN10, yr) %>%
  print()  # n = 7,246


#yr <- data.frame(yr = c(1980, 1970, 1960, 1950, 1940))
df_backcast <- NULL
for(i in unique(df_impute3$GISJOIN10)){
  
  JOIN <- data.frame(GISJOIN10 = i)
  df1 <- df_impute3 %>%
    filter(GISJOIN10 == i)
  
  if(df1$pred_code == 5){
    
    years <- data.frame(yr = c(1980, 1970, 1960, 1950, 1940))
    h <- 5
    
  } else if(df1$pred_code == 4){
    
    years <- data.frame(yr = c(1970, 1960, 1950, 1940))
    h <- 4
    
  } else if(df1$pred_code == 3){
    
    years <- data.frame(yr = c(1960, 1950, 1940))
    h <- 3
    
  } else if(df1$pred_code == 2){
    
    years <- data.frame(yr = c(1950, 1940))
    h <- 2
    
  } else{
    
    years <- data.frame(yr = 1940)
    h <- 1
    
  }
  
  df2 <- df1 %>%
    filter(!yr %in% years$yr)
  myts <- ts(rev(df2$hu_est_ts), frequency = 1)
  fc <- forecast(auto.arima(myts), h)
  hu_fc <- data.frame(hu_fc = fc$mean)
  temp <- bind_cols(JOIN, hu_fc, years)
  df_backcast <- bind_rows(df_backcast, temp)
  
}


df_backcast %>%
  arrange(GISJOIN10, yr) %>%
  as_tibble()

df_impute4 <- df_impute %>%
  left_join(df_backcast, by = c("GISJOIN10", "yr"))

df_impute4 %>% filter(mr_impute == 5)
