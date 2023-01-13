library(ape)
library(dplyr)
library(lubridate)
library(tidyverse)
library(zoo)

# Set the working directory as the same directory with the raw dataset folder
filenames <- dir(path = "lcl", pattern="*.csv")

df_monthly <- data.frame(LCLid = character(), month = numeric(), 
                         kwh_per_hh_avg = double(), 
                         kwh_per_hh_min = double(), 
                         kwh_per_hh_sum = double(), 
                         kwh_per_hh_max = double())
df_daily <- data.frame(LCLid = character(), date = as.Date(character()), 
                       kwh_per_hh_avg = double(), 
                       kwh_per_hh_min = double(),
                       kwh_per_hh_sum = double(), 
                       kwh_per_hh_max = double())
df_hourly <- data.frame(LCLid = character(), hour = numeric(), 
                        kwh_per_hh_avg = double(), 
                        kwh_per_hh_min = double(),
                        kwh_per_hh_sum = double(),
                        kwh_per_hh_max = double())

# Resample to monthly
for (filename in filenames){
  file_path <- paste0("lcl/", filename)
  print(file_path)
  df_temp <- read_csv(file_path, show_col_types = FALSE)
  df_temp <- df_temp %>% select(-stdorToU)
  df_temp$month <- month(df_temp$DateTime)
  df_temp$`KWH/hh (per half hour)` <- as.numeric(
    df_temp$`KWH/hh (per half hour)`)
  df_temp <- df_temp %>% group_by(LCLid, month) %>%
    summarise(kwh_per_hh_avg = mean(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_min = min(`KWH/hh (per half hour)`, na.rm = TRUE),
              kwh_per_hh_sum = sum(`KWH/hh (per half hour)`, na.rm = TRUE),
              kwh_per_hh_max = max(`KWH/hh (per half hour)`, na.rm = TRUE)) %>%
    ungroup()
  df_monthly <- rbind(df_monthly, df_temp)
}

df_monthly <- df_monthly %>% group_by(LCLid, month) %>%
  summarise(kwh_per_hh_avg = mean(kwh_per_hh_avg, na.rm = TRUE), 
            kwh_per_hh_min = min(kwh_per_hh_min, na.rm = TRUE), 
            kwh_per_hh_sum = sum(kwh_per_hh_sum, na.rm = TRUE), 
            kwh_per_hh_max = max(kwh_per_hh_max, na.rm = TRUE)) %>%
  ungroup()

write.csv(df_monthly, "household_consumption_monthly.csv", row.names = FALSE)

# Resample to daily
for (filename in filenames){
  file_path <- paste0("lcl/", filename)
  print(file_path)
  df_temp <- read_csv(file_path, show_col_types = FALSE)
  df_temp <- df_temp %>% select(-stdorToU)
  df_temp$date <- date(df_temp$DateTime)
  df_temp$`KWH/hh (per half hour)` <- as.numeric(
    df_temp$`KWH/hh (per half hour)`)
  df_temp <- df_temp %>% group_by(LCLid, date) %>%
    summarise(kwh_per_hh_avg = mean(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_min = min(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_sum = sum(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_max = max(`KWH/hh (per half hour)`, na.rm = TRUE)) %>%
  ungroup()
  df_daily <- rbind(df_daily, df_temp)
}

df_daily <- df_daily %>% group_by(LCLid, date) %>%
  summarise(kwh_per_hh_avg = mean(kwh_per_hh_avg, na.rm = TRUE), 
            kwh_per_hh_min = min(kwh_per_hh_min, na.rm = TRUE),
            kwh_per_hh_sum = sum(kwh_per_hh_sum, na.rm = TRUE), 
            kwh_per_hh_max = max(kwh_per_hh_max, na.rm = TRUE)) %>%
  ungroup()

df_daily_2011_2012 <- df_daily %>% filter(year(date) <= 2012)
df_daily_2013 <- df_daily %>% filter(year(date) == 2013)
df_daily_2014 <- df_daily %>% filter(year(date) == 2014)

write.csv(df_daily_2011_2012, "household_consumption_2011_2012_daily.csv", 
          row.names = FALSE)
write.csv(df_daily_2013, "household_consumption_2013_daily.csv", 
          row.names = FALSE)
write.csv(df_daily_2014, "household_consumption_2014_daily.csv", 
          row.names = FALSE)

# Resample to hourly
for (filename in filenames){
  file_path <- paste0("lcl/", filename)
  print(file_path)
  df_temp <- read_csv(file_path, show_col_types = FALSE)
  df_temp <- df_temp %>% select(-stdorToU)
  df_temp$hour <- hour(df_temp$DateTime)
  df_temp$`KWH/hh (per half hour)` <- as.numeric(
    df_temp$`KWH/hh (per half hour)`)
  df_temp <- df_temp %>% group_by(LCLid, hour) %>% 
    summarise(kwh_per_hh_avg = mean(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_min = min(`KWH/hh (per half hour)`, na.rm = TRUE),
              kwh_per_hh_sum = sum(`KWH/hh (per half hour)`, na.rm = TRUE), 
              kwh_per_hh_max = max(`KWH/hh (per half hour)`, na.rm = TRUE)) %>% 
    ungroup()
  df_hourly <- rbind(df_hourly, df_temp)
}

df_hourly <- df_hourly %>% group_by(LCLid, hour) %>% 
  summarise(kwh_per_hh_avg = mean(kwh_per_hh_avg, na.rm = TRUE), 
            kwh_per_hh_min = min(kwh_per_hh_min, na.rm = TRUE),
            kwh_per_hh_sum = sum(kwh_per_hh_min, na.rm = TRUE), 
            kwh_per_hh_max = max(kwh_per_hh_max, na.rm = TRUE)) %>%
  ungroup()

write.csv(df_hourly, "household_consumption_hourly.csv", row.names = FALSE)