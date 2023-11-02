# Data cleaning of miniDOT data for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman, Jasmine A. Krause, Kelly A. Loria
# Date Created: 2023-11-02

# ---------------------------- README -------------------------------------
# The following script will create unfiltered weather data files.

# Due to the sheer volume of  data, this workflow will export compiled files to
# the "data_working/do_data_cleaning" folder which will be ignored on Github.

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

#### Load Data ####

# Some of this code has been copied from step2 script.

# Import East Shore weather data from Synoptic.
# Source: Synoptic Station F9917 (Glenbrook)
# Excess rows of metadata trimmed.
gb_weather <- read_csv("data_raw/SynopticDownloads/F9917.2023-10-01_tidy.csv")
gb_weather$shore <- "E"
gb_trim <- gb_weather %>%
  mutate(Date_TimePST = with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
  select(Station_ID, Date_TimePST, solar_radiation_set_1, pressure_set_1d, 
         wind_speed_set_1, wind_direction_set_1, shore)

# Import West Shore weather data from Synoptic.
# Source: Synoptic Station D9413 & HMDC1 (Homewood)
# NOTE: TIME IN UTC.
# Excess rows of metadata trimmed.
bw_weather <- read_csv("data_raw/SynopticDownloads/D9413.2023-10-01_tidy.csv")
bw_weather$shore <- "W"
bw_trim <- bw_weather %>%
  mutate(Date_TimePST = with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
  select(Station_ID, Date_TimePST, pressure_set_1d, 
         wind_speed_set_1, wind_direction_set_1, shore) %>%
  # Create additional date/timestamp columns to join by since data is collected
  # at different intervals between the two datasets.
  mutate(Year = year(Date_TimePST),
         Month = month(Date_TimePST),
         Day = day(Date_TimePST),
         Hour = hour(Date_TimePST))

bw_solar <- read_csv("data_raw/SynopticDownloads/HMDC1.2023-10-31_tidy.csv")
bw_solar_trim <- bw_solar %>%
  mutate(Date_TimePST= with_tz(Date_Time, tzone = "America/Los_Angeles")) %>%
  select(Date_TimePST, solar_radiation_set_1) %>%
  mutate(Year = year(Date_TimePST),
         Month = month(Date_TimePST),
         Day = day(Date_TimePST),
         Hour = hour(Date_TimePST))

bw_both <- left_join(bw_trim, bw_solar_trim, by = c("Year", "Month", "Day", "Hour"),
                     multiple = "all")

bw_both_trim <- bw_both %>%
  rename(Date_TimePST = Date_TimePST.x) %>%
  select(Station_ID, Date_TimePST, solar_radiation_set_1, pressure_set_1d, 
         wind_speed_set_1, wind_direction_set_1, shore)

#### Combine weather datasets ####

# Bind weather data
both_weather <- rbind(bw_both_trim, gb_trim)

# Export dataset.
saveRDS(both_weather, "data_working/weather_aggregated_110223.rds")

# And create a MARSS-friendly version of this data with site, location, and
# replicate info included.

gb3NS1 <- gb_trim %>%
  mutate(site = "GB",
         location = "3m",
         replicate = "NS1")

gb3NS2 <- gb_trim %>%
  mutate(site = "GB",
         location = "3m",
         replicate = "NS2")

gb3NS3 <- gb_trim %>%
  mutate(site = "GB",
         location = "3m",
         replicate = "NS3")

gb10b <- gb_trim %>%
  mutate(site = "GB",
         location = "10m",
         replicate = "Benthic")

gb10p <- gb_trim %>%
  mutate(site = "GB",
         location = "10m",
         replicate = "Pelagic")

gb15b <- gb_trim %>%
  mutate(site = "GB",
         location = "15m",
         replicate = "Benthic")

gb15p <- gb_trim %>%
  mutate(site = "GB",
         location = "15m",
         replicate = "Pelagic")

gb20b <- gb_trim %>%
  mutate(site = "GB",
         location = "20m",
         replicate = "Benthic")

gb20p <- gb_trim %>%
  mutate(site = "GB",
         location = "20m",
         replicate = "Pelagic")

bw3NS1 <- bw_both_trim %>%
  mutate(site = "BW",
         location = "3m",
         replicate = "NS1")

bw3NS2 <- bw_both_trim %>%
  mutate(site = "BW",
         location = "3m",
         replicate = "NS2")

bw3NS3 <- bw_both_trim %>%
  mutate(site = "BW",
         location = "3m",
         replicate = "NS3")

bw10b <- bw_both_trim %>%
  mutate(site = "BW",
         location = "10m",
         replicate = "Benthic")

bw10p <- bw_both_trim %>%
  mutate(site = "BW",
         location = "10m",
         replicate = "Pelagic")

bw15b <- bw_both_trim %>%
  mutate(site = "BW",
         location = "15m",
         replicate = "Benthic")

bw15p <- bw_both_trim %>%
  mutate(site = "BW",
         location = "15m",
         replicate = "Pelagic")

bw20b <- bw_both_trim %>%
  mutate(site = "BW",
         location = "20m",
         replicate = "Benthic")

bw20p <- bw_both_trim %>%
  mutate(site = "BW",
         location = "20m",
         replicate = "Pelagic")

both_weather_long <- rbind(gb3NS1, gb3NS2)
both_weather_long <- rbind(both_weather_long, gb3NS3)
both_weather_long <- rbind(both_weather_long, gb10b)
both_weather_long <- rbind(both_weather_long, gb10p)
both_weather_long <- rbind(both_weather_long, gb15b)
both_weather_long <- rbind(both_weather_long, gb15p)
both_weather_long <- rbind(both_weather_long, gb20b)
both_weather_long <- rbind(both_weather_long, gb20p)
both_weather_long <- rbind(both_weather_long, bw3NS1)
both_weather_long <- rbind(both_weather_long, bw3NS2)
both_weather_long <- rbind(both_weather_long, bw3NS3)
both_weather_long <- rbind(both_weather_long, bw10b)
both_weather_long <- rbind(both_weather_long, bw10p)
both_weather_long <- rbind(both_weather_long, bw15b)
both_weather_long <- rbind(both_weather_long, bw15p)
both_weather_long <- rbind(both_weather_long, bw20b)
both_weather_long <- rbind(both_weather_long, bw20p)

# Export dataset.
saveRDS(both_weather_long, "data_working/weather_aggregated_long_110223.rds")

# End of script.
