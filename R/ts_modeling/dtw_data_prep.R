# Dynamic Time Warping Data Prep Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will prep data to be fit by the 
# dynamic time warping approach. This includes both raw
# DO data (in mg/L) as well as percent saturation DO.

#### SETUP ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(suntools)
library(here)

# Load data.
data <- readRDS("data_working/24_DOsat_offset.rds")

#### TIDY ####

# Assign daily indices.
data_hourly <- data %>%
  mutate(date = date(Pacific_Standard_Time),
         hour = hour(Pacific_Standard_Time)) %>%
  group_by(site, location, replicate, date, hour,
           Flag1, Flag2, Flag3, Flag4) %>%
  summarize(DO_sat = mean(o2_sat100, 
                           na.rm = TRUE),
            DO_mgL = mean(Dissolved_Oxygen_offset,
                          na.rm = TRUE),
            Temp_C = mean(Temperature_deg_C,
                          na.rm = TRUE)) %>%
  ungroup()

# Creating a function that can be applied by grouping.
indexFUN <- function(x){
  
  for(i in 2:nrow(x)){
    # if the time is equal to 4am
    if(x$hour[i] == 4){
      # the index value is one greater than the hour before
      x$index[i] = x$index[(i-1)] + 1
      # if the time is any other hour,
    } else {
      # the index value is the same as the hour before
      x$index[i] = x$index[(i-1)]
    }
  }
  
  return(x)
  
}

# See example grouping here: https://dplyr.tidyverse.org/reference/group_map.html
# data_indexed <- data_hourly %>%
#   group_by(site, location, replicate) %>%
#   group_modify(~ indexFUN(.x)) %>%
#   ungroup()
# Ok, so there are some issues happening, so we need to group differently.

# Making a new column with which we'll split into a list.
data_hourly <- data_hourly %>%
  mutate(ID = paste(site, location, replicate, sep = "_"))

# Also, create a baseline dataset of dates, because trying
# rules to anticipate indexing proved too challenging.
date_times <- seq(ymd_hms("2021-05-31 04:00:00"), 
                  ymd_hms("2023-10-01 03:00:00"),
                  by = "hour")

# NOTE - will be measuring from 4AM to 4AM!!

dates_all <- as_tibble(date_times) %>%
  rename(date_times = value) %>%
  mutate(hour = hour(date_times),
         index = 0)

# Just copy-pasting loop from above
for(i in 2:nrow(dates_all)){
  # if the time is equal to 4am
  if(dates_all$hour[i] == 4){
    # the index value is one greater than the hour before
    dates_all$index[i] = dates_all$index[(i-1)] + 1
    # if the time is any other hour,
  } else {
    # the index value is the same as the hour before
    dates_all$index[i] = dates_all$index[(i-1)]
  }
}

# Now, to create a base dataset to join with the raw data.
# Repeat 21 times.
dates_all <- dates_all %>% 
  slice(rep(1:n(), 21))

sites <- rep(c("BW_10m_Benthic", "BW_15m_Benthic", "BW_20m_Benthic",
               "BW_20m_Pelagic", "BW_3m_NS1", "BW_3m_NS2",
               "BW_3m_NS3", "GB_10m_Benthic", "GB_10m_Pelagic",
               "GB_15m_Benthic", "GB_15m_Pelagic", "GB_20m_Benthic",
               "GB_20m_Pelagic", "GB_3m_NS1", "GB_3m_NS2",
               "GB_3m_NS3", "SH_3m_NS1", "SH_3m_NS2", "SH_3m_NS3",
               "SS_3m_NS1", "SS_3m_NS2"),
            each = 20472)

# Noticed one weird NA value made it in there.
data_hourly <- data_hourly %>%
  drop_na(site)

# And join with sites.
dates_all <- dates_all %>%
  mutate(ID = sites,
         date = date(date_times))

##### Solar Noon #####

# Making custom solar noon function to pull out
# only the value I want.
solar_noon_fx <- function(Longitude, Latitude, Date){
  
  x <- solarnoon(matrix(c(Longitude, Latitude), nrow = 1),
                 as.POSIXct(Date, tz = "America/Los_Angeles"),
                            POSIXct.out = TRUE)
  
  solar_noon_time <- x$time[1]
  
  return(solar_noon_time)
  
}

# Add sites and lat/lon values for solar noon calculation.
# Using nearshore values from Kelly's NLDAS script located:
# https://github.com/kellyloria/Littoral-Lake-Metabolism/blob/e93fe82617e3c7aa3e7624f776efe9e1d4dd9ce4/climate_scripts/KAL_DL_NLDAS.R
dates_all <- dates_all %>%
  mutate(site = case_when(ID %in% c("BW_10m_Benthic",
                                    "BW_15m_Benthic",
                                    "BW_20m_Benthic",
                                    "BW_20m_Pelagic", 
                                    "BW_3m_NS1", 
                                    "BW_3m_NS2",
                                    "BW_3m_NS3") ~ "BW",
                          ID %in% c("GB_10m_Benthic",
                                    "GB_10m_Pelagic",
                                    "GB_15m_Benthic",
                                    "GB_15m_Pelagic",
                                    "GB_20m_Benthic",
                                    "GB_20m_Pelagic", 
                                    "GB_3m_NS1", 
                                    "GB_3m_NS2",
                                    "GB_3m_NS3") ~ "GB",
                          ID %in% c("SH_3m_NS1", 
                                    "SH_3m_NS2", 
                                    "SH_3m_NS3") ~ "SH",
                          ID %in% c("SS_3m_NS1", 
                                    "SS_3m_NS2") ~ "SS")) %>%
  mutate(lat = case_when(site == "BW" ~ "39.10697",
                         site == "GB" ~ "39.0880",
                         site == "SH" ~ "39.0944",
                         site == "SS" ~ "39.13904"),
         lon = case_when(site == "BW" ~ "-120.15721",
                         site == "GB" ~ "-119.9421",
                         site == "SH" ~ "-119.9436",
                         site == "SS" ~ "-120.15236")) 

# Trying it another way.
lon_vec <- as.numeric(dates_all$lon)
lat_vec <- as.numeric(dates_all$lat)
date_vec <- force_tz(dates_all$date, "America/Los_Angeles")

solar_noon_mx <- solarnoon(matrix(c(lon_vec, lat_vec), 
                                  nrow = 429912),
                 as.POSIXct(date_vec), POSIXct.out = TRUE)

dates_all$solar_noon_time <- solar_noon_mx$time

dates_all <- dates_all %>%
  mutate(solar_noon_hour = hour(solar_noon_time),
         solar_noon_minutes = minute(solar_noon_time),
         solar_noon_seconds = second(solar_noon_time)) %>%
  mutate(solar_noon = hms::as_hms(paste(solar_noon_hour,
                                solar_noon_minutes,
                                solar_noon_seconds,
                                sep = ":"))) # PHEW

# Just trimming down to columns of interest
dates_all <- dates_all %>%
  select(date_times, hour, index, ID, 
         date, lat, lon, solar_noon)
  
# Now, to combine the maximum possible date-times
# with the actual data.
data_full_hourly <- full_join(dates_all, data_hourly,
                              by = c("ID", "date", "hour")) %>%
  # and create new column
  mutate(ID_index = paste(ID, index, sep = "_"))

#### TRIM ####

data_indexed_filtered <- data_full_hourly %>%
  drop_na(DO_mgL) %>% # removes rows where DO is NA
  # filter for days that pass the flags
  # 1 - deployment/retrieval days
  # 2 - sensor quality (Q) < 0.7
  # 3 - wiper function (wipe time < 4 sec or current > 140)
  # 4 - photo evidence of biofouling
  filter(Flag1 == "NO",
         Flag2 == "NO",
         Flag3 == "NO",
         Flag4 == "NO")

length(unique(data_indexed_filtered$ID_index)) # 7,448 site-days remaining in total

# And split into 2022 and 2023 datasets.
data_2022 <- data_indexed_filtered %>%
  # only use data prior to installing of cinderblocks at
  # far from stream sites
  filter(date >= ymd_hms("2022-04-28 04:00:00") &
           date < ymd_hms("2023-02-12 04:00:00")) %>%
  # Note BW has about a month's worth of data before this
  # and 4 months worth of data after this
  # at deeper depths which could be added back in...
  # only want BW and GB sites
  filter(site %in% c("BW", "GB")) %>%
  # and omit pelagic sensors
  filter(replicate %in% c("NS1", "NS2", "NS3", "Benthic")) %>%
  group_by(ID_index) %>%
  mutate(count = n()) %>%
  ungroup() %>% 
  # Ok, checked here to be sure we aren't getting values >24
  filter(count == 24) %>%
  # and scale calibration-corrected % saturation DO values
  mutate(scaled_DO_sat = scale(DO_sat),
         scaled_DO_mgL = scale(DO_mgL),
         scaled_temp = scale(Temp_C))

data_2023 <- data_indexed_filtered %>%
  # only use data after installation of cinderblocks at
  # near stream sites
  filter(date >= ymd_hms("2023-06-01 04:00:00")) %>%
  group_by(ID_index) %>%
  mutate(count = n()) %>%
  ungroup() %>% 
  # Ok, checked here to be sure we aren't getting values >24
  filter(count == 24) %>%
  # REMOVE DEPTHS > 3m
  filter(location == "3m") %>%
  # and scale % DO saturation values
  # scale = (x - mean(x))/sd(x)
  mutate(scaled_DO_sat = scale(DO_sat),
         scaled_DO_mgL = scale(DO_mgL),
         scaled_temp = scale(Temp_C))

# Finally, split into lists based on unique IDs.
data_2022_l <- split(data_2022, data_2022$ID_index)
data_2023_l <- split(data_2023, data_2023$ID_index)

#### EXPORT ####

# Save out these datasets for use in analyses and figure making.
saveRDS(data_2022,
        "data_working/do_data_2022_052325.rds")
saveRDS(data_2023,
        "data_working/do_data_2023_052325.rds")
saveRDS(data_2022_l,
        "data_working/do_data_2022_dailylist_052325.rds")
saveRDS(data_2023_l,
        "data_working/do_data_2023_dailylist_052325.rds")

# End of script.
