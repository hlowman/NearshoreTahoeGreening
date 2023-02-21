#### Metabolism Modeling Workflow
#### February 15, 2023
#### Script created by: Heili Lowman

# This script is designed to combine Synoptic weather data, since
# solar radiation isn't available at all sites, for use in eventual
# metabolism modeling.

# This script will also be to combine cleaned DO data at sites.

# Load packages.
library(tidyverse)
library(lubridate)
library(here)

# Load raw datasets.
bw_syn_1 <- read_csv("data_raw/SynopticDownloads/D9413.2023-02-07_tidy.csv")
bw_syn_2 <- read_csv("data_raw/SynopticDownloads/HMDC1.2023-02-08_tidy.csv")

bw_do <- read_csv("data_raw/CleanedDO/BWNS1/BWNS1_20221017.csv")

#### Synoptic data compilation ####

# Pull out solar radiation & wind from station HMDC1
bw_syn_2_solar <- bw_syn_2 %>%
  select(Date_Time, solar_radiation_set_1, wind_speed_set_1) %>%
  rename(wind_speed_set_1_HMDC = wind_speed_set_1)

# Create additional date/timestamp columns to join by.
bw_syn_2_solar <- bw_syn_2_solar %>%
  mutate(Year = year(Date_Time),
         Month = month(Date_Time),
         Day = day(Date_Time),
         Hour = hour(Date_Time))

bw_syn_1 <- bw_syn_1 %>%
  mutate(Year = year(Date_Time),
         Month = month(Date_Time),
         Day = day(Date_Time),
         Hour = hour(Date_Time))

# Add to first dataset at station D9413
# Edited so I wasn't missing july data
bw_joined <- full_join(bw_syn_2_solar, bw_syn_1, by = c("Year", "Month", "Day", "Hour"))
# Note, this will throw an error message since D9413 collected
# data at 15 minute intervals while HMDC collected hourly.

# If data is missing, replace with data from HMDC, otherwise
# use existing D9413 data.
bw_joined$wind_speed_set_1_ed <- case_when(
  is.na(bw_joined$wind_speed_set_1) ==
    TRUE ~ bw_joined$wind_speed_set_1_HMDC,
  TRUE ~ bw_joined$wind_speed_set_1)

# Export for use in metab modeling.
saveRDS(bw_joined, "data_working/D9413_HMDC1solarwind_compiled_022123.rds")

#### DO data compilation ####

# At this time, only using one file's worth of data to model,
# so not compiling any DO data at the moment.

# End of script.
