#### Metabolism Modeling Workflow
#### February 15, 2023
#### Script created by: Heili Lowman

# This script is designed to combine Synoptic weather data, since
# solar radiation isn't available at all sites, for use in eventual
# metabolism modeling.

# Load packages.
library(tidyverse)
library(lubridate)
library(here)

# Load raw datasets.
bw_syn_1 <- read_csv("data_raw/SynopticDownloads/D9413.2023-02-07_tidy.csv")
bw_syn_2 <- read_csv("data_raw/SynopticDownloads/HMDC1.2023-02-08_tidy.csv")

# Pull out solar radiation from second dataset at station HMDC1
bw_syn_2_solar <- bw_syn_2 %>%
  select(Date_Time, solar_radiation_set_1)

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
bw_joined <- left_join(bw_syn_1, bw_syn_2_solar, by = c("Year", "Month", "Day", "Hour"))

# Export for use in metab modeling.
saveRDS(bw_joined, "D9413_HMDC1solar_compiled_021523.rds")

# End of script.
