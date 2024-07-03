# Dynamic Time Warping Data Prep Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will prep data to be fit by the 
# dynamic time warping approach.

#### SETUP ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

# Load data.
data <- readRDS("R/ts_modeling/shiny/do_sat_shiny_010924.rds")

#### TIDY ####

# STEP 1 - Assign daily indices.
data_hourly <- data %>%
  mutate(date = date(Pacific_Standard_Time),
         hour = hour(Pacific_Standard_Time)) %>%
  group_by(site, location, replicate, date, hour) %>%
  summarize(DO_mg_L = mean(Dissolved_O_mg_L, na.rm = TRUE)) %>%
  ungroup() #%>%
  # mutate(index = 0)

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
# rules to anticipate indexing proved too singular.
date_times <- seq(ymd_hms("2021-05-31 04:00:00"), ymd_hms("2023-10-01 03:00:00"),
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

# Repeat 17 times.
dates_all <- dates_all %>% 
  slice(rep(1:n(), 17))

sites <- rep(c("BW_10m_NS1", "BW_15m_NS1", "BW_20m_NS1", "BW_3m_NS1",
              "BW_3m_NS2", "BW_3m_NS3", "GB_10m_NS1", "GB_15m_NS1",
              "GB_20m_NS1", "GB_3m_NS1", "GB_3m_NS2", "GB_3m_NS3",
              "SH_3m_NS1", "SH_3m_NS2", "SH_3m_NS3", "SS_3m_NS1", "SS_3m_NS2"),
            each = 20472)

# And join with sites.
dates_all <- dates_all %>%
  mutate(ID = sites,
         date = date(date_times))

# Now, to combine the maximum possible date-times
# with the actual data.
data_full_hourly <- full_join(dates_all, data_hourly,
                              by = c("ID", "date", "hour")) %>%
  # and create new column
  mutate(ID_index = paste(ID, index, sep = "_"))

# Remove days with less than 24 hours of measurements.
data_indexed_complete <- data_full_hourly %>%
  drop_na(DO_mg_L) %>% # removes rows where DO is NA
  group_by(ID, index) %>%
  mutate(count = n()) %>%
  ungroup() %>% # Ok, checked here to be sure we aren't getting values >24
  filter(count == 24)

length(unique(data_indexed_complete$ID_index)) # 6,152 site-days remaining

# And split into 2021/2022 and 2023 datasets.
data_2022 <- data_indexed_complete %>%
  # only use data prior to installing of cinderblocks at
  # far from stream sites
  filter(date < ymd_hms("2023-02-12 04:00:00")) %>%
  # and scale DO values
  mutate(scaled_DO_mg_L = scale(DO_mg_L))

data_2023 <- data_indexed_complete %>%
  # only use data after installation of cinderblocks at
  # far from stream sites
  filter(date >= ymd_hms("2023-02-12 04:00:00")) %>%
  # and scale DO values
  # scale = (x - mean(x))/sd(x)
  mutate(scaled_DO_mg_L = scale(DO_mg_L))

# Finally, split into lists based on unique IDs.
data_2022_l <- split(data_2022, data_2022$ID_index)
data_2023_l <- split(data_2023, data_2023$ID_index)

#### EXPORT ####

# Save out these datasets for use in analyses.
# saveRDS(data_2022_l, "data_working/do_data_2022_dailylist_070324.rds")
# saveRDS(data_2023_l, "data_working/do_data_2023_dailylist_070324.rds")

# End of script.
