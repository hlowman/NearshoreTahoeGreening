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
  ungroup() %>%
  mutate(index = 0)

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
data_indexed <- data_hourly %>%
  group_by(site, location, replicate) %>%
  group_modify(~ indexFUN(.x)) %>%
  ungroup()
# Looks like this worked!

# Remove days with less than 24 hours of measurements.
data_indexed_complete <- data_indexed %>%
  group_by(site, location, replicate, index) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count == 24)

# And create a new character column by which to split apart
# all unique days.
data_indexed_complete <- data_indexed_complete %>%
  mutate(uniqueID = paste(site, location, replicate, 
                          index, sep="_"))

# And split into 2021/2022 and 2023 datasets.
data_2022 <- data_indexed_complete %>%
  # only use data prior to installing of cinderblocks at
  # far from stream sites
  filter(date < ymd("2023-02-12")) %>%
  # and scale DO values
  mutate(scaled_DO_mg_L = scale(DO_mg_L))

data_2023 <- data_indexed_complete %>%
  # only use data after installation of cinderblocks at
  # far from stream sites
  filter(date >= ymd("2023-02-12")) %>%
  # and scale DO values
  # scale = (x - mean(x))/sd(x)
  mutate(scaled_DO_mg_L = scale(DO_mg_L))

# Finally, split into lists based on unique IDs.
data_2022_l <- split(data_2022, data_2022$uniqueID)
data_2023_l <- split(data_2023, data_2023$uniqueID)

#### EXPORT ####

# Save out these datasets for use in analyses.
# saveRDS(data_2022_l, "data_working/do_data_2022_dailylist_053124.rds")
# saveRDS(data_2023_l, "data_working/do_data_2023_dailylist_053124.rds")

# End of script.
