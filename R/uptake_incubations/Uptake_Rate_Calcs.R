#### Uptake Rate Calculation Workflow
#### June 22, 2023
#### Script created by: Heili Lowman

# This script is designed to edit raw N incubation data and generate
# a new datasheet of uptake rates.

### Setup ####

# Load packages.
library(tidyverse)
library(lubridate)
library(here)

# And creating function here to turn time-formatted hours into decimal hours.
# https://stackoverflow.com/questions/21781311/how-to-convert-time-to-decimal
decimateTime <- function(time) {
  time_num = as.numeric(unlist(strsplit(time, ":")))
  time_new = time_num[1] + time_num[2]/60 + time_num[3]/360
  return(time_new)
  }

# Load raw datasets.
dat_raw <- read_csv("data_raw/N_incubations/N_Incubation_Metadata_062223_Maysheet.csv")

# Check data structure.
str(dat_raw)

#### Calculate baseline spike concentrations ####

# Before looking at any of the triplicate incubations, we must calculate
# the mean starting concentration for each spike (NH4 & NO3) in both
# kinds of lake water to account for any pre-existing concentrations of either.

# Add columns with concentrations in ug-N/L
dat_raw <- dat_raw %>%
  mutate(Conc_µgNL = 1000*Conc_mgNL) %>%
  # If hours are blank, assign an incubation time of 6 hours.
  # Else, make hours of incubation time numeric.
  rowwise() %>%
  mutate(Actual_incubation_hrs_dec = ifelse(is.na(Actual_incubation_hrs) == TRUE, 6, 
                                            decimateTime(as.character(Actual_incubation_hrs)))) %>%
  ungroup()

# Filter for only initial measurements of spikes in lake water.
dat_raw_before <- dat_raw %>%
  filter(Location %in% c("BW", "GB")) %>%
  filter(is.na(Vial_num) == TRUE) %>%
# And remove any flagged values.
  filter(Flag == "N")

# Group by spike and calculated mean concentrations.
dat_avg_before <- dat_raw_before %>%
  group_by(Location, Analyte, Spike_µg_L) %>%
  summarize(before_mean_Conc_µgNL = mean(Conc_µgNL)) %>%
  ungroup()

#### Calculate change in incubation concentrations ####

# Next, we take the difference between final measured concentrations and
# original concentrations to examine the change over time. We also calculate
# this as a rate based on the amount of time everything was incubated.

# In order to do this, we first append the dataset created above to the
# larger dataset, creating essentially a new column with our mean "before"
# concentrations.
dat_raw <- left_join(dat_raw, dat_avg_before, 
                     by = c("Location", "Analyte", "Spike_µg_L")) # Yay!

# Now, we calculate the change in concentrations and the rate of change.
dat_raw <- dat_raw %>%
  mutate(delta_Conc_µgNL = Conc_µgNL - before_mean_Conc_µgNL) %>%
  mutate(delta_Conc_µgNLhr = delta_Conc_µgNL/Actual_incubation_hrs_dec)

#### Calculate net uptake rates ####

# Finally, we subtract mean rates of change in lakewater alone from the
# triplicate mean rates of change of subtrate incubations to determine the
# true net uptake rates of the substrate (i.e., sediment or biofilm) alone.

# To do so, we first calculate average uptake rates of lakewater.
# Filter for only lake water.
dat_raw_water <- dat_raw %>%
  filter(Type == "water") %>%
  # And remove any flagged values.
  filter(Flag == "N")

# Group by spike and calculated mean uptake rates.
dat_avg_water <- dat_raw_water %>%
  group_by(Location, Analyte, Spike_µg_L) %>%
  summarize(water_delta_Conc_µgNLhr = mean(delta_Conc_µgNLhr)) %>%
  ungroup()

# Join with the larger dataset.
dat_raw <- left_join(dat_raw, dat_avg_water, 
                     by = c("Location", "Analyte", "Spike_µg_L"))

# And calculate net uptake rates.
dat_raw <- dat_raw %>%
  mutate(net_delta_Conc_µgNLhr = delta_Conc_µgNLhr - water_delta_Conc_µgNLhr)

#### Export data. ####

# Let's tidy the dataset a bit so that we can better see the steps, and we'll
# only include the sediment and biofilm uptake results.
dat_tidy <- dat_raw %>%
  filter(Type %in% c("biofilm", "sediment")) %>%
  select(Location, Depth, Analyte, Spike_µg_L, Type, Flag:net_delta_Conc_µgNLhr)

# Export for use in Michaelis-Menten calculation script.
saveRDS(dat_tidy, "data_working/N_May_Incubation_Uptake_Rates_062223.rds")

# End of script.
