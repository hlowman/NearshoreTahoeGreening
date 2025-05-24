# % Saturation Data Prep Script
# Authors: Heili E. Lowman
# Date Created: 2024-09-06

# ---------------------------- README ---------------------------------
# The following script will transform DO data, already
# with the instrument offset taken into account, into
# % saturation.

# Note, the original script used to download the barometric pressure
# data can be found at:
# https://github.com/kellyloria/Littoral-Lake-Metabolism/blob/e93fe82617e3c7aa3e7624f776efe9e1d4dd9ce4/climate_scripts/KAL_GLDAS_pressure.R

# And the original script used to process the barometric pressure data
# can be found at:
# https://github.com/kellyloria/Littoral-Lake-Metabolism/blob/e93fe82617e3c7aa3e7624f776efe9e1d4dd9ce4/climate_scripts/KAL_NLDAS_proc.R

#### SETUP ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

# Load data.
# DO & temperature data - 15 minute interval
data <- readRDS("data_raw/24_DO_offset.rds")
# Barometric pressure data - 3 hour interval
bpns_data <- read_csv("data_working/processed_baro/nearshore_NLDAS_baro.csv")
#bpos_data <- read_csv("data_working/processed_baro/offshore_NLDAS_baro.csv")

#### TIDY ####

# First, need to tidy the climate data a bit before joining
# the two datasets.
baro_near_data <- bpns_data %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%d-%mT%H:%M:%OS", tz = "UTC")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  select(site, datetime, baro_Pa) %>%
  # dat is ~4km resolution so called it from NLDAS 
  # based on center miniDOT in each array
  mutate(shore = case_when(site == "BWNS2" ~ "BW",
    site == "SHNS2" ~ "SH",
    site == "SSNS2" ~ "SS",
    site == "GBNS2" ~ "GB",
    TRUE ~ as.character(site))) %>%
  mutate(date = date(datetime),
         hour = hour(datetime)) %>%
  rename(datetime_bp = datetime,
         site_bp = site)

# using "nearshore" data for all buoys
# in each of the 4 locations monitored.
do_data <- data %>%
  mutate(date = date(Pacific_Standard_Time),
         hour = hour(Pacific_Standard_Time))

# Merge with DO data.
merged_data <- left_join(do_data, baro_near_data,
                         by = c("site" = "shore", "date", "hour"))

# And fill gaps.
merged_filled_data <- merged_data %>%
  group_by(site, location, replicate) %>%
  fill(baro_Pa, .direction = "down")

# Make a quick plot to be sure data populated correctly.
ggplot(merged_filled_data, 
       aes(x = date,
           y = baro_Pa,
           color = replicate)) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  facet_grid(site~location)
# ok, takes forever to plot but looks ok.

#### % Saturation ####

# Using the equation from Garcia & Gordon 1992.
# Assumes salinity = 0

o2.at.sat <- function(temp, bp){
  
  # first convert from Pa to millibars
  bp_mbar <- bp*0.01
  
  # then convert from millibars to mm Hg
  bp_mmHg <- bp_mbar*0.750061683
  
  # finally convert from mm Hg to atm
  bp_atm <- bp_mmHg/760
  
  # Conversion from mL/L (the usual output of the garcia eq.)
  # to mg/L per USGS memo 2011.03
  mgL.mlL <- 1.42905
  
  # this matches the `garcia-benson` option in the LM.o2.at.sat.R
  # in the LakeMetabolizer workflow
    
    satO_base <-(exp(2.00907 + 3.22014 * (log((298.15-temp) / (273.15 + temp))) + 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5))
    
    # satO = baseline[DO] * conversion from mL/L to mg/L * pressure
  
    satO_correctunits <- satO_base * mgL.mlL * bp_atm
    
    return(satO_correctunits)
  
}

dosat_data <- merged_filled_data %>%
  # calculate oxygen at saturation in mg/L
  mutate(do_eq = o2.at.sat(temp = Temperature_deg_C, 
                           bp = baro_Pa)) %>% 
  # divide measured oxygen (with correction) by oxygen at saturation
  mutate(o2_sat = Dissolved_Oxygen_offset/do_eq) %>%
  # multiply ratio by 100 to get in percent
  mutate(o2_sat100 = o2_sat*100)

#### Export ####

saveRDS(dosat_data, "data_working/24_DOsat_offset.rds")

# Other helpful code at:
# https://github.com/kellyloria/Littoral-Lake-Metabolism/blob/e93fe82617e3c7aa3e7624f776efe9e1d4dd9ce4/LM_data_agg.R
# https://github.com/kellyloria/Littoral-Lake-Metabolism/blob/e93fe82617e3c7aa3e7624f776efe9e1d4dd9ce4/saved_fxns/LM.o2.at.sat.R

# End of script.
