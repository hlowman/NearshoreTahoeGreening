# Data cleaning of miniDOT data for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman, Jasmine A. Krause, Kelly A. Loria
# Date Created: 2023-10-25

# ---------------------------- README -------------------------------------
# The following script will join QAQC'ed miniDOT data with weather data.

# Due to the sheer volume of  data, this workflow will export compiled files to
# the "data_working/do_data_cleaning" folder which will be ignored on Github.

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

#### Load Data ####

# Import aggregated DO data with flags.
do_dat <- readRDS("data_working/flagged_all_100423.rds") %>%
  # convert characters to date format
  mutate(Pacific_Standard_Time = ymd_hms(Pacific_Standard_Time))

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

#### Combine DO and weather datasets ####

# Bind weather data
both_weather <- rbind(bw_both_trim, gb_trim)

# Need to first convert bp to mm Hg.
both_weather <- both_weather %>%
  mutate(pressure_mm_Hg = pressure_set_1d*0.00750062)

# Now need to round data to 15 minute intervals so timestamps match up.
weather_15 <- both_weather %>%
  group_by(shore,
           round15 = lubridate::round_date(Date_TimePST, "15 minutes")) %>%
  summarize(meanP_Pascal = mean(pressure_set_1d, na.rm = TRUE),
            meanP_mmHg = mean(pressure_mm_Hg, na.rm = TRUE),
            meanWspeed = mean(wind_speed_set_1, na.rm = TRUE),
            meanWdirect = mean(wind_direction_set_1, na.rm = TRUE),
            meanSolar = mean(solar_radiation_set_1, na.rm = TRUE)) %>%
  ungroup()

# And make matching timestamps in the DO data with which we can bind the two.
do_dat <- do_dat %>%
  mutate(shore = case_when(site %in% c("GB", "SH") ~ "E",
                           site %in% c("BW", "SS") ~ "W"),
         round15 = lubridate::round_date(Pacific_Standard_Time, "15 minutes"))

# Join the two datasets.
do_weather_dat <- left_join(do_dat, weather_15, by = c("shore", "round15"))

# So, in many cases, weather observations are missing, so I'm going to
# choose to backfill the missing observations for now so that we can
# properly caculate % DO saturation for all of our data.
do_weather_dat <- do_weather_dat %>%
  fill(meanP_Pascal:meanSolar, .direction = "up")

#### Calculate % DO Saturation ####

# Calculation from Benson and Krause (1984) for oxygen saturation (Osat).
# see USGS 2011 memo page A4 for calculations used
Osat_calc <- function(temp, bp){
  
  # baseline concentration of dissolved oxygen - temp in Kelvin
  dO <- exp(-139.34411 + 
              (1.575701e5 / temp) - 
              (6.642308e7 / (temp^2)) + 
              (1.243800e10 / (temp^3)) - 
              (8.621949e11 / (temp^4)))
  
  # salinity correction factor
  Fs <- 1
  
  # pressure correction factor - bp in atm
  u <- exp(11.8571 - (3840.70 / temp) - (216961 / (temp^2)))
  
  theta <- 0.000975 - (1.426e-5 * temp) + (6.436e-8 * (temp^2))
  
  Fp <- ((bp - u)*(1-(theta*bp)))/((1-u)*(1-theta))
  
  # saturated dissolved oxygen concentration (mg/L)
  satO <- dO * Fs * Fp
  
  satO
  
}

# Convert my data to Kelvin and atm to use the equation above.
do_weather_dat <- do_weather_dat %>%
  mutate(Temperature_deg_Kelvin = Temperature_deg_C + 273.15,
         Pressure_atm = meanP_mmHg/760)

# Calculate using the function above.
do_weather_sat_dat <- do_weather_dat %>%
  mutate(DOsat = Osat_calc(Temperature_deg_Kelvin, Pressure_atm))

# Calculate percentage.
do_weather_sat_dat <- do_weather_sat_dat %>%
  mutate(percDOsat = (Dissolved_O_mg_L/DOsat)*100)

# Export data.
saveRDS(do_weather_sat_dat, "data_working/do_sat_aggregated_110223.rds")

# End of script.
