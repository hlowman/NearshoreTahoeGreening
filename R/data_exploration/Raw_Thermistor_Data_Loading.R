#### Thermistor Data
### November 9, 2022
## Heili Lowman

# The following script will compile some of the available thermistor data for
# Sally to plot up in MATLAB.

#### Setup ####

# Load packages.
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

# Load data.
# Glenbrook.
temp37 <- read_excel("data_raw/rawHOBO/GB20/21116499-37ft 2022-10-18 15-05-42 PDT (Data PDT).xlsx")
temp40 <- read_excel("data_raw/rawHOBO/GB20/21358078-40ft 2022-10-18 15-04-43 PDT (Data PDT).xlsx")
temp42.7 <- read_excel("data_raw/rawHOBO/GB20/21358079-427ft 2022-10-18 15-04-01 PDT (Data PDT).xlsx")
temp45.4 <- read_excel("data_raw/rawHOBO/GB20/21358080-454ft 2022-10-18 15-03-30 PDT (Data PDT).xlsx")
temp48.1 <- read_excel("data_raw/rawHOBO/GB20/21358081-481ft 2022-10-18 15-02-36 PDT (Data PDT).xlsx")
temp50.8 <- read_excel("data_raw/rawHOBO/GB20/21358082-508ft 2022-10-18 15-01-39 PDT (Data PDT).xlsx")
temp53.5 <- read_excel("data_raw/rawHOBO/GB20/21358083-535ft 2022-10-18 15-00-15 PDT (Data PDT) (1).xlsx")
temp56.2 <- read_excel("data_raw/rawHOBO/GB20/21358084-562ft 2022-10-18 14-58-49 PDT (Data PDT).xlsx")
temp58.9 <- read_excel("data_raw/rawHOBO/GB20/21358085-589ft 2022-10-18 14-57-53 PDT (Data PDT) (1).xlsx")
temp61.6 <- read_excel("data_raw/rawHOBO/GB20/21358086-616ft 2022-10-18 14-54-30 PDT (Data PDT).xlsx")
temp64.3 <- read_excel("data_raw/rawHOBO/GB20/21358087-643ft 2022-10-18 14-56-01 PDT (Data PDT) (1).xlsx")

# Blackwood
temp13.5 <- read_excel("data_raw/rawHOBO/BW20/21358088 2022-10-17 14_38_46 PDT (13.5m).xlsx")
temp9 <- read_excel("data_raw/rawHOBO/BW20/21358089 2022-10-17 14_20_08 PDT (9m).xlsx")
temp12 <- read_excel("data_raw/rawHOBO/BW20/21358090 2022-10-17 14_22_40 PDT (12m).xlsx")
temp6 <- read_excel("data_raw/rawHOBO/BW20/21358093 2022-10-17 14_16_52 PDT (6m).xlsx")

# And barometric pressure data for later.
# Source: Synoptic Station F9917 (Glenbrook)
# Excess rows of metadata trimmed.
gb_weather <- read_csv("data_raw/SynopticDownloads/F9917.2022-11-08_tidy.csv")
# Note - Synoptic data is in UTC, so need to convert to PST.
gb_weather$Date_TimePDT <- with_tz(gb_weather$Date_Time, 
                                    "US/Pacific")
gb_weather_trim <- gb_weather %>%
  select(Station_ID, Date_TimePDT, pressure_set_1d)

#### Glenbrook temperature data ####

# Tidy data.
temp37 <- temp37 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )",
         "Light_lux" = "Ch: 2 - Light   (lux)") %>%
  mutate(depth = 11.3) # feet to meters

temp40 <- temp40 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 12.2) # feet to meters

temp42.7 <- temp42.7 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 13) # feet to meters

temp45.4 <- temp45.4 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 13.8) # feet to meters

temp48.1 <- temp48.1 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 14.7) # feet to meters

temp50.8 <- temp50.8 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 15.5) # feet to meters

temp53.5 <- temp53.5 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 16.3) # feet to meters

temp56.2 <- temp56.2 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 17.1) # feet to meters

temp58.9 <- temp58.9 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 18) # feet to meters

temp61.6 <- temp61.6 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 18.8) # feet to meters

temp64.3 <- temp64.3 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 19.6) # feet to meters

# Trim the light data from the first depth.
temp37t <- temp37 %>%
  select(-Light_lux)

# Join datasets together.
j1 <- rbind(temp37t, temp40)
j2 <- rbind(j1, temp42.7)
j3 <- rbind(j2, temp45.4)
j4 <- rbind(j3, temp48.1)
j5 <- rbind(j4, temp50.8)
j6 <- rbind(j5, temp53.5)
j7 <- rbind(j6, temp56.2)
j8 <- rbind(j7, temp58.9)
j9 <- rbind(j8, temp61.6)
temp_all <- rbind(j9, temp64.3)

# Create a decimal date column.
temp_all <- temp_all %>%
  mutate(doy = yday(Date_TimePDT),
         hour = hour(Date_TimePDT)) %>%
  mutate(hour_decimal = hour/24) %>%
  mutate(date_decimal = doy + hour_decimal)

# And reformat the long dataset.
temp_all_long <- temp_all %>%
  select(Record, Date_TimePDT, date_decimal, Temp_C, depth)

# Also to wide format.
temp_all_wide <- temp_all_long %>%
  pivot_wider(names_from = depth, values_from = Temp_C)

# Export both files.
write_csv(temp_all_long, "data_working/GB20m_Thermistor_032223_long.csv")
write_csv(temp_all_wide, "data_working/GB20m_Thermistor_032223_wide.csv")

#### Blackwood temperature data ####

# Tidy data.
temp6 <- temp6 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 6) # meters

temp9 <- temp9 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 9) # meters

temp12 <- temp12 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 12) # meters

temp13.5 <- temp13.5 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 13.5) # meters

# Join datasets together.
k1 <- rbind(temp6, temp9)
k2 <- rbind(k1, temp12)
temp_all_bw <- rbind(k2, temp13.5)

# Create a decimal date column.
temp_all_bw <- temp_all_bw %>%
  mutate(doy = yday(Date_TimePDT),
         hour = hour(Date_TimePDT)) %>%
  mutate(hour_decimal = hour/24) %>%
  mutate(date_decimal = doy + hour_decimal)

# And reformat the long dataset.
temp_all_bw_long <- temp_all_bw %>%
  select(Record, Date_TimePDT, date_decimal, Temp_C, depth)

# Also to wide format.
temp_all_bw_wide <- temp_all_bw_long %>%
  pivot_wider(names_from = depth, values_from = Temp_C)

# Well, looks like many of these data are missing from 9 & 12m depths.

# Export both files.
write_csv(temp_all_long, "data_working/BW20m_Thermistor_032223_long.csv")
write_csv(temp_all_wide, "data_working/BW20m_Thermistor_032223_wide.csv")

#### Formatting DO data for Sally ####
gb20b <- read_csv("data_raw/CleanedDO/GB20/GB20m_20221018_miniDOTbenthic.csv")
gb20p <- read_csv("data_raw/CleanedDO/GB20/GB20mpelagic20230204.csv")

# Trim datasets to match.
gb20p <- gb20p %>%
  filter(PCT < mdy_hms("10/18/2022 00:00:00"))

gb20b <- gb20b %>%
  mutate(PCT = mdy_hm(PCT)) %>%
  filter(PCT < mdy_hm("10/18/2022 0:00"))

# Check timezones.
head(gb20p$PCT)

gb20p <- gb20p %>%
  force_tz(PCT, tz = "US/Pacific")

head(gb20p$PCT)

head(gb20b$PCT)

gb20b <- gb20b %>%
  force_tz(PCT, tz = "US/Pacific") %>%
  mutate(depth = 19.6)

head(gb20b$PCT)

# Tidy & join data.
gb20p_trim <- gb20p %>%
  rename("serial" = "serial.x",
         "site" = "site.x") %>%
  select("...1", "PCT", "V", "Temp", "DO", "DOT.sat", "Q", "serial", "site") %>%
  mutate(depth = 11.3)

gb20_do_join <- rbind(gb20p_trim, gb20b)

gb20_do_join <- gb20_do_join %>%
  drop_na(DO) %>% # remove NA rows
  rename("Date_TimePDT" = "PCT") # rename date column

# Create a decimal date.
gb20_do_join <- gb20_do_join %>%
  mutate(doy = yday(Date_TimePDT),
         hour = hour(Date_TimePDT)) %>%
  mutate(hour_decimal = hour/24) %>%
  mutate(date_decimal = doy + hour_decimal)

# And reformat the long dataset.
gb20_do_join <- gb20_do_join %>%
  select(Date_TimePDT, date_decimal, Temp, DO, depth) %>%
  rename("Temp_C" = "Temp",
         "DO_mgL" = "DO")

# Add DO saturation.
# Need to first convert bp and then aggregate data by 15 minutes so 
# timestamps match up.
gb_weather_trim <- gb_weather_trim %>%
  mutate(pressure_mm_Hg = pressure_set_1d*0.00750062)

gb_weather_15 <- gb_weather_trim %>%
  group_by(round15 = lubridate::floor_date(Date_TimePDT, "15 minutes")) %>%
  summarize(meanP_Pascal = mean(pressure_set_1d, na.rm = TRUE),
            meanP_mmHg = mean(pressure_mm_Hg, na.rm = TRUE)) %>%
  ungroup()

gb20_do_15 <- gb20_do_join %>%
  group_by(depth,
           round15 = lubridate::floor_date(Date_TimePDT, "15 minutes")) %>%
  summarize(meanDO = mean(DO_mgL, na.rm = TRUE),
            meanT = mean(Temp_C, na.rm = TRUE)) %>%
  ungroup()

# And join with appropriate datasets.
gb20_15_full <- left_join(gb20_do_15, gb_weather_15, by = c("round15"))

# Calculation from Garcia and Gordon (1992) for oxygen saturation (Osat).
Osat_calc <- function(temp, bp){
  
  satO <-(exp(2.00907 + 3.22014 * (log((298.15-temp) / (273.15 + temp))) + 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5)) * 1.4276 * bp / 760
  
  satO
  
}

# Calculate oxygen saturation using the function above.
gb20_15_full <- gb20_15_full %>%
  mutate(Osat = Osat_calc(meanT, meanP_mmHg))

# Calculate % DO saturation.
gb20_15_full <- gb20_15_full %>%
  mutate(DOsat_perc = (meanDO/Osat)*100)

# Also to wide format.
gb20_do_wide <- gb20_15_full %>%
  # so, pivot_wide is weird if there's lots of other values,
  # so need to pull out only columns of interest before pivotting
  select(depth, round15, meanDO) %>%
  pivot_wider(names_from = depth, values_from = c(meanDO))

gb20_dosat_wide <- gb20_15_full %>%
  select(depth, round15, DOsat_perc) %>%
  pivot_wider(names_from = depth, values_from = c(DOsat_perc))

# Fix column names and add decimal date back in.
gb20_do_wide <- gb20_do_wide %>%
  mutate(Record = seq(1, length(round15))) %>%
  mutate(doy = yday(round15),
         hour = hour(round15)) %>%
  mutate(hour_decimal = hour/24) %>%
  mutate(date_decimal = doy + hour_decimal) %>%
  rename("Date_TimePDT" = "round15") %>%
  select(Record, Date_TimePDT, date_decimal, `11.3`, `19.6`)

gb20_dosat_wide <- gb20_dosat_wide %>%
  mutate(Record = seq(1, length(round15))) %>%
  mutate(doy = yday(round15),
         hour = hour(round15)) %>%
  mutate(hour_decimal = hour/24) %>%
  mutate(date_decimal = doy + hour_decimal) %>%
  rename("Date_TimePDT" = "round15") %>%
  select(Record, Date_TimePDT, date_decimal, `11.3`, `19.6`)

# Export both WIDE files.
write_csv(gb20_do_wide, "data_working/GB20m_DO_032223_wide.csv")
write_csv(gb20_dosat_wide, "data_working/GB20m_DOsat_032223_wide.csv")

# End of script.
