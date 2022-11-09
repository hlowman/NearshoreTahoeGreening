#### Thermistor Data
### November 9, 2022
## Heili Lowman

# The following script will compile some of the available thermistor data for
# Sally to plot up in MATLAB.

# Load packages.
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

# Load data.
temp37 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21116499-37ft 2022-10-18 15-05-42 PDT (Data PDT).xlsx")
temp40 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358078-40ft 2022-10-18 15-04-43 PDT (Data PDT).xlsx")
temp42.7 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358079-427ft 2022-10-18 15-04-01 PDT (Data PDT).xlsx")
temp45.4 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358080-454ft 2022-10-18 15-03-30 PDT (Data PDT).xlsx")
temp48.1 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358081-481ft 2022-10-18 15-02-36 PDT (Data PDT).xlsx")
temp50.8 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358082-508ft 2022-10-18 15-01-39 PDT (Data PDT).xlsx")
temp53.5 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358083-535ft 2022-10-18 15-00-15 PDT (Data PDT) (1).xlsx")
temp56.2 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358084-562ft 2022-10-18 14-58-49 PDT (Data PDT).xlsx")
temp58.9 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358085-589ft 2022-10-18 14-57-53 PDT (Data PDT) (1).xlsx")
temp61.6 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358086-616ft 2022-10-18 14-54-30 PDT (Data PDT).xlsx")
temp64.3 <- read_excel("data_raw/BuoyDownloads/GB20m/Thermistor/20221018/21358087-643ft 2022-10-18 14-56-01 PDT (Data PDT) (1).xlsx")

# Tidy data.
temp37 <- temp37 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )",
         "Light_lux" = "Ch: 2 - Light   (lux)") %>%
  mutate(depth = 37)

temp40 <- temp40 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 40)

temp42.7 <- temp42.7 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 42.7)

temp45.4 <- temp45.4 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 45.4)

temp48.1 <- temp48.1 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 48.1)

temp50.8 <- temp50.8 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 50.8)

temp53.5 <- temp53.5 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 53.5)

temp56.2 <- temp56.2 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 56.2)

temp58.9 <- temp58.9 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 58.9)

temp61.6 <- temp61.6 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 61.6)

temp64.3 <- temp64.3 %>%
  rename("Record" = "#",
         "Date_TimePDT" = "Date-Time (PDT)",
         "Temp_C" = "Ch: 1 - Temperature   (°C )") %>%
  mutate(depth = 64.3)

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
write_csv(temp_all_long, "data_working/GB20m_Thermistor_110922_long.csv")
write_csv(temp_all_wide, "data_working/GB20m_Thermistor_110922_wide.csv")

# End of script.
