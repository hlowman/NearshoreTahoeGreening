# Data cleaning of miniDOT data for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman, Jasmine A. Krause, Kelly A. Loria
# Date Created: 2023-09-06

# ---------------------------- README -------------------------------------
# The following script will adapt the original workflow developed by Jasmine
# and Kelly to read in raw miniDOT data, join with wiper data (if deployed),
# and assign data QAQC flags if data quality seems poor.

# Due to the sheer volume of the raw data (separate daily text files on each
# instrument deployed as well as a single concatenated file for each
# deployment), this workflow will retrieve raw files from the Blaszczak lab
# server (i.e., Pinyon) and then export compiled files to the
# "data_working/do_data_cleaning" folder which will be ignored on Github.

#### Setup ####

# Load packages
library(tidyverse)
library(lubridate)
library(here)

# Point working directory to the appropriate umbrella folder on the server.
setwd("R:/Blaszczak_Lab/Ongoing Projects/Tahoe Data/Raw Data/raw_miniDOT")
getwd()

#### Load in DO data ####

# Read in concatenated miniDOT data for a given instrument download.
df <- read.delim("GBNS3/20211001/7450-224208/GBNS320211001miniDOT.txt",
                 sep = ',',
                 skip = 8)

# We've skipped the first 8 lines because these contain only metadata spit
# out by the instrument.

#### Tidy DO data ####

# Rename headers in new data
colnames(df) <- c("Unix_Timestamp_second",
                  "UTC_Date_Time",
                  "Pacific_Standard_Time",
                  "Battery_Volt",
                  "Temperature_deg_C",
                  "Dissolved_O_mg_L",
                  "Dissolved_O_Saturation_perc",
                  "Q")

# Add descriptive metadata columns
df$serial_miniDOT <- c("7450-224208") # Instrument serial number
df$deploy <- c("2021-06-10") # Instrument deployment date
df$retrieve <- c("2021-10-01") # Instrument retrieval date
df$site <- c("GB") # Site identifier
df$location <- c("3m") # Sub-site location identifier by approx. water depth
df$replicate <- c("NS3")

# Check data format
str(df)

# Note, the PST column does "wind back" the clocks and "spring forward"
# during the hours when DST takes place.

# Format Pacific Date/Time column
df$PT <- ymd_hms(df$Pacific_Standard_Time)
str(df)

#### Flag 1 ####

# Flag data for removal based on deployment and retrieval days.
df <- df %>%
  mutate(Flag1 = case_when(date(PT) <= ymd(deploy) |
                             date(PT) >= ymd(retrieve) ~ "YES",
                                         TRUE ~ "NO"))

#### Flag 2 ####

# Flag data for removal based on sensor quality (Q) > 0.7.

df <- df %>%
  mutate(Flag2 = case_when(Q <= 0.7 ~ "YES",
                           TRUE ~ "NO"))

#### Load in wiper data ####

# If no wiper data present, see !Line 155! for code that can add in appropriate
# columns as NAs.

# Read in concatenated wiper data for a given instrument download (if available).
wiper <- read.delim("SSNS1/20211014/Wiper/5958-941908/SSNS120211014.txt",
                    sep = ',',
                    skip = 8)

# We've skipped the first 8 lines because these contain only metadata spit
# out by the instrument.

#### Tidy wiper data ####

# Rename headers in new data
colnames(wiper) <- c("Unix_Timestamp_second_wiper",
                  "UTC_Date_Time_wiper",
                  "Pacific_Standard_Time_wiper",
                  "Battery_Volt_wiper",
                  "Temperature_deg_C_wiper",
                  "Wipes_Completed",
                  #"Cal_Wipe_Time_second",
                  "Wipe_Time_second",
                  "Forward_Start_Current_mA", #"Start_Current_mA",
                  "Average_Current_mA",
                  "Reverse_Start_Current_mA", #"Peak_Current_mA",
                  "Final_Current_mA",
                  "Source_Resistance_Ohm")

# Add descriptive metadata columns
wiper$serial_wiper <- c("5958-941908") # Instrument serial number

# Check data format
str(wiper)

# Note, the wiper PST column does the exact same "wind back"/"spring forward"
# during DST hours as the miniDOT instrument.

# Format Pacific Date/Time column
wiper$PT_wiper <- ymd_hms(wiper$Pacific_Standard_Time_wiper)
str(wiper)

#### Join DO and wiper data ####

# DO data is on a 5 minute increment but wiper data is on a 2 hour increment.
# So, when joining, there will be gaps. But first, there need to be some columns
# in common that we can join by before filling in the wiper data.

df <- df %>%
  mutate(PT_date = date(PT),
         PT_hour = hour(PT))

wiper <- wiper %>%
  mutate(PT_date = date(PT_wiper),
         PT_hour = hour(PT_wiper)-1)

# Setting the "hour" for the wipers to actually be the hour previous
# since that is the data the wiper data would filter out/pertains to.

# Join first by date, then by hour.
# And check to be sure the full total of DO observations stays the same.
df_wiper <- full_join(df, wiper, by = c("PT_date", "PT_hour"))

# Now, to populate the remainder of the wiper data. 
# Need to apply wiper data to 2 hours prior.
df_wiper <- df_wiper %>%
  fill(Unix_Timestamp_second_wiper:PT_wiper, .direction = "up")

# See here for more info re: the fill() function.
# https://stackoverflow.com/questions/67960986/how-to-fill-the-gaps-with-values-present-in-each-column-in-a-dataframe-in-r

# ONLY USE THE BELOW CODE IF THERE IS NO WIPER DATA:
df$Unix_Timestamp_second_wiper <- NA
df$UTC_Date_Time_wiper <- NA
df$Pacific_Standard_Time_wiper <- NA
df$Battery_Volt_wiper <- NA
df$Temperature_deg_C_wiper <- NA
df$Wipes_Completed <- NA
df$Wipe_Time_second <- NA
df$Forward_Start_Current_mA <- NA
df$Average_Current_mA <- NA
df$Reverse_Start_Current_mA <- NA
df$Final_Current_mA <- NA
df$Source_Resistance_Ohm <- NA
df$serial_wiper <- NA
df$PT_wiper <- NA
df$PT_date <- NA
df$PT_hour <- NA

df_wiper <- df

#### Flag 3 ####

# Flag data for removal based on wiper time/current (must pass both).

df_wiper <- df_wiper %>%
  mutate(Flag3 = case_when(Wipe_Time_second <= 4 | 
                             Average_Current_mA >= 140 ~ "YES",
                           TRUE ~ "NO"))

#### Flag 4 ####

# Examine data so far.
ggplot(df_wiper %>%
         mutate(flags = case_when(Flag1 == "YES" ~ "1",
                                     Flag2 == "YES" ~ "2",
                                     Flag3 == "YES" ~ "3",
                                     TRUE ~ "None")), 
       aes(x = PT, y = Dissolved_O_mg_L, color = flags)) +
  geom_point()

# Export plot for future reference.
ggsave(filename = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/figures/do_data_cleaning/GBNS3_2021_090623.png",
       width = 12, 
       height = 5)

# Looking for indicators such as:
# (1) increasing amplitude of DO fluctuations that seems unnaturally large
# (2) dive photos that show biofouling

# Site Notes
# BWNS1 Oct 21 - Neither
# BWNS2 Oct 21 - Neither
# BWNS3 Oct 21 - Neither
# SSNS1 Oct 21 - Neither
# GBNS1 Oct 21 - Neither, but no wiper data
# GBNS2 Oct 21 - (2) starting around Sept. 1, but no wiperd data
# GBNS3 Oct 21 - Neither, but no wiper data

# Flag data for removal based on suspected biofouling.
df_wiper <- df_wiper %>%
  mutate(Flag4 = case_when(#date(PT) >= ymd("2021-09-01") ~ "YES",
    TRUE ~ "NO"))

# If flagging data for biofouling, examine data once more.
ggplot(df_wiper %>%
         mutate(flags = case_when(Flag1 == "YES" ~ "1",
                                  Flag2 == "YES" ~ "2",
                                  Flag3 == "YES" ~ "3",
                                  Flag4 == "YES" ~ "4",
                                  TRUE ~ "None")), 
       aes(x = PT, y = Dissolved_O_mg_L, color = flags)) +
  geom_point()

# Replace plot for future reference.
ggsave(filename = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/figures/do_data_cleaning/GBNS2_2021_090623.png",
       width = 12, 
       height = 5)

#### Export dataset ####

# Export rds file into this project
saveRDS(df_wiper, file = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/data_working/do_data_cleaning/flagged_GBNS3_090623.rds")

# End of script.
