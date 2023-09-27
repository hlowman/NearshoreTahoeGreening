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
library(lubridate)
library(tidyverse)
library(here)

# Point working directory to the appropriate umbrella folder on the server.
setwd("R:/Blaszczak_Lab/Ongoing Projects/Tahoe Data/Raw Data/raw_miniDOT")
getwd()

#### Load in DO data ####

# Read in concatenated miniDOT data for a given instrument download.
df <- read.delim("SHNS3/20230525/7450-265933/SHNS320230525.txt",
                 sep = ',',
                 skip = 8)
# We've skipped the first 8 lines because these contain only metadata spit
# out by the instrument.

# If the concatenated file is corrupted, use the following code to manually
# read in all files in a given folder.
dlist <- list.files(path = "SHNS1/20230525/7450-099447",
                 pattern = "*Z.txt",
                 full.names = T)

df <- do.call("rbind", lapply(dlist, FUN = function(file) {
  read.table(file, skip = 3, sep=",") %>%
    mutate(filename = file) %>%
    mutate(folder = str_split_fixed(filename, pattern = "/", n = 4)) %>%
    mutate(date = str_split_fixed(folder[,4], pattern = " ", n = 2)) %>%
    mutate(Date = ymd(date[,1])) } ) )

df <- df %>%
  mutate(UTC = as_datetime(V1),
         PST = with_tz(UTC, tzone = "US/Pacific"),
         DOSat = NA) %>%
  select(V1, UTC, PST, V2, V3, V4, DOSat, V5)

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
df$serial_miniDOT <- c("7450-099447") # Instrument serial number
df$deploy <- c("2023-02-04") # Instrument deployment date
df$retrieve <- c("2023-05-25") # Instrument retrieval date
df$site <- c("SH") # Site identifier
df$location <- c("3m") # Sub-site location identifier by approx. water depth
df$replicate <- c("NS1") # "NS1" or "Pelagic/Benthic"

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

# If no wiper data present, see !Line 193! for code that can add in appropriate
# columns as NAs.

# Read in concatenated wiper data for a given instrument download (if available).
wiper <- read.delim("GBNS1/20230525/5958-514255/GBNS120230525_wiper.txt",
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
                  "Cal_Wipe_Time_second",
                  "Wipe_Time_second",
                  #"Forward_Start_Current_mA", 
                  "Start_Current_mA",
                  "Average_Current_mA",
                  #"Reverse_Start_Current_mA", 
                  "Peak_Current_mA",
                  "Final_Current_mA",
                  "Source_Resistance_Ohm")

# Add descriptive metadata columns
wiper$serial_wiper <- c("5958-514255") # Instrument serial number

# Check data format
str(wiper)

# Note, the wiper PST column does the exact same "wind back"/"spring forward"
# during DST hours as the miniDOT instrument.

# Format Pacific Date/Time column
wiper$PT_wiper <- ymd_hms(wiper$Pacific_Standard_Time_wiper)
str(wiper)

#### Join DO and wiper data ####

# DO data is on a 5 minute increment but wiper data is on a 2-6 hour increment.
# So, when joining, there will be gaps. But first, there need to be some columns
# in common that we can join by before filling in the wiper data.

df <- df %>%
  mutate(PT_date = date(PT),
         PT_hour = hour(PT))

wiper <- wiper %>%
  mutate(PT_date_raw = as.character(date(PT_wiper)),
    PT_hour = ifelse(hour(PT_wiper) > 0, hour(PT_wiper)-1, 23)) %>%
# had to add in this ifelse statement, because it would spit back a "-1"
# if the hour value ended up being 0 at midnight  
  mutate(PT_date = case_when(PT_hour == 23 ~ ymd(PT_date_raw)-days(1), 
                             TRUE ~ ymd(PT_date_raw))) %>%
# also had to add in this case_when statement, because it would not roll back
# to the previous day when subtracting at midnight
# note: the ifelse() function didn't play nice here, so forced to use case_when()
  filter(PT_date_raw >= df$deploy[1]) %>%
# and remove wiper data prior to deployment
  filter(PT_date_raw <= df$retrieve[1])
# and remove wiper data after retrieval (in case record is longer)

# Setting the "hour" for the wipers to actually be the hour previous
# since that is the data the wiper data would filter out/pertains to.

# Join first by date, then by hour.
# And check to be sure the full total of DO observations stays the same.
df_wiper <- full_join(df, wiper, by = c("PT_date", "PT_hour"))
# It may not in certain circumstances, due to differential lengths of wiper
# data, so be sure to double check this visually.

# And if the datasets have some additional wiper data at the end, 
# sometimes due to deployment discrepancies or DST, trim it.
df_wiper <- df_wiper %>%
  drop_na(Q)

# Now, to populate the remainder of the wiper data. 
# Need to apply wiper data to hours prior.
df_wiper <- df_wiper %>%
  fill(Unix_Timestamp_second_wiper:PT_wiper, .direction = "up")

# See here for more info re: the fill() function.
# https://stackoverflow.com/questions/67960986/how-to-fill-the-gaps-with-values-present-in-each-column-in-a-dataframe-in-r

## !!! !!! !!! CAUTION !!! !!! !!! ##

# ONLY USE THE BELOW CODE IF THERE IS NO WIPER DATA:

# df$Unix_Timestamp_second_wiper <- NA
# df$UTC_Date_Time_wiper <- NA
# df$Pacific_Standard_Time_wiper <- NA
# df$Battery_Volt_wiper <- NA
# df$Temperature_deg_C_wiper <- NA
# df$Wipes_Completed <- NA
# df$Wipe_Time_second <- NA
# df$Forward_Start_Current_mA <- NA
# df$Average_Current_mA <- NA
# df$Reverse_Start_Current_mA <- NA
# df$Final_Current_mA <- NA
# df$Source_Resistance_Ohm <- NA
# df$serial_wiper <- NA
# df$PT_wiper <- NA
# df$PT_date <- NA
# df$PT_hour <- NA
# 
# df_wiper <- df

#### Flag 3 ####

# Flag data for removal based on wiper time/current (must pass both).
# Both of these column names are present despite the different wiper data 
# formats.
df_wiper <- df_wiper %>%
  # flag when wipe time is too short a.k.a. blocked from completing a full wipe
  mutate(Flag3 = case_when(Wipe_Time_second <= 4 | 
  # flag when wipe current is too high a.k.a. takes more energy that
    # usual to wipe
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
ggsave(filename = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/figures/do_data_cleaning/SHNS1_spring2023_092723.png",
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
# GBNS2 Oct 21 - (1) starting around Sept. 1, no wiper data
# GBNS3 Oct 21 - Neither, but no wiper data
# BWNS1 May 22 - Neither
# BWNS2 May 22 - Neither, but *** REVIEW WITH GROUP ***
# BWNS3 May 22 - Neither, but *** REVIEW WITH GROUP *** based on wiper flags
# BW20m Mar 22 - Neither, although *slightly* green in photos
# SSNS1 Mar 22 - (2) & (1) starting around Feb. 25 (already wiper flagged)
# GBNS1 May 22 - (1) starting around Jan. 1 (already wiper flagged)
# GBNS2 May 22 - (1) starting around Jan. 1 (already wiper flagged)
# GB10m Apr 22 - Neither, but no wiper data or photos
# GB15m Apr 22 - Neither and bonus wiper data
# BW10m Jul 22 - Neither (no photos)
# BW15m Jul 22 - Neither (no photos)
# BW20m Jul 22 - Neither (no photos) - wiper data in 10/23 pelagic folder
# GBNS3 Jul 22 - Neither, but *** REVIEW WITH GROUP ***
# GB10m Jul 22 - Neither (no photos)
# GB15m Jul 22 - Neither, but no wiper data or photos
# BWNS1 Oct 22 - Neither (no photos)
# BWNS3 Oct 22 - Neither (no photos)
# SSNS1 Oct 22 - (1) starting around Jul. 14
# GBNS1 Oct 22 - Neither (no photos)
# GBNS2 Oct 22 - Neither, but no wiper data or photos
# GBNS3 Oct 22 - Neither, but no wiper data or photos (looked similar enough
# to other GBNS data so ruled not biofouled)
# GB10m Oct 22 - Neither, but no wiper data or photos
# GB15m Oct 22 - Using GB15m from Feb 23 instead since has full record
# GB20mb Oct 22 - Using GB20mb from Feb 23 instead since has full wiper record
# BW10m Oct 22 - Neither (photos look clean in Sept.)
# BW15m Oct 22 - Neither, but *** REVIEW WITH GROUP ***
# BW20mp Oct 22 - Neither (photos look clean in Sept.)
# BW20mb Oct 22 - Neither, but no wiper data so *** REVIEW WITH GROUP ***
# GBNS1 Feb 23 - Looks like may have fallen over (based on wipers)
# GB15m Feb 23 - Neither
# GB20mp Feb 23 - Neither, although *slightly* green in photos
# GB20mb Feb 23 - Neither, although *slightly* green in photos
# GBNS1 May 23 - (1) starting around May 7 (already wiper flagged)
# GBNS2 May 23 - (1) starting around March 15 (no wiper data but likely due to NS1)
# GBNS3 May 23 - (1) starting around March 15 (no wiper data but likely due to NS1)
# SHNS1 May 23 - (1) Starting around Feb 21 (maybe was buried ?)
# SHNS2 May 23 - (1) Starting around May 1 (maybe also fell over ?)
# SHNS3 May 23 - (1) Starting around May 1 (not quite as extreme as NS2)

# Flag data for removal based on suspected biofouling.
df_wiper <- df_wiper %>%
  mutate(Flag4 = case_when(date(PT) >= ymd("2023-02-21") ~ "YES",
    TRUE ~ "NO"))

# If flagging data for biofouling, examine data once more.
ggplot(df_wiper %>%
         mutate(flags = case_when(Flag1 == "YES" ~ "1",
                                  Flag2 == "YES" ~ "2",
                                  Flag4 == "YES" ~ "4",
                                  Flag3 == "YES" ~ "3",
                                  TRUE ~ "None")), 
       aes(x = PT, y = Dissolved_O_mg_L, color = flags)) +
  geom_point()

# Replace plot for future reference.
ggsave(filename = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/figures/do_data_cleaning/SHNS1_spring2023_092723.png",
       width = 12,
       height = 5)

#### Export dataset ####

# Export rds file into this project
saveRDS(df_wiper, file = "//tsclient/C/Users/hlowman/Documents/NearshoreTahoeGreening/data_working/do_data_cleaning/flagged_SHNS1_092723.rds")

# End of script.
