# Data cleaning of miniDOT data for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman, Jasmine A. Krause, Kelly A. Loria
# Date Created: 2023-10-04

# ---------------------------- README -------------------------------------
# The following script will join QAQC'ed miniDOT data.

# Due to the sheer volume of  data, this workflow will export compiled files to
# the "data_working/do_data_cleaning" folder which will be ignored on Github.

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

#### Load Data ####

# Load in all data from step1 script.

# Create a list of available files.
files <- list.files(path = "data_working/do_data_cleaning",
                 pattern = "flagged_*",
                 full.names = T)

# Read in files into a list.
dat_list <- lapply(files, function (x) data.table(readRDS(x)))

# Create function to reformat each of the datasets.
reformat <- function(df){
  
  trim <- df %>%
    # miniDOT time data
    select(UTC_Date_Time, Pacific_Standard_Time, 
           # site & miniDOT metadata
           site, location, replicate, deploy, retrieve, serial_miniDOT,
           # temp & DO data
           Temperature_deg_C, Dissolved_O_mg_L, Dissolved_O_Saturation_perc,
           # miniDOT QAQC data
           Battery_Volt, Q, 
           # wiper time data & metadata
           UTC_Date_Time_wiper, Pacific_Standard_Time_wiper, serial_wiper,
           # temp data
           Temperature_deg_C_wiper,
           # wiper QAQC data
           Battery_Volt_wiper, Wipe_Time_second, Average_Current_mA,
           # QAQC data flags
           Flag1, Flag2, Flag3, Flag4)%>%
    mutate(UTC_Date_Time = as.character(UTC_Date_Time),
           Pacific_Standard_Time = as.character(Pacific_Standard_Time),
           UTC_Date_Time_wiper = as.character(UTC_Date_Time_wiper),
           Pacific_Standard_Time_wiper = as.character(Pacific_Standard_Time_wiper))
  
  return(trim)
  
}

# Read in files into a list.
dat_trim_list <- lapply(dat_list, function (x) reformat(x))

# And bind all datasets into a single dataset.
dat_df <- rbindlist(dat_trim_list, fill = TRUE)

# OMG YAY!!!

# Export for safekeeping.
saveRDS(dat_df, "data_working/flagged_all_100423.rds")

#### Test Plots ####

(fig1 <- ggplot(dat_df %>%
                  filter(site %in% c("BW", "GB")) %>%
                  mutate(location = factor(location,
                                           levels = c("20m", "15m",
                                                      "10m", "3m"))), 
                aes(x = ymd_hms(Pacific_Standard_Time), 
                           y = Dissolved_O_mg_L,
                           color = replicate)) +
  geom_point() +
  labs(x = "Date",
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw() +
  facet_grid(location~site))

# ggsave(("combined_data_bwgb_100423.png"),
#        path = "figures",
#        width = 40,
#        height = 20,
#        units = "cm"
# )

# End of script.
