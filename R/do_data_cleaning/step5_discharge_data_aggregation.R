# Data cleaning of miniDOT data for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman, Jasmine A. Krause, Kelly A. Loria
# Date Created: 2023-12-12

# ---------------------------- README -------------------------------------
# The following script will create unfiltered stream discharge data files.

# Due to the sheer volume of  data, this workflow will export compiled files to
# the "data_working/do_data_cleaning" folder which will be ignored on Github.

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dataRetrieval)

#### Load data ####

# Set necessary parameters for downloading using the USGS dataRetrieval package.
# Define the site numbers for "GB" and "BW"
siteNo_GB <- "10336730"
siteNo_BW <- "10336660"
# Define the parameter codes for flow and stage
pCode_flow <- "00060"
pCode_stage <- "00065"
# Set the start and end dates
start.date <- "2022-01-01"
end.date <- Sys.Date()  # Use the current date as the end date

# First, need to download sub-daily Glenbrook Creek data from USGS.
HRflow_data_GB <- readNWISuv(siteNumbers = siteNo_GB, 
                             parameterCd = pCode_flow, 
                             startDate = start.date, 
                             endDate = end.date,
                             tz = "America/Los_Angeles") %>%
  mutate(Site = "GB", label = "east") %>%
  select(datePST = "dateTime", dischargeCFS = "X_00060_00000", Site, label) %>%
  mutate(dischargeCMS = c(dischargeCFS*0.0283168))

# Also, need to download sub-daily Blackwood Creek data from USGS.
HRflow_data_BW <- readNWISuv(siteNumbers = siteNo_BW, 
                             parameterCd = pCode_flow, 
                             startDate = start.date, 
                             endDate = end.date,
                             tz = "America/Los_Angeles") %>%
  mutate(Site = "BW", label = "west") %>%
  select(datePST = "dateTime", dischargeCFS = "X_00060_00000", Site, label) %>%
  mutate(dischargeCMS = c(dischargeCFS*0.0283168))

#### Combine and export data ####
# Bind discharge data
both_discharge <- rbind(HRflow_data_BW, HRflow_data_GB)

# Export dataset.
saveRDS(both_discharge, "data_working/discharge_aggregated_121223.rds")

# End of script.
