#### Initial Data Exploration
### September 26, 2022
## Heili Lowman

# The following script will accomplish some early data exploration ahead of the
# first all-hands meeting in late-September.

# NOTE - any plotly plots need to be prevented from being uploaded
# to GitHub because they are enormous.

# ----------------------------------README-------------------------------------
# Future to-do list:
# (1) Check wiper data in April/May 2022 for GB NS1 & NS2 (re: fouling).
# (2) Check to see what's going on at BW15 in the late summer/early fall.
# (3) ...

#### Setup ####

# Load packages
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)
library(webshot)
library(htmlwidgets)
library(patchwork)
library(R.matlab)

# Need to first test out how to load this format of text data files.

# List of column names
mylist <- c("Time_Sec", "BV_Volts", "Temp_C", "DO_mgL", "Q")

#### Test Workflow ####

# Load data
test_txt <- read_csv("data_raw/2021-10-28 221500Z.txt", skip = 3, col_names = mylist)

# Add file name to be turned into date
test_txt <- test_txt %>%
  mutate(txtfile = "2021-10-28 221500z.txt") %>%
  mutate(date = str_split_fixed(txtfile, pattern = " ", n = 2)) %>%
  mutate(Date = ymd(date[,1]))

# Now to try and iterate this over multiple files

# Create function to add filename and, by extension, date to each file as it is
# imported.
# Based on code found at :stackoverflow.com/questions/11433432/how-to-import-
# multiple-csv-files-at-once
read_plus_date <- function(flnm){
  
  read_csv(flnm, skip = 3, col_names = mylist) %>%
    mutate(filename = flnm) %>%
    mutate(folder = str_split_fixed(filename, pattern = "/", n = 10)) %>%
    mutate(date = str_split_fixed(folder[,10], pattern = " ", n = 2)) %>%
    mutate(Date = ymd(date[,1]))
  
}

# Figure out which directory to specify
# Note - initially got this working with a folder of n = 5
test2 <- read_plus_date("data_raw/BuoyDownloads/GB15m/7450-686243/2021-10-28 221500z.txt")

# ---------------------------------Glenbrook (GB)------------------------------

##### GB20m ####

###### Benthic #####

# Load in all files from the GB20m benthic directory
tbl_with_sources20b <- list.files(path = here("data_raw/BuoyDownloads/GB20m/Benthic/7450-227604"),
                                      pattern = "*.txt",
                                      full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources20b$date_time <- as_datetime(tbl_with_sources20b$Time_Sec)

# Trim and export data.
gb20b <- tbl_with_sources20b %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(gb20b, "data_working/GB20m_benthic_compiled_102622.csv")

###### Pelagic #####

# No data in this directory as of November 3, 2022.

##### GB15m ####

# Load in all files from the GB15m directory
tbl_with_sources <- list.files(path = here("data_raw/BuoyDownloads/GB15m/7450-686243"),
                               pattern = "*.txt",
                               full.names = T) %>%
  map_df(~read_plus_date(.))

# Convert 24 character to full date/time
tbl_with_sources$date_time <- as_datetime(tbl_with_sources$Time_Sec)

# GAH so I didn't even need the above lines re: folder/filenames. *sigh*

# Quick plot just before I move on

(ggplot(tbl_with_sources, aes(x = date_time, y = DO_mgL)) +
    geom_line() +
    labs(x = "Date", y = "DO (mg/L)") +
    theme_bw())

(ggplot(tbl_with_sources, aes(x = date_time, y = Temp_C)) +
    geom_line() +
    labs(x = "Date", y = "Temperature (C)") +
    theme_bw())

# Trim and export data.
gb15 <- tbl_with_sources %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(gb15, "data_working/GB15m_compiled_092622.csv")

# load in for additional compilation
gb15 <- read_csv("data_working/GB15m_compiled_092622.csv")

###### Oct 2022 Update #####
# So, the compilation above went through April, so I need to compile both July
# and October download dates.
# Load in all files from the GB15m July directory
tbl_with_sources15_july <- list.files(path = here("data_raw/BuoyDownloads/GB15m/20220714/7450-099447"),
                                 pattern = "*.txt",
                                 full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Load in all files from the GB15m October directory
tbl_with_sources15_oct <- list.files(path = here("data_raw/BuoyDownloads/GB15m/20221018/7450-099447"),
                                      pattern = "*.txt",
                                      full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources15_july$date_time <- as_datetime(tbl_with_sources15_july$Time_Sec)
tbl_with_sources15_oct$date_time <- as_datetime(tbl_with_sources15_oct$Time_Sec)

# Trim, join, and export data.
gb15_july <- tbl_with_sources15_july %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb15_oct <- tbl_with_sources15_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb15_1022 <- rbind(gb15_july, gb15_oct)
gb15_all <- rbind(gb15, gb15_1022)

write_csv(gb15_all, "data_working/GB15m_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(gb15_all$date_time, gb15_all$Temp_C) # Yep! :)

##### GB10m ####

# Load in all files from the GB10m directory
tbl_with_sources10 <- list.files(path = here("data_raw/BuoyDownloads/GB10m/20220428/7450-193411"),
                               pattern = "*.txt",
                               full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10$date_time <- as_datetime(tbl_with_sources10$Time_Sec)

# Trim and export data.
gb10 <- tbl_with_sources10 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(gb10, "data_working/GB10m_compiled_092622.csv")
# load in for additional compilation
gb10 <- read_csv("data_working/GB10m_compiled_092622.csv")

###### Oct 2022 Update #####
# So, the compilation above went through April, so I need to compile both July
# and October download dates.
# Load in all files from the GB10m July directory
tbl_with_sources10_july <- list.files(path = here("data_raw/BuoyDownloads/GB10m/20220714/7450-875894"),
                                      pattern = "*.txt",
                                      full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Load in all files from the GB10m October directory
tbl_with_sources10_oct <- list.files(path = here("data_raw/BuoyDownloads/GB10m/20221018/7450-875894"),
                                     pattern = "*.txt",
                                     full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10_july$date_time <- as_datetime(tbl_with_sources10_july$Time_Sec)
tbl_with_sources10_oct$date_time <- as_datetime(tbl_with_sources10_oct$Time_Sec)

# Trim, join, and export data.
gb10_july <- tbl_with_sources10_july %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb10_oct <- tbl_with_sources10_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb10_1022 <- rbind(gb10_july, gb10_oct)
gb10_all <- rbind(gb10, gb10_1022)

write_csv(gb10_all, "data_working/GB10m_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(gb10_all$date_time, gb10_all$Temp_C) # Yep! :)

##### GB3m ####

###### NS2 #####

# Load in all files from the GBNS2 directory
tbl_with_sources3 <- list.files(path = here("data_raw/BuoyDownloads/GBNS2/20220523/7450-224208"),
                                 pattern = "*.txt",
                                 full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3$date_time <- as_datetime(tbl_with_sources3$Time_Sec)

# Trim and export data.
gb3 <- tbl_with_sources3 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(gb3, "data_working/GBNS2_compiled_092622.csv")
# load in for compilation with other 3m sites
gbNS2 <- read_csv("data_working/GBNS2_compiled_092622.csv")

####### Oct 2022 Update ######
# Load in files from the GBNS2 October directory
tbl_with_sources3_oct <- list.files(path = here("data_raw/BuoyDownloads/GBNS2/20221018/7450-278010"),
                                pattern = "*.txt",
                                full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3_oct$date_time <- as_datetime(tbl_with_sources3_oct$Time_Sec)

# Trim, join, and export data.
gb3_oct <- tbl_with_sources3_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gbNS2_all <- rbind(gbNS2, gb3_oct)

#write_csv(gbNS2_all, "data_working/GBNS2_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(gbNS2_all$date_time, gbNS2_all$Temp_C) # Yep! :)

# Load in previous data to add Oct 2021 data in.
gbNS2_all <- read_csv("data_working/GBNS2_compiled_102622.csv")

# Load in files from the GBNS2 October directory
tbl_with_sources3_2021 <- list.files(path = here("data_raw/BuoyDownloads/GBNS2/20211001/7450-265933"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3_2021$date_time <- as_datetime(tbl_with_sources3_2021$Time_Sec)

# Trim, join, and export data.
gb3_2021 <- tbl_with_sources3_2021 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gbNS2_rev <- rbind(gb3_2021, gbNS2_all)

write_csv(gbNS2_rev, "data_working/GBNS2_compiled_110322.csv")

# Now, need to load in remaining shallow sites, because wasn't done in Sept.

###### NS1 #####

# Load in files from the GBNS1 May directory
tbl_with_sources3a_may <- list.files(path = here("data_raw/BuoyDownloads/GBNS1/20220523/7450-278010"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))
# Load in files from the GBNS1 October directory
tbl_with_sources3a_oct <- list.files(path = here("data_raw/BuoyDownloads/GBNS1/20221018/7450-224208"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3a_may$date_time <- as_datetime(tbl_with_sources3a_may$Time_Sec)
tbl_with_sources3a_oct$date_time <- as_datetime(tbl_with_sources3a_oct$Time_Sec)

# Trim, join, and export data.
gb3a_may <- tbl_with_sources3a_may %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb3a_oct <- tbl_with_sources3a_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gbNS1_all <- rbind(gb3a_may, gb3a_oct)

write_csv(gbNS1_all, "data_working/GBNS1_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(gbNS1_all$date_time, gbNS1_all$Temp_C) # Yep! :)

###### NS3 #####

# Load in files from the GBNS3 2021 directory
tbl_with_sources3c_21 <- list.files(path = here("data_raw/BuoyDownloads/GBNS3/7450-227604"),
                                     pattern = "*.txt",
                                     full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))
# Load in files from the GBNS3 July directory
tbl_with_sources3c_july <- list.files(path = here("data_raw/BuoyDownloads/GBNS3/20220706/7450-227604"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))
# Load in files from the GBNS3 October directory
tbl_with_sources3c_oct <- list.files(path = here("data_raw/BuoyDownloads/GBNS3/20221018/7450-193411"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3c_21$date_time <- as_datetime(tbl_with_sources3c_21$Time_Sec)
tbl_with_sources3c_july$date_time <- as_datetime(tbl_with_sources3c_july$Time_Sec)
tbl_with_sources3c_oct$date_time <- as_datetime(tbl_with_sources3c_oct$Time_Sec)

# Trim, join, and export data.
gb3c_21 <- tbl_with_sources3c_21 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb3c_july <- tbl_with_sources3c_july %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gb3c_oct <- tbl_with_sources3c_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
gbNS3 <- rbind(gb3c_21, gb3c_july)
gbNS3_all <- rbind(gbNS3, gb3c_oct)

write_csv(gbNS3_all, "data_working/GBNS3_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(gbNS3_all$date_time, gbNS3_all$Temp_C) # Yep! :)

####Blackwood (BW)####

##### BW20m ####

# Load in all files from the BW20m directory
tbl_with_sources20bw1 <- list.files(path = here("data_raw/BuoyDownloads/BW20m/20220324/7450-336792"),
                                pattern = "*.txt",
                                full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

tbl_with_sources20bw2 <- list.files(path = here("data_raw/BuoyDownloads/BW20m/20220715/benthic/7450-547404"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

tbl_with_sources20bw <- rbind(tbl_with_sources20bw1, tbl_with_sources20bw2)

# Convert 24 character to full date/time
tbl_with_sources20bw$date_time <- as_datetime(tbl_with_sources20bw$Time_Sec)

# Trim and export data.
bw20 <- tbl_with_sources20bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(bw20, "data_working/BW20m_compiled_092622.csv")

# load in for additional compilation
bw20 <- read_csv("data_working/BW20m_compiled_092622.csv")

###### Oct 2022 Update #####

###### Benthic #####

# Load in all files from the BW20m benthic directory
tbl_with_sources20bwb <- list.files(path = here("data_raw/BuoyDownloads/BW20m/Benthic/20221017/7450-547404"),
                                  pattern = "*.txt",
                                  full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources20bwb$date_time <- as_datetime(tbl_with_sources20bwb$Time_Sec)

# Trim and export data.
bw20b <- tbl_with_sources20bwb %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw20b, "data_working/BW20m_benthic_compiled_102622.csv")

# Join with data above to create longer benthic record.
bw20b_rev <- rbind(bw20, bw20b)

write_csv(bw20b_rev, "data_working/BW20m_benthic_compiled_110322.csv")

# Quick plot to see if all the data is there.
plot(bw20b_rev$date_time, bw20b_rev$Temp_C) # Yep! :)

###### Pelagic #####

# Load in all files from the BW20m pelagic directory
tbl_with_sources20bwp <- list.files(path = here("data_raw/BuoyDownloads/BW20m/Pelagic/20221017/7450-643897"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources20bwp$date_time <- as_datetime(tbl_with_sources20bwp$Time_Sec)

# Trim and export data.
bw20p <- tbl_with_sources20bwp %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw20p, "data_working/BW20m_pelagic_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(bw20p$date_time, bw20p$Temp_C) # Yep! :)

##### BW15m ####

# Load in all files from the BW15m directory
tbl_with_sources15bw <- list.files(path = here("data_raw/BuoyDownloads/BW15m/20220715/7450-666671"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources15bw$date_time <- as_datetime(tbl_with_sources15bw$Time_Sec)

# Trim and export data.
bw15 <- tbl_with_sources15bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(bw15, "data_working/BW15m_compiled_092622.csv")

# load in file for additional compilation
bw15 <- read_csv("data_working/BW15m_compiled_092622.csv")

###### Oct 2022 Update ####

# Load in all files from the BW15m October directory
tbl_with_sources15bw_oct <- list.files(path = here("data_raw/BuoyDownloads/BW15m/20221017/Benthic/7450-666671"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources15bw_oct$date_time <- as_datetime(tbl_with_sources15bw_oct$Time_Sec)

# Trim, join, and export data.
bw15_oct <- tbl_with_sources15bw_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bw15_full <- rbind(bw15, bw15_oct)

write_csv(bw15_full, "data_working/BW15m_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(bw15_full$date_time, bw15_full$Temp_C) # Yep! :)

##### BW10m ####

# Load in all files from the BW10m directory
tbl_with_sources10bw <- list.files(path = here("data_raw/BuoyDownloads/BW10m/20220715/7450-617000"),
                                   pattern = "*.txt",
                                   full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10bw$date_time <- as_datetime(tbl_with_sources10bw$Time_Sec)

# Trim and export data.
bw10 <- tbl_with_sources10bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(bw10, "data_working/BW10m_compiled_092622.csv")
# load in file for additional compilation
bw10 <- read_csv("data_working/BW10m_compiled_092622.csv")

###### Oct 2022 Update ####

# Load in all files from the BW10m October directory
tbl_with_sources10bw_oct <- list.files(path = here("data_raw/BuoyDownloads/BW10m/20221017/Benthic/7450-617000"),
                                       pattern = "*.txt",
                                       full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10bw_oct$date_time <- as_datetime(tbl_with_sources10bw_oct$Time_Sec)

# Trim, join, and export data.
bw10_oct <- tbl_with_sources10bw_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bw10_full <- rbind(bw10, bw10_oct)

write_csv(bw10_full, "data_working/BW10m_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(bw10_full$date_time, bw10_full$Temp_C) # Yep! :)

##### BW3m ####

###### NS2 #####

# Load in all files from the BWNS2 directory
tbl_with_sources3bw <- list.files(path = here("data_raw/BuoyDownloads/BWNS2/20220524/7450-287080"),
                                   pattern = "*.txt",
                                   full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3bw$date_time <- as_datetime(tbl_with_sources3bw$Time_Sec)

# Trim and export data.
bw3 <- tbl_with_sources3bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

#write_csv(bw3, "data_working/BWNS2_compiled_092622.csv")
# load data for additional compilation
bw3 <- read_csv("data_working/BWNS2_compiled_092622.csv")

# Quick plot to see if all the data is there.
plot(bw3$date_time, bw3$Temp_C) # Yep! :)

####### Oct 2022 Update ######

# No data in October directory as of November 3, 2022.

###### NS1 ######

# Load in files from the BWNS1 May directory
tbl_with_sources3bwa_may <- list.files(path = here("data_raw/BuoyDownloads/BWNS1/20220524/7450-174159"),
                                     pattern = "*.txt",
                                     full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))
# Load in files from the BWNS1 October directory
tbl_with_sources3bwa_oct <- list.files(path = here("data_raw/BuoyDownloads/BWNS1/20221017/Benthic/7450-276557"),
                                     pattern = "*.txt",
                                     full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3bwa_may$date_time <- as_datetime(tbl_with_sources3bwa_may$Time_Sec)
tbl_with_sources3bwa_oct$date_time <- as_datetime(tbl_with_sources3bwa_oct$Time_Sec)

# Trim, join, and export data.
bw3a_may <- tbl_with_sources3bwa_may %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bw3a_oct <- tbl_with_sources3bwa_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bwNS1_all <- rbind(bw3a_may, bw3a_oct)

write_csv(bwNS1_all, "data_working/BWNS1_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(bwNS1_all$date_time, bwNS1_all$Temp_C) # Yep! :)

###### NS3 ######

# Load in files from the BWNS3 May directory
tbl_with_sources3bwc_may <- list.files(path = here("data_raw/BuoyDownloads/BWNS3/20220524/7450-276557"),
                                       pattern = "*.txt",
                                       full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))
# Load in files from the BWNS1 October directory
tbl_with_sources3bwc_oct <- list.files(path = here("data_raw/BuoyDownloads/BWNS3/20221017/Benthic/7450-287080"),
                                       pattern = "*.txt",
                                       full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3bwc_may$date_time <- as_datetime(tbl_with_sources3bwc_may$Time_Sec)
tbl_with_sources3bwc_oct$date_time <- as_datetime(tbl_with_sources3bwc_oct$Time_Sec)

# Trim, join, and export data.
bw3c_may <- tbl_with_sources3bwc_may %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bw3c_oct <- tbl_with_sources3bwc_oct %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
bwNS3_all <- rbind(bw3c_may, bw3c_oct)

write_csv(bwNS3_all, "data_working/BWNS3_compiled_102622.csv")

# Quick plot to see if all the data is there.
plot(bwNS3_all$date_time, bwNS3_all$Temp_C) # Yep! :)

#### Trim and Join Sensor Data ####

# Trim values indicative of deployment and retrieval.

# Glenbrook sites
plot(gb20b$date_time, gb20b$DO_mgL) # just need to trim for deployment
gb20t <- gb20b %>%
  filter(date_time >= as_datetime("2022-07-15 00:00:00")) # trimming off first day
plot(gb20t$date_time, gb20t$DO_mgL)

plot(gb15_all$date_time, gb15_all$DO_mgL) # just need to trim for deployment
gb15t <- gb15_all %>%
  filter(date_time >= as_datetime("2021-10-30 00:00:00")) %>% # trimming off first day
  filter(date_time < as_datetime("2022-04-27 00:00:00") |
           date_time >= as_datetime("2022-04-29 00:00:00")) # And trimming may retrieval date 4/27-4/28.
plot(gb15t$date_time, gb15t$DO_mgL)

plot(gb10_all$date_time, gb10_all$DO_mgL) # just need to trim same dates
gb10t <- gb10_all %>%
  filter(date_time >= as_datetime("2021-10-30 00:00:00")) %>% # trimming off first day
  filter(date_time < as_datetime("2022-04-27 00:00:00") |
           date_time >= as_datetime("2022-04-29 00:00:00")) # And trimming may retrieval date 4/27-4/28.
plot(gb10t$date_time, gb10t$DO_mgL)

plot(gbNS2_rev$date_time, gbNS2_rev$DO_mgL) # need to trim anoxic period
gbNS2t <- gbNS2_rev %>%
  filter(date_time >= as_datetime("2021-06-12 00:00:00")) %>% # trimming off first day
  filter(date_time < as_datetime("2022-04-17 00:00:00") |
           date_time >= as_datetime("2022-05-06 00:00:00")) # And trimming april fouling
plot(gbNS2t$date_time, gbNS2t$DO_mgL)

plot(gbNS1_all$date_time, gbNS1_all$DO_mgL) # need to trim similar dates
gbNS1t <- gbNS1_all %>%
  filter(date_time >= as_datetime("2021-10-03 00:00:00")) %>% # trimming off first day
  filter(date_time < as_datetime("2022-04-17 00:00:00") |
           date_time >= as_datetime("2022-05-06 00:00:00")) # And trimming april fouling
plot(gbNS1t$date_time, gbNS1t$DO_mgL)

plot(gbNS3_all$date_time, gbNS3_all$DO_mgL) # need to trim similar dates
gbNS3t <- gbNS3_all %>%
  filter(date_time >= as_datetime("2021-10-03 00:00:00")) # trimming first day
plot(gbNS3t$date_time, gbNS3t$DO_mgL)

# Add a column to demarcate sensor ID.
gbNS3t$sensor <- "NS3"
gbNS2t$sensor <- "NS2"
gbNS1t$sensor <- "NS1"
gb10t$sensor <- NA
gb15t$sensor <- NA
gb20t$sensor <- NA

# Add a column to demarcate water depth.
gbNS3t$depth <- 3
gbNS2t$depth <- 3
gbNS1t$depth <- 3
gb10t$depth <- 10
gb15t$depth <- 15
gb20t$depth <- 20

# Join all Glenbrook datasets together.
gb_j1 <- rbind(gbNS3t, gbNS2t)
gb_j2 <- rbind(gb_j1, gbNS1t)
gb_j3 <- rbind(gb_j2, gb10t)
gb_j4 <- rbind(gb_j3, gb15t)
gb_all <- rbind(gb_j4, gb20t)

# Add a column to demarcate site.
gb_all$site <- "Glenbrook"

# And a column for location in the water column to match the bw columns
gb_all$location <- "Benthic"

# And re-order so the columns bind nicely below.
gb_all <- gb_all %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q, sensor, depth, location, site)

# Export to save progress.
saveRDS(gb_all, "data_working/GB_compiled_trimmed_110322.rds")

# Trim values indicative of deployment and retrieval.

# Blackwood sites
plot(bw20b_rev$date_time, bw20b_rev$DO_mgL) # need to trim a few places
bw20bt <- bw20b_rev %>%
  filter(date_time >= as_datetime("2021-10-30 00:00:00")) %>% # trimming off first day
  filter(date_time < as_datetime("2022-03-24 00:00:00") |
           date_time >= as_datetime("2022-03-25 00:00:00")) %>% # trimming next retrieval date
  # and trimming remaining max/min days
  filter(date_time < as_datetime("2022-07-09 00:00:00") |
           date_time >= as_datetime("2022-07-10 00:00:00")) %>%
  filter(date_time < as_datetime("2022-09-05 00:00:00") |
           date_time >= as_datetime("2022-09-06 00:00:00")) %>%
  filter(date_time < as_datetime("2022-10-13 00:00:00") |
           date_time >= as_datetime("2022-10-14 00:00:00"))
plot(bw20bt$date_time, bw20bt$DO_mgL)

plot(bw20p$date_time, bw20p$DO_mgL) # need to trim a few places
bw20pt <- bw20p %>%
  filter(date_time >= as_datetime("2022-07-17 00:00:00")) %>% # trimming off first day
  # and trimming remaining max/min days
  filter(date_time < as_datetime("2022-08-31 00:00:00") |
           date_time >= as_datetime("2022-09-02 00:00:00")) %>%
  filter(date_time < as_datetime("2022-09-24 00:00:00") |
           date_time >= as_datetime("2022-09-25 00:00:00")) %>%
  filter(date_time < as_datetime("2022-10-17 00:00:00"))
plot(bw20pt$date_time, bw20pt$DO_mgL)

plot(bw15_full$date_time, bw15_full$DO_mgL) # need to trim a few places
bw15t <- bw15_full %>%
  filter(date_time >= as_datetime("2022-03-25 00:00:00")) %>% # trimming off first day
  # and trimming remaining max/min days
  filter(date_time < as_datetime("2022-06-29 00:00:00") |
           date_time >= as_datetime("2022-06-30 00:00:00")) %>%
  filter(date_time < as_datetime("2022-07-09 00:00:00") |
           date_time >= as_datetime("2022-07-10 00:00:00")) %>%
  filter(date_time < as_datetime("2022-10-10 00:00:00"))
plot(bw15t$date_time, bw15t$DO_mgL)

plot(bw10_full$date_time, bw10_full$DO_mgL) # need to trim a few places
bw10t <- bw10_full %>%
  filter(date_time >= as_datetime("2022-03-25 00:00:00")) %>% # trimming off first day
  # and trimming remaining max/min days
  filter(date_time < as_datetime("2022-06-01 00:00:00") |
           date_time >= as_datetime("2022-06-02 00:00:00"))
plot(bw10t$date_time, bw10t$DO_mgL)

plot(bw3$date_time, bw3$DO_mgL) # need to trim a few places
bwNS2t <- bw3 %>%
  filter(date_time >= as_datetime("2021-10-17 00:00:00")) %>% # trimming off first day
  # and trimming last day
  filter(date_time < as_datetime("2022-05-24 00:00:00"))
plot(bwNS2t$date_time, bwNS2t$DO_mgL)

plot(bwNS1_all$date_time, bwNS1_all$DO_mgL) # need to trim a few places
bwNS1t <- bwNS1_all %>%
  filter(date_time >= as_datetime("2021-10-17 00:00:00")) %>% # trimming off first day
  # and trimming wonky day
  filter(date_time < as_datetime("2022-05-24 00:00:00") |
           date_time >= as_datetime("2022-05-25 00:00:00"))
plot(bwNS1t$date_time, bwNS1t$DO_mgL)

plot(bwNS3_all$date_time, bwNS3_all$DO_mgL) # need to trim same places
bwNS3t <- bwNS3_all %>%
  filter(date_time >= as_datetime("2021-10-17 00:00:00")) %>% # trimming off first day
  # and trimming wonky day
  filter(date_time < as_datetime("2022-05-24 00:00:00") |
           date_time >= as_datetime("2022-05-25 00:00:00"))
plot(bwNS3t$date_time, bwNS3t$DO_mgL)

# Add a column to demarcate sensor ID.
bwNS3t$sensor <- "NS3"
bwNS2t$sensor <- "NS2"
bwNS1t$sensor <- "NS1"
bw10t$sensor <- NA
bw15t$sensor <- NA
bw20pt$sensor <- NA
bw20bt$sensor <- NA

# Add a column to demarcate water depth.
bwNS3t$depth <- 3
bwNS2t$depth <- 3
bwNS1t$depth <- 3
bw10t$depth <- 10
bw15t$depth <- 15
bw20pt$depth <- 20
bw20bt$depth <- 20

# Add a column to demarcate location in the water column.
bwNS3t$location <- "Benthic"
bwNS2t$location <- "Benthic"
bwNS1t$location <- "Benthic"
bw10t$location <- "Benthic"
bw15t$location <- "Benthic"
bw20pt$location <- "Pelagic"
bw20bt$location <- "Benthic"

# Join all Blackwood datasets together.
bw_j1 <- rbind(bwNS3t, bwNS2t)
bw_j2 <- rbind(bw_j1, bwNS1t)
bw_j3 <- rbind(bw_j2, bw10t)
bw_j4 <- rbind(bw_j3, bw15t)
bw_j5 <- rbind(bw_j4, bw20bt)
bw_all <- rbind(bw_j5, bw20pt)

# Add a column to demarcate site.
bw_all$site <- "Blackwood"

# Export to save progress.
saveRDS(bw_all, "data_working/BW_compiled_trimmed_110322.rds")

# Join everythinggg
tahoe_all <- rbind(gb_all, bw_all)

# Export for future use.
write_csv(tahoe_all, "data_working/Tahoe_compiled_trimmed_110322.csv")
saveRDS(tahoe_all, "data_working/Tahoe_compiled_trimmed_110322.rds")

#### Plot ####

# Create a version of the dataset aggregated by hour to smooth the lines a bit.
tahoe_hourly <- tahoe_all %>%
  group_by(site, depth, location, sensor,
           hour = lubridate::floor_date(date_time, "1 hour")) %>%
  summarize(BV_Volts_mean = mean(BV_Volts, na.rm = TRUE),
            Temp_C_mean = mean(Temp_C, na.rm = TRUE),
            DO_mgL_mean = mean(DO_mgL, na.rm = TRUE),
            Q_mean = mean(Q, na.rm = TRUE)) %>%
  ungroup()
  
# Plots to explore initial DO values.
# 3m sites at Blackwood
(bw_do_fig_ns <- ggplot(tahoe_hourly %>% 
                       filter(site == "Blackwood") %>%
                         filter(depth == 3),
                    aes(x = hour, y = DO_mgL_mean)) +
  geom_line(aes(color = factor(sensor))) +
  scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
  labs(x = "Date",
       y = "DO (mg/L)",
       color = "Sensor Location",
       title = "Blackwood Nearshore (3m) Sensors") +
  theme_bw() +
  scale_x_datetime(date_breaks = "3 months"))

(bw_do_plotly_ns <- ggplotly(bw_do_fig_ns))

# Remaining sites at Blackwood
(bw_do_fig <- ggplot(tahoe_hourly %>% 
                          filter(site == "Blackwood") %>%
                          filter(depth != 3),
                        aes(x = hour, y = DO_mgL_mean)) +
    geom_line(aes(color = factor(depth),
                  linetype = factor(location))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO (mg/L)",
         color = "Water Depth",
         linetype = "Sensor Depth",
         title = "Blackwood Offshore Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(bw_do_plotly <- ggplotly(bw_do_fig))

# Export static paneled plot of Blackwood sites.
(bw_static <- bw_do_fig_ns + bw_do_fig +
    plot_annotation(tag_levels = "A") +
    plot_layout(nrow = 2))

# ggsave("figures/BW_DO_compiled_110322.png",
#        width = 40,
#        height = 30,
#        units = "cm")

# Export plotly plots for further exploration.
# saveWidget(as_widget(bw_do_plotly_ns), "plotly/BW_3m_DO_110322.html")
# saveWidget(as_widget(bw_do_plotly), "plotly/BW_to20m_DO_110322.html")

# 3m sites at Glenbrook
(gb_do_fig_ns <- ggplot(tahoe_hourly %>% 
                       filter(site == "Glenbrook") %>%
                       filter(depth == 3),
                     aes(x = hour, y = DO_mgL_mean)) +
    geom_line(aes(color = factor(sensor))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO (mg/L)",
         color = "Sensor Location",
         title = "Glenbrook Nearshore (3m) Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(gb_do_plotly_ns <- ggplotly(gb_do_fig_ns))

# Remaining sites at Glenbrook
(gb_do_fig <- ggplot(tahoe_hourly %>% 
                          filter(site == "Glenbrook") %>%
                          filter(depth != 3),
                        aes(x = hour, y = DO_mgL_mean)) +
    geom_line(aes(color = factor(depth),
                  linetype = factor(location))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO (mg/L)",
         color = "Water Depth",
         linetype = "Sensor Depth",
         title = "Glenbrook Offshore Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(gb_do_plotly <- ggplotly(gb_do_fig))

# Export static paneled plot of Glenbrook sites.
(gb_static <- gb_do_fig_ns + gb_do_fig +
    plot_annotation(tag_levels = "A") +
    plot_layout(nrow = 2))

# ggsave("figures/GB_DO_compiled_110322.png",
#        width = 40,
#        height = 30,
#        units = "cm")

# Export plotly plots for further exploration.
# saveWidget(as_widget(gb_do_plotly_ns), "plotly/GB_3m_DO_110322.html")
# saveWidget(as_widget(gb_do_plotly), "plotly/GB_to20m_DO_110322.html")

# End of script.
