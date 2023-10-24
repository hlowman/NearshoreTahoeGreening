# Prelim Data Analysis for Nearshore Greening Project in Lake Tahoe
# Authors: Heili E. Lowman
# Date Created: 2023-10-24

# ---------------------------- README ---------------------------------

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)

#### Load Data ####

dat <- readRDS("data_working/flagged_all_100423.rds")

#### Remove Flagged Data ####

# First, need to calculate SD to remove outliers.
dat_summ <- dat %>%
  group_by(site, location, replicate) %>%
  summarize(meanO = mean(Dissolved_O_mg_L),
            sdO = sd(Dissolved_O_mg_L)) %>%
  ungroup()

# Join this with the larger dataset to create Flag5.
dat <- left_join(dat, dat_summ) %>%
  mutate(Flag5 = case_when(Dissolved_O_mg_L > (meanO + (3*sdO)) |
                           Dissolved_O_mg_L < (meanO - (3*sdO)) ~ "YES",
                                               TRUE ~ "NO"))

# For all flags marked "YES" I will remove those data.
dat_clean <- dat %>%
  filter(Flag1 == "NO",
         Flag2 == "NO",
         Flag3 == "NO",
         Flag4 == "NO",
         Flag5 == "NO")

# 1163344 records remaining, so 24% of records removed.

#### Initial Plots ####

(fig_bw_do <- ggplot(dat_clean %>%
                       filter(site %in% c("BW")) %>%
                       filter(Pacific_Standard_Time > 
                                ymd_hms("2022-03-01 00:00:00")) %>%
                       filter(Pacific_Standard_Time <
                                ymd_hms("2023-02-28 23:59:59")) %>%
                       mutate(location = factor(location,
                                                levels = c("20m", "15m",
                                                          "10m", "3m"))),
                             aes(x = ymd_hms(Pacific_Standard_Time), 
                                 y = Dissolved_O_mg_L)) +
                geom_point(color = "#4CA49E") +
                labs(x = "Date",
                     y = "Dissolved Oxygen (mg/L)") +
                theme_bw() +
                facet_grid(location~.))

# ggsave(("do_data_bw_102423.png"),
#        path = "figures",
#        width = 40,
#        height = 20,
#        units = "cm"
# )

(fig_gb_do <- ggplot(dat_clean %>%
                       filter(site %in% c("GB")) %>%
                       filter(Pacific_Standard_Time > 
                                ymd_hms("2022-03-01 00:00:00")) %>%
                       filter(Pacific_Standard_Time <
                                ymd_hms("2023-02-28 23:59:59")) %>%
                       mutate(location = factor(location,
                                                levels = c("20m", "15m",
                                                           "10m", "3m"))),
                     aes(x = ymd_hms(Pacific_Standard_Time), 
                         y = Dissolved_O_mg_L)) +
    geom_point(color = "#4CA49E") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_grid(location~.))

# ggsave(("do_data_gb_102423.png"),
#        path = "figures",
#        width = 40,
#        height = 20,
#        units = "cm"
# )

# End of script.
