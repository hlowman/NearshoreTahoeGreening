# Posthoc Analysis Data Prep Script
# Authors: Heili E. Lowman
# Date Created: 2024-10-11

# ---------------------------- README ---------------------------------
# The following script will  prep data to be fit by  
# post-dynamic time warping approaches.

#### Setup ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)
library(viridis)
library(patchwork)

# Load data.

# First load in raw DO lists.
# hourly timestep
dosat_2022 <- readRDS("data_working/do_data_2022_dailylist_092324.rds")
dosat_2023 <- readRDS("data_working/do_data_2023_dailylist_011025.rds")

# As well as clusters resulting from
# dynamic time warping analysis.
clusters_2022 <- readRDS("data_working/dtw_clusters_2022_110124.rds")
clusters_2023 <- readRDS("data_working/dtw_clusters_2023_011025.rds")

# Next load in precip.
# daily timestep
ppt_data <- read_csv("data_raw/PRISM_Precip_dat.csv")

# As well as light.
# hourly timestep
light_ns <- read_csv("data_raw/nearshore_NLDAS_light.csv")
light_os <- read_csv("data_raw/offshore_NLDAS_light.csv")

# And barometric pressure.
# 3 hour timestep
baro_ns <- read_csv("data_raw/nearshore_NLDAS_baro.csv")
baro_os <- read_csv("data_raw/offshore_NLDAS_baro.csv")

# And wind.
wind_ns <- read_csv("data_raw/nearshore_NLDAS_windsp.csv")
wind_os <- read_csv("data_raw/offshore_NLDAS_windsp.csv")

# Finally discharge.
q_data <- readRDS("data_working/discharge_aggregated_121223.rds")

#### Tidy ####

# Need to collapse lists into dataframes.
dosat_2022_df <- do.call(rbind.data.frame, dosat_2022)
dosat_2023_df <- do.call(rbind.data.frame, dosat_2023)

# Trim the precipitation dataset.
ppt_trim <- ppt_data %>%
  select(site, date, ppt_mm:vpdmax_hPa)

# Combine and trim the light dataset.
light_trim <- rbind(light_ns, light_os) %>%
  mutate(date = date(datetime)) %>%
  rename(hour = Hour) %>%
  select(site, date, hour, light)

# Combine and trim the pressure dataset.
bp_trim <- rbind(baro_ns, baro_os) %>%
  mutate(date = date(datetime)) %>%
  rename(hour = Hour) %>%
  select(site, date, hour, baro_Pa)

# Combine and trim the wind dataset.
ws_trim <- rbind(wind_ns, wind_os) %>%
  mutate(date = date(datetime)) %>%
  rename(hour = Hour) %>%
  select(site, date, hour, windsp_ms)

# Summarize and trim the discharge dataset.
# Summarizing here to the hour level, since "days"
# will be assigned 4am to 4am below. Hourly will
# match light and DO data (lowest common denominator).
q_trim <- q_data %>%
  mutate(date = date(datePST),
         hour = hour(datePST)) %>%
  group_by(Site, date, hour) %>%
  summarize(mean_q_cms = mean(dischargeCMS,
                              na.rm = TRUE)) %>%
  ungroup()

#### Join ####

# Joining the covariate data to each of the DO files since
# I'll be running slightly different analyses for them.

##### Clusters #####

# Need to join with cluster assignments, which will
# be the response variable in my logistic regression.
dosat_2022_df <- full_join(dosat_2022_df,
                            clusters_2022,
                            by = c("ID_index" = "uniqueID"))
dosat_2023_df <- full_join(dosat_2023_df,
                           clusters_2023,
                           by = c("ID_index" = "uniqueID"))

##### Precip #####

dosat_2022_df <- dosat_2022_df %>%
  # create new column for joining with precip data
  mutate(precip_site = case_when(site == "BW" &
                                   replicate == "NS1" ~ "BWNS1",
                                 site == "BW" &
                                   replicate == "NS2" ~ "BWNS2",
                                 site == "BW" &
                                   replicate == "NS3" ~ "BWNS3",
                                 site == "GB" &
                                   replicate == "NS1" ~ "GBNS1",
                                 site == "GB" &
                                   replicate == "NS2" ~ "GBNS2",
                                 site == "GB" &
                                   replicate == "NS3" ~ "GBNS3",
                                 site == "BW" &
                                   location %in% c("10m",
                                                   "15m",
                                                   "20m") ~ "BW10m",
                                 site == "GB" &
                                   location %in% c("10m",
                                                   "15m",
                                                   "20m") ~ "GB10m"))

dosat_2023_df <- dosat_2023_df %>%
  # create new column for joining with precip data
  mutate(precip_site = case_when(site == "BW" &
                                   replicate == "NS1" ~ "BWNS1",
                                 site == "BW" &
                                   replicate == "NS2" ~ "BWNS2",
                                 site == "BW" &
                                   replicate == "NS3" ~ "BWNS3",
                                 site == "GB" &
                                   replicate == "NS1" ~ "GBNS1",
                                 site == "GB" &
                                   replicate == "NS2" ~ "GBNS2",
                                 site == "GB" &
                                   replicate == "NS3" ~ "GBNS3",
                                 site == "SS" &
                                   replicate == "NS1" ~ "SSNS1",
                                 site == "SS" &
                                   replicate == "NS2" ~ "SSNS2",
                                 site == "SS" &
                                   replicate == "NS3" ~ "SSNS3",
                                 site == "SH" &
                                   replicate == "NS1" ~ "SHNS1",
                                 site == "SH" &
                                   replicate == "NS2" ~ "SHNS2",
                                 site == "SH" &
                                   replicate == "NS3" ~ "SHNS3"))

dosat_ppt_22 <- left_join(dosat_2022_df, ppt_trim,
                          by = c("precip_site" = "site",
                                 "date"))
dosat_ppt_23 <- left_join(dosat_2023_df, ppt_trim,
                          by = c("precip_site" = "site",
                                 "date"))

# quick test plot to be sure this joined as it should
ggplot(dosat_ppt_23, aes(x = date, y = ppt_mm)) +
  geom_line() + theme_bw() + facet_wrap(.~precip_site)


##### Light #####
dosat_ppt_22 <- dosat_ppt_22 %>%
  # create new column for joining with light data
  mutate(light_site = case_when(site == "BW" &
                                   replicate %in% c("NS1",
                                                    "NS2",
                                                    "NS3") ~ "BWNS2",
                                 site == "GB" &
                                   replicate %in% c("NS1",
                                                    "NS2",
                                                    "NS3")~ "GBNS2",
                                 site == "BW" &
                                   location %in% c("10m",
                                                   "15m") ~ "BW10m",
                                 site == "BW" &
                                   location == "20m" ~ "BW20m",
                                 site == "GB" &
                                   location == "10m" ~ "GB10m",
                                 site == "GB" &
                                   location == "15m" ~ "GB15m",
                                 site == "GB" &
                                   location == "20m" ~ "GB20m"))

dosat_ppt_23 <- dosat_ppt_23 %>%
  # create new column for joining with light data
  mutate(light_site = case_when(site == "BW" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "BWNS2",
                                site == "GB" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "GBNS2",
                                site == "SS" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "SSNS2",
                                site == "SH" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "SHNS2"))

dosat_ppt_lt_22 <- left_join(dosat_ppt_22, light_trim,
                          by = c("light_site" = "site",
                                 "date", "hour"))
dosat_ppt_lt_23 <- left_join(dosat_ppt_23, light_trim,
                             by = c("light_site" = "site",
                                    "date", "hour"))

##### Baro P #####
dosat_ppt_lt_22 <- dosat_ppt_lt_22 %>%
  # create new column for joining with pressure data
  mutate(bp_site = case_when(site == "BW" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "BWNS2",
                                site == "GB" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3")~ "GBNS2",
                                site == "BW" &
                                  location %in% c("10m",
                                                  "15m") ~ "BW10m",
                                site == "BW" &
                                  location == "20m" ~ "BW20m",
                                site == "GB" &
                                  location == "10m" ~ "GB10m",
                                site == "GB" &
                                  location == "15m" ~ "GB15m",
                                site == "GB" &
                                  location == "20m" ~ "GB20m"))

dosat_ppt_lt_23 <- dosat_ppt_lt_23 %>%
  # create new column for joining with pressure data
  mutate(bp_site = case_when(site == "BW" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "BWNS2",
                                site == "GB" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "GBNS2",
                                site == "SS" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "SSNS2",
                                site == "SH" &
                                  replicate %in% c("NS1",
                                                   "NS2",
                                                   "NS3") ~ "SHNS2"))

dosat_ppt_lt_bp_22 <- left_join(dosat_ppt_lt_22, bp_trim,
                             by = c("bp_site" = "site",
                                    "date", "hour"))
dosat_ppt_lt_bp_23 <- left_join(dosat_ppt_lt_23, bp_trim,
                             by = c("bp_site" = "site",
                                    "date", "hour"))

##### Wind #####
dosat_ppt_lt_bp_22 <- dosat_ppt_lt_bp_22 %>%
  # create new column for joining with windspeed data
  mutate(ws_site = case_when(site == "BW" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3") ~ "BWNS2",
                             site == "GB" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3")~ "GBNS2",
                             site == "BW" &
                               location %in% c("10m",
                                               "15m") ~ "BW10m",
                             site == "BW" &
                               location == "20m" ~ "BW20m",
                             site == "GB" &
                               location == "10m" ~ "GB10m",
                             site == "GB" &
                               location == "15m" ~ "GB15m",
                             site == "GB" &
                               location == "20m" ~ "GB20m"))

dosat_ppt_lt_bp_23 <- dosat_ppt_lt_bp_23 %>%
  # create new column for joining with windspeed data
  mutate(ws_site = case_when(site == "BW" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3") ~ "BWNS2",
                             site == "GB" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3") ~ "GBNS2",
                             site == "SS" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3") ~ "SSNS2",
                             site == "SH" &
                               replicate %in% c("NS1",
                                                "NS2",
                                                "NS3") ~ "SHNS2"))

dosat_ppt_lt_bp_ws_22 <- left_join(dosat_ppt_lt_bp_22, ws_trim,
                                by = c("ws_site" = "site",
                                       "date", "hour"))
dosat_ppt_lt_bp_ws_23 <- left_join(dosat_ppt_lt_bp_23, ws_trim,
                                by = c("ws_site" = "site",
                                       "date", "hour"))

# quick test plot to be sure this joined as it should
ggplot(dosat_ppt_lt_bp_ws_22, aes(x = date_times, 
                                  y = windsp_ms)) +
  geom_point() + theme_bw() + facet_wrap(location~site)

##### Discharge #####
# Join the first dataset with Q data.
dosat_ppt_lt_bp_ws_q_22 <- left_join(dosat_ppt_lt_bp_ws_22,
                                     q_trim,
                                   by = c("site" = "Site",
                                          "date", "hour"))

dosat_ppt_lt_bp_ws_23 <- dosat_ppt_lt_bp_ws_23 %>%
  # create new column for joining with windspeed data
  mutate(q_site = case_when(site %in% c("BW", "SS") ~ "BW",
                            site %in% c("GB", "SH") ~ "GB"))

# Join the second dataset with Q data.
dosat_ppt_lt_bp_ws_q_23 <- left_join(dosat_ppt_lt_bp_ws_23,
                                     q_trim,
                                     by = c("q_site" = "Site",
                                            "date", "hour"))

#### Summarize ####

# Generate summary metrics for use in posthoc analysis.

# This will properly group days (from 4am to 4am) by index
# rather than by date to aggregate covariates.
summary_22 <- dosat_ppt_lt_bp_ws_q_22 %>%
  group_by(site, location, replicate, 
           ID, index, ID_index) %>%
  summarize(cluster_1 = cluster_1[1],
            cluster_2 = cluster_2[1],
            group = group[1],
            min_dosat = min(DO_sat, na.rm = TRUE),
            max_dosat = max(DO_sat, na.rm = TRUE),
            mean_dosat = mean(DO_sat, na.rm = TRUE),
            min_wtemp = min(Temp_C, na.rm = TRUE),
            max_wtemp = max(Temp_C, na.rm = TRUE),
            mean_wtemp = mean(Temp_C, na.rm = TRUE),
            mean_ppt = mean(ppt_mm, na.rm = TRUE),
            min_atemp = mean(tmin_C, na.rm = TRUE),
            max_atemp = mean(tmax_C, na.rm = TRUE),
            mean_atemp = mean(tmean_C, na.rm = TRUE),
            min_light = min(light, na.rm = TRUE),
            max_light = max(light, na.rm = TRUE),
            mean_light = mean(light, na.rm = TRUE),
            sum_light = sum(light, na.rm = TRUE),
            mean_bp = mean(baro_Pa, na.rm = TRUE),
            min_ws = min(windsp_ms, na.rm = TRUE),
            max_ws = max(windsp_ms, na.rm = TRUE),
            mean_ws = mean(windsp_ms, na.rm = TRUE),
            min_q = min(mean_q_cms, na.rm = TRUE),
            max_q = max(mean_q_cms, na.rm = TRUE),
            mean_q = mean(mean_q_cms, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(delta_dosat = max_dosat - min_dosat,
         delta_wtemp = max_wtemp - min_wtemp,
         delta_atemp = max_atemp - min_atemp,
         delta_light = max_light - min_light,
         delta_ws = max_ws - min_ws,
         delta_q = max_q - min_q)

summary_23 <- dosat_ppt_lt_bp_ws_q_23 %>%
  group_by(site, location, replicate, 
           ID, index, ID_index) %>%
  summarize(cluster_1 = cluster_1[1],
            cluster_2 = cluster_2[1],
            group = group[1],
            min_dosat = min(DO_sat, na.rm = TRUE),
            max_dosat = max(DO_sat, na.rm = TRUE),
            mean_dosat = mean(DO_sat, na.rm = TRUE),
            min_wtemp = min(Temp_C, na.rm = TRUE),
            max_wtemp = max(Temp_C, na.rm = TRUE),
            mean_wtemp = mean(Temp_C, na.rm = TRUE),
            mean_ppt = mean(ppt_mm, na.rm = TRUE),
            min_atemp = mean(tmin_C, na.rm = TRUE),
            max_atemp = mean(tmax_C, na.rm = TRUE),
            mean_atemp = mean(tmean_C, na.rm = TRUE),
            min_light = min(light, na.rm = TRUE),
            max_light = max(light, na.rm = TRUE),
            mean_light = mean(light, na.rm = TRUE),
            sum_light = sum(light, na.rm = TRUE),
            mean_bp = mean(baro_Pa, na.rm = TRUE),
            min_ws = min(windsp_ms, na.rm = TRUE),
            max_ws = max(windsp_ms, na.rm = TRUE),
            mean_ws = mean(windsp_ms, na.rm = TRUE),
            min_q = min(mean_q_cms, na.rm = TRUE),
            max_q = max(mean_q_cms, na.rm = TRUE),
            mean_q = mean(mean_q_cms, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(delta_dosat = max_dosat - min_dosat,
         delta_wtemp = max_wtemp - min_wtemp,
         delta_atemp = max_atemp - min_atemp,
         delta_light = max_light - min_light,
         delta_ws = max_ws - min_ws,
         delta_q = max_q - min_q)

#### Export ####

# Export both datasets.
# saveRDS(summary_22,
#         "data_working/do_covariate_daily_data_2022_111924.rds")
# saveRDS(summary_23,
#         "data_working/do_covariate_daily_data_2023_011025.rds")

# End of script.
