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
library(patchwork)
library(MARSS)

#### Load Data ####

dat <- readRDS("data_working/do_sat_aggregated_110223.rds")
weather_long <- readRDS("data_working/weather_aggregated_long_110223.rds")

#### Remove Flagged Data ####

# First, need to calculate SD to remove outliers.
dat_summ <- dat %>%
  group_by(site, location, replicate) %>%
  summarize(meanpDO = mean(percDOsat),
            sdpDO = sd(percDOsat)) %>%
  ungroup()

# Join this with the larger dataset to create Flag5.
dat <- left_join(dat, dat_summ) %>%
  mutate(Flag5 = case_when(percDOsat > (meanpDO + (3*sdpDO)) |
                           percDOsat < (meanpDO - (3*sdpDO)) ~ "YES",
                                               TRUE ~ "NO"))

# For all flags marked "YES" I will remove those data.
dat_clean <- dat %>%
  filter(Flag1 == "NO",
         Flag2 == "NO",
         Flag3 == "NO",
         Flag4 == "NO",
         Flag5 == "NO")

# 1162968 records remaining, so 24% of records removed.

#### Initial Plots ####

##### BW 2022 #####
# DO
(fig_bw_do22 <- ggplot(dat_clean %>%
                       filter(site %in% c("BW")) %>%
                       filter(Pacific_Standard_Time > 
                                ymd_hms("2022-03-01 00:00:00")) %>%
                       filter(Pacific_Standard_Time <
                                ymd_hms("2023-02-28 23:59:59")) %>%
                       mutate(location = factor(location,
                                                levels = c("20m", "15m",
                                                          "10m", "3m")),
                              replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                           replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                           TRUE ~ "NS3"),
                                                 levels = c("NS1", "NS2", "NS3"))),
                             aes(x = Pacific_Standard_Time, 
                                 y = percDOsat,
                                 group = month(Pacific_Standard_Time),
                                 color = replicate)) +
                geom_line() +
                scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
                labs(x = "Date",
                     y = "DO (% Saturation)") +
                theme_bw() +
                theme(legend.position = "none") +
                facet_grid(location~.))
# Temperature
(fig_bw_t22 <- ggplot(dat_clean %>%
                       filter(site %in% c("BW")) %>%
                       filter(Pacific_Standard_Time > 
                                ymd_hms("2022-03-01 00:00:00")) %>%
                       filter(Pacific_Standard_Time <
                                ymd_hms("2023-02-28 23:59:59")) %>%
                       mutate(location = factor(location,
                                                levels = c("20m", "15m",
                                                           "10m", "3m")),
                              replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                           replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                           TRUE ~ "NS3"),
                                                 levels = c("NS1", "NS2", "NS3"))),
                     aes(x = Pacific_Standard_Time, 
                         y = Temperature_deg_C,
                         group = month(Pacific_Standard_Time),
                         color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~.))

# Combine the two and export.
(fig_bw22 <- fig_bw_do22 / fig_bw_t22)

# ggsave("figures/2022_data_bw_102523.png",
#        width = 20,
#        height = 20,
#        units = "cm"
# )

##### GB 2022 #####
# DO
(fig_gb_do22 <- ggplot(dat_clean %>%
                         filter(site %in% c("GB")) %>%
                         filter(Pacific_Standard_Time > 
                                  ymd_hms("2022-03-01 00:00:00")) %>%
                         filter(Pacific_Standard_Time <
                                  ymd_hms("2023-02-28 23:59:59")) %>%
                         mutate(location = factor(location,
                                                  levels = c("20m", "15m",
                                                             "10m", "3m")),
                                replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                             replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                             TRUE ~ "NS3"),
                                                   levels = c("NS1", "NS2", "NS3"))),
                       aes(x = Pacific_Standard_Time, 
                           y = percDOsat,
                           group = month(Pacific_Standard_Time),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   labs(x = "Date",
        y = "DO (% Saturation)") +
   theme_bw() +
   theme(legend.position = "none") +
   facet_grid(location~.))
# Temperature
(fig_gb_t22 <- ggplot(dat_clean %>%
                        filter(site %in% c("GB")) %>%
                        filter(Pacific_Standard_Time > 
                                 ymd_hms("2022-03-01 00:00:00")) %>%
                        filter(Pacific_Standard_Time <
                                 ymd_hms("2023-02-28 23:59:59")) %>%
                        mutate(location = factor(location,
                                                 levels = c("20m", "15m",
                                                            "10m", "3m")),
                               replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                            replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                            TRUE ~ "NS3"),
                                                  levels = c("NS1", "NS2", "NS3"))),
                      aes(x = Pacific_Standard_Time, 
                          y = Temperature_deg_C,
                          group = month(Pacific_Standard_Time),
                          color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~.))

# Combine the two and export.
(fig_gb22 <- fig_gb_do22 / fig_gb_t22)

# ggsave("figures/2022_data_gb_102523.png",
#        width = 20,
#        height = 20,
#        units = "cm"
# )

##### BW 2023 #####
# DO
(fig_bw_do23 <- ggplot(dat_clean %>%
                         filter(site %in% c("BW", "SS")) %>%
                         filter(location %in% c("10m", "3m")) %>%
                         filter(Pacific_Standard_Time > 
                                  ymd_hms("2023-05-01 00:00:00")) %>%
                         filter(Pacific_Standard_Time <
                                  ymd_hms("2023-09-30 23:59:59")) %>%
                         mutate(location = factor(case_when(site == "BW" &
                                                              location == "3m" ~ "3m (near)",
                                                            site == "SS" &
                                                              location == "3m" ~ "3m (far)",
                                                            TRUE ~ "10m"),
                                                  levels = c("10m", 
                                                             "3m (near)", 
                                                             "3m (far)")),
                                replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                             replicate %in% c("NS2") ~ "NS2",
                                                             TRUE ~ "NS3"),
                                                   levels = c("NS1", "NS2", "NS3"))),
                       aes(x = Pacific_Standard_Time, 
                           y = percDOsat,
                           group = month(Pacific_Standard_Time),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   labs(x = "Date",
        y = "DO (% Saturation)") +
   theme_bw() +
   theme(legend.position = "none") +
   facet_grid(location~.))
# Temperature
(fig_bw_t23 <- ggplot(dat_clean %>%
                        filter(site %in% c("BW", "SS")) %>%
                        filter(location %in% c("10m", "3m")) %>%
                        filter(Pacific_Standard_Time > 
                                 ymd_hms("2023-05-01 00:00:00")) %>%
                        filter(Pacific_Standard_Time <
                                 ymd_hms("2023-09-30 23:59:59")) %>%
                        mutate(location = factor(case_when(site == "BW" &
                                                             location == "3m" ~ "3m (near)",
                                                           site == "SS" &
                                                             location == "3m" ~ "3m (far)",
                                                           TRUE ~ "10m"),
                                                 levels = c("10m", 
                                                            "3m (near)", 
                                                            "3m (far)")),
                               replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                            replicate %in% c("NS2") ~ "NS2",
                                                            TRUE ~ "NS3"),
                                                  levels = c("NS1", "NS2", "NS3"))),
                      aes(x = Pacific_Standard_Time, 
                          y = Temperature_deg_C,
                          group = month(Pacific_Standard_Time),
                          color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~.))

# Combine the two and export.
(fig_bw23 <- fig_bw_do23 / fig_bw_t23)

# ggsave("figures/2023_data_bw_102523.png",
#        width = 20,
#        height = 15,
#        units = "cm"
# )

##### GB 2023 #####
# DO
(fig_gb_do23 <- ggplot(dat_clean %>%
                         filter(site %in% c("GB", "SH")) %>%
                         filter(location %in% c("10m", "3m")) %>%
                         filter(Pacific_Standard_Time > 
                                  ymd_hms("2023-05-01 00:00:00")) %>%
                         filter(Pacific_Standard_Time <
                                  ymd_hms("2023-09-30 23:59:59")) %>%
                         mutate(location = factor(case_when(site == "GB" &
                                                              location == "3m" ~ "3m (near)",
                                                            site == "SH" &
                                                              location == "3m" ~ "3m (far)",
                                                            TRUE ~ "10m"),
                                                  levels = c("10m", 
                                                             "3m (near)", 
                                                             "3m (far)")),
                                replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                             replicate %in% c("NS2") ~ "NS2",
                                                             TRUE ~ "NS3"),
                                                   levels = c("NS1", "NS2", "NS3"))),
                       aes(x = Pacific_Standard_Time, 
                           y = percDOsat,
                           group = month(Pacific_Standard_Time),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   labs(x = "Date",
        y = "DO (% Saturation)") +
   theme_bw() +
   theme(legend.position = "none") +
   facet_grid(location~.))
# Temperature
(fig_gb_t23 <- ggplot(dat_clean %>%
                        filter(site %in% c("GB", "SH")) %>%
                        filter(location %in% c("10m", "3m")) %>%
                        filter(Pacific_Standard_Time > 
                                 ymd_hms("2023-05-01 00:00:00")) %>%
                        filter(Pacific_Standard_Time <
                                 ymd_hms("2023-09-30 23:59:59")) %>%
                        mutate(location = factor(case_when(site == "GB" &
                                                             location == "3m" ~ "3m (near)",
                                                           site == "SH" &
                                                             location == "3m" ~ "3m (far)",
                                                           TRUE ~ "10m"),
                                                 levels = c("10m", 
                                                            "3m (near)", 
                                                            "3m (far)")),
                               replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                            replicate %in% c("NS2") ~ "NS2",
                                                            TRUE ~ "NS3"),
                                                  levels = c("NS1", "NS2", "NS3"))),
                      aes(x = Pacific_Standard_Time, 
                          y = Temperature_deg_C,
                          group = month(Pacific_Standard_Time),
                          color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~.))

# Combine the two and export.
(fig_gb23 <- fig_gb_do23 / fig_gb_t23)

# ggsave("figures/2023_data_gb_102523.png",
#        width = 20,
#        height = 15,
#        units = "cm"
# )

# End of script.


#### MARSS 2022 ####

# This initial set of MARSS analyses will examine how DO varies with depth,
# and factor in biological (light) and physical (wind) drivers.

##### Data prep #####
# To prep the data for analysis, I am first going to filter for the 
# appropriate dates.
dat_2022 <- dat_clean %>%
  filter(site %in% c("BW", "GB")) %>%
  filter(Pacific_Standard_Time > 
           ymd_hms("2022-03-01 00:00:00")) %>%
  filter(Pacific_Standard_Time <
           ymd_hms("2023-02-28 23:59:59")) %>%
  # and then select columns I want
  select(Pacific_Standard_Time, shore, site, location, replicate, 
         percDOsat)

# And I will aggregate to hourly measurements.
dat_2022_hourly <- dat_2022 %>%
  mutate(Year = year(Pacific_Standard_Time),
         Month = month(Pacific_Standard_Time),
         Day = day(Pacific_Standard_Time),
         Hour = hour(Pacific_Standard_Time)) %>%
  group_by(shore, site, location, replicate,
           Year, Month, Day, Hour) %>%
  summarize(mean_percDOsat = mean(percDOsat, na.rm = TRUE)) %>%
  ungroup() %>%
  # make a new date column
  mutate(Minute = 0,
         Second = 0) %>%
  mutate(Date = make_datetime(Year, Month, Day, 
                              Hour, Minute, Second))

# Quick plot to see coverage.
ggplot(dat_2022_hourly, aes(x = Date, y = location)) +
  geom_line(aes(color = location)) +
  facet_grid(.~site) +
  theme_bw()

# And need to do the same with weather but doing separately so as not to
# lose any measurements (since MARSS requires all covariate data be present).
weather_2022 <- weather_long %>%
  filter(Date_TimePST > 
           ymd_hms("2022-03-01 00:00:00")) %>%
  filter(Date_TimePST <
           ymd_hms("2023-02-28 23:59:59")) %>%
  # and then select columns I want
  select(Date_TimePST, shore, site, location, replicate,
         solar_radiation_set_1, wind_speed_set_1)

# And I will aggregate to hourly measurements.
weather_2022_hourly <- weather_2022 %>%
  mutate(Year = year(Date_TimePST),
         Month = month(Date_TimePST),
         Day = day(Date_TimePST),
         Hour = hour(Date_TimePST)) %>%
  group_by(shore, site, location, replicate,
           Year, Month, Day, Hour) %>%
  summarize(mean_solar = mean(solar_radiation_set_1, na.rm = TRUE),
            mean_windspeed = mean(wind_speed_set_1, na.rm = TRUE)) %>%
  ungroup() %>%
  # make a new date column
  mutate(Minute = 0,
         Second = 0) %>%
  mutate(Date = make_datetime(Year, Month, Day, 
                              Hour, Minute, Second))

# Quick plot to see coverage.
ggplot(weather_2022_hourly, aes(x = Date, y = location)) +
  geom_line(aes(color = location)) +
  facet_grid(.~site) +
  theme_bw()

# And join the DO data to the weather data.
dat_all_2022 <- left_join(weather_2022_hourly, dat_2022_hourly,
                          by = c("shore", "site", "location",
                                 "replicate", "Date"))

# Now to start manipulating the dataframe into matrix format.
# first need to add unique sitenames
dat_gb22 <- dat_all_2022 %>%
  mutate(site_name = paste(site, location, replicate)) %>%
  filter(site == "GB") %>%
  # need to add index numbers to make each row "unique" otherwise the
  # next step throws an error
  mutate(index = rep(seq(1,8743), 9)) %>%
  # pivot wider for MARSS format
  select(
    site_name, index, 
    mean_percDOsat, 
    mean_solar, mean_windspeed) %>% 
  pivot_wider(
    names_from = site_name, 
    values_from = c(mean_percDOsat, 
                    mean_solar, mean_windspeed))

# indicate column #s of response and predictor vars
names(dat_gb22)
resp_cols = c(2:10)
cov_cols = c(11:28)

# log and scale transform response var
dat_gb22_scale = dat_gb22
dat_gb22_scale[,resp_cols] = scale(dat_gb22_scale[,resp_cols])

# Pull out only response var
dat_dep <- t(dat_gb22_scale[,c(resp_cols)])

# End of script.
