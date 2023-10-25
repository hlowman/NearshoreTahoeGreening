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

#### Load Data ####

dat <- readRDS("data_working/do_sat_aggregated_102523.rds")

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
