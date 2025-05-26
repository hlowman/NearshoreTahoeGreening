# Supplemental Figures Script
# April 29, 2025
# Heili Lowman

#### README ####

# The following script will create most supplementary figures
# included in the nearshore Tahoe DO manuscript, including raw
# data time series plots.

#### SETUP ####

# Load packages.
library(tidyverse)
library(lubridate)
library(here)
library(patchwork)
library(ggtext)
library(viridis)

# Load data.

# DO data.
data_2022 <- readRDS("data_working/do_data_2022_052325.rds") 
data_2023 <- readRDS("data_working/do_data_2023_052325.rds")
clim_2022 <- readRDS("data_working/do_covariate_daily_data_2022_052525.rds")
clim_2023 <- readRDS("data_working/do_covariate_daily_data_2023_052525.rds")

# Tidy data.
data_2022 <- data_2022 %>%
  mutate(date_times = ymd_hms(date_times)) %>%
  # join with climate data
  full_join(clim_2022) %>%
  # and add plotting column
  mutate(Location = factor(case_when(replicate %in% c("NS1", "NS2", "NS3") ~ replicate,
                                     location == "10m" ~ "SL",
                                     location == "15m" ~ "ML",
                                     location == "20m" ~ "DL"),
                           levels = c("NS1", "NS2",
                                      "NS3", "SL",
                                      "ML", "DL")))

data_2023 <- data_2023 %>%
  mutate(date_times = ymd_hms(date_times)) %>%
  # join with climate data
  full_join(clim_2023) %>%
  # and add plotting column
  mutate(Location = factor(case_when(replicate == "NS1" & site == "BW" ~ "BWNS1",
                                     replicate == "NS2" & site == "BW" ~ "BWNS2",
                                     replicate == "NS3" & site == "BW" ~ "BWNS3",
                                     replicate == "NS1" & site == "GB" ~ "GBNS1",
                                     replicate == "NS2" & site == "GB" ~ "GBNS2",
                                     replicate == "NS3" & site == "GB" ~ "GBNS3",
                                     replicate == "NS1" & site == "SH" ~ "SHNS1",
                                     replicate == "NS2" & site == "SH" ~ "SHNS2",
                                     replicate == "NS3" & site == "SH" ~ "SHNS3",
                                     replicate == "NS1" & site == "SS" ~ "SSNS1",
                                     replicate == "NS2" & site == "SS" ~ "SSNS2",
                                     replicate == "NS3" & site == "SS" ~ "SSNS3"),
                           levels = c("BWNS1", "BWNS2", "BWNS3",
                                      "SSNS1", "SSNS2", "SSNS3",
                                      "GBNS1", "GBNS2", "GBNS3",
                                      "SHNS1", "SHNS2", "SHNS3")))

#### Data TS ####

# First, the raw dissolved oxygen time series, followed by
# temperature, light, wind, and discharge.

##### S1 #####

# Glenbrook first.
(fig_S1a1 <- ggplot(data_2022 %>%
                    filter(site == "GB") %>%
                    filter(Location %in% c("NS1",
                                           "NS2",
                                           "NS3")),
                 aes(x = date_times,
                     y = DO_mgL,
                     color = Location,
                     group = ID_index)) +
  geom_line() +
  ylim(2, 13) +
  scale_color_manual(values = c("gray70", "gray55","gray40")) +
  scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
  labs(x = "Date",
       y = "Dissolved Oxygen (mg/L)") +
  theme_bw())

(fig_S1a2 <- ggplot(data_2022 %>%
                      filter(site == "GB") %>%
                      filter(Location %in% c("SL",
                                             "ML",
                                             "DL")),
                    aes(x = date_times,
                        y = DO_mgL,
                        color = Location,
                        group = ID_index)) +
    geom_line() +
    ylim(2, 13) +
    scale_color_manual(values = c("gray25","gray10","black")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw())

(fig_S1altdo <- ggplot(data_2022 %>%
                     filter(site == "GB"),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2, 13) +
    scale_color_manual(values = c("gray70", "gray55",
                                  "gray40", "gray25",
                                  "gray10", "black"),
                       guide = "none") +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S1altdo,
#        filename = "figures/SI_DO_GB_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S1a_sat1 <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location %in% c("NS1",
                                            "NS2",
                                            "NS3")),
                   aes(x = date_times,
                       y = DO_sat,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("pink1", "pink2", "pink3")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())

(fig_S1a_sat2 <- ggplot(data_2022 %>%
                          filter(site == "GB") %>%
                          filter(Location %in% c("SL",
                                                 "ML",
                                                 "DL")),
                        aes(x = date_times,
                            y = DO_sat,
                            color = Location,
                            group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("palevioletred2",
                                  "palevioletred3", 
                                  "palevioletred4")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())


(fig_S1b1 <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location %in% c("NS1",
                                            "NS2",
                                            "NS3")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("lightskyblue", "deepskyblue2",
                                  "deepskyblue3")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S1b2 <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location %in% c("SL",
                                            "ML",
                                            "DL")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("deepskyblue4",
                                  "dodgerblue4", "midnightblue")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S1alttemp <- ggplot(data_2022 %>%
                     filter(site == "GB"),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("lightskyblue", "deepskyblue2",
                                  "deepskyblue3", "deepskyblue4",
                                  "dodgerblue4", "midnightblue"),
                       guide = "none") +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S1alttemp,
#        filename = "figures/SI_Temp_GB_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S1c <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location == "NS2"),
                   aes(x = date,
                       y = sum_light,
                       color = Location)) +
    geom_line() +
    ylim(1500, 9000) +
    scale_color_manual(values = c("orange2"), guide = "none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Light (W/m<sup>2</sup>)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S1d <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location == "NS2"),
                   aes(x = date,
                       y = mean_ws,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("slateblue1"), guide = "none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw())

(fig_S1e <- ggplot(data_2022 %>%
                     filter(site == "GB") %>%
                     filter(Location == "NS2"),
                   aes(x = date,
                       y = mean_q,
                       color = Location)) +
    geom_line() +
    ylim(0, 0.5) +
    scale_color_manual(values = c("darkturquoise"), guide = "none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Discharge (m<sup>3</sup>/s)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S1 <- fig_S1a1 / 
  fig_S1a_sat1 /
  fig_S1b1 /
  fig_S1c /
  fig_S1d /
  fig_S1e)

# ggsave(fig_S1,
#        filename = "figures/S1_GB_TimeSeries_shallow.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

(fig_S1_deep <- fig_S1a2 / 
    fig_S1a_sat2 /
    fig_S1b2 /
    fig_S1c /
    fig_S1d /
    fig_S1e)

# ggsave(fig_S1_deep,
#        filename = "figures/S1_GB_TimeSeries_deep.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

##### S2 #####

# Next Blackwood.
(fig_S2a1 <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location %in% c("NS1",
                                            "NS2",
                                            "NS3")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2,13) +
    scale_color_manual(values = c("gray70", "gray55",
                                  "gray40")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw())

(fig_S2a2 <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location %in% c("SL",
                                            "ML",
                                            "DL")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2,13) +
    scale_color_manual(values = c("gray25",
                                  "gray10", "black")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw())

(fig_S2altdo <- ggplot(data_2022 %>%
                     filter(site == "BW"),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2,13) +
    scale_color_manual(values = c("black", "gray10",
                                  "gray25", "gray40",
                                  "gray55", "gray70"),
                       guide = "none") +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S2altdo,
#        filename = "figures/SI_DO_BW_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S2a_sat1 <- ggplot(data_2022 %>%
                          filter(site == "BW") %>%
                          filter(Location %in% c("NS1",
                                                 "NS2",
                                                 "NS3")),
                        aes(x = date_times,
                            y = DO_sat,
                            color = Location,
                            group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("pink1", "pink2", "pink3")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())

(fig_S2a_sat2 <- ggplot(data_2022 %>%
                          filter(site == "GB") %>%
                          filter(Location %in% c("SL",
                                                 "ML",
                                                 "DL")),
                        aes(x = date_times,
                            y = DO_sat,
                            color = Location,
                            group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("palevioletred2",
                                  "palevioletred3", 
                                  "palevioletred4")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())


(fig_S2b1 <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location %in% c("NS1",
                                            "NS2",
                                            "NS3")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("lightskyblue", "deepskyblue2",
                                  "deepskyblue3")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S2b2 <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location %in% c("SL",
                                            "ML",
                                            "DL")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("deepskyblue4",
                                  "dodgerblue4", "midnightblue")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S2alttemp <- ggplot(data_2022 %>%
                     filter(site == "BW"),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("lightskyblue", "deepskyblue2",
                                  "deepskyblue3", "deepskyblue4",
                                  "dodgerblue4", "midnightblue"),
                       guide = "none") +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S2alttemp,
#        filename = "figures/SI_Temp_BW_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S2c <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location == "NS1"),
                   aes(x = date,
                       y = sum_light,
                       color = Location)) +
    geom_line() +
    ylim(1500, 9000) +
    scale_color_manual(values = c("orange2"), guide = "none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Light (W/m<sup>2</sup>)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S2d <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location == "NS1"),
                   aes(x = date,
                       y = mean_ws,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("slateblue1"), guide ="none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw())

(fig_S2e <- ggplot(data_2022 %>%
                     filter(site == "BW") %>%
                     filter(Location == "NS1"),
                   aes(x = date,
                       y = mean_q,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("darkturquoise"), guide = "none") +
    scale_x_date(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Discharge (m<sup>3</sup>/s)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S2 <- fig_S2a1 / 
    fig_S2a_sat1 /
    fig_S2b1 /
    fig_S2c /
    fig_S2d /
    fig_S2e)

# ggsave(fig_S2,
#        filename = "figures/S2_BW_TimeSeries_shallow.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

(fig_S2_deep <- fig_S2a2 /
    fig_S2a_sat2 /
    fig_S2b2 /
    fig_S2c /
    fig_S2d /
    fig_S2e)

# ggsave(fig_S2_deep,
#        filename = "figures/S2_BW_TimeSeries_deep.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

##### S3 #####

# Now east shore for Stage II.
(fig_S3a <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2, 13) +
    scale_color_manual(values = c("black", "gray10",
                                  "gray25", "gray40",
                                  "gray55", "gray70")) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw())

(fig_S3altdo <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2, 13) +
    scale_color_manual(values = c("black", "gray10",
                                  "gray25", "gray40",
                                  "gray55", "gray70"),
                       guide = "none") +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S3altdo,
#        filename = "figures/SI_DO_GB2_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S3a_sat <- ggplot(data_2023 %>%
                          filter(site %in% c("GB",
                                             "SH")),
                        aes(x = date_times,
                            y = DO_sat,
                            color = Location,
                            group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("palevioletred4",
                                  "palevioletred3",
                                  "palevioletred2",
                                  "pink3", "pink2",
                                  "pink1")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())

(fig_S3b <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("midnightblue", "dodgerblue4",
                                  "deepskyblue4", "deepskyblue3",
                                  "deepskyblue2", "lightskyblue")) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S3alttemp <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("midnightblue", "dodgerblue4",
                                  "deepskyblue4", "deepskyblue3",
                                  "deepskyblue2", "lightskyblue"),
                       guide = "none") +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    facet_grid(Location~.))
 
# ggsave(fig_S3alttemp,
#        filename = "figures/SI_Temp_GB2_TimeSeries.jpeg",
#        height = 25,
#        width = 20,
#        units = "cm")

(fig_S3c <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")) %>%
                     filter(Location == "GBNS1"),
                   aes(x = date,
                       y = sum_light,
                       color = Location)) +
    geom_line() +
    ylim(1500, 9000) +
    scale_color_manual(values = c("orange2"), guide = "none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Light (W/m<sup>2</sup>)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S3d <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")) %>%
                     filter(Location == "GBNS1"),
                   aes(x = date,
                       y = mean_ws,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("slateblue1"), guide ="none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw())

(fig_S3e <- ggplot(data_2023 %>%
                     filter(site %in% c("GB", "SH")) %>%
                     filter(Location == "GBNS1"),
                   aes(x = date,
                       y = mean_q,
                       color = Location)) +
    geom_line() +
    ylim(0, 0.5) +
    scale_color_manual(values = c("darkturquoise"), guide = "none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Discharge (m<sup>3</sup>/s)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S3 <- fig_S3a /
    fig_S3a_sat /
    fig_S3b /
    fig_S3c /
    fig_S3d /
    fig_S3e)

# ggsave(fig_S3,
#        filename = "figures/S3_GBSH_TimeSeries.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

##### S4 #####

# And west shore for Stage II.
(fig_S4a <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2,13) +
    scale_color_manual(values = c("black", "gray10",
                                  "gray25", "gray55")) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw())

(fig_S4altdo <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")),
                   aes(x = date_times,
                       y = DO_mgL,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(2,13) +
    scale_color_manual(values = c("black", "gray10",
                                  "gray25", "gray55"),
                       guide = "none") +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S4altdo,
#        filename = "figures/SI_DO_BW2_TimeSeries.jpeg",
#        height = 17,
#        width = 20,
#        units = "cm")

(fig_S4a_sat <- ggplot(data_2023 %>%
                         filter(site %in% c("BW",
                                            "SS")),
                       aes(x = date_times,
                           y = DO_sat,
                           color = Location,
                           group = ID_index)) +
    geom_line() +
    ylim(60, 120) +
    scale_color_manual(values = c("palevioletred4",
                                  "palevioletred3",
                                  "palevioletred2",
                                  "pink2")) +
    scale_x_datetime(breaks = "2 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Dissolved Oxygen (% sat.)") +
    theme_bw())

(fig_S4b <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("midnightblue", "dodgerblue4",
                                  "deepskyblue4", 
                                  "deepskyblue2")) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw())

(fig_S4alttemp <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")),
                   aes(x = date_times,
                       y = Temp_C,
                       color = Location,
                       group = ID_index)) +
    geom_line() +
    ylim(3, 22) +
    scale_color_manual(values = c("lightskyblue", "deepskyblue2",
                                  "deepskyblue3", #"deepskyblue4",
                                  "dodgerblue4"),
                       guide = "none") +
    scale_x_datetime(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    facet_grid(Location~.))

# ggsave(fig_S4alttemp,
#        filename = "figures/SI_Temp_BW2_TimeSeries.jpeg",
#        height = 17,
#        width = 20,
#        units = "cm")

(fig_S4c <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")) %>%
                     filter(Location == "BWNS2"),
                   aes(x = date,
                       y = sum_light,
                       color = Location)) +
    geom_line() +
    ylim(1500, 9000) +
    scale_color_manual(values = c("orange2"), guide = "none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Light (W/m<sup>2</sup>)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S4d <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")) %>%
                     filter(Location == "BWNS2"),
                   aes(x = date,
                       y = mean_ws,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("slateblue1"), guide ="none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw())

(fig_S4e <- ggplot(data_2023 %>%
                     filter(site %in% c("BW", "SS")) %>%
                     filter(Location == "BWNS2"),
                   aes(x = date,
                       y = mean_q,
                       color = Location)) +
    geom_line() +
    ylim(0, 10) +
    scale_color_manual(values = c("darkturquoise"), guide = "none") +
    scale_x_date(breaks = "1 month", date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Discharge (m<sup>3</sup>/s)") +
    theme_bw() +
    theme(axis.title.y = element_markdown()))

(fig_S4 <- fig_S4a / 
    fig_S4a_sat /
    fig_S4b /
    fig_S4c /
    fig_S4d /
    fig_S4e)

# ggsave(fig_S4,
#        filename = "figures/S4_BWSS_TimeSeries.jpeg",
#        height = 30,
#        width = 20,
#        units = "cm")

#### %DO v Temp ####

##### S5 #####

Date_F <- function(x){
  format(as.Date(x, origin = '1970-01-01',"%Y-%m-%d"),"%m-%Y")
}

# Stage I, Glenbrook sites
(fig_S5 <- ggplot(data_2022 %>%
                   filter(site == "GB"),
                 aes(x = Temp_C,
                     y = DO_sat,
                     color = date)) +
   geom_point(alpha = 0.3) +
   scale_color_viridis(labels = Date_F,
                       direction = -1) +
   ylim(60, 120) +
   labs(x = "Temperature (°C)",
        y = "Dissolved Oxygen (% saturation)",
        color = "Date") +
   theme_bw() +
   facet_wrap(.~Location))

# ggsave(fig_S5,
#        filename = "figures/S5_GB_DOvTemp.jpeg",
#        height = 10,
#        width = 15,
#        units = "cm")

##### S6 #####

# Stage I, Blackwood sites
(fig_S6 <- ggplot(data_2022 %>%
                    filter(site == "BW"),
                  aes(x = Temp_C,
                      y = DO_sat,
                      color = date)) +
    geom_point(alpha = 0.3) +
    scale_color_viridis(labels = Date_F,
                        direction = -1) +
    ylim(60, 120) +
    labs(x = "Temperature (°C)",
         y = "Dissolved Oxygen (% saturation)",
         color = "Date") +
    theme_bw() +
    facet_wrap(.~Location))

# ggsave(fig_S6,
#        filename = "figures/S6_BW_DOvTemp.jpeg",
#        height = 10,
#        width = 15,
#        units = "cm")

##### S7 #####

# Stage II, East Shore sites
(fig_S7 <- ggplot(data_2023 %>%
                    filter(site %in% c("GB", "SH")),
                  aes(x = Temp_C,
                      y = DO_sat,
                      color = date)) +
    geom_point(alpha = 0.3) +
    scale_color_viridis(labels = Date_F,
                        direction = -1) +
    ylim(60, 120) +
    labs(x = "Temperature (°C)",
         y = "Dissolved Oxygen (% saturation)",
         color = "Date") +
    theme_bw() +
    facet_wrap(.~Location))

# ggsave(fig_S7,
#        filename = "figures/S7_GBSH_DOvTemp.jpeg",
#        height = 10,
#        width = 15,
#        units = "cm")

##### S8 #####

# Stage II, West Shore sites
(fig_S8 <- ggplot(data_2023 %>%
                    filter(site %in% c("BW", "SS")),
                  aes(x = Temp_C,
                      y = DO_sat,
                      color = date)) +
    geom_point(alpha = 0.3) +
    scale_color_viridis(labels = Date_F,
                        direction = -1) +
    ylim(60, 120) +
    labs(x = "Temperature (°C)",
         y = "Dissolved Oxygen (% saturation)",
         color = "Date") +
    theme_bw() +
    facet_wrap(.~Location))

# ggsave(fig_S8,
#        filename = "figures/S8_BWSS_DOvTemp.jpeg",
#        height = 10,
#        width = 10,
#        units = "cm")

# End of script.
