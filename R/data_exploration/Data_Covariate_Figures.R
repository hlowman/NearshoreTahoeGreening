#### DO, Wind, Temp Data Exploration
### March 28, 2023
## Heili Lowman

# The following script will read in compiled data and make some
# plots for inclusion in ASLO conference presentation slides.

#### Setup ####

# Load packages.
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
library(shinystan)
library(here)

#### Data Load-in ####

# Load datasets.
bwns1 <- read_csv("data_working/BWNS1Inputs.csv")
bw10 <- read_csv("data_working/BW10Inputs.csv")
bw15 <- read_csv("data_working/BW15Inputs.csv")
bw20 <- read_csv("data_working/BW20Inputs.csv")
gbns1 <- read_csv("data_working/GBNS1Inputs.csv")
gb10 <- read_csv("data_working/GB10Inputs.csv")
gb15 <- read_csv("data_working/GB15Inputs.csv")
gb20 <- read_csv("data_working/GB20Inputs.csv")

#### Figures ####

# Make preliminary paneled plots.

##### BWNS1 #####
(fig_0a <- ggplot(bwns1 %>%
                    filter(datetime_PST < date("2022-08-01 00:00:00")), 
                  aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   ylim(85, 108) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_0b <- ggplot(bwns1 %>%
                    filter(datetime_PST < date("2022-08-01 00:00:00")), 
                           aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    ylim(85, 108) +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_0c <- ggplot(bwns1 %>%
                    filter(datetime_PST < date("2022-08-01 00:00:00")), 
                  aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    xlim(4, 21) +
    ylim(85, 108) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig0 <- fig_0a + fig_0b + fig_0c +
    plot_annotation(title = "BW 3m", tag_levels = "A"))

# ggsave(fig0,
#        filename = "figures/BWNS1_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### BW10 #####
(fig_1a <- ggplot(bw10, aes(x = par, y = o2_sat*100)) +
  geom_point(color = "#E69512", alpha = 0.8) +
  labs(x = "Light",
       y = "% Saturation Dissolved Oxygen") +
  theme_bw())

(fig_1b <- ggplot(bw10, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_1c <- ggplot(bw10, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_1d <- ggplot(bw10, aes(x = hour, y = o2_sat*100)) +
    geom_point(color = "#6C568C", alpha = 0.8) +
    labs(x = "Hour of Day",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig1 <- fig_1a + fig_1b + fig_1c +
    plot_annotation(title = "BW 10m", tag_levels = "A"))

# ggsave(fig1,
#        filename = "figures/BW10_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### BW15 #####
(fig_2a <- ggplot(bw15, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_2b <- ggplot(bw15, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_2c <- ggplot(bw15, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig2 <- fig_2a + fig_2b + fig_2c +
    plot_annotation(title = "BW 15m", tag_levels = "A"))

# ggsave(fig2,
#        filename = "figures/BW15_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### BW20 #####
(fig_3a <- ggplot(bw20 %>%
                    filter(datetime_PST > date("2022-05-24 00:00:00")), 
                  aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   ylim(85, 108) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_3b <- ggplot(bw20 %>%
                    filter(datetime_PST > date("2022-05-24 00:00:00")), 
                  aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    ylim(85, 108) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_3c <- ggplot(bw20 %>%
                    filter(datetime_PST > date("2022-05-24 00:00:00")), 
                  aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    xlim(4, 21) +
    ylim(85, 108) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig3 <- fig_3a + fig_3b + fig_3c +
    plot_annotation(title = "BW 20m", tag_levels = "A"))

# ggsave(fig3,
#        filename = "figures/BW20_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

(fig0_3 <- fig_0a + fig_0b + fig_0c +
    fig_3a + fig_3b + fig_3c +
    plot_annotation(title = "BW", tag_levels = "A"))

# ggsave(fig0_3,
#        filename = "figures/BWNS1_20_DO_Covar_050523.jpg",
#        width = 30,
#        height = 20,
#        units = "cm")

##### GBNS1 #####
(fig_4a <- ggplot(gbns1, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_4b <- ggplot(gbns1, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_4c <- ggplot(gbns1, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig4 <- fig_4a + fig_4b + fig_4c +
    plot_annotation(title = "GB 3m", tag_levels = "A"))

# ggsave(fig4,
#        filename = "figures/GBNS1_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### GB10 #####
(fig_5a <- ggplot(gb10, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_5b <- ggplot(gb10, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_5c <- ggplot(gb10, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_5d <- ggplot(gb10, aes(x = hour, y = o2_sat*100)) +
    geom_point(color = "#6C568C", alpha = 0.8) +
    labs(x = "Hour of Day",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig5 <- fig_5a + fig_5b + fig_5c +
    plot_annotation(title = "GB 10m", tag_levels = "A"))

# ggsave(fig5,
#        filename = "figures/GB10_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### GB15 #####
(fig_6a <- ggplot(gb15, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_6b <- ggplot(gb15, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_6c <- ggplot(gb15, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig6 <- fig_6a + fig_6b + fig_6c +
    plot_annotation(title = "GB 15m", tag_levels = "A"))

# ggsave(fig6,
#        filename = "figures/GB15_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

##### GB20 #####
(fig_7a <- ggplot(gb20, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_7b <- ggplot(gb20, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_7c <- ggplot(gb20, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
    labs(x = "Water Temperature",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig7 <- fig_7a + fig_7b + fig_7c +
    plot_annotation(title = "GB 20m", tag_levels = "A"))

# ggsave(fig7,
#        filename = "figures/GB20_DO_Covar_032823.jpg",
#        width = 30,
#        height = 10,
#        units = "cm")

# Mega-figures
(figBW <- fig0 / fig1 / fig2 / fig3 +
    plot_annotation(title = "BW 3, 10, 15, & 20m descending"))

# ggsave(figBW,
#        filename = "figures/BW_DO_Covar_032823.jpg",
#        width = 30,
#        height = 40,
#        units = "cm")

(figGB <- fig4 / fig5 / fig6 / fig7 +
    plot_annotation(title = "GB 3, 10, 15, & 20m descending"))

# ggsave(figGB,
#        filename = "figures/GB_DO_Covar_032823.jpg",
#        width = 30,
#        height = 40,
#        units = "cm")

#### Time-series ####

# BW 3m
# DO
(fig_8a <- ggplot(bwns1, aes(x = datetime_PST, y = do)) +
   geom_line(color = "#69B9FA") +
   ylim(c(6.5,10)) +
   labs(y = "Dissolved Oxygen (mg/L)") +
   theme_bw() +
   theme(axis.title.x = element_blank(),
         text = element_text(size = 30)))

# Temperature
(fig_8b <- ggplot(bwns1, aes(x = datetime_PST, y = wtemp)) +
    geom_line(color = "#4CA49E") +
    ylim(c(4,22)) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 30)))

# Light
bwns1_daily <- bwns1 %>%
  mutate(month = month(datetime_PST),
         day = day(datetime_PST)) %>%
  group_by(year, month, day) %>%
  summarize(par_d = mean(par),
            wind_d = mean(wspeed)) %>%
  ungroup() %>%
  mutate(date = make_date(year, month, day))

(fig_8c <- ggplot(bwns1_daily %>%
                    filter(date > as.Date(as.character("2022-05-24"))), 
                           aes(x = date, y = par_d)) +
    geom_line(color = "#FFAA00") +
    labs(x = "Date",
         y = "Incoming Light (PAR)") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 30)))

# Wind
(fig_8d <- ggplot(bwns1_daily, aes(x = date, y = wind_d)) +
    geom_line(color = "#3793EC") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw() +
    theme(text = element_text(size = 30)))

(fig8 <- fig_8a + fig_8b + fig_8c + fig_8d +
    plot_annotation(title = "BW 3m", tag_levels = "A") +
    plot_layout(nrow = 4))

# ggsave(fig8,
#        filename = "figures/BW3m_TS_Covar_041323.jpg",
#        width = 40,
#        height = 40,
#        units = "cm")

# BW 20m
# DO
(fig_9a <- ggplot(bw20, aes(x = datetime_PST, y = do)) +
    geom_line(color = "#69B9FA") +
    ylim(c(6.5,10)) +
    labs(x = "Date",
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 30)))

# Temperature
(fig_9b <- ggplot(bw20, aes(x = datetime_PST, y = wtemp)) +
    geom_line(color = "#4CA49E") +
    ylim(c(4,22)) +
    labs(x = "Date",
         y = "Temperature (C)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 30)))

# Light
bw20_daily <- bw20 %>%
  mutate(month = month(datetime_PST),
         day = day(datetime_PST)) %>%
  group_by(year, month, day) %>%
  summarize(par_d = mean(par),
            wind_d = mean(wspeed)) %>%
  ungroup() %>%
  mutate(date = make_date(year, month, day))

(fig_9c <- ggplot(bw20_daily, 
                  aes(x = date, y = par_d)) +
    geom_line(color = "#FFAA00") +
    labs(x = "Date",
         y = "Incoming Light (PAR)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 30)))

# Wind
(fig_9d <- ggplot(bw20_daily, aes(x = date, y = wind_d)) +
    geom_line(color = "#3793EC") +
    labs(x = "Date",
         y = "Windspeed (m/s)") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          text = element_text(size = 30)))

(fig9 <- fig_9a + fig_9b + fig_9c + fig_9d +
    plot_annotation(title = "BW 20m", tag_levels = "A") +
    plot_layout(nrow = 4))

# ggsave(fig9,
#        filename = "figures/BW20m_TS_Covar_041323.jpg",
#        width = 40,
#        height = 40,
#        units = "cm")

# End of script.
