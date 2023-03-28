#### DO, Wind, Temp Data Exploration
### March 28, 2023
## Heili Lowman

# The following script will read in compiled data and make some
# exploratory plots ahead of the ASLO conference.

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

#### Figures ####

# Make preliminary paneled plots.

##### BWNS1 #####
(fig_0a <- ggplot(bwns1, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_0b <- ggplot(bwns1, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_0c <- ggplot(bwns1, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
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
(fig_3a <- ggplot(bw20, aes(x = par, y = o2_sat*100)) +
   geom_point(color = "#E69512", alpha = 0.8) +
   labs(x = "Light",
        y = "% Saturation Dissolved Oxygen") +
   theme_bw())

(fig_3b <- ggplot(bw20, aes(x = wspeed, y = o2_sat*100)) +
    geom_point(color = "#0FB2D3", alpha = 0.8) +
    scale_x_log10() +
    labs(x = "Windspeed",
         y = "% Saturation Dissolved Oxygen") +
    theme_bw())

(fig_3c <- ggplot(bw20, aes(x = wtemp, y = o2_sat*100)) +
    geom_point(color = "#D3105C", alpha = 0.8) +
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

# End of script.
