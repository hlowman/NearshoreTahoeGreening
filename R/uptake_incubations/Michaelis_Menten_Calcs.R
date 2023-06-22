#### Uptake Rate Calculation Workflow
#### June 22, 2023
#### Script created by: Heili Lowman

# This script is designed to use calculated uptake rates to estimate parameters
# for Michaelis-Menten model of uptake rates, based on this workflow:
# https://rpubs.com/RomanL/6752

#### Setup ####

# Load packages.
library(tidyverse)
library(lubridate)
library(here)
library(drc)
library(patchwork)

# Load data
dat_rates <- readRDS("data_working/N_May_Incubation_Uptake_Rates_062223.rds")

# Also going to multiply uptake rates by -1 so that uptake of N yields positive
# values and production of N yields negative values.
dat_rates <- dat_rates %>%
  mutate(uptake_µgNLhr = net_delta_Conc_µgNLhr*(-1))

#### Biofilm Models ####

##### West Shore (BW) #####

# NO3

# Fit the MM model.
bw0.5_bf_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                   filter(Location == "BW",
                          Type == "biofilm",
                          Analyte == "NO3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(bw0.5_bf_NO3)

# And predict values to display when plotting results.
bw0.5_bf_NO3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                   length.out = 100))
bw0.5_bf_NO3pred$uptake_µgNLhr <- predict(bw0.5_bf_NO3, newdata = bw0.5_bf_NO3pred)

# And plot the values and the model results.
(bw0.5_bf_NO3plot <- ggplot(dat_rates %>%
         filter(Location == "BW",
                Type == "biofilm",
                Analyte == "NO3"), aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
  theme_bw() +
  xlab("NO3 Concentration [µg/L]") +
  ylab("Uptake Rate [µg-N/L-hr]") +
  ggtitle("BW 0.5m biofilm Michaelis-Menten kinetics") +
  geom_point(alpha = 0.5, color = "black") +
  geom_line(data = bw0.5_bf_NO3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
            color = "#B5C861", size = 1))

# NH3

# Fit the MM model.
bw0.5_bf_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "BW",
                              Type == "biofilm",
                              Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(bw0.5_bf_NH3)

# And predict values to display when plotting results.
bw0.5_bf_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
bw0.5_bf_NH3pred$uptake_µgNLhr <- predict(bw0.5_bf_NH3, newdata = bw0.5_bf_NH3pred)

# And plot the values and the model results.
(bw0.5_bf_NH3plot <- ggplot(dat_rates %>%
                              filter(Location == "BW",
                                     Type == "biofilm",
                                     Analyte == "NH3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("BW 0.5m biofilm Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = bw0.5_bf_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

##### East Shore (GB) #####

# NO3

# Fit the MM model.
gb0.5_bf_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "GB",
                              Type == "biofilm",
                              Analyte == "NO3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb0.5_bf_NO3)

# And predict values to display when plotting results.
gb0.5_bf_NO3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
gb0.5_bf_NO3pred$uptake_µgNLhr <- predict(gb0.5_bf_NO3, newdata = gb0.5_bf_NO3pred)

# And plot the values and the model results.
(gb0.5_bf_NO3plot <- ggplot(dat_rates %>%
                              filter(Location == "GB",
                                     Type == "biofilm",
                                     Analyte == "NO3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NO3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 0.5m biofilm Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb0.5_bf_NO3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#B5C861", size = 1))

# NH3

# Fit the MM model.
gb0.5_bf_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "GB",
                              Type == "biofilm",
                              Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb0.5_bf_NH3)

# And predict values to display when plotting results.
gb0.5_bf_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
gb0.5_bf_NH3pred$uptake_µgNLhr <- predict(gb0.5_bf_NH3, newdata = gb0.5_bf_NH3pred)

# And plot the values and the model results.
(gb0.5_bf_NH3plot <- ggplot(dat_rates %>%
                              filter(Location == "GB",
                                     Type == "biofilm",
                                     Analyte == "NH3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 0.5m biofilm Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb0.5_bf_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

##### Compiled figure #####

# Pull together all four figures above into a paneled biofilm figure.
(bf_plot <- bw0.5_bf_NH3plot + gb0.5_bf_NH3plot +
  bw0.5_bf_NO3plot + gb0.5_bf_NO3plot +
  plot_layout(ncol = 2))

# Export figure.
ggsave(plot = bf_plot,
       filename = "figures/uptake_biofilm_May_062223.png",
       width = 12, height = 8, dpi=300)

#### Sediment Models ####

##### 0.5m #####

###### West Shore (BW) ######

# NO3

# Fit the MM model.
bw0.5_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "BW",
                              Type == "sediment",
                              Depth == "0.5",
                              Analyte == "NO3") %>%
                    drop_na(uptake_µgNLhr), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
# "Convergence failed"

# And plot the values and the model results.
(bw0.5_NO3plot <- ggplot(dat_rates %>%
                              filter(Location == "BW",
                                     Type == "sediment",
                                     Depth == "0.5",
                                     Analyte == "NO3"), 
                         aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NO3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("BW 0.5m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black"))

# NH3

# Fit the MM model.
bw0.5_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "BW",
                              Type == "sediment",
                              Depth == "0.5",
                              Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(bw0.5_NH3)

# And predict values to display when plotting results.
bw0.5_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
bw0.5_NH3pred$uptake_µgNLhr <- predict(bw0.5_NH3, newdata = bw0.5_NH3pred)

# And plot the values and the model results.
(bw0.5_NH3plot <- ggplot(dat_rates %>%
                              filter(Location == "BW",
                                     Type == "sediment",
                                     Depth == "0.5",
                                     Analyte == "NH3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("BW 0.5m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = bw0.5_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

###### East Shore (GB) ######

# NO3

# Fit the MM model.
gb0.5_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "GB",
                              Type == "sediment",
                              Depth == "0.5",
                              Analyte == "NO3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb0.5_NO3)

# And predict values to display when plotting results.
gb0.5_NO3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
gb0.5_NO3pred$uptake_µgNLhr <- predict(gb0.5_NO3, newdata = gb0.5_NO3pred)

# And plot the values and the model results.
(gb0.5_NO3plot <- ggplot(dat_rates %>%
                              filter(Location == "GB",
                                     Type == "sediment",
                                     Depth == "0.5",
                                     Analyte == "NO3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NO3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 0.5m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb0.5_NO3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#B5C861", size = 1))

# NH3

# Fit the MM model.
gb0.5_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                       filter(Location == "GB",
                              Type == "sediment",
                              Depth == "0.5",
                              Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb0.5_NH3)

# And predict values to display when plotting results.
gb0.5_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                                length.out = 100))
gb0.5_NH3pred$uptake_µgNLhr <- predict(gb0.5_NH3, newdata = gb0.5_NH3pred)

# And plot the values and the model results.
(gb0.5_NH3plot <- ggplot(dat_rates %>%
                              filter(Location == "GB",
                                     Type == "sediment",
                                     Depth == "0.5",
                                     Analyte == "NH3"), 
                            aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 0.5m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb0.5_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

###### Compiled figure ######

# Pull together all four figures above into a paneled 0.5m sediment figure.
(sed0.5_plot <- bw0.5_NH3plot + gb0.5_NH3plot +
   bw0.5_NO3plot + gb0.5_NO3plot +
   plot_layout(ncol = 2))

# Export figure.
ggsave(plot = sed0.5_plot,
       filename = "figures/uptake_sediment0.5_May_062223.png",
       width = 12, height = 8, dpi=300)

##### 10m #####

###### West Shore (BW) ######

# NO3

# Fit the MM model.
bw10_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                    filter(Location == "BW",
                           Type == "sediment",
                           Depth == "10",
                           Analyte == "NO3") %>%
                    drop_na(uptake_µgNLhr), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(bw10_NO3)

# And predict values to display when plotting results.
bw10_NO3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                             length.out = 100))
bw10_NO3pred$uptake_µgNLhr <- predict(bw10_NO3, newdata = bw10_NO3pred)


# And plot the values and the model results.
(bw10_NO3plot <- ggplot(dat_rates %>%
                           filter(Location == "BW",
                                  Type == "sediment",
                                  Depth == "10",
                                  Analyte == "NO3"), 
                         aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NO3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("BW 10m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black")+
    geom_line(data = bw10_NO3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#B5C861", size = 1))

# NH3

# Fit the MM model.
bw10_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                    filter(Location == "BW",
                           Type == "sediment",
                           Depth == "10",
                           Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(bw10_NH3)

# And predict values to display when plotting results.
bw10_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                             length.out = 100))
bw10_NH3pred$uptake_µgNLhr <- predict(bw10_NH3, newdata = bw10_NH3pred)

# And plot the values and the model results.
(bw10_NH3plot <- ggplot(dat_rates %>%
                           filter(Location == "BW",
                                  Type == "sediment",
                                  Depth == "10",
                                  Analyte == "NH3"), 
                         aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("BW 10m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = bw10_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

###### East Shore (GB) ######

# NO3

# Fit the MM model.
gb10_NO3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                    filter(Location == "GB",
                           Type == "sediment",
                           Depth == "10",
                           Analyte == "NO3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb10_NO3)

# And predict values to display when plotting results.
gb10_NO3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                             length.out = 100))
gb10_NO3pred$uptake_µgNLhr <- predict(gb10_NO3, newdata = gb10_NO3pred)

# And plot the values and the model results.
(gb10_NO3plot <- ggplot(dat_rates %>%
                           filter(Location == "GB",
                                  Type == "sediment",
                                  Depth == "10",
                                  Analyte == "NO3"), 
                         aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NO3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 10m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb10_NO3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#B5C861", size = 1))

# NH3

# Fit the MM model.
gb10_NH3 <- drm (uptake_µgNLhr ~ Spike_µg_L, data = dat_rates %>%
                    filter(Location == "GB",
                           Type == "sediment",
                           Depth == "10",
                           Analyte == "NH3"), fct = MM.2())

# Examine parameter estimates.
# d = Vmax or asymptote
# e = K or half-saturation
summary(gb10_NH3)

# And predict values to display when plotting results.
gb10_NH3pred <- data.frame(Spike_µg_L = seq(0, max(dat_rates$Spike_µg_L), 
                                             length.out = 100))
gb10_NH3pred$uptake_µgNLhr <- predict(gb10_NH3, newdata = gb10_NH3pred)

# And plot the values and the model results.
(gb10_NH3plot <- ggplot(dat_rates %>%
                           filter(Location == "GB",
                                  Type == "sediment",
                                  Depth == "10",
                                  Analyte == "NH3"), 
                         aes(x = Spike_µg_L, y = uptake_µgNLhr)) +
    theme_bw() +
    xlab("NH3 Concentration [µg/L]") +
    ylab("Uptake Rate [µg-N/L-hr]") +
    ggtitle("GB 10m sediment Michaelis-Menten kinetics") +
    geom_point(alpha = 0.5, color = "black") +
    geom_line(data = gb10_NH3pred, aes(x = Spike_µg_L, y = uptake_µgNLhr), 
              color = "#8AA789", size = 1))

###### Compiled figure ######

# Pull together all four figures above into a paneled 10m sediment figure.
(sed10_plot <- bw10_NH3plot + gb10_NH3plot +
   bw10_NO3plot + gb10_NO3plot +
   plot_layout(ncol = 2))

# Export figure.
ggsave(plot = sed10_plot,
       filename = "figures/uptake_sediment10_May_062223.png",
       width = 12, height = 8, dpi=300)

# End of script.
