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
library(forecast)

#### Load Data ####

dat <- readRDS("data_working/do_sat_aggregated_110223.rds")
weather_long <- readRDS("data_working/weather_aggregated_long_110223.rds")
discharge <- readRDS("data_working/discharge_aggregated_121223.rds")

#### Create Shiny app dataset ####
dat_shiny <- dat %>%
  # Filter out flagged data.
  # For all flags marked "YES" I will remove those data.
  filter(Flag1 == "NO",
         Flag2 == "NO",
         Flag3 == "NO",
         Flag4 == "NO") %>%
  # remove pelagic data for display
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
  # add column with location names
  mutate(location_f = factor(case_when(location == "20m" ~ "deep littoral",
                                       location == "15m" ~ "mid-depth littoral",
                                       location == "10m" ~ "shallow littoral",
                                       location == "3m" ~ "nearshore"),
                             levels = c("nearshore", "shallow littoral",
                                        "mid-depth littoral", "deep littoral")),
         # and column simply for coloration
         replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", "NS3")))

# saveRDS(dat_shiny, "data_working/do_sat_shiny_010924.rds")

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

# 1,162,968 records remaining, so 24% of records removed.

# Deployment dates
dat_deploy <- dat %>%
  select(site, location, replicate, deploy, retrieve) %>%
  unique()

#### Initial Plots ####

##### BW 2022 #####

# Make plotting dataset
dat_clean_BW22 <- dat_clean %>%
  filter(site %in% c("BW")) %>%
  # remove pelagic data for display
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
  filter(Pacific_Standard_Time > 
           ymd_hms("2022-03-01 00:00:00")) %>%
  filter(Pacific_Standard_Time <
           ymd_hms("2023-02-28 23:59:59")) %>%
  mutate(location_f = factor(case_when(location == "20m" ~ "deep littoral",
                                       location == "15m" ~ "mid-depth litt.",
                                       location == "10m" ~ "shallow litt.",
                                       location == "3m" ~ "nearshore"),
                           levels = c("nearshore", "shallow litt.",
                                      "mid-depth litt.", "deep littoral")),
         replicate = factor(case_when(replicate %in% c("NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      replicate %in% c("NS3") ~ "NS3",
                                      TRUE ~ "Littoral"),
                            levels = c("NS1", "NS2", "NS3", "Littoral")))
  
# DO
(fig_bw_do22 <- ggplot(dat_clean_BW22, aes(x = Pacific_Standard_Time, 
                                 y = percDOsat,
                                 group = month(Pacific_Standard_Time),
                                 color = replicate)) +
                geom_line() +
                scale_color_manual(values = c("#A8CBB8","#7AC9B7",
                                              "#4CA49E","#3B7D6E")) +
                labs(x = "Date",
                     y = "DO (% Saturation)") +
                theme_bw() +
                facet_grid(location_f~.) +
                theme(legend.position = "bottom",
                      legend.title = element_blank(),
                      strip.text.y = element_blank()))

# Temperature
(fig_bw_temp22 <- ggplot(dat_clean_BW22, aes(x = Pacific_Standard_Time, 
                         y = Temperature_deg_C,
                         group = month(Pacific_Standard_Time),
                         color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#69B9FA", "#59A3F8", 
                                  "#4B8FF7", "#5A7ECB")) +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    facet_grid(location_f~.))

# Combine the two and export.
(fig_bw22 <- fig_bw_do22 + fig_bw_temp22 + plot_annotation(tag_levels = 'A'))

# ggsave("figures/2022_data_bw_041524.png",
#        width = 30,
#        height = 12,
#        units = "cm"
# )

##### GB 2022 #####

# Make plotting dataset
dat_clean_GB22 <- dat_clean %>%
  filter(site %in% c("GB")) %>%
  # remove pelagic data for display
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
  filter(Pacific_Standard_Time > 
           ymd_hms("2022-03-01 00:00:00")) %>%
  filter(Pacific_Standard_Time <
           ymd_hms("2023-02-28 23:59:59")) %>%
  mutate(location_f = factor(case_when(location == "20m" ~ "deep littoral",
                                       location == "15m" ~ "mid-depth litt.",
                                       location == "10m" ~ "shallow litt.",
                                       location == "3m" ~ "nearshore"),
                             levels = c("nearshore", "shallow litt.",
                                        "mid-depth litt.", "deep littoral")),
         replicate = factor(case_when(replicate %in% c("NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      replicate %in% c("NS3") ~ "NS3",
                                      TRUE ~ "Littoral"),
                            levels = c("NS1", "NS2", "NS3", "Littoral")))
  
# DO
(fig_gb_do22 <- ggplot(dat_clean_GB22, aes(x = Pacific_Standard_Time, 
                           y = percDOsat,
                           group = month(Pacific_Standard_Time),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#A8CBB8","#7AC9B7",
                                 "#4CA49E","#3B7D6E")) +
   labs(x = "Date",
        y = "DO (% Saturation)") +
    theme_bw() +
    facet_grid(location_f~.) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.text.y = element_blank()))

# Temperature
(fig_gb_temp22 <- ggplot(dat_clean_GB22, aes(x = Pacific_Standard_Time, 
                          y = Temperature_deg_C,
                          group = month(Pacific_Standard_Time),
                          color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#69B9FA", "#59A3F8", 
                                  "#4B8FF7", "#5A7ECB")) +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    facet_grid(location_f~.))

# Combine the two and export.
(fig_gb22 <- fig_gb_do22 + fig_gb_temp22 + plot_annotation(tag_levels = 'A'))

ggsave("figures/2022_data_gb_041524.png",
       width = 30,
       height = 12,
       units = "cm"
)

##### BW 2023 #####

# Make plotting dataset
dat_clean_BW23 <- dat_clean %>%
  filter(site %in% c("BW", "SS")) %>%
  filter(location %in% c("10m", "3m")) %>%
  filter(Pacific_Standard_Time > 
           ymd_hms("2023-03-01 00:00:00")) %>%
  filter(Pacific_Standard_Time <
           ymd_hms("2023-09-30 23:59:59")) %>%
  mutate(location_f = factor(case_when(site == "BW" &
                                       location == "3m" ~ "n.s. near stream",
                                     site == "SS" &
                                       location == "3m" ~ "n.s. far stream",
                                     TRUE ~ "shallow littoral"),
                           levels = c("n.s. near stream", 
                                      "n.s. far stream", 
                                      "shallow littoral")),
         replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", "NS3")))
  
# DO
(fig_bw_do23 <- ggplot(dat_clean_BW23 %>%
                         mutate(date = as_date(Pacific_Standard_Time)), 
                           aes(x = date, 
                           y = percDOsat, group = month(date),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   scale_x_date(date_labels = "%b %Y") +
   labs(x = "Date",
        y = "DO (% Saturation)",
        title = "Stage II - West Shore") +
    theme_bw() +
    facet_grid(location_f~.) +
    theme(legend.position = "none",
          strip.text.y = element_blank()))

# Temperature
(fig_bw_temp23 <- ggplot(dat_clean_BW23 %>%
                           mutate(date = as_date(Pacific_Standard_Time)), 
                          aes(x = date, 
                          y = Temperature_deg_C, group = month(date),
                          color = replicate)) +
    geom_line() +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    scale_x_date(date_labels = "%b %Y") +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two and export.
(fig_bw23 <- fig_bw_do23 + fig_bw_temp23)

# ggsave("figures/2023_data_bw_020124.png",
#        width = 18,
#        height = 10,
#        units = "cm"
# )

##### GB 2023 #####

# Make plotting data.
dat_clean_GB23 <- dat_clean %>%
  filter(site %in% c("GB", "SH")) %>%
  filter(location %in% c("10m", "3m")) %>%
  filter(Pacific_Standard_Time > 
           ymd_hms("2023-03-01 00:00:00")) %>%
  filter(Pacific_Standard_Time <
           ymd_hms("2023-09-30 23:59:59")) %>%
  mutate(location_f = factor(case_when(site == "GB" &
                                       location == "3m" ~ "n.s. near stream",
                                     site == "SH" &
                                       location == "3m" ~ "n.s. far stream",
                                     TRUE ~ "shallow littoral"),
                           levels = c("n.s. near stream", 
                                      "n.s. far stream", 
                                      "shallow littoral")),
         replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", "NS3")))

# DO
(fig_gb_do23 <- ggplot(dat_clean_GB23 %>%
                         mutate(date = as_date(Pacific_Standard_Time)), 
                           aes(x = date, 
                           y = percDOsat, group = month(date),
                           color = replicate)) +
   geom_line() +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   scale_x_date(date_labels = "%b %Y") +
   labs(x = "Date",
        y = "DO (% Saturation)",
        title = "Stage II - East Shore") +
    theme_bw() +
    facet_grid(location_f~.) +
    theme(legend.position = "none",
          strip.text.y = element_blank()))

# Temperature
(fig_gb_temp23 <- ggplot(dat_clean_GB23 %>%
                        mutate(date = as_date(Pacific_Standard_Time)), 
                          aes(x = date, 
                          y = Temperature_deg_C, group = month(date),
                          color = replicate)) +
    geom_line() +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two and export.
(fig_gb23 <- fig_gb_do23 + fig_gb_temp23)

# ggsave("figures/2023_data_gb_020124.png",
#        width = 18,
#        height = 10,
#        units = "cm"
# )

#### MARSS GB 2022 w/ light ####

# This initial set of MARSS analyses will examine how DO varies with depth,
# and factor in biological (light) and physical (wind) drivers.

##### Data prep #####
# To prep the data for analysis, I am first going to filter for the 
# appropriate dates.
dat_2022 <- dat_clean %>%
  filter(site %in% c("BW", "GB")) %>%
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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

##### Model fit ####

# Now to start manipulating the dataframe into matrix format.
# first need to add unique sitenames
dat_gb22 <- dat_all_2022 %>%
  mutate(site_name = paste(site, location, replicate)) %>%
  filter(site == "GB") %>%
  # need to add index numbers to make each row "unique" otherwise the
  # next step throws an error
  mutate(index = rep(seq(1,8743), 6)) %>%
  # pivot wider for MARSS format
  select(
    site_name, index, 
    mean_percDOsat, 
    mean_solar, mean_windspeed) %>% 
  pivot_wider(
    names_from = site_name, 
    values_from = c(mean_percDOsat, 
                    mean_solar, mean_windspeed))

# Ok, but since I'm interested in different states, I should only
# have one set of covariates that applies to ALL SITES.
# And for this first test - to avoid craz matrices - I'll be looking
# at only light.
dat_gb22_trim = dat_gb22[,c(1:8)]

# indicate column #s of response and predictor vars
names(dat_gb22_trim)
resp_cols = c(2:7)
cov_cols = c(8)

# scale transform response var
dat_gb22_scale <- dat_gb22_trim
dat_gb22_scale[,resp_cols] = scale(dat_gb22_scale[,resp_cols])

# Pull out only response var
dat_dep <- t(dat_gb22_scale[,c(resp_cols)])

# Make covariate inputs
dat_cov <- dat_gb22_scale[,c(cov_cols)]
# scale and transpose
dat_cov <- t(scale(dat_cov))
row.names(dat_cov)
# check for nas, nans, or infs after scaling (none allowed)
sum(is.nan(dat_cov)) #0
sum(is.na(dat_cov)) #0
sum(is.infinite(dat_cov)) #0
# check for cols with all zeros. this can cause model convergence issues
any(colSums(dat_cov)==0) # FALSE

# make C matrix for 
CC <- matrix(list( 
  # 6 state model
  "10m Benthic",
  "15m Benthic",
  "20m Benthic",
  "3m NS1",
  "3m NS2",
  "3m NS3"), 6, 1)

# Model setup for MARSS 

mod_list <- list(
  ### inputs to process model ###
  B = "diagonal and unequal",
  U = "zero",
  C = CC, 
  c = dat_cov,
  Q = "diagonal and unequal", # proc. error covariance matrix
  ### inputs to observation model ###
  Z = "identity", # default - 6 state
  A = "zero",
  D = "zero" ,
  d = "zero",
  R = "diagonal and equal", # obs. error covariance matrix
  ### initial conditions ###
  tinitx = 0,
  V0 = "zero"
)

# Fit MARSS model

# fit BFGS with priors - started 10:19am - 10:35am
kemfit <- MARSS(y = dat_dep, model = mod_list,
                control = list(maxit = 100, allow.degen = TRUE, 
                               trace = 1, safe = TRUE), fit = TRUE)

# use kemfit to inform MARSS fit - started 10:35am - 10:42am
fit_gb22_6state <- MARSS(y = dat_dep, model = mod_list,
             control = list(maxit = 5000), method = "BFGS", inits = kemfit$par)

# export model fit
# saveRDS(fit_gb22_6state,
#         file = "data_model_outputs/marss_fit_gb22_6state_mBFGS_110923.rds")

# DIAGNOSES 
## check for hidden errors
# some don't appear in output in console
# this should print all of them out, those displayed and those hidden
fit_gb22_6state[["errors"]]
# NULL

### Compare to null model ###
# make sure this matches the fitted model in all ways besides the inclusion of C and c
mod_list_null <- list(
  ### inputs to process model ###
  B = "diagonal and unequal",
  U = "zero",
  #C = CC, 
  #c = dat_cov,
  Q = "diagonal and unequal", # proc. error covariance matrix
  ### inputs to observation model ###
  Z = "identity", # 6 state
  A = "zero",
  D = "zero" ,
  d = "zero",
  R = "diagonal and equal", # obs. error covariance matrix
  ### initial conditions ###
  tinitx = 0,
  V0 = "zero"
)

# 10:43am - 10:53 am
null.kemfit <- MARSS(y = dat_dep, model = mod_list_null,
                     control = list(maxit= 100, allow.degen=TRUE, 
                                    trace=1), fit=TRUE) # default method = "EM"

# 10:55am - 10:57am
null.fit_gb22_6state <- MARSS(y = dat_dep, model = mod_list_null,
                  control = list(maxit = 5000), method = "BFGS", 
                  inits=null.kemfit$par)

MARSSaic(fit_gb22_6state) # AICc -12824.72
MARSSaic(null.fit_gb22_6state) # AICc -10797.7 

### **** Autoplot diagnoses: VIEW AND RESPOND TO Qs BELOW **** ###
autoplot.marssMLE(fit_gb22_6state)

# Plots 1 (xtT) & 2 (fitted.ytT): Do fitted values seem reasonable? Yes

# Plot 3 (model.resids.ytt1): Do resids have temporal patterns?  No
# Do 95% of resids fall withing the CIs? Seems to although density makes
# it a bit hard to discern

# Plot 4 (std.model.resids.ytT): Do resids have temporal patterns?  No
# Do 95% of resids fall withing the CIs? Yes

# Plot 5 (std.state.resids.xtT): Any outliers? No

# Plots 6 & 7 (qqplot.std.model.resids.ytt1: Are resids normal (straight lines)?
# Kind of curvy - need to ask Alex about this

# Plot 8 (acf.std.model.resids.ytt1): Do resids have temporal autocorrelation?
# 24 hour lag in nearshore sites - OOOOOOO

##### Plot Results #####
# For 95th percentile credible intervals.
gb22_6state_est95 <- MARSSparamCIs(fit_gb22_6state, alpha = 0.05)

# Unable to calculate CIs, so this likely means the model is a poor fit.

# Format confidence intervals into dataframes
gb22_6state_CI95 = data.frame(
  "Est." = gb22_6state_est95$par$U,
  "Lower" = gb22_6state_est95$par.lowCI$U,
  "Upper" = gb22_6state_est95$par.upCI$U)
gb22_6state_CI95$Parameter = rownames(gb22_6state_CI95)
gb22_6state_CI95[,1:3] = round(gb22_6state_CI95[,1:3], 3)
gb22_6state_CI95$Model = "6 state"

# Plot results
(gb_light_fig <- ggplot(gb22_6state_CI95, aes(x = Parameter, y = Est.)) + 
    # coloring by both percentiles but using 99th perc. error bars to be most conservative
    geom_errorbar(aes(ymin = Lower, ymax = Upper),
                  position=position_dodge(width = 0.5), width = 0) +
    geom_point(position = position_dodge(width = 0.5), 
               alpha = 0.8, size = 8) + 
    theme_bw()+
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    coord_flip(ylim = c(-1, 1)) + 
    labs(y = "Effect Size", 
         x = "Covariates"))

# Export plot.
# ggsave(("MARSS_gb_light_110923.png"),
#        path = "figures",
#        width = 10,
#        height = 10,
#        units = "cm"
# )

#### MARSS 2022 no covar. ####

# Since the introduction of a covariate (above) yielded poor results,
# I will next investigate the spatial structure of the full dataset.

# Guided by approach in Chapter 9 of MARSS User Guide
# https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf

# H1 12 sites all behaving differently - 12 states
# H2 Water depths on certain sides behave similarly - 8 state
# H3 Water depths behave similarly - 4 state
# H4 Sides of the lake behave similarly - 2 state
# H5 All sites behave similarly - 1 state

# Will assume independent process errors with same variance (Q)

##### Data prep #####

# Use dat_all_2022 but filter down to stratification period only
# May 27 - November 23, 2022

dat_strat_2022 <- dat_all_2022 %>%
  filter(Date > ymd_hms("2022-05-27 00:00:00")) %>%
  filter(Date < ymd_hms("2022-11-23 24:00:00"))

##### Model fit ##### 

# Now to start manipulating the dataframe into matrix format.
gb <- as.data.frame(rep(seq(1,4338), 6)) %>%
  rename(index = `rep(seq(1, 4338), 6)`)
bw <- as.data.frame(rep(seq(1,3751), 6)) %>%
  rename(index = `rep(seq(1, 3751), 6)`)

index_all <- rbind(gb, bw)

# first need to add unique sitenames
dat_strat_2022_wide <- dat_strat_2022 %>%
  mutate(site_name = paste(site, location, replicate)) %>%
  # need to add index numbers to make each row "unique" otherwise the
  # next step throws an error
  mutate(index = index_all) %>%
  # pivot wider for MARSS format
  select(site_name, index,
    mean_percDOsat) %>% 
  pivot_wider(
    names_from = site_name, 
    values_from = c(mean_percDOsat))

# indicate column #s of response and predictor vars
names(dat_strat_2022_wide)
resp_cols = c(2:13)

# scale transform response var
dat_22_scale <- dat_strat_2022_wide
dat_22_scale[,resp_cols] = scale(dat_22_scale[,resp_cols])

# Pull out only response var
dat_dep <- t(dat_22_scale[,c(resp_cols)])

# Model setup for MARSS 

# Different Z matrix construction
# H1 12 state
Z1 <- factor(c("east10m", "east15m", "east20m", "east3m1", "east3m2", "east3m3",
               "west10m", "west15m", "west20m", "west3m1", "west3m2", "west3m3"))

# H2 8 state
Z2 <- factor(c("east10m", "east15m", "east20m", "east3m", "east3m", "east3m",
               "west10m", "west15m", "west20m", "west3m", "west3m", "west3m"))

# H3 4 state
Z3 <- factor(c("10m", "15m", "20m", "3m", "3m", "3m",
               "10m", "15m", "20m", "3m", "3m", "3m"))

# H4 2 state
Z4 <- factor(c("east", "east", "east", "east", "east", "east",
               "west", "west", "west", "west", "west", "west"))

# H5 1 state
Z5 <- factor(rep("lake", 12))

# Combine them all
Z.models <- list(Z1, Z2, Z3, Z4, Z5)
names(Z.models) <- c("site", "shore+depth", "depth", "shore", "lake")

mod_list <- list(
  ### inputs to process model ###
  B = "identity",
  U = "unequal",
  Q = "unconstrained", # proc. error covariance matrix
  ### inputs to observation model ###
  A = "scaling",
  R = "diagonal and equal", # obs. error covariance matrix
  ### initial conditions ###
  tinitx = 0,
  x0 = "unequal",
  V0 = "zero"
)

# Fit MARSS model

# Q unconstrained 2:57pm - ?pm
out.tab <- NULL
fits <- list()
for (i in 1:length(Z.models)) {
  
  fit.model <- c(list(Z = Z.models[[i]]), mod_list)
  
  fit <- MARSS(y = dat_dep, 
               model = fit.model,
               silent = TRUE,
               control = list(maxit = 1000))
  
  out <- data.frame(
    H = names(Z.models) [i], Q = "unconstrained", U = "unequal",
    logLik = fit$logLik, AICc = fit$AICc, num.param = fit$num.params,
    m = length(unique(Z.models[[i]])),
    num.iter = fit$numIter, converged = !fit$convergence,
    stringsAsFactors = FALSE)
  
  out.tab <- rbind(out.tab, out)
  
  fits <- c(fits, list(fit))
  
}

# export model fit
# saveRDS(fits,
#         file = "data_model_outputs/marss_fit_strat22_allstates_Quncon_113023.rds")

# Evaluate model fits using AICc and AIC weights
# Sort fits based on AICc
min.AICc <- order(out.tab$AICc)
out.tab.1 <- out.tab[min.AICc,]

# Add the delta AICc values
out.tab.1 <- cbind(out.tab.1,
                   delta.AICc = out.tab.1$AICc - out.tab.1$AICc[1])

# Add the relative likelihood (- delta AICc/2)
out.tab.1 <- cbind(out.tab.1,
                   rel.like = exp(-1 * out.tab.1$delta.AICc / 2))

# Add AIC weight (relative likelihood / sum of all relative likelihoods)
out.tab.1 <- cbind(out.tab.1,
                   AIC.weight = out.tab.1$rel.like/sum(out.tab.1$rel.like))

# Examine model weights
out.tab.1

# Results of stratified period 2022 MARSS fit
# H             Q       U     logLik      AICc num.param  m num.iter converged delta.AICc
# 1        site unconstrained unequal  -2963.003  6132.526       103 12     1000     FALSE       0.00
# 2 shore+depth unconstrained unequal -17064.600 34243.360        57  8     1000     FALSE   28110.83
# 4       shore unconstrained unequal -35167.908 70371.833        18  2      101      TRUE   64239.31
# 3       depth unconstrained unequal -36427.503 72909.043        27  4      362      TRUE   66776.52
# 5        lake unconstrained unequal -41635.280 83300.572        15  1       80      TRUE   77168.05

#### Covariate Plots 2022 ####

# First make a trimmed, aggregated weather dataset.
# need to go back to raw sub 15minute timestep data
# bc the aggregated data in dat was giving me some weird values
dat_cov_2022 <- weather_long %>%
  mutate(date = date(Date_TimePST)) %>%
  filter(Date_TimePST > 
           ymd_hms("2022-03-01 00:00:00")) %>%
  filter(Date_TimePST <
           ymd_hms("2023-02-28 23:59:59")) %>%
  group_by(shore, date) %>%
  summarize(light_mean = mean(solar_radiation_set_1, na.rm = TRUE),
            wind_mean = mean(wind_speed_set_1, na.rm = TRUE)) %>%
  ungroup()

# Plot covariates themselves.
(fig_light22 <- ggplot(dat_cov_2022 %>%
                         mutate(shore = factor(shore, levels = c("W", "E"))), 
                       aes(x = date, y = light_mean)) +
   geom_point(color = "#F2B705") +
   labs(x = "Date") +
   ylab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
   theme_bw() +
   facet_grid(.~shore))

(fig_wind22 <- ggplot(dat_cov_2022 %>%
                        mutate(shore = factor(shore, levels = c("W", "E"))), 
                      aes(x = date, y = wind_mean)) +
    geom_point(color = "#c39ca4") +
    labs(x = "Date",
         y = "Mean Daily Windspeed (m/s)") +
    theme_bw() +
    facet_grid(.~shore))

dat_Q_2022 <- discharge %>%
  mutate(date = date(datePST),
         shore = case_when(label == "east" ~ "E",
                           label == "west" ~ "W")) %>%
  filter(datePST > ymd_hms("2022-03-01 00:00:00")) %>%
  filter(datePST < ymd_hms("2023-02-28 23:59:59")) %>%
  group_by(shore, date) %>%
  summarize(discharge_mean = mean(dischargeCMS, na.rm = TRUE),
            discharge_delta = max(dischargeCMS, na.rm = TRUE) -
              min(dischargeCMS, na.rm = TRUE)) %>%
  ungroup()

(fig_Q22 <- ggplot(dat_Q_2022 %>%
                     mutate(shore = factor(shore, levels = c("W", "E"))), 
                      aes(x = date, y = discharge_mean)) +
    geom_point(color = "#336887") +
    labs(x = "Date") +
    ylab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    scale_y_log10(breaks = c(0.01, 0.1, 1, 5)) +
    theme_bw() +
    facet_grid(.~shore))

(fig_cov22 <- fig_light22 / fig_wind22 / fig_Q22)

# Export figure.
# ggsave("figures/2022_light_wind_Q_121223.png",
#        width = 20,
#        height = 18,
#        units = "cm"
# )

# First going to plot DO by light, paneled by shore and water depth (similar to initial plots above).
(fig_light_do22_bw <- ggplot(dat_all_2022 %>%
                            filter(site == "BW") %>%
                         # filter for only daylight hours
                         filter(hour(Date) < 18) %>% # before 6pm
                           filter(hour(Date) > 6) %>% # and after 6am
                         # removing any pelagic measures from here
                         filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
                         # and smoosh down to three levels for coloration
                         mutate(replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                             replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                             TRUE ~ "NS3"),
                                              levels = c("NS1", "NS2", "NS3"))) %>%
                         # and re-order water depths
                         mutate(location = factor(location,
                                                  levels = c("3m", "10m",
                                                             "15m", "20m"))),
                       aes(x = mean_solar, y = mean_percDOsat, color = replicate)) +
   geom_point(alpha = 0.7) +
   scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
   labs(x = "Light (W/m2)",
        y = "DO (% Saturation)") +
   theme_bw() +
   theme(legend.position = "none") +
   facet_grid(location~season))

# Export figure.
# ggsave("figures/2022_do_light_bw_112023.png",
#        width = 12,
#        height = 10,
#        units = "cm"
# )

(fig_light_do22_gb <- ggplot(dat_all_2022 %>%
                               filter(site == "GB") %>%
                               # filter for only daylight hours
                               filter(hour(Date) < 18) %>% # before 6pm
                               filter(hour(Date) > 6) %>% # and after 6am
                               # removing any pelagic measures from here
                               filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
                               # and smoosh down to three levels for coloration
                               mutate(replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                                   replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                                   TRUE ~ "NS3"),
                                                         levels = c("NS1", "NS2", "NS3"))) %>%
                               # and re-order water depths
                               mutate(location = factor(location,
                                                        levels = c("3m", "10m",
                                                                   "15m", "20m"))),
                             aes(x = mean_solar, y = mean_percDOsat, color = replicate)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
    labs(x = "Light (W/m2)",
         y = "DO (% Saturation)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~season))

# Export figure.
# ggsave("figures/2022_do_light_gb_112023.png",
#        width = 12,
#        height = 10,
#        units = "cm"
# )

# Next going to plot DO by wind, paneled by shore and water depth (similar to initial plots above).
(fig_wind_do22_bw <- ggplot(dat_all_2022 %>%
                           filter(site == "BW") %>%
                            # removing any pelagic measures from here
                            filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
                            # and smoosh down to three levels for coloration
                            mutate(replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                                replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                                TRUE ~ "NS3"),
                                                      levels = c("NS1", "NS2", "NS3"))) %>%
                            # and re-order water depths
                            mutate(location = factor(location,
                                                     levels = c("3m", "10m",
                                                                "15m", "20m"))),
                          aes(x = mean_windspeed, y = mean_percDOsat, color = replicate)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    labs(x = "Windspeed (m/s)",
         y = "DO (% Saturation)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~season))

# Export figure.
# ggsave("figures/2022_do_wind_bw_112023.png",
#        width = 12,
#        height = 10,
#        units = "cm"
# )

(fig_wind_do22_gb <- ggplot(dat_all_2022 %>%
                              filter(site == "GB") %>%
                              # removing any pelagic measures from here
                              filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
                              # and smoosh down to three levels for coloration
                              mutate(replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                                                  replicate %in% c("Pelagic", "NS2") ~ "NS2",
                                                                  TRUE ~ "NS3"),
                                                        levels = c("NS1", "NS2", "NS3"))) %>%
                              # and re-order water depths
                              mutate(location = factor(location,
                                                       levels = c("3m", "10m",
                                                                  "15m", "20m"))),
                            aes(x = mean_windspeed, y = mean_percDOsat, color = replicate)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    labs(x = "Windspeed (m/s)",
         y = "DO (% Saturation)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location~season))

# Export figure.
# ggsave("figures/2022_do_wind_gb_112023.png",
#        width = 12,
#        height = 10,
#        units = "cm"
# )

#### Covariate Plots 2023 ####

# First make a trimmed, aggregated weather dataset.
# need to go back to raw sub 15minute timestep data
# bc the aggregated data in dat was giving me some weird values
dat_cov_2023 <- weather_long %>%
  mutate(date = date(Date_TimePST)) %>%
  filter(Date_TimePST > 
           ymd_hms("2023-05-25 00:00:00")) %>%
  filter(Date_TimePST <
           ymd_hms("2023-09-30 23:59:59")) %>%
  group_by(shore, date) %>%
  summarize(light_mean = mean(solar_radiation_set_1, na.rm = TRUE),
            wind_mean = mean(wind_speed_set_1, na.rm = TRUE)) %>%
  ungroup()

# Plot covariates themselves.
(fig_light23 <- ggplot(dat_cov_2023 %>%
                         mutate(shore = factor(shore, levels = c("W", "E"))), 
                       aes(x = date, y = light_mean)) +
    geom_point(color = "#F2B705") +
    labs(x = "Date") +
    ylab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
    theme_bw() +
    facet_grid(.~shore))

(fig_wind23 <- ggplot(dat_cov_2023 %>%
                        mutate(shore = factor(shore, levels = c("W", "E"))), 
                      aes(x = date, y = wind_mean)) +
    geom_point(color = "#c39ca4") +
    labs(x = "Date",
         y = "Mean Daily Windspeed (m/s)") +
    theme_bw() +
    facet_grid(.~shore))

dat_Q_2023 <- discharge %>%
  mutate(date = date(datePST),
         shore = case_when(label == "east" ~ "E",
                           label == "west" ~ "W")) %>%
  filter(datePST > ymd_hms("2023-05-25 00:00:00")) %>%
  filter(datePST < ymd_hms("2023-09-30 23:59:59")) %>%
  group_by(shore, date) %>%
  summarize(discharge_mean = mean(dischargeCMS, na.rm = TRUE),
            discharge_delta = max(dischargeCMS, na.rm = TRUE) -
              min(dischargeCMS, na.rm = TRUE)) %>%
  ungroup()

(fig_Q23 <- ggplot(dat_Q_2023 %>%
                     mutate(shore = factor(shore, levels = c("W", "E"))), 
                   aes(x = date, y = discharge_mean)) +
    geom_point(color = "#336887") +
    labs(x = "Date") +
    ylab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    scale_y_log10(breaks = c(0.01, 0.1, 1, 10)) +
    theme_bw() +
    facet_grid(.~shore))

(fig_cov23 <- fig_light23 / fig_wind23 / fig_Q23)

# Export figure.
# ggsave("figures/2023_light_wind_Q_121223.png",
#        width = 15,
#        height = 18,
#        units = "cm"
# )

#### Autocorrelation Plots 2022 ####

##### BW #####

dat_bw_3m_NS1 <- dat_all_2022 %>%
  filter(site == "BW",
         location == "3m",
         replicate == "NS1")

forecast::ggtsdisplay(dat_bw_3m_NS1$mean_percDOsat, lag.max = 100)

# ggsave("figures/2022_bw3mNS1_pacf_110923.png",
#        width = 15,
#        height = 9,
#        units = "cm"
# )

dat_bw_3m_NS2 <- dat_all_2022 %>%
  filter(site == "BW",
         location == "3m",
         replicate == "NS2")

forecast::ggtsdisplay(dat_bw_3m_NS2$mean_percDOsat, lag.max = 100)

dat_bw_3m_NS3 <- dat_all_2022 %>%
  filter(site == "BW",
         location == "3m",
         replicate == "NS3")

forecast::ggtsdisplay(dat_bw_3m_NS3$mean_percDOsat, lag.max = 100)

dat_bw_10m <- dat_all_2022 %>%
  filter(site == "BW",
         location == "10m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_bw_10m$mean_percDOsat, lag.max = 100)

dat_bw_15m <- dat_all_2022 %>%
  filter(site == "BW",
         location == "15m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_bw_15m$mean_percDOsat, lag.max = 100)

dat_bw_20m <- dat_all_2022 %>%
  filter(site == "BW",
         location == "20m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_bw_20m$mean_percDOsat, lag.max = 100)

# ggsave("figures/2022_bw20m_pacf_110923.png",
#        width = 15,
#        height = 9,
#        units = "cm"
# )

##### GB #####

dat_gb_3m_NS1 <- dat_all_2022 %>%
  filter(site == "GB",
         location == "3m",
         replicate == "NS1")

forecast::ggtsdisplay(dat_gb_3m_NS1$mean_percDOsat, lag.max = 100)

# ggsave("figures/2022_gb3mNS1_pacf_110923.png",
#        width = 15,
#        height = 9,
#        units = "cm"
# )

dat_gb_3m_NS2 <- dat_all_2022 %>%
  filter(site == "GB",
         location == "3m",
         replicate == "NS2")

forecast::ggtsdisplay(dat_gb_3m_NS2$mean_percDOsat, lag.max = 100)

dat_gb_3m_NS3 <- dat_all_2022 %>%
  filter(site == "GB",
         location == "3m",
         replicate == "NS3")

forecast::ggtsdisplay(dat_gb_3m_NS3$mean_percDOsat, lag.max = 100)

dat_gb_10m <- dat_all_2022 %>%
  filter(site == "GB",
         location == "10m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_gb_10m$mean_percDOsat, lag.max = 100)

dat_gb_15m <- dat_all_2022 %>%
  filter(site == "GB",
         location == "15m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_gb_15m$mean_percDOsat, lag.max = 100)

dat_gb_20m <- dat_all_2022 %>%
  filter(site == "GB",
         location == "20m",
         replicate == "Benthic")

forecast::ggtsdisplay(dat_gb_20m$mean_percDOsat, lag.max = 100)

# ggsave("figures/2022_gb20m_pacf_110923.png",
#        width = 15,
#        height = 9,
#        units = "cm"
# )

#### DO Amplitude ####

# Rather than using the raw data, I'll instead be investigating using
# daily DO amplitude as a response variable.

dat_amp <- dat_clean %>% # using the full cleaned dataset from above
  mutate(date = date(Pacific_Standard_Time)) %>% # create a date only column
  group_by(site, location, replicate, date) %>% # group by day
  summarize(temp_max = max(Temperature_deg_C, na.rm = TRUE),
            temp_min = min(Temperature_deg_C, na.rm = TRUE),
            temp_amp = max(Temperature_deg_C, na.rm = TRUE) - 
              min(Temperature_deg_C, na.rm = TRUE),
            percDOsat_max = max(percDOsat, na.rm = TRUE),
            percDOsat_min = min(percDOsat, na.rm = TRUE),
            percDOsat_amp = max(percDOsat, na.rm = TRUE) - 
              min(percDOsat, na.rm = TRUE)) %>% # calculate the daily amplitudes
  ungroup() %>%
  mutate(shore = case_when(site %in% c("GB", "SH") ~ "E",
                           site %in% c("BW", "SS") ~ "W"))

# need to go back to raw sub 15minute timestep data
# bc the aggregated data in dat was giving me some weird values
dat_cov <- weather_long %>%
  mutate(date = date(Date_TimePST)) %>%
  group_by(shore, date) %>%
  summarize(light_mean = mean(solar_radiation_set_1, na.rm = TRUE),
            wind_mean = mean(wind_speed_set_1, na.rm = TRUE)) %>%
  ungroup()

# and do the same for the discharge data
dat_Q <- discharge %>%
  mutate(date = date(datePST),
         shore = case_when(label == "east" ~ "E",
                           label == "west" ~ "W")) %>%
  group_by(shore, date) %>%
  summarize(discharge_mean = mean(dischargeCMS, na.rm = TRUE),
            discharge_delta = max(dischargeCMS, na.rm = TRUE) -
              min(dischargeCMS, na.rm = TRUE)) %>%
  ungroup()

dat_amp1 <- left_join(dat_amp, dat_cov, by = c("shore", "date"))
dat_amp <- left_join(dat_amp1, dat_Q, by = c("shore", "date"))

##### Plots #####

###### BW 2022 ######

# Make dataset for plotting.
dat_amp_BW22 <- dat_amp %>%
  filter(site %in% c("BW")) %>%
  # remove pelagic data for display
  filter(replicate %in% c("Benthic", "NS1", 
                          "NS2", "NS3")) %>%
  filter(date > ymd("2022-03-01")) %>%
  filter(date < ymd("2023-02-28")) %>%
  mutate(location_f = factor(case_when(location == "20m" ~ "deep littoral",
                                       location == "15m" ~ "mid-depth litt.",
                                       location == "10m" ~ "shallow litt.",
                                       location == "3m" ~ "nearshore"),
                             levels = c("nearshore", "shallow litt.",
                                        "mid-depth litt.", "deep littoral")),
         # new group for better coloration
         replicate = factor(case_when(replicate %in% 
                                        c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% 
                                        c("Pelagic", "NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", 
                                       "NS3")))
# DO amplitude
(fig_bw_do_amp22 <- ggplot(dat_amp_BW22, aes(x = date, y = percDOsat_amp,
                           group = month(date), color = replicate)) +
   geom_point(alpha = 0.75) +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   labs(x = "Date",
        y = "Δ Daily DO (% Saturation)",
        title = "Stage I - West Shore") +
    theme_bw() +
    facet_grid(location_f~.) +
    theme(legend.position = "none",
          strip.text.y = element_blank()))

# Temperature amplitude
(fig_bw_t_amp22 <- ggplot(dat_amp_BW22, aes(x = date, 
                          y = temp_amp, color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Δ Daily Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two ts plots and export.
(fig_bw_amp22 <- fig_bw_do_amp22 + fig_bw_t_amp22)

# ggsave("figures/2022_data_bw_amp_020824.png",
#        width = 30,
#        height = 12,
#        units = "cm"
# )

# DO vs. temp amplitude
(fig_bw_do_temp_amp22 <- ggplot(dat_amp_BW22, aes(x = temp_amp, y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    xlab(expression(paste({Delta}," Daily Temperature (", ~degree*C, ")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean light
(fig_bw_do_light_mean22 <- ggplot(dat_amp_BW22, aes(x = light_mean, y = percDOsat_amp,
                                                   color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
    xlab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean wind
(fig_bw_do_wind_mean22 <- ggplot(dat_amp_BW22, aes(x = wind_mean, y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    xlab(expression(paste("Mean Daily Windspeed (m/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. discharge amplitude
(fig_bw_do_Q_amp22 <- ggplot(dat_amp_BW22, aes(x = discharge_delta, 
                                               y = percDOsat_amp,
                                               color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste({Delta}, "Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10(breaks = c(0.01, 0.1, 1, 10))+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean discharge
(fig_bw_do_Q_mean22 <- ggplot(dat_amp_BW22, aes(x = discharge_mean, 
                                                y = percDOsat_amp,
                                                color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10(breaks = c(0.01, 0.1, 1, 10))+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine covariate figures
(fig_bw_do_covar_amp22 <- fig_bw_do_temp_amp22 |
    fig_bw_do_light_mean22 |
    fig_bw_do_wind_mean22 |
    fig_bw_do_Q_mean22)

# ggsave("figures/2022_data_bw_covar_amp_121223.png",
#        width = 28,
#        height = 15,
#        units = "cm"
# )

###### GB 2022 ######

# Make dataset for plotting
dat_amp_GB22 <- dat_amp %>%
  filter(site %in% c("GB")) %>%
  # remove pelagic data for display
  filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
  filter(date > ymd("2022-03-01")) %>%
  filter(date < ymd("2023-02-28")) %>%
  mutate(location_f = factor(case_when(location == "20m" ~ "deep littoral",
                                       location == "15m" ~ "mid-depth litt.",
                                       location == "10m" ~ "shallow litt.",
                                       location == "3m" ~ "nearshore"),
                             levels = c("nearshore", "shallow litt.",
                                        "mid-depth litt.", "deep littoral")),
         replicate = factor(case_when(replicate %in% c("Benthic",
                                                       "NS1") ~ "NS1",
                                      replicate %in% c("Pelagic",
                                                       "NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", "NS3")))
# DO Amplitude
(fig_gb_do_amp22 <- ggplot(dat_amp_GB22, aes(x = date, y = percDOsat_amp,
                           color = replicate)) +
   geom_point(alpha = 0.75) +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   labs(x = "Date",
        y = "Δ Daily DO (% Saturation)",
        title = "Stage I - East Shore") +
    theme_bw() +
    facet_grid(location_f~.) +
    theme(legend.position = "none",
          strip.text.y = element_blank()))

# Temperature Amplitude
(fig_gb_t_amp22 <- ggplot(dat_amp_GB22, aes(x = date, y = temp_amp,
                          color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Δ Daily Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two and export.
(fig_gb_amp22 <- fig_gb_do_amp22 + fig_gb_t_amp22)

# ggsave("figures/2022_data_gb_amp_020824.png",
#        width = 30,
#        height = 12,
#        units = "cm"
# )

# DO vs. temp amplitude
(fig_gb_do_temp_amp22 <- ggplot(dat_amp_GB22, aes(x = temp_amp, y = percDOsat_amp,
                                    color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    xlab(expression(paste({Delta}," Daily Temperature (", ~degree, "C)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean light
(fig_gb_do_light_mean22 <- ggplot(dat_amp_GB22, aes(x = light_mean, y = percDOsat_amp,
                                    color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
    xlab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean wind
(fig_gb_do_wind_mean22 <- ggplot(dat_amp_GB22, aes(x = wind_mean, y = percDOsat_amp,
                                                   color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    xlab(expression(paste("Mean Daily Windspeed (m/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. discharge amplitude
(fig_gb_do_Q_amp22 <- ggplot(dat_amp_GB22, aes(x = discharge_delta, 
                                               y = percDOsat_amp,
                                               color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste({Delta}, "Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1))+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean discharge
(fig_gb_do_Q_mean22 <- ggplot(dat_amp_GB22, aes(x = discharge_mean, 
                                               y = percDOsat_amp,
                                               color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1))+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine covariate figures
(fig_gb_do_covar_amp22 <- fig_gb_do_temp_amp22 |
  fig_gb_do_light_mean22 |
  fig_gb_do_wind_mean22 |
  fig_gb_do_Q_mean22)

# ggsave("figures/2022_data_gb_covar_amp_121223.png",
#        width = 28,
#        height = 15,
#        units = "cm"
# )

# One additional stream Q figure for presentation just in case.
(fig_bw_gb_do_Q_amp22 <- fig_bw_do_Q_amp22 |
  fig_gb_do_Q_amp22)

# ggsave("figures/2022_data_gb_bw_Qamp_121223.png",
#        width = 14,
#        height = 15,
#        units = "cm"
# )

###### BW 2023 ######

# Make dataset for plotting.
dat_amp_BW23 <- dat_amp %>%
  filter(site %in% c("BW", "SS")) %>%
  filter(location %in% c("10m", "3m")) %>%
  filter(date > ymd("2023-03-01")) %>%
  filter(date < ymd("2023-09-30")) %>%
  mutate(location_f = factor(case_when(site == "BW" &
                                       location == "3m" ~ "n.s. near stream",
                                     site == "SS" &
                                       location == "3m" ~ "n.s. far stream",
                                     TRUE ~ "shallow littoral"),
                           levels = c("n.s. near stream", 
                                      "n.s. far stream", 
                                      "shallow littoral")),
         replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                            levels = c("NS1", "NS2", "NS3")))

# DO amplitude
(fig_bw_do_amp23 <- ggplot(dat_amp_BW23, aes(x = date, 
                           y = percDOsat_amp, color = replicate)) +
   geom_point(alpha = 0.75) +
   scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
   scale_x_date(date_labels = "%b %Y") +
   labs(x = "Date",
        y = "Δ Daily DO (% Saturation)",
        title = "Stage II - West Shore") +
   theme_bw() +
   theme(legend.position = "none",
          strip.text.y = element_blank()) +
   facet_grid(location_f~.))

# Temperature amplitude
(fig_bw_t_amp23 <- ggplot(dat_amp_BW23, aes(x = date, 
                          y = temp_amp, color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date", 
         y = "Δ Daily Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two ts plots and export.
(fig_bw_amp23 <- fig_bw_do_amp23 + fig_bw_t_amp23)

# ggsave("figures/2023_data_bw_amp_020824.png",
#        width = 18,
#        height = 10,
#        units = "cm"
# )

# DO vs. temp amplitude
(fig_bw_do_temp_amp23 <- ggplot(dat_amp_BW23, aes(x = temp_amp, 
                                                  y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    xlab(expression(paste({Delta}," Daily Temperature (", ~degree*C,")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean light
(fig_bw_do_light_mean23 <- ggplot(dat_amp_BW23, aes(x = light_mean, 
                                                    y = percDOsat_amp,
                                                   color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
    xlab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean windspeed
(fig_bw_do_wind_mean23 <- ggplot(dat_amp_BW23, aes(x = wind_mean, 
                                                   y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    xlab(expression(paste("Mean Daily Windspeed (m/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean discharge
(fig_bw_do_Q_mean23 <- ggplot(dat_amp_BW23, aes(x = discharge_mean, 
                                                y = percDOsat_amp,
                                                color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10(breaks = c(0.1, 1, 10))+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine covariate figures
(fig_bw_do_covar_amp23 <- fig_bw_do_temp_amp23 |
    fig_bw_do_light_mean23 |
    fig_bw_do_wind_mean23 |
    fig_bw_do_Q_mean23)

# ggsave("figures/2023_data_bw_covar_amp_121223.png",
#        width = 28,
#        height = 15,
#        units = "cm"
# )

###### GB 2023 ######

# Make dataset for plotting.
dat_amp_GB23 <- dat_amp %>%
  filter(site %in% c("GB", "SH")) %>%
  filter(location %in% c("10m", "3m")) %>%
  filter(date > ymd("2023-03-01")) %>%
  filter(date < ymd("2023-09-30")) %>%
  mutate(location_f = factor(case_when(site == "GB" & location == "3m" ~ "n.s. near stream",
                                     site == "SH" & location == "3m" ~ "n.s. far stream",
                                     TRUE ~ "shallow littoral"),
                           levels = c("n.s. near stream", "n.s. far stream", "shallow littoral")),
         replicate = factor(case_when(replicate %in% c("Benthic", "NS1") ~ "NS1",
                                      replicate %in% c("NS2") ~ "NS2",
                                      TRUE ~ "NS3"),
                           levels = c("NS1", "NS2", "NS3")))

# DO amplitude
(fig_gb_do_amp23 <- ggplot(dat_amp_GB23, aes(x = date, 
                                             y = percDOsat_amp, color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#3B7D6E","#4CA49E","#7AC9B7")) +
    labs(x = "Date",
         y = "Δ Daily DO (% Saturation)",
         title = "Stage II - East Shore") +
    theme_bw() +
    theme(legend.position = "none",
          strip.text.y = element_blank()) +
    facet_grid(location_f~.))

# Temperature amplitude
(fig_gb_t_amp23 <- ggplot(dat_amp_GB23, aes(x = date, 
                                            y = temp_amp, color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    labs(x = "Date",
         y = "Δ Daily Temperature (°C)") +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine the two ts plots and export.
(fig_gb_amp23 <- fig_gb_do_amp23 + fig_gb_t_amp23)

# ggsave("figures/2023_data_gb_amp_020824.png",
#        width = 18,
#        height = 10,
#        units = "cm"
# )

# DO vs. temp amplitude
(fig_gb_do_temp_amp23 <- ggplot(dat_amp_GB23, aes(x = temp_amp, y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#5A7ECB","#4B8FF7","#59A3F8")) +
    xlab(expression(paste({Delta}," Daily Temperature (", ~degree*C,")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean light
(fig_gb_do_light_mean23 <- ggplot(dat_amp_GB23, aes(x = light_mean, y = percDOsat_amp,
                                                   color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#F2B705","#F28705","#D95204")) +
    xlab(expression(paste("Mean Daily Light (W/", m^{2}, ")"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean windspeed
(fig_gb_do_wind_mean23 <- ggplot(dat_amp_GB23, aes(x = wind_mean, y = percDOsat_amp,
                                                  color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#c39ca4","#713d3f","#381f21")) +
    xlab(expression(paste("Mean Daily Windspeed (m/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# DO vs. mean discharge
(fig_gb_do_Q_mean23 <- ggplot(dat_amp_GB23, aes(x = discharge_mean, 
                                                y = percDOsat_amp,
                                                color = replicate)) +
    geom_point(alpha = 0.75) +
    scale_color_manual(values = c("#A9B4BC","#8197A4","#336887")) +
    xlab(expression(paste("Mean Daily Discharge (", m^{3}, "/s)"))) +
    ylab(expression(paste({Delta}," Daily DO (% Saturation)"))) +
    scale_x_log10()+
    theme_bw() +
    theme(legend.position = "none") +
    facet_grid(location_f~.))

# Combine covariate figures
(fig_gb_do_covar_amp23 <- fig_gb_do_temp_amp23 |
    fig_gb_do_light_mean23 |
    fig_gb_do_wind_mean23 | 
    fig_gb_do_Q_mean23)

# ggsave("figures/2023_data_gb_covar_amp_121223.png",
#        width = 28,
#        height = 15,
#        units = "cm"
# )

# End of script.
