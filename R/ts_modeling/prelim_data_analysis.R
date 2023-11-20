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
                       # remove pelagic data for display
                       filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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
                       # remove pelagic data for display
                       filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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

# ggsave("figures/2022_data_bw_110923.png",
#        width = 20,
#        height = 20,
#        units = "cm"
# )

##### GB 2022 #####
# DO
(fig_gb_do22 <- ggplot(dat_clean %>%
                         filter(site %in% c("GB")) %>%
                         # remove pelagic data for display
                         filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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
                        # remove pelagic data for display
                        filter(replicate %in% c("Benthic", "NS1", "NS2", "NS3")) %>%
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

# ggsave("figures/2022_data_gb_110923.png",
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

# Use dat_all_2022

##### Model fit ##### 

# Now to start manipulating the dataframe into matrix format.
gb <- as.data.frame(rep(seq(1,8743), 6)) %>%
  rename(index = `rep(seq(1, 8743), 6)`)
bw <- as.data.frame(rep(seq(1,7850), 6)) %>%
  rename(index = `rep(seq(1, 7850), 6)`)

index_all <- rbind(gb, bw)

# first need to add unique sitenames
dat_all_2022_wide <- dat_all_2022 %>%
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
names(dat_all_2022_wide)
resp_cols = c(2:13)

# scale transform response var
dat_22_scale <- dat_all_2022_wide
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

# Q diagonal and equal 12:06pm - 2:55pm
# Q unconstrained 3:17pm - 8:31?pm
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
#         file = "data_model_outputs/marss_fit_22_allstates_Quncon_110923.rds")

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

#### Covariate Plots 2022 ####

# Making a new column to split into seasons.
dat_all_2022 <- dat_all_2022 %>%
  mutate(season = factor(case_when(Date < ymd_hms("2022-05-27 00:00:00") ~ "Spring",
                            Date >= ymd_hms("2022-05-27 00:00:00") &
                              Date < ymd_hms("2022-11-23 00:00:00") ~ "Summer/Fall",
                            Date >= ymd_hms("2022-11-23 00:00:00") ~ "Winter"),
                         levels = c("Spring", "Summer/Fall", "Winter")))

# Plot covariates themselves.
(fig_light22 <- ggplot(dat_all_2022 %>%
                         mutate(date = date(Date)) %>%
                         group_by(shore, date) %>%
                         summarize(dailyLm = mean(mean_solar, na.rm = TRUE),
                                   dailyLc = sum(mean_solar, na.rm = TRUE)) %>%
                         ungroup() %>%
                         mutate(shore = factor(shore, levels = c("W", "E"))), 
                       aes(x = date, y = dailyLm)) +
   geom_point(color = "#F2B705") +
   labs(x = "Date",
        y = "Mean Daily Light (W/m2)") +
   theme_bw() +
   facet_grid(.~shore))

(fig_wind22 <- ggplot(dat_all_2022 %>%
                        mutate(date = date(Date)) %>%
                        group_by(shore, date) %>%
                        summarize(dailyWs = mean(mean_windspeed, na.rm = TRUE)) %>%
                        ungroup() %>%
                        mutate(shore = factor(shore, levels = c("W", "E"))), 
                      aes(x = date, y = dailyWs, group = month(date))) +
    geom_point(color = "#c39ca4") +
    labs(x = "Date",
         y = "Mean Daily Windspeed (m/s)") +
    theme_bw() +
    facet_grid(.~shore))

(fig_meteo22 <- fig_light22 / fig_wind22)

# Export figure.
# ggsave("figures/2022_light_wind_110923.png",
#        width = 20,
#        height = 12,
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

# End of script.
