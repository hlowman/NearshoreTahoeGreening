# Posthoc Regression Script
# Authors: Heili E. Lowman
# Date Created: 2024-11-01

# ---------------------------- README ---------------------------------
# The following script will fit logistic regressions.

# NOTE - DO (% saturation) is what is included in the main portion of the
# manuscript, so these are the models that will fit what is present in
# the text. The DO concentrations in mg/L include temperature as a covariate,
# and these regressions are instead presented in the SI.

#### Setup ####

# Load packages.
library(lubridate)
library(tidyverse)
library(here)
library(viridis)
library(patchwork)
library(GGally)
library(brms)
library(tidybayes)
library(bayesplot)

# Load data.
data_2022 <- readRDS("data_working/do_covariate_daily_data_2022_052525.rds")
data_2023 <- readRDS("data_working/do_covariate_daily_data_2023_052525.rds")

# Adding water depth column to both.
data_2022 <- data_2022 %>%
  mutate(w_depth = case_when(location == "3m" ~ 3,
                             location == "10m" ~ 10,
                             location == "15m" ~ 15,
                             location == "20m" ~ 20))

data_2023 <- data_2023 %>%
  mutate(w_depth = case_when(location == "3m" ~ 3,
                             location == "10m" ~ 10,
                             location == "15m" ~ 15,
                             location == "20m" ~ 20))

#### 2022 DO Fit ####

##### Data QAQC #####

# First, we would typically check data missingness,
# but since we've created this dataset in the
# script prior, this has already been done.

# Instead, I'll do a quick gut check of correlated
# variables.
covs22 <- ggpairs(data_2022 %>%
          select(w_depth, min_dosat:delta_q))

# ggsave(covs22,
#        filename = "figures/covariates_2022.jpg",
#        width = 50,
#        height = 50,
#        units = "cm")

# Next, we will select only the columns of interest
# for this analysis, namely:
# - clustering group (dependent variable)
# - cumulative daily light
# - mean daily water temperature
# - mean daily windspeed
# - mean daily discharge

# Ensured, using plot above, that none were correlated
# i.e., above 0.7.

data_2022_select <- data_2022 %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            location == "10m" ~ "10m",
                            location == "15m" ~ "15m",
                            location == "20m" ~ "20m",
                            TRUE ~ NA)) %>%
  select(group, site, sensor,
         sum_light, mean_wtemp, mean_ws, mean_q)

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group, data = data_2022_select)
boxplot(mean_wtemp ~ group, data = data_2022_select)
boxplot(mean_ws ~ group, data = data_2022_select)
boxplot(log(mean_q) ~ group, data = data_2022_select)

# Also examine across sites & sensors.
# Want these to be roughly similar variance since using as random intercepts.
boxplot(sum_light ~ site, data = data_2022_select)
boxplot(mean_wtemp ~ site, data = data_2022_select)
boxplot(mean_ws ~ site, data = data_2022_select)
boxplot(log(mean_q) ~ site, data = data_2022_select)

# expecting this since BW is a much larger creek
boxplot(sum_light ~ sensor, data = data_2022_select)
boxplot(mean_wtemp ~ sensor, data = data_2022_select)
boxplot(mean_ws ~ sensor, data = data_2022_select)
boxplot(log(mean_q) ~ sensor, data = data_2022_select)
# Ok, these look alright to include as nested random effects.

# Need to make factors, log scale q, and scale transform
# numeric variables.
data_2022_select <- data_2022_select %>%
  mutate(group = factor(group,
                        # making the "Neither" group the reference variable
                        levels = c("Neither",
                                   "Cluster 1",
                                   "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_temp = scale(mean_wtemp),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2022_multireg <- data_2022_select %>%
  select(group, site, sensor,
         scale_light, scale_temp, scale_wind, scale_q)

# saveRDS(data_2022_multireg, "data_working/clustering_multireg_050425.rds")

##### Model Fit #####

# Note, after much googling, it seems no packages support
# the frequentist format as well as the Bayesian ones,
# so defaulting to a Bayesian approach here only for that
# reason, not because we necessarily have priors for
# any of the covariates.

# Fit multilevel multinomial logistic regression model.
fit_2022 <- brm(group ~ scale_light + 
                  scale_temp +
                  scale_wind + 
                  scale_q +
                  #scale_depth +
                  (1|site/sensor), # nested random effect
                data = data_2022_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~20 minutes on laptop.
# Started at 12:30 pm. Finished at 12:48.

# Save model fit.
saveRDS(fit_2022,
        "data_model_outputs/brms_2022_050425_nodepth.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2022)
# Despite 125 divergent transitions, Rhats look good!

plot(fit_2022, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_temp",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_temp",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022, type = "neff")
# Only one at 0.1.

# Examine relationships for each predictor. 
plot(conditional_effects(fit_2022, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_temp",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_q",
                         categorical = TRUE))
# light greater in cluster 2
# temp greater in cluster 1
# wind greater in cluster 2
# discharge lower in cluster 2

##### Visualization #####

# Create conditional effects object to better customize plots.
# https://discourse.mc-stan.org/t/change-linetype-aestetics-conditional-effects/14962
c_eff <- conditional_effects(fit_2022, categorical = T)

# Light prediction plot.
c_eff_light <- as.data.frame(c_eff$`scale_light`)

(figSI_light <- ggplot(c_eff_light,
                   aes(x = scale_light,
                       y = estimate__, 
                       group = cats__)) +
  geom_ribbon(aes(ymin = lower__, 
                  ymax = upper__, 
                  fill = cats__), alpha = 0.2) +
  geom_line(size = 1, aes(color = cats__))+
  scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                               "Cluster 2"= "#D46F10", 
                               "Neither"= "gray70"),
                    guide = "none") +
  scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                "Cluster 2"= "#D46F10", 
                                "Neither"= "gray70"),
                     guide = "none")+
  labs(y = "Probability", 
       x = "Scaled Light") +
  theme_bw())

# Temperature prediction plot.
c_eff_temp <- as.data.frame(c_eff$`scale_temp`)

(figSI_temp <- ggplot(c_eff_temp,
                       aes(x = scale_temp,
                           y = estimate__, 
                           group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Temperature") +
    theme_bw())

# Wind prediction plot.
c_eff_wind <- as.data.frame(c_eff$`scale_wind`)

(figSI_wind <- ggplot(c_eff_wind,
                      aes(x = scale_wind,
                          y = estimate__, 
                          group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70")) +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"))+
    labs(y = "Probability", 
         x = "Scaled Windspeed",
         color = "Membership",
         fill = "Membership") +
    theme_bw()) # +
    #theme(legend.position = c(0.8, 0.8)))

# Discharge prediction plot.
c_eff_q <- as.data.frame(c_eff$`scale_q`)

(figSI_q <- ggplot(c_eff_q, 
                   aes(x = scale_q,
                       y = estimate__, 
                       group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Discharge") +
    theme_bw())

# Combine posterior predictive plots into a single figure
# to be included in the supplemental information.
(figSI_StageI <- figSI_temp | figSI_light | figSI_q | figSI_wind)

# ggsave(figSI_StageI,
#        filename = "figures/S9_StageI_PredPlots.jpg",
#        height = 10,
#        width = 40,
#        units = "cm")

# Examine the posterior data.
post_data <- mcmc_intervals_data(fit_2022,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

View(post_data)

(fig_custom <- ggplot(post_data %>%
                          filter(parameter %in% c("b_muCluster1_scale_light",
                                                  "b_muCluster1_scale_temp",
                                                  "b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster2_scale_light",
                                                  "b_muCluster2_scale_temp",
                                                  "b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q")) %>%
                          mutate(par_f = factor(parameter, 
                                                levels = c("b_muCluster1_scale_wind",
                                                           "b_muCluster2_scale_wind",
                                                           "b_muCluster1_scale_q",
                                                           "b_muCluster2_scale_q",
                                                           "b_muCluster1_scale_light",
                                                           "b_muCluster2_scale_light",
                                                           "b_muCluster1_scale_temp",
                                                           "b_muCluster2_scale_temp"))), 
                        aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   linewidth = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage I") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_temp" = "Cluster 1 Temp.",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_temp" = "Cluster 2 Temp.",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q")) +
    theme_bw() +
    scale_color_manual(values = c("#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# ggsave(fig_custom,
#        filename = "figures/brms_2022_050425.jpg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Also making supplementary figure for model including depth.
fit_2022_wdepth <- readRDS("data_model_outputs/brms_2022_050425_nointeract.rds")

# Examine the posterior data.
post_data_wdepth <- mcmc_intervals_data(fit_2022_wdepth,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

(fig_custom_SI <- ggplot(post_data_wdepth %>%
                        filter(parameter %in% c("b_muCluster1_scale_light",
                                                "b_muCluster1_scale_temp",
                                                "b_muCluster1_scale_wind",
                                                "b_muCluster1_scale_q",
                                                "b_muCluster1_scale_depth",
                                                "b_muCluster2_scale_light",
                                                "b_muCluster2_scale_temp",
                                                "b_muCluster2_scale_wind",
                                                "b_muCluster2_scale_q",
                                                "b_muCluster2_scale_depth")) %>%
                        mutate(par_f = factor(parameter, 
                                              levels = c("b_muCluster1_scale_depth",
                                                         "b_muCluster2_scale_depth",
                                                         "b_muCluster1_scale_wind",
                                                         "b_muCluster2_scale_wind",
                                                         "b_muCluster1_scale_q",
                                                         "b_muCluster2_scale_q",
                                                         "b_muCluster1_scale_light",
                                                         "b_muCluster2_scale_light",
                                                         "b_muCluster1_scale_temp",
                                                         "b_muCluster2_scale_temp"))), 
                      aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   linewidth = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage I") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_temp" = "Cluster 1 Temp.",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster1_scale_depth" = "Cluster 1 Depth",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_temp" = "Cluster 2 Temp.",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q",
                                "b_muCluster2_scale_depth" = "Cluster 2 Depth")) +
    theme_bw() +
    scale_color_manual(values = c("#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# ggsave(fig_custom_SI,
#        filename = "figures/brms_2022_wdepth_050525.jpg",
#        height = 20,
#        width = 20,
#        units = "cm")

#### 2022 DOsat Fit ####

##### Data QAQC #####

# First, we would typically check data missingness,
# but since we've created this dataset in the
# script prior, this has already been done.

# Examined correlated variables above.

# Next, we will select the columns of interest:
# - clustering group (dependent variable)
# - water depth
# - cumulative daily light
# - mean daily windspeed
# - mean daily discharge

data_2022_select_dosat <- data_2022 %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            location == "10m" ~ "10m",
                            location == "15m" ~ "15m",
                            location == "20m" ~ "20m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, w_depth,
         sum_light, mean_ws, mean_q)

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group_dosat, data = data_2022_select_dosat)
boxplot(w_depth ~ group_dosat, data = data_2022_select_dosat)
boxplot(mean_ws ~ group_dosat, data = data_2022_select_dosat)
boxplot(log(mean_q) ~ group_dosat, data = data_2022_select_dosat)

# Also examine across sites & sensors.
# Want these to be roughly similar variance since using as random intercepts.
boxplot(sum_light ~ site, data = data_2022_select_dosat)
boxplot(w_depth ~ site, data = data_2022_select_dosat)
boxplot(mean_ws ~ site, data = data_2022_select_dosat)
boxplot(log(mean_q) ~ site, data = data_2022_select_dosat)
# expecting this since BW is a much larger creek
boxplot(sum_light ~ sensor, data = data_2022_select_dosat)
boxplot(w_depth ~ sensor, data = data_2022_select_dosat)
boxplot(mean_ws ~ sensor, data = data_2022_select_dosat)
boxplot(log(mean_q) ~ sensor, data = data_2022_select_dosat)
# Ok, these look alright to include as nested random effects.

# Need to make factors, log scale q, and scale transform
# numeric variables.
data_2022_select_dosat <- data_2022_select_dosat %>%
  mutate(group_dosat = factor(group_dosat,
                        # making the "Neither" group the reference variable
                        levels = c("Neither",
                                   "Cluster 1",
                                   "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_depth = scale(w_depth),
         scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2022_multireg_dosat <- data_2022_select_dosat %>%
  select(group_dosat, site, sensor,
         scale_depth, scale_light, scale_wind, scale_q)

# saveRDS(data_2022_multireg_dosat, 
#         "data_working/clustering_dosat_multireg_052525.rds")

##### Model Fit #####

# Fit multilevel multinomial logistic regression model.
fit_2022_dosat <- brm(group_dosat ~ scale_depth + 
                  scale_light +
                  scale_wind + 
                  scale_q +
                  (1|site/sensor), # nested random effect
                data = data_2022_multireg_dosat,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~60 minutes on laptop.
# Started at 11:05 pm. Finished at 12:08.

# Save model fit.
saveRDS(fit_2022_dosat,
        "data_model_outputs/brms_dosat_2022_052525.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2022_dosat)
# Only 54 divergent transitions, Rhats look good!

plot(fit_2022_dosat, variable = c("b_muCluster1_scale_depth",
                            "b_muCluster1_scale_light",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_depth",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_dosat, type = "neff")
# All good!

# Examine relationships for each predictor. 
plot(conditional_effects(fit_2022_dosat, effects = "scale_depth",
                         categorical = TRUE))
plot(conditional_effects(fit_2022_dosat, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2022_dosat, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2022_dosat, effects = "scale_q",
                         categorical = TRUE))
# depth effect is meh
# light greater in cluster 2
# wind greater in cluster 2
# discharge greater in cluster 1

##### Visualization #####

# Create conditional effects object to better customize plots.
# https://discourse.mc-stan.org/t/change-linetype-aestetics-conditional-effects/14962
c_eff_sat <- conditional_effects(fit_2022_dosat, categorical = T)

# Light prediction plot.
c_eff_sat_light <- as.data.frame(c_eff_sat$`scale_light`)

(figSI_light_sat <- ggplot(c_eff_sat_light,
                       aes(x = scale_light,
                           y = estimate__, 
                           group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Light") +
    theme_bw())

# Depth prediction plot.
c_eff_sat_depth <- as.data.frame(c_eff_sat$`scale_depth`)

(figSI_depth_sat <- ggplot(c_eff_sat_depth,
                      aes(x = scale_depth,
                          y = estimate__, 
                          group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Depth") +
    theme_bw())

# Wind prediction plot.
c_eff_sat_wind <- as.data.frame(c_eff_sat$`scale_wind`)

(figSI_wind_sat <- ggplot(c_eff_sat_wind,
                      aes(x = scale_wind,
                          y = estimate__, 
                          group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70")) +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"))+
    labs(y = "Probability", 
         x = "Scaled Windspeed",
         color = "Membership",
         fill = "Membership") +
    theme_bw())

# Discharge prediction plot.
c_eff_sat_q <- as.data.frame(c_eff_sat$`scale_q`)

(figSI_q_sat <- ggplot(c_eff_sat_q, 
                   aes(x = scale_q,
                       y = estimate__, 
                       group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#FABA39FF", 
                                 "Cluster 2"= "#D46F10", 
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#FABA39FF", 
                                  "Cluster 2"= "#D46F10", 
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Discharge") +
    theme_bw())

# Combine posterior predictive plots into a single figure
# to be included in the supplemental information.
(figSI_StageI_sat <- figSI_depth_sat | figSI_light_sat | figSI_q_sat | figSI_wind_sat)

# ggsave(figSI_StageI_sat,
#        filename = "figures/S9_DOsat_StageI_PredPlots.jpg",
#        height = 10,
#        width = 40,
#        units = "cm")

# Examine the posterior data.
post_data_sat <- mcmc_intervals_data(fit_2022_dosat,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

View(post_data_sat)

(fig_custom_dosat <- ggplot(post_data_sat %>%
                        filter(parameter %in% c("b_muCluster1_scale_depth",
                                                "b_muCluster1_scale_light",
                                                "b_muCluster1_scale_wind",
                                                "b_muCluster1_scale_q",
                                                "b_muCluster2_scale_depth",
                                                "b_muCluster2_scale_light",
                                                "b_muCluster2_scale_wind",
                                                "b_muCluster2_scale_q")) %>%
                        mutate(par_f = factor(parameter, 
                                              levels = c("b_muCluster1_scale_wind",
                                                         "b_muCluster2_scale_wind",
                                                         "b_muCluster1_scale_q",
                                                         "b_muCluster2_scale_q",
                                                         "b_muCluster1_scale_light",
                                                         "b_muCluster2_scale_light",
                                                         "b_muCluster1_scale_depth",
                                                         "b_muCluster2_scale_depth"))), 
                      aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   linewidth = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(limits = c(-6,6),
                       breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage I") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Light (Syn.)",
                                "b_muCluster1_scale_depth" = "Depth (Syn.)",
                                "b_muCluster1_scale_wind" = "Wind (Syn.",
                                "b_muCluster1_scale_q" = "Q (Syn.)",
                                "b_muCluster2_scale_light" = "Light (Lag)",
                                "b_muCluster2_scale_depth" = "Depth (Lag)",
                                "b_muCluster2_scale_wind" = "Wind (Lag)",
                                "b_muCluster2_scale_q" = "Q (Lag)")) +
    theme_bw() +
    scale_color_manual(values = c("#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

#### 2023 DO Fit ####

##### Data QAQC #####

# First, we would typically check data missingness,
# but since we've created this dataset in the
# script prior, this has already been done.

# Instead, I'll do a quick gut check of correlated
# variables.
covs23 <- ggpairs(data_2023 %>%
                    select(w_depth,min_dosat:delta_q))

# ggsave(covs23,
#        filename = "figures/covariates_2023.jpg",
#        width = 50,
#        height = 50,
#        units = "cm")

# Next, we will select only the columns of interest
# for this analysis, namely:
# - clustering group (dependent variable)
# - cumulative daily light
# - mean daily temperature
# - mean daily windspeed
# - mean daily discharge

# Ensured, using plot above, that none were correlated
# i.e., above 0.7.

data_2023_select <- data_2023 %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group, site, sensor,
         sum_light, mean_wtemp, mean_ws, mean_q) %>%
  # and creating new column with edited Q data
  # to delineate no flow at SS/SH sites
  # making this a small number rather than zero
  # so log scaling will still work below
  mutate(mean_q_ed = case_when(site %in% c("BW", "GB") ~ mean_q,
                               site %in% c("SH", "SS") ~ 0.0001))

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group, data = data_2023_select)
boxplot(mean_wtemp ~ group, data = data_2023_select)
boxplot(mean_ws ~ group, data = data_2023_select)
boxplot(log(mean_q_ed) ~ group, data = data_2023_select)

# Also examine across sites & sensors.
boxplot(sum_light ~ site, data = data_2023_select)
boxplot(mean_wtemp ~ site, data = data_2023_select)
boxplot(mean_ws ~ site, data = data_2023_select)
boxplot(log(mean_q_ed) ~ site, data = data_2023_select)
# again expecting this since BW is a much larger creek

boxplot(sum_light ~ sensor, data = data_2023_select)
boxplot(mean_wtemp ~ sensor, data = data_2023_select)
boxplot(mean_ws ~ sensor, data = data_2023_select)
boxplot(log(mean_q_ed) ~ sensor, data = data_2023_select)
# Ok, these look alright to include as nested random effects.

# Need to make factors, log scale q, and scale transform
# numeric variables.
data_2023_select <- data_2023_select %>%
  mutate(group = factor(group,
                        # making base the "Neither" group
                        levels = c("Neither",
                                   "Cluster 1",
                                   "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q_ed)) %>%
  mutate(scale_light = scale(sum_light),
         scale_temp = scale(mean_wtemp),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2023_multireg <- data_2023_select %>%
  select(group, site, sensor,
         scale_light, scale_temp, scale_wind, scale_q)

# saveRDS(data_2023_multireg, "data_working/clustering_multireg23_050425.rds")

##### Model Fit #####

# Fit multilevel multinomial logistic regression model.
fit_2023 <- brm(group ~ scale_light + 
                  scale_temp + 
                  scale_wind + 
                  scale_q +
                  (1|site/sensor), # nested random effect
                data = data_2023_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~4 minutes on laptop.
# Started at 1:19 pm. Finished at 1:23.

# Save model fit.
# saveRDS(fit_2023, "data_model_outputs/brms_2023_050425.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2023)
# Has 205 divergent transitions, but Rhats look good!

plot(fit_2023, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_temp",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_temp",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023, type = "neff")
# Hmm some Neffs are < 0.1

# Examine relationships for each predictor.
plot(conditional_effects(fit_2023, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2023, effects = "scale_temp",
                         categorical = TRUE))
plot(conditional_effects(fit_2023, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2023, effects = "scale_q",
                         categorical = TRUE))

# Appears cumulative daily light is lowest and temperature
# is greatest in Cluster 1, high discharge most strongly predicts
# Cluster 1, and windspeed is equivocal.

##### Visualization #####

# Create conditional effects object to better customize plots.
# https://discourse.mc-stan.org/t/change-linetype-aestetics-conditional-effects/14962
c_eff23 <- conditional_effects(fit_2023, categorical = T)

# Light prediction plot.
c_eff23_light <- as.data.frame(c_eff23$`scale_light`)

(figSI23_light <- ggplot(c_eff23_light,
                       aes(x = scale_light,
                           y = estimate__, 
                           group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Light") +
    theme_bw())

# Temperature prediction plot.
c_eff23_temp <- as.data.frame(c_eff23$`scale_temp`)

(figSI23_temp <- ggplot(c_eff23_temp,
                      aes(x = scale_temp,
                          y = estimate__, 
                          group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Temperature") +
    theme_bw())

# Wind prediction plot.
c_eff23_wind <- as.data.frame(c_eff23$`scale_wind`)

(figSI23_wind <- ggplot(c_eff23_wind,
                      aes(x = scale_wind,
                          y = estimate__, 
                          group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70")) +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"))+
    labs(y = "Probability", 
         x = "Scaled Windspeed",
         color = "Membership",
         fill = "Membership") +
    theme_bw()) # +
#theme(legend.position = c(0.8, 0.8)))

# Discharge prediction plot.
c_eff23_q <- as.data.frame(c_eff23$`scale_q`)

(figSI23_q <- ggplot(c_eff23_q, 
                   aes(x = scale_q,
                       y = estimate__, 
                       group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Discharge") +
    theme_bw())

# Combine posterior predictive plots into a single figure
# to be included in the supplemental information.
(figSI_StageII <- figSI23_temp | figSI23_light | figSI23_q | figSI23_wind)

# ggsave(figSI_StageII,
#        filename = "figures/S10_StageII_PredPlots.jpg",
#        height = 10,
#        width = 40,
#        units = "cm")

# Examine the posterior data.
post_data23 <- mcmc_intervals_data(fit_2023,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

View(post_data23)

(fig_custom23 <- ggplot(post_data23 %>%
                        filter(parameter %in% c("b_muCluster1_scale_light",
                                                "b_muCluster1_scale_temp",
                                                "b_muCluster1_scale_wind",
                                                "b_muCluster1_scale_q",
                                                "b_muCluster2_scale_light",
                                                "b_muCluster2_scale_temp",
                                                "b_muCluster2_scale_wind",
                                                "b_muCluster2_scale_q")) %>%
                        mutate(par_f = factor(parameter, 
                                              levels = c("b_muCluster1_scale_wind",
                                                         "b_muCluster2_scale_wind",
                                                         "b_muCluster1_scale_q",
                                                         "b_muCluster2_scale_q",
                                                         "b_muCluster1_scale_light",
                                                         "b_muCluster2_scale_light",
                                                         "b_muCluster1_scale_temp",
                                                         "b_muCluster2_scale_temp"))), 
                      aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   size = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8, 10, 12, 14, 16)) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage II") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_temp" = "Cluster 1 Temp.",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_temp" = "Cluster 2 Temp.",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q")) +
    theme_bw() +
    scale_color_manual(values = c("#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# ggsave(fig_custom23,
#        filename = "figures/brms_2023_050425.jpg",
#        height = 15,
#        width = 20,
#        units = "cm")

#### 2023 DOsat Fit ####

##### Data QAQC #####

# First, we would typically check data missingness,
# but since we've created this dataset in the
# script prior, this has already been done.

# Checked correlated variables above.

# Next, we will select only the columns of interest:
# - clustering group (dependent variable)
# - cumulative daily light
# - mean daily windspeed
# - mean daily discharge

# Ensured, using plot above, that none were correlated
# i.e., above 0.7.

data_2023_select_dosat <- data_2023 %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor,
         sum_light, mean_ws, mean_q) %>%
  # and creating new column with edited Q data
  # to delineate no flow at SS/SH sites
  # making this a small number rather than zero
  # so log scaling will still work below
  mutate(mean_q_ed = case_when(site %in% c("BW", "GB") ~ mean_q,
                               site %in% c("SH", "SS") ~ 0.0001))

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group_dosat, data = data_2023_select_dosat)
boxplot(mean_ws ~ group_dosat, data = data_2023_select_dosat)
boxplot(log(mean_q_ed) ~ group_dosat, data = data_2023_select_dosat)

# Also examine across sites & sensors.
boxplot(sum_light ~ site, data = data_2023_select_dosat)
boxplot(mean_ws ~ site, data = data_2023_select_dosat)
boxplot(log(mean_q_ed) ~ site, data = data_2023_select_dosat)
# again expecting this since BW is a much larger creek
boxplot(sum_light ~ sensor, data = data_2023_select_dosat)
boxplot(mean_ws ~ sensor, data = data_2023_select_dosat)
boxplot(log(mean_q_ed) ~ sensor, data = data_2023_select_dosat)
# Ok, these look alright to include as nested random effects.

# Need to make factors, log scale q, and scale transform
# numeric variables.
data_2023_select_dosat <- data_2023_select_dosat %>%
  mutate(group_dosat = factor(group_dosat,
                        # making base the "Neither" group
                        levels = c("Neither",
                                   "Cluster 1",
                                   "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q_ed)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2023_multireg_dosat <- data_2023_select_dosat %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# saveRDS(data_2023_multireg_dosat, 
#         "data_working/clustering_dosat_multireg23_052525.rds")

##### Model Fit #####

# Fit multilevel multinomial logistic regression model.
fit_2023_dosat <- brm(group_dosat ~ scale_light + 
                  scale_wind + 
                  scale_q +
                  (1|site/sensor), # nested random effect
                data = data_2023_multireg_dosat,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~10 minutes on laptop.
# Started at 12:49 pm. Finished at 12:57.

# Save model fit.
saveRDS(fit_2023_dosat, "data_model_outputs/brms_dosat_2023_052525.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2023_dosat)
# Only 1 divergent transition, and Rhats look good!

plot(fit_2023_dosat, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023_dosat, type = "neff")
# No Neffs are < 0.1

# Examine relationships for each predictor.
plot(conditional_effects(fit_2023_dosat, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2023_dosat, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2023_dosat, effects = "scale_q",
                         categorical = TRUE))

# Appears cumulative daily light is lowest in Cluster 1, 
# high discharge most strongly predicts Cluster 2, 
# and windspeed is equivocal.

##### Visualization #####

# Create conditional effects object to better customize plots.
# https://discourse.mc-stan.org/t/change-linetype-aestetics-conditional-effects/14962
c_eff23_sat <- conditional_effects(fit_2023_dosat, categorical = T)

# Light prediction plot.
c_eff23_sat_light <- as.data.frame(c_eff23_sat$`scale_light`)

(figSI23_light_sat <- ggplot(c_eff23_sat_light,
                         aes(x = scale_light,
                             y = estimate__, 
                             group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Light") +
    theme_bw())

# Wind prediction plot.
c_eff23_sat_wind <- as.data.frame(c_eff23_sat$`scale_wind`)

(figSI23_wind_sat <- ggplot(c_eff23_sat_wind,
                        aes(x = scale_wind,
                            y = estimate__, 
                            group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70")) +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"))+
    labs(y = "Probability", 
         x = "Scaled Windspeed",
         color = "Membership",
         fill = "Membership") +
    theme_bw()) # +
#theme(legend.position = c(0.8, 0.8)))

# Discharge prediction plot.
c_eff23_sat_q <- as.data.frame(c_eff23_sat$`scale_q`)

(figSI23_q_sat <- ggplot(c_eff23_sat_q, 
                     aes(x = scale_q,
                         y = estimate__, 
                         group = cats__)) +
    geom_ribbon(aes(ymin = lower__, 
                    ymax = upper__, 
                    fill = cats__), alpha = 0.2) +
    geom_line(size = 1, aes(color = cats__))+
    scale_fill_manual(values = c("Cluster 1"= "#0FB2D3",
                                 "Cluster 2"= "#026779",
                                 "Neither"= "gray70"),
                      guide = "none") +
    scale_color_manual(values = c("Cluster 1"= "#0FB2D3",
                                  "Cluster 2"= "#026779",
                                  "Neither"= "gray70"),
                       guide = "none")+
    labs(y = "Probability", 
         x = "Scaled Discharge") +
    theme_bw())

# Combine posterior predictive plots into a single figure
# to be included in the supplemental information.
(figSI_StageII_sat <- figSI23_light_sat | figSI23_q_sat | figSI23_wind_sat)

# ggsave(figSI_StageII_sat,
#        filename = "figures/S10_DOsat_StageII_PredPlots.jpg",
#        height = 10,
#        width = 30,
#        units = "cm")

# Examine the posterior data.
post_data23_sat <- mcmc_intervals_data(fit_2023_dosat,
                                   point_est = "median", # default = "median"
                                   prob = 0.66, # default = 0.5
                                   prob_outer = 0.95) # default = 0.9

View(post_data23_sat)

(fig_custom23_dosat <- ggplot(post_data23_sat %>%
                          filter(parameter %in% c("b_muCluster1_scale_light",
                                                  "b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster2_scale_light",
                                                  "b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q")) %>%
                          mutate(par_f = factor(parameter, 
                                                levels = c("b_muCluster2_scale_wind",
                                                           "b_muCluster1_scale_wind",
                                                           "b_muCluster2_scale_q",
                                                           "b_muCluster1_scale_q",
                                                           "b_muCluster2_scale_light",
                                                           "b_muCluster1_scale_light"))), 
                        aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   linewidth = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(limits = c(-6, 6),
                       breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage II") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Light (Lag)",
                                "b_muCluster1_scale_wind" = "Wind (Lag)",
                                "b_muCluster1_scale_q" = "Q (Lag)",
                                "b_muCluster2_scale_light" = "Light (Syn)",
                                "b_muCluster2_scale_wind" = "Wind (Syn)",
                                "b_muCluster2_scale_q" = "Q (Syn)")) +
    theme_bw() +
    scale_color_manual(values = c("#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779")) +
    theme(text = element_text(size = 20),
          axis.title.y = element_blank(),
          legend.position = "none"))

#### Manuscript Figures ####

# Join the plots above into a single figure.
(fig_custom_both <- (fig_custom + fig_custom23) +
   plot_annotation(tag_levels = 'A'))

# ggsave(fig_custom_both,
#        filename = "figures/brms_bothyrs_050425.jpg",
#        height = 20,
#        width = 40,
#        units = "cm")

# Join the % saturation plots above into a single figure.
(fig_custom_both_dosat <- (fig_custom_dosat + fig_custom23_dosat) +
    plot_annotation(tag_levels = 'A'))

# ggsave(fig_custom_both_dosat,
#        filename = "figures/brms_dosat_bothyrs_112425.jpg",
#        height = 20,
#        width = 40,
#        units = "cm")

#### Site-Level Fits ####

# Fitting models to each sensor by stage
# for DO % sat data to create final regression figure for SI
# with points separated out by site.

##### 2022 GBNS Fit #####

# Select the data & columns of interest:
data_2022_gbns <- data_2022 %>%
  filter(site == "GB") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_gbns <- data_2022_gbns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_gbns <- data_2022_gbns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_gbns <- brm(group_dosat ~ scale_light +
                        scale_wind + 
                        scale_q +
                        (1|sensor), # single random effect
                      data = data_2022_multireg_gbns,
                      # specify categorical if vectorized data
                      family = categorical())

# Runs in ~3 minutes on laptop.
# Started at 3:50 pm. Finished at 3:52.

# Save model fit.
saveRDS(fit_2022_gbns,
        "data_model_outputs/brms_dosat_2022_gbns_052625.rds")

# Examine model fit.
summary(fit_2022_gbns)
# 2 divergent transitions, Rhats look good!

plot(fit_2022_gbns, variable = c("b_muCluster1_scale_light",
                                  "b_muCluster1_scale_wind",
                                  "b_muCluster1_scale_q",
                                  "b_muCluster2_scale_light",
                                  "b_muCluster2_scale_wind",
                                  "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_gbns, type = "neff")
# Nope!

# Examine the posterior data.
post_data_gbns <- mcmc_intervals_data(fit_2022_gbns,
                                     point_est = "median",
                                     prob = 0.66, # default = 0.5
                                     prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_gbns <- post_data_gbns %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "GB",
         location = "NS")

##### 2022 GBSL Fit #####

# Select the data & columns of interest:
data_2022_gbsl <- data_2022 %>%
  filter(site == "GB") %>%
  filter(location == "10m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "10m" ~ "10m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_gbsl <- data_2022_gbsl %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_gbsl <- data_2022_gbsl %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_gbsl <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_gbsl,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_gbsl,
        "data_model_outputs/brms_dosat_2022_gbsl_052625.rds")

# Examine model fit.
summary(fit_2022_gbsl)
# 337 divergent transitions, Rhats look meh

plot(fit_2022_gbsl, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_gbsl, type = "neff")
# Lots below 0.1...

# Examine the posterior data.
post_data_gbsl <- mcmc_intervals_data(fit_2022_gbsl,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_gbsl <- post_data_gbsl %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "GB",
         location = "SL")

##### 2022 GBML Fit #####

# Select the data & columns of interest:
data_2022_gbml <- data_2022 %>%
  filter(site == "GB") %>%
  filter(location == "15m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "15m" ~ "15m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_gbml <- data_2022_gbml %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_gbml <- data_2022_gbml %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_gbml <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_gbml,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_gbml,
        "data_model_outputs/brms_dosat_2022_gbml_052625.rds")

# Examine model fit.
summary(fit_2022_gbml)
# 55 divergent transitions, Rhats look good!

plot(fit_2022_gbml, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_gbml, type = "neff")
# 2 below 0.1...

# Examine the posterior data.
post_data_gbml <- mcmc_intervals_data(fit_2022_gbml,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_gbml <- post_data_gbml %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "GB",
         location = "ML")

##### 2022 GBDL Fit #####

# Select the data & columns of interest:
data_2022_gbdl <- data_2022 %>%
  filter(site == "GB") %>%
  filter(location == "20m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "20m" ~ "20m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_gbdl <- data_2022_gbdl %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_gbdl <- data_2022_gbdl %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_gbdl <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_gbdl,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_gbdl,
        "data_model_outputs/brms_dosat_2022_gbdl_052625.rds")

# Examine model fit.
summary(fit_2022_gbdl)
# 25 divergent transitions, Rhats look good!

plot(fit_2022_gbdl, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_gbdl, type = "neff")
# Nope!

# Examine the posterior data.
post_data_gbdl <- mcmc_intervals_data(fit_2022_gbdl,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_gbdl <- post_data_gbdl %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "GB",
         location = "DL")

##### 2022 BWNS Fit #####

# Select the data & columns of interest:
data_2022_bwns <- data_2022 %>%
  filter(site == "BW") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_bwns <- data_2022_bwns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_bwns <- data_2022_bwns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_bwns <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_bwns,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~3 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_bwns,
        "data_model_outputs/brms_dosat_2022_bwns_052625.rds")

# Examine model fit.
summary(fit_2022_bwns)
# 9 divergent transitions, Rhats look good!

plot(fit_2022_bwns, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_bwns, type = "neff")
# Nope!

# Examine the posterior data.
post_data_bwns <- mcmc_intervals_data(fit_2022_bwns,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_bwns <- post_data_bwns %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "BW",
         location = "NS")

##### 2022 BWSL Fit #####

# Select the data & columns of interest:
data_2022_bwsl <- data_2022 %>%
  filter(site == "BW") %>%
  filter(location == "10m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "10m" ~ "10m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_bwsl <- data_2022_bwsl %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_bwsl <- data_2022_bwsl %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_bwsl <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_bwsl,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_bwsl,
        "data_model_outputs/brms_dosat_2022_bwsl_052625.rds")

# Examine model fit.
summary(fit_2022_bwsl)
# 90 divergent transitions, Rhats look fine

plot(fit_2022_bwsl, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_bwsl, type = "neff")
# Lots below 0.1...

# Examine the posterior data.
post_data_bwsl <- mcmc_intervals_data(fit_2022_bwsl,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_bwsl <- post_data_bwsl %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "BW",
         location = "SL")

##### 2022 BWML Fit #####

# Select the data & columns of interest:
data_2022_bwml <- data_2022 %>%
  filter(site == "BW") %>%
  filter(location == "15m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "15m" ~ "15m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_bwml <- data_2022_bwml %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_bwml <- data_2022_bwml %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_bwml <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_bwml,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_bwml,
        "data_model_outputs/brms_dosat_2022_bwml_052625.rds")

# Examine model fit.
summary(fit_2022_bwml)
# 12 divergent transitions, Rhats look good!

plot(fit_2022_bwml, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_bwml, type = "neff")
# Nope!

# Examine the posterior data.
post_data_bwml <- mcmc_intervals_data(fit_2022_bwml,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_bwml <- post_data_bwml %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "BW",
         location = "ML")

##### 2022 BWDL Fit #####

# Select the data & columns of interest:
data_2022_bwdl <- data_2022 %>%
  filter(site == "BW") %>%
  filter(location == "20m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(location == "20m" ~ "20m",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2022_bwdl <- data_2022_bwdl %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2022_multireg_bwdl <- data_2022_bwdl %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2022_bwdl <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2022_multireg_bwdl,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~2 minutes on laptop.

# Save model fit.
saveRDS(fit_2022_bwdl,
        "data_model_outputs/brms_dosat_2022_bwdl_052625.rds")

# Examine model fit.
summary(fit_2022_bwdl)
# 10 divergent transitions, Rhats look good!

plot(fit_2022_bwdl, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks alright!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022_bwdl, type = "neff")
# Nope!

# Examine the posterior data.
post_data_bwdl <- mcmc_intervals_data(fit_2022_bwdl,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_bwdl <- post_data_bwdl %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "BW",
         location = "DL")

##### 2023 GBNS Fit #####

# Select the data & columns of interest:
data_2023_gbns <- data_2023 %>%
  filter(site == "GB") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2023_gbns <- data_2023_gbns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2023_multireg_gbns <- data_2023_gbns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2023_gbns <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2023_multireg_gbns,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~1 minute on laptop.

# Save model fit.
saveRDS(fit_2023_gbns,
        "data_model_outputs/brms_dosat_2023_gbns_052625.rds")

# Examine model fit.
summary(fit_2023_gbns)
# 4 divergent transitions, Rhats look good!

plot(fit_2023_gbns, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023_gbns, type = "neff")
# Nope!

# Examine the posterior data.
post_data_gbns23 <- mcmc_intervals_data(fit_2023_gbns,
                                      point_est = "median",
                                      prob = 0.66, # default = 0.5
                                      prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_gbns23 <- post_data_gbns23 %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "GB",
         location = "NS")

##### 2023 SHNS Fit #####

# Select the data & columns of interest:
data_2023_shns <- data_2023 %>%
  filter(site == "SH") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2023_shns <- data_2023_shns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2023_multireg_shns <- data_2023_shns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2023_shns <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2023_multireg_shns,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~1 minute on laptop.

# Save model fit.
saveRDS(fit_2023_shns,
        "data_model_outputs/brms_dosat_2023_shns_052625.rds")

# Examine model fit.
summary(fit_2023_shns)
# 21 divergent transitions, Rhats look good!

plot(fit_2023_shns, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023_shns, type = "neff")
# Quite a few...

# Examine the posterior data.
post_data_shns23 <- mcmc_intervals_data(fit_2023_shns,
                                        point_est = "median",
                                        prob = 0.66, # default = 0.5
                                        prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_shns23 <- post_data_shns23 %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "SH",
         location = "NS")

##### 2023 BWNS Fit #####

# Select the data & columns of interest:
data_2023_bwns <- data_2023 %>%
  filter(site == "BW") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2023_bwns <- data_2023_bwns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2023_multireg_bwns <- data_2023_bwns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2023_bwns <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2023_multireg_bwns,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~1 minute on laptop.

# Save model fit.
saveRDS(fit_2023_bwns,
        "data_model_outputs/brms_dosat_2023_bwns_052625.rds")

# Examine model fit.
summary(fit_2023_bwns)
# No divergent transitions and Rhats look good!
# but lots of exceedances of maximum tree depth

plot(fit_2023_bwns, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks pretty crummy...

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023_bwns, type = "neff")
# Quite a few...

# Examine the posterior data.
post_data_bwns23 <- mcmc_intervals_data(fit_2023_bwns,
                                        point_est = "median",
                                        prob = 0.66, # default = 0.5
                                        prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_bwns23 <- post_data_bwns23 %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "BW",
         location = "NS")

##### 2023 SSNS Fit #####

# Select the data & columns of interest:
data_2023_ssns <- data_2023 %>%
  filter(site == "SS") %>%
  filter(location == "3m") %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group_dosat, site, sensor, 
         sum_light, mean_ws, mean_q)

# Need to make proper transformations.
data_2023_ssns <- data_2023_ssns %>%
  mutate(group_dosat = factor(group_dosat,
                              # "Neither" group as reference
                              levels = c("Neither",
                                         "Cluster 1",
                                         "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(mean_ws),
         scale_q = scale(log_mean_q))

# Create dataset for model fit.
data_2023_multireg_ssns <- data_2023_ssns %>%
  select(group_dosat, site, sensor,
         scale_light, scale_wind, scale_q)

# Fit multilevel multinomial logistic regression model.
fit_2023_ssns <- brm(group_dosat ~ scale_light +
                       scale_wind + 
                       scale_q +
                       (1|sensor), # single random effect
                     data = data_2023_multireg_ssns,
                     # specify categorical if vectorized data
                     family = categorical())

# Runs in ~1 minute on laptop.

# Save model fit.
saveRDS(fit_2023_ssns,
        "data_model_outputs/brms_dosat_2023_ssns_052625.rds")

# Examine model fit.
summary(fit_2023_ssns)
# 53 divergent transitions and Rhats look good!

plot(fit_2023_ssns, variable = c("b_muCluster1_scale_light",
                                 "b_muCluster1_scale_wind",
                                 "b_muCluster1_scale_q",
                                 "b_muCluster2_scale_light",
                                 "b_muCluster2_scale_wind",
                                 "b_muCluster2_scale_q"))
# Chain mixing looks pretty crummy...

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023_ssns, type = "neff")
# Quite a few...

# Examine the posterior data.
post_data_ssns23 <- mcmc_intervals_data(fit_2023_ssns,
                                        point_est = "median",
                                        prob = 0.66, # default = 0.5
                                        prob_outer = 0.95) # default = 0.9

# Pull out necessary data for final, combined figure.
est_ssns23 <- post_data_ssns23 %>%
  select(parameter, ll, m, hh) %>%
  filter(parameter %in% c("b_muCluster1_scale_light",
                          "b_muCluster1_scale_wind",
                          "b_muCluster1_scale_q",
                          "b_muCluster2_scale_light",
                          "b_muCluster2_scale_wind",
                          "b_muCluster2_scale_q")) %>%
  rename("CIlower" = ll,
         "median" = m,
         "CIupper" = hh) %>%
  mutate(site = "SS",
         location = "NS")

##### SI Figure #####

# Join all datasets with parameter estimates.
s1_est <- rbind(est_gbns, est_gbsl, est_gbml, est_gbdl,
                est_bwns, est_bwsl, est_bwml, est_bwdl)
s2_est <- rbind(est_gbns23, est_shns23,
                est_bwns23, est_ssns23)

# Save out.
saveRDS(s1_est, "data_model_outputs/brms_dosat_2022_estbysite_052625.rds")
saveRDS(s2_est, "data_model_outputs/brms_dosat_2023_estbysite_052625.rds")

# And edit for figure customization.
s1_est <- s1_est %>%
  mutate(par_f = factor(parameter, 
                        levels = c("b_muCluster1_scale_wind",
                                   "b_muCluster2_scale_wind",
                                   "b_muCluster1_scale_q",
                                   "b_muCluster2_scale_q",
                                   "b_muCluster1_scale_light",
                                   "b_muCluster2_scale_light"))) %>%
  mutate(depth_f = factor(location, levels = c("NS", "SL", "ML", "DL"))) %>%
  mutate(fill_f = factor(case_when(site == "GB" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "GBfill1",
                                   site == "GB" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "GBfill2",
                                   site == "BW" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "BWfill1",
                                   site == "BW" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "BWfill2"),
                         levels = c("BWfill1", "BWfill2", "GBfill1", "GBfill2")))

s2_est <- s2_est %>%
  mutate(site_f = factor(site,
                         levels = c("GB", "SH",
                                    "BW", "SS"))) %>%
  mutate(par_f = factor(parameter, 
                        levels = c("b_muCluster1_scale_wind",
                                   "b_muCluster2_scale_wind",
                                   "b_muCluster1_scale_q",
                                   "b_muCluster2_scale_q",
                                   "b_muCluster1_scale_light",
                                   "b_muCluster2_scale_light"))) %>%
  mutate(stream_f = factor(case_when(site %in% c("GB", "BW") ~ "near",
                                     site %in% c("SH", "SS") ~ "far"),
                                     levels = c("near", "far"))) %>%
  mutate(fill_f = factor(case_when(site == "GB" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "GBfill1",
                                   site == "GB" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "GBfill2",
                                   site == "BW" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "BWfill1",
                                   site == "BW" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "BWfill2",
                                   site == "SH" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "SHfill1",
                                   site == "SH" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "SHfill2",
                                   site == "SS" & 
                                     par_f %in% c("b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_light") ~ "SSfill1",
                                   site == "SS" &
                                     par_f %in% c("b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_light") ~ "SSfill2"),
                         levels = c("BWfill1", "BWfill2", "GBfill1", "GBfill2",
                                    "SSfill1", "SSfill2", "SHfill1", "SHfill2"))) %>%
  mutate(CIlower_ed = case_when(CIlower < -10 ~ -10,
                                TRUE ~ CIlower))

# First, the Stage I figure with points by site & depth.
(figSI_custom_dosat22 <- ggplot(s1_est %>%
                                arrange(desc(depth_f), site), 
                            aes(x = median, y = par_f, 
                                color = par_f, 
                                shape = depth_f,
                                fill = fill_f)) +
    geom_linerange(position = position_dodge(width = 0.5),
                   aes(xmin = CIlower, xmax = CIupper),
                   linewidth = 1, alpha = 0.6) +
    geom_point(position = position_dodge(width = 0.5),
                size = 3, stroke = 1) +
    vline_at(v = 0) +
    scale_x_continuous(limits = c(-6,6),
                       breaks = c(-4, -2, 0, 2, 4)) +
    scale_color_manual(values = c("#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10"),
                       guide = FALSE) +
    scale_shape_manual(values = c(21, 22, 23, 24)) +
    scale_fill_manual(values = c("#FABA39FF", "#D46F10",
                                 "white", "white"),
                      guide = FALSE) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage I",
         shape = "Location") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q")) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "bottom"))

# Then, the Stage II figure with points by site & depth.
(figSI_custom_dosat23 <- ggplot(s2_est %>%
                                arrange(site_f), 
                              aes(x = median, y = par_f, 
                                  color = par_f, 
                                  shape = stream_f,
                                  fill = fill_f)) +
    geom_linerange(position = position_dodge(width = 0.5),
                   aes(xmin = CIlower_ed, xmax = CIupper),
                   linewidth = 1, alpha = 0.6) +
    geom_point(position = position_dodge(width = 0.5),
               size = 3, stroke = 1) +
    vline_at(v = 0) +
    scale_x_continuous(limits = c(-10,10),
                       breaks = c(-9, -6, -3, 0, 3, 6, 9)) +
    scale_color_manual(values = c("#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779"),
                       guide = FALSE) +
    scale_shape_manual(values = c(21, 22)) +
    scale_fill_manual(values = c("#0FB2D3", "#026779",
                                 "white", "white",
                                 "#0FB2D3", "#026779",
                                 "white", "white"),
                      guide = FALSE) +
    labs(x = "Posterior Estimates",
         y = "Predictors",
         title = "Stage II",
         shape = "Distance from stream") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q")) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "bottom"))

# Join the plots above into a single figure.
(figSI_custom_dosat <- (figSI_custom_dosat22 + figSI_custom_dosat23) +
    plot_annotation(tag_levels = 'A'))

# ggsave(figSI_custom_dosat,
#        filename = "figures/brms_dosat_sepsites_052625.jpg",
#        height = 30,
#        width = 40,
#        units = "cm")

#### Resources ####

# Online Resources:

# ~ Frequentist Multinomial Regression ~
# https://www.princeton.edu/~otorres/LogitR101.pdf
# https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
# https://www.carlosivanrodriguez.com/guides/statistics/logistic-regression/multinomial-regression/

# ~ Bayesian Multinomial Regression ~
# https://www.andrewheiss.com/blog/2023/08/12/conjoint-multilevel-multinomial-guide/#tldr-moral-of-the-story

# End of script.
