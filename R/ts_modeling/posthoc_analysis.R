# Posthoc Regression Script
# Authors: Heili E. Lowman
# Date Created: 2024-11-01

# ---------------------------- README ---------------------------------
# The following script will fit logistic regressions.

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
data_2022 <- readRDS("data_working/do_covariate_daily_data_2022_050425.rds")
data_2023 <- readRDS("data_working/do_covariate_daily_data_2023_050425.rds")

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

#### 2022 Fit ####

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

#### 2023 Fit ####

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
# 

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

#### Manuscript Figure ####

# Join the plots above into a single figure.
(fig_custom_both <- (fig_custom + fig_custom23) +
   plot_annotation(tag_levels = 'A'))

# ggsave(fig_custom_both,
#        filename = "figures/brms_bothyrs_050425.jpg",
#        height = 20,
#        width = 40,
#        units = "cm")

#### Sources ####

# Online Resources:

# ~ Frequentist Multinomial Regression ~
# https://www.princeton.edu/~otorres/LogitR101.pdf
# https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
# https://www.carlosivanrodriguez.com/guides/statistics/logistic-regression/multinomial-regression/

# ~ Bayesian Multinomial Regression ~
# https://www.andrewheiss.com/blog/2023/08/12/conjoint-multilevel-multinomial-guide/#tldr-moral-of-the-story

# End of script.
