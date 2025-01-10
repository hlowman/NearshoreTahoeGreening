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
data_2022 <- readRDS("data_working/do_covariate_daily_data_2022_111924.rds")
data_2023 <- readRDS("data_working/do_covariate_daily_data_2023_011025.rds")

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
# - maximum daily windspeed
# - mean daily discharge
# - water depth
# Ensured, using plot above, that none were correlated
# i.e., above 0.6.

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
         sum_light, max_ws, mean_q, w_depth)

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group, data = data_2022_select)
boxplot(max_ws ~ group, data = data_2022_select)
boxplot(log(mean_q) ~ group, data = data_2022_select)
boxplot(w_depth ~ group, data = data_2022_select)

# Also examine across sites & sensors.
boxplot(sum_light ~ site, data = data_2022_select)
boxplot(max_ws ~ site, data = data_2022_select)
boxplot(log(mean_q) ~ site, data = data_2022_select)
boxplot(w_depth ~ site, data = data_2022_select)
# expecting this since BW is a much larger creek
boxplot(sum_light ~ sensor, data = data_2022_select)
boxplot(max_ws ~ sensor, data = data_2022_select)
boxplot(log(mean_q) ~ sensor, data = data_2022_select)
boxplot(w_depth ~ sensor, data = data_2022_select)
# Ok, these look alright to include as nested random effects.

# Need to make factors, log scale q, and scale transform
# numeric variables.
data_2022_select <- data_2022_select %>%
  mutate(group = factor(group,
                        # making base the "Neither" group
                        levels = c("Neither",
                                   "Cluster 1",
                                   "Cluster 2")),
         site = factor(site),
         sensor = factor(sensor)) %>%
  mutate(log_mean_q = log(mean_q)) %>%
  mutate(scale_light = scale(sum_light),
         scale_wind = scale(max_ws),
         scale_q = scale(log_mean_q),
         scale_depth = scale(w_depth))

# Create and export data for model fit.
data_2022_multireg <- data_2022_select %>%
  select(group, site, sensor,
         scale_light, scale_wind, scale_q, scale_depth)

# saveRDS(data_2022_multireg, "data_working/clustering_multireg_121724.rds")

##### Model Fit #####

# Note, after much googling, it seems no packages support
# the frequentist format as well as the Bayesian ones,
# so defaulting to a Bayesian approach here only for that
# reason, not because we necessarily have priors for
# any of the covariates.

# Fit multilevel multinomial logistic regression model.
fit_2022 <- brm(group ~ scale_light + 
                  scale_wind + 
                  scale_q*scale_depth +
                  (1|site/sensor), # nested random effect
                data = data_2022_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~20 minutes on laptop.
# Started at 2:16 pm. Finished at 2:40.

# Save model fit.
# saveRDS(fit_2022,
#         "data_model_outputs/brms_2022_121724.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2022)
# Despite 33 divergent transitions, Rhats look good!

plot(fit_2022, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster1_scale_depth",
                            "b_muCluster1_scale_q:scale_depth",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q",
                            "b_muCluster2_scale_depth",
                            "b_muCluster2_scale_q:scale_depth"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022, type = "neff")

# Examine relationships for each predictor.
# Could think about including these in supplement.
plot(conditional_effects(fit_2022, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_q",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_depth",
                         categorical = TRUE))

# Appears mean daily q is greatest in Cluster 1,
# cumulative daily light is greatest in Cluster 2,
# the effect of wind is not as clearly significant,
# and deeper depths typically correspond to Neither.

##### Visualization #####

# Examine the posterior data.
post_data <- mcmc_intervals_data(fit_2022,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

View(post_data)

(fig_custom <- ggplot(post_data %>%
                          filter(parameter %in% c("b_muCluster1_scale_light",
                                                  "b_muCluster1_scale_wind",
                                                  "b_muCluster1_scale_q",
                                                  "b_muCluster1_scale_depth",
                                                  "b_muCluster1_scale_q:scale_depth",
                                                  "b_muCluster2_scale_light",
                                                  "b_muCluster2_scale_wind",
                                                  "b_muCluster2_scale_q",
                                                  "b_muCluster2_scale_depth",
                                                  "b_muCluster2_scale_q:scale_depth")) %>%
                          mutate(par_f = factor(parameter, 
                                                levels = c("b_muCluster1_scale_light",
                                                           "b_muCluster2_scale_light",
                                                           "b_muCluster1_scale_wind",
                                                           "b_muCluster2_scale_wind",
                                                           "b_muCluster1_scale_q",
                                                           "b_muCluster2_scale_q",
                                                           "b_muCluster1_scale_depth",
                                                           "b_muCluster2_scale_depth",
                                                           "b_muCluster1_scale_q:scale_depth",
                                                           "b_muCluster2_scale_q:scale_depth"))), 
                        aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   size = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-2, -1, 0, 1, 2)) +
    labs(x = "Posterior Estimates",
         y = "Predictors") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster1_scale_depth" = "Cluster 1 Depth",
                                "b_muCluster1_scale_q:scale_depth" = "Cluster 1 Q x Depth",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q",
                                "b_muCluster2_scale_depth" = "Cluster 2 Depth",
                                "b_muCluster2_scale_q:scale_depth" = "Cluster 2 Q x Depth")) +
    theme_bw() +
    scale_color_manual(values = c("#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10",
                                  "#FABA39FF", "#D46F10")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# ggsave(fig_custom,
#        filename = "figures/brms_2022_121724.jpg",
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
# - maximum daily windspeed
# - mean daily discharge
# Ensured, using plot above, that none were correlated
# i.e., above 0.6.

data_2023_select <- data_2023 %>%
  # make new "sensor" column
  mutate(sensor = case_when(replicate == "NS1" ~ "NS1",
                            replicate == "NS2" ~ "NS2",
                            replicate == "NS3" ~ "NS3",
                            TRUE ~ NA)) %>%
  select(group, site, sensor,
         sum_light, max_ws, mean_q) %>%
  # and creating new column with edited Q data
  # to delineate no flow at SS/SH sites
  # making this a small number rather than zero
  # so log scaling will still work below
  mutate(mean_q_ed = case_when(site %in% c("BW", "GB") ~ mean_q,
                               site %in% c("SH", "SS") ~ 0.0001))

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(sum_light ~ group, data = data_2023_select)
boxplot(max_ws ~ group, data = data_2023_select)
boxplot(log(mean_q_ed) ~ group, data = data_2023_select)

# Also examine across sites & sensors.
boxplot(sum_light ~ site, data = data_2023_select)
boxplot(max_ws ~ site, data = data_2023_select)
boxplot(log(mean_q_ed) ~ site, data = data_2023_select)
# again expecting this since BW is a much larger creek
boxplot(sum_light ~ sensor, data = data_2023_select)
boxplot(max_ws ~ sensor, data = data_2023_select)
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
         scale_wind = scale(max_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2023_multireg <- data_2023_select %>%
  select(group, site, sensor,
         scale_light, scale_wind, scale_q)

# saveRDS(data_2023_multireg, "data_working/clustering_multireg23_011025.rds")

##### Model Fit #####

# Fit multilevel multinomial logistic regression model.
fit_2023 <- brm(group ~ scale_light + 
                  scale_wind + 
                  scale_q +
                  (1|site/sensor), # nested random effect
                data = data_2023_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Runs in ~3 minutes on laptop.
# Started at 3:37 pm. Finished at 3:40.

# Save model fit.
saveRDS(fit_2023,
        "data_model_outputs/brms_2023_011025.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2023)
# Only 2 divergent transitions, Rhats look good!

plot(fit_2023, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2023, type = "neff")

# Examine relationships for each predictor.
plot(conditional_effects(fit_2023, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2023, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2023, effects = "scale_q",
                         categorical = TRUE))

# Appears cumulative daily light is greatest in
# Cluster 1, high discharge most strongly predicts
# Neither/Cluster 1, and max windspeed is not significant.

##### Visualization #####

# Examine the posterior data.
post_data23 <- mcmc_intervals_data(fit_2023,
                                 point_est = "median", # default = "median"
                                 prob = 0.66, # default = 0.5
                                 prob_outer = 0.95) # default = 0.9

View(post_data23)

(fig_custom23 <- ggplot(post_data23 %>%
                        filter(parameter %in% c("b_muCluster1_scale_light",
                                                "b_muCluster1_scale_wind",
                                                "b_muCluster1_scale_q",
                                                "b_muCluster2_scale_light",
                                                "b_muCluster2_scale_wind",
                                                "b_muCluster2_scale_q")) %>%
                        mutate(par_f = factor(parameter, 
                                              levels = c("b_muCluster1_scale_light",
                                                         "b_muCluster2_scale_light",
                                                         "b_muCluster1_scale_wind",
                                                         "b_muCluster2_scale_wind",
                                                         "b_muCluster1_scale_q",
                                                         "b_muCluster2_scale_q"))), 
                      aes(x = m, y = par_f, color = par_f)) +
    geom_linerange(aes(xmin = ll, xmax = hh),
                   size = 3, alpha = 0.5) +
    geom_point(size = 6) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1)) +
    labs(x = "Posterior Estimates",
         y = "Predictors") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_wind" = "Cluster 1 Wind",
                                "b_muCluster1_scale_q" = "Cluster 1 Q",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_wind" = "Cluster 2 Wind",
                                "b_muCluster2_scale_q" = "Cluster 2 Q")) +
    theme_bw() +
    scale_color_manual(values = c("#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779",
                                  "#0FB2D3", "#026779")) +
    theme(text = element_text(size = 20),
          legend.position = "none"))

# ggsave(fig_custom23,
#        filename = "figures/brms_2023_011025.jpg",
#        height = 15,
#        width = 20,
#        units = "cm")

#### Manuscript Figure ####

# Join the plots above into a single figure.
(fig_custom_both <- (fig_custom + fig_custom23) +
   plot_annotation(tag_levels = 'A'))

# ggsave(fig_custom_both,
#        filename = "figures/brms_bothyrs_011025.jpg",
#        height = 20,
#        width = 40,
#        units = "cm")

#### Supp. Code ####

# Make dataframe with all possible combinations of values.
all_data <- data_2022_multireg %>%
  expand(scale_light, scale_wind, scale_q, site)

# And generate predicted draws.
all_predictions <- fit_2022 %>%
  epred_draws(newdata = all_data)

# Note for next time - the above made too large of a dataset,
# so I'll need to make epred draws similar to how I did for the
# biomass project and take it from there...
# see: https://github.com/hlowman/ContinentalRiverBiomass/blob/main/code/beartooth_spring23/13_Posthoc_analyses.R

#### Sources ####

# Online Resources:

# ~ Frequentist Multinomial Regression ~
# https://www.princeton.edu/~otorres/LogitR101.pdf
# https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
# https://www.carlosivanrodriguez.com/guides/statistics/logistic-regression/multinomial-regression/

# ~ Bayesian Multinomial Regression ~
# https://www.andrewheiss.com/blog/2023/08/12/conjoint-multilevel-multinomial-guide/#tldr-moral-of-the-story

# End of script.
