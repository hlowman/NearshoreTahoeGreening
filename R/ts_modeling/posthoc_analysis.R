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
data_2022 <- readRDS("data_working/do_covariate_daily_data_2022_110124.rds")
data_2023 <- readRDS("data_working/do_covariate_daily_data_2023_110124.rds")

#### 2022 Fit ####

##### Data QAQC #####

# First, we would typically check data missingness,
# but since we've created this dataset in the
# script prior, this has already been done.

# Instead, I'll do a quick gut check of correlated
# variables.
covs22 <- ggpairs(data_2022 %>%
          select(min_dosat:delta_q))

# ggsave(covs22,
#        filename = "figures/covariates_2022.jpg",
#        width = 50,
#        height = 50,
#        units = "cm")

# Next, we will select only the columns of interest
# for this analysis, namely:
# - clustering group (dependent variable)
# - change in daily light (or cumulative?)
# - maximum daily windspeed
# - mean daily discharge
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
         delta_light, max_ws, mean_q)

# Examine plots for covariates of interest vs.
# cluster assignments.
boxplot(delta_light ~ group, data = data_2022_select)
boxplot(max_ws ~ group, data = data_2022_select)
boxplot(log(mean_q) ~ group, data = data_2022_select)

# Also examine across sites & sensors.
boxplot(delta_light ~ site, data = data_2022_select)
boxplot(max_ws ~ site, data = data_2022_select)
boxplot(log(mean_q) ~ site, data = data_2022_select)
boxplot(delta_light ~ sensor, data = data_2022_select)
boxplot(max_ws ~ sensor, data = data_2022_select)
boxplot(log(mean_q) ~ sensor, data = data_2022_select)
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
  mutate(scale_light = scale(delta_light),
         scale_wind = scale(max_ws),
         scale_q = scale(log_mean_q))

# Create and export data for model fit.
data_2022_multireg <- data_2022_select %>%
  select(group, site, sensor,
         scale_light, scale_wind, scale_q)

# saveRDS(data_2022_multireg, "data_working/clustering_multireg_110124.rds")

##### Model Fit #####

# Note, after much googling, it seems no packages support
# the frequentist format as well as the Bayesian ones,
# so defaulting to a Bayesian approach here only for that
# reason, not because we necessarily have priors for
# any of the covariates.

# Fit multilevel multinomial logistic regression model.
fit_2022 <- brm(group ~ scale_light + scale_wind + scale_q
                + (1|site/sensor), # nested random effect
                data = data_2022_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Save model fit.
# saveRDS(fit_2022,
#         "data_model_outputs/brms_2022_110424.rds")

##### Diagnostics #####

# Examine model fit.
summary(fit_2022)
# Despite 57 divergent transitions, Rhats look good!

plot(fit_2022, variable = c("b_muCluster1_scale_light",
                            "b_muCluster1_scale_wind",
                            "b_muCluster1_scale_q",
                            "b_muCluster2_scale_light",
                            "b_muCluster2_scale_wind",
                            "b_muCluster2_scale_q"))
# Chain mixing looking good!

# Be sure no n_eff are < 0.1
mcmc_plot(fit_2022, type = "neff")

# Examine relationships for each predictor.
plot(conditional_effects(fit_2022, effects = "scale_light",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_wind",
                         categorical = TRUE))
plot(conditional_effects(fit_2022, effects = "scale_q",
                         categorical = TRUE))

# Appears mean daily q is greatest in Cluster 1,
# daily change in light is greatest in Cluster 2,
# and effect of wind is not as clearly delineated.

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
                   size = 2, alpha = 0.5) +
    geom_point(size = 3) +
    vline_at(v = 0) +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    labs(x = "Posterior Estimates",
         y = "Predictors") +
    scale_y_discrete(labels = c("b_muCluster1_scale_light" = "Cluster 1 Light",
                                "b_muCluster1_scale_wind" = "Cluster 1 Windspeed",
                                "b_muCluster1_scale_q" = "Cluster 1 Discharge",
                                "b_muCluster2_scale_light" = "Cluster 2 Light",
                                "b_muCluster2_scale_wind" = "Cluster 2 Windspeed",
                                "b_muCluster2_scale_q" = "Cluster 2 Discharge")) +
    theme_bw() +
    scale_color_manual(values = c("#FABA39","#1AE4B6", 
                                  "#FABA39","#1AE4B6",
                                  "#FABA39","#1AE4B6")) +
    theme(text = element_text(size = 10),
          legend.position = "none"))

# ggsave(fig_custom,
#        filename = "figures/brms_2022_110424.jpg",
#        height = 8,
#        width = 12,
#        units = "cm")

# STOPPED HERE.

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

# Online Resources:
# ~ Frequentist Multinomial Regression ~
# https://www.princeton.edu/~otorres/LogitR101.pdf
# https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
# https://www.carlosivanrodriguez.com/guides/statistics/logistic-regression/multinomial-regression/
# ~ Bayesian Multinomial Regression ~
# https://www.andrewheiss.com/blog/2023/08/12/conjoint-multilevel-multinomial-guide/#tldr-moral-of-the-story

# End of script.
