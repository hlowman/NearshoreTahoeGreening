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

# Fit multilevel multinomial logistic regression model.
# Note, after much googling, it seems no packages support
# the frequentist format as well as the Bayesian ones,
# so defaulting to a Bayesian approach here only for that
# reason, not because we necessarily have priors for
# any of the covariates.
fit_2022 <- brm(group ~ scale_light + scale_wind + scale_q 
                + (1|site/sensor),
                data = data_2022_multireg,
                # specify categorical if vectorized data
                # specify multinomial if data is a matrix
                family = categorical())

# Save model fit.
# saveRDS(fit_2022,
#         "data_model_outputs/multreg_2022_110124.rds")

# Examine model fit in format that adds p-values.
fit_df <- broom::tidy(fit_2022, conf.int = TRUE)

# In order to better translate results, will exponentiate
# coefficients.
fit_df$expB <- exp(fit_df$estimate)

# And exponentiate separately to work with function below.
fit_exp <- exp(coef(fit_2022))

# Examine in another table format. 
stargazer(fit_df, type = "text", coef = list(fit_exp))

# For example, the odds of a day being assigned to Cluster 1
# increases relative to Neither by 132% for each unit increase
# in scale_q. And the odds of a day being assigned to
# Cluster 2 increases relative to Neither by 200% for each
# unit increase in scale_light.
# Another way of putting it is, for each unit increase in
# light values, you are 0.20 times more likely to stay in
# the Cluster 1 category as compared to Neither category
# (so the risk/odds is 80% lower).

# End of script.
