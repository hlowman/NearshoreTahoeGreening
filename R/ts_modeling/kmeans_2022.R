# K Means Clustering Analysis Script
# Authors: Heili E. Lowman
# Date Created: 2024-10-11

# ---------------------------- README ---------------------------------
# The following script will fit data from Stage I of
# the project using the k-means clustering approach.

# More information about this approach is available at:
# https://www.tidymodels.org/learn/statistics/k-means/

#### Setup ####

# Load packages.
library(lubridate)
library(tidyverse)
library(tidymodels)
library(data.table)
library(here)
library(stats)
library(patchwork)

# Load data.
dat_2022 <- readRDS("data_working/do_covariate_daily_data_2022_101124.rds")

#### Prep ####

# Prep data to be used - select only necessary covariates.
dat_2022_select <- dat_2022 %>%
  column_to_rownames(var = "ID_index") %>%
  select(delta_dosat, # response variable
         mean_light, # light = photosynthesis
         max_ws, # wind = mixing
         mean_q) %>% # discharge = inflow
  scale()

#### Fit ####

# Offering options from 2 through 12 clusters as we
# did for the dynamic time warping approach.
kclusts <- 
  tibble(k = 2:12) %>%
  mutate(
    kclust = map(k, ~kmeans(dat_2022_select, .x)),
    # summaries per cluster
    tidied = map(kclust, tidy),
    # single row summary
    glanced = map(kclust, glance),
    # adds clusters to original data
    augmented = map(kclust, augment, dat_2022_select))

# Evaluate model fit
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Examine number of clusters that best maps
# to given data.
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() # 4 looks to be the "elbow"

#### Results ####

k4_fit <- assignments %>%
  filter(k == 4) %>%
  select(`.rownames`,`.cluster`) %>%
  rename(ID_index = `.rownames`,
         Cluster = `.cluster`)

dat_2022 <- full_join(dat_2022, k4_fit)

fig_kmeans22 <- ggplot(dat_2022 %>%
         select(ID_index, Cluster,
                delta_dosat, mean_light, 
                max_ws, mean_q) %>%
         pivot_longer(mean_light:mean_q,
                      names_to = "var",
                      values_to = "value") %>%
         mutate(var = factor(var,
                             levels = c("mean_light",
                                        "max_ws",
                                        "mean_q"))),
       aes(x = value, 
           y = delta_dosat,
           color = var)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("#FFAA00",
                                "#6B6D9F",
                                "#69B9FA")) +
  theme_bw() +
  facet_grid(Cluster~var, scales = "free") +
  theme(legend.position = "none")

# ggsave(plot = fig_kmeans22,
#        filename = "figures/kmeans_2022_101124.png",
#        width = 20,
#        height = 20,
#        units = "cm")

# End of script.