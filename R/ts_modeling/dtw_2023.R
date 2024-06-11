# Dynamic Time Warping Stage II Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will fit data from the second stage of
# the project using the clustered dynamic time warping approach.

#### SETUP ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)

# Load data.
data <- readRDS("data_working/do_data_2023_dailylist_053124.rds")

#### TIDY ####

# Must include ONLY values in the input dataset.
# And applying scale-transform.
data_DO <- lapply(data, function(x) {x$scaled_DO_mg_L})

#### MODEL FIT ####

# Perform clustered DTW on data from February through
# September 2023 on either side of the lake and all replicates 
# across 3m, 10m, and 15m water depths, both near and far from
# mouths of streams.
dtw_2_15 <- tsclust(data_DO, 
                    
                    # partitional - ts may only belong to one group
                    # fuzzy - ts may belong partially to multiple groups
                    type="partitional", 
                    
                    # 2L - 2 state hypothesis (biology vs. physics)
                    # 15L - 15 state hypothesis (independent site behavior)
                    k = 2L:15L, # 13 possible
                    
                    # dtw_basic - faster distance measure per:
                    # https://www.rdocumentation.org/packages/dtwclust/versions/5.5.12/topics/tsclust
                    distance="dtw_basic", 
                    
                    # dba - DTA barycenter averaging which "iteratively refines
                    # cluster membership until convergence criteria are met"
                    # per Johnson et al. 2024
                    centroid="dba")
# Only took about a minute or so.

# Export model fit.
# saveRDS(dtw_2_15, "data_modelfits/dtw_2023_2thru15_061124.rds")

# Examine cluster validity indices.
# Now, this takes longer than the model fit (~ 3 minutes).
dtw_results <- lapply(dtw_2_15, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12", "L13",
                                              "L14", "L15"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize SIL
# Maximize SF
# Maximize CH
# Minimize DB
# Minimize DBstar
# Maximize D
# Minimize COP

# Examine most parsimonious output.
plot(dtw_2_15[[1]]) +
  ylim(c(-5, 5))# by most measures
plot(dtw_2_15[[11]]) +
  ylim(c(-5, 5))# by D measure
plot(dtw_2_15[[14]]) +
  ylim(c(-5, 5))# by COP measure

# Hmmmm, this is again looking messy.
# But let's examine the data for the most parsimonious fit.
plot(dtw_2_15[[1]], type = "c") +
  ylim(c(-5,5))# plots centroids only

plot(dtw_2_15[[11]], type = "c") +
  ylim(c(-5,5))# plots centroids only

plot(dtw_2_15[[14]], type = "c") +
  ylim(c(-5,5))# plots centroids only

# And pull out cluster membership
clusters <- dtw_2_15[[1]]@cluster 

# Append to the original data
data_df <- plyr::ldply(data, data.frame) 

data_sites <- unique(data_df$`.id`)

data_sites_clusters <- as.data.frame(cbind(data_sites, clusters))

data_df <- left_join(data_df, data_sites_clusters,
                     by = c(`.id` = "data_sites"))

data_clusters <- data_df %>%
  group_by(site, location, replicate, uniqueID) %>%
  summarize(cluster = mean(as.numeric(clusters))) %>%
  ungroup()

# Also creating an additional dataset with months
# but doing separately bc something is wonky
data_months <- data_df %>%
  mutate(month = factor(case_when(month(date) == 1 ~ "Jan",
                                  month(date) == 2 ~ "Feb",
                                  month(date) == 3 ~ "Mar",
                                  month(date) == 4 ~ "Apr",
                                  month(date) == 5 ~ "May",
                                  month(date) == 6 ~ "Jun",
                                  month(date) == 7 ~ "Jul",
                                  month(date) == 8 ~ "Aug",
                                  month(date) == 9 ~ "Sep",
                                  month(date) == 10 ~ "Oct",
                                  month(date) == 11 ~ "Nov",
                                  month(date) == 12 ~ "Dec",
                                  month(date) == 1 ~ "Jan",
                                  TRUE ~ NA),
                        levels = c("Jan", "Feb", "Mar",
                                   "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep",
                                   "Oct", "Nov", "Dec"))) %>%
  group_by(site, location, replicate, month, uniqueID) %>%
  summarize(cluster = mean(as.numeric(clusters))) %>%
  ungroup()

# And plot results to see what belongs to which cluster.
ggplot(data_clusters, aes(x = site)) +
  geom_bar(aes(fill = factor(cluster))) +
  labs(x = "Site",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw()

ggplot(data_clusters, aes(x = location)) +
  geom_bar(aes(fill = factor(cluster))) +
  labs(x = "Location",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw()

# since these are roughly equivalent, let's facet by site
ggplot(data_clusters, aes(x = location)) +
  geom_bar(aes(fill = factor(cluster))) +
  labs(x = "Location",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  facet_wrap(.~site) +
  theme_bw()

ggplot(data_months, aes(x = month)) +
  geom_bar(aes(fill = factor(cluster))) +
  labs(x = "Time of Year",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw() # these results are most stark!
# cluster 1 again occurring almost exclusively in summer

# End of script.
