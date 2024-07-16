# Dynamic Time Warping Stage I Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will fit data from the first stage of
# the project using the clustered dynamic time warping approach.
# Note, future runs of fuzzy clustering should be run on the Pinyon
# server - they take ~1 day to run on personal laptops.

#### Setup ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)

# Load data.
data <- readRDS("data_working/do_data_2022_dailylist_070324.rds")

#### Tidy ####

# Must include ONLY values in the input dataset.
# And applying scale-transform.
data_DO <- lapply(data, function(x) {x$scaled_DO_mg_L})

#### Partitional Fit ####

# Perform clustered DTW on data from June 2021 through
# February 2023 on either side of the lake and all replicates 
# across 3m, 10m, and 20m water depths.
dtw_2_12 <- tsclust(data_DO, 
                 
                 # partitional - ts may only belong to one group
                 # fuzzy - ts may belong partially to multiple groups
                 type="partitional", 
                 
                 # 2L - 2 state hypothesis (biology vs. physics)
                 # 12L - 12 state hypothesis (independent site behavior)
                 k = 2L:12L, # 10 possible
                 
                 # dtw_basic - faster distance measure per:
                 # https://www.rdocumentation.org/packages/dtwclust/versions/5.5.12/topics/tsclust
                 distance="dtw_basic", 
                 
                 # dba - DTA barycenter averaging which "iteratively refines
                 # cluster membership until convergence criteria are met"
                 # per Johnson et al. 2024
                 centroid="dba")
# Only took about a minute or so.

# Export model fit.
# saveRDS(dtw_2_12, "data_modelfits/dtw_2022_2thru12_060624.rds")

# Examine cluster validity indices.
# Now, this takes longer than the model fit (~ 3 minutes).
dtw_results <- lapply(dtw_2_12, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize SIL
# Maximize SF
# Maximize CH
# Minimize DB
# Minimize DBstar
# Maximize D
# Minimize COP

# For more info on CVIs, see
# https://rdrr.io/cran/dtwclust/man/cvi.html

# Examine the most parsimonious clusterings.
plot(dtw_2_12[[1]]) # by most measures (2 clusters)
plot(dtw_2_12[[7]]) # by D measure (8 clusters)
plot(dtw_2_12[[11]]) # by COP measure (12 clusters)

# Hmmmm, this is looking fairly messy, likely due to the
# sheer number of days for which I have data.

# But let's examine the data for the most parsimonious fit.
plot(dtw_2_12[[1]], type = "c") # plots centroids only

# And pull out cluster membership
clusters <- dtw_2_12[[1]]@cluster 

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

ggplot(data_months, aes(x = month)) +
  geom_bar(aes(fill = factor(cluster))) +
  labs(x = "Time of Year",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw() # these results are most stark!
# with cluster 1 occurring almost exclusively in summer

#### Fuzzy Fit ####

# Perform clustered DTW on data from June 2021 through
# February 2023 on either side of the lake and all replicates 
# across 3m, 10m, and 20m water depths.
dtw_fuzzy_2_12 <- tsclust(data_DO, 
                    
                    # partitional - ts may only belong to one group
                    # fuzzy - ts may belong partially to multiple groups
                    type = "fuzzy", 
                    
                    # 2L - 2 state hypothesis (biology vs. physics)
                    # 12L - 12 state hypothesis (independent site behavior)
                    k = 2L:12L, # 10 possible
                    
                    # state that no preprocessing should occur
                    preproc = NULL,
                    
                    # dtw - ???
                    distance="dtw", 
                    
                    # fcmdd - fuzzy c-mediods
                    centroid="fcmdd",
                    
                    # setting window size
                    # should be approx. 10% of series length
                    # 24 * .1 = 2.4 ~ 3
                    args = tsclust_args(dist = list(window.size = 3L)),
                    
                    # controls number of iterations
                    control = fuzzy_control(iter.max = 1000L),
                    
                    # prints progress to screen
                    trace = T)

# Takes about 10 hours on my laptop
# (started 12:03pm, finished at 10:40 am)

# Takes about ?? hours on Pinyon
# Run2 (started 3:06pm, finished at ?:??)

# Export model fit.
# saveRDS(dtw_fuzzy_2_12, "data_modelfits/dtw_2022_fuzzy_062824.rds")
saveRDS(dtw_fuzzy_2_12, "data_model_outputs/dtw_2022_fuzzy_070324.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_results <- lapply(dtw_fuzzy_2_12, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize MPC
# Minimize K
# Minimize T
# Maximize SC
# Maximize PBMF

# Examine the most parsimonious clusterings.
plot(dtw_fuzzy_2_12[[1]]) # Per MPC, K, and T metrics (2 clusters)
plot(dtw_fuzzy_2_12[[1]], type = "c")
plot(dtw_fuzzy_2_12[[7]]) # Per SC and PBMF metrics (8 clusters)

# Export cluster groupings for most parsimonious model fit.
dtw_clusters <- as.data.frame(dtw_fuzzy_2_12[[1]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 2)

# Will need to pull each cluster membership df separately and
# merge together if needed to.

# Add new column to assign groupings with 90% cutoff.
dtw_clusters$group <- case_when(dtw_clusters$cluster_1 >= 0.9 ~ "Cluster 1",
                                dtw_clusters$cluster_2 >= 0.9 ~ "Cluster 2",
                                TRUE ~ "None")

# And plot these results.
# First need to make the dataset into a df.
data_df <- plyr::ldply(data, data.frame)

# And join with the cluster data.
full_df <- left_join(data_df, dtw_clusters) %>%
  # need to add plotting index otherwise hours appear weirdly
  mutate(hour_index = case_when(hour == 4 ~ 1,hour == 5 ~ 2,hour == 6 ~ 3,
                                hour == 7 ~ 4,hour == 8 ~ 5,hour == 9 ~ 6,
                                hour == 10 ~ 7,hour == 11 ~ 8,hour == 12 ~ 9,
                                hour == 13 ~ 10,hour == 14 ~ 11,hour == 15 ~ 12,
                                hour == 16 ~ 13,hour == 17 ~ 14,hour == 18 ~ 15,
                                hour == 19 ~ 16,hour == 20 ~ 17,hour == 21 ~ 18,
                                hour == 22 ~ 19,hour == 23 ~ 20,hour == 0 ~ 21,
                                hour == 1 ~ 22,hour == 2 ~ 23,hour == 3 ~ 24))

ggplot(full_df, aes(x = hour_index, y = scaled_DO_mg_L,
                    color = group, group = uniqueID)) +
  geom_line() +
  theme_bw() +
  facet_wrap(group~.) +
  theme(legend.position = "none")

ggplot(full_df, aes(x = site)) +
  geom_bar(aes(fill = factor(group))) +
  labs(x = "Site",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw()

ggplot(full_df, aes(x = location)) +
  geom_bar(aes(fill = factor(group))) +
  labs(x = "Location",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw()

# Since these appear roughly proportional, let's do a quick
# data check to be sure.
location_counts <- full_df %>%
  group_by(location, group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = group, values_from = count) %>%
  mutate(Total = (`Cluster 1` + `Cluster 2` + `None`))

location_perc <- location_counts %>%
  mutate(Cluster1_perc = `Cluster 1`/Total,
         Cluster2_perc = `Cluster 2`/Total,
         None_perc = None/Total) %>%
  pivot_longer(Cluster1_perc:None_perc, names_to = "group_perc")

ggplot(location_perc, aes(x = location, y = value)) +
  geom_bar(aes(fill = factor(group_perc)), stat = "identity") +
  labs(x = "Location",
       y = "Timeseries count (% of total)",
       fill = "Cluster ID") +
  theme_bw()

# Also creating column with months
# but doing separately bc something is wonky
full_df <- full_df %>%
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
                                   "Oct", "Nov", "Dec")))

ggplot(full_df, aes(x = month)) +
  geom_bar(aes(fill = factor(group))) +
  labs(x = "Time of Year",
       y = "Timeseries count (days)",
       fill = "Cluster ID") +
  theme_bw() # these results are most stark!
# with cluster 2 occurring almost exclusively in summer

# Hmmmm, just out of curiosity, let's look at the actual ts
# split by cluster but colored by month
library(viridis)

ggplot(full_df, aes(x = hour_index, y = scaled_DO_mg_L,
                    color = month, group = uniqueID)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  facet_wrap(group~.)

# And doing the same for the 8 cluster results.
# Export cluster groupings for most parsimonious model fit.
dtw_clusters8 <- as.data.frame(dtw_fuzzy_2_12[[7]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 8)

# Add new column to assign groupings with 50% cutoff.
dtw_clusters8$group <- case_when(dtw_clusters8$cluster_1 >= 0.5 ~ "Cluster 1",
                                dtw_clusters8$cluster_2 >= 0.5 ~ "Cluster 2",
                                dtw_clusters8$cluster_3 >= 0.5 ~ "Cluster 3",
                                dtw_clusters8$cluster_4 >= 0.5 ~ "Cluster 4",
                                dtw_clusters8$cluster_5 >= 0.5 ~ "Cluster 5",
                                dtw_clusters8$cluster_6 >= 0.5 ~ "Cluster 6",
                                dtw_clusters8$cluster_7 >= 0.5 ~ "Cluster 7",
                                dtw_clusters8$cluster_8 >= 0.5 ~ "Cluster 8",
                                TRUE ~ "None")

# And join with the cluster data.
full_df8 <- left_join(data_df, dtw_clusters8) %>%
  # need to add plotting index otherwise hours appear weirdly
  mutate(hour_index = case_when(hour == 4 ~ 1,hour == 5 ~ 2,hour == 6 ~ 3,
                                hour == 7 ~ 4,hour == 8 ~ 5,hour == 9 ~ 6,
                                hour == 10 ~ 7,hour == 11 ~ 8,hour == 12 ~ 9,
                                hour == 13 ~ 10,hour == 14 ~ 11,hour == 15 ~ 12,
                                hour == 16 ~ 13,hour == 17 ~ 14,hour == 18 ~ 15,
                                hour == 19 ~ 16,hour == 20 ~ 17,hour == 21 ~ 18,
                                hour == 22 ~ 19,hour == 23 ~ 20,hour == 0 ~ 21,
                                hour == 1 ~ 22,hour == 2 ~ 23,hour == 3 ~ 24))

# And plot these results.
ggplot(full_df8, aes(x = hour_index, y = scaled_DO_mg_L,
                    color = group, group = uniqueID)) +
  geom_line() +
  theme_bw() +
  facet_wrap(group~.) +
  theme(legend.position = "none")
  
# End of script.
