# Dynamic Time Warping Stage II Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will fit data from the second stage of
# the project using the clustered dynamic time warping approach.

#### Setup ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)

# Load data.
data <- readRDS("data_working/do_data_2023_dailylist_091024.rds")
data_trim <- readRDS("data_working/do_data_2023_trim_dailylist_082124.rds")

#### Tidy ####

# Must include ONLY values in the input dataset.
# And applying scale-transform.
data_DO <- lapply(data, function(x) {x$scaled_DO_sat})
data_trim_DO <- lapply(data_trim, function(x) {x$scaled_DO_mg_L})

# Or temperature data for the supplementary model fits.
data_Temp <- lapply(data, function(x) {x$scaled_temp})

#### Partitional Fit ####

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

#### Fuzzy Fit ####

##### DO #####

# Perform clustered DTW on data from March through
# September 2023 on either side of the lake and all replicates 
# at ONLY 3m water depth.
dtw_fuzzy_2_12 <- tsclust(data_DO, 
                          
                          # fuzzy - ts may belong partially to multiple groups
                          type = "fuzzy", 
                          
                          # 2L - 2 state hypothesis (biology vs. physics)
                          # 12L - 12 state hypothesis (independent sites)
                          k = 2L:12L, # 10 possible
                          
                          # state that no preprocessing should occur
                          # since we already scaled values
                          preproc = NULL,
                          
                          # dtw - ???
                          distance="dtw", 
                          
                          # fcmdd - fuzzy c-mediods
                          centroid="fcmdd",
                          
                          # setting window size
                          # should be approx. 10% of series length
                          # 24 * .1 = 2.4 ~ 3
                          args = tsclust_args(dist = list(window.size = 1L)),
                          
                          # controls number of iterations
                          control = fuzzy_control(iter.max = 1000L),
                          
                          # prints progress to screen
                          trace = T)

# Takes about 15 minutes on Pinyon.
# Started at 7:26PM. Ran until 7:42PM.

# Export model fit.
saveRDS(dtw_fuzzy_2_12, 
        "data_model_outputs/dtw_2023_fuzzy_090624.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_results <- lapply(dtw_fuzzy_2_12, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize MPC - 3
# Minimize K - 4
# Minimize T - 4
# Maximize SC - 4
# Maximize PBMF - 11

# Examine the most parsimonious clusterings.
plot(dtw_fuzzy_2_12[[2]]) # Per MPC metric (3 clusters)
plot(dtw_fuzzy_2_12[[3]]) # Per K, T, and SC metrics (4 clusters)
plot(dtw_fuzzy_2_12[[10]]) # Per PBMF metric (11 clusters)

# Export cluster groupings for most parsimonious model fit.
dtw_clusters <- as.data.frame(dtw_fuzzy_2_12[[3]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 4)

# Will need to pull each cluster membership df separately and
# merge together if needed to.

# Add new column to assign groupings with 90% cutoff.
dtw_clusters$group <- case_when(dtw_clusters$cluster_1 >= 0.7 ~ "Cluster 1",
                                dtw_clusters$cluster_2 >= 0.7 ~ "Cluster 2",
                                dtw_clusters$cluster_3 >= 0.7 ~ "Cluster 3",
                                dtw_clusters$cluster_4 >= 0.7 ~ "Cluster 4",
                                TRUE ~ "Neither")

# And plot these results.
# First need to make the dataset into a df.
data_df <- plyr::ldply(data, data.frame)

# And join with the cluster data.
full_df <- left_join(data_df, dtw_clusters,
                     by = c(".id" = "uniqueID")) %>%
  # need to add plotting index otherwise hours appear weirdly
  mutate(hour_index = case_when(hour == 4 ~ 1,hour == 5 ~ 2,hour == 6 ~ 3,
                                hour == 7 ~ 4,hour == 8 ~ 5,hour == 9 ~ 6,
                                hour == 10 ~ 7,hour == 11 ~ 8,hour == 12 ~ 9,
                                hour == 13 ~ 10,hour == 14 ~ 11,hour == 15 ~ 12,
                                hour == 16 ~ 13,hour == 17 ~ 14,hour == 18 ~ 15,
                                hour == 19 ~ 16,hour == 20 ~ 17,hour == 21 ~ 18,
                                hour == 22 ~ 19,hour == 23 ~ 20,hour == 0 ~ 21,
                                hour == 1 ~ 22,hour == 2 ~ 23,hour == 3 ~ 24))

(fig_curves <- ggplot(full_df, aes(x = hour_index, 
                                   y = DO_sat,
                                   color = group, group = `.id`)) +
    geom_line() +
    scale_color_manual(values = c("#FABA39FF", "#1AE4B6FF",
                                  "#4662D7FF", "#D3105C",
                                  "#E4B3E2")) + 
    labs(x = "Hour of Day (+4)", y = "Dissolved Oxygen (% Saturation)") +
    theme_bw() +
    facet_wrap(group~.) +
    theme(legend.position = "none"))

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
                                  TRUE ~ NA),
                        levels = c("Jan", "Feb", "Mar",
                                   "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep",
                                   "Oct", "Nov", "Dec")))

# Now, before plotting counts, I have to remember to
# collapse this down to days (since all hours are currently
# represented and inflating counts).
full_df_daily <- full_df %>%
  # first need to trim down the dataset so it doesn't
  # duplicate days (4am one -> 4am the next)
  group_by(`.id`) %>%
  slice_head() %>%
  ungroup() %>%
  select(`.id`, date, month, site, location, replicate, group) %>%
  unique()

counts_daily <- full_df_daily %>%
  count(group) %>%
  ungroup()

(fig_months <- ggplot(full_df_daily %>%
                        mutate(site_f = factor(site,
                                levels = c("BW", "SS", 
                                           "GB", "SH"))), 
                      aes(x = month)) +
    geom_bar(aes(fill = factor(group))) +
    scale_fill_manual(values = c("#FABA39","#1AE4B6",
                                 "#4662D7", "#D3105C",
                                 "#E4B3E2")) +
    labs(x = "Month of Year",
         y = "Timeseries count (days)",
         fill = "Cluster ID") +
    theme_bw() +
    facet_grid(site_f~., scales = "free"))

# Export figure.
(fig_all <- fig_curves | fig_months)

# ggsave(plot = fig_all,
#        filename = "figures/dtw_2023_090624.png",
#        width = 40,
#        height = 10,
#        units = "cm")

###### Posthoc analyses #####

# I also need to generate some descriptive statistics about
# each of these groups.
full_df_daily_summary <- full_df %>%
  group_by(`.id`, site, 
           location, replicate, group) %>%
  summarize(mean_DOsat = mean(DO_sat, na.rm = TRUE),
            max_DOsat = max(DO_sat, na.rm = TRUE),
            min_DOsat = min(DO_sat, na.rm = TRUE),
            mean_Temp = mean(Temp_C, na.rm = TRUE),
            max_Temp = max(Temp_C, na.rm = TRUE),
            min_Temp = min(Temp_C, na.rm = TRUE),) %>%
  ungroup() %>%
  mutate(range_DOsat = max_DOsat - min_DOsat,
         range_Temp = max_Temp - min_Temp)

full_df_group_summary <- full_df_daily_summary %>%
  group_by(group) %>%
  summarize(median_mean_DOsat = median(mean_DOsat),
            mean_range_DOsat = mean(range_DOsat),
            median_mean_Temp = median(mean_Temp),
            mean_range_Temp = mean(range_Temp)) %>%
  ungroup()

##### Trim DO DF #####

# Below, we'll re-fit the fuzzy clustering approach but to a 
# dataset containing 45% less days, because we've trimmed it
# down to include only days on which ALL sensors were functioning
# at all locations.

dtw_fuzzy_trim_2_12 <- tsclust(data_trim_DO, 
                               
                               # fuzzy - ts may belong partially to 
                               # multiple groups (output a proportion)
                               type = "fuzzy", 
                               
                               # 2L - 2 state hypothesis 
                               # (biology vs. physics)
                               # 12L - 12 state hypothesis 
                               # (sites independent)
                               k = 2L:12L, # 10 possible
                               
                               # state that no pre-processing should occur
                               preproc = NULL,
                               
                               # dtw - ???
                               distance = "dtw", 
                               
                               # fcm - fuzzy c-means; 
                               # fcmdd - fuzzy c-mediods
                               centroid = "fcmdd",
                               
                               # setting window size
                               # should be approx. 10% of series length
                               # 24 * .1 = 2.4 ~ 3
                               # window.size refers to distance between
                               # a value and one of the edges
                               args = tsclust_args(dist = 
                                                     list(window.size = 1L)),
                               
                               # controls number of iterations
                               control = fuzzy_control(iter.max = 1000L),
                               
                               # prints progress to screen
                               trace = T)

# Takes about 5 minutes on Pinyon
# Run started 1:58pm, finished at 2:01pm

# Export model fit.
saveRDS(dtw_fuzzy_trim_2_12, 
        "data_model_outputs/dtw_2023_fuzzy_trim_082124.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_results_trim <- lapply(dtw_fuzzy_trim_2_12, cvi)

dtw_results_trim_df <- as.data.frame(dtw_results_trim,
                                     col.names = c("L2", "L3", "L4",
                                                   "L5", "L6", "L7",
                                                   "L8", "L9", "L10",
                                                   "L11", "L12"))

dtw_results_trim_df <- t(dtw_results_trim_df)

# Want to:
# Maximize MPC - 2 clusters
# Minimize K - 2 clusters
# Minimize T - 2 clusters
# Maximize SC - 6 clusters
# Maximize PBMF - 11 clusters

# Export cluster groupings for most parsimonious model fit.
dtw_trim_clusters <- as.data.frame(dtw_fuzzy_trim_2_12[[1]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 2)

# Add new column to assign groupings with 90% cutoff.
dtw_trim_clusters$group <- case_when(dtw_trim_clusters$cluster_1 >= 0.9 ~
                                       "Cluster 1",
                                     dtw_trim_clusters$cluster_2 >= 0.9 ~
                                       "Cluster 2",
                                     TRUE ~ "Neither")

# And plot these results.
data_trim_df <- plyr::ldply(data_trim, data.frame)

# And join with the cluster data.
full_trim_df <- left_join(data_trim_df, dtw_trim_clusters,
                          by = c(".id" = "uniqueID")) %>%
  # need to add plotting index otherwise hours appear weirdly
  mutate(hour_index = case_when(hour == 4 ~ 1,hour == 5 ~ 2,
                                hour == 6 ~ 3,
                                hour == 7 ~ 4,hour == 8 ~ 5,
                                hour == 9 ~ 6,
                                hour == 10 ~ 7,hour == 11 ~ 8,
                                hour == 12 ~ 9,
                                hour == 13 ~ 10,hour == 14 ~ 11,
                                hour == 15 ~ 12,
                                hour == 16 ~ 13,hour == 17 ~ 14,
                                hour == 18 ~ 15,
                                hour == 19 ~ 16,hour == 20 ~ 17,
                                hour == 21 ~ 18,
                                hour == 22 ~ 19,hour == 23 ~ 20,
                                hour == 0 ~ 21,
                                hour == 1 ~ 22,hour == 2 ~ 23,
                                hour == 3 ~ 24))

(fig_trim_curves <- ggplot(full_trim_df, 
                           aes(x = hour_index, y = DO_mg_L,
                               color = group, group = `.id`)) +
    geom_line() +
    scale_color_manual(values = c("#FABA39FF", "#1AE4B6FF",
                                  "#4662D7FF")) + 
    labs(x = "Hour of Day (+4)", 
         y = "Dissolved Oxygen (mg/L)") +
    theme_bw() +
    facet_wrap(group~.) +
    theme(legend.position = "none"))

# Also creating column with months
# but doing separately bc something is wonky
full_trim_df <- full_trim_df %>%
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
                        # and ordering them as they were collected
                        levels = c("Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep",
                                   "Oct", "Nov", "Dec",
                                   "Jan", "Feb", "Mar")))

# Now, before plotting counts, I have to remember to
# collapse this down to days (since all hours are currently
# represented and inflating counts).
full_trim_df_daily <- full_trim_df %>%
  # first need to trim down the dataset so it doesn't
  # duplicate days (4am one -> 4am the next)
  group_by(`.id`) %>%
  slice_head() %>%
  ungroup() %>%
  select(`.id`, date, month, site, location, replicate, group) %>%
  unique()

(fig_trim_months <- ggplot(full_trim_df_daily, 
                           aes(x = month)) +
    geom_bar(aes(fill = factor(group))) +
    scale_fill_manual(values = c("#FABA39","#1AE4B6",
                                 "#4662D7")) +
    labs(x = "Month of Year",
         y = "Timeseries count (days)",
         fill = "Cluster ID") +
    theme_bw() +
    facet_grid(.~site, scales = "free"))

# Export figure.
(fig_trim_all <- (fig_trim_curves | fig_trim_months))

# ggsave(plot = fig_trim_all,
#        filename = "figures/dtw_2023_trim_082124.png",
#        width = 40,
#        height = 10,
#        units = "cm")

##### Temp #####

# Perform clustered DTW on data from March through
# September 2023 on either side of the lake and all replicates 
# at ONLY 3m water depth.
dtw_fuzzy_2_12T <- tsclust(data_Temp, 
                          
                          # fuzzy - ts may belong partially to multiple groups
                          type = "fuzzy", 
                          
                          # 2L - biology vs. physics
                          # 12L - independent sites
                          k = 2L:12L, # 10 possible
                          
                          # no preprocessing should occur
                          # since we already scaled values
                          preproc = NULL,
                          
                          # dtw - ???
                          distance="dtw", 
                          
                          # fcmdd - fuzzy c-mediods
                          centroid="fcmdd",
                          
                          # setting window size
                          # should be approx. 10% of series
                          # 24 * .1 = 2.4 ~ 3
                          args = tsclust_args(dist = list(window.size = 1L)),
                          
                          # controls number of iterations
                          control = fuzzy_control(iter.max = 1000L),
                          
                          # prints progress to screen
                          trace = T)

# Takes about 15 minutes on Pinyon.
# Started at 2:43PM. Ran until 3:09PM.

# Export model fit.
saveRDS(dtw_fuzzy_2_12T, 
        "data_model_outputs/dtw_2023_fuzzy_temp_091024.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_results <- lapply(dtw_fuzzy_2_12T, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize MPC - 3
# Minimize K - 2
# Minimize T - 2
# Maximize SC - 6
# Maximize PBMF - 4

# Examine the most parsimonious clusterings.
plot(dtw_fuzzy_2_12T[[1]]) # Per K & T metrics (2 clusters)
# although this one isn't as much of a majority as the
# others have been

# Export cluster groupings for most parsimonious model fit.
dtw_clusters <- as.data.frame(dtw_fuzzy_2_12T[[1]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 2)

# Will need to pull each cluster membership df separately and
# merge together if needed to.

# Add new column to assign groupings with 90% cutoff.
dtw_clusters$group <- case_when(dtw_clusters$cluster_1 >= 0.9 ~ "Cluster 1",
                                dtw_clusters$cluster_2 >= 0.9 ~ "Cluster 2",
                                TRUE ~ "Neither")

# And plot these results.
# First need to make the dataset into a df.
data_df <- plyr::ldply(data, data.frame)

# And join with the cluster data.
full_df <- left_join(data_df, dtw_clusters,
                     by = c(".id" = "uniqueID")) %>%
  # need to add plotting index otherwise hours appear weirdly
  mutate(hour_index = case_when(hour == 4 ~ 1,hour == 5 ~ 2,hour == 6 ~ 3,
                                hour == 7 ~ 4,hour == 8 ~ 5,hour == 9 ~ 6,
                                hour == 10 ~ 7,hour == 11 ~ 8,hour == 12 ~ 9,
                                hour == 13 ~ 10,hour == 14 ~ 11,hour == 15 ~ 12,
                                hour == 16 ~ 13,hour == 17 ~ 14,hour == 18 ~ 15,
                                hour == 19 ~ 16,hour == 20 ~ 17,hour == 21 ~ 18,
                                hour == 22 ~ 19,hour == 23 ~ 20,hour == 0 ~ 21,
                                hour == 1 ~ 22,hour == 2 ~ 23,hour == 3 ~ 24))

(fig_curvesT <- ggplot(full_df, aes(x = hour_index, 
                                   y = Temp_C,
                                   color = group, group = `.id`)) +
    geom_line() +
    scale_color_manual(values = c("#FABA39FF", "#1AE4B6FF",
                                  "#4662D7FF")) + 
    labs(x = "Hour of Day (+4)", 
         y = "Temperature (Degrees C)") +
    theme_bw() +
    facet_wrap(group~.) +
    theme(legend.position = "none"))

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
                                  TRUE ~ NA),
                        levels = c("Jan", "Feb", "Mar",
                                   "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep",
                                   "Oct", "Nov", "Dec")))

# Now, before plotting counts, I have to remember to
# collapse this down to days (since all hours are currently
# represented and inflating counts).
full_df_daily <- full_df %>%
  # first need to trim down the dataset so it doesn't
  # duplicate days (4am one -> 4am the next)
  group_by(`.id`) %>%
  slice_head() %>%
  ungroup() %>%
  select(`.id`, date, month, site, location, replicate, group) %>%
  unique()

counts_daily <- full_df_daily %>%
  count(group) %>%
  ungroup()

(fig_monthsT <- ggplot(full_df_daily %>%
                        mutate(site_f = factor(site,
                                               levels = c("BW", "SS", 
                                                          "GB", "SH"))), 
                      aes(x = month)) +
    geom_bar(aes(fill = factor(group))) +
    scale_fill_manual(values = c("#FABA39","#1AE4B6",
                                 "#4662D7")) +
    labs(x = "Month of Year",
         y = "Timeseries count (days)",
         fill = "Cluster ID") +
    theme_bw() +
    facet_grid(site_f~., scales = "free"))

# Export figure.
(fig_allT <- fig_curvesT | fig_monthsT)

# ggsave(plot = fig_allT,
#        filename = "figures/dtw_2023_temp_091024.png",
#        width = 40,
#        height = 10,
#        units = "cm")

# End of script.
