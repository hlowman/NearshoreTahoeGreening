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
library(patchwork)
library(hms)

# Load data.
data <- readRDS("data_working/do_data_2023_dailylist_050225.rds")

# data_trim <- readRDS("data_working/do_data_2023_trim_dailylist_082124.rds")

#### Tidy ####

# Must include ONLY values in the input dataset.
# And using scale-transform.
data_DO <- lapply(data, function(x) {x$scaled_DO_mgL})

# data_trim_DO <- lapply(data_trim, function(x) {x$scaled_DO_mg_L})

# Or temperature data for the supplementary model fits.
# data_Temp <- lapply(data, function(x) {x$scaled_temp})

#### Fuzzy Fit ####

##### DO #####

# Perform clustered DTW on data from June through
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
                          
                          # dtw - default
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

# Takes about 4 minutes on desktop.
# Started at 10:51AM. Ran until 12:50PM.

# Export model fit.
saveRDS(dtw_fuzzy_2_12, 
        "data_model_outputs/dtw_2023_fuzzy_050325.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_results <- lapply(dtw_fuzzy_2_12, cvi)

dtw_results_df <- as.data.frame(dtw_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_results_df <- t(dtw_results_df)

# Want to:
# Maximize MPC - 2
# Minimize K - 2
# Minimize T - 2
# Maximize SC - 5
# Maximize PBMF - 12

# Examine the most parsimonious clusterings.
plot(dtw_fuzzy_2_12[[1]]) # Per MPC, K, and T metrics (2 clusters)
plot(dtw_fuzzy_2_12[[4]]) # Per SC metric (5 clusters)
plot(dtw_fuzzy_2_12[[11]]) # Per PBMF metric (12 clusters)

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
                                TRUE ~ "Neither")

# Export for use in posthoc analyses.
saveRDS(dtw_clusters,
        "data_working/dtw_clusters_2023_050325.rds")

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
    scale_color_manual(values = c("#FABA39FF", 
                                  "#1AE4B6FF", 
                                  "gray30")) + 
    # adding annotation to better delineate time of day
    annotate('rect', xmin = 0, xmax = 3,
             ymin = 60, ymax = 125,
             alpha = 0.2, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 24,
             ymin = 60, ymax = 125,
             alpha = 0.2, fill = "black") + #sunset
    labs(x = "Hour of Day (+4)", y = "Dissolved Oxygen (% Saturation)") +
    theme_bw() +
    facet_wrap(group~.) +
    theme(legend.position = "none"))

# Also making some summary statistics to more clearly plot
# the clusters.
summary_df <- full_df %>%
  # across the daily time series in each cluster...
  group_by(group, hour_index) %>%
  # calculate the median and interquartile ranges
  summarize(medianDO_mgL = median(DO_mgL),
            q25DO_mgL = quantile(DO_mgL, probs = 0.25),
            q75DO_mgL = quantile(DO_mgL, probs = 0.75),
            medianDOscale_mgL = median(scaled_DO_mgL),
            q25DOscale_mgL = quantile(scaled_DO_mgL, 
                                      probs = 0.25),
            q75DOscale_mgL = quantile(scaled_DO_mgL, 
                                      probs = 0.75)) %>%
  ungroup()

(fig2_curves <- ggplot(summary_df, 
                       aes(x = hour_index, 
                           y = medianDO_mgL,
                           ymin = q25DO_mgL, 
                           ymax = q75DO_mgL,
                           color = group, 
                           fill = group)) +
    # adding annotation to better delineate time of day
    annotate('rect', xmin = 0, xmax = 3,
             ymin = 7, ymax = 9.5,
             alpha = 0.15, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 24,
             ymin = 7, ymax = 9.5,
             alpha = 0.15, fill = "black") + #sunset
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.5,
                linewidth = 0.1) + 
    scale_color_manual(values = c("#0FB2D3", 
                                  "#026779", 
                                  "gray70")) +
    scale_fill_manual(values = c("#0FB2D3", 
                                 "#026779", 
                                 "gray70")) +
    labs(x = "Hour of Day", 
         y = "Dissolved Oxygen (mg/L)") +
    scale_x_continuous(breaks = c(0,5,10,15,20),
                       labels = c(4,9,14,19,24)) +
    theme_bw() +
    facet_wrap(group~.) +
    theme(legend.position = "none",
          text = element_text(size = 18)))

# Calculate mean solar noon across dataset to add a solar
# maxima line to the plot below.
mean(full_df$solar_noon) # 47019.9 secs or 13:03:04

(fig2_scaled <- ggplot(summary_df %>%
                         filter(group %in% c("Cluster 1",
                                             "Cluster 2")), 
                       aes(x = hour_index, 
                           y = medianDOscale_mgL,
                           ymin = q25DOscale_mgL, 
                           ymax = q75DOscale_mgL,
                           color = group, 
                           fill = group)) +
    # adding annotation to better delineate time of day
    annotate('rect', xmin = 0, xmax = 3,
             ymin = -1.5, ymax = 1.5,
             alpha = 0.15, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 24,
             ymin = -1.5, ymax = 1.5,
             alpha = 0.15, fill = "black") + #sunset
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.5,
                linewidth = 0.1) + 
    # adding annotation to more clearly delineate DO maxima
    geom_vline(xintercept = 9.06108, color = "#FFAA00",
               linetype = "dashed", linewidth = 2) +
    geom_point(x = 11.02535, y = -0.469708558,
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") +
    geom_point(x = 10.75814, y = 1.261503122,
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") +
    scale_color_manual(values = c("#0FB2D3", 
                                  "#026779")) +
    scale_fill_manual(values = c("#0FB2D3", 
                                 "#026779")) +
    labs(x = "Hour of Day", 
         y = "Dissolved Oxygen (scaled)") +
    scale_x_continuous(breaks = c(0,5,10,15,20),
                       labels = c(4,9,14,19,24)) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 18)))

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
  select(`.id`, date, month, site, 
         location, replicate, group) %>%
  unique()

counts_daily <- full_df_daily %>%
  count(group) %>%
  ungroup()

# New facet label names
site.labs <- c("W Near Stream", 
               "W Far from Stream",
               "E Near Stream",
               "E Far from Stream")
names(site.labs) <- c("BW", "SS", "GB", "SH")

(fig_months <- ggplot(full_df_daily %>%
                        mutate(site_f = factor(site,
                                levels = c("BW", "GB", 
                                           "SS", "SH"))), 
                      aes(x = month)) +
    geom_bar(aes(fill = factor(group))) +
    scale_fill_manual(values = c("#0FB2D3", 
                                 "#026779",
                                 "gray80")) +
    labs(x = "Month of Year",
         y = "Timeseries count (days)",
         fill = "Cluster ID") +
    theme_bw() +
    facet_wrap(.~site_f, scales = "free_y",
               labeller = labeller(site_f = site.labs)) +
    theme(text = element_text(size = 18)))

# Export figure.
(fig_all <- ((fig2_curves / fig2_scaled) | fig_months) +
    plot_annotation(tag_levels = 'A'))

# ggsave(plot = fig_all,
#        filename = "figures/dtw_2023_050325.png",
#        width = 30,
#        height = 20,
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
            mean_DOmgL = mean(DO_mgL, na.rm = TRUE),
            max_DOmgL = max(DO_mgL, na.rm = TRUE),
            min_DOmgL = min(DO_mgL, na.rm = TRUE),
            mean_Temp = mean(Temp_C, na.rm = TRUE),
            max_Temp = max(Temp_C, na.rm = TRUE),
            min_Temp = min(Temp_C, na.rm = TRUE),) %>%
  ungroup() %>%
  mutate(range_DOsat = max_DOsat - min_DOsat,
         range_DOmgL = max_DOmgL - min_DOmgL,
         range_Temp = max_Temp - min_Temp)

full_df_group_summary <- full_df_daily_summary %>%
  group_by(group) %>%
  summarize(median_mean_DOsat = median(mean_DOsat),
            mean_range_DOsat = mean(range_DOsat),
            median_mean_DOmgL = median(mean_DOmgL),
            mean_range_DOmgL = mean(range_DOmgL),
            median_mean_Temp = median(mean_Temp),
            mean_range_Temp = mean(range_Temp)) %>%
  ungroup()

# Need to calculate the difference between max DO sat
# and solar noon.
full_df_max_DO_difftimes <- full_df %>%
  group_by(`.id`, site, 
           location, replicate, group) %>%
  slice_max(DO_sat) %>%
  ungroup() %>%
  mutate(DOmax_hour = hour(date_times),
         DOmax_minutes = minute(date_times),
         DOmax_seconds = second(date_times)) %>%
  mutate(DOmax_time = hms::as_hms(paste(DOmax_hour,
                                   DOmax_minutes,
                                   DOmax_seconds,
                                   sep = ":"))) %>%
  mutate(max_offset = solar_noon - DOmax_time)

offset_group_summary <- full_df_max_DO_difftimes %>%
  group_by(group) %>%
  summarize(median_offset_min = median(as.numeric(max_offset))/60,
            mean_offset_min = mean(as.numeric(max_offset))/60,
            minimum_offset_min = min(as.numeric(max_offset))/60,
            maximum_offset_min = max(as.numeric(max_offset))/60,
            stddev_offset_min = sd(as.numeric(max_offset))/60) %>%
  ungroup()

# Also calculating mean hour of maximum DO
max_summary <- full_df_max_DO_difftimes %>%
  group_by(group) %>%
  summarize(mean_time_of_DOmax = mean(DOmax_time)) %>%
  ungroup() %>%
  mutate(mean_minute_of_DOmax = as.numeric(mean_time_of_DOmax)/60) %>%
  mutate(mean_hour_of_DOmax = mean_minute_of_DOmax/60)

# Difference between mean peak times of either cluster
15.02535-14.75814 # 0.26721

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
