# Dynamic Time Warping Stage I Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will fit data from the first stage of
# the project using the clustered dynamic time warping approach.

#### Setup ####

# Load packages.
library(lubridate)
library(hms)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)
library(viridis)
library(patchwork)

# Load data.
data <- readRDS("data_working/do_data_2022_dailylist_052325.rds")

#### Tidy ####

# Must include ONLY DO values in the input dataset.
# And using scale-transformed ones.
data_DO <- lapply(data, function(x) {x$scaled_DO_mgL})
data_DOsat <- lapply(data, function(x) {x$scaled_DO_sat})

#### Fuzzy Fit ####

##### DO mg/L #####

# Perform clustered DTW on data from April 2022 through
# February 2023 on either side of the lake and all replicates 
# across 3m, 10m, and 20m water depths.
dtw_fuzzy_2_12 <- tsclust(data_DO, 
                    
                    # fuzzy - ts may belong partially to 
                    # multiple groups (output a proportion)
                    type = "fuzzy", 
                    
                    # 2L - 2 state hypothesis (biology vs. physics)
                    # 12L - 12 state hypothesis (sites independent)
                    k = 2L:12L, # 10 possible
                    
                    # state that no pre-processing should occur
                    preproc = NULL,
                    
                    # dtw - ???
                    distance = "dtw", 
                    
                    # fcm - fuzzy c-means; fcmdd - fuzzy c-mediods
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

# Takes about 10 hours on my laptop
# (started 12:03pm, finished at 10:40 am)

# Takes about 2 hours on UNR Desktop
# (started 10:00am, finished at 11:48 am)

# Takes about 1 hour on Pinyon
# Run2 (started 4:49pm, finished at 5:53pm)

# Export model fit.
saveRDS(dtw_fuzzy_2_12, 
        "data_model_outputs/dtw_2022_fuzzy_050225.rds")

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
plot(dtw_fuzzy_2_12[[8]]) # Per SC&PBMF metrics (9 clusters)

# Export cluster groupings for most parsimonious model fit.
dtw_clusters <- as.data.frame(dtw_fuzzy_2_12[[1]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 2)

# Will need to pull each cluster membership df separately and
# merge together if needed too.

# Add new column to assign groupings with 90% cutoff.
dtw_clusters$group <- case_when(dtw_clusters$cluster_1 >= 0.9 ~ "Cluster 1",
                                dtw_clusters$cluster_2 >= 0.9 ~ "Cluster 2",
                                TRUE ~ "Neither")

# Export for use in posthoc analyses.
saveRDS(dtw_clusters,
        "data_working/dtw_clusters_2022_050225.rds")

# And plot these results.
# First need to make the dataset into a df.
data_df <- plyr::ldply(data, data.frame)

# And join with the cluster data.
full_df <- left_join(data_df, dtw_clusters,
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

(fig_curves <- ggplot(full_df %>%
                        filter(site %in% c("BW", "GB")), 
                      aes(x = hour_index, y = DO_sat,
                      color = group, group = `.id`)) +
               geom_line() +
               scale_color_manual(values = c("#FABA39FF", 
                                             "#1AE4B6FF",
                                             "gray30")) + 
               labs(x = "Hour of Day (+4)", 
                    y = "Dissolved Oxygen (% Saturation)") +
               #ylim(30, 180) +
    # adding annotation to better delineate time of day
               annotate('rect', xmin = 0, xmax = 3,
                        ymin = 35, ymax = 170,
                        alpha = 0.2, fill = "black") + #sunrise
               annotate('rect', xmin = 14, xmax = 24,
                        ymin = 35, ymax = 170,
                        alpha = 0.2, fill = "black") + #sunset
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
            q25DO_mgL = quantile(DO_mgL, 
                                 probs = 0.25),
            q75DO_mgL = quantile(DO_mgL, 
                                 probs = 0.75),
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
             ymin = 7.5, ymax = 10,
             alpha = 0.15, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 24,
             ymin = 7.5, ymax = 10,
             alpha = 0.15, fill = "black") + #sunset
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.5,
                linewidth = 0.1) + 
    scale_color_manual(values = c("#FABA39FF", 
                                  "#D46F10",
                                  "gray70")) +
    scale_fill_manual(values = c("#FABA39FF", 
                                 "#D46F10",
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
mean(full_df$solar_noon) # 45597.65 secs or 12:39:58

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
    geom_vline(xintercept = 8.66601, color = "#FFAA00",
               linetype = "dashed", linewidth = 2) +
    geom_point(x = 10.10119, y = -1.09124643, 
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") +
    geom_point(x = 7.50414, y = 0.84321457, 
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") +
    scale_color_manual(values = c("#FABA39FF", 
                                  "#D46F10",
                                  "gray70")) +
    scale_fill_manual(values = c("#FABA39FF", 
                                 "#D46F10",
                                 "gray70")) +
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

# New facet label names
site.labs <- c("West Shore",
               "East Shore")
names(site.labs) <- c("BW", "GB")

(fig_months <- ggplot(full_df_daily %>%
                      mutate(location_f = factor(
                        case_when(location == "3m" ~ "nearshore",
                                  location == "10m" ~ "shallow", 
                                  location == "15m" ~ "mid",
                                  location == "20m" ~ "deep"),
                        levels = c("nearshore",
                                   "shallow",
                                   "mid",
                                   "deep"))), 
                    aes(x = month)) +
              geom_bar(aes(fill = factor(group))) +
              scale_fill_manual(values = c("#FABA39FF", 
                                           "#D46F10",
                                           "gray80")) +
              # customizing which months print below the x axis
              scale_x_discrete(breaks = levels(full_df_daily$month)[c(T, F, F, T, F, F, T, F, F, T, F)]) +
              labs(x = "Month of Year",
                   y = "Timeseries count (days)",
                   fill = "Cluster ID") +
              theme_bw() +
              facet_grid(location_f~site, scales = "free",
                         labeller = labeller(site = site.labs)) +
    theme(text = element_text(size = 18)))

# Export figure.
(fig_all <- ((fig2_curves / fig2_scaled) | fig_months) +
    plot_annotation(tag_levels = 'A'))

# ggsave(plot = fig_all,
#        filename = "figures/dtw_2022_050325.png",
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
  summarize(no_days = n(),
            median_mean_DOsat = median(mean_DOsat),
            mean_range_DOsat = mean(range_DOsat),
            median_mean_DOmgL = median(mean_DOmgL),
            mean_range_DOmgL = mean(range_DOmgL),
            median_mean_Temp = median(mean_Temp),
            mean_range_Temp = mean(range_Temp)) %>%
  ungroup()

# Need to calculate the difference between max DO 
# and solar noon.
full_df_max_DO_difftimes <- full_df %>%
  group_by(`.id`, site, 
           location, replicate, group) %>%
  slice_max(DO_mgL) %>%
  ungroup() %>%
  mutate(DOmax_hour = hour(date_times),
         DOmax_minutes = minute(date_times),
         DOmax_seconds = second(date_times)) %>%
  mutate(DOmax_time = as_hms(paste(DOmax_hour,
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
14.10119-11.50414 # 2.59705

##### DO % sat #####

# Perform clustered DTW on data from April 2022 through
# February 2023 on either side of the lake and all replicates 
# across 3m, 10m, and 20m water depths.
dtw_fuzzy_2_12sat <- tsclust(data_DOsat, 
                          
                          # fuzzy - ts may belong partially to 
                          # multiple groups (output a proportion)
                          type = "fuzzy", 
                          
                          # 2L - 2 state hypothesis (biology vs. physics)
                          # 12L - 12 state hypothesis (sites independent)
                          k = 2L:12L, # 10 possible
                          
                          # state that no pre-processing should occur
                          preproc = NULL,
                          
                          # dtw - ???
                          distance = "dtw", 
                          
                          # fcm - fuzzy c-means; fcmdd - fuzzy c-mediods
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

# Takes about 2 hours on UNR Desktop
# (started 2:46pm, finished at 3:37 pm)

# Takes about 1 hour on Pinyon
# Run2 (started 4:49pm, finished at 5:53pm)

# Export model fit.
saveRDS(dtw_fuzzy_2_12sat, 
        "data_model_outputs/dtw_2022_fuzzy_052325.rds")

# Examine cluster validity indices. Be patient - takes a moment.
dtw_sat_results <- lapply(dtw_fuzzy_2_12sat, cvi)

dtw_sat_results_df <- as.data.frame(dtw_sat_results,
                                col.names = c("L2", "L3", "L4",
                                              "L5", "L6", "L7",
                                              "L8", "L9", "L10",
                                              "L11", "L12"))

dtw_sat_results_df <- t(dtw_sat_results_df)

# Want to:
# Maximize MPC
# Minimize K
# Minimize T
# Maximize SC
# Maximize PBMF

# Examine the most parsimonious clusterings.
plot(dtw_fuzzy_2_12sat[[1]]) # Per MPC, K, and T metrics (2 clusters)
plot(dtw_fuzzy_2_12sat[[1]], type = "c")
plot(dtw_fuzzy_2_12sat[[11]]) # Per SC & PBMF metrics (12 clusters)

# Export cluster groupings for most parsimonious model fit.
dtw_sat_clusters <- as.data.frame(dtw_fuzzy_2_12sat[[1]]@fcluster) %>%
  rownames_to_column() %>%
  rename(uniqueID = rowname) %>%
  mutate(clusters = 2)

# Add new column to assign groupings with 90% cutoff.
dtw_sat_clusters$group <- case_when(dtw_sat_clusters$cluster_1 >= 0.9 ~ "Cluster 1",
                                dtw_sat_clusters$cluster_2 >= 0.9 ~ "Cluster 2",
                                TRUE ~ "Neither")

# Export for use in posthoc analyses.
saveRDS(dtw_sat_clusters,
        "data_working/dtw_clusters_2022_052425.rds")

# And plot these results.
# First need to make the dataset into a df.
data_df <- plyr::ldply(data, data.frame)

# And join with the cluster data.
full_df_sat <- left_join(data_df, dtw_sat_clusters,
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

# Also making some summary statistics to more clearly plot
# the clusters.
summary_df_sat <- full_df_sat %>%
  # across the daily time series in each cluster...
  group_by(group, hour_index) %>%
  # calculate the median and interquartile ranges
  summarize(medianDO_sat = median(DO_sat),
            q25DO_sat = quantile(DO_sat, 
                                 probs = 0.25),
            q75DO_sat = quantile(DO_sat, 
                                 probs = 0.75),
            medianDOscale_sat = median(scaled_DO_sat),
            q25DOscale_sat = quantile(scaled_DO_sat, 
                                      probs = 0.25),
            q75DOscale_sat = quantile(scaled_DO_sat, 
                                      probs = 0.75)) %>%
  ungroup()

(fig2_curves_sat <- ggplot(summary_df_sat, 
                       aes(x = hour_index, 
                           y = medianDO_sat,
                           ymin = q25DO_sat, 
                           ymax = q75DO_sat,
                           color = group, 
                           fill = group)) +
    # adding annotation to better delineate time of day
    annotate('rect', xmin = 0, xmax = 3,
             ymin = 95, ymax = 112,
             alpha = 0.15, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 24,
             ymin = 95, ymax = 112,
             alpha = 0.15, fill = "black") + #sunset
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.5,
                linewidth = 0.1) + 
    scale_color_manual(values = c("#FABA39FF", 
                                  "#D46F10",
                                  "gray70")) +
    scale_fill_manual(values = c("#FABA39FF", 
                                 "#D46F10",
                                 "gray70")) +
    labs(x = "Hour of Day", 
         y = "DO (% saturation)") +
    scale_x_continuous(breaks = c(0,5,10,15,20),
                       labels = c(4,9,14,19,24)) +
    theme_bw() +
    facet_wrap(group~.,
               labeller = labeller(
                 group = c('Cluster 1' = "Synchronous",
                           'Cluster 2' = "Lagged",
                           'Neither' = "Neither")
               )) +
    theme(legend.position = "none",
          text = element_text(size = 18)))

# Calculate solar noon across dataset to add a solar
# line to the plot below.
# Trim down to unique days.
trim_df_sat <- full_df_sat %>%
  group_by(ID_index) %>%
  slice_head()

median(trim_df_sat$solar_noon) # 46536 secs or 12:55:36
quantile(trim_df_sat$solar_noon, prob = 0.025) # 11:44:33 
quantile(trim_df_sat$solar_noon, prob = 0.975) # 13:06:55

mean(trim_df_sat$solar_noon) # 45599.2 secs or 12.66645 hrs
sd(trim_df_sat$solar_noon) # 1695.047 secs or 0.4708 hrs

(fig2_scaled_sat <- ggplot(summary_df_sat %>%
                         filter(group %in% c("Cluster 1",
                                             "Cluster 2")), 
                       aes(x = hour_index, 
                           y = medianDOscale_sat,
                           ymin = q25DOscale_sat, 
                           ymax = q75DOscale_sat,
                           color = group, 
                           fill = group)) +
    # adding annotation to better delineate time of day
    annotate('rect', xmin = 2, xmax = 3,
             ymin = -1.5, ymax = 1.5,
             alpha = 0.15, fill = "black") + #sunrise
    annotate('rect', xmin = 14, xmax = 15,
             ymin = -1.5, ymax = 1.5,
             alpha = 0.15, fill = "black") + #sunset
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.5,
                linewidth = 0.1) +
    # adding annotation to more clearly delineate mean solar noon
    # 12.66645 - 4 hours
    geom_vline(xintercept = 8.66645, color = "#FFAA00",
               linetype = "dashed", linewidth = 2) +
    # 8.66645 - 0.4708 hours and 8.66645 + 0.4708 hours
    annotate('rect', xmin = 8.1956, xmax = 9.1373,
             ymin = -1.5, ymax = 1.5,
             alpha = 0.15, fill = "#FFAA00") + # +/- 1 S.D.
    # adding annotation to more clearly delineate DO maxima
    # values calculated below on line 782
    geom_point(x = 9.20580, y = -0.5, 
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") + # Cluster 1
    geom_point(x = 11.30502, y = 0.93, 
               shape = 8, size = 5, stroke = 1.5,
               color = "gray20") + # Cluster 2
    scale_color_manual(values = c("#FABA39FF", 
                                  "#D46F10",
                                  "gray70")) +
    scale_fill_manual(values = c("#FABA39FF", 
                                 "#D46F10",
                                 "gray70")) +
    labs(x = "Hour of Day", 
         y = "DO (scaled)") +
    scale_x_continuous(limits = c(2,15),
                       breaks = c(2,6,10,14),
                       labels = c(6,10,14,18)) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 18)))

# Also creating column with months
full_df_sat <- full_df_sat %>%
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
# collapse this down to days.
full_df_sat_daily <- full_df_sat %>%
  # first need to trim down the dataset so it doesn't
  # duplicate days (4am one -> 4am the next)
  group_by(`.id`) %>%
  slice_head() %>%
  ungroup() %>%
  select(`.id`, date, month, site, location, replicate, group) %>%
  unique()

counts_sat_daily <- full_df_sat_daily %>%
  count(group) %>%
  ungroup()

# New facet label names
site.labs <- c("West Shore",
               "East Shore")
names(site.labs) <- c("BW", "GB")

(fig_months_sat <- ggplot(full_df_sat_daily %>%
                        mutate(location_f = factor(
                          case_when(location == "3m" ~ "nearshore",
                                    location == "10m" ~ "shallow", 
                                    location == "15m" ~ "mid",
                                    location == "20m" ~ "deep"),
                          levels = c("nearshore",
                                     "shallow",
                                     "mid",
                                     "deep"))), 
                      aes(x = month)) +
    geom_bar(aes(fill = factor(group))) +
    scale_fill_manual(values = c("#FABA39FF", 
                                 "#D46F10",
                                 "gray80"),
                      labels = c("Cluster 1" = "Synchronous", 
                                 "Cluster 2" = "Lagged", 
                                 "Neither" = "Neither")) +
    # customizing which months print below the x axis
    scale_x_discrete(breaks = levels(full_df_sat_daily$month)[c(T, F, F, T, F, F, T, F, F, T, F)]) +
    labs(x = "Month of Year",
         y = "Timeseries count (days)",
         fill = "Cluster") +
    theme_bw() +
    facet_grid(location_f~site, scales = "free",
               labeller = labeller(site = site.labs)) +
    theme(text = element_text(size = 18)))

# Export figure.
(fig_all_sat <- ((fig2_curves_sat / fig2_scaled_sat) | fig_months_sat) +
    plot_annotation(tag_levels = 'A'))

# ggsave(plot = fig_all_sat,
#        filename = "figures/dtw_2022_112425.png",
#        width = 30,
#        height = 20,
#        units = "cm")

###### Posthoc analyses #####

# I also need to generate some descriptive statistics about
# each of these groups.
full_df_sat_daily_summary <- full_df_sat %>%
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

full_df_sat_group_summary <- full_df_sat_daily_summary %>%
  group_by(group) %>%
  summarize(no_days = n(),
            mean_mean_DOsat = mean(mean_DOsat),
            sd_mean_DOsat = sd(mean_DOsat),
            mean_range_DOsat = mean(range_DOsat),
            sd_range_DOsat = sd(range_DOsat),
            median_mean_DOmgL = median(mean_DOmgL),
            mean_range_DOmgL = mean(range_DOmgL),
            mean_mean_Temp = mean(mean_Temp),
            sd_mean_Temp = sd(mean_Temp),
            mean_range_Temp = mean(range_Temp),
            sd_range_Temp = sd(range_Temp)) %>%
  ungroup()

# Need to calculate the difference between max DO 
# and solar noon.
full_df_sat_max_DO_difftimes <- full_df_sat %>%
  group_by(`.id`, site, 
           location, replicate, group) %>%
  slice_max(DO_sat) %>%
  ungroup() %>%
  mutate(DOmax_hour = hour(date_times),
         DOmax_minutes = minute(date_times),
         DOmax_seconds = second(date_times)) %>%
  mutate(DOmax_time = as_hms(paste(DOmax_hour,
                                   DOmax_minutes,
                                   DOmax_seconds,
                                   sep = ":"))) %>%
  mutate(max_offset = solar_noon - DOmax_time)

offset_group_summary_sat <- full_df_sat_max_DO_difftimes %>%
  group_by(group) %>%
  summarize(median_offset_min = median(as.numeric(max_offset))/60,
            mean_offset_min = mean(as.numeric(max_offset))/60,
            minimum_offset_min = min(as.numeric(max_offset))/60,
            maximum_offset_min = max(as.numeric(max_offset))/60,
            stddev_offset_min = sd(as.numeric(max_offset))/60) %>%
  ungroup()

# Also calculating mean hour of maximum DO
max_summary_sat <- full_df_sat_max_DO_difftimes %>%
  group_by(group) %>%
  summarize(mean_time_of_DOmax = mean(DOmax_time)) %>%
  ungroup() %>%
  mutate(mean_minute_of_DOmax = as.numeric(mean_time_of_DOmax)/60) %>%
  mutate(mean_hour_of_DOmax = mean_minute_of_DOmax/60)

# Difference between mean peak times of either cluster
13.20580-15.30502 # -2.09922

# End of script.
