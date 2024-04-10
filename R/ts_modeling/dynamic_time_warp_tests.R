# Dynamic Time WArping Analysis Test Script
# Authors: Heili E. Lowman
# Date Created: 2024-04-10

# ---------------------------- README ---------------------------------
# More information on the dtw and dtwclust packages may be found at these
# sources:
# --- for dtw package ---
# https://rpubs.com/esobolewska/dtw-time-series
# https://rtavenar.github.io/blog/dtw.html
# http://cran.nexr.com/web/packages/dtw/vignettes/dtw.pdf
# --- for dtwclust package ---
# https://github.com/asardaes/dtwclust
# https://cran.r-project.org/web/packages/dtwclust/dtwclust.pdf

#### Setup ####

# Load packages
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(patchwork)
library(dtw)
library(dtwclust)

# Load data
dat <- readRDS("data_working/do_sat_shiny_010924.rds")

#### Tidy ####

# Flagged data has already been removed, but I'm not sure how this algorithm
# works with missing/large datasets, so I need to trim down to a complete and
# smaller amount to start.

# Let's start with nearshore (a.k.a. cinder block data that I anticipate will
# show a diel pattern at some points).
test_dat <- dat %>%
  filter(site == "BW",
         location == "3m",
         replicate == "NS2",
         Pacific_Standard_Time >= ymd("2022-03-01"),
         Pacific_Standard_Time < ymd("2022-04-01"))

# And need to assign indices to each day (4am-4am) to delineate each separate
# ts to be compared.
test_dat <- test_dat %>%
  mutate(hour = hour(Pacific_Standard_Time),
         index = 0)

for(i in 2:nrow(test_dat)){
  # if the time is equal to 4am, AND it's the first instance
  if(test_dat$hour[i] == 4 & test_dat$hour[i-1] == 3){
    # the index value is one greater than the hour before
    test_dat$index[i] = test_dat$index[(i-1)] + 1
    # if the time is any other hour,
  } else {
    # the index value is the same as the hour before
    test_dat$index[i] = test_dat$index[(i-1)]
  }
} # Yay!

# And finally split the dataset by day.
test_dat <- test_dat %>%
  filter(index > 0)

test_list <- split(test_dat, test_dat$index)

# And remove days with missing values.
test_list <- test_list[-12]
test_list <- test_list[-30]

#### Basic DTW ####

# Beginning with comparison of only two days.
test_dtw1 <- dtw(test_list$`1`$Dissolved_O_mg_L, test_list$`2`$Dissolved_O_mg_L)
# This ran quickly - that's promising.

# Now, let's plot to see what this looks like.
plot(dtw(test_list$`1`$Dissolved_O_mg_L, test_list$`2`$Dissolved_O_mg_L, keep=TRUE), 
     xlab="Day 1 DO", ylab="Day 2 DO", #xaxp  = c(0,10,10), yaxp = c(0,10,10), 
     type="threeway")

#### Tidy Again ####

# Let's pull each DO column out.

test_list_DO <- lapply(test_list, function(x) {x$Dissolved_O_mg_L})
# Days with missing data already removed above.

# test_df_DO <- as.data.frame(test_list_DO)

# Renaming column names for easier interpretation on the back end.
# colnames(test_df_DO) <- c("Mar1", "Mar2", "Mar3", "Mar4", "Mar5",
#                           "Mar6", "Mar7", "Mar8", "Mar9", "Mar10",
#                           "Mar11", "Mar13", "Mar14", "Mar15", "Mar16",
#                           "Mar17", "Mar18", "Mar19", "Mar20", "Mar21",
#                           "Mar22", "Mar23", "Mar24", "Mar25", "Mar26",
#                           "Mar27", "Mar28", "Mar29", "Mar30")

# Scale transform.
# test_df_DO_tscale <- scale(test_df_DO_t)

#### Clustered DTW ####

# Perform clustered DTW on the month of March.
test_dtw2 <- tsclust(test_list_DO, 
                     type="partitional", 
                     k=2L, # 2 clusters
                     distance="dtw_basic", 
                     centroid="pam")

# Examine output.
plot(test_dtw2)

# Examine cluster validity indices.
cvi(test_dtw2)

# Sil       SF           CH       DB       DBstar   D         COP 
# 0.2114766 8.881784e-16 5.361354 1.544541 1.544541 0.1262940 0.6186508 

# What about with 4 clusters?
test_dtw4 <- tsclust(test_list_DO, 
                     type="partitional", 
                     k=4L, # 4 clusters
                     distance="dtw_basic", 
                     centroid="pam")
# Took a few seconds.

# Examine output.
plot(test_dtw4)

# Examine cluster validity indices.
cvi(test_dtw4)

# Sil        SF        CH          DB         DBstar     D          COP 
# 0.1931061  0.0000000 11.5317926  1.0538800  1.1829872  0.2475608  0.3142939 
# Performs better than 2 clusters.

# What about with 10 clusters?
test_dtw10 <- tsclust(test_list_DO, 
                     type="partitional", 
                     k=10L, # 10 clusters
                     distance="dtw_basic", 
                     centroid="pam")
# Took a few seconds.

# Examine output.
plot(test_dtw10)

# Examine cluster validity indices.
cvi(test_dtw10)

# Sil       SF        CH        DB        DBstar    D         COP 
# 0.0593305 0.0000000 3.5808396 1.0515499 1.3365006 0.2477077 0.2167566 
# Performs worse than 4 clusters.

# Want to:
# Maximize SIL
# Maximize SF
# Maximize CH
# Minimize DB
# Minimize DBstar
# Maximize D
# Minimize COP

#### Tidy Yet Again ####

# ok, so part of my oncern with this method is that it is so sensitive
# that variation across sites might just get swamped by noise in the data.

# so, let's take a look at some other sites for that same month.

# ARGH - ok so I'm aggregating to the closest hour as well because the
# data frequencies at different depths are different >_<

# Let's start with 3m again.
test_dat3 <- dat %>%
  filter(site == "BW",
         location == "3m",
         replicate == "NS2",
         Pacific_Standard_Time >= ymd("2022-04-01"),
         Pacific_Standard_Time < ymd("2022-05-01"))

# And need to assign indices to each day (4am-4am) to delineate each separate
# ts to be compared.
test_dat3 <- test_dat3 %>%
  mutate(date = date(Pacific_Standard_Time),
    hour = hour(Pacific_Standard_Time)) %>%
  group_by(date, hour) %>%
  summarize(DO_mg_L = mean(Dissolved_O_mg_L, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(index = 0)

for(i in 2:nrow(test_dat3)){
  # if the time is equal to 4am
  if(test_dat3$hour[i] == 4){
    # the index value is one greater than the hour before
    test_dat3$index[i] = test_dat3$index[(i-1)] + 1
    # if the time is any other hour,
  } else {
    # the index value is the same as the hour before
    test_dat3$index[i] = test_dat3$index[(i-1)]
  }
}

# And now 10m.
test_dat10 <- dat %>%
  filter(site == "BW",
         location == "10m",
         Pacific_Standard_Time >= ymd("2022-04-01"),
         Pacific_Standard_Time < ymd("2022-05-01"))

# And need to assign indices to each day (4am-4am) to delineate each separate
# ts to be compared.
test_dat10 <- test_dat10 %>%
  mutate(date = date(Pacific_Standard_Time),
         hour = hour(Pacific_Standard_Time)) %>%
  group_by(date, hour) %>%
  summarize(DO_mg_L = mean(Dissolved_O_mg_L, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(index = 100)

for(i in 2:nrow(test_dat10)){
  # if the time is equal to 4am
  if(test_dat10$hour[i] == 4){
    # the index value is one greater than the hour before
    test_dat10$index[i] = test_dat10$index[(i-1)] + 1
    # if the time is any other hour,
  } else {
    # the index value is the same as the hour before
    test_dat10$index[i] = test_dat10$index[(i-1)]
  }
}

# And now 20m.
test_dat20 <- dat %>%
  filter(site == "BW",
         location == "20m",
         Pacific_Standard_Time >= ymd("2022-04-01"),
         Pacific_Standard_Time < ymd("2022-05-01"))

# And need to assign indices to each day (4am-4am) to delineate each separate
# ts to be compared.
test_dat20 <- test_dat20 %>%
  mutate(date = date(Pacific_Standard_Time),
         hour = hour(Pacific_Standard_Time)) %>%
  group_by(date, hour) %>%
  summarize(DO_mg_L = mean(Dissolved_O_mg_L, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(index = 200)

for(i in 2:nrow(test_dat20)){
  # if the time is equal to 4am
  if(test_dat20$hour[i] == 4){
    # the index value is one greater than the hour before
    test_dat20$index[i] = test_dat20$index[(i-1)] + 1
    # if the time is any other hour,
  } else {
    # the index value is the same as the hour before
    test_dat20$index[i] = test_dat20$index[(i-1)]
  }
}


# Join all the datasets together.
test_dat3 <- test_dat3 %>%
  filter(index > 0)
test_dat10 <- test_dat10 %>%
  filter(index > 100)
test_dat20 <- test_dat20 %>%
  filter(index > 200)

test_dat_depths <- rbind(test_dat3, test_dat10)
test_dat_depths <- rbind(test_dat_depths, test_dat20)

# And finally split the dataset by day.
test_list_depths <- split(test_dat_depths, test_dat_depths$index)

# And remove days with missing values.
test_list_depths <- test_list_depths[-89]
test_list_depths <- test_list_depths[-60]
test_list_depths <- test_list_depths[-30]

# And finally pull only the DO data.
test_list_depths_DO <- lapply(test_list_depths, 
                              function(x) {x$DO_mg_L})

#### Clustered DTW (Depth) ####

# Perform clustered DTW on the month of March across
# 3m, 10m, and 20m water depths.
test_dtw_depth4 <- tsclust(test_list_depths_DO, 
                     type="partitional", 
                     k=4L, # 4 clusters
                     distance="dtw_basic", 
                     centroid="pam")
# Took no time.

# Examine output.
plot(test_dtw_depth4)

test_dtw_depth2 <- tsclust(test_list_depths_DO, 
                           type="partitional", 
                           k=2L, # 2 clusters
                           distance="dtw_basic", 
                           centroid="pam")

# Examine output.
plot(test_dtw_depth2)

test_dtw_depth8 <- tsclust(test_list_depths_DO, 
                           type="partitional", 
                           k=8L, # 8 clusters
                           distance="dtw_basic", 
                           centroid="pam")

# Examine output.
plot(test_dtw_depth8)

# Examine cluster validity indices.
cvi(test_dtw_depth2)
cvi(test_dtw_depth4)
cvi(test_dtw_depth8)

# Based on values to minimize/maximize, it seems the 2 cluster approach is best.
# So let's append the clusters to the original dataset.

index <- unique(test_dat_depths$index)
index <- index[-89]
index <- index[-60]
index <- index[-30]

test_dat_clusters <- cbind(as.data.frame(index), 
                           cluster = test_dtw_depth2@cluster)

# Adding another column to id the depth.
test_dat_clusters <- test_dat_clusters %>%
  mutate(site = factor(case_when(index < 100 ~ "BW3m",
                          index > 100 & index < 200 ~ "BW10m",
                          index > 200 ~ "BW20m"),
                       levels = c("BW3m", "BW10m", "BW20m"))) %>%
  mutate(cluster_f = factor(cluster))

ggplot(test_dat_clusters, aes(x = site)) +
  geom_bar(aes(fill = cluster_f)) +
  labs(x = "Site",
       y = "Timeseries count (days)",
       fill = "Cluster ID",
       title = "Dynamic Time Warping in April 2023") +
  theme_bw()

# End of script.
