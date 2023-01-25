#### RBR Cast Data
### January 18, 2023
## Heili Lowman

# The following script will examine the RBR data to determine the best
# way of automating profile delineation.

# Load packages.
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

# Load data.
rbr_df <- read_excel(
  "data_raw/Lake_Profiles_GB_and_BW_063584_20220926_20220926_1902.xlsx", 
  sheet = "Data", # only "Data" worksheet
  skip = 1) # skip the first column

# By my estimation there should be roughly 11 casts in here, but 
# let's use a quick plot to see.

# Removing tests done on 9/22.

test_df <- rbr_df %>%
  filter(Time > as_date(as.character("2022-09-26 00:00:00")))

ggplot(test_df, aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Ok, so does look like there's roughly 12 casts, and the depths
# match up with what we'd imagine the depths to be (20, 10, 15, 5 etc.)

# Now, let's load in the instrument's cast delineations.

casts_df <- read_excel(
  "data_raw/Lake_Profiles_GB_and_BW_063584_20220926_20220926_1902.xlsx", 
  sheet = "Profile annotation", # only this worksheet
  skip = 1) # skip the first column

# Filter out for only the downcasts.
downcasts <- casts_df %>%
  filter(Type == "DOWN")

# And add a time interval by which we can filter the full dataset.
downcasts <- downcasts %>%
  mutate(interval = interval(start = `Time 1`, 
                             end = `Time 2`))

# And now to filter the data set by the time frames of the downcasts alone.
# Create a list of the intervals by which we're filtering.
lst <- as.list(downcasts$interval)

# And create a new column in the raw dataset to indicate whether the dates
# should be kept (TRUE) or filtered out (FALSE).
rbr_df <- rbr_df %>%
  mutate(Downcast = Time %within% lst)

# And let's look at the data to see how this compares to plotting the raw data
# the first time around.
dc_df <- rbr_df %>%
  filter(Downcast == TRUE) # filter for only downcast times

ggplot(dc_df, aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Hmmmm ok so this seems to be cutting off some of the shallower parts 
# of the casts.

# Let's look at some of the other data available.
ggplot(test_df, aes(x = Time, y = Temperature)) +
  geom_point() +
  theme_bw()

ggplot(test_df, aes(x = Time, y = `Dissolved O₂ concentration`)) +
  geom_point() +
  theme_bw()

ggplot(dc_df, aes(x = Temperature, y = Depth)) +
  geom_point() +
  theme_bw()

# At the moment, can't think of a tighter delineation strategy than above,
# so, adding cast #s to the dataset above.

# create placeholder columns for day-to-day differences and sequences
dc_df <- dc_df %>%
  mutate(diff_time = 0, cast = 1)
  
# calculate the difference from one measurement to the next
for(i in 2:nrow(dc_df)){
  dc_df$diff_time[i] = difftime(time1 = dc_df$Time[i], 
                                time2 = dc_df$Time[(i-1)],
                                units = "secs") 
  }
  
# delineate sequenced time frames based on differences
# anything less than 60 second gaps is permissible
for(i in 2:nrow(dc_df)){
    if(dc_df$diff_time[i] < 60){ # if the gap is smaller than 60 seconds
      dc_df$cast[i] = dc_df$cast[(i-1)] # this row has the same cast # as the row before
    } else { # if the gap is larger than 60 seconds
      dc_df$cast[i] = dc_df$cast[(i-1)] + 1 # this row has a new cast # (+1) than the row before
    }
  }
  
# Ok, so now we should be able to plot and split by cast #.
ggplot(dc_df, aes(x = Temperature, y = -Depth, color = cast)) +
  geom_point() +
  theme_bw() +
  facet_wrap(.~cast)

# The cutoffs still seem a bit weird, so we may need to play around with this.
# But it's a start!

#### Trying again ####

# So, after chatting with Joanna, I may still be able to automate this
# but I may need to use max/min values instead of the "down" values from
# the second tab of this dataset.

# zooming in on the larger dataset a bit more
ggplot(test_df %>%
         filter(Time < as_datetime(as.character("2022-09-26 13:00:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Filter out for the full casts.
fullcasts <- casts_df %>%
  filter(Type == "PROFILE")

# And add a time interval by which we can filter the full dataset.
fullcasts <- fullcasts %>%
  mutate(interval = interval(start = `Time 1`, 
                             end = `Time 2`))

# And now to filter the data set by the time frames of the full casts.
# Create a list of the intervals by which we're filtering.
lst2 <- as.list(fullcasts$interval)

# And create a new column in the raw dataset to indicate whether the dates
# should be kept (TRUE) or filtered out (FALSE).
rbr_df <- rbr_df %>%
  mutate(Fullcast = Time %within% lst2)

# And let's look at the data to see how this compares to plotting the raw data
# the first time around.
fc_df <- rbr_df %>%
  filter(Fullcast == TRUE) # filter for only downcast times

ggplot(fc_df, aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Ok, so that doesn't really work either.

# Adding a new column to detect difference in depth w previous measurement.
test_df <- test_df %>%
  mutate(lag_depth = 0) # placeholder column

for(i in 2:nrow(test_df)){
  test_df$lag_depth[i] = test_df$Depth[i] - test_df$Depth[(i-1)] 
}

# So, lags will be positive on the way down, negative on the way up.
# Let's try filtering by the lag and see what that leaves us with.

dc_lag_df <- test_df %>%
  filter(lag_depth > 0)

ggplot(dc_lag_df %>%
         filter(Time < as_datetime(as.character("2022-09-26 13:00:00"))),
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Hmmm, need to add some sort of additional filter to take out the remaining points.

# Let's try using the up casts from the other dataset. So, using similar
# workflow as above.

# Filter out for only the upcasts.
upcasts <- casts_df %>%
  filter(Type == "UP")

# And add a time interval by which we can filter the full dataset.
upcasts <- upcasts %>%
  mutate(interval = interval(start = `Time 1`, 
                             end = `Time 2`))

# And now to filter the data set by the time frames of the upcasts alone.
# Create a list of the intervals by which we're filtering.
lst3 <- as.list(upcasts$interval)

# And create a new column in the raw dataset to indicate whether the dates
# should be kept (TRUE) or filtered out (FALSE).
test_df <- test_df %>%
  mutate(Upcast = Time %within% lst3)

# What does just the upcast data look like?
uc_df <- test_df %>%
  filter(Upcast == "TRUE")

ggplot(uc_df %>%
         filter(Time < as_datetime(as.character("2022-09-26 13:00:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Ok, so those are crazy clean, so let's just filter by those.

dc_df2 <- test_df %>%
  filter(Upcast == "FALSE")

ggplot(dc_df2 %>%
         filter(Time < as_datetime(as.character("2022-09-26 13:00:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# So, some of the upcast is still in there, so let's now also filter out by lag.

dc_df3 <- dc_df2 %>%
  filter(lag_depth > 0)

ggplot(dc_df3 %>%
         filter(Time < as_datetime(as.character("2022-09-26 13:00:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# YAY!!! Ok, so this looks pretty good.

#### Final workflow ####

##### Step 1: Load in packages & data. #####

# Load packages.
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

# Load cast data.
rbr_df <- read_excel(
  "Lake_Profiles_GB_and_BW_063584_20220926_20220926_1902.xlsx", 
  sheet = "Data", # only "Data" worksheet
  skip = 1) # skip the first column

# Load cast metadata.
casts_df <- read_excel(
  "Lake_Profiles_GB_and_BW_063584_20220926_20220926_1902.xlsx", 
  sheet = "Profile annotation", # only this worksheet
  skip = 1) # skip the first column

##### Step 2: Add columns to the "rbr_df" cast data #####
# to denote depth lags and what the instrument deems to be the "upcasts".

# Adding a new column to detect difference in depth w previous measurement.
rbr_df <- rbr_df %>%
  mutate(lag_depth = 0) # placeholder column

# So, lags will be positive on the way down, negative on the way up.
for(i in 2:nrow(rbr_df)){
  rbr_df$lag_depth[i] = rbr_df$Depth[i] - rbr_df$Depth[(i-1)] 
}

# Now, filter metadata dataframe for only the upcasts.
upcasts <- casts_df %>%
  filter(Type == "UP")

# And add a time interval by which we can filter the full dataset.
# Run ?interval() in the console for more info about this function in the
# `lubridate` package.
upcasts <- upcasts %>%
  mutate(interval = interval(start = `Time 1`, 
                             end = `Time 2`))

# And now to filter the data set by the time frames of the upcasts alone.
# Create a list of the intervals by which we're filtering.
cast_list <- as.list(upcasts$interval)

# And create a new column in the raw dataset to indicate whether the dates
# should be kept (TRUE) or filtered out (FALSE).
rbr_df <- rbr_df %>%
  mutate(Upcast = Time %within% cast_list)

##### Step 3: Filter the RBR data. #####

# So, this is what our data looks like right now, and I'm choosing to zoom
# in on the first two casts just for visualization's sake.
ggplot(rbr_df %>%
         # this extra date filter is to remove a data point when it was
         # turned on in lab on September 21st
         filter(Time > as_datetime(as.character("2022-09-22 12:00:00")) & 
                Time < as_datetime(as.character("2022-09-26 12:40:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# First, we filter out upcasts, because these are much cleaner than recorded
# downcasts for whatever reason.
rbr_df_uc <- rbr_df %>%
  filter(Upcast == "FALSE")

ggplot(rbr_df_uc %>%
         filter(Time > as_datetime(as.character("2022-09-22 12:00:00")) & 
                Time < as_datetime(as.character("2022-09-26 12:40:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Next, to get rid of these little extra blips, we will also filter by
# the depth lags so that we only have records when the instrument is traveling
# down in the water column.
rbr_df_uc_lag <- rbr_df_uc %>%
  filter(lag_depth > 0)

ggplot(rbr_df_uc_lag %>%
         filter(Time > as_datetime(as.character("2022-09-22 12:00:00")) &
                Time < as_datetime(as.character("2022-09-26 12:40:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Woohoo!! This looks pretty darn good.

##### Step 4: Remove top 0.5m of data. #####

# This step we should run by Joanna, but in my experience, the top little
# bit of most profiles is unreliable anyway, because the instrument itself is
# kind of long, so here's code for it that we can change based on the depth
# we feel is appropriate.
rbr_df_uc_lag_trim <- rbr_df_uc_lag %>%
  filter(Depth > 0.5)

ggplot(rbr_df_uc_lag_trim %>%
         filter(Time > as_datetime(as.character("2022-09-22 12:00:00")) &
                  Time < as_datetime(as.character("2022-09-26 12:40:00"))), 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

##### Step 5: Assign cast numbers and export the data. #####

# Let's take one last look at the full dataset to see if there's any
# additional trimming we should do.
ggplot(rbr_df_uc_lag_trim, 
       aes(x = Time, y = Depth)) +
  geom_point() +
  theme_bw()

# Some of the loss of data in the surface is inevitable.

# Assign cast numbers to each of the casts.
# create placeholder columns for day-to-day differences and sequences
df_for_export <- rbr_df_uc_lag_trim %>%
  mutate(diff_time = 0, cast = 1)

# calculate the difference from one measurement to the next
for(i in 2:nrow(df_for_export)){
  df_for_export$diff_time[i] = difftime(time1 = df_for_export$Time[i], 
                                time2 = df_for_export$Time[(i-1)],
                                units = "secs") 
}

# delineate sequenced time frames based on differences
# anything less than 60 second gaps is permissible
for(i in 2:nrow(df_for_export)){
  if(df_for_export$diff_time[i] < 60){ # if the gap is smaller than 60 seconds
    df_for_export$cast[i] = df_for_export$cast[(i-1)] # then this row has the same cast # as the row before
  } else { # if the gap is larger than 60 seconds
    df_for_export$cast[i] = df_for_export$cast[(i-1)] + 1 # then this row has a new cast # (+1) than the row before
  }
}

# And let's make cast a factor just to be certain.
df_for_export <- df_for_export %>%
  mutate(cast_factor = factor(cast))

# Ok, so now we should be able to plot and split by cast #.
# And let's make it pretty!!
ggplot(df_for_export, aes(x = Temperature, y = -Depth, color = cast_factor)) +
  geom_point() +
  scale_color_manual(values = c("#CECEB9", "#B6CCB8", "#9ECBB7", "#86C9B7",
                                "#78C3AF", "#74B7A1", "#70AC92", "#6CA184", 
                                "#5C9DA1", "#4D99BF", "#3E94DD", "#3586CF", 
                                "#316C97")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~cast_factor, scales = "free")

# And export the data as either .rds or .csv.
saveRDS(df_for_export, "data_working/RBR_Data_Clean_GB_BW_092622.rds")
write_csv(df_for_export, "data_working/RBR_Data_Clean_GB_BW_092622.csv")

# End of script.
