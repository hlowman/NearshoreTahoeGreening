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
  
# calculate the difference from one masurement to the next
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

# End of script.
