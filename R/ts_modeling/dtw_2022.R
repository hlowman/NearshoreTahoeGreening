# Dynamic Time Warping Stage I Script
# Authors: Heili E. Lowman
# Date Created: 2024-05-31

# ---------------------------- README ---------------------------------
# The following script will fit data from the first stage of
# the project using the clustered dynamic time warping approach.

#### SETUP ####

# Load packages.
library(lubridate)
library(tidyverse)
library(data.table)
library(here)
library(dtwclust)

# Load data.
data <- readRDS("data_working/do_data_2022_dailylist_053124.rds")

#### TIDY ####

# Must include ONLY values in the input dataset.
# And applying scale-transform.
data_DO <- lapply(data, function(x) {x$scaled_DO_mg_L})

#### MODEL FIT ####

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

# Examine most parsimonious output.
plot(dtw_2_12[[1]]) # by most measures
plot(dtw_2_12[[4]]) # by D measure
plot(dtw_2_12[[10]]) # by COP measure

# Hmmmm, this is looking fairly messy, likely due to the
# sheer number of days for which I have data.

# End of script.
