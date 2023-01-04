#### Cross Correlation Test Script
#### January 4, 2023
#### Heili Lowman

# The following script will read in the miniDOT data compiled las November,
# and run some preliminary cross correlation analyses to see if this analysis
# could be used to link DO with physical water parcel movement if closely
# correlated with changes in temperature.

# ----------------------------------README-------------------------------------
# At this time, I will only be conducting analyses on Blackwood data.

#### Setup ####

# Load packages
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(patchwork)

# Load data
tahoe_all <- readRDS("data_working/Tahoe_compiled_trimmed_111422.rds")

#### ccf() ####

# Time-step of cross-correlation analysis will be time-step of data, so in each
# case, a lag = 1 means a lag time of 15 minutes.

# Create vector variables.
benthic_20m_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 20) %>%
  filter(location == "Benthic") %>%
  select(Temp_C)

benthic_20m_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 20) %>%
  filter(location == "Benthic") %>%
  select(DO_mgL)

pelagic_20m_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 20) %>%
  filter(location == "Pelagic") %>%
  select(Temp_C)

pelagic_20m_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 20) %>%
  filter(location == "Pelagic") %>%
  select(DO_mgL)

benthic_15m_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 15) %>%
  filter(location == "Benthic") %>%
  select(Temp_C)

benthic_15m_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 15) %>%
  filter(location == "Benthic") %>%
  select(DO_mgL)

benthic_10m_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 10) %>%
  filter(location == "Benthic") %>%
  select(Temp_C)

benthic_10m_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(depth == 10) %>%
  filter(location == "Benthic") %>%
  select(DO_mgL)

benthic_NS1_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS1") %>%
  select(Temp_C)

benthic_NS1_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS1") %>%
  select(DO_mgL)

benthic_NS2_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS2") %>%
  select(Temp_C)

benthic_NS2_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS2") %>%
  select(DO_mgL)

benthic_NS3_temp <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS3") %>%
  select(Temp_C)

benthic_NS3_do <- tahoe_all %>%
  filter(site == "Blackwood") %>%
  filter(sensor == "NS3") %>%
  select(DO_mgL)

# Calculate cross-covariance across all depths and sensors (n = 7)
cc20b <- ccf(benthic_20m_temp, benthic_20m_do, 
             lag.max = 200, type = c("covariance"))

cc20p <- ccf(pelagic_20m_temp, pelagic_20m_do, 
             lag.max = 200, type = c("covariance"))

cc15 <- ccf(benthic_15m_temp, benthic_15m_do, 
            lag.max = 200, type = c("covariance"))

cc10 <- ccf(benthic_10m_temp, benthic_10m_do, 
             lag.max = 200, type = c("covariance"))

# It appears the strongest correlation is a negative one at all deeper sites 
# (and strongest [acf = -4 to -1] at benthic relative to pelagic sensors).
# In addition, the correlation of greatest magnitude occurs at lag = 0 for
# all four tests above, suggesting that DO is strongly driven by temp (a.k.a.
# physical water parcel movement) rather than biology at these sites.

ccNS1 <- ccf(benthic_NS1_temp, benthic_NS1_do,
             lag.max = 200, type = c("covariance"))

ccNS2 <- ccf(benthic_NS2_temp, benthic_NS2_do,
             lag.max = 200, type = c("covariance"))

ccNS2dat <- data.frame(lagNS2 = ccNS2$lag, acfNS2 = ccNS2$acf) # Max @ -121

ccNS3 <- ccf(benthic_NS3_temp, benthic_NS3_do,
             lag.max = 200, type = c("covariance"))

# NS1 and NS3 display a similar trend, with the greatest (negative) correlation
# at lag = 0. NS2, however, displays the greatest lag = 121 or 30 hours.
# Overall, the trend looks similar, but perhaps just more exaggerated due to
# the likely biofouling occurring on this sensor.

# Plot all ccfs.
par(mfrow = c(3,2))
plot(cc20b)
#plot(cc20p)
plot(cc15)
plot(cc10)
plot(ccNS1)
plot(ccNS2)
plot(ccNS3)

# Additional plots of data to demonstrate relationships.
par(mfrow = c(3, 2))
o_ylim <- c(6, 12)
t_xlim <- c(4, 22)
plot(benthic_20m_temp$Temp_C, benthic_20m_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)
# plot(pelagic_20m_temp$Temp_C, pelagic_20m_do$DO_mgL, 
#      ylim = o_ylim, xlim = t_xlim)
plot(benthic_15m_temp$Temp_C, benthic_15m_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)
plot(benthic_10m_temp$Temp_C, benthic_10m_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)
plot(benthic_NS1_temp$Temp_C, benthic_NS1_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)
plot(benthic_NS2_temp$Temp_C, benthic_NS2_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)
plot(benthic_NS3_temp$Temp_C, benthic_NS3_do$DO_mgL, 
     ylim = o_ylim, xlim = t_xlim)

# Overall, the relationship between temperature and DO is negatively linear,
# but the variance about the linear relationship is greater at shallower
# (NS1-3) sites.

# End of script.
