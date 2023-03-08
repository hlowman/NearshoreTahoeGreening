#### Metabolism Modeling Workflow
#### February 20, 2023
#### Script created by: Noah Lottig, Facundo Scordo
#### Script edited by: Kelly Loria, Heili Lowman

# This script is designed to calculate k600 values using the
# LakeMetabolizer package for use in the final modeling step.

# Load packages.
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)

#### Get and process high frequency sensor data ####

lake <- "GB10"
lake_id <- "GB10"
# ASSUMPTIONS HERE
max_d <- 501 # total depth of lake tahoe in m
lake.area <- 496000 # need to adjust for nearshore area
out.time.period <- "60 min"
tz <-  c('US/Pacific')

# Load in dataset prepped in Step2 script. 
sonde <- read_csv("data_working/GB10Inputs.csv") 

years <- c(2022)

# Examine data to check times
head(sonde$datetime_PST) # loads back in as UTC gah how annoying!!

#if(lake == "BWNS1"){
  data <- sonde %>% 
    # Need to coerce the correct time zone from UTC to PST
    mutate(datetime_PST = with_tz(datetime_PST, tz=tz)) %>% 
    # Need to remove the extinction coefficient column before...
    select(-extcoef) %>%
    # Dropping all NAs.
    drop_na()
#  }
  
# Visually compared "data" above to "data_full" from Step2 script
# to be sure times are correct and daily PAR trends look correct.
  
# All good :)

# ASSUMPTION HERE
data$z <- c(10) # assume complete mixing at 3 meters

# Identify and filter records that have < 23 hrs of data. 
data_summ <- data %>% 
  group_by(year, yday) %>%
  mutate(obs = sum(!is.na(do))) %>%       
  ungroup()

# Not making additional changes to the mixed layer now, but
# keeping this code here for later...
#mutate(z = ifelse(z<=0.5,.5,z))%>% 
#can't have zero depth zmix
#mutate(z = ifelse(z>=3,3,z)) 
#in littoral zone depth zmix can not be deeper than the littoral depth

# determine data frequency obs/day should be 24
freq <- nrlmetab::calc.freq(data$datetime_PST)

# Calculate k600.
data_k600 <- data_summ %>% 
  filter(obs >= (freq-((freq/24)*2))) %>% # allow for 2 hours missing
  mutate(k600 = k.vachon.base(wnd = wspeed,
              lake.area = lake.area)) %>% # estimate K in m/day
  mutate(kgas = k600.2.kGAS.base(k600 = k600,
              temperature = wtemp,gas = "O2")) %>% # m/d
  mutate(k = (kgas/freq)/z) #convert gas to T^-1

### ASSUMPTION HERE TOO ###
#if(lake == "BWNS1") { 
  data_k600 <- data_k600 %>% 
    mutate(k = ifelse(z<10, 0, k)) 
  # We assume no DO exchange with the Atmosphere. 
  # All DO change is related to metabolism.
#}

ggplot(data = data_k600, aes(x = yday, y = do_eq)) + 
  geom_line() + 
  facet_wrap(vars(year),scales="free_x") +
  geom_point(aes(x = yday, y = z),col="blue",size=0.2)
ggplotly()

#### Prepare for data analysis ####

# Prepare data
sonde_prep <- data_k600 %>%
  arrange(year, yday, hour) %>%
  # for each year, create identifier for uninterrupted stretches of observations
  group_by(year) %>%
  mutate(i = ifelse(is.na(do)==T, 1, 0), 
         j = c(1,abs(diff(i)))) %>% 
  filter(is.na(do)==F) %>%
  mutate(series = cumsum(j)) %>% 
  ungroup() %>%
  # create unique index for each series
  # remove series with fewer than 24 observations
  mutate(unique_series = year + series/length(unique(series))) %>%
  group_by(unique_series) %>%
  mutate(series_length = length(unique_series)) %>%
  ungroup() %>%
  # recreate series index and make unique index for days
  # create index for observations (for joining later)
  # replace 0 par_int with smallest non-zero value
  mutate(unique_series = as.factor(unique_series) %>% 
           as.numeric(),
         unique_day = paste(year, yday) %>% 
           as.factor() %>% 
           as.numeric(),
         index = 1:length(do),
         par_int = ifelse(par_int==0,0.00001, par_int)) #%>%
  #select(-i, -j) 

# Return missing observations for check
sonde_check <- data_k600 %>% 
  expand(year, yday, hour) %>%
  full_join(sonde_prep) %>%
  arrange(year, yday)

ggplot(sonde_check, aes(x = datetime_PST, y = do)) + 
  geom_point(size=0.2) + 
  geom_line() + 
  facet_wrap(vars(year),scales="free_x")

# Export prepared data
#if(length(years) == 1) {
  sonde_check %>%
    write_csv(paste("data_working/sonde_prep_",
                    lake,"_",years,".csv",sep =""))
# } else {
#   sonde_check %>%
#     write_csv(paste("data_working/sonde_prep_",
#                     lake,"_",min(years),"_",max(years),".csv",sep =""))
# }

#### Package data for STAN model ####

# define variables in environment 
o2_freq = freq
o2_obs = 1000*sonde_prep$do # convert to mg m^-3
o2_eq = 1000*sonde_prep$do_eq # convert to mg m^-3
light = sonde_prep$par_int
temp = sonde_prep$wtemp
wspeed = sonde_prep$wspeed
# sch_conv = sonde_prep$sch_conv
map_days = sonde_prep$unique_day
k = sonde_prep$k

#if(length(years) == 1) {
  days_per_year <- array(c({sonde_prep %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value), dim = 1) # 144
# } else {
#   days_per_year <- array(c({sonde_prep %>%
#       group_by(year) %>%
#       summarize(value = length(unique(unique_day)))}$value))
# }

obs_per_series <- array(c({sonde_prep %>%
    group_by(unique_series) %>%
    summarize(value = length(unique_series))}$value)) # 3520

obs_per_day <- array(c({sonde_prep %>%
    group_by(unique_day) %>%
    summarize(value = length(unique_day))}$value)) 

z = sonde_prep$z
n_obs = length(o2_obs)
n_series = length(obs_per_series) 
n_days = sum(days_per_year)
n_years = length(days_per_year)

# Export as .R for STAN model.
# if(length(years)>1) {
#   stan_rdump(c("o2_freq","o2_obs","o2_eq","light",
#                "temp","wspeed","map_days","obs_per_series",
#                "days_per_year","obs_per_day","z","k",
#                "n_obs","n_series","n_days","n_years"),
#              file = paste("data_working/",lake,"_",min(years),
#                           "_",max(years),"_sonde_list.R",sep=""))
# } else {
  stan_rdump(c('o2_freq','o2_obs','o2_eq','light',
               'temp','wspeed','map_days','obs_per_series',
               'days_per_year','obs_per_day','z','k',
               'n_obs','n_series','n_days','n_years'),
             file = paste("data_working/",lake,"_",years,
                          "_sonde_list.R",sep=""))
#}

# End of script.
