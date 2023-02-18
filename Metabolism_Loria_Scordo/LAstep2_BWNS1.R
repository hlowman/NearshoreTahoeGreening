#========== 
#========== Preliminaries
#=
rm(list=ls())
# load packages
library(tidyverse)
library(lubridate)
library(LakeMetabolizer)
library(rstan)
library(patchwork)
library(plotly)

#=========================================== 
# Get and process high frequency sensor data
#===========================================
lake <- "BWNS1"
lake_id <- "BWNS1"
max_d <- 501 # total depth of lake tahoe in m
lake.area <- 496000 # still not sure how far this model should cover in the lake.
out.time.period <- "60 min"
tz <-  c('America/Los_Angeles')#"US/Central"

# 
sonde = read_csv("./FinalInputs/BWNS1Inputs.csv") 

years = c(2021)#,2022) # add in 2021 later

if(lake == "BWNS1"){
  data <- sonde %>% 
    mutate(datetime = ymd_hms(force_tz(datetime,tz=tz))) %>% 
    filter(year == 2021 & yday >= 182 & yday <=273)  %>% 
    drop_na()}

#data[!duplicated(data), ]
data$z<- c(3) # assume complete mix at 3 meters

data <- data %>% 
  group_by(year,yday) %>%
  mutate(obs = sum(!is.na(do))) %>%       #identify and filter records that have < 23 hrs of data 
  ungroup() %>%
  mutate(z = ifelse(z<=0.5,.5,z))%>% #can't have zero depth zmix
  mutate(z = ifelse(z>=3,3,z)) #in littoral zone depth zmix can not be deeper than the littoral depth

freq <- nrlmetab::calc.freq(data$datetime) # determine data frequency obs/day should be 24

data <- data %>% filter(obs>=(freq-(freq/24*2))) %>% #allow for 2 hours
  mutate(k600 = k.vachon.base(wnd = wspeed,lake.area = lake.area)) %>% #estimate K in m/day
  mutate(kgas = k600.2.kGAS.base(k600 = k600,temperature = wtemp,gas = "O2")) %>%  #m/d
  mutate(k = (kgas/freq)/z) %>% #convert gas to T^-1
  select(-kgas,-k600,-obs)

if(lake == "BWNS1") { 
  data <- data %>% 
    mutate(k = ifelse(z<3,0,k)) #We assume no DO exchange with the Atmosphere. All DO change is related to metabolism
}

ggplot(data=data,aes(x=yday,y=do_eq)) + geom_line() + facet_wrap(vars(year),scales="free_x") +
  geom_point(aes(x=yday,y=z),col="blue",size=0.2)
ggplotly()


#==========
#========== Prepare for data analysis
#==========

# prepare data
sonde_prep = data %>%
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
  mutate(unique_series = as.factor(unique_series) %>% as.numeric(),
         unique_day = paste(year, yday) %>% as.factor() %>% as.numeric(),
         index = 1:length(do),
         par_int = ifelse(par_int==0,0.00001, par_int)) %>%
  select(-i, -j) 

# return missing observations for check
sonde_check = data %>% 
  tidyr::expand(year,yday,hour) %>%
  full_join(sonde_prep) %>%
  arrange(year,yday)

ggplot(sonde_check,aes(x=datetime,y=do)) + geom_point(size=0.2) + geom_line() + facet_wrap(vars(year),scales="free_x")

sonde_check<- na.omit(sonde_check)
range(sonde_check$datetime)

###
###
###

###

# export prepared data
if(length(years) == 1) {
  sonde_check %>%
    write_csv(paste("./ModelInputs/sonde_prep_",lake,"_",years,".csv",sep =""))
} else {
  sonde_check %>%
    write_csv(paste("./ModelInputs/sonde_prep_",lake,"_",min(years),"_",max(years),".csv",sep =""))
}

#==========
#========== Package data for "f" replace "sonde_prep" with "Fcheck"
#==========

# define variables in environment 
o2_freq = freq;
o2_obs = 1000*sonde_check$do # convert to mg m^-3
o2_eq = 1000*sonde_check$do_eq # convert to mg m^-3
light = sonde_check$par_int
temp = sonde_check$wtemp
wspeed = sonde_check$wspeed
# sch_conv = sonde_prep$sch_conv
map_days = sonde_check$unique_day
k = sonde_check$k
if(length(years) == 1) {
  days_per_year = array(c({sonde_check %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value), dim = 1) #,dim = 1
} else {
  days_per_year = array(c({sonde_check %>%
      group_by(year) %>%
      summarize(value = length(unique(unique_day)))}$value)) #,dim = 1 
}
obs_per_series = array(c({sonde_check %>%
    group_by(unique_series) %>%
    summarize(value = length(unique_series))}$value)) 
obs_per_day = array(c({sonde_check %>%
    group_by(unique_day) %>%
    summarize(value = length(unique_day))}$value)) 
z = sonde_check$z
n_obs = length(o2_obs)
n_series = length(obs_per_series) 
n_days = sum(days_per_year)
n_years = length(days_per_year)



# export as .R
if(length(years)>1) {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("./ModelInputs/",lake,"_",min(years),"_",max(years),"_sonde_list.R",sep=""))
} else {
  stan_rdump(c("o2_freq","o2_obs","o2_eq","light","temp","wspeed","map_days","obs_per_series","days_per_year",
               "obs_per_day", "z","k","n_obs","n_series","n_days","n_years"),
             file=paste("./ModelInputs/",lake,"_",years,"_sonde_list.R",sep=""))
}



