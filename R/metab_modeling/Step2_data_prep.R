#### Metabolism Modeling Workflow
#### February 20, 2023
#### Script created by: Noah Lottig, Facundo Scordo
#### Script edited by: Kelly Loria, Heili Lowman

# This script is designed to tidy raw data, for use in eventual
# metabolism modeling.

# Load packages.
library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
library(nrlmetab) # devtools::install_github("nrlottig/nrlmetab")
library(zoo)
library(suncalc)
library(LakeMetabolizer)
library(readxl)
library(patchwork)
library(gridExtra)
library(here)

# Load raw DO dataset.
do_raw <- read_csv("data_raw/CleanedDO/BWNS1/BWNS1_20221017.csv")

# Rename columns (dates already formatted)
do.ts <- do_raw %>% 
  select(PCT, DO) %>% 
  rename(datetime_PST = PCT, do.obs = DO)

# Create new temperature dataset as well
wtr.ts <- do_raw %>% select(PCT,Temp) %>% 
  rename(datetime_PST = PCT, wtr = Temp)

# Note - Eventually need to aggregate RBR profiles for DO and 
# water temp to account for drift

# Load in raw Synoptic climate data.
climate.raw <- readRDS("data_working/D9413_HMDC1solar_compiled_021523.rds") %>%
  rename(datetime='Date_Time.x')

head(climate.raw$datetime) # check time zone

# Can view recognized timezones in OlsonNames()

# Note - Synoptic data is in UTC, so need to convert to PST.
climate.raw$datetime_PST <- with_tz(climate.raw$datetime, 
                                    "US/Pacific")

# Create separate windspeed dataset.
wsp.ts <- climate.raw %>% 
  select(datetime_PST, wind_speed_set_1) %>% 
  rename(wspeed = 'wind_speed_set_1')

# Creater separate light dataset by multiplying PAR by 2.114.
par.ts <- climate.raw %>% 
  select(datetime_PST, solar_radiation_set_1) %>% 
  rename(par = "solar_radiation_set_1") %>% 
  mutate(par = par*2.114) %>%
  select(datetime_PST, par)

# Need to trim outliers from DO data.

# Visualize DO data.
ggplot(data = do.ts,aes(x = datetime_PST, y = do.obs)) + 
  geom_line()

describe.ts(do.ts)

# usually use 3, but the spikes might be poor.  
do.ts.clean <- trim.outliers(do.ts, width = 7, sd.dev = 3) %>%
  rename(datetime_PST = datetime)

# Plot once more w/ df's overlaying one another.
ggplot(data = do.ts, aes(x = datetime_PST, y = do.obs)) + 
  geom_line() + 
  geom_line(data = do.ts.clean,
            aes(x = datetime_PST, y = do.obs), col = "red")

do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 60)
#Picks max values during day, and min during night.
#do.ts.avg <- extract.do.data(data = do.ts.clean,time.step = 60,lat = 41.226,lon = -122.383,tz = "US/Pacific")


######
###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 7,sd.dev = 5) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)

######
### clean wind speed data
ggplot(data = wsp.ts,aes(x=datetime,y=wspeed)) + geom_line()
describe.ts(wsp.ts)
wsp.ts.clean <- trim.outliers(wsp.ts,width = 50,sd.dev = 50) # usually use 3, but the spikes might be poor.  
ggplot(data = wsp.ts,aes(x=datetime,y=wspeed)) + geom_line() + 
  geom_line(data=wsp.ts.clean,aes(x=datetime,y=wspeed),col="red")
wsp.ts.avg <- aggregate.data(data = wsp.ts.clean,time.step = 60)


### clean par data
ggplot(data = par.ts,aes(x=datetime,y=par)) + geom_line()
describe.ts(par.ts)
par.ts.clean <- trim.outliers(par.ts,width = 50,sd.dev = 50) # usually use 3, but the spikes might be poor.  
ggplot(data = par.ts,aes(x=datetime,y=par)) + geom_line() + 
  geom_line(data=par.ts.clean,aes(x=datetime,y=par),col="red")
par.ts.avg <- aggregate.data(data = par.ts.clean,time.step = 60)


#Join the drift dataset and Calcualte do_eq and do_sat

dat <- do.ts.avg %>% #note in this case I am not using the drift correction -KL
  full_join(wtr.ts.avg) %>% 
  mutate(year=year(datetime), 
         yday=yday(datetime),
         hour=hour(datetime)+1) %>% 
  mutate(do_eq=o2.at.sat.base(temp = wtr,altitude = 1897)) %>%  # Tahoe altitude in m  = 1897, we barometeric 
  mutate(o2_sat=do.obs/do_eq) 
# %>% 
#   full_join(climate)


dat2 <- dat %>% #note in this case I am not using the drift correction -KL
  full_join(par.ts.avg) %>% 
  mutate(year=year(datetime), 
         yday=yday(datetime),
         hour=hour(datetime)+1) 

dat3 <- dat2 %>% #note in this case I am not using the drift correction -KL
  full_join(wsp.ts.avg) %>% 
  mutate(year=year(datetime), 
         yday=yday(datetime),
         hour=hour(datetime)+1) 


#convert windspeed to 10m wind (required for gas exchange models per LakeMetabolizer)
dat <- dat3 %>% mutate(wspeed=wind.scale.base(wspeed,wnd.z=50.9)) # z check for the weather station for altitude 6.1 


####
#Calculating par_int base on extinction coefficents, PAR and zmix (in this case I am using
# 2 in line 151 instead of z which is the variable zmix from dat)

extcoef <- read_csv("./CleanDat/Rose2009extcoef.csv") 

extcoef <- extcoef %>% 
  mutate(year = year(datetime),yday=yday(datetime)) %>% 
  select(year,yday,extcoef) 

#date_matrix <- extcoef %>% expand(year, yday=full_seq(yday,1)) 

date_matrix22 <- extcoef %>% tidyr::expand(year, yday=full_seq(yday,1)) %>%
  subset(year==2022)

extcoef22 <- date_matrix22 %>% left_join(extcoef) 
#These lines are the one that do the linear extrapolation
extcoefb <- extcoef22 %>%
  mutate(
    extcoef=na.approx(extcoef))

ggplot(data = extcoefb ,aes(x=yday,y=extcoef))+geom_point()+scale_y_reverse()


extcoef <- extcoefb %>% 
  select(yday,extcoef) 

dat <- dat %>% 
  full_join(extcoef) %>% 
  mutate(par_int = round((par - par*exp(-extcoef*3))/(extcoef*3),digits=0)) %>% # multipler = "(extcoef*3)" should be depth of water column
  select(-extcoef)


range(dat$datetime)
dat <- dat %>% 
  rename(do = do.obs, wtemp= wtr) 

ggplot(data = dat,aes(x=datetime,y=par_int)) + geom_point()
ggplot(data = dat,aes(x=datetime,y=do)) + geom_point()

dat <- na.omit(dat)
range(dat$datetime)

# dat2 <- dat[!duplicated(dat),]

# write.table(x = dat, file = "./FinalInputs/BWNS1Inputs.txt", row.names = TRUE)
# write.csv(x = dat, file = "./FinalInputs/BWNS1Inputs.csv", row.names = TRUE)
