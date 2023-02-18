library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
library(nrlmetab)
library(zoo)
library(suncalc)
library(LakeMetabolizer)
library(readxl)
library(patchwork)
library(gridExtra)

col_names <- names(read.csv("./CleanDat/AggregatedDO/BWNS1.csv",
                              header=T, sep = ','))

do_raw <- read.delim("./CleanDat/AggregatedDO/BWNS1.csv",
                     header=T, sep = ',')

colnames(do_raw)<- col_names

do.ts <- do_raw %>% select(timestamp, DO) %>% 
  rename(datetime = timestamp, do.obs = DO) %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))

wtr.ts <- do_raw %>% select(timestamp,Temp) %>% 
  rename(datetime = timestamp, wtr = Temp) %>%
  mutate(datetime = as.POSIXct((datetime), format ="%Y-%m-%d %H:%M:%S"))


## Need to aggregate RBR profiles for DO and water temp-- (eventually use for drift)
# profile <- prof_BWNS1 %>% # out of deployment date range for prelim data # also F thinks this RBR is wrong
#   select(datetime, do_obs, wtemp) %>%
#   rename(doobs_3.0= 'do_obs', wtr_3.0 = 'wtemp')

climate.raw <- read_csv("./CleanDat/Climate/WestNSclimate.csv") %>%
  rename(datetime='Date_Time') 

#climate.raw$datetime<- round(as.POSIXct(climate.raw$datetime, format="%H:%M:%S", tz="UTC"), units="hours")
climate.raw$datetime <- climate.raw$datetime - hours(6) # datetinme no longer in UTC

wsp.ts <- climate.raw %>% select(datetime,wind_speed_set_1) %>% 
  rename(wspeed='wind_speed_set_1') %>% 
  select(datetime,wspeed) # datetime in UTC


# mutate by multiplying 2.114

par.ts <- climate.raw %>% select(datetime,solar_radiation_set_1) %>% 
  rename(par="solar_radiation_set_1") %>% 
  mutate(par= par*2.114) %>%
  select(datetime,par)

##



# EEK matbe where things went poorly   
#climate$datetime <-round(as.POSIXct(climate$datetime, format="%H:%M:%S", tz="UTC"), units="hours")
#climate$datetime <- climate$datetime - hours(6) # datetinme no longer in UTC

######
###clean DO data

ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 7,sd.dev = 3) # usually use 3, but the spikes might be poor.  
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
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
