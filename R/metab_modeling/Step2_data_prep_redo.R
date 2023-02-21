# Not commenting much of this since ran this morning...

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
library(here)

do_raw <- read_csv("data_raw/CleanedDO/BWNS1/BWNS1_20221017.csv")

do.ts <- do_raw %>% 
  select(PCT, DO) %>% 
  rename(datetime_PST = PCT, do.obs = DO)

wtr.ts <- do_raw %>% 
  select(PCT,Temp) %>% 
  rename(datetime_PST = PCT, wtr = Temp)

climate.raw <- readRDS(
  "data_working/D9413_HMDC1solar_compiled_022023.rds") %>%
  rename(datetime='Date_Time.x')  # in UTC currently

climate.raw$datetime_PST <- with_tz(climate.raw$datetime, "US/Pacific")

wsp.ts <- climate.raw %>% 
  select(datetime_PST, wind_speed_set_1) %>% 
  rename(wspeed='wind_speed_set_1')

# mutate by multiplying 2.114

par.ts <- climate.raw %>% 
  select(datetime_PST, solar_radiation_set_1) %>% 
  rename(par="solar_radiation_set_1") %>% 
  mutate(par= par*2.114) %>%
  select(datetime_PST, par)

######
###clean DO data

ggplot(data = do.ts,aes(x = datetime_PST, y = do.obs)) + 
  geom_line()

describe.ts(do.ts)

do.ts.clean <- trim.outliers(do.ts,
                             width = 7,
                             sd.dev = 3)  %>%
  rename(datetime_PST = datetime)
# usually use 3, but the spikes might be poor.

ggplot(data = do.ts, aes(x = datetime_PST, y = do.obs)) + 
  geom_line() + 
  geom_line(data = do.ts.clean, 
            aes(x = datetime_PST, y = do.obs), col="red")

#### NOTES/FORMATTING PICK UP HERE ####
# Aggregate data by the hour.
do.ts.avg <- aggregate.data(data = do.ts.clean,
                            time.step = 60) %>%
  rename(datetime_PST = datetime)

# Next, tidy temperature data using similar workflow as above.
# Visualize.
ggplot(data = wtr.ts,aes(x = datetime_PST, y = wtr)) + 
  geom_line()

describe.ts(wtr.ts)

# Trim outliers.
wtr.ts.clean <- trim.outliers(wtr.ts,
                              width = 7,
                              sd.dev = 5) %>%
  rename(datetime_PST = datetime)

# Visualize once more.
ggplot(data = wtr.ts, aes(x = datetime_PST, y = wtr)) + 
  geom_line() + 
  geom_line(data = wtr.ts.clean, aes(x = datetime_PST, y = wtr), 
            col="red")

# Aggregate to the nearest hour.
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean, time.step = 60) %>%
  rename(datetime_PST = datetime)

# Then, tidy wind data using the same workflow.
# Visualize.
ggplot(data = wsp.ts, aes(x = datetime_PST, y = wspeed)) + 
  geom_line()

describe.ts(wsp.ts)

# Trim outliers.
wsp.ts.clean <- trim.outliers(wsp.ts, width = 50, sd.dev = 50) %>%
  rename(datetime_PST = datetime)

# Visualize again.
ggplot(data = wsp.ts, aes(x = datetime_PST, y = wspeed)) + 
  geom_line() + 
  geom_line(data = wsp.ts.clean, aes(x = datetime_PST, y = wspeed),
            col="red")

# Aggregate to the nearest hour.
wsp.ts.avg <- aggregate.data(data = wsp.ts.clean, time.step = 60) %>%
  rename(datetime_PST = datetime)

# And finally, tidy the PAR dataset using workflow from above.
# Visualize.
ggplot(data = par.ts, aes(x = datetime_PST, y = par)) + 
  geom_line()

describe.ts(par.ts)

# Trim outliers.
par.ts.clean <- trim.outliers(par.ts,
                              width = 50,
                              sd.dev = 50) %>%
  rename(datetime_PST = datetime)

# Visualize again.
ggplot(data = par.ts, aes(x = datetime_PST, y = par)) + 
  geom_line() + 
  geom_line(data = par.ts.clean, aes(x = datetime_PST, y = par),
            col="red")

# Aggregate to the nearest hour.
par.ts.avg <- aggregate.data(data = par.ts.clean, time.step = 60) %>%
  rename(datetime_PST = datetime)

# Calculate do_eq and do_sat.
dat <- do.ts.avg %>% # no drift correction at the moment.
  left_join(wtr.ts.avg) %>% # join with temperature
  mutate(year = year(datetime_PST), 
         yday = yday(datetime_PST),
         hour = hour(datetime_PST)) %>% 
  # o2.at.sat.base() from lakeMetabolizer
  mutate(do_eq = o2.at.sat.base(temp = wtr,altitude = 1897)) %>%  
  # Tahoe altitude in m  = 1897
  mutate(o2_sat = do.obs/do_eq) 

dat2 <- dat %>% 
  left_join(par.ts.avg) # join above with light data

dat3 <- dat2 %>% 
  left_join(wsp.ts.avg) # join above with windspeed data

# Convert windspeed to 10m wind (required for gas exchange models 
# per LakeMetabolizer).
dat3$wspeed10m <- wind.scale.base(dat3$wspeed, wnd.z = 50.9) 
# z check for the weather station for altitude 6.1

# Calculating par_int based on extinction coefficents, PAR and zmix 
# (in this case I am using 2 in line 151 instead of z which is the
# variable zmix from dat).

# Load in experimental extinction coefficient values.
extcoef <- read_csv("data_raw/Rose2009extcoef.csv") 

extcoef <- extcoef %>% 
  mutate(year = year(datetime),yday = yday(datetime)) %>% 
  select(year, yday, extcoef) 

# Make rows for all days of the year.
date_matrix <- extcoef %>% 
  expand(year, yday = full_seq(yday, 1))

extcoef2 <- date_matrix %>% left_join(extcoef) 

# Perform linear extrapolation.
extcoefb <- extcoef2 %>%
  mutate(extcoef = na.approx(extcoef))

# Visualize to see how it looks.
ggplot(data = extcoefb, aes(x = yday, y = extcoef)) +
  geom_point() +
  scale_y_reverse()

extcoef_final <- extcoefb %>% 
  select(yday, extcoef) 

# Join with full dataset.
dat_full <- dat3 %>% 
  full_join(extcoef_final) %>% 
# Note, since I don't currently have data for the full year, I am
  # using 0.08 for all dates prior to June 21 and after Sept 28
  mutate(par_int = case_when(extcoef > 0 ~ round((par - par*exp(-extcoef*3))/(extcoef*3),
                                                 digits = 0),
                             TRUE ~ round((par - par*exp(-0.08*3))/(0.08*3),
                                          digits = 0)))
# multiplier = "(extcoef*3)" should be depth of water column

range(dat_full$datetime_PST) # May through October

dat_full <- dat_full %>% 
  rename(do = do.obs, wtemp= wtr) 

# Take one last look at the data.
ggplot(data = dat_full, aes(x = datetime_PST, y = par_int)) + 
  geom_point()

ggplot(data = dat_full, aes(x = datetime_PST, y = do)) + 
  geom_point()

# Export datasets.
write.table(x = dat_full, 
            file = "data_working/BWNS1Inputs.txt", 
            row.names = TRUE)
write_csv(x = dat_full, 
          file = "data_working/BWNS1Inputs.csv")

# End of script.
