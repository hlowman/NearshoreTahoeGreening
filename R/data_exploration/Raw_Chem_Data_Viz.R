#### Initial Nutrient Data Exploration
### March 7, 2023
## Heili Lowman

# The following script will read in all chenistry data currently available
# on the Google Drive and compile make some initial plots.

# Load packages.
library(tidyverse)
library(here)
library(calecopal)

# Load dataset.
nut_dat <- read_csv("data_raw/Water_chemistry_record_030723.csv")

# Check the structure of the dataset.
str(nut_dat)

# Need to coerce a few columns to be numeric.
nut_dat$`AQ400_NO2_NO3_H2O in mg/L` <- as.numeric(nut_dat$`AQ400_NO2_NO3_H2O in mg/L`)

nut_dat$`AQ400 o-Phosphate_water ug/L` <- as.numeric(nut_dat$`AQ400 o-Phosphate_water ug/L`)

# And need to reformat so easier plotting occurs.
nut_dat <- nut_dat %>%
  mutate(NO2_NO3_mgL = case_when(is.na(AQ400_NO2_NO3_rerun) == FALSE ~
                                   AQ400_NO2_NO3_rerun,
                                 TRUE ~ `AQ400_NO2_NO3_H2O in mg/L`),
         OP_ugL = case_when(is.na(`ophos temp`) == FALSE ~ `ophos temp`,
                            TRUE ~ `AQ400 o-Phosphate_water ug/L`),
         NH4_mgL = case_when(is.na(`NH4 rerun`) == FALSE ~ `NH4 rerun`,
                             TRUE ~ `AQ400 NH4 mg/L`),
         DOC_mgL = `Shimadzu DOC mgL`,
         TN_ugL = `Shimadazu TN ugL`,
         TC_ugL = `shimadzu TC`,
         Location = case_when(Site %in% c("Blackwood", "Blackwood lower",
                                          "Blackwood lower HOR", "Blackwood upper",
                                          "Blackwood 0.5m", "Blackwood outlet") ~
                                "Blackwood",
                              Site %in% c("Glenbrook Low", "Glenbrook Up",
                                          "Glenbrook lower", "Glenbrook upper",
                                          "GBL", "GBU") ~ "Glenbrook",
                              Site %in% c("Incline", "Incline upper HOR") ~ "Incline",
                              TRUE ~ Site))

# Quick plots
(fig_no3 <- ggplot(nut_dat, aes(x = Date, y = NO2_NO3_mgL)) +
    geom_point(size = 3, aes(color = Location)) +
    scale_color_manual(values = cal_palette("figmtn")) +
    theme_bw())

# There's lots of overlay so here's on summarizing by both location and day.
(fig_no3 <- ggplot(nut_dat %>%
                     drop_na(Location) %>%
                     group_by(Location, Date) %>%
                     summarize(mean_NO2_NO3_mgL = mean(NO2_NO3_mgL)) %>%
                     ungroup(), 
                   aes(x = Date, y = mean_NO2_NO3_mgL)) +
    geom_point(size = 3, aes(color = Location)) +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_wrap(.~Location) +
    theme_bw() +
    theme(legend.position = "none"))

# ggsave(fig_no3,
#        filename = "figures/Chem_NO3_030723.jpg",
#        width = 25,
#        height = 10,
#        units = "cm")

# Ammonium figure.
(fig_nh4 <- ggplot(nut_dat %>%
                     drop_na(Location) %>%
                     group_by(Location, Date) %>%
                     summarize(mean_NH4_mgL = mean(NH4_mgL)) %>%
                     ungroup(), 
                   aes(x = Date, y = mean_NH4_mgL)) +
    geom_point(size = 3, aes(color = Location)) +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_wrap(.~Location) +
    theme_bw() +
    theme(legend.position = "none"))

# ggsave(fig_nh4,
#        filename = "figures/Chem_NH4_030723.jpg",
#        width = 25,
#        height = 10,
#        units = "cm")

# DOC figure.
(fig_doc <- ggplot(nut_dat %>%
                     drop_na(Location) %>%
                     group_by(Location, Date) %>%
                     summarize(mean_DOC_mgL = mean(DOC_mgL)) %>%
                     ungroup(), 
                   aes(x = Date, y = mean_DOC_mgL)) +
    geom_point(size = 3, aes(color = Location)) +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_wrap(.~Location) +
    theme_bw() +
    theme(legend.position = "none"))

# ggsave(fig_doc,
#        filename = "figures/Chem_DOC_030723.jpg",
#        width = 25,
#        height = 10,
#        units = "cm")

# Ortho-phosphate figure.
(fig_op <- ggplot(nut_dat %>%
                     drop_na(Location) %>%
                     group_by(Location, Date) %>%
                     summarize(mean_OP_ugL = mean(OP_ugL)) %>%
                     ungroup(), 
                   aes(x = Date, y = mean_OP_ugL)) +
    geom_point(size = 3, aes(color = Location)) +
    scale_color_manual(values = cal_palette("figmtn")) +
    facet_wrap(.~Location) +
    theme_bw() +
    theme(legend.position = "none"))

# ggsave(fig_doc,
#        filename = "figures/Chem_OP_030723.jpg",
#        width = 25,
#        height = 10,
#        units = "cm")

# End of script.
