#### Oxygen Saturation Conversion
### October 31, 2022
## Heili Lowman

# The following script will convert DO data as it's downloaded from the sensors
# (mg/L) to oxygen saturation.

#### Setup ####

# Load packages
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)
library(webshot)
library(htmlwidgets)
library(patchwork)

# Load assembled and trimmed data
tahoe_all <- read_csv("data_working/Tahoe_compiled_trimmed_110322.csv")

# Load Blackwood barometric pressure data.
# Source: Synoptic Station D9413 (Homewood)
# Excess rows of metadata trimmed.
bw_weather <- read_csv("data_raw/SynopticDownloads/D9413.2022-10-31_tidy.csv")
bw_weather$site <- "Blackwood"
bw_trim <- bw_weather %>%
  select(Station_ID, Date_Time, pressure_set_1d, site)

# Load Glenbrook barometric pressure data.
# Source: Synoptic Station F9917 (Glenbrook)
# Excess rows of metadata trimmed.
gb_weather <- read_csv("data_raw/SynopticDownloads/F9917.2022-10-31_tidy.csv")
gb_weather$site <- "Glenbrook"
gb_trim <- gb_weather %>%
  select(Station_ID, Date_Time, pressure_set_1d, site)

# Bind weather data
both_weather <- rbind(bw_trim, gb_trim)

#### Formatting ####

# Need to first convert bp and then aggregate data by 15 minutes so 
# timestamps match up.
both_weather <- both_weather %>%
  mutate(pressure_mm_Hg = pressure_set_1d*0.00750062,
         round15 = ymd_hms(cut(Date_Time, breaks = "15 min")))

weather_15 <- both_weather %>%
  group_by(site, round15) %>%
  summarize(meanP_Pascal = mean(pressure_set_1d, na.rm = TRUE),
            meanP_mmHg = mean(pressure_mm_Hg, na.rm = TRUE)) %>%
  ungroup()
  
tahoe_all <- tahoe_all %>%
  mutate(by15 = cut(date_time, breaks = "15 min"))

tahoe_15 <- tahoe_all %>%
  group_by(site, depth, location, sensor, by15) %>%
  summarize(meanDO = mean(DO_mgL, na.rm = TRUE),
            meanT = mean(Temp_C, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date_time15 = ymd_hms(by15))

# Trim down to columns of interest
tahoe_15_trim <- tahoe_15 %>%
  select(date_time15, meanDO, meanT, sensor, location, depth, site) %>%
  rename("round15" = "date_time15")

# And join with appropriate datasets.
tahoe_15_full <- left_join(tahoe_15_trim, weather_15, by = c("site", "round15"))

#### Calculate O Saturation ####

# Calculation from Garcia and Gordon (1992) for oxygen saturation (Osat).
Osat_calc <- function(temp, bp){
  
  satO <-(exp(2.00907 + 3.22014 * (log((298.15-temp) / (273.15 + temp))) + 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5)) * 1.4276 * bp / 760
  
 satO
  
}

# Calculate using the function above.
tahoe_15_full <- tahoe_15_full %>%
  mutate(Osat = Osat_calc(meanT, meanP_mmHg))

# Calculate percentage.
tahoe_15_full <- tahoe_15_full %>%
  mutate(DOsat_perc = (meanDO/Osat)*100)

#### Plot ####

# Create a version of the dataset aggregated by hour to smooth the lines a bit.
tahoe_h <- tahoe_15_full %>%
  group_by(site, depth, location, sensor,
           roundhour = lubridate::floor_date(round15, "1 hour")) %>%
  summarize(DO_mgL_mean = mean(meanDO, na.rm = TRUE),
            Temp_C_mean = mean(meanT, na.rm = TRUE),
            Press_P_mean = mean(meanP_Pascal, na.rm = TRUE),
            Press_mmHg_mean = mean(meanP_mmHg, na.rm = TRUE),
            Osat_mgL_mean = mean(Osat, na.rm = TRUE),
            DOsat_perc_mean = mean(DOsat_perc, na.rm = TRUE)) %>%
  ungroup()

# Plots to explore initial DO saturation values.
# 3m sites at Blackwood
(bw_dosat_fig_ns <- ggplot(tahoe_h %>% 
                          filter(site == "Blackwood") %>%
                          filter(depth == 3),
                        aes(x = roundhour, y = DOsat_perc_mean)) +
   geom_line(aes(color = factor(sensor))) +
   scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
   labs(x = "Date",
        y = "DO Saturation (%)",
        color = "Sensor Location",
        title = "Blackwood Nearshore (3m) Sensors") +
   theme_bw() +
   scale_x_datetime(date_breaks = "3 months"))

(bw_dosat_plotly_ns <- ggplotly(bw_dosat_fig_ns))

# Remaining sites at Blackwood
(bw_dosat_fig <- ggplot(tahoe_h %>% 
                       filter(site == "Blackwood") %>%
                       filter(depth != 3),
                     aes(x = roundhour, y = DOsat_perc_mean)) +
    geom_line(aes(color = factor(depth),
                  linetype = factor(location))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO Saturation (%)",
         color = "Water Depth",
         linetype = "Sensor Depth",
         title = "Blackwood Offshore Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(bw_dosat_plotly <- ggplotly(bw_dosat_fig))

# Export static paneled plot of Blackwood sites.
(bwsat_static <- bw_dosat_fig_ns + bw_dosat_fig +
    plot_annotation(tag_levels = "A") +
    plot_layout(nrow = 2))

# ggsave("figures/BW_DOsat_compiled_110322.png",
#        width = 40,
#        height = 30,
#        units = "cm")

# Export plotly plots for further exploration.
# saveWidget(as_widget(bw_dosat_plotly_ns), "plotly/BW_3m_DOsat_103122.html")
# saveWidget(as_widget(bw_dosat_plotly), "plotly/BW_to20m_DOsat_103122.html")

# 3m sites at Glenbrook
(gb_dosat_fig_ns <- ggplot(tahoe_h %>% 
                          filter(site == "Glenbrook") %>%
                          filter(depth == 3),
                        aes(x = roundhour, y = DOsat_perc_mean)) +
    geom_line(aes(color = factor(sensor))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO Saturation (%)",
         color = "Sensor Location",
         title = "Glenbrook Nearshore (3m) Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(gb_dosat_plotly_ns <- ggplotly(gb_dosat_fig_ns))

# Remaining sites at Glenbrook
(gb_dosat_fig <- ggplot(tahoe_h %>% 
                       filter(site == "Glenbrook") %>%
                       filter(depth != 3),
                     aes(x = roundhour, y = DOsat_perc_mean)) +
    geom_line(aes(color = factor(depth),
                  linetype = factor(location))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO Saturation (%)",
         color = "Water Depth",
         linetype = "Sensor Depth",
         title = "Glenbrook Offshore Sensors") +
    theme_bw() +
    scale_x_datetime(date_breaks = "3 months"))

(gb_dosat_plotly <- ggplotly(gb_dosat_fig))

# Export static paneled plot of Glenbrook sites.
(gbsat_static <- gb_dosat_fig_ns + gb_dosat_fig +
    plot_annotation(tag_levels = "A") +
    plot_layout(nrow = 2))

# ggsave("figures/GB_DOsat_compiled_110322.png",
#        width = 40,
#        height = 30,
#        units = "cm")

# Export plotly plots for further exploration.
# saveWidget(as_widget(gb_dosat_plotly_ns), "plotly/GB_3m_DOsat_110322.html")
# saveWidget(as_widget(gb_dosat_plotly), "plotly/GB_to20m_DOsat_110322.html")

# End of script.
