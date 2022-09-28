#### Initial Data Exploration
### September 26, 2022
## Heili Lowman

# The following script will accomplish some early data exploration ahead of the
# first all-hands meeting in late-September.

# NOTE - any plotly plots need to be prevented from being uploaded
# to GitHub because they are enormous.

# Load packages
library(here)
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)
library(webshot)
library(htmlwidgets)
library(patchwork)

# Need to first test out how to load this format of text data files.

# List of column names
mylist <- c("Time_Sec", "BV_Volts", "Temp_C", "DO_mgL", "Q")

# Load data
test_txt <- read_csv("data_raw/2021-10-28 221500Z.txt", skip = 3, col_names = mylist)

# Add file name to be turned into date
test_txt <- test_txt %>%
  mutate(txtfile = "2021-10-28 221500z.txt") %>%
  mutate(date = str_split_fixed(txtfile, pattern = " ", n = 2)) %>%
  mutate(Date = ymd(date[,1]))

# Now to try and iterate this over multiple files

# Create function to add filename and, by extension, date to each file as it is
# imported.
# Based on code found at :stackoverflow.com/questions/11433432/how-to-import-
# multiple-csv-files-at-once
read_plus_date <- function(flnm){
  
  read_csv(flnm, skip = 3, col_names = mylist) %>%
    mutate(filename = flnm) %>%
    mutate(folder = str_split_fixed(filename, pattern = "/", n = 10)) %>%
    mutate(date = str_split_fixed(folder[,10], pattern = " ", n = 2)) %>%
    mutate(Date = ymd(date[,1]))
  
}

# Figure out which directory to specify
# Note - initially got this working with a folder of n = 5
test2 <- read_plus_date("data_raw/BuoyDownloads/GB15m/7450-686243/2021-10-28 221500z.txt")

#### GB15m ####
# Load in all files from the GB15m directory
tbl_with_sources <- list.files(path = here("data_raw/BuoyDownloads/GB15m/7450-686243"),
                               pattern = "*.txt",
                               full.names = T) %>%
  map_df(~read_plus_date(.))

# Convert 24 character to full date/time
tbl_with_sources$date_time <- as_datetime(tbl_with_sources$Time_Sec)

# GAH so I didn't even need the above lines re: folder/filenames. *sigh*

# Quick plot just before I move on

(ggplot(tbl_with_sources, aes(x = date_time, y = DO_mgL)) +
    geom_line() +
    labs(x = "Date", y = "DO (mg/L)") +
    theme_bw())

(ggplot(tbl_with_sources, aes(x = date_time, y = Temp_C)) +
    geom_line() +
    labs(x = "Date", y = "Temperature (C)") +
    theme_bw())

# Trim and export data.
gb15 <- tbl_with_sources %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(gb15, "data_working/GB15m_compiled_092622.csv")

#### GB10m ####
# Load in all files from the GB10m directory
tbl_with_sources10 <- list.files(path = here("data_raw/BuoyDownloads/GB10m/20220428/7450-193411"),
                               pattern = "*.txt",
                               full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10$date_time <- as_datetime(tbl_with_sources10$Time_Sec)

# Trim and export data.
gb10 <- tbl_with_sources10 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(gb10, "data_working/GB10m_compiled_092622.csv")

#### GB3m ####
# Load in all files from the GBNS2 directory
tbl_with_sources3 <- list.files(path = here("data_raw/BuoyDownloads/GBNS2/20220523/7450-224208"),
                                 pattern = "*.txt",
                                 full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3$date_time <- as_datetime(tbl_with_sources3$Time_Sec)

# Trim and export data.
gb3 <- tbl_with_sources3 %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(gb3, "data_working/GBNS2_compiled_092622.csv")

#### BW20m ####
# Load in all files from the BW20m directory
tbl_with_sources20bw1 <- list.files(path = here("data_raw/BuoyDownloads/BW20m/20220324/7450-336792"),
                                pattern = "*.txt",
                                full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

tbl_with_sources20bw2 <- list.files(path = here("data_raw/BuoyDownloads/BW20m/20220715/benthic/7450-547404"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

tbl_with_sources20bw <- rbind(tbl_with_sources20bw1, tbl_with_sources20bw2)

# Convert 24 character to full date/time
tbl_with_sources20bw$date_time <- as_datetime(tbl_with_sources20bw$Time_Sec)

# Trim and export data.
bw20 <- tbl_with_sources20bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw20, "data_working/BW20m_compiled_092622.csv")

#### BW15m ####
# Load in all files from the BW15m directory
tbl_with_sources15bw <- list.files(path = here("data_raw/BuoyDownloads/BW15m/20220715/7450-666671"),
                                    pattern = "*.txt",
                                    full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources15bw$date_time <- as_datetime(tbl_with_sources15bw$Time_Sec)

# Trim and export data.
bw15 <- tbl_with_sources15bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw15, "data_working/BW15m_compiled_092622.csv")

#### BW10m ####
# Load in all files from the BW10m directory
tbl_with_sources10bw <- list.files(path = here("data_raw/BuoyDownloads/BW10m/20220715/7450-617000"),
                                   pattern = "*.txt",
                                   full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources10bw$date_time <- as_datetime(tbl_with_sources10bw$Time_Sec)

# Trim and export data.
bw10 <- tbl_with_sources10bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw10, "data_working/BW10m_compiled_092622.csv")

#### BW3m ####
# Load in all files from the BWNS2 directory
tbl_with_sources3bw <- list.files(path = here("data_raw/BuoyDownloads/BWNS2/20220524/7450-287080"),
                                   pattern = "*.txt",
                                   full.names = T) %>%
  map_df(~read_csv(., skip = 3, col_names = mylist))

# Convert 24 character to full date/time
tbl_with_sources3bw$date_time <- as_datetime(tbl_with_sources3bw$Time_Sec)

# Trim and export data.
bw3 <- tbl_with_sources3bw %>%
  select(date_time, BV_Volts, Temp_C, DO_mgL, Q)

write_csv(bw3, "data_working/BWNS2_compiled_092622.csv")

#### Join and Plot ####

# Add a column to demarcate water depth.
gb3$depth <- 3
gb10$depth <- 10
gb15$depth <- 15
# Join all Glenbrook datasets together.
gb_j1 <- rbind(gb3, gb10)
gb_all <- rbind(gb_j1, gb15)
# Add a column to demarcate site.
gb_all$site <- "Glenbrook"

# Add a column to demarcate water depth.
bw3$depth <- 3
bw10$depth <- 10
bw15$depth <- 15
bw20$depth <- 20
# Join all Blackwood datasets together.
bw_j1 <- rbind(bw3, bw10)
bw_j2 <- rbind(bw_j1, bw15)
bw_all <- rbind(bw_j2, bw20)
# Add a column to demarcate site.
bw_all$site <- "Blackwood"

# Join everythinggg
tahoe_all <- rbind(gb_all, bw_all)

# Export for future use.
write_csv(tahoe_all, "data_working/Tahoe_compiled_092622.csv")

# Plots for all-hands meeting September 28, 2022.
(bw_do_fig <- ggplot(tahoe_all %>% 
                       filter(site == "Blackwood") %>%
                       filter(date_time > "2022-03-25 00:00:00"),
                    aes(x = date_time, y = DO_mgL)) +
  geom_line(aes(color = factor(depth))) +
  scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184", "#3793EC")) +
  labs(x = "Date",
       y = "DO (mg/L)",
       color = "Water Depth (m)") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 day"))

(bw_do_plotly <- ggplotly(bw_do_fig))

(bw_temp_fig <- ggplot(tahoe_all %>% 
                       filter(site == "Blackwood") %>%
                       filter(date_time > "2022-03-25 00:00:00"),
                     aes(x = date_time, y = Temp_C)) +
    geom_line(aes(color = factor(depth))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184", "#3793EC")) +
    labs(x = "Date",
         y = "Temperature (C)",
         color = "Water Depth (m)") +
    theme_bw() +
    scale_x_datetime(date_breaks = "1 day"))

(bw_temp_plotly <- ggplotly(bw_temp_fig))

(gb_do_fig <- ggplot(tahoe_all %>% 
                       filter(site == "Glenbrook") %>%
                       filter(date_time > "2021-11-01 00:00:00") %>%
                       filter(date_time < "2022-04-15 00:00:00"),
                     aes(x = date_time, y = DO_mgL)) +
    geom_line(aes(color = factor(depth))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184")) +
    labs(x = "Date",
         y = "DO (mg/L)",
         color = "Water Depth (m)") +
    theme_bw() +
    scale_x_datetime(date_breaks = "1 day"))

(gb_do_plotly <- ggplotly(gb_do_fig))

(gb_temp_fig <- ggplot(tahoe_all %>% 
                         filter(site == "Glenbrook") %>%
                         filter(date_time > "2021-11-01 00:00:00") %>%
                         filter(date_time < "2022-04-15 00:00:00"),
                       aes(x = date_time, y = Temp_C)) +
    geom_line(aes(color = factor(depth))) +
    scale_color_manual(values = c("#CECEB9", "#7AC9B7", "#6CA184", "#3793EC")) +
    labs(x = "Date",
         y = "Temperature (C)",
         color = "Water Depth (m)") +
    theme_bw()+
    scale_x_datetime(date_breaks = "1 day"))

(gb_temp_plotly <- ggplotly(gb_temp_fig))

# Export static Blackwood plot for powerpoint.
(bw_both <- bw_do_fig / bw_temp_fig)

# ggsave("figures/BW_compiled_092622.png",
#        width = 30,
#        height = 20,
#        units = "cm")

# And Glenbrook plot.
(gb_both <- gb_do_fig / gb_temp_fig)

# ggsave("figures/GB_compiled_092622.png",
#        width = 35,
#        height = 20,
#        units = "cm")

# Export plotly plots for further exploration.
saveWidget(as_widget(bw_do_plotly), "plotly/BW_DO_092722.html")
saveWidget(as_widget(bw_temp_plotly), "plotly/BW_Temp_092722.html")
saveWidget(as_widget(gb_do_plotly), "plotly/GB_DO_092722.html")
saveWidget(as_widget(gb_temp_plotly), "plotly/GB_Temp_092722.html")

# End of script.
