#### Uptake Rate Calculation Workflow
#### June 22, 2023
#### Script created by: Heili Lowman

# This script is designed to edit raw N incubation data and generate
# a new datasheet of uptake rates.

### Setup ####

# Load packages.
library(tidyverse)
library(lubridate)
library(here)

# Load raw datasets.
dat_raw <- read_csv("data_raw/N_incubations/N_Incubation_Metadata_062223_Maysheet.csv")

# Check data structure.
str(dat_raw)




# End of script.
