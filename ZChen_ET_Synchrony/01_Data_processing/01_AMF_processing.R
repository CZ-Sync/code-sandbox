# Author: Zhaozhe Chen
# Update date: 2025.6.29

# This code conducts QC for AMF dataset
# Output SM, ET
# More descriptions here (need update)
# This is a test code, just to get hourly SM and ET for US-Ne1

# --------- Global ---------
library(dplyr)
library(lubridate)
# Input path to raw AMF dataset
setwd("D:/OneDrive - UW-Madison/Research/ET Synchrony/Data/")
# Source functions for AMF processing
source("./../Codes/Github_backup/01_Data_processing/AMF_processing_functions.R")

# Input path to raw AMF dataset
Input_path <- "./01_AMF_raw/"
# Output path for cleaned AMF hourly dataset
Output_path <- "./"

# ---------- Main ---------
gc()
# Get site info for PFT at each site
Site_info <- read.csv(paste0(Input_path,"ameriflux_site_info.csv"))

# List all dataset file names
file_names <- list.files(Input_path,pattern = "_data_",full.names = TRUE)

# Read in the raw file
# Only test at one site for now
i <- 2
AMF_raw <- read.csv(file_names[i])
# Only keep data for the target site
AMF_site <- AMF_raw %>%
  filter(site_id == "US-Ne1")
rm(AMF_raw)
# Extract SWC data
SWC <- Mean_QC_Var("SWC",AMF_site,Coarse = FALSE)
# Get SM Unit: m3/m3
SM <- SWC/100
# Get ET Unit: Convert from W/m2 to mm/day
ET <- Mean_QC_Var("LE_F",AMF_site,Coarse = FALSE) * 3600 * 24/(2.45*10^6)
ET[ET < 0] <- 0
# Get Time 
Time <- AMF_site$date_time

# Get the final df
AMF_site_cleaned <- data.frame(Time = Time,
                               SM = SM,
                               ET = ET)
# Aggregate to hourly data
df_hourly <- AMF_site_cleaned %>%
  mutate(
    # Convert character to POSIXct
    Time = ymd_hms(Time,tz="UTC"),
    # Round down to hour, in case this is half-hourly data
    Time_hour = floor_date(Time,unit="hour")) %>%
  group_by(Time_hour) %>%
  summarise(
    SM = mean(SM,na.rm=TRUE),
    ET = mean(ET,na.rm=TRUE),
    .groups = "drop"
  )

# Output this data frame
write.csv(df_hourly,paste0(Output_path,"US-Ne1_hourly_SM_ET_test.csv"))




