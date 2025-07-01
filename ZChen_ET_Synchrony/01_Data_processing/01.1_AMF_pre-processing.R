# Author: Zhaozhe Chen
# Update date: 2025.6.30

# This code conducts QC for raw sub-daily scale AMF dataset
# The raw AMF dataset is from https://drive.google.com/drive/folders/1SagFhGwxrJMRygiENTWesFhSd64eJSfq
# Output include a series of variables of interest
# For some variables, there may be multiple sub-variables in the dataset, the average of them was used for each unique variable
# All half-hourly data was aggregated to hourly
# The variables were output as in their original units.
# Outputs 5 files, corresponding to the original 5 files

# --------- Global ---------
library(here)
library(dplyr)
library(lubridate)

# Input path to AMF dataset
Data_path <- "D:/OneDrive - UW-Madison/Research/ET Synchrony/Data/"
# Source functions for AMF processing
source(here("01_Data_processing","AMF_processing_functions.R"))

# This is a list of required variables for output
var_ls <- c("SWC", # Soil Water Content, Unit: %
            "TA", # Air temperature, Unit: degree C
            "VPD", # Vapor pressure deficit, Unit: hPa
            "P_F", # Precipitation (to be separated with pressure), Unit: mm
            "WS", # Wind speed, Unit: ms-1
            "USTAR", # Friction velocity, Unit: ms-1
            "NETRAD", # Net radiation, Unit: Wm-2
            "TS", # Soil temperature, Unit: degree C
            "LE_F" # Latent heat flux, Unit: Wm-2
            )

# ---------- Main ---------
# The raw hourly dataset is divided into 5 files for all sites
# Loop over the 5 files
for(arrayid in 1:5){
  gc()
  # Get file names for the 5 files
  file_names <- list.files(paste0(Data_path,"01_AMF_raw"),pattern = "_data_",full.names = TRUE)
  # Read in raw AMF dataset of one file
  AMF_raw <- read.csv(file_names[arrayid])
  # For each required variable, QC all sub-variables for them, and get the average of all sub-variables for each variable
  AMF_df <- Var_QC_all(var_ls,AMF_raw)
  rm(AMF_raw)
  gc()
  # Aggregate all data to hourly data (some AMF sites have half-hourly data)
  AMF_df <- AMF_df %>%
    mutate(Time_hour = floor_date(Time,unit="hour")) %>%
    group_by(Time_hour,Site_ID) %>%
    summarise(across(where(is.numeric),~ mean(.x,na.rm=TRUE)),
              .groups = "drop") %>%
    rename(Time = Time_hour)
  
  # If there is only one variable that is not NA at each time, remove the row
  # Note: Site_ID and Time cannot be NA
  AMF_df <- AMF_df[rowSums(!is.na(AMF_df)) > 3,]
  
  # Output this file
  write.csv(AMF_df,paste0(Data_path,"02_AMF_cleaned/AMF_Hourly/","AMF_cleaned_hourly_",arrayid,".csv"))  
  message(paste("Complete",arrayid))
}

