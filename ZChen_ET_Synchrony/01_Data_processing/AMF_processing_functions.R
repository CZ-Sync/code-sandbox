# Author: Zhaozhe Chen
# Date: 2025.6.29

# These are functions for pre-processing AMF dataset
# 01_AMF_processing.R is the main function which outputs a df of required hourly variables

# Get required variable after QC
# For fine resolution (Hourly or Half-hourly), QC = 0 and QC = 1 were kept
# For coarser resolutions (daily through yearly), QC <=0 were removed
# Input is the variable name, data frame to process, and if the resolution is Fine or Coarse
Var_QC <- function(varname, df,Coarse = TRUE){
  # All variable names in df
  var_ls <- names(df)
  # variable and variable QC names
  vars <- grep(varname,names(df),value = TRUE)
  # Get the variable name
  var <- vars[!grepl("_QC$",vars)]
  # Get its QC name
  var_QC <- vars[grepl("_QC",vars)]
  if(Coarse){
    # Remove all QC <= 0
    df[[var]][df[[var_QC]]<=0] <- NA
  }else{
    # Keep QC ==0|1
    df[[var]][df[[var_QC]]!=0 & df[[var_QC]]!=1] <- NA
  }
  message(paste("Complete QC for",var,"using",var_QC))
  return(df[[var]])
}