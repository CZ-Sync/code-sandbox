# Author: Zhaozhe Chen
# Date: 2025.6.30

# These are functions for pre-processing AMF dataset
# 01_AMF_processing.R is the main function which outputs a df of required hourly variables

# Get required variable after QC
# For fine resolution (Hourly or Half-hourly), QC = 0 and QC = 1 were kept
# For coarser resolutions (daily through yearly), QC <=0 were removed
# Input include:
# The exact variable name (which should be the sub-variable name for a variable group)
# df: The data frame for processing, normally the raw AMF dataset
# Coarse: whether the temporal resolution is coarse or fine (sub-daily). This should be a logical value (TRUE or FALSE)

Var_QC <- function(varname,df,Coarse = FALSE){
  # Get the variable
  var_values <- df[[varname]]
  # Get the QC name for this variable
  varname_qc <- paste0(varname,"_QC")
  # Get the corresponding QC
  var_QC <- df[[varname_qc]]
  
  if(!varname_qc %in% names(df)){
    warning(paste("Variable",varname,"Does not have a QC file. Returning unmodified values"))
    return(var_values)
  }
  
  if(Coarse){
    # Remove all QC <= 0
    var_values[var_QC <= 0] <- NA
  }else{
    # Keep QC ==0|1
    var_values[var_QC != 0 & var_QC != 1] <- NA
  }
  message(paste("Complete QC for",varname))
  return(var_values)
}

# Since there could be multiple sub-variables for each variable
# This function extracts all sub-variable for the required variable, QC each of them, and get the average of them
# At hourly scale
# Input include:
# key_varname: Part of the variable name, which could be used to uniquely identify the variable
# df: The data frame, which should be the raw AMF dataset before QC
# Coarse: if True, the resolution is daily, if FALSE, the resolution is sub-daily (default: FALSE)
Mean_QC_Var <- function(key_varname,df,Coarse = FALSE){
  # Identify all variable names that starts with key_varname
  varnames <- grep(paste0("^",key_varname),names(df),value=TRUE)
  # Exclude those end with _QC
  varnames <- varnames[!grepl("_QC$",varnames)]
  
  if(length(varnames) == 0){
    stop(paste("No matching variable in this dataset for",key_varname))
  }
  
  # QC for each of the sub-variables
  qc_vars <- lapply(varnames,function(var) Var_QC(var,df,Coarse = Coarse))
  
  # Get means of the sub-variables
  qc_matrix <- do.call(cbind,qc_vars)
  qc_var_mean <- rowMeans(qc_matrix,na.rm = TRUE)
  return(qc_var_mean)
}

# This function is the wrapped function to extract all required variables and QC them
# Input includes:
# var_ls: the list of variables to process
# df: the dataset to be processed, usually the raw AMF dataset, with original variables and their QC variables
Var_QC_all <- function(var_ls,df){
  # Conduct QC for all required variables, and get the mean of them after QC
  results_ls <- lapply(var_ls, function(var){
    Mean_QC_Var(var,df,Coarse = FALSE)
  })

  # Combine these variables into a data frame
  Var_df <- as.data.frame(results_ls)
  names(Var_df) <- var_ls
  Var_df <- cbind(Site_ID = df$site_id,
                  Time = ymd_hms(df$date_time,tz="UTC"),
                  Var_df)
    
  # Set VPD, SWC and TS to 0 if negative
  Var_df$VPD[Var_df$VPD < 0] <- 0
  Var_df$SWC[Var_df$SWC < 0] <- 0
  Var_df$TS[Var_df$TS < 0] <- 0
  return(Var_df)
}



