# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# This code includes functions for TE implementation, and data processing before TE implementation

# This code can handle with zero values. When discretizing continuous data, it puts zero values in the first bin

# This function calculates the difference between each two time steps in a time series
# Input df: the data frame of time series variables
# Input varname: variable name to process
delta_TS <- function(df,varname){
  # TS at t0
  TS0 <- head(df[[varname]],-1)
  # TS with 1 time step shifted
  TS1 <- tail(df[[varname]],-1)
  # Get the difference
  delta_var <- TS1 - TS0
  return(delta_var)
}

# This function is to deal with outliers before discretization of continuous data
# Input include:
# The time series need to be discritized: var
# The lower boundary for folding the first bin: lower_qt (as a quantile)
# The upper boundary for folding the last bin: upper_qt (as a quantile)
# Output: TS after dealing with the outliers
fold_outliers <- function(var,lower_qt,upper_qt){
  q <- quantile(var,probs = c(lower_qt,upper_qt),na.rm=TRUE)
  var[var < q[1]] <- q[1]
  var[var > q[2]] <- q[2]
  return(var)
}

# This function is to adjust zero values in the data when discretizing continuous data, true zero values are put in the first bin
# Input are:
# TS data to be processed (x)
# number of total bins for descretization: nbins
# lower and upper quantile for the outliers
# Output is: the bin of each value
zero_adjustment <- function(var,nbins,lower_qt = NULL,upper_qt = NULL){
  # a tiny value to expand the two edges
  ths <- 1e-6
  # Deal with outliers
  var <- fold_outliers(var,lower_qt,upper_qt)
  
  # Initialize a vector to store results (location of bins)
  bin_loc <- rep(NA,length(var))
  # Index for zero values
  zero_idx <- which(var == 0)
  nonzero_idx <- which(var!=0 & !is.na(var))
  # Non-zero values
  var_nonzero <- var[nonzero_idx]
  
  # Get the range of data for binning
  lower_bd <- min(var_nonzero) - ths
  upper_bd <- max(var_nonzero) + ths
  
  # Get breaks
  breaks <- seq(lower_bd,upper_bd,length.out = nbins)
  
  # Discretize non-zero values to bins 2 and more
  bins <- cut(var_nonzero,breaks = breaks,include.lowest = TRUE,labels = FALSE)

  if(length(zero_idx != 0)){
    bins <- bins + 1
    # Assign 0 to the first bin
    bin_loc[zero_idx] <- 1
  }

  # Assign non-zero values to their corresponding bins
  bin_loc[nonzero_idx] <- bins
  
  return(bin_loc)
}

# This function calculates entropy based on the bins
# Input is a table of the number of obs in each bins
cal_entropy <- function(counts){
  # Calculate probability based on counts
  probs <- counts/sum(counts,na.rm=TRUE)
  probs <- probs[probs > 0]
  # Calculate entropy
  H <- -sum(probs*log2(probs))
  return(H)
}

# This function calculates joint Shannon entropy from multiple variables
# Inputs are the vectors of the bins of which values in TS belong to
joint_entropy <- function(...){
  df <- data.frame(...)
  # Get the joint distribution of multiple variables
  counts <- table(df)
  H <- cal_entropy(as.numeric(counts))
  return(H)
}

# This function is the wrapped function to calculate TE from var1 to var2
# TE(x->Y) = H(Yt,Yt-1) + H(Yt-1,Xt-lag) - H(Yt-1) - H(Xt-lag,Yt,Yt-1)
# Input includes:
# TS of the source: var1
# TS of the sink: var2
# # of bins for discretization: nbins
# lower and upper quantiles to deal with outliers
# cr is a logical value, if TRUE, it means this run is for critical values, then TS are shuffled
cal_transfer_entropy <- function(var1,var2,nbins,lower_qt,upper_qt,lag,cr = FALSE){
  # Total length of the TS
  n <- length(var2)
  x_lag <- var1[1:(n-lag-1)]
  yt <- var2[(lag+2):n]
  yt_1 <- var2[(lag+1):(n-1)]
  
  if(cr){
    # This destroy the temporal structure while keeping the distribution of values intact
    yt <- sample(yt)
    yt_1 <- sample(yt_1)
    x_lag <- sample(x_lag)
  }

  # Adjust for zero and get bins
  x_lag_bins <- zero_adjustment(x_lag,nbins,lower_qt,upper_qt)
  yt_bins <- zero_adjustment(yt,nbins,lower_qt,upper_qt)
  yt_1_bins <- zero_adjustment(yt_1,nbins,lower_qt,upper_qt)
  
  # Calculate entropy
  H_ytyt_1 <- joint_entropy(yt_bins,yt_1_bins)
  H_yt_1_x_lag <- joint_entropy(yt_1_bins,x_lag_bins)
  H_yt_1 <- cal_entropy(table(yt_1_bins))
  H_y_yt_1_x_lag <- joint_entropy(yt_bins,yt_1_bins,x_lag_bins)

  # Transfer entropy
  TE <- H_ytyt_1 + H_yt_1_x_lag - H_yt_1 - H_y_yt_1_x_lag
  # Return TE
  return(TE)
}

# This function is the main function to run TE, and output critical TE values
# This function calculates TE from var1 to var2, for lags from 0 to max_lag
# Input includes:
# var1: source variable TS
# var2: sink variable TS
# max_lag: The maximum lag considered for running TE
# nbins: # of bins for discretization
# alpha: alpha level for significance inference (default=0.05)
# nshuffle: number of shuffles to get critical TE values (default=300)
# lower and upper quantiles to deal with outliers
Cal_TE_main <- function(var1,var2,max_lag,nbins,alpha=0.05,nshuffle = 300,upper_qt,lower_qt){
  # Calculate the total entropy for the whole sink variable
  H_sink <- cal_entropy(table(zero_adjustment(var2,nbins,lower_qt,upper_qt)))
  
  # Compute lag-independent critical TE if needed
  if(!Lag_Dependent_Crit){
    # Shuffle-based null distribution of TE
    TE_shuffled_global <- replicate(nshuffle,{
      cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag=0,cr=TRUE)
    })
    cr_TE_global <- quantile(TE_shuffled_global,1-alpha)
  }
  
  # Initialize a list to store all TE results
  TE_list <- progressr::with_progress({
    # Initiate a progressor
    p <- progressor(along = 0:max_lag)
    # Calculate TE across all lags
    future_lapply(0:max_lag, function(lag){
      TE_results <- cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag)
      
      # Get Lag-dependent TE
      if(Lag_Dependent_Crit){
        # Shuffle-based null distribution of TE
        TE_shuffled <- future_replicate(nshuffle,{
          cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag,cr=TRUE)
        })
        
        # Critical TE, using quantile
        cr_TE <- quantile(TE_shuffled,1-alpha)  
      }else{
        cr_TE <- cr_TE_global
      }
      
      p()
      list(Lag = lag,TE = TE_results,cr_TE = cr_TE)
    },future.seed = TRUE)
  })
  # Combine all output into a data frame
  TE_df <- do.call(rbind.data.frame,TE_list)
  TE_df$H_sink <- rep(H_sink,nrow(TE_df))
  return(TE_df)
}







