# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# This code includes functions for TE implementation, and data processing before TE implementation

# NOTE: This code DOES NOT treat zero values differently
# It deals with outliers
# TE calculation was conducted using the RtransferEntropy package
# Estimated running time is half of the manual version

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

# This function is to deal with outliers before discritization of continuous data
# Input include:
# The time series need to be discritized: var
# The lower boundary for folding the first bin: lower_qt (as a quantile)
# The upper boundary for folding the last bin: upper_qt (as a quantile)
fold_outliers <- function(var,lower_qt,upper_qt){
  q <- quantile(var,probs = c(lower_qt,upper_qt),na.rm=TRUE)
  var[var < q[1]] <- q[1]
  var[var > q[2]] <- q[2]
  return(var)
}


# This is a wrapped function for TE calculation
# From var1 to var2, unit: bits
# One direction
quick_TE <- function(var1,var2){
  TE <- calc_te(x=var1,y=var2,
                lx=1,ly=1, # Markov order (memory of the two variables)
                entropy = "Shannon",# Use Shannon type of transfer entropy
                q = "1",# Use Shannon type of transfer entropy (q->1 means Shannon)
                type = "bins", # Method used for discretization of continuous data
                bins = n_bin, # Number of bins for discretization
                na.rm = TRUE)
  return(TE)
}

# This function calculates TE from var1 to var2, for lags from 0 to max_lag
# There are two ways of normalization: Shannon (default) or Theory (which uses log(n_bin),the theoretic max of TE)
# Input includes:
# var1: source variable TS
# var2: sink variable TS
# max_lag: The maximum lag considered for running TE

# alpha: the alpha level used, default 0.05
# nshuffle: Number of shuffling for critical TE, default 300
Cal_TE <- function(var1,var2,max_lag,alpha=0.05,nshuffle = 300){
  
  # Since var1 and var2 are stationary, assume outlier bounds do not vary largely across lags
  # For computational speed, bin outliers once
  var1 <- fold_outliers(var1, lower_qt = lower_qt, upper_qt = upper_qt)
  var2 <- fold_outliers(var2, lower_qt = lower_qt, upper_qt = upper_qt)
  
  # Initialize a list to store all TE
  TE_list <- progressr::with_progress({
    # Initiate a progressor
    p <- progressor(along = 0:max_lag)
    # Calculate TE across all lags
    future_lapply(0:max_lag, function(lag){
      # Align TS of the sink and source
      var2_lagged <- var2[(lag+1):length(var2)]
      var1_aligned <- var1[1:(length(var1)-lag)]
      
      # Calculate TE(var1,var2),unit: bits
      # One direction: var1 -> var2
      TE <- quick_TE(var1_aligned,var2_lagged)
      
      # Shuffle-based null distribution of TE
      TE_shuffled <- replicate(nshuffle,{
        # This destroy the temporal structure while keeping the distribution of values intact
        var2_shuffled <- sample(var2_lagged)
        quick_TE(var1_aligned,var2_shuffled)
      })
      
      # Critical TE, using quantile
      cr_TE <- quantile(TE_shuffled,1-alpha)
      
      # Calculate Shannon entropy of the sink
      var2_disc <- discretize(na.omit(var2_lagged),numBins = n_bin)
      H_sink <- entropy(var2_disc,unit="log2")
      
      p()
      list(Lag = lag,TE = TE,cr_TE = cr_TE,H_sink = H_sink)
    },future.seed = TRUE)
  })
  
  # Combine all output into a data frame
  TE_df <- do.call(rbind.data.frame,TE_list)
  return(TE_df)
}

