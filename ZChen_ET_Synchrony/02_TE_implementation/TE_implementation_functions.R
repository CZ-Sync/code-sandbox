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

# This function is to calculate binning edges when discretizing continuous data
# Input are:
# TS data to be processed: var
# number of total bins for descretization: nbins
# lower and upper quantile for the outliers
# Output: the edges for binning
get_binning_edges <- function(var,nbins,lower_qt = NULL, upper_qt = NULL){
  # a tiny value to expand the two edges
  ths <- 1e-4
  # Remove outliers in the data
  var <- fold_outliers(var,lower_qt,upper_qt)
  # Non-zero values
  nonzero <- var[var!=0 & !is.na(var)]
  # Get the range of data for binning
  lower_bd <- min(nonzero) - ths
  upper_bd <- max(nonzero) + ths
  # Get breaks (note, for nbins, the edge should be nbins+1)
  breaks <- seq(lower_bd,upper_bd,length.out = nbins + 1)
  return(breaks)
}

# This function is to adjust zero values in the data when discretizing continuous data, true zero values are put in the first bin
# Input are:
# TS data to be processed: var
# number of total bins for descretization: nbins
# lower and upper quantile for the outliers
# If ZFlag is TRUE: zero-adjustment is needed; if FALSE: no zero-adjustment
# Output is: the bin of each value
zero_adjustment <- function(var,nbins,lower_qt = NULL, upper_qt = NULL,ZFlag){
  # Initialize a vector to store results (location of bins)
  bin_loc <- rep(NA,length(var))
  # Index for zero and non-zero values
  zero_idx <- which(var == 0)
  nonzero_idx <- which(var!=0 & !is.na(var))
  # Non-zero values
  var_nonzero <- var[nonzero_idx]
  
  # Determine number of bins for non-zero values
  non_zero_bins <- if(ZFlag) nbins-1 else nbins
  
  # Get bin edges and bin the non-zero values
  breaks <- get_binning_edges(var_nonzero,nbins=non_zero_bins,lower_qt,upper_qt)
  bins <- cut(var_nonzero,breaks=breaks,include.lowest = TRUE,labels = FALSE,right = FALSE)
  
  # Shift bins if zero exists, and put zero in the first bin
  if(ZFlag){
    bins <- bins + 1
    bin_loc[zero_idx] <- 1
  }
  # Assign the bins to non-zero values
  bin_loc[nonzero_idx] <- bins
  return(bin_loc)
}

# This function is to shuffle matrix, for calculation of critical TE values
# It shuffles each column in the matrix, while preserving the locations of NA
# Input is the matrix to shuffle
shuffle_matrix <- function(M){
  # Initialize a blank matrix
  M0 <- matrix(NA,nrow=nrow(M),ncol=ncol(M))
  # Loop over the columns
  for(i in 1:ncol(M)){
    col_values <- M[,i]
    non_na_idx <- which(!is.na(col_values))
    shuffled_values <- sample(col_values[non_na_idx])
    M0[non_na_idx,i] <- shuffled_values
  }
  return(M0)
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

# This function takes one or more binned vectors and returns the joint bin 1D (1 to nbins^k)
# This ensures that each individual combination of bins is transformed to a unique 1D value
# Input include:
# ...: The bin vectors for each of the TS variables
# nbins: number of bins
joint_bin_index <- function(...,nbins){
  binned_list <- list(...)
  # Convert binned vectors into joint indices
  joint_index <- rep(0,length(binned_list[[1]]))
  k <- length(binned_list)
  for(i in 1:k){
    # e.g., for 3D: (b1 - 1)*nbins^2 + (b2-1)^nbins + b3
    joint_index <- joint_index + (binned_list[[i]]-1)*nbins^(k-i)
  }
  joint_index <- joint_index[!is.na(joint_index)]
  return(joint_index + 1)
}

# This function calculates joint Shannon entropy from multiple variables
# Inputs are the vectors of the bins of which values in TS belong to
# Input is the joint_index, calculated from joint_bin_index
joint_entropy <- function(joint_index){
  # Get the joint distribution of multiple variables
  counts <- table(joint_index)
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
# ZFlag_Source is whether the Source needs zero adjustment (TURE or FALSE)
# ZFlag_Sink is whether the Sink needs zero adjustment (TURE or FALSE)
cal_transfer_entropy <- function(var1,var2,nbins,lower_qt,upper_qt,lag,cr = FALSE,ZFlag_Source,ZFlag_Sink){
  # Total length of the TS
  n <- length(var2)
  x_lag <- var1[1:(n-lag-1)]
  yt <- var2[(lag+2):n]
  yt_1 <- var2[(lag+1):(n-1)]
  
  # Put them into a matrix
  M <- cbind(x_lag,yt,yt_1)
  # Remove rows if there is any NA, to ensure complete observations
  M <- M[complete.cases(M),]

  if(cr){
    # This destroy the temporal structure while keeping the distribution of values intact
    M <- shuffle_matrix(M)
    x_lag <- M[,1]
    yt <- M[,2]
    yt_1 <- M[,3]
  }

  # Adjust for zero and get bins
  x_lag_bins <- zero_adjustment(M[,1],nbins,lower_qt,upper_qt,ZFlag_Source)
  yt_bins <- zero_adjustment(M[,2],nbins,lower_qt,upper_qt,ZFlag_Sink)
  yt_1_bins <- zero_adjustment(M[,3],nbins,lower_qt,upper_qt,ZFlag_Sink)
  
  # Calculate entropy
  H_ytyt_1 <- joint_entropy(joint_bin_index(yt_bins,yt_1_bins,nbins = nbins))
  H_yt_1_x_lag <- joint_entropy(joint_bin_index(yt_1_bins,x_lag_bins,nbins = nbins))
  H_yt_1 <- joint_entropy(joint_bin_index(yt_1_bins,nbins=nbins))
  H_y_yt_1_x_lag <- joint_entropy(joint_bin_index(yt_bins,yt_1_bins,x_lag_bins,nbins = nbins))

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
# ZFlag_Source is whether the Source needs zero adjustment (TURE or FALSE)
# ZFlag_Sink is whether the Sink needs zero adjustment (TURE or FALSE)

Cal_TE_main <- function(var1,var2,max_lag,nbins,alpha=0.05,nshuffle = 300,upper_qt,lower_qt,ZFlag_Source,ZFlag_Sink){
  # Calculate the total entropy for the whole sink variable
  # Assuming H does not change largely across lags (this is to save computational time)
  H_sink <- cal_entropy(table(zero_adjustment(var2,nbins,lower_qt,upper_qt,ZFlag_Sink)))
  
  # Compute lag-independent critical TE if needed
  if(!Lag_Dependent_Crit){
    # Shuffle-based null distribution of TE
    TE_shuffled_global <- replicate(nshuffle,{
      cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag=0,cr=TRUE,ZFlag_Source = ZFlag_Source,ZFlag_Sink = ZFlag_Sink)
    })
    # Get critical TE based on T-statistics
    t_crit <- qt(1-alpha,df = nshuffle - 1)
    cr_TE_global <- mean(TE_shuffled_global,na.rm=TRUE) + t_crit*sd(TE_shuffled_global,na.rm=TRUE)
  }
  
  # Initialize a list to store all TE results
  TE_list <- progressr::with_progress({
    # Initiate a progressor
    p <- progressor(along = 0:max_lag)
    # Calculate TE across all lags
    future_lapply(0:max_lag, function(lag){
      TE_results <- cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag,ZFlag_Source = ZFlag_Source,ZFlag_Sink = ZFlag_Sink)
      
      # Get Lag-dependent TE
      if(Lag_Dependent_Crit){
        # Shuffle-based null distribution of TE
        TE_shuffled <- future_replicate(nshuffle,{
          cal_transfer_entropy(var1,var2,nbins,lower_qt,upper_qt,lag,cr=TRUE,ZFlag_Source = ZFlag_Source,ZFlag_Sink = ZFlag_Sink)
        })
        
        # Critical TE, using T-statistics
        t_crit <- qt(1-alpha,nshuffle - 1)
        cr_TE <- mean(TE_shuffled,na.rm=TRUE) + t_crit*sd(TE_shuffled,na.rm=TRUE)
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







