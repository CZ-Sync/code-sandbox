# Author: Zhaozhe Chen
# Update date: 2025.6.29

# This code is to compare TE implementation in R with TE implementation in python
# Using the same test dataset (AMF daily SM and ET at US-Ne1)
# Using the same parameter sets
# The python version is in https://github.com/CZ-Sync/code-sandbox/tree/main/DMurray_TEcodepractice

# Only Transfer Entropy: TE(SM->ET) was calculated, mutual information or correlation were not included in this test code

# ------- Global --------
# Source functions for TE implementation and visualization
source(here("02_TE_implementation","TE_implementation_functions.R"))
source(here("05_Visualization","Plotting_functions.R"))

# Use the same dataset as for the python version
AMF_df <- read.csv(here("00_Data","AMF_test.csv"))
# This is the results from python TE implementation
SM_TE_df_py <- read.csv(here("02_TE_implementation","Results","TE_comparison_R_vs_python","AmerifluxTestSite_TEresults_python.csv"))
# Output path for results and figures
Output_path <- here("02_TE_implementation","Results/TE_comparison_R_vs_python/")

# Parameters for TE implementation
nbin <- 11 # Number of bins for TE discritization of continuous data (e.g., SM)
max_lag <- 90 # Maximum lag to consider (This should be adjusted according to the processes and the temporal resolution of data)
Lag_Dependent_Crit <- TRUE # Determine if critical TE is lag-dependent
nshuffle <- 500 # Number of shuffles (bootstrap) for critical TE for statistical inference
alpha <- 0.05 # Confidence level for critical TE
# Set parallel session
plan(multisession,workers = availableCores()-1)
# Ensure reproducibility
set.seed(50)

# These are folding parameters to deal with extreme values (outliers) in the time series
# i.e., extreme values will be binned into the first or last bin
lower_qt <- 0.001
upper_qt <- 0.999

# ---------- Main ----------
# Timing the TE calculation
start_time <- Sys.time()
# Run TE from SM to ET
SM_TE_df <- Cal_TE_main(var1 = AMF_df$SM,
                        var2 = AMF_df$ET,
                        max_lag = max_lag,
                        nbins = nbin,
                        alpha = alpha,
                        nshuffle = nshuffle,
                        upper_qt = upper_qt,
                        lower_qt = lower_qt,
                        ZFlag_Source = FALSE,
                        ZFlag_Sink = FALSE)

end_time <- Sys.time()
print(end_time - start_time)
# Record: running time (3.4 mins)
g_TE <- TE_lag_plot(SM_TE_df,"SM->ET","None")
# Output this figure
print_g(g_TE,"TE_SM_ET_daily_US-Ne1_test",6,4)

# Compare R results with python results
TE_df <- data.frame(TE_R = SM_TE_df$TE[1:90],
                    TE_Py = SM_TE_df_py$TE_bits)
# Calculate R2 between TE_R and TE_Py
R2 <- round(summary(lm(data=TE_df,TE_Py~TE_R))$r.squared,2)
g <- ggplot(TE_df,aes(TE_R,TE_Py))+
  geom_point(size=2,color="grey",alpha=0.8)+
  geom_abline(slope=1,intercept = 0,linetype="dashed")+
  my_theme+
  theme(aspect.ratio = 1:1)+
  annotate("text",x=0.105,y=0.09,
           label=paste("R2 =",R2),
           size=6)
print_g(g,"TE_comparison_R_vs_python",4,4)
