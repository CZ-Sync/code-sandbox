# -*- coding: utf-8 -*-
"""
Created on Jun 26 2025

@author: deni_
Code is modified from Edom Moges (personal communication), original sent May 2024

"""
'''
This code is intended to compute different lagged information theory interaction metrics
Has the option to handle outliers

In computing probabilities, it accounts for the effect of zeros which are common in hydrology. 
For instancce, precipitation and streamflow data. The metrics include:

Entropies -  𝐻(𝑋)  and  𝐻(𝑌) 
Mutual information -  𝐼(𝑌𝑡;𝑋𝑡−𝑙𝑎𝑔) 
Transfer entropy -  𝐼(𝑌𝑡;𝑋𝑡−𝑙𝑎𝑔|𝑌𝑡−1) 
Correlation coeficients -  𝜌(𝑌𝑡,𝑋𝑡−𝑙𝑎𝑔) 
Total information based on X -  𝐼(𝑌𝑡;𝑋𝑡,𝑋𝑡−1) 
Total information based on Y -  𝐼(𝑌𝑡;𝑋𝑡,𝑌𝑡−1)

'''
# This code adapted from the Jupyter notebook code from Edom Moges at ESDL (Hydrobench)
# Running information flow metrics for ameriflux data as test run between packages and example analyses
# This code should only be run AFTER normalization or anomaly calculations have been performed on source-sink data
# Data formatting for this code is single .csv files per site-source-sink-time variable combinations 
# (e.g., a csv file per site/temporal agg (like season) with three columns: date, sink timeseries, and source timeseries vars IN THAT ORDER)
# Preferble if .csv's have a numeric heading to them, I have used '1' to be simple but could be a site/var code that we have a dictionary for 
# NOTE: this algorithm can handle NA/blanks as well as 0's

#%%Import packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
from tqdm import tqdm
import glob

#%%Set working directory for functions
pathData1 = r'C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center'
os.chdir(pathData1)
print(os.getcwd()) #check your working directory

#Import local functions (save in same working directory as this file)
import SourceInfoMetricsWithOutliersV2 #to calculate the equations 1-6; this is the main function for info theory

#%% File location for working data (this should be in its own folder with no other types of files besides .csv's)
pathData2 = r'C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center\Test data'

#SOURCE VARIABLE = SM (soil moisture) --> SINK VARIABLE = ET (evapotranspiration)

# Read in file names for computing IT algorithms later
os.chdir(pathData2)
fileNames = [] 
for file in glob.glob("*.csv"):
     fileNames.append(file)

nameTab = pd.Series(fileNames)
fileNames = nameTab.apply(lambda x: x.split('.')[0])
fileNames = fileNames[0:1]  #change this if iterating through more than one file
fileNames
#%% Setting up your variable parameters before calculating information theory

NoStn = len(fileNames)
NoVar = 1 # only one source variable (soil moisture); can increase the number but need to modify into a loop to go through each source variable

nbins = 11 # number of bins; recommended value is 11, and for this code the value is set to 0 for first bin.
numiter = 500 # number of shuffles for critical metric value; statistical significance, shuffle the dataset to destroy time component; how different is my paired observations of MI compared to random shuffling MI? can reduce this if code is running slow but this wil mess w/ statistical significance so don not do final results with value less than 300
alpha = 0.05 # confidence level for critical metric value; 99% confidence (strong confidence; 0.01 = 99%)
shift = [-1,0,-1] #  Do not change for analysis. Conditioning variable (of the sink) lagging is fixed to -1; for TE regardless of variable lag of 1
MaxLag = 90# the maximum number of lags considered; need to consider mechanistic interpretation; consider how long my processes taking? also consider temporal resolution of the data
ncore = 1 # cores for parallelization

# folding percentage to deal with outliers in histogram development
lowerSink = .1 #1% from the minimum folded into the first percentile from 0.1 to 99.9 is the dataset, everything else on the other side of these will be folded into the first and last bin respectively.
upperSink = 99.9
lowerSource = .1
upperSource = 99.9

# Info-metrics 
Hx = np.ones([NoStn,MaxLag+1])*np.nan
Hy = np.ones([NoStn,MaxLag+1])*np.nan
MI = np.ones([NoStn,MaxLag+1])*np.nan
TE = np.ones([NoStn,MaxLag+1])*np.nan
CorrCoef = np.ones([NoStn,MaxLag+1])*np.nan
IQtXtXt_1 = np.ones([NoStn,MaxLag+1])*np.nan
MIp = np.ones([NoStn,MaxLag+1])*np.nan
TEp = np.ones([NoStn,MaxLag+1])*np.nan
IQtXtQt_1 = np.ones([NoStn,MaxLag+1])*np.nan

# ======Lag dependency of critical values ========== 
LagDependentCrit = 1 #set this equal to 1 if you want to have lag dependent critical values (dynamic), set to 0 if you want lag indepedent critical values (static) 

# Info-metrics critical thresholds # *** UNHASH THIS SECTION IF YOU SET LagDependentCrit = 0 *** (AND HASH THE SECTION BELOW)
#if LagDependentCrit == 0: 
#    
#    MIcrit = np.ones([NoStn,2])*np.nan
#    TEcritQ = np.ones([NoStn,2])*np.nan
#    CorrCrit = np.ones([NoStn,2])*np.nan
#    criticalTotMI_P = np.ones([NoStn,2])*np.nan
#    MIcritP = np.ones([NoStn,2])*np.nan
#    TEcritP = np.ones([NoStn,2])*np.nan
#    criticalTotMI_Q = np.ones([NoStn,2])*np.nan
    
if LagDependentCrit == 1: # Lag dependent Critical values (you get a different critical value for every lag analysis this is computed aka more dynamic to data)
    
    MIcrit = np.ones([NoStn,MaxLag+1])*np.nan
    TEcritQ = np.ones([NoStn,MaxLag+1])*np.nan
    CorrCrit = np.ones([NoStn,MaxLag+1])*np.nan
    criticalTotMI_P = np.ones([NoStn,MaxLag+1])*np.nan
    MIcritP = np.ones([NoStn,MaxLag+1])*np.nan
    TEcritP = np.ones([NoStn,MaxLag+1])*np.nan
    criticalTotMI_Q = np.ones([NoStn,MaxLag+1])*np.nan

   
#%% Run loop for calculating information theory metrics --> 
# run time for loop is 12 min for daily data per site if lag dependent crit values calculated
# run time for loop is 14 sec for daily data per site if lag independent crit values calculated

for k, i in enumerate(tqdm(fileNames)): 
    
    # Read data
    table = pd.read_csv((pathData2 +'\\' + i + '.csv'), header = 0,
                        index_col = 'Date',low_memory=False)
 
    Qt = table.iloc[:,0].values #sink values (ET)
    Xt = table.iloc[:,1].values #source values (SM)
    
   ### If there is no zero adjustment required, uncomment the following two lines.
   #Zeros were removed from the dataset so no zero adjustment is required.
    ZFlagsource = 0
    ZFlagsink = 0
    
    ### Automated Zero adjustments (if you have meaningful 0's in your dataset, unhash this code and make sure to hash the above 2 lines)
  #  Sink variable zero adjustment [Non-manual]
  #  if any(Qt==0):
         #ZFlagsink = 1 # Zero adjustment required
  #  else:
  #      ZFlagsink = 0 # Zero adjustment not required
         
    # # Source variable zero adjustment if there exist zeros
  #  if any(Xt==0):
        # ZFlagsource = 1 # Zero adjustment required
   # else:
        # ZFlagsource = 0 # Zero adjustment not required

   #Create array of information flow metrics (one array, each row is a different combination, each column is a different lag)
   #and create array of information flow metrics critical threshold (one array, each row is a different combination, the column is the critical value that the combination metrics must be above)
    Hx[k,0], Hy[k,0], MI[k,0], TE[k,0], CorrCoef[k,0], IQtXtXt_1[k,0], IQtXtQt_1[k,0], MIp[k,0], TEp[k,0], \
    MIcrit[k,0], TEcritQ[k,0], CorrCrit[k,0], criticalTotMI_P[k,0], MIcritP[k,0], TEcritP[k,0],\
    criticalTotMI_Q[k,0]= np.repeat(fileNames[k],16)
   
    Hx[k,1:], Hy[k,1:], MI[k,1:], TE[k,1:], CorrCoef[k,1:], IQtXtXt_1[k,1:], IQtXtQt_1[k,1:], MIp[k,1:], TEp[k,1:], \
    MIcrit[k,1:], TEcritQ[k,1:], CorrCrit[k,1:], criticalTotMI_P[k,1:], MIcritP[k,1:],TEcritP[k,1:],\
    criticalTotMI_Q[k,1:]= SourceInfoMetricsWithOutliersV2.ComputeInofoTheoreticMetricsAndSignificance\
    (Qt, Xt, nbins, numiter, alpha, shift, MaxLag, ncore,lowerSink, upperSink, lowerSource, upperSource, ZFlagsink,ZFlagsource, LagDependentCrit)

#%% Define a plotting function for data viz.
def plotMI_TE_Rho(MI, TE, CorrCoef, MIcrit, TEcritQ, CorrCrit):
    
    plt.figure(figsize=[12,4])
    plt.subplot(1,3,1)
    plt.plot(MI[1:],'g', label='MI')
    if LagDependentCrit ==0:
        plt.plot(np.repeat(MIcrit[1:],MaxLag),'r.', label='MI critical')
    else:
        plt.plot(MIcrit[1:],'r.', label='MI critical')
    plt.title('Mutual information')
    plt.xlabel('Lag in Days')
    plt.ylabel('MI in bits')
    plt.legend()
    plt.grid(linestyle='-.')

    plt.subplot(1,3,2)
    plt.plot(TE[1:],'g',label='TE')
    if LagDependentCrit ==0:
        plt.plot(np.repeat(TEcritQ[1:],MaxLag),'r.',label='TE critical')
    else :
        plt.plot(TEcritQ[1:],'r.',label='TE critical')
    plt.title('Transfer Entropy')
    plt.xlabel('Lag in Days')
    plt.ylabel('TE in bits')
    plt.legend()
    plt.grid(linestyle='-.')

    plt.subplot(1,3,3)
    plt.plot(CorrCoef[1:],'g',label='Corr. Coeff ')
    if LagDependentCrit ==0:
        plt.plot(np.repeat(CorrCrit[1:],MaxLag),'r.',label=r'$\rho$ critical')
    else:
        plt.plot(CorrCrit[1:],'r.',label=r'$\rho$ critical')
    plt.title('Pearson Correlation Coeff')
    plt.xlabel('Lag in Days')
    plt.ylabel(r"$\rho$")
    plt.legend()
    plt.grid(linestyle='-.')

    plt.subplots_adjust(wspace=0.5,hspace=0.1)

    plt.show()
    
#%% Plot raw results (e.g., not normalized to Hy or critical value)
St = 0 # select the first file (in this case the only file)
plotMI_TE_Rho(MI[St,:], TE[St,:], CorrCoef[St,:], MIcrit[St,:], TEcritQ[St,:], CorrCrit[St,:])

#How to interpret these plots:
# On the x axis is lags in days and the y axis the the MI, TE, and pearsons R between the source and sink variable at each lag.
# The green line displays the variability in these synchrony metrics across each lag
# The red line is the lag dependent critcal value (in this MI and R are significant, but TE is not)
# The primary difference between MI and TE is that TE is conditioning informaiton transfer on antecendent conditions in the sink variable while MI is just calculating overlapping amount of information (no conditioning)

#%%Create dataframes of TE and TEcrit (repeat code for MI if this is of interest)
TEcritdf = pd.DataFrame(data = TEcritQ)
TEcritdf.columns = [col - 1 for col in TEcritdf.columns] 
TEcritdf = TEcritdf.rename(columns = {TEcritdf.columns[0]: 'fileName'})
TEcritdf['Variable'] = 'TEcrit'
TEcritdf = pd.melt(TEcritdf, id_vars = ['fileName', 'Variable'], var_name = 'Lag_days', value_name = 'TEcrit')


TEdf = pd.DataFrame(data = TE)
TEdf.columns = [col - 1 for col in TEdf.columns] 
TEdf = TEdf.rename(columns = {TEdf.columns[0]: 'fileName'})
TEdf['Variable'] = 'TE_bits'
TEdf = pd.melt(TEdf, id_vars = ['fileName', 'Variable'], var_name = 'Lag_days', value_name = 'TE_bits')

#Merge TEcrit and TEdf
TEdf = pd.merge(TEdf, TEcritdf, on = ['fileName', 'Lag_days'])
TEdf = TEdf[['fileName', 'Lag_days', 'TE_bits', 'TEcrit']]

#Create df for Hy (Shannon entropy of sink variable) to calculate % uncertainty reductions
Hydf = pd.DataFrame(data = Hy)
Hydf.columns = [col - 1 for col in Hydf.columns]
Hydf = Hydf.rename(columns = {Hydf.columns[0]: 'fileName'})
Hydf = pd.melt(Hydf, id_vars = ['fileName'], var_name = 'Lag_days', value_name = 'Hy_bits')

#Merge for final df
TEdf = pd.merge(TEdf, Hydf, on = ['fileName', 'Lag_days'])

#Calculate TE percent uncertainty (TE/Hy)*100
TEdf['TE_perc_uncertainty_reduction'] = (TEdf['TE_bits'] / TEdf['Hy_bits'])*100

#Calculate TE relative and signficance (True/False)
TEdf['TE_relative'] = TEdf['TE_bits'] - TEdf['TEcrit'] #can also make this into a percent of Hy and plot this relative percent uncertainty reduction as well.
TEdf['Significance'] = np.where(TEdf['TE_bits'] > TEdf['TEcrit'], True, False)

#Calculate peak lag
maxTE = TEdf.TE_perc_uncertainty_reduction.max()
TE_peaklag = TEdf.loc[TEdf.TE_perc_uncertainty_reduction == maxTE,]

#Write results to .csv
TEdf.to_csv(r'C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center\AmerifluxTestSite_TEresults.csv')

#%% Plot TE as percent uncertainty reduction over each lag
for i in TEdf.fileName.unique(): #here 'i' will be each data combinations (eg. fileName)
    print(i)
    dat =TEdf.loc[TEdf.fileName == i,]
    maxi = dat.loc[dat['TE_perc_uncertainty_reduction'] == (dat['TE_perc_uncertainty_reduction'].max()), 'Lag_days'].item()
    plt.plot('Lag_days', TEdf['TEcrit']/TEdf['Hy_bits'].mean()*100, 'k-', data = dat, label = 'MI % rel uncert.')
    plt.plot('Lag_days', 'TE_perc_uncertainty_reduction', 'k--', data = dat, label = 'TE Critical Threshold %')
    plt.axvline(x = maxi, color = 'b')
    y_pos = dat['TE_perc_uncertainty_reduction'].max()
    plt.text(x=maxi, y=y_pos, s=f'Max = {maxi} days', color='blue', va='bottom', ha='left', fontsize=9)

    plt.xlabel('Lag (days)')
    plt.ylabel('TE % uncert. reduct.')
    plt.title('Ameriflux test site: SM --> ET')
    plt.show()

#This plot is now showing the percent uncertainty reduction at each lag time -- this is taking the TE and normalzing it to the amount of total transferrable information in the sink variable.
# The dashed line is the variability in TE Percent uncertainty reduction at each lag
# The solid black line is the lag dependent critical value
# The blue line is the lag at which our maxiumum TE occurs (though in this case its not signficant)