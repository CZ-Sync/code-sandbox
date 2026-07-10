# -*- coding: utf-8 -*-
"""
Created on Wed May 27 09:42:30 2026

@author: deni_
"""
import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from matplotlib.colors import ListedColormap
import plotly.express as px
import plotly.offline as pyo
import plotly.graph_objects as go
from scipy import stats
from sklearn.preprocessing import StandardScaler
from scipy.cluster.hierarchy import linkage, dendrogram
from sklearn.cluster import KMeans
from sklearn.impute import SimpleImputer

#%% Data was downloaded to my local computer, but here are the links for all data seen here:
#startIn1993_60pct_signatures.csv = https://drive.google.com/drive/u/1/folders/1scMDIqmm7fFKi7V4e8gGFzrj5Ss_e1Pf
#hydroATLAS, MODIS LAI/ LULC and ECOREGION: https://drive.google.com/drive/folders/1vIe-dhcpqMwlGOhh7R0XlhzKlM9lq4m1

#%% Read in data and subset columns relevant to trend exploratory analysis
all_dat = pd.read_csv(r'C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center\Sno2Flo Trends\startIn1993_60pct_signatures.csv')

# 1. Isolate the first 20 columns (0 through 19)
metadata = all_dat.iloc[:, :20]
metadata = metadata[['gage_id', 'latitude', 'longitude', 'basin_area', 'gage_type', 'start_water_year', 'end_water_year','REGULATED']]

# 2. Isolate columns 
mk_senn_cols = all_dat.filter(regex='mk|senn', axis=1)
mk_senn_focus = mk_senn_cols[['BFI_Eckhardt_mk_pval', 'BFI_Eckhardt_mk_rho', 'BFI_Eckhardt_senn_slp',
                              'D25_to_D75_mk_pval', 'D25_to_D75_mk_rho', 'D25_to_D75_senn_slp',
                              'Dmax_mk_pval', 'Dmax_mk_rho', 'Dmax_senn_slp',
                              'dur_high_pulses_year_mk_pval', 'dur_high_pulses_year_mk_rho', 'dur_high_pulses_year_senn_slp',
                              'dur_low_pulses_year_mk_pval', 'dur_low_pulses_year_mk_rho', 'dur_low_pulses_year_senn_slp',
                              'TQmean_mk_pval', 'TQmean_mk_rho', 'TQmean_senn_slp',
                              'Qann_mk_pval', 'Qann_mk_rho', 'Qann_senn_slp',
                              'annual_runoff_ratio_mk_pval', 'annual_runoff_ratio_mk_rho', 'annual_runoff_ratio_senn_slp',
                              'Qfal_mk_pval', 'Qfal_mk_rho', 'Qfal_senn_slp',
                              'Qspr_mk_pval', 'Qspr_mk_rho', 'Qspr_senn_slp',
                              'Qsum_mk_pval', 'Qsum_mk_rho', 'Qsum_senn_slp',
                              'Qwin_mk_pval', 'Qwin_mk_rho', 'Qwin_senn_slp',
                              'fall_runoff_ratio_mk_pval', 'fall_runoff_ratio_mk_rho', 'fall_runoff_ratio_senn_slp',
                              'winter_runoff_ratio_mk_pval', 'winter_runoff_ratio_mk_rho', 'winter_runoff_ratio_senn_slp',
                              'spring_runoff_ratio_mk_pval', 'spring_runoff_ratio_mk_rho', 'spring_runoff_ratio_senn_slp',
                              'summer_runoff_ratio_mk_pval', 'summer_runoff_ratio_mk_rho', 'summer_runoff_ratio_senn_slp',]]

# 3. Combine and drop any duplicates (if a column falls into both categories)
trends = pd.concat([metadata, mk_senn_focus], axis=1).loc[:, lambda x: ~x.columns.duplicated()]

#%% Convert to long format: metadata, variable, mk rho, mk pval, senn slp

meta_cols = ['gage_id', 'latitude', 'longitude', 'basin_area', 'gage_type','start_water_year', 
             'end_water_year','REGULATED']

# Melt to long format
long_df = trends.melt(id_vars=meta_cols,var_name='full_var',value_name='value')

# Extract metric suffix
metrics = ['mk_pval', 'mk_rho', 'senn_slp']
long_df['Metric'] = long_df['full_var'].apply(lambda x: next((m for m in metrics if x.endswith(m)), None))

# Remove metric suffix to get variable name
long_df['Variable'] = long_df.apply(lambda row: row['full_var'].replace(f"_{row['Metric']}", ""), axis=1)
long_df = long_df[['gage_id', 'latitude', 'longitude', 'basin_area', 'gage_type',
       'start_water_year', 'end_water_year', 'REGULATED', 'value','Metric', 'Variable']]

# Pivot wider so metrics become columns
long_df = (long_df.pivot(index=meta_cols + ['Variable'],columns='Metric',values='value').reset_index())

#Drop rows with nan in trend columns
long_df.dropna(subset = ['senn_slp', 'mk_rho', 'mk_pval'], inplace = True) 

#%% make histograms of mk pval and mk rho
sns.histplot(data = long_df, x = 'mk_pval')
plt.axvline(x=0.05, color='red', linestyle='--')

sns.histplot(data = long_df, x = 'mk_rho')

#%% Classify trend significance

conditions = [
    # Very likely increasing
    (long_df["senn_slp"] > 0) &
    (long_df["mk_rho"] > 0) &
    (long_df["mk_pval"] <= 0.3),

    # Somewhat likely increasing
    (long_df["senn_slp"] > 0) &
    (long_df["mk_rho"] > 0) &
    (long_df["mk_pval"] > 0.3) &
    (long_df["mk_pval"] <= 0.6),

    # Trend as likely as not
    (long_df["mk_pval"] > 0.6),

    # Somewhat likely decreasing
    (long_df["senn_slp"] < 0) &
    (long_df["mk_rho"] < 0) &
    (long_df["mk_pval"] > 0.3) &
    (long_df["mk_pval"] <= 0.6),

    # Very likely decreasing
    (long_df["senn_slp"] < 0) &
    (long_df["mk_rho"] < 0) &
    (long_df["mk_pval"] <= 0.3),
]

choices = [
    "Increasing",
    "Somewhat_Increasing",
    "Stable",
    "Somewhat_Decreasing",
    "Decreasing",
]

long_df["classification"] = np.select(
    conditions,
    choices,
    default="Unclassified"
)

long_df['classification'] = np.where(long_df.classification == 'Unclassified', 'Stable', long_df.classification)

#%% Frequency of trend class by variable
long_df_count = long_df.groupby(['Variable', 'classification'])['gage_id'].count().to_frame().reset_index()
long_df_gage_sum_var = long_df_count.groupby('Variable')['gage_id'].sum().reset_index()
long_df_perc = pd.merge(long_df_count, long_df_gage_sum_var, on = 'Variable')
long_df_perc['gage_perc'] = (long_df_perc['gage_id_x'] / long_df_perc['gage_id_y'])*100

#Stacked bar chart of percent of sites in each trend class
plot_df = long_df_perc.pivot(index='Variable', columns='classification',values='gage_perc')
plot_df = plot_df[['Decreasing','Somewhat_Decreasing', 'Stable','Somewhat_Increasing','Increasing']]
colors = {'Decreasing': '#6AA84F','Somewhat_Decreasing': '#B6D7A8','Stable': '#D9D9D9',
          'Somewhat_Increasing': '#F9E79F','Increasing': '#F1C232'}
plot_df.plot( kind='bar',stacked=True, figsize=(10, 6),color=[colors[c] for c in plot_df.columns])
plt.ylabel('Percent of Sites')
plt.xlabel('Variable')
plt.legend(title='Classification', bbox_to_anchor=(1.05, 1))
plt.tight_layout()
plt.show()

#%% Read in L1 ecoregion data and assign each site a L1 region and then make plot for each L1

# Read ecoregion shapefile
eco = gpd.read_file(r"C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center\Sno2Flo Trends\Watershed Attribute Facets\na_cec_eco_l1\NA_CEC_Eco_Level1.shp")

# Create point geometry from coordinates
gdf_pts = gpd.GeoDataFrame(long_df,geometry=gpd.points_from_xy(long_df["longitude"], long_df["latitude"]), crs="EPSG:4326")

# Reproject points if necessary
if eco.crs != gdf_pts.crs:
    gdf_pts = gdf_pts.to_crs(eco.crs)

# Spatial join
joined = gpd.sjoin(gdf_pts, eco[["NA_L1NAME", "geometry"]], how="left",predicate="within")

# Add ecoregion name back to dataframe
long_df["NA_L1NAME"] = joined["NA_L1NAME"].values

#%% Make pie chart of number (%) of gages in each ecoregion
eco_count = long_df.groupby(['Variable', 'NA_L1NAME'])['gage_id'].count().reset_index()
eco_count = eco_count.loc[eco_count.Variable == 'BFI_Eckhardt',] #6054 total
eco_count['perc_gages'] = (eco_count['gage_id'] / 6054) * 100
eco_perc = eco_count[['NA_L1NAME', 'perc_gages']]

ecoregion_colors = {'ARCTIC CORDILLERA': '#8AA7FF','TUNDRA': '#B8CBFF', 'TAIGA': '#9ED0F6',
    'HUDSON PLAIN': '#8FE6D4', 'NORTHERN FORESTS': '#A3F0C9', 'NORTHWESTERN FORESTED MOUNTAINS': '#39E600',
    'MARINE WEST COAST FOREST': '#00B050', 'EASTERN TEMPERATE FORESTS': '#C8E6A0','GREAT PLAINS': '#E6D67A',
    'NORTH AMERICAN DESERTS': '#FFD84D', 'MEDITERRANEAN CALIFORNIA': '#B8DC5A','SOUTHERN SEMIARID HIGHLANDS': '#9BE03C',
    'TEMPERATE SIERRAS': '#D6F08B','TROPICAL WET FORESTS': '#D95CF2'}

# Build color list in the same order as the dataframe
colors = [ecoregion_colors.get(name.upper(), '#CCCCCC')
    for name in eco_perc['NA_L1NAME']
]

fig, ax = plt.subplots(figsize=(10, 8))
wedges, _, _ = ax.pie(eco_perc['perc_gages'],colors=colors,autopct='%1.1f%%',startangle=90)
ax.legend( wedges, eco_perc['NA_L1NAME'], title='Level 1 Ecoregion', loc='center left', bbox_to_anchor=(1.0, 0.5), frameon=False)
ax.set_title('Percent of Sites by Level 1 Ecoregion')
plt.tight_layout()
plt.show()

#%% Perform same frequency trend but by ecoregion
long_df_count_eco = long_df.groupby(['Variable', 'classification', 'NA_L1NAME'])['gage_id'].count().to_frame().reset_index()
long_df_gage_sum_var_eco = long_df_count_eco.groupby(['Variable', 'NA_L1NAME'])['gage_id'].sum().reset_index()
long_df_perc_eco = pd.merge(long_df_count_eco, long_df_gage_sum_var_eco, on = ['Variable', 'NA_L1NAME'])
long_df_perc_eco['gage_perc'] = (long_df_perc_eco['gage_id_x'] / long_df_perc_eco['gage_id_y'])*100


idx = long_df_perc_eco.groupby(
    ['Variable', 'NA_L1NAME']
)['gage_perc'].idxmax()

long_df_perc_eco_max = long_df_perc_eco.loc[idx].reset_index(drop=True)

colors = {'Decreasing': '#6AA84F','Somewhat_Decreasing': '#B6D7A8','Stable': '#D9D9D9',
          'Somewhat_Increasing': '#F9E79F','Increasing': '#F1C232'}

eco_order = [ 'TUNDRA',  'TAIGA',  'NORTHWESTERN FORESTED MOUNTAINS', 'MARINE WEST COAST FOREST',
    'TEMPERATE SIERRAS',  'MEDITERRANEAN CALIFORNIA', 'NORTHERN FORESTS',  'HUDSON PLAIN',
    'GREAT PLAINS', 'NORTH AMERICAN DESERTS','SOUTHERN SEMIARID HIGHLANDS', 'EASTERN TEMPERATE FORESTS',
    'TROPICAL WET FORESTS']

long_df_perc_eco_max['NA_L1NAME'] = pd.Categorical( long_df_perc_eco_max['NA_L1NAME'], categories=eco_order, ordered=True)

plt.figure(figsize=(8, 6))  # width, height in inches

ax = sns.scatterplot(
    data=long_df_perc_eco_max.sort_values('NA_L1NAME'),
    x='NA_L1NAME',
    y='Variable',
    hue='classification',
    size='gage_perc',
    palette=colors
)
plt.legend(bbox_to_anchor=(1.05, 1), loc="upper left")
plt.xticks(rotation=90)
plt.tight_layout()
plt.show()

#%% map of significant trends (color by direction, increase v decrease binary) 
test_var_sig = long_df.loc[long_df.Variable == 'Qwin',]

# Map of slopes

fig = px.scatter_geo(test_var_sig, 
                     lat='latitude',
                     lon='longitude',
                     color = 'classification',
                     color_discrete_map = colors
                     )

fig.update_traces(marker=dict(size = 8))

# Update the layout of the map
fig.update_layout(title='Site Locations',
                  geo=dict(
                      scope='north america',
                      showland=True,
                      showcountries=True,
                      countrycolor='white',  # Outline color
                      showcoastlines=True,
                      coastlinecolor='white',  # Coastline color
                      showlakes=True,
                      lakecolor='white',
                      projection_scale=0.6,  # Zoom into the East Coast
                      center=dict(lat=39.7, lon=-77),
                      bgcolor='white'  # Background color
                  ))

fig.update_traces(marker=dict(line=dict(width=1, color='black')))
fig.update_traces(textposition='middle left')

# Render the figure in Spyder
fig.write_html('SigTrend_BFI_Eckhardt.html')

# Open the HTML file in the default web browser
import webbrowser
webbrowser.open('SigTrend_BFI_Eckhardt.html')

#%% Read in some watershed facet vars

hydroatlas = pd.read_csv(r"C:\Users\deni_\OneDrive - USNH\Desktop\University of New Hampshire\Projects\CZ Synchrony- Powell Center\Sno2Flo Trends\Watershed Attribute Facets\HydroATLAS\watershed_hydroatlas_metadata_29may2026.csv")
hydroatlas = hydroatlas[['gage_id', 'outlet_UP_AREA', 'dor_pc_pva', 'ari_ix_uav', 'snw_pc_uyr', 
                         'for_pc_use', 'hft_ix_u93', 'wet_cl_smj', 'lit_cl_smj']]
hydroatlas['gage_id'] = hydroatlas['gage_id'].apply(
    lambda x: x[1:] if isinstance(x, str) and len(x) >= 8 and x.startswith('0') else x)

long_df['gage_id'] = long_df['gage_id'].astype(str).str.strip()
hydroatlas['gage_id'] = hydroatlas['gage_id'].astype(str).str.strip()

long_df_atlas = pd.merge(long_df, hydroatlas, on = 'gage_id', how = 'left')

#%% Using perc_snow cover as an example facet:

#Assign bin in long_df_atlas to high snow, mid snow, low snow cover
long_df_atlas['snow_cover_cat'] = pd.cut(
    long_df_atlas['snw_pc_uyr'],
    bins=[0, 30, 50, 80],
    labels=['low_snow', 'med_snow', 'high_snow'],
    include_lowest=True
)

#Visualize trends in each bin:
long_df_atlas_count = long_df_atlas.groupby(['Variable', 'classification', 'snow_cover_cat'])['gage_id'].count().to_frame().reset_index()
long_df_gage_sum_var_atlas = long_df_atlas_count.groupby(['Variable', 'snow_cover_cat'])['gage_id'].sum().reset_index()
long_df_perc_atlas = pd.merge(long_df_atlas_count, long_df_gage_sum_var_atlas, on = ['Variable', 'snow_cover_cat'])
long_df_perc_atlas['gage_perc'] = (long_df_perc_atlas['gage_id_x'] / long_df_perc_atlas['gage_id_y'])*100

#Stacked bar chart of percent of sites in each trend class
facet_1 = long_df_perc_atlas.loc[long_df_perc_atlas.snow_cover_cat == 'high_snow',]
facet_1 = facet_1.loc[facet_1.Variable == 'Qwin',]
plot_df = facet_1.pivot(index='Variable', columns='classification',values='gage_perc')
plot_df = plot_df[['Decreasing','Somewhat_Decreasing', 'Stable','Somewhat_Increasing','Increasing']]
colors = {'Decreasing': '#6AA84F','Somewhat_Decreasing': '#B6D7A8','Stable': '#D9D9D9',
          'Somewhat_Increasing': '#F9E79F','Increasing': '#F1C232'}
plot_df.plot( kind='bar',stacked=True, figsize=(10, 6),color=[colors[c] for c in plot_df.columns])
plt.ylabel('Percent of Sites')
plt.xlabel('Variable')
plt.legend(title='Classification', bbox_to_anchor=(1.05, 1))
plt.tight_layout()
plt.title('High Snow Trend Class')
plt.show()

#%% Plot mann kendall test of one var vs. another var...need to workshop this because significance classes might play a role. 
# to see if these variables are changing in tandem or not in tandem. 

test_pair = trends[['gage_id', 'annual_runoff_ratio_mk_pval',
                    'annual_runoff_ratio_mk_rho', 'winter_runoff_ratio_mk_pval',
                    'winter_runoff_ratio_mk_rho']].dropna()

test_pair['Sig_trend_both'] = np.where(((test_pair.annual_runoff_ratio_mk_pval < 0.05) & 
                                        (test_pair.winter_runoff_ratio_mk_pval < 0.05)), True, False)
test_pair_sig = test_pair.loc[test_pair.Sig_trend_both == True,]

plt.scatter(test_pair.annual_runoff_ratio_mk_rho, test_pair.winter_runoff_ratio_mk_rho, alpha = 0.2, label = 'Non sig')
plt.scatter(test_pair_sig.annual_runoff_ratio_mk_rho, test_pair_sig.winter_runoff_ratio_mk_rho, alpha = 0.8, label = 'Sig')
plt.legend()
plt.axvline(x=0, color='black', linestyle='-')
plt.axhline(y=0, color='black', linestyle='-')

#%%
test_pair = trends[['gage_id', 'Qfal_mk_pval',
                    'Qfal_mk_rho', 'Qwin_mk_pval',
                    'Qwin_mk_rho']].dropna()

test_pair['Sig_trend_both'] = np.where(((test_pair.Qfal_mk_pval < 0.05) & (test_pair.Qwin_mk_pval < 0.05)), True, False)
test_pair_sig = test_pair.loc[test_pair.Sig_trend_both == True,]

plt.scatter(test_pair.Qfal_mk_rho, test_pair.Qwin_mk_rho, alpha = 0.2, label = 'Non sig')
plt.scatter(test_pair_sig.Qfal_mk_rho, test_pair_sig.Qwin_mk_rho, alpha = 0.8, label = 'Sig')
plt.legend()
plt.axvline(x=0, color='black', linestyle='-')
plt.axhline(y=0, color='black', linestyle='-')


#%%
test_pair = trends[['gage_id', 'BFI_Eckhardt_mk_pval',
                    'BFI_Eckhardt_mk_rho', 'dur_low_pulses_year_mk_pval',
                    'dur_low_pulses_year_mk_rho']].dropna()

test_pair['Sig_trend_both'] = np.where(((test_pair.BFI_Eckhardt_mk_pval < 0.05) & (test_pair.dur_low_pulses_year_mk_pval < 0.05)), True, False)
test_pair_sig = test_pair.loc[test_pair.Sig_trend_both == True,]

plt.scatter(test_pair.BFI_Eckhardt_mk_rho, test_pair.dur_low_pulses_year_mk_rho, alpha = 0.2, label = 'Non sig')
plt.scatter(test_pair_sig.BFI_Eckhardt_mk_rho, test_pair_sig.dur_low_pulses_year_mk_rho, alpha = 0.8, label = 'Sig')
plt.legend()
plt.axvline(x=0, color='black', linestyle='-')
plt.axhline(y=0, color='black', linestyle='-')

#%%Put the trend data into a cluster algorithm to find areas where places are moving in similar directions

# significance flag
long_df['sig'] = long_df['mk_pval'] < 0.05

summary = long_df.groupby('Variable').agg(
    mean_slope=('senn_slp', 'mean'),
    mean_rho=('mk_rho', 'mean'),
    frac_sig=('sig', 'mean'),
    frac_increasing=('senn_slp', lambda x: (x > 0).mean()),
    frac_decreasing=('senn_slp', lambda x: (x < 0).mean())
)


X = StandardScaler().fit_transform(summary)

Z = linkage(X, method='ward')

plt.figure(figsize=(10,5))
dendrogram(Z, labels=summary.index, leaf_rotation=90)
plt.show()


kmeans = KMeans(n_clusters=4, random_state=0)
summary['cluster'] = kmeans.fit_predict(X)

sns.clustermap(
    summary.drop(columns='cluster', errors='ignore'),
    cmap='vlag',
    standard_scale=1
)

