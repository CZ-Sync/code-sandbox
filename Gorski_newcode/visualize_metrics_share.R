#===================================================================================#
# NOTES: code to visualize streamflow metrics. This script uses intermediate files for the basins that have been subset
# and assigned an ecoregion. Those objects were made in the script process_visualize_metrics_share.R, using the 
# combined_watershed_metadata_OCT2025.csv and the all_basins_merged.zip files dated 10/19 and 9/15 respectively.
# All the files that are read in this script should be in our shared google drive space.
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2025-10-03                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rmapshaper)
library(mapview)
#####
#===================================================================================#
# read in the streamflow signatures
sig_new <- read_csv('data/google drive/streamflowSignature_summaryData_OCT2025.csv') %>%
  mutate(gage_id = case_when(gage_type == 'USGS' ~ str_pad(as.numeric(gage_id), 8,pad = '0'),
                             .default = gage_id))

#read in signature dictionary
sig_dict <- readxl::read_xlsx('data/intermediate/signature_dictionary_251023.xlsx')

# read in the gage locations associated with ecoregions. The ecoregions were assigned based on the majority of the 
# area of the basin. There are 6,087 gages, all with sufficient streamflow data and basin 
# area < 85,000km2.
gage_locs_annotated <- st_read('data/intermediate/gage_location_annotated_6087_251021.shp') %>%
  select(-c(basin_r:longitd, prcssn_:name, n_bsns_, ecrgn_l))

gage_locs_annotated %>%
  filter(st_is_empty(.))

#merge gage_locs with the signatures, on an inner join with the newly calculated signatures, I'm only getting 5699 matches, even though the signatures are calculated
#for 5776 gages
gage_locs_sigs <- gage_locs_annotated %>%
  inner_join(sig_new, 
             by = 'gage_id'
  ) 

#read in problem sites, these need to be filtered out
ps <- read_csv('data/intermediate/problem_sites_251023.csv') %>%
  pull(gage_id)

#read in simplified ecoregions
ecoregions_simplified <- st_read('data/EPA_Ecoregions/ecoregions_simplified.shp')

# Generate consistent colors, abbreviations, and plotting order for ecoregions
eco_levels <- unique(gage_locs_sigs$ecoregn)
ecoregn_abbr = c('MWCF','NWFM','TU','NF','ETF','TA','HP','GP','NAD','TS','SSH','MC')
eco_colors <- setNames(RColorBrewer::brewer.pal(n = length(eco_levels), "Set3"), paste0(eco_levels, " (", ecoregn_abbr, ")"))

eco_abbr <- c('MWCF','NWFM','TU','NF','ETF','TA','HP','GP','NAD','TS','SSH','MC')
eco_abbr_colors <- setNames(RColorBrewer::brewer.pal(n = length(eco_levels), "Set3"), eco_abbr)

# Basin count per ecoregion (for labels)
eco_counts <- gage_locs_sigs %>%
  st_drop_geometry() %>%
  count(ecoregn, name = "n_basins_eco") %>%
  mutate(ecoregn_abbr = c('ETF','GP','HP','MWCF','MC','NAD','NF','NWFM','SSH','TA','TS','TU')) %>%
  mutate(plotting_order = c(12, 10, 11, 1, 5, 6, 7, 2, 9, 3, 8, 4))

# Merge count back for annotation
gage_locs_sigs <- gage_locs_sigs %>%
  left_join(eco_counts, by = "ecoregn") %>%
  mutate(ecoregion_label = paste0(ecoregn, " (", ecoregn_abbr, ")"))

# Order factor levels of ecoregion_label by eco_counts
eco_order <- eco_counts %>%
  arrange(plotting_order) %>%       
  mutate(ecoregion_label = paste0(ecoregn, " (", ecoregn_abbr, ")")) %>%
  pull(ecoregion_label)

# Order factor levels of ecoregion_abb
eco_abbr_order <- eco_counts %>%
  arrange(plotting_order) %>%   
  pull(ecoregn_abbr)

# Apply factor levels in that order
gage_locs_sigs <- gage_locs_sigs %>%
  mutate(
    ecoregion_label = factor(ecoregion_label, levels = eco_order),
    ecoregn_abbr = factor(ecoregn_abbr, levels = eco_abbr_order)
  )

#apply ecoregions abbrevations to the ecoregions spatial data for plotting
ecoregions_simplified <- ecoregions_simplified %>%
  left_join(eco_counts, by = c('ecoregion' = 'ecoregn')) %>%
  mutate(ecoregion_label = paste0(ecoregion, " (", ecoregn_abbr, ")"))



# --- Get a North America outline ---
north_america_states_prov <- ne_states(country = c('United States of America', 'Canada', 'Mexico'))

# Transform to match the projection used (same as ecoregions)
north_america_states_prov <- st_transform(north_america_states_prov, crs = st_crs(ecoregions_simplified)) %>%
  select(name)

north_america <- ne_countries(scale = "small", continent = "North America", returnclass = "sf") %>%
  filter(geounit %in% c('United States of America','Canada','Mexico'))

# Transform to match the projection used (same as ecoregions)
north_america <- st_transform(north_america, crs = st_crs(ecoregions_simplified)) %>%
  select(name)




#Make maps
locator_map <- ggplot() +
  geom_sf(data = ecoregions_simplified, aes(fill = ecoregion_label), color = "white", size = 0.2, alpha = 0.9) +
  geom_sf(data = north_america, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = variable_plotting, shape = 16, size = 0.3, color = 'black')+
  coord_sf(xlim = c(-4939299.1, 3586452.2), ylim = c(-394685.6, 6198273.8), expand = FALSE)+
  scale_fill_manual(values = eco_colors, name = "") +
  theme_void() +  # removes grid lines and background
  labs(title = "EPA Level I Ecoregions") +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.25, "cm"),         # shrink legend boxes
    legend.text = element_text(size = 8),      # smaller text
    legend.title = element_text(size = 8),     # smaller title
    legend.spacing.x = unit(0.1, "cm"),        # tighter horizontal spacing
    plot.title = element_text(hjust = 0.5)
  )+
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))

locator_map


variables <- c('Qann','Qwin','Qspr','Qsum','Qfal',
               paste0('Q',c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,99)),
               'Q95.Q10','FDCall','CFD90th','FDCmid','flashinessRB',
               paste0('D',c(5,10,20,30,40,50,60,70,80,90,95),'_day'),
               'D25_to_D75','Dmax',
               'n_high_pulses_year','n_low_pulses_year','n_high_pulses_all','n_low_pulses_all',
               'dur_high_pulses_year','dur_low_pulses_year','dur_high_pulses_all','dur_low_pulses_all',
               'TQmean', paste0('Flow_Reversals_',c('annual','spring','summer','winter','fall')),
               'BFI_Eckhardt','BFI_LyneHollick','log_a_pointcloud','log_a_events','b_pointcloud','b_events',
               'concavity')

#loop through and make plots
for(variable in variables){
  
  variable_names <- sig_dict %>%
    filter(grepl(variable,`Column name new`))
  
  #select variable of interest
  variable_plotting <- gage_locs_sigs %>%
    select(gage_id, ecoregn, contains(variable), ecoregn_abbr, ecoregion_label) %>%
    filter(!gage_id %in% ps)
  
  #Q annual has two outliers that make plotting the slope difficult so this 
  #code removes them, I have no reason to think these are an error, but they're easier to remove
  #for plotting
  if(variable == 'Qann'){
    variable_plotting <- variable_plotting %>%
      arrange(Qann_slp) %>%
      slice(-c(1,nrow(variable_plotting))) 
  }
  
  metric_panel_list <- list()
  
  for(i in 1:5){
    
    
    plot_var <- variable_names$`Column name new`[i]
    defn_var <- sig_dict %>%
      filter(`Column name new` == plot_var) %>%
      pull(Definition)
    stat <- sig_dict %>%
      filter(`Column name new` == plot_var) %>%
      pull(`Statistical metric`)
    log_plot <- sig_dict %>%
      filter(`Column name new` == plot_var) %>%
      pull(`Log plot`)
    
    
    metric_map <- ggplot() +
      geom_sf(data = north_america_states_prov, fill = NA, color = "darkgray", size = 0.5) +
      geom_sf(data = variable_plotting, shape = 21, size = 0.75, aes_string(fill = plot_var), color = 'transparent')+
      coord_sf(xlim = c(-4939299.1, 3586452.2), ylim = c(-394685.6, 6198273.8), expand = FALSE)+
      scale_fill_viridis_c(option = "H", name = stringr::str_wrap(stat, width = 20), direction = -1)+
      theme_void() +  # removes grid lines and background
      labs(title = stringr::str_wrap(defn_var, width = 60)) +
      theme(
        legend.position = "right",
        legend.key.size = unit(0.75, "cm"),         # shrink legend boxes
        legend.text = element_text(size = 10),      # smaller text
        legend.title = element_text(size = 10),     # smaller title
        legend.spacing.x = unit(0.1, "cm"),        # tighter horizontal spacing
        plot.title = element_text(hjust = 0.5)
      )
    
    #metric_map
    
    
    box_plot <- ggplot(variable_plotting, aes_string(x = 'ecoregn_abbr', y = plot_var, fill = 'ecoregn_abbr')) +
      geom_boxplot(outlier.size = 0.5) +
      scale_fill_manual(values = eco_abbr_colors, guide = "none") +
      theme_minimal() +
      labs(
        x = "Ecoregion",
        y = stringr::str_wrap(stat, width = 20)
      )+
      theme(axis.text.y = element_text(size = 10),
            plot.margin = margin(t = 10, r = 70, b = 10, l = 70))
    
    if(log_plot){
      box_plot <- box_plot +
        scale_y_log10()
      
      metric_map <- metric_map+
        scale_fill_viridis_c(option = "H", name = stringr::str_wrap(defn_var, width = 20), direction = -1, trans = 'log10')
    }
    
    
    metric_panel_list[[defn_var]] <- cowplot::plot_grid(metric_map, box_plot, ncol = 1, rel_heights = c(2,1))
    
  }
  
  cowplot::plot_grid(locator_map, metric_panel_list[[5]], metric_panel_list[[4]],
                     metric_panel_list[[1]], metric_panel_list[[2]], metric_panel_list[[3]], ncol = 3)
  ggsave(paste0('fig/251028/', variable, '.png'), bg = 'white', height = 10, width = 20)
}
