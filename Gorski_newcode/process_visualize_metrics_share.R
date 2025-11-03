#===================================================================================#
# NOTES: code for processing data to visualize streamflow metrics
#
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
#####
#===================================================================================#
sig <- read_csv('data/google drive/streamflow_signatures_USGS-HYDAT.csv') %>%
  mutate(gage_id = case_when(gage_type == 'USGS' ~ str_pad(as.numeric(gage_id), 8,pad = '0'),
                             .default = gage_id))

#read in all basins
basins <- st_read('data/google drive/all_basins_merged/all_basins_merged.shp')
#read in combined watershed metadata
metadata <- read_csv('data/google drive/combined_watershed_metadata_OCT2025.csv')

#subset to find sites with sufficient data coverage
suff_q <- metadata %>% 
  filter(processing_status == 'success' | processing_status == 'processing') %>%
  mutate(gage_id = case_when(gage_type == 'USGS' ~ str_pad(as.numeric(gage_id), 8,pad = '0'),
                             .default = gage_id))

#subset basins based on the sufficient streamflow data
basin_subset <- basins %>%
  filter(area_km2 < 85000) %>%
  inner_join(suff_q, by = c('site_id' = 'gage_id'))

#now subset the suff_q data set on the basins in basin subset, this is because there
#are some sites within the suff_q data frame that have basin_area == NA, but when matched 
#with this basin area in the basins object actually have basin areas that are > 85000, so 
#this is a backward way of filtering them out, both should have 6,041 unique basins
suff_q <- suff_q %>%
  filter(gage_id %in% basin_subset$site_id)


#write the subset of basins to the file
st_write(basin_subset, 'data/google drive/basin_subset_85k_2501022.shp')

basin_subset <- st_read('data/google drive/basin_subset_85k_2501022.shp')


#read in EPA Level 1 EcoRegions
ecoregions <- st_read('data/EPA_Ecoregions/na_cec_eco_l1/NA_CEC_Eco_Level1.shp')

ecoregions <- st_transform(ecoregions, crs = st_crs(basin_subset)) %>%
  rename(ecoregion = NA_L1NAME) %>%
  st_make_valid() #%>%
#group_by(ecoregion) %>%
#summarise()

ecoregions_simplified <- ms_simplify(ecoregions, keep = 0.05, keep_shapes = TRUE)

#st_write(ecoregions_simplified, 'data/EPA_Ecoregions/ecoregions_simplified.shp')

#Intersect basins with ecoregions
intersections <- st_intersection(
  basin_subset %>% select(site_id, are_km2),  # select minimal needed cols
  ecoregions %>% select(ecoregion)
)


# #Calculate area of intersected pieces
intersections <- intersections %>%
  st_make_valid() %>%
  mutate(part_area = as.numeric(st_area(geometry)))

st_write(intersections, 'data/basin_eco_region_majority_intersections_251022.shp')
intersections <- st_read('data/basin_eco_region_majority_intersections_251022.shp')

#For each basin, find the ecoregion with the largest intersected area
dominant_ecoregion <- intersections %>%
  group_by(site_id) %>%
  slice_max(part_area, with_ties = FALSE) %>%
  ungroup() %>%
  select(site_id, ecoregion) %>%
  st_drop_geometry()

#Join back to original basins
basins_classified <- basin_subset %>%
  left_join(dominant_ecoregion, by = "site_id")

# Generate consistent colors for ecoregions using. [1:12] omits WATER which is fine
eco_levels <- unique(basins_classified$ecoregion)[1:12]
eco_colors <- setNames(RColorBrewer::brewer.pal(n = length(eco_levels), "Set3"), eco_levels)

# Basin count per ecoregion (for labels)
eco_counts <- basins_classified %>%
  st_drop_geometry() %>%
  count(ecoregion, name = "n_basins_eco")

# Merge count back for annotation
basins_annotated <- basins_classified %>%
  left_join(eco_counts, by = "ecoregion") %>%
  mutate(ecoregion_label = paste0(ecoregion, "\n(n=", n_basins_eco, ")"))

# Order factor levels of ecoregion_label by eco_counts
eco_order <- eco_counts %>%
  arrange(n_basins_eco) %>%       # or use arrange(n_basins_eco) for ascending order
  mutate(ecoregion_label = paste0(ecoregion, "\n(n=", n_basins_eco, ")")) %>%
  pull(ecoregion_label)

# Apply factor levels in that order
basins_annotated <- basins_annotated %>%
  mutate(
    ecoregion_label = factor(ecoregion_label, levels = eco_order)
  )

#st_write(basins_annotated, 'data/intermediate/basins_6087_annotated_251021.shp')
# ---Add the gage locations for mapping ---
gages_locs <- suff_q %>%
  st_as_sf(coords = c('longitude','latitude'), crs = 4326)

st_write(gages_locs, 'data/intermediate/gages_6087_locations_251021.shp')

# --- Get a North America outline ---
north_america <- ne_countries(scale = "small", continent = "North America", returnclass = "sf") %>%
  filter(geounit %in% c('United States of America','Canada','Mexico'))

# Transform to match the projection used (same as ecoregions)
north_america <- st_transform(north_america, crs = st_crs(ecoregions)) %>%
  select(name)

map_plot <- ggplot() +
  geom_sf(data = ecoregions_simplified, aes(fill = ecoregion), color = "white", size = 0.2, alpha = 0.9) +
  geom_sf(data = north_america, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = gages_locs, shape = 16, size = 0.3, color = 'black')+
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

# Plot distribution of basin sizes by ecoregion
box_plot <- ggplot(basins_annotated, aes(x = ecoregion_label, y = are_km2, fill = ecoregion)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = eco_colors, guide = "none") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Basin Area Distribution by Ecoregion",
    x = "Ecoregion",
    y = "Area (sq km)"
  )+
  scale_y_log10()+
  theme(axis.text.y = element_text(size = 7),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 0))

cowplot::plot_grid(map_plot, box_plot, nrow = 1, rel_widths = c(1.25, 1), rel_heights = c(1, 0.75))
ggsave('fig/251009/ecoregion_test_loc.png', bg = 'white', height = 6, width = 12)

#merge and save for plotting
gages_locs %>%
  inner_join(basins_annotated %>%
               st_drop_geometry() %>%
               select(site_id, name, are_km2, latitud, longitd, ecoregion:ecoregion_label),
             by = c('gage_id' = 'site_id')) %>%
  st_write('data/intermediate/gage_location_annotated_6087_251021.shp')

