### Identifying gages

# One basin per hydrologic landscape region (20); different than HUC, also USGS product; 
# Add classifier to ensure that some locations include a snotel site
# OWP: flashy, “normal”, snow
# Make sure that we have a mixture of these
# CAMELS are Gages II reference; so include some non-reference
# https://github.com/peckhams/nextgen_basin_repo 
# Use the collated data: nextgen_basin_repo/__Collated at main · peckhams/nextgen_basin_repo

setwd('LPlatt_NextGenBasinSubset')

library(tidyverse)

local_basin_file <- 'S_Peckham_NextGen_basins_info.tsv'
if(!file.exists(local_basin_file)) {
  read_tsv('https://raw.githubusercontent.com/peckhams/nextgen_basin_repo/main/__Collated/collated_basins_all.tsv') %>% 
    write_tsv(local_basin_file)
}

local_snotel_file <- 'S_Peckham_NextGen_snotel_info.tsv'
if(!file.exists(local_snotel_file)) {
  read_tsv('https://raw.githubusercontent.com/peckhams/nextgen_basin_repo/5e1317256a9365ae3a24a250358314e1e9ffc339/USDA_SNOTEL/Data/SNOTEL_Site_Info.tsv') %>% 
    mutate(HUC12 = str_extract(huc, '[0-9]{12}')) %>% 
    write_tsv(local_snotel_file)
}

# I couldn't find a column that already had SNOTEL site information, so 
# joined that in myself.
snotel_data <- read_tsv(local_snotel_file) %>% 
  mutate(Has_SNOTEL = 'Y') %>% 
  select(HUC = HUC12, Has_SNOTEL) %>% 
  distinct()
basin_info <- read_tsv(local_basin_file) %>% 
  left_join(snotel_data, by = 'HUC') %>% 
  replace_na(list(Has_SNOTEL = '-'))

# Final filtering and sampling
base_gages_list <- basin_info %>% 
  # A gage must be part of GAGES II (ref or not) & be a stream site
  filter(Is_GAGES2_Any == "Y",
         Site_Type == 'Stream',
         # Only keep types that fit into one of these 3 
         # (excludes unknown, slow, low-flow, and regulated)
         Hgraph_Type %in% c('normal', 'snow-dom', 'flashy')) 

# Summarize the base data we are starting with
base_gages_list_summary <- base_gages_list %>% 
  group_by(HLR_Code_Outlet) %>% 
  summarize(N_total = n(),
            N_CAMELS = sum(Is_CAMELS == "Y"),
            N_SNOTEL = sum(Has_SNOTEL == "Y"),
            N_owp_normal = sum(Hgraph_Type == 'normal'),
            N_owp_snow = sum(Hgraph_Type == 'snow-dom'),
            N_owp_flashy = sum(Hgraph_Type == 'flashy')) %>% 
  arrange(HLR_Code_Outlet)

# Create a file to upload with these data
write_csv(base_gages_list_summary, 'basin_details_summary.csv')

# Now choose a random set of sites, do it 10 times until we have a good distribution
set.seed(19)
possible_i <- sample(1:1000, size=10)
subset_list <- list()
for(i in possible_i) {
  set.seed(i)
  stations_subset <- base_gages_list %>% 
    group_by(HLR_Code_Outlet) %>% 
    sample_n(1)
  subset_list <- append(subset_list, list(stations_subset))
}

# Check which one(s) match our criteria
map(subset_list, ~{
  tibble(CAMELS = sum(.x$Is_CAMELS == "Y"),
         SNOTEL = sum(.x$Has_SNOTEL == "Y"),
         OWP_normal = sum(.x$Hgraph_Type == 'normal'),
         OWP_snow = sum(.x$Hgraph_Type == 'snow-dom'),
         OWP_flashy = sum(.x$Hgraph_Type == 'flashy'))
}) %>% bind_rows() %>% 
  mutate(list_id = row_number(), .before = everything()) %>% 
  # Now remove any rows where any of these are 0
  rowwise() %>% 
  filter(!if_any(everything(), ~ . == 0))

# The output:
# list_id CAMELS SNOTEL OWP_normal OWP_snow OWP_flashy
#   4      3      1         12        8          1
#   5      2      1         12        8          1
#   7      2      2         13        7          1


# I chose list_id == 4
final_station_sublist <- subset_list[[4]]

# Here's a summary of that data
final_station_sublist_summary <- final_station_sublist %>% 
  group_by(HLR_Code_Outlet) %>% 
  summarize(N_total = n(),
            N_CAMELS = sum(Is_CAMELS == "Y"),
            N_SNOTEL = sum(Has_SNOTEL == "Y"),
            N_owp_normal = sum(Hgraph_Type == 'normal'),
            N_owp_snow = sum(Hgraph_Type == 'snow-dom'),
            N_owp_flashy = sum(Hgraph_Type == 'flashy')) %>% 
  arrange(HLR_Code_Outlet)

final_station_sublist_summary %>% 
  select(-HLR_Code_Outlet) %>% 
  summarise(across(everything(), ~sum(.x, na.rm=TRUE)))

write_csv(final_station_sublist, 'basin_subset_all_details.csv')
final_station_sublist %>% 
  select(HLR_Code = HLR_Code_Outlet, 
         Site_ID, Site_Name, State_Code, Lon, Lat, Elev, 
         OWP_Type = Hgraph_Type, Is_CAMELS, Has_SNOTEL) %>% 
  # Convert yes/no to TRUE/FALSE
  mutate(Is_CAMELS = Is_CAMELS == 'Y',
         Has_SNOTEL = Has_SNOTEL == 'Y') %>% 
  write_csv('basin_subset.csv')
