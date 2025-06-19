source("05_visualize/src/plot_maps.R")
source("05_visualize/src/viz_helpers.R")
source("05_visualize/src/build_maps.R")
source("05_visualize/src/build_map_matrix.R")
source("05_visualize/src/plot_breakdown_bars.R")
source("05_visualize/src/plot_breakdown_lines.R")
source("05_visualize/src/plot_breakdown_figs.R")
source("05_visualize/src/format_uncertainty_tbl.R")
source("05_visualize/src/map_of_van_metre_regions.R")
source("05_visualize/src/flux_geofacet_plot.R")
source("05_visualize/src/streamflow_percentiles_geofacet.R")
source("05_visualize/src/streamflow_baseflow_grid.R")

p5_targets <- list(
  # Model comparison breakdown --------------------
  # Plot bars
  tar_target(
    p5_breakdown_bars,
    plot_breakdown_bars(
      bar_data = p4_breakdown_bar_df_list,
      season_order = c("annual", "fall", "winter", "spring", "summer"),
      pal = c("#B9BA93", "#0067A2", "#D18F47")
    )
  ),

  # Plot lines
  tar_target(
    p5_breakdown_lines,
    plot_breakdown_lines(
      line_data = p4_breakdown_line_df_list,
      pal = c("#B9BA93", "#0067A2", "#D18F47"),
      water_yr = TRUE
    )
  ),

  # Generate model comparison breakdown figures
  tar_target(
    p5_breakdown_figures_png,
    plot_breakdown_figs(
      bar_figs = p5_breakdown_bars,
      line_figs = p5_breakdown_lines,
      bar_title_pattern = "%s Total",
      line_titles = c(
        "Total Timseries CONUS",
        "Average Annual Timeseries CONUS"
      ),
      y_labels_pattern = "%s (mm)",
      y_title_override = list(
        `Soil moisture percentage` = "Soil moisture saturation (percentage)"
      ),
      out_png_pattern = "05_visualize/out/Model_Comparison_%s.png"
    ),
    pattern = map(p5_breakdown_bars, p5_breakdown_lines),
    format = "file"
  ),

  # Map Matrix ---------------
  # Define variables for different figures
  tar_target(
    p5_map_matrix_viz_tbl,
    tibble::tibble(
      map_vars = p1_vars_flux_map,
      figure = "fluxes",
      map_data = c(
        setNames(p4_c404_summary_tifs, names(p1_vars_c404)),
        setNames(p4_ensemble_summary_tifs, p3_ensemble_tbl[["var_name"]])
      )[map_vars],
      legend_title = "mm",
      pal = c("davos", "vik", "lapaz"),
      pal_dir = c(-1, -1, -1),
      pal_offset = c(1, 0, 1),
      breaks = list(
        list(c(20, 35, 50, 70, 90, 105, 120, Inf)),
        list(c(10, 20, 30, 35, 45, 60, 80, Inf)),
        list(c(0.25, 1, 5, 10, 20, 30, 50, Inf))
      ),
      n_breaks = list(NULL, NULL, NULL)
    )
  ),

  # Get files for map matrix - this is necessary to maintain dependency tracking
  tarchetypes::tar_files(
    p5_map_matrix_files,
    p5_map_matrix_viz_tbl[["map_data"]]
  ),

  # Define matrix-wide arguments
  tar_target(
    p5_matrix_attribute_list,
    list(
      column_order = c(
        Annual = "full",
        Fall = "fall",
        Winter = "winter",
        Spring = "spring",
        Summer = "summer"
      ),
      maxcell = 5e+05,
      rel_widths = c(0.1, 1, 0.1, 1, 1, 1, 1),
      width = 6,
      height = 5
    )
  ),
  tar_target(
    p5_map_matrix_figs_png,
    build_map_matrix(
      map_attr_tbl = p5_map_matrix_viz_tbl,
      matrix_attr_list = p5_matrix_attribute_list,
      map_data = p5_map_matrix_files,
      hr_region_sf = p3_van_metre_simplified_sf,
      y_title_override = list(`Surface runoff` = "Streamflow"),
      out_path_pattern = "05_visualize/out/map_matrix_ensemble_%s.png",
      version = "full"
    )
  ),

  # Define matrix-wide arguments for seasonal map maptrix
  tar_target(
    p5_matrix_seasonal_attribute_list,
    list(
      column_order = c(
        Fall = "fall",
        Winter = "winter",
        Spring = "spring",
        Summer = "summer"
      ),
      maxcell = 5e+05,
      rel_widths = c(0.1, 1, 1, 1, 1),
      width = 6,
      height = 4
    )
  ),
  tar_target(
    p5_map_matrix_seasonal_figs_png,
    build_map_matrix(
      map_attr_tbl = p5_map_matrix_viz_tbl,
      matrix_attr_list = p5_matrix_seasonal_attribute_list,
      map_data = p5_map_matrix_files,
      hr_region_sf = p3_van_metre_simplified_sf,
      y_title_override = list(`Surface runoff` = "Streamflow"),
      out_path_pattern = "05_visualize/out/map_matrix_ensemble_seasonal_%s.png",
      version = "seasonal"
    )
  ),
  tar_target(
    p5_uncertainty_tbls,
    format_uncertainty_table(
      in_csv = p3_uncertainty_csvs,
      out_path_pattern = "05_visualize/out/uncertainty_table_%s.html",
      variable_name = unique(p3_uncertainty_tbl[["variable"]]),
      group_col = "Region",
      Region, Model
    ),
    format = "file",
    pattern = map(p3_uncertainty_csvs, p3_uncertainty_tbl)
  ),

  #### MAP OF VAN METRE REGIONS ####
  # plot van metre regions map
  tar_target(
    p5_van_metre_region_plot,
    van_metre_map(hr_region_sf = p3_van_metre_simplified_sf)
  ),

  #### FLUX GEOFACET PLOT ####
  # read in grid that determines geofacet layout
  tar_target(
    p5_vm_grid_csv,
    "05_visualize/in/van_metre_grid.csv",
    format = "file"
  ),

  tar_target(
    p5_van_metre_grid,
    readr::read_csv(p5_vm_grid_csv, col_types = "ccdd")
  ),

  # Create tibble of files being used for this figure with HUC level and
  # variable name precipitation, evapotranspiration, and surface runoff
  tar_target(
    p5_water_budget,
    tibble::tibble(
      file = c(
        p3_c404_precip_csv,
        grep("Evapotranspiration", p3_ensembled_model_csvs, value = TRUE),
        grep("Surface runoff", p3_ensembled_model_csvs, value = TRUE)
      ),
      huc_level = c(12, 12, 8),
      var_name = c("Precipitation", "Evapotranspiration", "Streamflow")
    )
  ),
  # summarize data from csvs p5_water_budget to regional and monthly means
  tar_target(
    p5_van_metre_means_plot,
    van_metre_monthly_means(
      water_budget = p5_water_budget,
      van_metre_xwalk = p3_van_metre_xwalk
    )
  ),

  # create base flux geofacet plot
  tar_target(
    p5_flux_geofacet_plot,
    flux_geofacet_plot(
      van_metre_means_plot = p5_van_metre_means_plot,
      van_metre_grid = p5_van_metre_grid
    )
  ),
  # save base flux geofacet plot as png
  tar_target(
    p5_flux_geofacet_base_png,
    ggplot2::ggsave(
      filename = "05_visualize/out/flux_geofacet_base.png",
      plot = p5_flux_geofacet_plot + ggplot2::theme(legend.position = "None"),
      width = 16,
      height = 9
    ),
    format = "file"
  ),
  # combine base plot with van metre region map with cowplot and save as png
  tar_target(
    p5_flux_geofacet_png,
    save_geofacet_w_map(
      geofacet = p5_flux_geofacet_plot,
      region_map = p5_van_metre_region_plot,
      plot_width = 16,
      plot_height = 9,
      background_color = "white",
      outfile = "05_visualize/out/new_flux_geofacet.pdf"
    ),
    format = "file"
  ),

  #### STREAMFLOW PERCENTILES GEOFACET PLOT ####
  # read in data
  tar_target(
    p5_huc12_streamflow_wide,
    readr::read_csv(
      grep("Surface runoff", p3_ensembled_huc12_csvs, value = TRUE),
      col_types = readr::cols()
    )
  ),
  # Get area-weighted means of monthly percentiles for each Van Metre region
  tar_target(
    p5_VM_AW_percentiles,
    vm_AreaWeighted_percentiles(
      percentile_values = list(0.05, 0.1, 0.5, 0.9, 0.95),
      huc12_streamflow_wide = p5_huc12_streamflow_wide,
      van_metre_xwalk = p3_van_metre_xwalk
    )
  ),
  # area-weighted means of monthly flow for each Van Metre region
  tar_target(
    p5_VM_AW_streamflow,
    vm_AreaWeighted(
      huc12_streamflow_wide = p5_huc12_streamflow_wide,
      van_metre_xwalk = p3_van_metre_xwalk
    )
  ),
  # plot streamflow percentile ribbons
  tar_target(
    p5_geom_percentile_ribbons,
    percentile_ribbons_geom(vm_percentiles_aw = p5_VM_AW_percentiles)
  ),
  # create base plot of streamflow and percentiles
  tar_target(
    p5_base_plot_percentile,
    percentile_base_plot(
      vm_streamflow_aw = p5_VM_AW_streamflow,
      ribbon_layers_single_year = p5_geom_percentile_ribbons,
      VM_AW_percentiles = p5_VM_AW_percentiles
    )
  ),
  # geofacet the base plot
  tar_target(
    p5_geofacet_plot_percentile,
    geofacet_percentiles(
      base_percentile_plot = p5_base_plot_percentile,
      van_metre_grid = p5_van_metre_grid
    )
  ),
  # combine base plot with van metre region map with cowplot and save as png
  tar_target(
    p5_percentiles_geofacet_png,
    save_geofacet_w_map(
      geofacet = p5_geofacet_plot_percentile,
      region_map = p5_van_metre_region_plot,
      plot_width = 16,
      plot_height = 9,
      background_color = "white",
      outfile = "05_visualize/out/percentiles_geofacet.pdf"
    ),
    format = "file"
  ),

  #### STREAMFLOW, BASEFLOW, BASEFLOW FRACTION ####

  # Create tibble of files being used for this figure with huc level and
  # variable name, summarize to monthly and regional means, and process data for
  # plotting baseflow, quickflow, and van metre crosswalk
  tar_target(
    p5_flow_subset_wide,
    wide_flow_subset(
      ensembled_model_csvs_BF = stringr::str_subset(
        p3_ensembled_model_csvs,
        "Baseflow"
      ),
      ensembled_model_csvs_QF = stringr::str_subset(
        p3_ensembled_model_csvs,
        "Quickflow"
      ),
      van_metre_xwalk = p3_van_metre_xwalk
    )
  ),
  # join processed data to the simplified border van metre region shape file
  tar_target(
    p5_flow_subset_sf,
    wide_flow_subset_sf(
      van_metre_regions_sf_simp = p3_van_metre_simplified_sf,
      flow_subset_wide = p5_flow_subset_wide
    )
  ),
  # create base plot (van metre region map)
  tar_target(
    p5_base_plot,
    plot_base(flow_subset_sf = p5_flow_subset_sf)
  ),
  # plot streamflow to van metre region centroids
  tar_target(
    p5_streamflow_ps_plot,
    flow_plot(
      base_plot = p5_base_plot,
      flow_subset_sf = p5_flow_subset_sf,
      var = Streamflow
    )
  ),
  # plot baseflow to van metre region centroids
  tar_target(
    p5_baseflow_ps_plot,
    flow_plot(
      base_plot = p5_base_plot,
      flow_subset_sf = p5_flow_subset_sf,
      var = Baseflow
    )
  ),
  # chloropleth map of baseflow fraction by van metre region
  tar_target(
    p5_baseflow_fraction_plot,
    baseflow_fract_plot(base_plot = p5_base_plot)
  ),
  # combine each plot into one figure
  tar_target(
    p5_streamflow_baseflow_grid_png,
    streamflow_baseflow_plot(
      fileout = "05_visualize/out/streamflow_baseflow_grid.png",
      flow_subset_sf = p5_flow_subset_sf,
      streamflow_ps_plot = p5_streamflow_ps_plot,
      baseflow_ps_plot = p5_baseflow_ps_plot,
      baseflow_fraction_plot = p5_baseflow_fraction_plot
    ),
    format = "file_fast"
  )
)
