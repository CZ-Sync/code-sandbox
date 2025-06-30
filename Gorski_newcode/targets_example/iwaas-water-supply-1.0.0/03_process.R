# Set-up -----------------------------------------
# Load function scripts
source("03_process/src/process_functions.R")
source("03_process/src/get_dates.R")
source("03_process/src/prep_huc_sf.R")
source("03_process/src/prep_hydrolakes.R")
source("03_process/src/prep_van_metre.R")
source("03_process/src/model_nc_process_functions.R")
source("03_process/src/ensemble_models.R")
source("03_process/src/extract_ensemble_huc12.R")
source("03_process/src/data_mask.R")
source("03_process/src/uncertainty_metrics.R")
source("03_process/src/hydrolakes_processing.R")

p3_targets <- list(
  # Van metre -----------------------------------
  tar_target(
    p3_van_metre_boundaries_shp,
    unzip_files(
      zip_files = p2_van_metre_boundaries_zip,
      path_out = "03_process/out",
      unzip_file_pattern = "HydrologicRegions.",
      file_out_pattern = "HydrologicRegions.shp$"
    ),
    format = "file_fast"
  ),
  
  tar_target(
    p3_van_metre_simplified_sf,
    prep_van_metre_sf(p3_van_metre_boundaries_shp)
  ),
  
  # HUCs ----------------------------------------
  tar_target(
    p3_mainstem_HUC12_gpkg,
    fix_mainstems_geom(
      in_zip = p2_mainstem_zip,
      layer = "WBDHU12",
      out_file = "03_process/out/mainstems_huc12.gpkg"
    ),
    format = "file_fast"
  ),
  
  tar_target(
    p3_mainstem_HUC8_gpkg,
    fix_mainstems_geom(
      in_zip = p2_mainstem_zip,
      layer = "WBDHU8",
      out_file = "03_process/out/mainstems_huc8.gpkg"
    ),
    format = "file_fast"
  ),
  
  # Read in HUC12 shapefile
  tar_target(
    p3_huc12_oconus_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC12_gpkg,
      huc_wt_csv = c(
        p2_oconus_nhm_huc_wts_csv_pr, p2_oconus_nhm_huc_wts_csv_ak,
        p2_oconus_nhm_huc_wts_csv_hi
      ),
      huc_wt_trshld = 0.8,
      crs_out = NULL, # Retain the CRS of the input dataset
      oconus = TRUE
    )
  ),
  
  tarchetypes::tar_group_by(
    p3_huc12_oconus_sf_grouped,
    p3_huc12_oconus_sf |> 
      mutate(
        HUC2 = str_sub(HUC, 1, 2),
        region_group = case_when(
          HUC2 == "19" ~ "AK",
          HUC2 == "20" ~ "HI",
          HUC2 == "21" ~ "PR",
          .default = "CONUS"
        )
      ) |> 
      filter(region_group != "CONUS") |> 
      group_by(region_group),
    region_group
  ),
  
  tar_target(
    p3_huc12_extract_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC12_gpkg,
      huc_wt_csv = p2_nhm_huc_wts_csv,
      huc_wt_trshld = 0.8,
      crs_out = p1_crs_for_extracting
    )
  ),
  
  tar_target(
    p3_huc12_plot_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC12_gpkg,
      huc_wt_csv = p2_nhm_huc_wts_csv,
      huc_wt_trshld = 0.8,
      crs_out = p1_crs_for_plotting,
      exclude_non_plot_hucs = TRUE
    )
  ),
  
  # Read in HUC8 shapefile
  tarchetypes::tar_group_by(
    p3_huc8_oconus_grouped_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC8_gpkg,
      crs_out = NULL,  # Retain the CRS of the input dataset
      oconus = TRUE
    ) |> 
      mutate(
        HUC2 = str_sub(HUC, 1, 2),
        region_group = case_when(
          HUC2 == "19" ~ "AK",
          HUC2 == "20" ~ "HI",
          HUC2 == "21" ~ "PR",
          .default = "CONUS"
        )
      ) |> 
      filter(region_group != "CONUS") |> 
      group_by(region_group),
    region_group
  ),
  
  tar_target(
    p3_huc8_plot_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC8_gpkg,
      crs_out = p1_crs_for_plotting,
      exclude_non_plot_hucs = TRUE
    )
  ),
  
  tar_target(
    p3_huc8_extract_sf,
    prepare_hucs(
      huc_path = p3_mainstem_HUC8_gpkg,
      crs_out = p1_crs_for_extracting
    )
  ),

  # Van Metre Region Crosswalk ----
  tar_target(
    p3_van_metre_xwalk,
    prep_vm_crosswalk(
      in_csv = p2_van_metre_xwalk_csv,
      huc_sf = p3_huc12_oconus_sf,
      col_types = cols(Area_sqkm = col_number(), .default = col_character())
    )
  ),

  # CONUS404 - Precip ------------------
  tar_target(
    p3_c404_precip_csv,
    model_nc_extract_vars_to_csv(
      nc_file = p2_c404_tabular_nc,
      method = "CONUS404",
      var_names =  c(Precipitation = "RAIN"),
      month_filter = p1_months_Y_m, 
      out_path_pattern = "03_process/out/huc_summaries/c404bc_%s.csv" # varname then units
    ),
    format = "file"
  ),
  
  # Hydrolakes - Lake storage --------------------
  tar_target(
    p3_hydrolakes_gdb,
    unzip_files(
      zip_files = p2_hydrolakes_zip,
      path_out = "03_process/out/",
      file_out_pattern = ".."
    )
  ),
  
  tar_target(
    p3_huc12_lake_storage,
    hydrolakes_to_huc12(
      hydrolakes_gdb = p3_hydrolakes_gdb,
      mainstems_gpkg = p3_mainstem_HUC12_gpkg,
      chunk_size = 1000
    )
  ),

  tar_target(
    p3_hydrolakes_csv,
    prep_hydrolakes(
      in_df = p3_huc12_lake_storage,
      var_name = p1_vars_hydrolakes,
      out_pattern = "03_process/out/huc_summaries/hydrolakes_%s.csv",
      xwalk = p3_van_metre_xwalk,
      huc_col = "HUC12",
      xwalk_huc_col = "HUC12"
    ),
    format = "file"
  ),
  
  # NHM ---------
  tar_target(
    p3_nhm_csvs,
    model_nc_extract_vars_to_csv(
      nc_file = c(
        p2_nhm_raw_nc, p2_nhm_oconus_raw_nc_pr, p2_nhm_oconus_raw_nc_ak,
        p2_nhm_oconus_raw_nc_hi
      ),
      method = "NHM",
      var_names =  p1_vars_nhm,
      month_filter = p1_months_Y_m, 
      scale_factors = list(`Soil moisture percentage` = 100),
      convert_to_huc8 = c(
        "Surface runoff",
        "Soil moisture volume",
        "Soil moisture percentage"
      ),
      out_path_pattern = "03_process/out/huc_summaries/nhm_c404bc_%s.csv" # varname then units
    ),
    format = "file"
  ),

  # WRF-HYDRO ---------
  tar_target(
    p3_wrfhydro_csvs,
    model_nc_extract_vars_to_csv(
      nc_file = p2_wrfhydro_raw_nc,
      method = "WRF-Hydro",
      var_names = p1_vars_wrfhydro,
      month_filter = p1_months_Y_m,
      scale_factors = list(`Soil moisture percentage` = 100),
      convert_to_huc8 = c(
        "Surface runoff",
        "Soil moisture volume",
        "Soil moisture percentage"
      ),
      out_path_pattern = "03_process/out/huc_summaries/wrfhydro_c404bc_%s.csv", # varname then units
    ),
    format = "file"
  ),

  # Ensemble models
  # Build table with variables and paths
  tar_target(
    p3_ensemble_tbl,
    build_ensemble_tbl(
      nhm_vars = p1_vars_nhm,
      nhm_files = p3_nhm_csvs,
      wrfhydro_vars = p1_vars_wrfhydro,
      wrfhydro_files = p3_wrfhydro_csvs
    )
  ),
  
  # Create branched file targets for file paths to track upstream changes
  tarchetypes::tar_files(
    p3_ensemble_nhm_files,
    pull(p3_ensemble_tbl, "nhm_path")
  ),
  
  tarchetypes::tar_files(
    p3_ensemble_wrf_files,
    pull(p3_ensemble_tbl, "wrfhydro_path")
  ),
  
  # Ensemble models using a mean
  tar_target(
    p3_ensembled_model_csvs,
    ensemble_models(
      var_tbl_row = p3_ensemble_tbl,
      out_path_pattern = "03_process/out/huc_summaries/model_ensemble_%s.csv",
      nhm_file = p3_ensemble_nhm_files,
      wrf_file = p3_ensemble_wrf_files,
      huc_col = "HUC"
    ),
    format = "file",
    pattern = map(p3_ensemble_tbl, p3_ensemble_nhm_files, p3_ensemble_wrf_files)
  ),

  # Get Streamflow and Soil moisture at HUC12 because several variables are
  #   output to the HUC8 scale. See the `convert_to_huc8` argument of
  #   `model_nc_extract_vars_to_csv()` above for those variables.
  tar_target(
    p2_huc12_rebuild_vars,
    c("Surface runoff", "Soil moisture volume", "Soil moisture percentage")
  ),

  tar_target(
    p3_ensembled_huc12_csvs,
    get_model_huc12(
      var_to_rerun = p2_huc12_rebuild_vars,
      nc_files = list(
        nhm = c(
          p2_nhm_raw_nc, p2_nhm_oconus_raw_nc_pr, p2_nhm_oconus_raw_nc_ak,
          p2_nhm_oconus_raw_nc_hi
        ),
        wrfhydro = p2_wrfhydro_raw_nc
      ),
      model_vars = list(nhm = p1_vars_nhm, wrfhydro = p1_vars_wrfhydro),
      model_csvs = list(nhm = p3_nhm_csvs, wrfhydro = p3_wrfhydro_csvs),
      month_filter = p1_months_Y_m,
      scale_factors = list(`Soil moisture percentage` = 100),
      out_path_pattern_ind = "03_process/out/huc_summaries/%s_c404bc_%s_huc12.csv",
      out_path_pattern_ens = "03_process/out/huc_summaries/model_ensemble_huc12_%s.csv"
    ),
    format = "file",
    pattern = map(p2_huc12_rebuild_vars)
  ),

  # WaterWatch ------------------------------------------
  tar_target(
    p3_waterwatch_txt,
    unzip_files(
      zip_files = p2_waterwatch_zip,
      path_out = "03_process/out",
      unzip_file_pattern = "mv01d_row_data.txt",
      file_out_pattern = "|"
    ),
    format = "file"
  ),
  
  tar_target(
    p3_waterwatch_exct_csv,
    write_to_csv(
      in_file = p3_waterwatch_txt,
      out_file = "03_process/out/huc_summaries/waterwatch_monthly.csv",
      # Tab delimited
      delim = "\t",
      # Read in only HUC or columns withing the analysis period
      col_select = all_of(c("HUC", p1_months_Y_m)),
      # All columns numeric except for HUC column, which is a character
      col_types = cols(col_character(), .default = col_number())
    ),
    format = "file"
  ),
  
  # GridMET --------------------------------------------------------
  # Get dates for daily gridmet rasters
  tar_target(
    p3_gridmet_dates,
    get_gridmet_dates(rast(p2_gridmet_nc))
  ),
  
  # Sum daily data by month
  tar_target(
    p3_gridmet_monthly_tif,
    aggregate_monthly(
      in_data = p2_gridmet_nc,
      out_path = "03_process/out/gridmet_monthly.tif",
      in_dates = p3_gridmet_dates,
      date_subset = p1_days
    )
  ),
  
  # SSEBOP ------------------------------------------------
  # Zip files -> monthly SpatRaster
  tar_target(
    p3_ssebop_monthly_tif,
    unzip_monthly_rasters(
      zip_files = p2_ssebop_zip,
      out_path = "03_process/out/ssebop_monthly.tif",
      layer_name_date_pattern = "\\d{6}",
      name_replacement_pattern = c("^(.{4})(.*)$" = "\\1_\\2"),
      crop_latlon = c(-130, -60, 20, 60),
      memfrac = 0.8
    ),
    format = "file"
  ),
  
  # ESA-CCI Soil Moisture -------------------------------------
  # Read in soil moisture rasters
  tar_target(
    p3_cci_active_rast,
    combine_rasters(
      in_rast_files = p2_cci_active_nc,
      out_path = "03_process/out/cci_active_daily.tif"
    ),
    format = "file"
  ),
  
  tar_target(
    p3_cci_combined_rast,
    combine_rasters(
      in_rast_files = p2_cci_combined_nc,
      out_path = "03_process/out/cci_combined_daily.tif"
    ),
    format = "file"
  ),
  
  # Get dates for daily ESA-CCI raster layers
  tar_target(
    p3_cci_active_dates,
    as.Date(names(rast(p3_cci_active_rast)))
  ),
  
  tar_target(
    p3_cci_combined_dates,
    as.Date(names(rast(p3_cci_combined_rast)))
  ),
  
  # Average daily data by month
  tar_target(
    p3_cci_active_monthly_tif,
    aggregate_monthly(
      in_data = p3_cci_active_rast,
      out_path = "03_process/out/cci_active_monthly.tif",
      in_dates = p3_cci_active_dates,
      .fun = mean
    ),
    format = "file"
  ),
  
  # Average daily data by month
  tar_target(
    p3_cci_combined_monthly_tif,
    aggregate_monthly(
      in_data = p3_cci_combined_rast,
      out_path = "03_process/out/cci_combined_monthly.tif",
      in_dates = p3_cci_combined_dates,
      .fun = mean
    ),
    format = "file"
  ),
  
  # SNODAS SWE -------------------------------------------------
  # Unzip/Aggregate monthly
  tar_target(
    p3_snodas_monthly_tif,
    daily_zip_to_monthly_rast(
      in_zip = p2_snodas_tar,
      out_path = "03_process/out/snodas_monthly.tif",
      method = "SNODAS",
      dates = get_snodas_dates(p2_snodas_tar),
      .fun = mean
    ),
    format = "file"
  ),
  
  # Prep data for extraction ----------------------
  # Vector of raster tifs
  # tar_files maintains dependency tracking of list of file paths
  tarchetypes::tar_files(
    p3_extraction_raster_tifs,
    c(p3_gridmet_monthly_tif, p3_ssebop_monthly_tif, p3_cci_active_monthly_tif,
      p3_cci_combined_monthly_tif, p3_snodas_monthly_tif),
    format = "file"
  ),
  
  # These are the polygon sf scales to extract to. This is based on the desired
  #  resolution of the analysis, which is driven by the benchmark resolution.
  #  They must be in the same order as p3_extraction_raster_tifs.
  tar_target(
    p3_extraction_spatial_scale,
    c("huc12", "huc12","huc8", "huc8", "huc12")
  ),
  
  # Extract tifs to wide dataframes ----------------------
  tar_target(
    p3_monthly_huc_summaries_csv,
    extract_awm(
      raster_data = p3_extraction_raster_tifs,
      polygon_data = list(
        huc8 = p3_huc8_extract_sf,
        huc12 = p3_huc12_extract_sf
      ),
      spatial_scale = p3_extraction_spatial_scale,
      out_path_pattern = "03_process/out/huc_summaries/%s",
      id_col = "HUC",
      max_cells_in_memory = 5e+08
    ),
    format = "file",
    pattern = map(p3_extraction_raster_tifs, p3_extraction_spatial_scale)
  ),
  
  # Calculate uncertainty metrics ------------------
  # Build table to organize data
  tarchetypes::tar_group_by(
    p3_uncertainty_tbl,
    build_uncertainty_tbl(
      benchmark_names = p1_benchmark_datasets,
      c404_vars = p1_vars_c404,
      nhm_vars = p1_vars_nhm,
      wrfhydro_vars = p1_vars_wrfhydro,
      benchmark_csvs = c(p3_monthly_huc_summaries_csv, p3_waterwatch_exct_csv),
      c404_csvs = p3_c404_precip_csv,
      nhm_csvs = p3_nhm_csvs,
      wrfhydro_csvs = p3_wrfhydro_csvs,
      ensemble_tbl = p3_ensemble_tbl,
      ensemble_csvs = p3_ensembled_model_csvs
    ),
    variable
  ),
  
  # TODO: Make target that is a list of paths to datasets of SWE data
  
  # Make SWE data mask for soil moisture data
  # TODO: branch over datasets/models
  tar_target(
    p3_swe_data_mask_df,
    mask_by_swe(
      csv_swe = filter(p3_uncertainty_tbl, benchmark == "snodas")$benchmark_data[1],
      df_xwalk = p3_van_metre_xwalk,
      swe_threshold = 1,
      area_threshold = 0.25,
      swe_huc12_col = "HUC",
      xwalk_cols = c(huc8 = "HUC8", huc12 = "HUC12", area = "Area_sqkm")
    )
  ),
  
  # TODO: Apply SWE mask to soil moisture data
  
  # TODO: Update uncertainty table with masked data 
  #   OR: update calc_uncertainty_metrics to take override arguments where
  #       masked data can be passed in (preferable option?)

  #
  tar_target(
    p3_uncertainty_csvs,
    calc_uncertainty_metrics(
      uncert_tbl = p3_uncertainty_tbl,
      out_path_pattern = "03_process/out/uncertainty_metrics/uncertainty_metrics_%s.csv",
      huc_col = "HUC",
      xwalk = p3_van_metre_xwalk,
      xwalk_reg_col = "Region_nam",
      xwalk_huc_col = "HUC12",
      xwalk_huc_col_override = list(
        `Surface runoff` = "HUC8",
        `Soil moisture volume` = "HUC8",
        `Soil moisture percentage` = "HUC8"
      )
    ),
    format = "file",
    pattern = map(p3_uncertainty_tbl),
    iteration = "list"
  )
)
