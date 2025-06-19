# Set-up -----------------------------------------
# Load function scripts
source("04_viz_prep/src/prep_viz_data.R")
source("04_viz_prep/src/model_summary_fxns.R")
source("04_viz_prep/src/build_breakdown_var_tbl.R")
source("04_viz_prep/src/build_breakdown_fig_data.R")

p4_targets <- list(

  # Summarize data for map matrices ----------------------
  # CONUS404 - summarize data
  tar_target(
    p4_c404_summary_csvs,
    map_matrix_model_summaries(
      model_csv_files = p3_c404_precip_csv,
      var_names = p1_vars_c404,
      periods = c("full", "seasonal"),
      out_path_pattern = "04_viz_prep/out/summarized_c404_%s.csv"
    ),
    format = "file"
  ),

  # NHM/WRF-Hydro model ensemble - summarize data
  tar_target(
    p4_ensemble_summary_csvs,
    map_matrix_model_summaries(
      model_csv_files = p3_ensembled_model_csvs,
      var_names = p3_ensemble_tbl[["var_name"]],
      periods = c("full", "seasonal"),
      out_path_pattern = "04_viz_prep/out/summarized_ensembled_%s.csv",
      dataset_override = list(`Surface runoff` = stringr::str_subset(
        p3_ensembled_huc12_csvs,
        "Surface runoff"
      ))
    ),
    format = "file"
  ),

  # NHM/WRF-Hydro SWE - get average annual maximum SWE
  tar_target(
    p4_ensemble_swe_summary_csv,
    map_matrix_model_summary_max_swe(
      model_csv_files = p3_ensembled_model_csvs,
      var_name = "Snow water equivalent",
      out_path_pattern = "04_viz_prep/out/summarized_ensembled_avg_annual_max_%s.csv"
    ),
    format = "file"
  ),

  # Rasterize data for map matrices ----------------------
  # CONUS404 - Rasterize summaries
  tar_target(
    p4_c404_summary_tifs,
    rasterize_summaries(
      sf = p3_huc12_plot_sf,
      summary_csvs = p4_c404_summary_csvs,
      var_names = p1_vars_c404,
      res = 1000,
      out_path_pattern = "04_viz_prep/out/c404_summaries_%s.tif",
      sf_override = NULL
    ),
    format = "file"
  ),

  # Hydrolakes - Rasterize summaries
  tar_target(
    p4_hydrolakes_summary_tif,
    rasterize_summaries(
      sf = p3_huc12_plot_sf,
      summary_csvs = p3_hydrolakes_csv,
      var_names = p1_vars_hydrolakes,
      res = 1000,
      out_path_pattern = "04_viz_prep/out/hydrolakes_summaries_%s.tif",
      sf_override = NULL
    ),
    format = "file"
  ),
  tar_target(
    p4_hydrolakes_oconus_summary_tif,
    rasterize_oconus_summaries(
      sf = p3_huc12_oconus_sf_grouped,
      summary_csvs = p3_hydrolakes_csv,
      crs_list = list(AK = "EPSG:3338", HI = "ESRI:102007", PR = "EPSG:2866"),
      var_names = p1_vars_hydrolakes,
      res = 1000,
      out_path_pattern = "04_viz_prep/out/hydrolakes_summaries_%s_%s.tif",
      sf_override = NULL
    ),
    format = "file",
    pattern = map(p3_huc12_oconus_sf_grouped, p3_huc8_oconus_grouped_sf),
    iteration = "list"
  ),

  # NHM/WRF-Hydro Ensemble - Rasterize summaries
  tar_target(
    p4_ensemble_summary_tifs,
    rasterize_summaries(
      sf = p3_huc12_plot_sf,
      summary_csvs = p4_ensemble_summary_csvs,
      var_names = p3_ensemble_tbl[["var_name"]],
      res = 1000,
      out_path_pattern = "04_viz_prep/out/ensemble_summaries_%s.tif",
      sf_override = NULL
    ),
    format = "file"
  ),
  tar_target(
    p4_ensemble_oconus_summary_tifs,
    rasterize_oconus_summaries(
      sf = p3_huc12_oconus_sf_grouped,
      summary_csvs = c(p4_ensemble_summary_csvs, p4_c404_summary_csvs),
      crs_list = list(AK = "EPSG:3338", HI = "ESRI:102007", PR = "EPSG:2866"),
      var_names = c(p3_ensemble_tbl[["var_name"]], p1_vars_c404),
      res = 1000,
      out_path_pattern = "04_viz_prep/out/ensemble_summaries_%s_%s.tif",
      sf_override = NULL
    ),
    format = "file",
    pattern = map(p3_huc12_oconus_sf_grouped, p3_huc8_oconus_grouped_sf),
    iteration = "list"
  ),

  # NHM/WRF-Hydro SWE - Rasterize summary
  tar_target(
    p4_max_swe_summary_tif,
    rasterize_summaries(
      sf = p3_huc12_plot_sf,
      summary_csvs = p4_ensemble_swe_summary_csv,
      var_names = "Snow water equivalent",
      res = 1000,
      out_path_pattern = "04_viz_prep/out/ensemble_summaries_avg_annual_max_%s.tif",
      sf_override = NULL
    ),
    format = "file"
  ),
  tar_target(
    p4_max_swe_oconus_summary_tif,
    rasterize_oconus_summaries(
      sf = p3_huc12_oconus_sf_grouped,
      summary_csvs = p4_ensemble_swe_summary_csv,
      crs_list = list(AK = "EPSG:3338", HI = "ESRI:102007", PR = "EPSG:2866"),
      var_names = "Snow water equivalent",
      res = 1000,
      out_path_pattern = "04_viz_prep/out/ensemble_summaries_avg_annual_max_%s_%s.tif",
      sf_override = NULL
    ),
    format = "file",
    pattern = map(p3_huc12_oconus_sf_grouped, p3_huc8_oconus_grouped_sf),
    iteration = "list"
  ),

  # Summarize data for breakdown figures -----------------
  # Build breakdown variable tibble
  tar_target(
    p4_breakdown_var_tbl,
    build_breakdown_var_tbl(
      figs_variables = p1_breakdown_fig_vars,
      benchmark_names = p1_benchmark_datasets,
      c404_vars = p1_vars_c404,
      nhm_vars = p1_vars_nhm,
      wrfhydro_vars = p1_vars_wrfhydro,
      benchmark_csvs = c(p3_monthly_huc_summaries_csv, p3_waterwatch_exct_csv),
      c404_csvs = p3_c404_precip_csv,
      nhm_csvs = p3_nhm_csvs,
      wrfhydro_csvs = p3_wrfhydro_csvs,
      huc_col = "HUC",
      xwalk_huc_col = "HUC12"
    )
  ),

  # Build line chart data
  tar_target(
    p4_breakdown_line_df_list,
    prep_all_breakdown_data(
      var_tbl = p4_breakdown_var_tbl,
      figure_method = "line",
      vm_xwalk = p3_van_metre_xwalk,
      area_col = "Area_sqkm",
      huc8_override = list(
        `Surface runoff` = "HUC8",
        `Soil moisture volume` = "HUC8",
        `Soil moisture percentage` = "HUC8"
      )
    )
  ),

  # Build bar chart data
  tar_target(
    p4_breakdown_bar_df_list,
    prep_all_breakdown_data(
      var_tbl = p4_breakdown_var_tbl,
      figure_method = "bar",
      vm_xwalk = p3_van_metre_xwalk,
      area_col = "Area_sqkm",
      reg_col = "Region",
      agg_reg_col = "AggRegion_nam",
      huc8_override = list(
        `Surface runoff` = "HUC8",
        `Soil moisture volume` = "HUC8",
        `Soil moisture percentage` = "HUC8"
      )
    )
  )
)
