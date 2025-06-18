# Set-up -----------------------------------------
# Load function scripts
source("02_fetch/src/sb_login_token.R")
source("02_fetch/src/download_data.R")

p2_targets <- list(
  # HUCs ----------------------------------------
  # Download Mainstems data from ScienceBase
  tar_target(
    p2_mainstem_zip,
    sb_initialize_and_download(
      "63cb38b2d34e06fef14f40ad",
      names = "WBD_National_GDB.zip",
      destinations = "02_fetch/out/mainstems/WBD_National_GDB.zip"
    ),
    format = "file_fast"
  ),

  # CONUS404 (tabular) ----------------------------------------
  tar_target(
    p2_c404_tabular_nc,
    sb_initialize_and_download(
      sb_id = "64a6b54ad34ef77fcb063921",
      names = "huc12_monthly_conus404ba_WY1980_WY2021.nc",
      destinations = "02_fetch/out/conus404/huc12_monthly_conus404ba_WY1980_WY2021.nc"
    ),
    format = "file_fast"
  ),

  # Hydrolakes ----------------------------------------
  tar_target(
    p2_hydrolakes_zip,
    download_files(
      urls = "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10.gdb.zip",
      out_paths = "02_fetch/out/hydrolakes/HydroLAKES_polys_v10.gdb.zip"
    )
  ),

  # NHM - CONUS404 Bias-corrected -------------------
  tar_target(
    p2_nhm_raw_nc,
    sb_initialize_and_download(
      sb_id = "64120b48d34eb496d1cdcd40",
      names = "huc12_monthly_nhmprms_conus404ba_1980_2021.nc",
      destinations = "02_fetch/out/nhm/huc12_monthly_nhmprms_conus404ba_1980_2021.nc"
    ),
    format = "file_fast"
  ),

  # NHM HRU/HUC12 weights
  tar_target(
    p2_nhm_huc_wts_csv,
    sb_initialize_and_download(
      sb_id = "64120b48d34eb496d1cdcd40",
      names = "weights_hru_to_huc12_nhmprms_conus404ba.csv",
      destinations = "02_fetch/out/nhm/nhmprms_conus404ba_hru_to_huc12_weights.csv"
    ),
    format = "file_fast"
  ),

  # NHM OCONUS - CONUS404 Bias-corrected -------------------
  tarchetypes::tar_map(
    values = tibble::tibble(
      states = c("pr", "ak", "hi"),
      sb_ids = c(
        "6489d846d34ef77fcafe5bae", "667b23c8d34e6151c9d6be10",
        "667b2c75d34e6151c9d6bf6d"
      ),
      nc_files = c(
        "PR_huc12_monthly_nhmprms_daymet_1950_2021.nc",
        "AK_huc12_monthly_nhmprms_daymet_1980_2021.nc",
        "HI_huc12_monthly_nhmprms_daymet_1980_2021.nc"
      ),
      wts_csv_files = c(
        c(
          "PR_weights_hru_to_huc12_nhmprms_daymet.csv",
          "AK_weights_hru_to_huc12_nhmprms_daymet.csv",
          "HI_weights_hru_to_huc12_nhmprms_daymet.csv"
        )
      )
    ),
    tar_target(
      p2_nhm_oconus_raw_nc,
      sb_initialize_and_download(
        sb_id = sb_ids,
        names = nc_files,
        destinations = sprintf("02_fetch/out/nhm/%s", nc_files)
      ),
      format = "file_fast"
    ),

    # NHM HRU/HUC12 weights
    tar_target(
      p2_oconus_nhm_huc_wts_csv,
      sb_initialize_and_download(
        sb_id = sb_ids,
        names = wts_csv_files,
        destinations = sprintf("02_fetch/out/nhm/%s", wts_csv_files)
      ),
      format = "file_fast"
    ),
    names = states,
    description = NULL
  ),

  # WRF-HYDRO - CONUS404 Bias-corrected ----------------------------------------
  # Download CONUS404 data from controlled project ScienceBase page
  tar_target(
    p2_wrfhydro_raw_nc,
    sb_initialize_and_download(
      sb_id = "6411fd40d34eb496d1cdc99d",
      names = "huc12_monthly_wrfhydro_conus404ba_WY2010_2021.nc",
      destinations = sprintf(
        "02_fetch/out/wrfhydro/%s",
        "huc12_monthly_wrfhydro_conus404ba_WY2010_2021.nc"
      )
    ),
    format = "file_fast"
  ),

  # WaterWatch runoff ----------------------------------------
  tar_target(
    p2_waterwatch_zip,
    download_files(
      urls = "https://waterwatch.usgs.gov/download/?m=roplt3&dt=mv01d&ht=8&fmt=row",
      out_paths = "02_fetch/out/waterwatch/waterwatch.zip",
      ignore_existing = TRUE
    ),
    format = "file_fast"
  ),

  # GridMET precip ----------------------------------------
  # Build list of file names for NetCDF files
  tar_target(
    p2_gridmet_urls,
    sprintf(
      "https://www.northwestknowledge.net/metdata/data/pr_%s.nc",
      p1_calendar_years
    )
  ),

  # Download data and return file paths+names for .nc files
  tar_target(
    p2_gridmet_nc,
    download_files(
      urls = p2_gridmet_urls,
      dest_dir = "02_fetch/out/gridmet",
      ignore_existing = TRUE
    ),
    format = "file_fast"
  ),

  # SSEBOP ET ----------------------------------------
  # Build SSEBop file names
  tar_target(
    p2_ssebop_urls,
    sprintf(
      "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/global/monthly/etav5/downloads/m%s.zip",
      p1_months_Ym
    )
  ),

  # Download SSEBop data
  tar_target(
    p2_ssebop_zip,
    download_files(
      urls = p2_ssebop_urls,
      dest_dir = "02_fetch/out/ssebop",
      ignore_existing = TRUE
    ),
    format = "file_fast"
  ),

  # ESA-CCI soil moisture ----------------------------------------
  # List file names for daily  soil moisture NetCDF files
  tar_target(
    p2_cci_active_nc_urls,
    sprintf(
      "https://dap.ceda.ac.uk/neodc/esacci/soil_moisture/data/daily_files/ACTIVE/v07.1/%s/ESACCI-SOILMOISTURE-L3S-SSMS-ACTIVE-%s000000-fv07.1.nc",
      p1_days_Y,
      p1_days_YMD
    )
  ),
  tar_target(
    p2_cci_active_nc,
    download_files(
      urls = p2_cci_active_nc_urls,
      dest_dir = "02_fetch/out/esa-cci/esa-cci_active",
      ignore_existing = TRUE,
      resume = FALSE
    ),
    format = "file_fast"
  ),
  tar_target(
    p2_cci_combined_nc_urls,
    sprintf(
      "https://dap.ceda.ac.uk/neodc/esacci/soil_moisture/data/daily_files/COMBINED/v07.1/%s/ESACCI-SOILMOISTURE-L3S-SSMV-COMBINED-%s000000-fv07.1.nc",
      p1_days_Y,
      p1_days_YMD
    )
  ),
  tar_target(
    p2_cci_combined_nc,
    download_files(
      urls = p2_cci_combined_nc_urls,
      dest_dir = "02_fetch/out/esa-cci/esa-cci_combined",
      ignore_existing = TRUE,
      resume = FALSE
    ),
    format = "file_fast"
  ),

  # SNODAS SWE ----------------------------------------
  # Get missing SNODAS data file list
  tar_target(
    snodas_missing_data_txt,
    download_files(
      urls = "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/G02158_missing_files.txt",
      dest_dir = "02_fetch/out/snodas"
    ),
    format = "file_fast"
  ),

  # Remove dates that are missing from the SNODAS data
  tar_target(
    p2_snodas_dates,
    remove_missing_snodas(
      in_dates = p1_days_YMD,
      missing_files_txt = snodas_missing_data_txt
    )
  ),
  tar_target(
    p2_snodas_urls,
    sprintf(
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/%s/SNODAS_%s.tar",
      format(as.Date(p2_snodas_dates, "%Y%m%d"), format = "%Y/%m_%b"),
      p2_snodas_dates
    ),
  ),
  tar_target(
    p2_snodas_tar,
    download_files(
      urls = p2_snodas_urls,
      dest_dir = "02_fetch/out/snodas"
    ),
    format = "file_fast"
  ),

  # Van Metre Regions ----------------------------------------
  tar_target(
    p2_van_metre_boundaries_zip,
    sbtools::item_file_download(
      "6408e679d34e76f5f75e4f35",
      names = "Boundaries.zip",
      destinations = "02_fetch/out/van-metre/Boundaries.zip"
    ),
    format = "file_fast"
  ),
  tar_target(
    p2_van_metre_xwalk_csv,
    sb_initialize_and_download(
      "66833671d34e57e93663d8a5",
      names = "HUC12_HydroRegion_crosswalk.csv",
      destinations = "02_fetch/out/van-metre/HUC12_HydroRegion_crosswalk.csv"
    ),
    format = "file_fast"
  )
)
