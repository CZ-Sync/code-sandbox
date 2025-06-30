# Load scripts
source("01_configuration/src/years_to_days.R")
source("01_configuration/src/years_to_months.R")

# Check if login credentials are stored securely
# If not, read README.md for instructions how
p1_targets <- list(

  # Define parameters  --------------------
  # Define water years of interest - must be a vector
  tar_target(p1_water_years, 2010:2020),
  tar_target(p1_crs_for_extracting, "EPSG:4326"),
  tar_target(p1_crs_for_plotting, "EPSG:5070"),

  # Reformat dates --------------------
  # Days in analysis period
  tar_target(p1_days, water_years_to_days(p1_water_years, "%Y-%m-%d")),
  tar_target(p1_days_YMD, water_years_to_days(p1_water_years, "%Y%m%d")),
  tar_target(p1_days_Y, water_years_to_days(p1_water_years, "%Y")),

  # Months in analysis in period
  tar_target(p1_months_Y_m, water_years_to_months(p1_water_years, "%Y_%m")),
  tar_target(p1_months_Ym, water_years_to_months(p1_water_years, "%Y%m")),

  # Calendar years included (partially) in analysis period
  tar_target(
    p1_calendar_years,
    seq(min(p1_water_years) - 1, max(p1_water_years))
  ),

  # Define CONUS404, NHM, and WRF-Hydro tibbles --------------------

  #  Values should be a named character vector. Names should be the variable and
  #   character elements should be the name of the NHM/WRF-Hydro component that
  #   corresponds to the variable. Character values can also be an equation with
  #   the new variable name on the left-hand side and the simple formula to
  #   perform the equation on the right-hand side; must be able to able to be
  #   applied to numeric data frame and return a data frame (+, -, *)

  # Define CONUS404 variables (see below for format details)
  tar_target(p1_vars_c404, c(Precipitation = "RAIN")),
  tar_target(p1_vars_hydrolakes, c(`Lake storage` = "Lake storage")),

  # All required named variables from NHM
  tar_target(
    p1_vars_nhm,
    c(
      ssres_flow = "nhm_ssres_flow_post",
      sroff = "nhm_sroff_post",
      Baseflow = "nhm_gwres_flow_post",
      Quickflow = "Quickflow = ssres_flow + sroff",
      Evapotranspiration = "nhm_actet_post",
      `Surface runoff` = "Streamflow = Baseflow + Quickflow",
      `Soil moisture volume` = "nhm_soil_moisture_total_depth_post",
      `Soil moisture percentage` = "nhm_soil_moisture_fraction",
      `Snow water equivalent` = "nhm_pkwater_equiv_post"
    )
  ),

  # All required named variables from WRF-Hydro
  tar_target(
    p1_vars_wrfhydro,
    c(
      Baseflow = "Baseflow",
      Quickflow = "Surfaceflow",
      Evapotranspiration = "ET",
      `Surface runoff` = "Streamflow = Baseflow + Quickflow",
      `Soil moisture volume` = "SoilWater",
      `Soil moisture percentage` = "avgSOILSAT_wltadj_top1",
      `Snow water equivalent` = "SWE"
    )
  ),

  # Variable names required for plotting maps (rows)
  tar_target(
    p1_vars_flux_map,
    c("Precipitation", "Evapotranspiration", "Surface runoff")
  ),

  # Define breakdown figure parameters ---------------
  tar_target(
    p1_benchmark_datasets,
    c(
      Precipitation = "gridmet",
      Evapotranspiration = "ssebop",
      `Surface runoff` = "waterwatch",
      `Soil moisture volume` = "cci_combined",
      `Soil moisture percentage` = "cci_active",
      `Snow water equivalent` = "snodas"
    )
  ),
  tar_target(
    p1_breakdown_fig_vars,
    c(
      "Precipitation",
      "Surface runoff",
      "Evapotranspiration",
      "Soil moisture percentage",
      "Snow water equivalent"
    )
  )
)
