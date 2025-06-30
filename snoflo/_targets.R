# Load packages required to define the pipeline:
library(targets)

# Set target options:
# I've added some packages from another project, but we should change this as needed
tar_option_set(
  packages = c(
    # 02_fetch
    "tidyverse",
    "sf",
    # 02_data_ingest
    # 03_metrics
    "assertthat",
    "archive",
    "bit64",
    "fasterize",
    "ncdf4",
    "raster",
    "sf",
    "terra", # Need at least version 1.6-17
    "exactextractr",
    "units",
    "hydroGOF",
    # 04_trends
    # 05_joint_metrics
    # 06_te
    # 07_summarize
    "scico",
    "dataRetrieval",
    "geofacet",
    "rmapshaper",
    "spData",
    "tidyterra",
    "cowplot",
    "ggthemes",
    "grid",
    "gt",
    "RColorBrewer"
  ),
  workspace_on_error = TRUE,
  #this is the default, set to "none" if you don't want anything saved
  storage = 'worker'
)

# Phase target makefiles:
source("01_basin_match.R")
source("02_data_match.R")
source("03_metric.R")
source("04_trends.R")
source("05_joint_metrics.R")
source("06_te.R")
source("07_summarize.R")
