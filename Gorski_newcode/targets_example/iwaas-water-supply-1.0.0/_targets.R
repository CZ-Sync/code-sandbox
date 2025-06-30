# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    # 02_fetch
    "tidyverse",
    "sbtools",
    # 03_process
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
    # 04_viz_prep
    "data.table",
    # 05_visualize
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
  workspace_on_error = TRUE
)

# Phase target makefiles:
source("01_configuration.R")
source("02_fetch.R")
source("03_process.R")
source("04_viz_prep.R")
source("05_visualize.R")

# Target list
c(p1_targets, p2_targets, p3_targets, p4_targets, p5_targets)
