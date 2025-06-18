library(targets)

tar_option_set(
  packages = c(
    'dplyr',
    'ggplot2',
    'dataRetrieval'
  )
)

# Load all targets by phase
source('01_download.R')
source('02_process.R')
source('03_visualize.R')

# Combine all targets from each phase recipe
c(p1, p2, p3)
