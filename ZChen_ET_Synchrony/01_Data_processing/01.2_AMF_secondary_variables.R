# Author: Zhaozhe Chen
# Update date: 2025.7.1

# This code is to calculate secondary variables from cleaned AmeriFlux dataset
# The calculated secondary variables include:
# Soil water potential
# PET
# 




# ---------- Global ---------------
library(dplyr)
library(here)

# Input path to cleaned AMF dataset
AMF_path <- ""