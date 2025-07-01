# ET Synchrony
This repository contains the full workflow for the analysis of ET synchrony project.
A test dataset of daily scale AmeriFlux data is provided in 00_Data

## Overview
Write overview here
## 01_Data_processing
### 01.1 QC and pre-processing
`01.1_AMF_pre-processing.R` conducts QC for raw AmeriFlux dataset, aggregate half-hourly data to hourly, output variables include:
- **SWC**: Soil Water Content (Unit: %)
- **TA**: Air temperature (Unit: degree C)
- **VPD**: Vapor pressure deficit (Unit: hPa)
- **P_F**: Precipitation (Unit: mm)
- **WS**: Wind speed (Unit: ms-1)
- **USTAR**: Friction velocity (Unit: ms-1)
- **NETRAD**: Net radiation (Unit: Wm-2)
- **TS**: Soil temperature (Unit: degree C)
- **LE_F**: Latent heat flux (Unit: Wm-2)
