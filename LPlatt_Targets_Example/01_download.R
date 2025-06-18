
# This file includes the target recipes for targets that download data

p1 <- list(
  
  # Declare USGS NWIS sites
  tar_target(p1_sites, 
             c('12150800',   # Snohomish River Near Monroe, WA
               '09070500',   # Colorado River Near Dotsero, CO
               '03185400'    # New River at Thurmond, WV
             )),
  
  # Download USGS NWIS daily discharge data
  tar_target(p1_flow_daily_tbl,
             readNWISdv(siteNumbers = p1_sites,
                        parameterCd = '00060',
                        startDate = '2022-10-01',
                        endDate = '2025-06-15') %>% 
               renameNWISColumns())
  
)
