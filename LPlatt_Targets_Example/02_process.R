
# This file includes the target recipes for targets that summarize the data

p2 <- list(
  
  tar_target(p2_flow_monthly_tbl,
             p1_flow_daily_tbl %>% 
               mutate(year_month = format(Date, '%Y_%m')) %>% 
               group_by(site_no, year_month) %>% 
               summarize(Flow_monthly = mean(Flow, na.rm=TRUE)))
  
)
