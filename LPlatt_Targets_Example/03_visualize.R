
# This file includes the target recipes for targets that summarize the data

p3 <- list(
  
  # Example of grouping data to use for mapping in future target
  tar_target(p3_flow_daily_grp, 
             p1_flow_daily_tbl %>% 
               group_by(site_no) %>% 
               tar_group(),
             iteration = 'group'),
  
  # This will use each group's data as input and create a branched target per group
  # It is also a file target meaning it creates a file and tracks changes to that
  # file. So, I name it with the file extension as the suffix and you have to 
  # return the filepath saved AND add `format = 'file'` to the `tar_target()` recipe
  tar_target(p3_hydrographs_png, {
    
    # Prepare filename to save
    out_file <- sprintf('hydrograph_%s.png', unique(p3_flow_daily_grp$site_no))
    
    # Create a ggplot object
    p <- ggplot(p3_flow_daily_grp, aes(x = Date, y = Flow)) +
      geom_line() +
      ylab('Flow, cfs') +
      ggtitle(sprintf('Hydrograph for %s', unique(p3_flow_daily_grp$site_no))) +
      theme_bw()
    
    # Save the plot as a PNG file
    ggsave(out_file, p, width = 7, height = 5, units = 'in', dpi = 150)
    
    # Return the file path
    return(out_file)
    
  }, pattern = map(p3_flow_daily_grp), format = 'file'),
  
  # Create a boxplot of monthly average flow featuring all sites
  tar_target(p3_flow_boxplot_png, {
    
    # Prepare filename to save
    out_file <- 'flow_boxplot.png'
    
    # Create a ggplot object
    p <- ggplot(p2_flow_monthly_tbl, aes(x = site_no, y = Flow_monthly)) +
      geom_boxplot() +
      ylab('Flow, cfs') +
      ggtitle('Boxplot of average monthly flow for each site') +
      theme_bw()
    
    # Save the plot as a PNG file
    ggsave(out_file, p, width = 5, height = 5, units = 'in', dpi = 150)
    
    # Return the file path
    return(out_file)
    
  }, format = 'file')
  
)
