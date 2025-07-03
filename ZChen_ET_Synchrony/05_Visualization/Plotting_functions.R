# Author: Zhaozhe Chen (zhaozhe.chen@wisc.edu)
# This code is to make plots

library(ggplot2)
library(cowplot)

# Theme for all plots
my_theme <- theme(
  #axis.line=element_line(color="black"),
  panel.background = element_blank(),
  panel.border = element_rect(colour="black",fill=NA),
  legend.key = element_blank(),
  #legend.key.size = unit(6,"cm"),
  #aspect.ratio = 1/1,
  #legend.key.size = unit(0.3,'cm'),
  legend.text = element_text(size=14),
  plot.title = element_text(size=14),
  axis.text = element_text(size=14),
  axis.title = element_text(size=14),
  legend.position = "none"
)

# This function is to print pdf and png figure
# Input is the figure g,title,width, and height
print_g <- function(g,title,w,h){
  pdf(paste0(Output_path,"/",title,".pdf"),
      width=w,height=h)
  print(g)
  dev.off()
  png(paste0(Output_path,"/",title,".png"),
      width=w,height=h,units = "in",
      res=600)
  print(g)
  dev.off()
}

# This function is to plot both the full time series
# And the annual cycle (mean across all years)
# And the distribution of data
# Input var_TS: The time series of variable to plot
# Input time: The time for the time series
# Input title: The title on the y axis

TS_plot <- function(var_TS,time,title){
  # Make a df
  df <- data.frame(time = time,
                   var = var_TS)
  # Plot the full time series
  g_all <- ggplot(df,aes(x = time,y=var))+
    geom_point(alpha = 0.3,size=2,color="black")+
    my_theme+
    labs(x = "",y=title)
  # Get DOY
  df <- df %>%
    mutate(DOY = yday(time)) %>%
    # Calculate daily mean across the years
    group_by(DOY) %>%
    mutate(mean = mean(var,na.rm=TRUE),
           Date = as.Date(format(time,"2020-%m-%d")))
  # Plot the annual cycle
  g_annual <- ggplot(df,aes(x = Date,y=var))+
    geom_point(alpha = 0.3,size=2,color="grey")+
    geom_line(aes(y=mean),linewidth=0.8,color="black")+
    my_theme+
    scale_x_date(date_breaks = "2 month",date_labels = "%b")+
    labs(x = "",y=title)
  # Plot the distribution of data
  g_hist <- ggplot(df,aes(var))+
    geom_histogram(bins = n_bin,color="black",position ="identity",alpha=0.5)+
    my_theme+
    labs(x=title)
  # Combine these three plots
  g <- plot_grid(g_annual,g_hist,nrow=1,
                 align = "hv",
                 axis="tblr")
  g <- plot_grid(g_all,g,nrow=2,
                 align = "h",
                 axis = "tblr")
  return(g)
}

# This function is to plot TE vs lag from var1 to var2
# Input is the TE_df
# And the name of the two variables varname1 and varname2
# And title of the plot
# norm: "None","Shannon" or "Theory"
TE_lag_plot <- function(TE_df,title,norm){
  if(norm == "None"){
    y_title <- "TE (bits)"
  }else{
    y_title <- "TE (%)"
  }
  # Plot of TE
  g_TE <- ggplot(data=TE_df,aes(x=Lag,y=TE))+
    geom_line(linewidth = 0.4)+
    #geom_ribbon(aes(ymin = TE-TE_se,ymax=TE+TE_se),alpha=0.3,color=NA)+
    geom_line(aes(y=cr_TE),linewidth=0.4,linetype = "dashed",color="royalblue2")+
    my_theme+
    labs(x = "Lag (days)",y=y_title,color="",fill="")+
    ggtitle(title)
  return(g_TE)
}



