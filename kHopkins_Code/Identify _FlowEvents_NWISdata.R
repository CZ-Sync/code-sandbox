
# Krissy Hopkins
# 11/5/2019

############################################################################
### Using Unit/Instantaneous Data ###
#####################################
library(dataRetrieval) # USGS-R package to get NWIS data
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


# Set parameters
startDate <- "2018-08-14"
endDate <- "2019-12-12"

# Download gage height
parameterCd <- c("00065")  #Discharge 00060
# Precip 00045
# Gage Height 00065

# Set site number. Check the site name
siteNumbers <- c("0208734795","0208735460", "0208734210")
siteINFO <- readNWISsite(siteNumbers)
siteINFO$station_nm

streamflow <- renameNWISColumns(readNWISdata(sites=siteNumbers, startDate=startDate, endDate=endDate, service="iv", tz="UTC", parameterCd = parameterCd))
streamflow = streamflow[,c(2,3,4)]
streamflow$unit = "feet"
colnames(streamflow) = c("site", "dateTime", "Depth", "Unit")


# Download Precip
siteNumbers <- c("354528078372645")
parameterCd <- c("00045")  
precip <- renameNWISColumns(readNWISdata(sites=siteNumbers, startDate=startDate, endDate=endDate, service="iv", tz="UTC", parameterCd = parameterCd))
precip = precip[,c(2,3,4)]
precip$unit = "inches"
colnames(precip) = c("site", "dateTime", "Depth", "Unit")

# combine streamflow and precip
##################################################################################

df = rbind(streamflow, precip)
rm(precip, streamflow)

df$site[df$site == "0208734795"] = "Walnut Creek S. Wilmington"
df$site[df$site == "0208735460"] = "Walnut Creek S. State"
df$site[df$site == "0208734210"] = "Walnut Creek Trailwood"

df$site[df$site == "354528078372645"] = "Precip (S. State)"


###################################################################################

library(ggplot2)
library(ggthemes)
library(plotly)

p = ggplot(data = df,
       aes(x = dateTime, 
           y = Depth, 
           color = site))+
  geom_line()+
  theme_few()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("blue", "lightblue", "black"))
  
ggplotly(p)

###################################################################################
##### Plot Data 
library(dygraphs)
library(xts)

Q <- xts(x = streamflow2[,-1], order.by = streamflow2$dateTime)

dygraph(Q, main = "Events", ylab = "Discharge (cfS)") %>% 
  dySeries("Precip_inches", axis = 'y2', color = "blue",drawPoints = TRUE, pointSize = 2, strokeWidth=1, fillGraph = TRUE) %>% 
  dySeries("Flow_Inst_cfs", fillGraph = TRUE, color = "orange", axis = 'y', drawPoints = TRUE, pointSize = 2, strokeWidth=1) %>%  
  dyAxis("y2", valueRange = c(0.5,0), label = "Precip (inches)") %>% 
  dyRangeSelector(dateWindow = c("2019-01-01", "2019-03-20"), height = 50, strokeColor = "") %>%
  dyOptions(useDataTimezone = TRUE) %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.5,
              hideOnMouseOut = FALSE) %>%
  dyLegend(show = "auto")
  

##################################################################################
# Run baseflow separation filter
# Must not have an NAs for streamflow (filter out NAs with na.omit())
# Use 0.99 for the filter parameter 
library(EcoHydRology)
#detach(package:dplyr)
library(dplyr)

Q = streamflow2 %>% na.omit()
Q = Q[,1:2]
Q_Base <- BaseflowSeparation(Q$Flow_Inst_cfs, filter_parameter = 0.99, passes = 3)
Q_Base <- cbind(Q, Q_Base)  
colnames(Q_Base) <- c("dateTime", "Q_cfs", "Qbase_cfs", "Qquick_cfs")

rm(Q)
###########################################################################
# Create dataframe with timesteps every 5-min
# Join with the flow data
Q_Com <- data.frame(seq.POSIXt(as.POSIXct("2018-09-30 4:00", tz="UTC"),
                               as.POSIXct("2019-10-02 03:55", tz="UTC"), 
                               by="5 mins"))
colnames(Q_Com) <- "dateTime"
final <- left_join(Q_Com, Q_Base, by="dateTime")

rm(Q_Base, Q_Com)

###########################################################################
# Round very low baseflow and quickflow values to zero
final$Qbase_cfs[final$Qbase_cfs < 0.01] <- 0
final$Qquick_cfs[final$Qquick_cfs < 0.01] <- 0


##############################
# Calculate Rolling minimums #
##############################
# Values are the number of time intervals
# I'm using 5-min intervals
library(zoo)

final$Qquick_leadrollmin_24hr <- rollapply(final$Qquick_cfs, 288, fill = NA, align = "left", min) # lead, falling limb
final$Qquick_lagrollmin_12hr <- rollapply(final$Qquick_cfs, 144, fill = NA, align = "right", min) # lag rising limb

# Calculate quickflow minus rolling mins
final = final %>% 
  mutate(Qquick_minLead24hr = Qquick_cfs - Qquick_leadrollmin_24hr) %>% 
  mutate(Qquick_minlag12hr = Qquick_cfs - Qquick_lagrollmin_12hr) 

#########################
# Create indicator code #
#########################
# Use for TR104 and TR109 and Cabin
final$EventInd <- 0
final$EventInd[final$Qquick_cfs > 0.25] <- 1
final$EventInd[final$Q_cfs > 2] <- 1
final$EventInd[final$Qquick_minLead24hr > 0.25] <- 1
final$EventInd[final$Qquick_minlag12hr > 0.25] <- 1

####################
# check indicators #
####################
library(ggthemes)

# Plot indicator
ggplot(final %>% 
         filter(dateTime >= as.POSIXct("2019-07-01") & 
                  dateTime <= as.POSIXct("2019-07-25")), 
       aes(dateTime, Qquick_cfs, color = factor(EventInd))) + 
  geom_point(size = 1)+
  scale_colour_brewer(palette = "Set1")+
  ylim(0,5)+
  theme_few()+
  labs(colour = "Variable", y = "Q (cfs)", 
       title = "Rock Branch")+
  geom_hline(yintercept = 2, color = "black", size=1)

################################################################################
# Make eventlengthd_df to find out lengths of time periods between rain events #
################################################################################
# Uses rle () run legnth to determine the lengths of time betweek rain events
# Calculate event characteristics

library(tidyr)
library(reshape)
library(plyr)

colnames(final)[1] <- "Date"
stormeventlengths_df <- as.data.frame(cbind(rle(final$EventInd)$lengths, rle(final$EventInd)$values))
colnames(stormeventlengths_df) <- c("length", "indicator")
stormeventlengths_df$qLength <- cumsum(stormeventlengths_df$length)

stormeventlengths_df$hours <- NA  # add hours column

# use difftime() to calculate time difference between first row of flow recording to the end of that first event, first qLength
start1 <- final[1,"Date"]
end1 <- final[(stormeventlengths_df[1,"qLength"]),"Date"]
time1 <- as.numeric(difftime(end1,start1, units="hours"))


#######################################################################################
# function to calculate all but the first time difference of continuously recorded 0/1s
#######################################################################################
calcEventHours <- function(x){
  start <- final[(stormeventlengths_df[x-1,"qLength"]),"Date"]
  end <- final[(stormeventlengths_df[x,"qLength"]),"Date"]
  return(as.numeric(difftime(end,start, units="hours")))
}
#######################################################################################
# END function 

# calculate the length of all other periods for hours column 
# make a vector of the rest of the events to calculate the time of

events_vector <- c(2:nrow(stormeventlengths_df))

# apply function to calculate the rest of the events to fill in the rest of the hours column

stormeventlengths_df$hours <- c(time1, sapply(events_vector, calcEventHours))

#######################################################################################

# set threshold amount of time (hours) for no stormflow
noFlow_T = 6

# for continuously recorded no flow periods (indicator == 0), add TRUE/FALSE depending on whether duration exceeds noFlow_T
stormeventlengths_df$dry_duration <- NA
stormeventlengths_df$dry_duration[(stormeventlengths_df$hours>noFlow_T & stormeventlengths_df$indicator==0)] <- TRUE
stormeventlengths_df$dry_duration[(stormeventlengths_df$hours<noFlow_T & stormeventlengths_df$indicator==0) ] <- FALSE

# identify the start of flow events that meet minimum inter-event time
# event.starts is a vector of the rows in the events.df which have long enough dry periods
eventstarts <- as.numeric(as.vector(row.names(stormeventlengths_df[which(stormeventlengths_df$dry_duration==TRUE),])))

#################################################################################################
# function that makes a dataframe of a storm events that meet inter-event time threshold
#################################################################################################

makeEventdf <- function(x){
  start.row <- stormeventlengths_df[eventstarts[x],"qLength"]     # starting row finds a row in eventlengths_df that meets criteria and finds the end of the dry period
  end.row <- stormeventlengths_df[eventstarts[x+1]-1,"qLength"]   # ending row looks for the row before the next row that meets criteria, ie. finds the end of the wet period right before the next long enough dry period
  event.df <- final[(start.row+1):end.row,]              # subset the rain data using these rows, add one to start row because that is the end of the dry period
  return(event.df)  
}

#################################################################################################
# END function that makes data frame of a precipitation event that meets inter-event time threshold
#################################################################################################


# number of storm events is the length of eventstarts -1
number_storm_events <- length(eventstarts)-1

# Calculate stats
storm_events_list <- lapply(c(1:number_storm_events), makeEventdf) 

start.time <- sapply(storm_events_list, function(x) strftime(x[1,"Date"], tz = "UTC"))

end.time <- sapply(storm_events_list, function(x) strftime(x[nrow(x),"Date"], tz = "UTC"))

prior.dry.times.hrs <- sapply(c(1:number_storm_events), function(x) round(stormeventlengths_df[eventstarts[x],]$hours,2))

minQbase_cfs <- sapply(storm_events_list, function(x) min(x$Qbase_cfs))

time.max.flow.cfs <- sapply(storm_events_list, function(x) strftime(x[which.max(x$Q_cfs),"Date"], tz = "UTC"))

max.flow.cfs <- sapply(storm_events_list, function(x) max(x$Q_cfs))

min.flow.cfs <- sapply(storm_events_list, function(x) min(x$Q_cfs))

duration.mins <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"Date"],x[1,"Date"], units="mins")),digits=0))

duration.hrs <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"Date"],x[1,"Date"], units="hours")),digits=2))

totalflow.cf_5min <- sapply(storm_events_list, function(x) sum(x$Q_cfs*60*5))

totalquickflow.cf_5min <- sapply(storm_events_list, function(x) sum(x$Qquick_cfs*60*5))

totalbaseflow.cf_5min <- sapply(storm_events_list, function(x) sum(x$Qbase_cfs*60*5))


all.storm.events.info <-
  cbind(as.data.frame(cbind(start.time, end.time)),
        as.data.frame(cbind(time.max.flow.cfs, 
                             max.flow.cfs, 
                            min.flow.cfs,
                            prior.dry.times.hrs,
                            minQbase_cfs,
                            totalflow.cf_5min,
                            totalquickflow.cf_5min,
                            totalbaseflow.cf_5min,
                            duration.mins,
                            duration.hrs)))

all.storm.events.info = all.storm.events.info %>%
  mutate_all(as.character) %>% 
  mutate_at(vars(max.flow.cfs, min.flow.cfs, prior.dry.times.hrs,minQbase_cfs,
                 totalflow.cf_5min, totalquickflow.cf_5min, totalbaseflow.cf_5min,
                 duration.mins, duration.hrs), as.numeric)

all.storm.events.info$Q_Range <- all.storm.events.info$max.flow.cfs - all.storm.events.info$min.flow.cfs

finalstormevents = all.storm.events.info %>% 
  filter(storm.events_duration.mins > 15) %>% 
  filter(Q_Range > 0.25)

finalstormevents$start.time <- as.POSIXct(strptime(finalstormevents$start.time, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
finalstormevents$end.time <- as.POSIXct(strptime(finalstormevents$end.time, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
finalstormevents$time.max.flow.cfs <- as.POSIXct(strptime(finalstormevents$time.max.flow.cfs, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

finalstormevents$TimeToPeak_hrs = round(difftime(finalstormevents$time.max.flow.cfs, finalstormevents$start.time, tz = "UTC", units = "hours"),2)

write.csv(finalstormevents, "/Users/khopkins/Documents/R/StormID/RockyBranch_WY2019.csv", row.names = FALSE)


