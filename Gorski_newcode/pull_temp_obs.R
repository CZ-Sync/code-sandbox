library(dataRetrieval)
library(tidyverse)
library(qs)
library(yaml)
library(sf)
library(mapview)
library(lubridate)
library(ncdf4)


source('2_nwis_pull/src/inventory_nwis.R')
source('2_nwis_pull/src/nwis_pull.R')

inv_ind <- ''
nwis_pull_params <- yaml::read_yaml('2_nwis_pull/cfg/nwis_pull_params.yml')
service <- 'uv'
all_dat <- inventory_nwis(inv_ind, nwis_pull_params, service)

all_dat <- dataRetrieval::whatNWISdata(huc = '01', service = 'uv', parameterCd = '00010')

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

all_dat_sf <- all_dat %>% 
  filter(!is.na(dec_long_va)) %>%
  sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = st_crs(5703))

mapview(all_dat_sf, zcol = 'count_nu')



#pull data
site_data <- readNWISuv('01073319', parameterCd = '00010', tz = 'America/New_York')

site_data %>%
  mutate(datetime = as_datetime(dateTime)) %>%
  mutate(date = date(dateTime)) %>%
  filter(date > '2022-07-01' & date < '2022-07-10') %>%
  mutate(temp_roll = zoo::rollapply(X_00010_00000, 4, mean, align = 'right', fill = NA)) %>%
  ggplot(aes(x = dateTime, y = temp_roll))+
  geom_point()+
  scale_x_datetime(date_labels = "%H:%M", date_breaks = '6 hour')

             
# site_data %>%
#   mutate(datetime = as_datetime(dateTime)) %>%
#   mutate(date = date(dateTime)) %>%
#   filter(date == '2024-10-01') %>%
#   select(dateTime, X_00010_00000)

#open the CONUS404 data
# Open the NetCDF file
nc_data <- nc_open("2_nwis_pull/out/01073319_saved_on_disk.nc")

# Read specific variable from the NetCDF file
temp <- ncvar_get(nc_data, "T2")  
time <- ncvar_get(nc_data, "time")  
# Close the NetCDF file
nc_close(nc_data)


start_date <- as.POSIXct("1979-10-01", tz = "UTC")
datetime <- start_date + (time * 3600)  # Convert hours to seconds

site_temp <- tibble(datetime = datetime, air_temp = temp) %>%
  mutate(date = date(datetime), Hour = hour(datetime)) %>%
  group_by(date, Hour) %>% 
  summarise(air_temp=mean(air_temp)) %>%
  mutate(datetime = as_datetime(paste0(date, ' ',str_pad(Hour, 2, pad = '0'),':', '00:00'))) %>%
  ungroup() %>%
  select(datetime, air_temp)

site_temp %>%
  ggplot(aes(x = datetime, y = air_temp))+
  geom_line()
#

#merge air temp with stream temp
stream_temp <- site_data %>%
  mutate(datetime = as_datetime(dateTime)) %>%
  mutate(date = date(dateTime)) %>%
  filter(date > '2020-07-01' & date < '2020-07-10') %>%
  
  mutate(date = date(dateTime), Hour = hour(dateTime)) %>%
  group_by(date, Hour) %>% 
  summarise(stream_temp=mean(X_00010_00000)) %>%
  mutate(datetime = as_datetime(paste0(date, ' ',str_pad(Hour, 2, pad = '0'),':', '00:00'))) %>%
  ungroup %>%
  select(datetime, stream_temp) 
  

stream_temp %>%
  left_join(site_temp, by = 'datetime') %>%
  pivot_longer(cols = stream_temp:air_temp, names_to = 'stream_air', values_to = 'temp') %>%
  ggplot(aes(x = datetime, y = temp, color = stream_air))+
  geom_line()+
  facet_wrap(~stream_air, scales = 'free', nrow = 2)


########
#Subset data for single data of processing

proc_date <- '2020-07-02'
air_temp_day <- site_temp %>%
  mutate(hour = hour(datetime), date = date(datetime)) %>%
  filter(date == proc_date) %>%
  select(hour, air_temp) %>%
  rename('mean' = 'air_temp')

# Estimate the true mean and amplitude
air_true_mean <- mean(air_temp_day$mean, na.rm = TRUE)
air_true_max <- max(air_temp_day$mean, na.rm = TRUE)
air_true_min <- min(air_temp_day$mean, na.rm = TRUE)
air_true_amplitude <- (air_true_max - air_true_min) / 2

  
stream_temp_day <- stream_temp %>%
  mutate(hour = hour(datetime), date = date(datetime)) %>%
  filter(date == proc_date) %>%
  select(hour, stream_temp) %>%
  rename('mean' = 'stream_temp')

# Sinusoidal model fitting function
sinusoidal_model <- function(x, mean, amplitude, phase) {
  mean + amplitude * sin((2 * pi / 24) * (x - phase))
}

# Fit model to the observed data for air temperature
fit_air <- nls(air_temp_day ~ sinusoidal_model(hour, mean, amplitude, phase),
               start = list(mean = mean, amplitude = air_true_amplitude, phase = 0))




# Extract parameters from the fit
params_air <- coef(fit_air)
mean_air <- params_air['mean']
amplitude_air <- params_air['amplitude']
phase_air <- params_air['phase']

# Fit model to the observed data for water temperature
fit_water <- nls(observed_water_temp ~ sinusoidal_model(hours, mean, amplitude, phase),
                 start = list(mean = true_mean, amplitude = true_amplitude, phase = 0))

# Extract parameters from the fit
params_water <- coef(fit_water)
mean_water <- params_water['mean']
amplitude_water <- params_water['amplitude']
phase_water <- params_water['phase']

# Calculate root mean squared error (RMSE)
fit_air_vals <- predict(fit_air)
fit_water_vals <- predict(fit_water)

rmse_air <- sqrt(mean((fit_air_vals - observed_air_temp) ^ 2))
rmse_water <- sqrt(mean((fit_water_vals - observed_water_temp) ^ 2))

# Calculate explained variance
ss_total_air <- sum((observed_air_temp - mean(observed_air_temp)) ^ 2)
ss_residual_air <- sum((observed_air_temp - fit_air_vals) ^ 2)
explained_variance_air <- 1 - (ss_residual_air / ss_total_air)

ss_total_water <- sum((observed_water_temp - mean(observed_water_temp)) ^ 2)
ss_residual_water <- sum((observed_water_temp - fit_water_vals) ^ 2)
explained_variance_water <- 1 - (ss_residual_water / ss_total_water)

# Check for good fits (explained variance threshold)
threshold <- 0.70  # 70%
if (explained_variance_air > threshold && explained_variance_water > threshold) {
  cat("Both fits are satisfactory.\n")
} else {
  cat("At least one fit is unsatisfactory.\n")
}

# Calculate metrics
AR_d <- amplitude_water / amplitude_air  # Amplitude Ratio
PL_d <- (phase_water - phase_air) * (24 / (2 * pi))  # Phase Lag in hours
MR_d <- mean_water / mean_air  # Mean Ratio

# Show results
cat(sprintf("Mean Air Temp: %.2f, Amplitude Air Temp: %.2f, Phase Air Temp: %.2f\n", mean_air, amplitude_air, phase_air))
cat(sprintf("Mean Water Temp: %.2f, Amplitude Water Temp: %.2f, Phase Water Temp: %.2f\n", mean_water, amplitude_water, phase_water))
cat(sprintf("Root Mean Squared Error (Air): %.2f, Explained Variance (Air): %.2f\n", rmse_air, explained_variance_air))
cat(sprintf("Root Mean Squared Error (Water): %.2f, Explained Variance (Water): %.2f\n", rmse_water, explained_variance_water))
cat(sprintf("Amplitude Ratio (AR_d): %.2f\n", AR_d))
cat(sprintf("Phase Lag (PL_d): %.2f hours\n", PL_d))
cat(sprintf("Mean Ratio (MR_d): %.2f\n", MR_d))

# Plotting the results
observed_data <- data.frame(hours, observed_air_temp, observed_water_temp, 
                            fit_air = fit_air_vals, fit_water = fit_water_vals)

ggplot(observed_data, aes(x = hours)) +
  geom_point(aes(y = observed_air_temp), color = 'red', alpha = 0.5) +
  geom_line(aes(y = fit_air), color = 'darkred') +
  labs(title = "Air Temperature Fit", y = "Temperature (°C)") +
  theme_minimal() +
  xlab("Hours") +
  theme(legend.position = "bottom") +
  scale_y_continuous(name = "Air Temperature (°C)", limits = c(10, 30))

ggplot(observed_data, aes(x = hours)) +
  geom_point(aes(y = observed_water_temp), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = fit_water), color = 'darkblue') +
  labs(title = "Water Temperature Fit", y = "Temperature (°C)") +
  theme_minimal() +
  xlab("Hours") +
  theme(legend.position = "bottom") +
  scale_y_continuous(name = "Water Temperature (°C)", limits = c(10, 30))