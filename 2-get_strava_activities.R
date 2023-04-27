# Load Required Packages
library(tidyverse)  # data manipulation
library(rStrava)    # Strava API functions
library(httr)       # make API requests
library(config)     # config file for credentials
library(jsonlite)   # work with JSON files
library(googleway)  # Google API functions
library(geosphere)  # calculate distances

# Load Required Functions
source("run_every_street_functions.R")

# Load API Credentials
strava <- config::get("strava")
google <- config::get("google")

# Get Strava API token
app_scope = 'activity:read_all'
cache = TRUE
token <- httr::config(token = strava_oauth(strava$app_name, strava$client_id, strava$secret, app_scope, cache))

# Get All Strava Activities
my_acts <- get_activity_list(token)
df_my_acts <- compile_activities(my_acts)

################################
### Update Raw Activity Data ###
################################

# Get Latest Raw Activity Stream From Strava (activity =  1 is the last activity uploaded to Strava)
activity = 1
df_activity_raw <- get_activity_raw(my_acts, token, activity)

# Add to Dataframe and Save
df_strava_detail_raw <- rbind(readRDS("Data/strava_activity_detail_raw.rds"), df_activity_raw)
saveRDS(df_strava_detail_raw, "Data/strava_activity_detail_raw.rds")

##############################################
### Update 'Snapped to Road' Activity Data ###
##############################################

# Snap Raw Activity Data to nearest road using Google 'Snap to Roads' API
df_activity_snap <- activity_snap_to_road(df_activity_raw,df_my_acts[activity,],google$api_key)

# Add to Dataframe and Save
df_strava_detail_snap <- rbind(readRDS("Data/strava_activity_detail_snap.rds"), df_activity_snap)
saveRDS(df_strava_detail_snap, "Data/strava_activity_detail_snap.rds")

####################################
### Update Activity Summary Data ###
####################################

# Get Summary Data from Strava and Encode Snapped Route to a Polyline Using Google API
activity_summary <- df_my_acts[activity,] %>%
                    mutate(dt = as.Date(start_date_local),
                           snap_polyline = encode_pl(lat=df_activity_snap$lat, lon=df_activity_snap$lon)) %>%
                    select(strava_id = id, 
                           name, 
                           dt, 
                           distance, 
                           elapsed_time, 
                           moving_time, 
                           total_elevation_gain, 
                           start_latlng1, 
                           start_latlng2, 
                           end_latlng1, 
                           end_latlng2, 
                           map.summary_polyline,
                           snap_polyline)


# Add to Dataframe and Save
df_strava_summary <- rbind(activity_summary,readRDS("Data/strava_activity_summary.rds"))
saveRDS(df_strava_summary, "Data/strava_activity_summary.rds")

##################################
### Map Completed Activities ###
##################################

# Map Snapped Activity Data Using Google Maps API
map_activities(df_strava_summary, 'snap_polyline', google$api_key)

# Map Raw Activity Data Using Google Maps API  
map_activities(df_strava_summary, 'map.summary_polyline', google$api_key)

