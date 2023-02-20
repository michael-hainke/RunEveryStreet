library(tidyverse)  # data manipulation
library(rStrava)    # Strava API functions
library(httr)       # make API requests
library(config)     # config file for credentials
library(jsonlite)   # work with JSON files
library(googleway)  # Google API functions
library(geosphere)  # calculate distances

########################################
### Strava and Google Authentication ###
########################################

# Get Strava Credentials and Authorize
strava <- config::get("strava")
app_scope = 'activity:read_all'
cache = TRUE

token <- httr::config(token = strava_oauth(strava$app_name, strava$client_id, strava$secret, app_scope, cache))

# Get Google Credentials and Authorize
google <- config::get("google")

#####################################
### Get Activity Data from Strava ###
#####################################

# read existing data
df_final <- readRDS("df_final.rds")

# get all activities
my_acts <- get_activity_list(token)
df_my_acts <- compile_activities(my_acts)

# update latest activity
activity = 1
act <- get_activity_streams(my_acts, token, acts = activity)

# snap to road and join to data
i = 1
act_snap <- NULL
while (i <= length(act$lat)) {
  if (i+99 > length(act$lat)) { j = length(act$lat) } else { j = i+99 }
  act_temp <- act %>% slice(i:j) %>% select(lat, lng)
  act_temp <- google_snapToRoads(df_path = act_temp, lat='lat', lon='lng', interpolate = FALSE, key = google$api_key)
  act_temp <- as.data.frame(act_temp$snappedPoints$location) %>% select(snap_lat=latitude, snap_lon=longitude)
  act_snap <- rbind(act_snap,act_temp)
  i = i + 100
}

act_snap <- act_snap %>%
            mutate(name = my_acts[[activity]]$name,
                   ID = my_acts[[activity]]$id,
                   dt = str_sub(my_acts[[activity]]$start_date_local,1,10),
                   moving_time = my_acts[[activity]]$moving_time,
                   dist = my_acts[[activity]]$distance,
                   distance = 0,
                   duplicate = 0,
                   id = row_number())

# Calc Snapped Distance & Duplicate Points
for (i in 2:length(act_snap$snap_lon)) {
  dist_haver = distHaversine(c(act_snap$snap_lon[i],act_snap$snap_lat[i]),c(act_snap$snap_lon[(i-1)],act_snap$snap_lat[(i-1)]))
  act_snap$distance[i] = dist_haver
  if (i > 4 ) {
    for (j in 1:(i-4)) {
      if ( distHaversine(c(act_snap$snap_lon[i],act_snap$snap_lat[i]),c(act_snap$snap_lon[(j)],act_snap$snap_lat[(j)])) < 3 ) {
        act_snap$duplicate[i] = 1
        break
        }
    }
  }
}

# more duplicates (if non duplicate between 2 duplicate pts then assign as duplicate)
for (i in 2:(length(act_snap$snap_lon)-2)) {
  if ( act_snap$duplicate[(i-1)] == 1 & act_snap$duplicate[i] == 0 & sum(act_snap$duplicate[(i+1):(i+2)]) > 0 ) { act_snap$duplicate[i] = 1 }
}

########################################
# Check for new roads from entire list #
########################################

df_check <- df_final %>%
            filter(ID %in% c('8334455438','8355595543'),
                   duplicate == 0)

for (i in 1:length(act_snap$snap_lon)) {
    if(act_snap$duplicate[i]==0) {
    for (j in 1:length(df_check$ID)) {
      if ( distHaversine(c(act_snap$snap_lon[i],act_snap$snap_lat[i]),c(df_check$snap_lon[(j)],df_check$snap_lat[(j)])) < 3 ) {
        act_snap$duplicate[i] = 1
        break
      }
    }
  }
}

# more duplicates (if non duplicate between 2 duplicate pts then assign as duplicate)
for (i in 2:(length(act_snap$snap_lon)-2)) {
  if ( act_snap$duplicate[(i-1)] == 1 & act_snap$duplicate[i] == 0 & sum(act_snap$duplicate[(i+1):(i+2)]) > 0 ) { act_snap$duplicate[i] = 1 }
}



# Add to overall dataframe
df_final <- rbind(df_final,act_snap)

# Save New file
saveRDS(df_final, "df_final.rds")

# new roads
act_snap %>% group_by(duplicate) %>% summarise(dist = sum(distance))


# Graph Single Run
loc = c(median(act_snap$snap_lat),median(act_snap$snap_lon))
google_map(key = google$api_key, location=loc, data = act_snap) %>%
  add_polylines(data = act_snap, lat='snap_lat', lon='snap_lon')

# Graph All Runs
df_polylines <- NULL

for (act_id in unique(df_final$ID)) {
  df <- df_final %>% filter(ID == act_id)
  polyline <- encode_pl(lat=df$snap_lat, lon=df$snap_lon)
  df_polylines <- rbind(df_polylines,data.frame(dt=df$dt[1], name=df$name[1], ID=act_id, polyline=polyline))
}

df_polylines <- df_polylines %>% mutate(description = paste0(dt," ",name))

loc = c(median(df_final$snap_lat),median(df_final$snap_lon))
pic <- google_map(key = google$api_key, location=loc, data = df_polylines) %>%
  add_polylines(data = df_polylines, polyline = "polyline", mouse_over = "name",
                stroke_weight = 2 )
pic
