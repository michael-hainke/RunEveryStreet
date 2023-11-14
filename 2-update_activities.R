# Load Required Packages
library(googleway)   # Google API functions
library(tidyverse)   # data manipulation
library(rStrava)     # Strava API functions
library(httr)        # make API requests
library(openxlsx)    # Excel files
library(htmlwidgets) # Save maps
library(readxl)      # Read Excel Files
library(geosphere)   # Calc distances

# Set directory
setwd("~/GitHub/RunEveryStreet")

# Parameters
threshold <- 10  # what's the minimum distance (m) a run location needs to be to a road location to count as 'complete'
extents   <- 0.005  # how much to extend bounding box to check for nearest points

# Load API Credentials
strava <- config::get("strava")
google <- config::get("google")

# Get Strava API token
app_scope = 'activity:read_all'
cache = TRUE
token <- httr::config(token = rStrava::strava_oauth(strava$app_name, strava$client_id, strava$secret, app_scope, cache))

### Functions ###
#################

# For each street location find closest run point
find_closest_point <- function(street_lat, street_lon, df) {
  distances <- distHaversine(matrix(c(street_lon, street_lat), ncol = 2),
                             df[, c("lon", "lat")])
  closest_point_idx <- which.min(distances)
  closest_point <- df[closest_point_idx, ]
  closest_point$new_distance <- distances[closest_point_idx]
  closest_point <- closest_point %>% select(new_complete_dt=dt, new_set=set, new_distance)
  return(closest_point)
}

# Update closest run points for an activity
activity_closest_points <- function(df_progress, df_activity, threshold) {
  
  # filter all street points within bounding box of activity
  df_progress_new <- df_progress %>%
    filter(lat > min(df_activity$lat) - extents,
           lat < max(df_activity$lat) + extents,
           lon > min(df_activity$lon) - extents,
           lon < max(df_activity$lon) + extents,
           complete == 0)
  
  # find closest points
  closest_points <- NULL
  for (i in 1:length(df_progress_new$osm_id)) {
    tmp <- find_closest_point(df_progress_new[i,"lat"],df_progress_new[i,"lon"], df_activity)
    if (i %% 500 == 0) { print(paste0(i,"/",length(df_progress_new$osm_id))) }
    closest_points <- rbind(closest_points,tmp)
  }
  print(paste0(length(df_progress_new$osm_id),"/",length(df_progress_new$osm_id)))
  df_progress_new <- cbind(df_progress_new,closest_points)
  
  # return updated activity data
  df_progress_new <- df_progress_new %>%
    filter(new_distance <= threshold) %>%
    mutate(complete = 1) %>%
    select(osm_id, id, lon, lat, dist, name, city, complete_dt = new_complete_dt, set = new_set, distance = new_distance, complete, osm_unique_id)
  
  return(df_progress_new)
}

# Calculate total new road distance completed
calc_dist <- function(df,dt,typ="total") {
  df <- df %>%
    filter(complete_dt >= dt,
           id > 1,
           complete == 1,
           lag(complete == 1))
  
  if (typ == "total") {
    df <- df %>%
      summarise(new_road = sum(dist) / 1000 ) %>%
      pull(new_road)
  } else if (typ == "by_date") {
    df <- df %>%
      group_by(complete_dt) %>%
      summarise(new_road = sum(dist) / 1000 )
  }
  return(df)
}

# Calculate activity summary metrics
calc_summary <- function(df,dt_act) {
  df %>%
    filter(dt >= dt_act) %>%
    summarise(distance = sum(distance),
              moving_time = sum(moving_time)/3600,
              elevation = sum(total_elevation_gain),
              hoops = sum(hoops, na.rm=T),
              chairs = sum(chairs, na.rm=T))
}

# Summarize completed streets
calc_strt_complete <- function(df) {
  df %>%
    group_by(name,city) %>%
    summarise(complete = sum(complete),
              total = n()) %>%
    mutate(perc_complete = complete / total * 100) %>%
    ungroup() %>%
    filter(perc_complete > 0) %>%
    arrange(desc(perc_complete))
}

### Get New Strava Activity Data ###
####################################

# Get All Strava Activity Data
my_acts <- rStrava::get_activity_list(token)
df_my_acts <- rStrava::compile_activities(my_acts) %>%
              mutate(activity = row_number())

# Get Strava Summary data of completed activities and determine which need to be added
df_strava_summary <- readRDS("Data/strava_activity_summary.rds")

new_activities <- df_my_acts %>%
                  filter(as.Date(start_date_local) > max(df_strava_summary$dt))

for (activity in length(new_activities$activity):1) {
  
  # Check whether to update activity
  {
    update <- readline(paste0("Update Activity '",new_activities$name[activity],"' from ",as.Date(new_activities$start_date_local[activity]),"? [Y/N]: "))
  }
  
  if (tolower(update) == 'y') {

    ########################################################
    ### Update Activity Coordinate Data - Raw & Snapped  ###
    ########################################################
    
    print("Getting Raw Activity Data and Get Data 'Snapped to Roads'")
    
    # Get Latest Raw Activity Stream From Strava 
    df_activity_raw <- rStrava::get_activity_streams(my_acts, token, acts = activity) %>%
                       mutate(strava_id = id,
                              id = row_number(),
                              set = 'raw') %>%
                       select(strava_id, id, set, lat, lon = lng)
    
    # Snap Raw Activity Data to nearest road using Google 'Snap to Roads' API
    i = 1
    df <- NULL
    while (i <= length(df_activity_raw$lat)) {
      if (i+99 > length(df_activity_raw$lat)) { j = length(df_activity_raw$lat) } else { j = i+99 }
      df_temp <- df_activity_raw %>% slice(i:j) %>% select(lat, lon)
      df_temp <- googleway::google_snapToRoads(df_path = df_temp, lat='lat', lon='lon', interpolate = FALSE, key = google$api_key)
      if ( length(df_temp$snappedPoints$location) > 0 ) {
        df_temp <- as.data.frame(df_temp$snappedPoints$location) %>% select(lat=latitude, lon=longitude)
        df <- rbind(df,df_temp)
      }
      i = i + 100
    }
    
    df_activity_snap <- df %>%
                        mutate(strava_id = df_my_acts[activity,"id"],
                               set = 'snap',
                               id = row_number()) %>%
                        select(strava_id, id, set, lat, lon)

    # Add to Dataframe and Save
    df_strava_update <- rbind(readRDS("Data/strava_activity_coords.rds"), df_activity_raw, df_activity_snap)
    saveRDS(df_strava_update, "Data/strava_activity_coords.rds")
    
    ####################################
    ### Update Activity Summary Data ###
    ####################################
    
    print("Update Activity Summary Data")
    
    # Get Values for hoops and chairs
    {
      print(paste0("Values for: ", format(as.Date(df_my_acts[activity,"start_date_local"]), "%A, %B %d %Y")))
      hoops <- readline("Number of hoops: ")
      chairs <- readline("Number of chairs: ")
    }
    
    # Get Summary Data from Strava and Encode Snapped Route to a Polyline Using Google API
    activity_summary <- df_my_acts[activity,] %>%
                        mutate(dt = as.Date(start_date_local),
                               snap_polyline = googleway::encode_pl(lat=df_activity_snap$lat, lon=df_activity_snap$lon),
                               chairs = as.numeric(chairs),
                               hoops = as.numeric(hoops)) %>%
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
                               snap_polyline,
                               chairs,
                               hoops)
    
    # Add to Dataframe and Save
    df_strava_summary <- rbind(activity_summary, df_strava_summary)
    saveRDS(df_strava_summary, "Data/strava_activity_summary.rds")

    ####################################################
    ### Update Minimum Distance to nodes in OSM Data ###
    ####################################################
    
    print("Update Completed Nodes")
    
    # load progress file
    df_progress <- readRDS("Data/df_progress.rds")
    
    # get all coordinate data and join activity date
    df_act_all <- df_strava_update %>%
                  left_join(df_strava_summary, by="strava_id") %>%
                  mutate(dt = as.character(dt)) %>%
                  select(dt, set, lat, lon)

    # Flag closest points for streets within activity bounding box
    df_progress_update <- activity_closest_points(df_progress, (df_act_all %>% filter(dt == activity_summary$dt[1])), threshold)
    
    # Join to full dataframe
    df_progress <- df_progress %>%
                   filter(!osm_unique_id %in% df_progress_update$osm_unique_id) %>%
                   rbind(df_progress_update)
      
    df_progress <- df_progress %>% arrange(osm_id,id)
 
    ########################################
    ### Manual Update of Completed Nodes ###
    ########################################
    
    print("Manual Node Updates: Check for nodes that should be complete and add to manual_completions.csv")

    # Select all point around activity and color code them
    df_activity <- df_progress %>% filter(complete_dt == activity_summary$dt[1])
    
    tmp <- df_progress %>%
           filter(lat > min(df_activity$lat) - extents,
                  lat < max(df_activity$lat) + extents,
                  lon > min(df_activity$lon) - extents,
                  lon < max(df_activity$lon) + extents) %>%
           mutate(colour = case_when(complete == 1 ~ 'green',
                                     TRUE ~ 'red'),
                  info = paste0("osm_id: ",osm_id," id: ",id," distance: ",round(distance,2)))
    
    # Select polyline
    df_polyline <- df_strava_summary %>% filter(dt == activity_summary$dt[1])
    
    # map points and identify completed points that were not added to progress file
    # add these points to the manual_completions.csv file
    pic <- googleway::google_map(key = google$api_key, data = tmp) %>%
           add_markers(lat = 'lat', lon = 'lon', colour = 'colour', info_window = "info") %>%
           add_polylines(data = df_polyline, polyline = "map.summary_polyline", stroke_weight = 2 )        
    htmlwidgets::saveWidget(pic, file = "Outputs/manual_completions.html")
    
    {
      comp <- readline(paste0("Finished adding manual updates to manual_completions.csv? [Hit Enter]:"))
    }
    
    # Load Latest version of manually updated completed points and add to progress
    df_manual_completions <- readxl::read_excel('data/manual_changes.xlsx', sheet = 'manual_completions')

    df_manual_nodes <- NULL
    for (i in 1:length(df_manual_completions$osm_id)) {
      for (j in df_manual_completions$start_node[i]:df_manual_completions$end_node[i]) {
        df_manual_nodes <- rbind(df_manual_nodes, c(df_manual_completions$osm_id[i], as.character(df_manual_completions$complete_dt_update[i]), j))
      }
    }
    colnames(df_manual_nodes) <- c('osm_id','complete_dt_update','id')
    
    df_manual_nodes <- as.data.frame(df_manual_nodes) %>%
                       mutate(osm_id = as.character(osm_id),
                              id = as.numeric(id),
                              complete_dt_update = as.character(complete_dt_update))
    
    df_progress <- df_progress %>%
                   left_join(df_manual_nodes, by=c("osm_id","id")) %>%
                   mutate(complete_dt = case_when(!is.na(complete_dt_update) ~ complete_dt_update, TRUE ~ complete_dt),
                          set = case_when(!is.na(complete_dt_update) ~ 'manual', TRUE ~ set),
                          distance = case_when(!is.na(complete_dt_update) ~ threshold, TRUE ~ distance),
                          complete = case_when(!is.na(complete_dt_update) ~ 1, TRUE ~ complete)) %>%
                   select(-complete_dt_update)
    
    # Save final output
    saveRDS(df_progress,"Data/df_progress.rds")
    
    ####################################
    ### Save Latest Progress Results ###
    ####################################

    # Get activity dates
    dts <- unique(df_progress$complete_dt[df_progress$complete_dt != ""]) %>% sort()
    
    # Set start of week (either 7 days prior or set to specific dat)
    wk_dt <- as.Date(max(dts)) - 6

    # Summary: New Roads
    new_roads <- data.frame(Total = calc_dist(df_progress, min(dts)),
                            Last_Activity = calc_dist(df_progress, max(dts)),
                            This_Week = calc_dist(df_progress, wk_dt))
    rownames(new_roads) <- "New Roads"
    
    # Summary: New Roads by date
    new_roads_bydate <- calc_dist(df_progress, min(dts), "by_date")
    
    # Summary: Other Activity Metrics
    act_summary = t(rbind(calc_summary(df_strava_summary, min(dts))
                          ,calc_summary(df_strava_summary, max(dts))
                          ,calc_summary(df_strava_summary, wk_dt)))
    
    
    # Summary: Street completion data
    df_strt_summary <- calc_strt_complete(df_progress) %>%
                       left_join(calc_strt_complete(df_progress %>% filter(complete_dt != max(dts))),
                                 by=c("name","city"),
                                 suffix = c("_new", "_old")) %>%
                       replace_na(list(perc_complete_old=0)) %>%
                       mutate(diff = perc_complete_new - perc_complete_old)
    
    # Save Summary File
    wb <- createWorkbook()
    sheet = "activity summary"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, x = "new roads", startCol = 1, startRow = 2)
    writeData(wb, sheet, x = new_roads, startCol = 2, startRow = 1)
    writeData(wb, sheet, x = act_summary, startCol = 1, startRow = 3, colNames = F, rowNames = T)
    
    writeData(wb, sheet, x = "% Complete", startCol = 1, startRow = 9)
    writeData(wb, sheet, x = new_roads[1,1]/(sum(df_progress$dist)/1000), startCol = 2, startRow = 9)
    
    writeData(wb, sheet, x = "Distance by Date (km)", startCol = 6, startRow = 1)
    writeData(wb, sheet, x = new_roads_bydate, startCol = 6, startRow = 2)
    
    writeData(wb, sheet, x = "Completed Streets", startCol = 9, startRow = 1)
    writeData(wb, sheet, x = df_strt_summary, startCol = 9, startRow = 2)
    
    saveWorkbook(wb, file = paste0("Outputs/",Sys.Date(),"_activity_summary.xlsx"), overwrite = TRUE)

  }
}



