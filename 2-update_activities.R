# Load Required Packages
library(tidyverse)  # data manipulation
library(rStrava)    # Strava API functions
library(httr)       # make API requests
library(config)     # config file for credentials
library(jsonlite)   # work with JSON files
library(googleway)  # Google API functions
library(geosphere)  # calculate distances
library(openxlsx)   # Excel files
library(htmlwidgets)# Save maps
library(webshot2)   # Save screenshots of maps

# Load Required Functions
source("0-run_every_street_functions.R")

# Parameters
threshold <- 10  # what's the minimum distance (m) a run location needs to be to a road location to count as 'complete'
extents = 0.005  # how much to extend bounding box to check for nearest points

# Load API Credentials
strava <- config::get("strava")
google <- config::get("google")

# Get Strava API token
app_scope = 'activity:read_all'
cache = TRUE
token <- httr::config(token = strava_oauth(strava$app_name, strava$client_id, strava$secret, app_scope, cache))

# Get All Strava Activity Data
my_acts <- get_activity_list(token)
df_my_acts <- compile_activities(my_acts) %>%
              mutate(activity=row_number())

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
    df_activity_raw <- get_activity_raw(my_acts, token, activity)
    
    # Snap Raw Activity Data to nearest road using Google 'Snap to Roads' API
    df_activity_snap <- activity_snap_to_road(df_activity_raw,df_my_acts[activity,],google$api_key)
    
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
                               snap_polyline = encode_pl(lat=df_activity_snap$lat, lon=df_activity_snap$lon),
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
    pic <- google_map(key = google$api_key, data = tmp) %>%
      add_markers(lat = 'lat', lon = 'lon', colour = 'colour', info_window = "info") %>%
      add_polylines(data = df_polyline, polyline = "map.summary_polyline", stroke_weight = 2 )        
    saveWidget(pic, file = "Outputs/manual_completions.html")
    
    {
      comp <- readline(paste0("Finished adding manual updates to manual_completions.csv? [Hit Enter]:"))
    }
    
    # Load Latest version of manually updated completed points and add to progress
    df_progress <- df_progress %>%
                   left_join(read.csv("Data/manual_completions.csv"), by=c("osm_id","id")) %>%
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
    
    # Summary: New Roads
    new_roads <- data.frame(Total = calc_dist(df_progress, min(dts)),
                            Last_Activity = calc_dist(df_progress, max(dts)),
                            This_Week = calc_dist(df_progress, as.Date(max(dts)) - 6))
    rownames(new_roads) <- "New Roads"
    
    # Summary: New Roads by date
    new_roads_bydate <- calc_dist(df_progress, min(dts), "by_date")
    
    # Summary: Other Activity Metrics
    act_summary = t(rbind(calc_summary(df_strava_summary, min(dts))
                          ,calc_summary(df_strava_summary, max(dts))
                          ,calc_summary(df_strava_summary, as.Date(max(dts)) - 6)))
    
    
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

    ###############################################
    ### Polyline Data for all Completed Streets ###
    ###############################################
    
    # Load OSM street data
    df_streets <- readRDS("Data/df_osm_street_summary_final.rds") %>% select(osm_id,polyline)
    
    # Filter completed street segments
    df_strt_seg_complete <- df_progress %>%
                            group_by(osm_id,name,city) %>%
                            summarise(complete = sum(complete),
                                      total = n(),
                                      length = sum(dist)) %>%
                            filter(complete == total) %>%
                            left_join(df_streets, by="osm_id") %>%
                            ungroup()
    
    # Filter partially completed streets
    id_count = 1
    df_strt_partial <- df_progress %>%
                       filter(!osm_id %in% df_strt_seg_complete$osm_id,
                               complete == 1) %>%
                       arrange(osm_id,id) %>%
                       mutate(id_partial = case_when(row_number() == 1 ~ paste0(osm_id,"-",id_count),
                                                      TRUE ~ ''))
    
    # Loop through all nodes and create a unique id_partial for each segment (osm_id - id_count)
    # Increment id_count if:
    #  1) First node in for next street in dataframe OR
    #  2) Next street OR
    #  3) Gap in current street (ie not the next node)
    for (i in 2:length(df_strt_partial$osm_id)) {
      if(df_strt_partial$id[i] == 1 ||
         df_strt_partial$osm_id[i] != df_strt_partial$osm_id[i-1] ||
         (df_strt_partial$id[i] - df_strt_partial$id[i-1] > 1 & df_strt_partial$osm_id[i] == df_strt_partial$osm_id[i-1])) { id_count = id_count + 1 }
      df_strt_partial$id_partial[i] = paste0(df_strt_partial$osm_id[i],"-",id_count)
    }
    
    # Identify all the single node segments and remove from partial street list
    single_pts <- df_strt_partial %>%
                  group_by(id_partial) %>%
                  tally() %>% 
                  filter(n == 1) %>%
                  pull(id_partial)
    df_strt_partial <- df_strt_partial %>% filter(!id_partial %in% single_pts)
    
    # Summarize partial streets and encode polylines
    df_strt_partial_list <- df_strt_partial %>%
                            group_by(id_partial, name) %>%
                            summarise(pts = n()) %>%
                            mutate(polyline = '') %>%
                            ungroup()
    
    for (i in 1:length(df_strt_partial_list$id_partial)) {
      print(paste0(i,": ",df_strt_partial_list$name[i]))
      tmp <- df_strt_partial %>% filter(id_partial == df_strt_partial_list$id_partial[i])
      polyline <- encode_pl(lat=tmp$lat, lon=tmp$lon)
      df_strt_partial_list$polyline[i] <- polyline
    }
    
    # combine full and partial streets into single dataframe for mapping and save
    df_strts_map <- rbind(df_strt_seg_complete %>% select(name,polyline),
                          df_strt_partial_list %>% select(name,polyline))
    saveRDS(df_strts_map,"Data/df_completed_streets.rds")
    
  }
}


##################################
### Map Completed Activities ###
##################################

if (!exists("df_strava_summary")) { df_strava_summary <- readRDS("Data/strava_activity_summary.rds") }

df_strava_summary <- df_strava_summary %>%
                     mutate(description = paste0(dt," ",name))

# Map Snapped Activity Data Using Google Maps API
map <- map_activities(df_strava_summary, 'snap_polyline', 'description', google$api_key)
saveWidget(map, file = "Outputs/map.html")
webshot2::webshot("Outputs/map.html", vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_snapped_activity.png"))

# Map Raw Activity Data Using Google Maps API  
map <- map_activities(df_strava_summary, 'map.summary_polyline', 'description', google$api_key)
saveWidget(map, file = "Outputs/map.html")
webshot2::webshot("Outputs/map.html", vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_raw_activity.png"))

# Map OSM Street Data
if (!exists("df_streets")) { df_streets <- readRDS("Data/df_osm_street_summary_final.rds") }
map <- map_activities(df_streets, 'polyline', 'osm_id', google$api_key)
saveWidget(map, file = "Outputs/map.html")
webshot2::webshot("Outputs/map.html", vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_osm_streets.png"))

# Map Completed Streets
if (!exists("df_strts_map")) { df_streets <- readRDS("Data/df_completed_streets.rds") }
map <- map_activities(df_strts_map, 'polyline', 'name', google$api_key)
saveWidget(map, file = "Outputs/map.html")
webshot2::webshot("Outputs/map.html", vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_completed_streets.png"))

# Map Nodes
if (!exists("df_progress")) { df_streets <- readRDS("Data/df_progress.rds") }

tmp <- df_progress %>% 
       filter(osm_id=='508134094') %>%
       mutate(colour = case_when(complete == 1 ~ 'green',
                                 TRUE ~ 'red'),
              info = paste0("osm_id: ",osm_id," id: ",id," distance: ",round(distance,2)))

google_map(key = google$api_key, data = tmp) %>%
  add_markers(lat = 'lat', lon = 'lon', colour = 'colour', info_window = "info")      



