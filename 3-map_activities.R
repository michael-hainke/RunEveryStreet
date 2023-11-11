# Load Required Packages
library(googleway)  # Google API functions
library(webshot2)   # Save screenshots of maps
library(tidyverse)  # data manipulation
library(htmlwidgets)# Save maps

# Load API Credentials
google <- config::get("google")

#################
### FUNCTIONS ###
#################

# generate dataframe with all streets complete (full and partial)
complete_streets <- function() {
  
  # Load OSM street data
  df_streets <- readRDS("Data/df_osm_street_summary_final.rds") %>% select(osm_id,polyline)
  df_progress <- readRDS("Data/df_progress.rds")
  
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
                        df_strt_partial_list %>% select(name,polyline)) %>%
                  as.data.frame()
  
  return(df_strts_map)
  
}

# map activities (Google API)
map_activities <- function(df, polyline_col, description_col, google_key, output_name) {
  df <- df %>%
    mutate(description = (!!sym(description_col)),
           plot_polyline = (!!sym(polyline_col)) )

 map <- google_map(key = google_key, data = df) %>%
        add_polylines(data = df, polyline = "plot_polyline", mouse_over = "description",
                      stroke_weight = 2 )

    saveWidget(map, file = paste0("Outputs/",output_name,".html"))
  webshot2::webshot(paste0("Outputs/",output_name,".html"), vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_",output_name,".png"))
  
}

# map nodes (Google API)
map_nodes <- function(df, ids=NULL, nodes=c(0,1), google_key, output_name) {
  
  if (!is.null(ids)) { df <- df %>% filter(osm_id %in% ids) }
  df <- df %>%
        filter(complete %in% nodes) %>%
        mutate(colour = case_when(complete == 1 ~ 'green',
                                  TRUE ~ 'red'),
               info = paste0("osm_id: ",osm_id," id: ",id," distance: ",round(distance,2)))

  map <- google_map(key = google$api_key, data = df) %>%
         add_markers(lat = 'lat', lon = 'lon', colour = 'colour', info_window = "info")      
  saveWidget(map, file = paste0("Outputs/",output_name,".html"))
  webshot2::webshot(paste0("Outputs/",output_name,".html"), vwidth = 1920, vheight = 1080, paste0("Outputs/",Sys.Date(),"_",output_name,".png"))
  
}

#####################
### Generate Maps ###
#####################

# Map Snapped Activity Data Using Google Maps API
map_activities(readRDS("Data/strava_activity_summary.rds") %>% mutate(description = paste0(dt," ",name)),
               'snap_polyline',
               'description',
               google$api_key,
               'snapped_activity')

# Map Raw Activity Data Using Google Maps API  
map_activities(readRDS("Data/strava_activity_summary.rds") %>% mutate(description = paste0(dt," ",name)),
               'map.summary_polyline',
               'description',
               google$api_key,
               'raw_activity')


# Map OSM Street Data
map_activities(readRDS("Data/df_osm_street_summary_final.rds"),
               'polyline',
               'osm_id',
               google$api_key,
               'osm_map')

# Map Nodes
# ids = provide list of osm_ids if only want to map some ids
# nodes = select which nodes to map (0 = not complete, 1 = complete)
map_nodes(readRDS("Data/df_progress.rds"),
          ids=c('23926188'),
          nodes=c(0,1),
          google_key,
          'node_map')

map_nodes(df = readRDS("Data/df_progress.rds"),
          nodes = c(0),
          google_key = google_key,
          output_name = 'node_map')

# Map Completed Streets
map_activities(complete_streets(),
               'polyline',
               'name',
               google$api_key,
               'completed_streets')



