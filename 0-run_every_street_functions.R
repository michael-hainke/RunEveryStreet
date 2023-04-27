# get street data from OSM
get_streets <- function(bb,nm) {
  
  strts <- bb %>%
    opq() %>%
    add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary",
                                               "residential", "living_street", "unclassified", "service")) %>%
    osmdata_sf()
  
  df <- data.frame(strts$osm_lines) %>%
    mutate(length = 0,
           polyline = '',
           valid_street = 1,
           city = nm) %>%
    select(osm_id,
           city,
           name,
           access,
           highway,
           geometry,
           length,
           polyline,
           valid_street)
  return(df)  
  
}

# get all lat/lon coordinates
extract_coordinates <- function(df_strts) {
  
  df <- NULL
  
  for (i in 1:length(df_strts$osm_id)) {
    print(df_strts$name[i])
    tmp <- data.frame(st_coordinates(df_strts$geometry[i])) %>%
      mutate(osm_id = df_strts$osm_id[i],
             id = row_number())
    df <- rbind(df,tmp)
  }
  
  df <- df %>% select(osm_id,id,lon=X,lat=Y)
  return(df)
  
}

# calculate distances
calc_distance <- function(df_coord) {
  
  df_coord <- df_coord %>% mutate(dist = 0)
  for (i in 1:length(df_coord$id)) {
    if (df_coord$id[i] > 1) { df_coord$dist[i] = distHaversine(c(df_coord$lon[i],df_coord$lat[i]),c(df_coord$lon[(i-1)],df_coord$lat[(i-1)])) }
  }
  return(df_coord)
}

# add total street lengths
street_length <- function(df_strt, df_coord) {
  
  road_list <- unique(df_coord$osm_id)
  for (i in road_list) {
    tmp <- df_coord %>% filter(osm_id == i)
    df_strt[df_strt$osm_id == i,"length"] = sum(tmp$dist)
  }
  return(df_strt)
}

# encode all lat/lon into Google polylines
get_polylines <- function(df, df_coords) {
  
  for (i in 1:length(df$osm_id)) {
    print(paste0(i,": ",df$name[i]))
    tmp <- df_coords %>% filter(osm_id == df$osm_id[i])
    polyline <- encode_pl(lat=tmp$lat, lon=tmp$lon)
    df$polyline[i] <- polyline
    Sys.sleep(0.25)
  }
  return(df)
  
}

# map polylines
map_polylines <- function(loc, df, api_key) {
  
  google_map(key = google$api_key, location=loc, data = df) %>%
    add_polylines(data = df, polyline = "polyline", mouse_over = "name_id",
                  stroke_weight = 3, stroke_colour = "#eb3440" )
  
}


# get raw activity stream data
get_activity_raw <- function(my_acts, token, activity = 1) {
  
  get_activity_streams(my_acts, token, acts = activity) %>%
  mutate(strava_id = id,
         id = row_number(),
         set = 'raw') %>%
  select(strava_id, id, set, lat, lon = lng)

}


# snap coordinates to nearest road (Google API)
activity_snap_to_road <- function(act,act_summary,google_key) {

  # snap points to nearest road (send 100 coordinates at a time)
  i = 1
  df <- NULL
  while (i <= length(act$lat)) {
    if (i+99 > length(act$lat)) { j = length(act$lat) } else { j = i+99 }
    df_temp <- act %>% slice(i:j) %>% select(lat, lon)
    df_temp <- google_snapToRoads(df_path = df_temp, lat='lat', lon='lon', interpolate = FALSE, key = google_key)
    if ( length(df_temp$snappedPoints$location) > 0 ) {
      df_temp <- as.data.frame(df_temp$snappedPoints$location) %>% select(lat=latitude, lon=longitude)
      df <- rbind(df,df_temp)
    }
    i = i + 100
  }
  
  # add other activity data
  df <- df %>%
        mutate(strava_id = act_summary$id,
               set = 'snap',
               id = row_number()) %>%
        select(strava_id, id, set, lat, lon)
  
  return(df)

}

# map activities (Google API)
map_activities <- function(df, polyline_col, google_key) {
  df <- df %>%
    mutate(description = paste0(dt," ",name),
           plot_polyline = (!!sym(polyline_col)) )
  loc = c(median(df$start_latlng1),median(df$start_latlng2))
  
  google_map(key = google_key, location=loc, data = df) %>%
    add_polylines(data = df, polyline = "plot_polyline", mouse_over = "description",
                  stroke_weight = 2 )
  
}


## Old Function:  Flags all duplicate coordinates during activity and with other activities
# flag all duplicate coordinates
flag_duplicates <- function(act, act_check=NULL) {
  act <- act %>%
    mutate(duplicate = 0)
  
  # if coordinates within 3m of any point more than 4 points ago then flag as duplicate
  for (i in 5:length(act$lon)) {
    for (j in 1:(i-4)) {
      if ( distHaversine(c(act$lon[i],act$lat[i]),c(act$lon[(j)],act$lat[(j)])) < 3 ) {
        act$duplicate[i] = 1
        break
      }
    }
  }
  
  #  if coordinates within 3m of any point from another overlapping activity then flag as duplicate
  if (!is.null(act_check)) {
    for (i in 1:length(act$lon)) {
      if(act$duplicate[i]==0) {
        for (j in 1:length(act_check$lon)) {
          if ( distHaversine(c(act$lon[i],act$lat[i]),c(act_check$lon[(j)],act_check$lat[(j)])) < 3 ) {
            act$duplicate[i] = 1
            break
          }
        }
      }
    }
  }
  
  # if non duplicate between 2 duplicate points then assign as duplicate
  for (i in 2:(length(act$lon)-2)) {
    if ( act$duplicate[(i-1)] == 1 & act$duplicate[i] == 0 & sum(act$duplicate[(i+1):(i+2)]) > 0 ) { act$duplicate[i] = 1 }
  }
  return(act)
}
