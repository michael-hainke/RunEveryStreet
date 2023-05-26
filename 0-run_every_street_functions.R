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
  
  road_list <- unique(df_strt$osm_id)
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
map_activities <- function(df, polyline_col, description_col, google_key) {
  df <- df %>%
        mutate(description = (!!sym(description_col)),
               plot_polyline = (!!sym(polyline_col)) )
  loc = c(median(df$start_latlng1),median(df$start_latlng2))
  
  google_map(key = google_key, location=loc, data = df) %>%
    add_polylines(data = df, polyline = "plot_polyline", mouse_over = "description",
                  stroke_weight = 2 )
  
}

# For each street location find closest run point
find_closest_point <- function(street_lat, street_lon, df_runs) {
  distances <- distHaversine(matrix(c(street_lon, street_lat), ncol = 2),
                             df_runs[, c("lon", "lat")])
  closest_point_idx <- which.min(distances)
  closest_point <- df_runs[closest_point_idx, ]
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
