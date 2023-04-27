library(tidyverse)  # data manipulation
library(rStrava)    # Strava API functions
library(httr)       # make API requests
library(config)     # config file for credentials
library(jsonlite)   # work with JSON files
library(googleway)  # Google API functions
library(geosphere)  # calculate distances
library(openxlsx)   # output to Excel

##################
### Parameters ###
##################

threshold <- 10  # what's the minimum distance (m) a run location needs to be to a road location to count as 'complete'
extents = 0.005  # how much to extend bounding box to check for nearest points

#################
### Functions ###
#################

# map activities (Google API)
map_activities <- function(df, polyline_col, google_key) {
  df <- df %>%
        mutate(description = name,
               plot_polyline = (!!sym(polyline_col)) )

  google_map(key = google_key, data = df) %>%
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
activity_closest_points <- function(df_progress, df_activity) {
  
  # filter all street points within bounding box of activity
  df_progress_new <- df_progress %>%
    filter(lat > min(df_activity$lat) - extents,
           lat < max(df_activity$lat) + extents,
           lon > min(df_activity$lon) - extents,
           lon < max(df_activity$lon) + extents)
  
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
  df_progress_new %>%
  filter(distance == -1 | new_distance < distance) %>%
  select(osm_id,id,osm_unique_id, lat, lon, dist, name, city, complete_dt = new_complete_dt, set = new_set, distance = new_distance)
  
}

#############################
### Google Authentication ###
#############################

# Get Google Credentials and Authorize
google <- config::get("google")

##################################
# Update 'Run Every Street' Data #
##################################

# load progress file
df_progress <- readRDS("Data/df_progress.rds")

# load completed activities (pre-processed snapped data & raw data)
df_act_all <- readRDS("Data/strava_activity_detail_raw.rds") %>%
              left_join(readRDS("Data/strava_activity_summary.rds"), by="strava_id") %>%
              mutate(set = 'raw',
                     dt = as.character(dt)) %>%
              select(dt, set, lat, lon) %>%
              rbind(readRDS("Data/strava_activity_detail_snap.rds") %>%
                    mutate(set = 'snapped') %>%
                    select(dt, set, lat, lon))

# get list of activities not added to progress file
dts <- df_act_all %>% 
       distinct(dt) %>% 
       arrange(dt) %>%
       filter(dt > max(df_progress$complete_dt)) %>% 
       pull(dt)

# loop through all activities and update closest points
for (activity_dt in dts) {

  # Select activity data
  print(activity_dt)
  df_activity <- df_runs_all %>% filter(dt == activity_dt)
  
  # Flag closest points for streets within activity bounding box
  df_progress_update <- activity_closest_points(df_progress, df_activity)
    
  # Join to full dataframe
  df_progress <- df_progress %>%
                 filter(!osm_unique_id %in% df_progress_update$osm_unique_id) %>%
                 rbind(df_progress_update)

}

# Save final output
saveRDS(df_progress,"Data/df_progress.rds")

####################################
### Save Latest Progress Results ###
####################################

# load data
if (!exists(df_progress)) { df_progress <- readRDS("Data/df_progress.rds") }
df_streets <- readRDS("Data/df_osm_streets_final.rds") %>%
              select(osm_id,polyline)

# Calculate total distance complete
df_dist <- df_progress %>%
           filter(id > 1,
                  distance <= threshold & distance != -1,
                  lag(distance <= threshold & distance != -1))
total_dist = sum(df_dist$dist) / 1000

# Calculate distance by date
df_dist_summary <- df_dist %>%
                   filter(complete_dt != '') %>%
                   mutate(complete_dt = as.Date(complete_dt)) %>%
                   group_by(complete_dt) %>%
                   summarise(length = sum(dist)/1000)


# Flag all completed points
df_progress <- df_progress %>%
               mutate(complete = case_when(distance <= threshold  & distance != -1 ~ 1,
                                           TRUE ~ 0))

# Summarize completed streets
df_strt_complete <- df_progress %>%
                    group_by(name,city) %>%
                    summarise(complete = sum(complete),
                              total = n()) %>%
                    mutate(perc_complete = complete / total * 100) %>%
                    ungroup() %>%
                    filter(perc_complete > 0) %>%
                    arrange(desc(perc_complete))

# Save Summary File
wb <- createWorkbook()
sheet = "activity summary"
addWorksheet(wb, sheet)
writeData(wb, sheet, x = "Total Distance (km): ", startCol = 1, startRow = 1)
writeData(wb, sheet, x = total_dist, startCol = 2, startRow = 1)

writeData(wb, sheet, x = "Distance by Date (km)", startCol = 4, startRow = 1)
writeData(wb, sheet, x = df_dist_summary, startCol = 4, startRow = 2)

writeData(wb, sheet, x = "Completed Streets", startCol = 7, startRow = 1)
writeData(wb, sheet, x = df_strt_complete, startCol = 7, startRow = 2)

saveWorkbook(wb, file = paste0("Outputs/",Sys.Date(),"_activity_summary.xlsx"), overwrite = TRUE)

#############################
### Map Completed Streets ###
#############################

### Summarize completed street segments
df_strt_seg_complete <- df_progress %>%
                        group_by(osm_id,name,city) %>%
                        summarise(complete = sum(complete),
                                  total = n(),
                                  length = sum(dist)) %>%
                        filter(complete == total) %>%
                        left_join(df_streets, by="osm_id") %>%
                        ungroup()

### Summarize partially completed streets and encode

id_count = 1
df_strt_partial  <- df_progress_new %>%
                    filter(!osm_id %in% df_strt_complete$osm_id,
                           complete == 1) %>%
                    arrange(osm_id,id) %>%
                    mutate(id_partial = case_when(row_number() == 1 ~ paste0(osm_id,"-",id_count),
                                                  TRUE ~ ''))

for (i in 2:length(df_strt_partial$osm_id)) {
  if(df_strt_partial$id[i] == 1 ||
     df_strt_partial$osm_id[i] != df_strt_partial$osm_id[i-1] ||
     (df_strt_partial$id[i] - df_strt_partial$id[i-1] > 1 & df_strt_partial$osm_id[i] == df_strt_partial$osm_id[i-1])) { id_count = id_count + 1 }
  df_strt_partial$id_partial[i] = paste0(df_strt_partial$osm_id[i],"-",id_count)
}

single_pts <- df_strt_partial %>% group_by(id_partial) %>% tally() %>% filter(n == 1) %>% pull(id_partial)

df_strt_partial <- df_strt_partial %>%
                   filter(!id_partial %in% single_pts)

df_strt_partial_list <- df_strt_partial %>% group_by(id_partial, name) %>% summarise(pts = n()) %>% mutate(polyline = '') %>% ungroup()

# encode all lat/lon into Google polylines

for (i in 1:length(df_strt_partial_list$id_partial)) {
  print(paste0(i,": ",df_strt_partial_list$name[i]))
  tmp <- df_strt_partial %>% filter(id_partial == df_strt_partial_list$id_partial[i])
  polyline <- encode_pl(lat=tmp$lat, lon=tmp$lon)
  df_strt_partial_list$polyline[i] <- polyline
}

# combine full and partial streets
df_strts_map <- rbind(df_strt_seg_complete %>% select(name,polyline), df_strt_partial_list %>% select(name,polyline))


### Map all polylines

map_activities(df_strts_map, 'polyline', google$api_key)

#################################################

### Test: Plot points

tmp <- df_dist %>% filter(as.Date(complete_dt) == '2023-03-11')

loc = c(median(tmp$lat),median(tmp$lon))
pic <- google_map(key = google$api_key, location=loc, data = tmp) %>%
  add_markers(data = tmp, lat = 'lat', lon = 'lon' )
pic

