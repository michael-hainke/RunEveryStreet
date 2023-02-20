library(tidyverse)  # data manipulation
library(config)     # config file for credentials
library(googleway)  # Google API functions
library(geosphere)  # calculate distances
library(osmdata)    # get Open Street Map data
library(ggmap)      # mapping function
library(rlist)      # decoding lists
library(sf)         # spatial vector data

# authenticate Google
google <- config::get("google")
ggmap::register_google(key = google$api_key)

### Functions ###
#################

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

### Load All Street Data ###
############################

# get all street data from North Vancouver & West Vancouver
df_streets <- rbind(get_streets(getbb("District of North Vancouver"), "North Vancouver"),
                    get_streets(getbb("West Vancouver"), "West Vancouver"))

# filter the duplicate streets from overlapping bounding boxes (keep the North Van copy)
dup_street <- df_streets %>% group_by(osm_id) %>% tally() %>% filter(n >1) %>% pull(osm_id)
df_streets <- df_streets %>% filter(!(city == 'West Vancouver' & osm_id %in% dup_street))

# extract all the lat/lon coordinates
df_coords <- extract_coordinates(df_streets)

# calculate Haversine distance between coordinates
df_coords <- calc_distance(df_coords)

# add street lengths
df_streets <- street_length(df_streets, df_coords)
  
# encode polylines
df_streets <- get_polylines(df_streets, df_coords)

# save files
saveRDS(df_streets,paste0(Sys.Date(),"_df_streets_raw.rds"))
saveRDS(df_coords,paste0(Sys.Date(),"_df_coords_raw.rds"))

### Update Streets with Manual Changes ###
##########################################

# load raw files (if necessary)
df_streets <- readRDS("2023-02-16_df_streets_raw.rds")
df_coords <- readRDS("2023-02-16_df_coords_raw.rds")

# load manual updates (remove invalid streets & rename NA streets)
df_changes <- read.csv("road_updates.csv") %>%
              mutate(osm_id = as.character(osm_id)) %>%
              select(osm_id,keep_street,new_name)

# join and update streets dataframe
df_streets <- df_streets %>%
              left_join(df_changes, by="osm_id") %>%
              mutate(valid_street = case_when(keep_street == 0 ~ 0,
                                              keep_street == 1 ~ 1, 
                                              TRUE ~ valid_street),
                     name = case_when(!is.na(new_name) ~ new_name, TRUE ~ name)) %>%
              select(-keep_street, -new_name)

# load new manual streets and calculate distances
df_coords_new <- read.csv("custom_streets.csv") %>%
                 mutate(dist = 0) %>%
                 calc_distance()

# load new street summary and calculate length and encode polylines
df_strts_new <- read.csv("custom_strt_summary.csv") %>%
                street_length(df_coords_new) %>%
                get_polylines(df_coords_new)

# join with existing coordinates and streets
df_streets <- rbind(df_strts_new,df_streets)
df_coords <- rbind(df_coords, df_coords_new)

# remove all U2 streets (with no name) and also Highway 1 (freeway)
df_streets <- df_streets %>%
              filter(valid_street == 1,
                     !is.na(name),
                     name != 'Trans-Canada Highway')

df_coords <- df_coords %>%
             filter(osm_id %in% df_streets$osm_id)

# save final files
saveRDS(df_streets,paste0(Sys.Date(),"_df_strts_final.rds"))
saveRDS(df_coords,paste0(Sys.Date(),"_df_strt_coords_final.rds"))

### Helper Script ###
#####################

# # map all current streets with osm_id hover for validation
# map_polylines(c(49.3632564,-123.128715), (df_streets %>% mutate(name_id = osm_id)), google$api_key)

