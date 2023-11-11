library(tidyverse)  # data manipulation
library(config)     # config file for credentials
library(geosphere)  # calculate distances
library(osmdata)    # get Open Street Map data
library(ggmap)      # mapping function
library(sf)         # spatial vector data
library(readxl)     # read Excel files
library(googleway)  # Google API functions

# Parameters
cities <- c("District of North Vancouver","West Vancouver")

# Authenticate Google (for encoding Polylines)
google <- config::get("google")
ggmap::register_google(key = google$api_key)

### Load OSM Street Data ###
############################

if (!file.exists("Data/df_osm_street_summary_final.rds")) {

# query OSM API
df_streets <- NULL
for (city in cities) {

  streets <- osmdata::opq(osmdata::getbb(city)) %>%
             osmdata::add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary",
                                                                 "residential", "living_street", "unclassified", "service")) %>%
             osmdata::osmdata_sf()
  
  df_tmp <- data.frame(streets$osm_lines) %>%
            mutate(valid_street = 1,
                   city = case_when(city == 'District of North Vancouver' ~ 'North Vancouver',
                                    TRUE ~ city)) %>%
            select(osm_id,
                   name,
                   city,
                   geometry,
                   valid_street)
  
  df_streets <- rbind(df_streets, df_tmp)
  
}

# filter the duplicate streets from overlapping bounding boxes (keep the North Van copy)
dup_street <- df_streets %>% group_by(osm_id) %>% tally() %>% filter(n >1) %>% pull(osm_id)
df_streets <- df_streets %>% filter(!(city == 'West Vancouver' & osm_id %in% dup_street))

# load manual updates (remove invalid streets & rename NA streets)
df_changes <- readxl::read_excel('data/manual_changes.xlsx', sheet = 'osm_updates') %>%
              mutate(osm_id = as.character(osm_id)) %>%
              select(osm_id,keep_street,new_name)

# make changes to df_streets dataframe
df_streets <- df_streets %>%
              left_join(df_changes, by="osm_id") %>%
              mutate(valid_street = case_when(keep_street == 0 ~ 0,
                                              keep_street == 1 ~ 1, 
                                              TRUE ~ valid_street),
                     name = case_when(!is.na(new_name) ~ new_name, TRUE ~ name)) %>%
              select(-keep_street, -new_name)

# remove all U2 streets (with no name) and also Highway 1 (freeway)
df_streets <- df_streets %>%
              filter(valid_street == 1,
                     !is.na(name),
                     name != 'Trans-Canada Highway') %>%
              select(-valid_street) %>%
              mutate(polyline = '') %>%
              select(osm_id, name, city, geometry, polyline)

} else { df_streets <- readRDS("Data/df_osm_street_summary_final.rds") }

### Manual Changes (add/remove streets) ###
###########################################

# Load additional manual changes (streets to add)
df_new_streets <- read_excel('data/manual_changes.xlsx', sheet = 'new_streets') %>% mutate(osm_id = as.character(osm_id))

# Download OSM BC Data from GeoFrabrik (Use this for data for new streets that weren't captured in original OSM query)
# https://download.geofabrik.de/north-america/canada/british-columbia.html
df_osm_streets <- sf::st_read('Data/british-columbia-latest-free.shp/gis_osm_roads_free_1.shp')

# Filter new roads that need to be added to file
df_new <- df_osm_streets %>%
          filter(osm_id %in% df_new_streets$osm_id) %>%
          filter(!osm_id %in% df_streets$osm_id) %>%
          as.data.frame() %>%
          left_join(df_new_streets, by=c("osm_id")) %>%
          select(osm_id,name,city,geometry) %>%
          mutate(polyline = '')

# Add new Streets to file
df_streets <- rbind(df_streets,df_new)

# Load additional manual changes (streets to remove)
df_remove_streets <- read_excel('data/manual_changes.xlsx', sheet = 'remove_full_streets')

df_streets <- df_streets %>% filter(!osm_id %in% df_remove_streets$osm_id)

### Create Progress File with Coordinate Data ###
#################################################

# Load existing coordinate file if available and identify new streets that need coordinates
if (file.exists("Data/df_progress.rds")) {
  df_progress <- readRDS("Data/df_progress.rds")
  df_new_streets <- df_streets %>% filter(!osm_id %in% df_progress$osm_id)
} else {
  df_progress <- NULL
  df_new_streets <- df_streets
}

# extract all the lat/lon coordinates & calculate distance between coordinates
df_coords <- NULL
for (i in 1:length(df_new_streets$osm_id)) {
  print(df_new_streets$name[i])
  tmp <- data.frame(sf::st_coordinates(df_new_streets$geometry[i])) %>%
         mutate(osm_id = df_new_streets$osm_id[i],
                id = row_number(),
                dist = 0) %>%
         select(osm_id, id, lon = X, lat = Y, dist) 

  for (j in 1:length(tmp$id)) {
    if (tmp$id[j] > 1) { tmp$dist[j] = geosphere::distHaversine(c(tmp$lon[j],tmp$lat[j]),c(tmp$lon[(j-1)],tmp$lat[(j-1)])) }
  }
  
  df_coords <- rbind(df_coords,tmp)
}

# add extra fields that needed to track progress
df_coords <- df_coords %>%
             left_join(select(df_streets,osm_id,name,city), by=c("osm_id")) %>%
             mutate(complete_dt = '',
                    set = '',
                    distance = -1,
                    complete = 0,
                    osm_unique_id = paste0(osm_id,"-",id))

# add new coordinates to existing dataframe
df_progress <- rbind(df_progress, df_coords)

# Load additional manual changes (nodes to remove)
df_remove_nodes   <- read_excel('data/manual_changes.xlsx', sheet = 'remove_partial_streets')

# assume either removing either end of street nodes (no removal of distance required)
# or removing beginning (need to set distance to 0 for new first node of remaining street)

for (i in 1:length(df_remove_nodes$osm_id)) {
  
  # define osm_id and nodes to remove
  street <- df_remove_nodes$osm_id[i]
  nodes <- c(df_remove_nodes$start_node[i]:df_remove_nodes$end_node[i])
  
  # define first node of street if removing first 1+ nodes
  if (min(nodes) == 1) { first_new_node = max(nodes) + 1 } else { first_new_node = NA}
  
  # remove from df_osm_street_coords_final.rds
  df_progress <- df_progress %>%
                 filter(!(osm_id == street & id %in% nodes)) %>%
                 mutate(dist = case_when(osm_id == street & id == first_new_node ~ 0,
                                         TRUE ~ dist))
}

### Add Polylines to streets ###
################################

# For each street without a polyine encode a polyline using Google API
for (i in 1:length(df_streets$osm_id)) {
  if (is.na(df_streets$polyline[i]) | df_streets$polyline[i] == '') {
    tmp <- df_coords %>% filter(osm_id == df_streets$osm_id[i])
    polyline <- encode_pl(lat=tmp$lat, lon=tmp$lon)
    df_streets$polyline[i] <- polyline
    print(df_streets$osm_id[i])
    Sys.sleep(0.25)
  }
}

### Save Final Files ###
########################

saveRDS(df_streets,"Data/df_osm_street_summary_final.rds")
saveRDS(df_progress,"Data/df_progress.rds")

