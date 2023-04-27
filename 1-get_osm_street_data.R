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
saveRDS(df_streets,"Data/df_osm_street_summary_raw.rds")
saveRDS(df_coords,"Data/df_osm_street_coords_raw.rds")

### Update Streets with Manual Changes ###
##########################################

# load raw files (if necessary)
if (!exists(df_streets)) { df_streets <- readRDS("Data/df_osm_street_summary_raw.rds") }
if (!exists(df_coords)) { df_coords <- readRDS("Data/df_osm_street_coords_raw.rds") }

# load manual updates (remove invalid streets & rename NA streets)
df_changes <- read.csv("Data/road_updates.csv") %>%
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
df_coords_new <- read.csv("Data/custom_streets.csv") %>%
                 mutate(dist = 0) %>%
                 calc_distance()

# load new street summary and calculate length and encode polylines
df_strts_new <- read.csv("Data/custom_strt_summary.csv") %>%
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
saveRDS(df_streets,"Data/df_osm_street_summary_final.rds")
saveRDS(df_coords,"Data/df_osm_street_coords_final.rds")


### Create Progress File ###
############################

# load files (if required)
if (!exists(df_streets)) { df_streets <- readRDS("Data/df_osm_street_summary_final.rds") }
if (!exists(df_coords)) { df_coords <- readRDS("Data/df_osm_street_coords_final.rds") }

# join name and city
df_progress <- df_coords %>%
               left_join(df_streets %>% select(osm_id, name, city), by="osm_id") %>%
               mutate(complete_dt = '',
                      set = '',
                      distance = -1)

# save data
saveRDS(df_progress,"Data/df_progress.rds")



