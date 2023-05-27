library(tidyverse)  # data manipulation
library(config)     # config file for credentials
library(googleway)  # Google API functions
library(geosphere)  # calculate distances
library(osmdata)    # get Open Street Map data
library(ggmap)      # mapping function
library(rlist)      # decoding lists
library(sf)         # spatial vector data

# Load Required Functions
source("0-run_every_street_functions.R")

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

# save files
saveRDS(df_streets,"Data/df_osm_street_summary_raw.rds")
saveRDS(df_coords,"Data/df_osm_street_coords_raw.rds")

### Update Streets with Manual Changes ###
##########################################

# load raw files (if necessary)
if (!exists("df_streets")) { df_streets <- readRDS("Data/df_osm_street_summary_raw.rds") }
if (!exists("df_coords")) { df_coords <- readRDS("Data/df_osm_street_coords_raw.rds") }

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

# remove all U2 streets (with no name) and also Highway 1 (freeway)
df_streets <- df_streets %>%
              filter(valid_street == 1,
                     !is.na(name),
                     name != 'Trans-Canada Highway')

# combine coords with new manually created streets and calculate distances
df_coords <- rbind(read.csv("Data/custom_streets.csv"), df_coords) %>%
             calc_distance()

# join with existing coordinates and streets
df_streets <- rbind(read.csv("Data/custom_strt_summary.csv"),df_streets) %>%
              street_length(df_coords) %>%
              get_polylines(df_coords)

df_coords <- df_coords %>%
             filter(osm_id %in% df_streets$osm_id)

# save final files
saveRDS(df_streets,"Data/df_osm_street_summary_final.rds")
saveRDS(df_coords,"Data/df_osm_street_coords_final.rds")

### Create Progress File ###
############################

# load files (if required)
if (!exists("df_streets")) { df_streets <- readRDS("Data/df_osm_street_summary_final.rds") }
if (!exists("df_coords")) { df_coords <- readRDS("Data/df_osm_street_coords_final.rds") }

# join name and city
df_progress <- df_coords %>%
               left_join(df_streets %>% select(osm_id, name, city), by="osm_id") %>%
               mutate(complete_dt = '',
                      set = '',
                      distance = -1,
                      complete = 0,
                      osm_unique_id = paste0(osm_id,"-",id))

# save data
saveRDS(df_progress,"Data/df_progress.rds")



