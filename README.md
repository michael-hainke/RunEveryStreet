# RunEveryStreet

This is the R code to track progress in running every street in North and West Vancouver, BC, Canada.  It can be customized to track progress in any city or area.  Uses Open Street Map (OSM) data to map all streets within area and user's Strava activity data to measure progress.  Produces summary files in Excel and maps using Google Maps.  Code is divided into 3 files:

## Table of Contents

[1 - Get OSM Street Data](#Get-Open-Street-Map-(OSM)-Street-Data)  
[2 - Update Progress](#Update-Progress)  
[3 - Map Activities](#Map-Activities)

### Define Common Functions  
0-run_every_street_functions.R  
This file contains a number of functions below which are needed for the rest of the code:  

- `get_streets(bb,nm)`  Retrieves all the street data using OSM API for a given bounding box (bb).  Convert the results from an sf object into a dataframe and add a column for the given identifying name (nm)  
- `extract_coordinates(df_strts)` Takes the dataframe created from the *get_streets* function and extracts the linestring objects in the 'geometry' column into a new dataframe  
- `calc_distance(df_coord)`  Calculates Haversine distance between consecutive points for each street  
- `street_length(df_strt, df_coord)` Calculates the total length of each street from 'df_coord' dataframe and adds it to the 'df_strt' dataframe  
- `get_polylines(df_strt, df_coord)` Given the 'df_coord' dataframe, encodes each set of lat/lon coordinates into a Google polyline and adds it to the 'df_strt' dataframe  
- `get_activity_raw(my_acts, token, activity)` Wrapper function for *get_activity_streams* function from RStrava package to get raw activity stream data and format it  
- `activity_snap_to_road(act,act_summary,google_key)` Uses Google API function to snap an activity's coordinates to the nearest road.  Provide 'act' dataframe of activity coordinates, an 'act_summary' dataframe with activity summary data and a 'google_key'  
- `map_activities(df, polyline_col, description_col, google_key)`  Generate a Google map by providing a dataframe 'df' of activities with a column containing the polyline 'polyline_col' and a description 'description_col'  
- `find_closest_point(street_lat, street_lon, df)` 


### Get Open Street Map (OSM) Street Data

Get all street data from a bounding box around North Vancouver and West Vancouver.  Remove any duplicates from overlap.  Then I did some manual curation to remove most unnamed streets, freeways, alleys, private roads etc.  Also added a few custom created roads where there were gaps.  Save final file as df_progress, which will become the file that all activities are matched against to measure progress <br>

### Update Progress

Get all new Strava activity data since last update.  If added to progress file then get raw stream coordinate data, as well as use the Google 'snap to road' API to snap all coordinate to nearest road to help with matching against OSM road nodes.  Also update the activity summary file which has summary data about each activity including a Google route polyline.  Just for fun I am also tracking the total number of basketball hoops and red Muskoka (Adirondack) chairs that I spot on my runs.  Then the script is matching all new activity coordinates to the nearest OSM street node and if within my set threshold (10m) then that node is marked as complete.  I output all nodes around current run to a Google map, which I review to manually update any nodes where were not marked as complete (just outside of 10m or an error in the activity tracking).  Those manual updates are saved to a .csv and then the script adds them to the progress file.  Finally, I generate an Excel file with a summary of progress to date, as well as last activity and last week.  There is a also a list of completed roads.  There are also some mapping functions to map the raw activity data, the 'snapped' activity data, and also a map of completed streets from the OSM data.

### Remove and Add Streets

#### Map Activities


``` r
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")
```
`ray_shade()` uses user specified light directions to calculate a global shadow map for an elevation

# Test
