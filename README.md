# RunEveryStreet
Objective: To track progress of goal to run every street in North Vancouver and West Vancouver in 2023.

Use Open Street Map (OSM) data to measure progress from all run activities tracked in Strava.  This code is still a work in progress, but the basic structure is as follows: <br>
0 - Create a file to store all the common functions required across all remaining files <br>
1 - Using OSM API query all streets and coordinate data, manually curate to define complete list and data for all roads in North and West Van <br>
2 - Using Strava API query all activities and coordinate data <br>
3 - Combine the OSM and Strava data to summarize the current progress <br>
