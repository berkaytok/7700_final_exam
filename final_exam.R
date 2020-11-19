# Exam Questions ----------------------------------------------------------


# “SeminarR.gdb” contains “USA_counties” and “USA_roads” feature classes (epsg: 5070).
# “cities1.csv” file contains lat long column for geocoding as GCS NAD83 (epsg: 4269). 
# “Terrain1.tif” is elevation data vertical unit in meter (epsg: 5070).
# 
# 1.	How many cities located within 20 km buffer from the roads? Report how many cities and mean population (using a column “POPULATION” in cities1.csv) by states.  (exclude Alaska and Hawaii) 
# 2.	Extract elevation (Terrain1.tif) for all cities location and report summary statistics for elevation (mean and sd) by states.
# 3.	Create a choropleth map that best visualizing a state-level age group differences between AGE_5_17 and AGE_65_UP.    

#----

rm(list = ls())
cat('\f')

#libraries

library(raster)
library(tidyverse)
library(sf)

#imports

gdb <- st_layers("data/SeminarR.gdb")
counties <- st_read("data/SeminarR.gdb", "USA_counties")
roads <- st_read("data/SeminarR.gdb", "USA_roads")
elev <- raster("data/Terrain1.tif")
cities <- read_csv("data/cities1.csv")

#convert csv to sf

cities_sf <- st_as_sf(cities, coords = c("long","lat"),crs=4269)

#match cities sf object crs to roads crs

st_crs(cities_sf) <- st_crs(roads) 

# QUESTION 1 --------------------------------------------------------------

# 1.	How many cities located within 20 km buffer from the roads? 

#take out alaska and hawaii

counties_exc <- counties %>% dplyr::filter(!STATE_NAME %in% c("Alaska", "Hawaii"))
cities_exc <- cities %>% dplyr::filter(!ST %in% c("AL", "HI"))

#CAN'T GET THIS WORKING

sel = st_is_within_distance(cities_sf, counties_exc, dist = 20000) # can only return a sparse matrix
lengths(sel) > 0

#intersect cities sf object to counties

join_city <- st_intersection(cities_sf, counties_sf)
plot(st_geometry(join_city))

