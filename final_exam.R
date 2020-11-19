rm(list = ls())
cat('\f')

#libraries

library(tidyverse)
library(sf)
library(raster)

#imports

gdb <- st_layers("data/SeminarR.gdb.zip")
counties <- st_read("data/SeminarR.gdb.zip", "USA_counties")
roads <- st_read("data/SeminarR.gdb.zip", "USA_roads")
elev <- raster("data/Terrain1.tif")
cities <- read_csv("data/cities1.csv")

#convert csv to sf

cities_sf <- st_as_sf(cities, coords = c("long","lat"),crs=4269)

#match cities sf object crs to roads crs

st_crs(cities_sf) <- st_crs(roads) 


# QUESTION 1 --------------------------------------------------------------

# 1.	How many cities located within 20 km buffer from the roads? 

sel = st_is_within_distance(cities_sf, counties_exc, dist = 20000) # can only return a sparse matrix
lengths(sel) > 0

#take out alaska and hawaii

counties_exc <- counties %>% dplyr::filter(!STATE_NAME %in% c("Alaska", "Hawaii"))
cities_exc <- cities %>% dplyr::filter(!ST %in% c("AL", "HI"))

#intersect cities sf object to counties

join_city <- st_intersection(cities_sf, counties_)
plot(st_geometry(join_city))

