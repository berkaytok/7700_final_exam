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

cities <- st_as_sf(cities, coords = c("long","lat"),crs=4269)

#match cities sf object crs to roads crs

st_crs(cities) <- st_crs(roads) 

# QUESTION 1 --------------------------------------------------------------

# 1.	How many cities located within 20 km buffer from the roads? 

#take out alaska and hawaii

counties_nalhi <- counties %>% dplyr::filter(!STATE_NAME %in% c("Alaska", "Hawaii"))
cities_nalhi <- cities %>% dplyr::filter(!ST %in% c("HI", "AK"))

#buffer

roads_join <- roads %>% 
  st_buffer(20000) 
#%>% st_join(cities_exc)
plot(st_geometry(roads_join))
plot(st_geometry(cities_nalhi))
st_crs(cities_nalhi)





#roads_join_merge <- left_join(roads_join, cities)

numberofcities <- roads_join %>% select(CLASS) %>% group_by(CLASS) %>% 
  summarise(NumberCities = n()) %>% arrange(desc(NumberCities)) %>% as.data.frame(ncities[1,1:2]) 

numberofcities

# tot_cities <- as.data.frame(numberofcities[1,1:2])   
# tot_cities

#sel <- st_is_within_distance(cities_exc, roads, dist = 20000) # can only return a sparse matrix

plot(st_geometry(counties_exc))
plot(st_geometry(cities_sf))
plot(st_geometry(counties_grp))

#create logical vector

sel_logical <- lengths(sel) > 0
length(sel_logical[sel_logical == TRUE])

#intersect cities sf object to counties

#NO BOUNDING BOX?
join_city <- st_intersection(counties_exc, cities_exc)
plot(st_geometry(join_city))

# QUESTION 2 --------------------------------------------------------------

# 2.	Extract elevation (Terrain1.tif) for all cities location and report summary statistics for elevation (mean and sd) by states.

ext_elev <- raster::extract(elev, cities_exc)

plot(ext_elev)
plot(st_geometry(cities_exc))

# QUESTION 3 --------------------------------------------------------------

# 3.	Create a choropleth map that best visualizing a state-level age group differences between AGE_5_17 and AGE_65_UP.    



