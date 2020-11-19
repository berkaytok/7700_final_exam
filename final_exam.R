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
library(tmap)
library(rgdal)
st_centroid(counties)
tmap_mode("view")


#imports

gdb <- st_layers("data/SeminarR.gdb")
counties <- st_read("data/SeminarR.gdb", "USA_counties")
roads <- st_read("data/SeminarR.gdb", "USA_roads")
elev <- raster("data/Terrain1.tif")
cities <- read_csv("data/cities1.csv")

#convert csv to sf

cities <- st_as_sf(cities, coords = c("long","lat"), crs=5070) #had to switch lat/long columns to long/lat
cit_vert <- st_as_sf(cities, coords = c("lat","long"), crs=5070) #this looks vertical

st_crs(cities) ==  st_crs(counties) #looks correct
st_crs(counties) == st_crs(roads)
crs(elev)
qtm(cities)
qtm(cit_vert)
plot(st_geometry(cities))
plot(st_geometry(cit_vert)) #looks vertical
#trying to figure things out, no changes done here

st_bbox(counties) #      xmin       ymin       xmax       ymax 
                  #-6293474.1   311822.4  2256319.2  6198810.9 

st_bbox(cities)   #      xmin       ymin       xmax       ymax 
                  #19.06873 -176.64275   71.26784  178.87460

st_bbox(elev)     #    xmin     ymin     xmax     ymax 
                  #-2453944  -252919  2315056  3723081

st_is_longlat(counties) #I don't understand why these return FALSE
st_is_longlat(cities) #I don't understand why these return FALSE
st_is_longlat(roads) #I don't understand why these return FALSE

qtm(counties) #looks correct on the basemap
qtm(cities) #looks out of bounds on the basemap
qtm(roads) #looks out of bounds on the basemap
crs(elev)
st_crs(cities)

#counties_join <- left_join(counties_nalhi, cities, by = c("STATE_NAME", "ST"))

#match cities sf object crs to roads crs

st_crs(cities) <- st_crs(roads) 

# QUESTION 1 --------------------------------------------------------------

# 1.	How many cities located within 20 km buffer from the roads? 

#take out alaska and hawaii

counties_nalhi <- counties %>% dplyr::filter(!STATE_NAME %in% c("Alaska", "Hawaii"))
cities_nalhi <- cities %>% dplyr::filter(!ST %in% c("HI", "AK"))


plot(st_geometry(counties_nalhi))
plot(st_geometry(cities_nalhi), add = T)
plot(st_geometry(cities_nalhi))

#buffer

roads_join <- roads %>% 
  st_buffer(20000) %>% st_join(cities_nalhi)
plot(st_geometry(roads_join))
plot(st_geometry(cities_nalhi))
st_crs(cities_nalhi)

st_intersection(counties_nalhi, roads)


numberofcities <- roads_join %>% select(CLASS) %>% group_by(CLASS) %>% 
  summarise(numbercities = n()) %>% arrange(desc(numbercities)) %>% as.data.frame(numberofcities[1,1:2]) 

numberofcities

st_bbox(cities_nalhi)
st_bbox(counties_nalhi)

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

cities_nalhi$elevation <- raster::extract(elev, cities_nalhi)
colnames(cities_nalhi)



qtm(counties_nalhi) + 
  tm_basemap(leaflet::providers$Stamen.toner)
qtm(cities_nalhi) +
  tm_basemap(leaflet::providers$Stamen.toner)

# QUESTION 3 --------------------------------------------------------------

# 3.	Create a choropleth map that best visualizing a state-level age group differences between AGE_5_17 and AGE_65_UP.    

states <- counties %>%
  group_by(STATE_NAME) %>% 
  tally()
