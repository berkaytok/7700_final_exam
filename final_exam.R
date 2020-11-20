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


#imports

gdb <- st_layers("data/SeminarR.gdb")
counties <- st_read("data/SeminarR.gdb", "USA_counties")
roads <- st_read("data/SeminarR.gdb", "USA_roads")
elev <- raster("data/Terrain1.tif")
cities <- read_csv("data/cities1.csv")

#convert csv to sf

cities_sf <- st_as_sf(cities, coords = c("long", "lat"), crs = 4269) 
cities_transform <- st_transform(cities_sf, crs = 5070)

# QUESTION 1 --------------------------------------------------------------

# 1.	How many cities located within 20 km buffer from the roads? 

counties_nalhi <- counties %>% 
  filter(!STATE_NAME %in% c("Alaska", "Hawaii"))
cities_nalhi <- cities_transform %>% 
  filter(!ST %in% c("HI", "AK"))

cities_roads <- roads %>% 
  select(ROUTE) %>%
  st_buffer(20000) %>%
  st_join(cities_nalhi) %>%
  group_by(ST) %>%
  summarise(num_of_cities = n(), 
            pop_mean = mean(POPULATION)) %>%
  arrange(desc(num_of_cities))

nrow(cities_roads)
head(cities_roads)

# QUESTION 2 --------------------------------------------------------------

# 2.	Extract elevation (Terrain1.tif) for all cities location and report summary statistics for elevation (mean and sd) by states.

cities_transform$elevation <- raster::extract(elev, cities_transform)

cit_elev <- cities_transform %>%
  group_by(ST) %>%
  summarise(elev_mean = mean(elevation),
            elev_sd = sd(elevation))

nrow(cit_elev)
head(cit_elev)

# QUESTION 3 --------------------------------------------------------------

# 3.	Create a choropleth map that best visualizing a state-level age group differences between AGE_5_17 and AGE_65_UP.    

pop_difference <- counties_nalhi %>%
  group_by(STATE_NAME) %>%
  summarise(age517 = sum(AGE_5_17), 
            age65 = sum(AGE_65_UP), 
            difference = abs(age65 - age517))

tm_shape(pop_difference) +
  tm_polygons(col = "difference", 
              title = "Age Difference", 
              breaks = c(min(pop_difference$difference),
                         (min(pop_difference$difference) + median(pop_difference$difference))/2,
                         median(pop_difference$difference), 
                         (max(pop_difference$difference) + median(pop_difference$difference))/2,
                         max(pop_difference$difference))) +
  tm_compass(position = c('right', 'bottom'), text.size = 0.7) +
  tm_scale_bar(position = c('right', 'bottom'), text.size = 0.4) +
  tm_layout(title = "Age Difference Map",
            title.bg.color = "black",
            title.bg.alpha = 0.5) +
  tm_legend()


