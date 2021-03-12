##### Capital Bikeshare data #####

##### RQs #####

#•	Which Capital Bikeshare Station pairs have the highest traffic? 
#  o	What is the most direct route between those stations?  
#  o	How long does the most direct take by bike (predicted, from OpenRouteService)? 
#  o	How long is the median most direct ride time (actuals, from Capital Bikeshare)?
#  o	Based on the predicted direct ride time and median recorded ride time, how many bicyclists are traveling directly from Station A to Station B? 


##### Packages #####

library(tidyverse)
library(jsonlite)
`%notin%` <- Negate(`%in%`)
library(lubridate)
library(reclin)
library(readxl)
library(writexl)

##### Get data #####

# MoCo BikeShare Stations

url <- "https://data.montgomerycountymd.gov/resource/pdp9-g3gw.json"

moco_stations <- fromJSON(url)

# Bike Rides
# using subset of data in csv for now, if possible will switch to the API later

rides_jan2021 <- read_csv("202101-capitalbikeshare-tripdata.csv")

##### Process Data #####

#make the datetime strings into datetime objects
rides_jan2021$started_at <- as_datetime(rides_jan2021$started_at)
rides_jan2021$ended_at <- as_datetime(rides_jan2021$ended_at)

#calculate duration
rides_jan2021$duration <- difftime(rides_jan2021$ended_at, rides_jan2021$started_at, units = "mins")

all_stations <- rides_jan2021 %>%
  select(start_station_name, start_station_id,start_lat,start_lng) %>%
  distinct() 

#station names don't match exactly between MoCo Data and Bikeshare data - use reclin to match pairs

possible_pairs <- pair_blocking(all_stations,moco_stations) %>%
  compare_pairs(by=c("name"),default_comparator = jaro_winkler()) %>%
  score_problink() %>%
  select_n_to_m() %>%
  link(all_x = F,all_y=T) 

#reclin did a good job but needs some manual fixes
write_xlsx(possible_pairs,"possible_station_pairs.xlsx")

#filter to just the moco stations
stations <- possible_pairs %>%
  filter(name.y %in% bike_stations$name) %>%
  rename(name = name.x,
         moco_name = name.y) %>%
  mutate(coords = the_geom$coordinates) %>%
  mutate(lng = unlist(coords)[c(T,F)],
         lat = unlist(coords)[c(F,T)]) %>%
  select(name, lat, lng)

#limit to stations where Start AND End are in MoCo
rides_jan2021 <- rides_jan2021 %>%
  filter(start_station_name %in% stations$name) %>%
  filter(end_station_name %in% stations$name)

#find busiest routes
stations_traffic <- rides_jan2021 %>%
  group_by(start_station_name,end_station_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

#to do
# - manually clean bikeshare matching data... or come up with a matching algo that scores 100%
# - find routing data