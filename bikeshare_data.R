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
# historical data only available as CSV
# use entire year 2020 as source

#data is split into 11 monthly files (April lumped in with May)
#iterate over each and read them in

bikerides <- NULL

file_list <- list.files("rides_data")

for (i in 1:length(file_list)) {
  rides <- read_csv(paste0("rides_data/",file_list[i]))
  
  #cleanup change in data format midway through 2020
  if (i <= 3) {
    rides <- rides %>%
      rename(started_at = `Start date`,
             ended_at = `End date`,
             start_station_name = `Start station`,
             end_station_name = `End station`)
  }
  
  rides <- rides %>%
    select(started_at,ended_at,start_station_name,end_station_name)
  
  bikerides <- rbind(bikerides,rides)
}

##### Process Data #####

#make the datetime strings into datetime objects
bikerides$started_at <- as_datetime(bikerides$started_at)
bikerides$ended_at <- as_datetime(bikerides$ended_at)

#calculate duration
bikerides$duration <- difftime(bikerides$ended_at, bikerides$started_at, units = "mins")

all_stations <- bikerides %>%
  select(start_station_name, start_station_id) %>%
  distinct()

#station names don't match exactly between MoCo Data and Bikeshare data - use reclin to match pairs

possible_pairs <- pair_blocking(all_stations,moco_stations) %>%
  compare_pairs(by=c("name"),default_comparator = lcs()) %>%
  score_problink() %>%
  select_n_to_m() %>%
  link(all_x = F,all_y=T) 

#reclin did a good job but needs some manual fixes
#export to excel for cleanup
write_xlsx(possible_pairs,"possible_station_pairs.xlsx")

#re-import
#it appears that some stations in the MoCo list have been closed or moved; name.x is NA for those. Need to remove.
matched_stations <- read_excel("possible_pairs_cleaned.xlsx") %>%
  filter(!is.na(name.x))

#filter to just the moco stations
stations <- matched_stations %>%
  filter(name.y %in% bike_stations$name) %>%
  rename(name = name.x,
         moco_name = name.y) %>%
  select(name, moco_name) %>%
  left_join(moco_stations, by = c("moco_name" = "name")) %>%
  select(name, the_geom)

#limit to stations where Start AND End are in MoCo
bikerides <- bikerides %>%
  filter(start_station_name %in% stations$name) %>%
  filter(end_station_name %in% stations$name)

#find busiest routes
stations_traffic <- bikerides %>%
  group_by(start_station_name,end_station_name) %>%
  summarise(n = n()) %>%
  filter(start_station_name != end_station_name) %>% #filter out roundtrips that begin and end at a single station
  arrange(desc(n))

#to do
# - find routing data most direct route
# - how long does most direct route take?
# - what's the avg (median) ride duration?