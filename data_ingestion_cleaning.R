#this script collects all of my data ingestion and cleaning into one place.
#Since some of these take a long time to run, and all together they may take as long as an hour,
#I've saved the end result as an .Rdata file that can be read into other scripts directly instead of having to run this

#### LIBRARIES ####

library(tidyverse)
library(jsonlite)
`%notin%` <- Negate(`%in%`)
library(lubridate)
library(reclin)
library(readxl)
library(writexl)
library(osmdata)
library(sp)
library(sf)
library(ggspatial)
library(ggmap)

`%notin%` <- Negate(`%in%`)

#### CAPITOL BIKESHARE DATA ####

### Ingest MoCo Station List ###

# MoCo BikeShare Stations

url <- "https://data.montgomerycountymd.gov/resource/pdp9-g3gw.json"

moco_stations <- fromJSON(url)

### Ingest Bikeshare Rides data ###
# historical data only available as CSV
# use entire year 2020 as source

#data is split into 11 monthly files (April lumped in with May)
#iterate over each and read them in

bikerides <- NULL

file_list <- list.files("data/rides_data")

for (i in 1:length(file_list)) {
  rides <- read_csv(paste0("data/rides_data/",file_list[i]))
  
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

### Process Data ###

#make the datetime strings into datetime objects
bikerides$started_at <- as_datetime(bikerides$started_at)
bikerides$ended_at <- as_datetime(bikerides$ended_at)

#calculate duration
bikerides$duration <- difftime(bikerides$ended_at, bikerides$started_at, units = "mins")

all_stations <- bikerides %>%
  select(start_station_name) %>%
  distinct()

#station names don't match exactly between MoCo Data and Bikeshare data - use reclin to match pairs

#possible_pairs <- pair_blocking(all_stations,moco_stations) %>%
#  compare_pairs(by=c("name"),default_comparator = lcs()) %>%
#  score_problink() %>%
#  select_n_to_m() %>%
#  link(all_x = F,all_y=T) 

#reclin did a good job but needs some manual fixes
#export to excel for cleanup
#write_xlsx(possible_pairs,"possible_station_pairs.xlsx")

#re-import
#it appears that some stations in the MoCo list have been closed or moved; name.x is NA for those. Need to remove.
matched_stations <- read_excel("possible_pairs_cleaned.xlsx") %>%
  filter(!is.na(name.x))

#filter to just the moco stations
stations <- matched_stations %>%
  filter(name.y %in% moco_stations$name) %>%
  rename(name = name.x,
         moco_name = name.y) %>%
  select(name, moco_name) %>%
  left_join(moco_stations, by = c("moco_name" = "name")) %>%
  select(name, the_geom)

#limit to stations where Start AND End are in MoCo
bikerides <- bikerides %>%
  filter(start_station_name %in% stations$name) %>%
  filter(end_station_name %in% stations$name)

#### BIKE INCIDENTS DATA ####

#bike incidents list
#API returns only 1000 rows, use csv to access full dataset
bike_incidents <- read_csv("data/Crash_Reporting_-_Non-Motorists_Data.csv")

bike_incidents <- bike_incidents %>%
  filter(`Pedestrian Type` == "BICYCLIST")

#street data (this will take a few minutes)

moco_bb <- getbb("Montgomery County, MD")

#this gets bike trails
osm_cycleways <- opq(bbox = moco_bb) %>%
  add_osm_feature(key = 'highway', value = 'cycleway')


#streets w bike lanes
osm_bikelanes <- opq(bbox = moco_bb) %>%
  add_osm_feature(key = "cycleway") 


#bike lanes AND cycleways - full picture of bike infrastructure

osm_bike <- c(osmdata_sf(osm_cycleways),osmdata_sf(osm_bikelanes))

plot(osm_bike$osm_lines$geometry)

##### Get map #####

moco_map <- get_map(location = moco_bb)

#count accident streets

streets_with_incidents <- bike_incidents %>%
  group_by(`Road Name`) %>%
  summarise(n = n())

#data quality is good, no typos in street names. Must use an autofill system to populate

#examine the NAs

incidents_no_street <- bike_incidents %>%
  filter(is.na(`Road Name`))

#looks like mostly sidewalk and parking lot accidents. 
#Should attempt to keep the sidewalk accidents and attempt to extract the street names from the notes.

incidents_no_street <- incidents_no_street %>%
  filter(`Pedestrian Location` == "SIDEWALK")

#since there are only 30 I'll just drop them

streets_with_incidents <- bike_incidents %>%
  rename(name = `Road Name`) %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  na.omit()

#get data about all streets in MoCo that have names
## this may take a few minutes and will download 906 MB worth of data
osm_streets <- opq(bbox = moco_bb) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sf()

streets <- as(osm_streets$osm_lines,"SpatialLines")

streets_df <- as.data.frame(osm_streets$osm_lines)
streets_df$name <- toupper(streets_df$name)

#use {reclin} to match street names
#without a blocking variable this takes a long time....

#possible_pairs <- pair_blocking(streets_with_incidents,streets_df) %>%
#  compare_pairs(by=c("name"),default_comparator = lcs()) %>%
#  score_problink() %>%
#  select_n_to_m() %>%
#  link(all_x = T,all_y=F) 

#save output, clean by hand, and read it in so I don't have to run this block again

#write_xlsx(possible_pairs,"possible_street_pairs.xlsx")

#read in cleaned data
matched_streets <- read_excel("possible_street_pairs_cleaned.xlsx") %>%
  rename(osm_name = name.y,
         moco_name = name.x)

#save objects for use in other scripts


save(bike_incidents,matched_streets,moco_bb,moco_stations,osm_streets,bikerides,osm_bike, file="data/bike_data.RData")
