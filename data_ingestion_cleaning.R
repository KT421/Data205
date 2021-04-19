#this script collects all of my data ingestion and cleaning into one place.
#Since some of these take a long time to run, and all together they may take as long as an hour,
#I've saved the end result as an .Rdata file that can be read into other scripts directly instead of having to run this

#### LIBRARIES ####

library(tidyverse)
library(jsonlite)
`%notin%` <- Negate(`%in%`)
library(reclin)
library(readxl)
library(writexl)
library(osmdata)
library(sp)
library(sf)
library(ggspatial)
library(ggmap)

`%notin%` <- Negate(`%in%`)

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
matched_streets <- read_excel("data/possible_street_pairs_cleaned.xlsx") %>%
  rename(osm_name = name.y,
         moco_name = name.x)

#save objects for use in other scripts

save(bike_incidents,matched_streets,moco_bb,osm_streets,osm_bike, file="data/bike_data.RData")
