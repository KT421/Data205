##### Bike Incident Data #####

##### RQs #####

#Where do bike accidents occur in MoCo?
#What streets have the highest number of bike accidents (normalize per mile/year)

##### Packages #####

library(tidyverse)
library(ggmap)
library(osmdata)
library(tidyverse)
library(sp)
library(sf)
library(ggspatial)
library(ggmap)
library(writexl)

##### Get Data #####

#bike incidents list
#API returns only 1000 rows, use csv to access full dataset
bike_incidents <- read_csv("data/Crash_Reporting_-_Non-Motorists_Data.csv")

bike_incidents <- bike_incidents %>%
  filter(`Pedestrian Type` == "BICYCLIST")

#street data (this will take a few minutes)

moco_bb <- getbb("Montgomery County, MD")

osm_cycleways <- opq(bbox = moco_bb) %>%
  add_osm_feature(key = 'highway', value = 'cycleway') %>%
  osmdata_sp()

cycleways <- as(osm_cycleways$osm_lines,"SpatialLines")

##### Get map #####

moco_map <- get_map(location = moco_bb)

#visualize accident locations

ggmap(moco_map) +
  layer_spatial(cycleways) +
  geom_point(data = bike_incidents, aes(x = Longitude, y = Latitude, color = `Injury Severity`)) 

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

#now, can I normalize by mile?
#will need to find out low long each road actually is

#get data about all streets in MoCo that have names
## this may take a few minutes and will 906 MB worth of data
osm_streets <- opq(bbox = moco_bb) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_sp()

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

write_xlsx(possible_pairs,"possible_street_pairs.xlsx")

matched_streets <- read_excel("possible_street_pairs_cleaned.xlsx") %>%
  rename(osm_name = name.y,
         moco_name = name.x)
#to do: 
#-filter for streets of interest (has had an accident)
#-find length of road/road segment
