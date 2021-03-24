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

load("data/bike_data.Rdata")

#Visualizations of where bike accidents occur

map2 <- get_map(location = moco_bb)

plot(map2)

ggmap(map2) +
  geom_point(data = bike_incidents, aes(x = Longitude, y = Latitude, color = `Injury Severity`)) 

#find street length