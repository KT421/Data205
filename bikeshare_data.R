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

load("data/bike_data.Rdata")


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

