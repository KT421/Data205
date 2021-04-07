# Data205 Capstone Project

Katelyn Schreyer   
Spring 2021

This capstone project is intended to provide recommendations on the highest impact locations for new bicycle infrastructure in Montgomery County, Maryland. 

## Project Timeline

### Record Linkage - March 24 

- [x] Where do bike accidents occur in MoCo?
    - [x] What streets have the highest number of bike accidents
- [x] Connect Datasets - record linkage
    - [x] Bikeshare station names <-> MoCo Data station names
    - [x] OpenStreetMap street names <-> MoCo incident data street names
- [x] Move all of the time intensive data import/cleaning to separate script

### Finding Bike Lanes - March 31

- [x] Where do bike lanes already exist?
- [x] What is the incident rate per mile for each street

### Estimating Bike Traffic - April 7 

- [x] Progress Report - PPT
- [x] Which roads have high bike traffic?
    - [x] Which Capital Bikeshare Station pairs have the highest traffic?

### Using QGIS - April 14

- [ ] Use QGIS to examine and extract LTS data
    -[ ] Connect each incident to the LTS score and other available road data for that road segment

### Predicted Change in Incident Rate - April 21

- [ ] Correlation of LTS and Incident rate? Severity?
    - [ ] What other correlations exist? (weather, daylight, etc)
- [ ] Prediction of change in Incident rate if a bike lane is installed (decrease in LTS?)

### Synthesizing data - April 28

- [ ] Which roads or road segments should be prioritized for bike lane additions? 
    - [ ] Synthesis of low LTS, high bike traffic, possibly also incident/mile rate
- [ ] implement `{renv}` with finalized package list

### Draft Report - May 5

- [ ] Produce final list of recommendations
- [ ] Submit Draft final report
- [ ] Resume

### Final - May 12

- [ ] Final Report
- [ ] Final Presentation

## References

- [Geocomputation with R](https://geocompr.robinlovelace.net/)
- [30 Day Map Challenge](https://rud.is/books/30-day-map-challenge/)

## Data Sources

- [dataMontgomery Capital Bikeshare Stations](https://data.montgomerycountymd.gov/Community-Recreation/Bikeshare/pdp9-g3gw)
- [Capital Bikeshare system data](https://www.capitalbikeshare.com/system-data)
- [dataMontgomery Non-motorist collisions](https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Non-Motorists-Data/n7fk-dce5)
- [OpenStreetMap](https://www.openstreetmap.org/)
- [OpenRouteService](https://openrouteservice.org/)
- [Montgomery Countery Bicycle Master Plan](https://montgomeryplanning.org/planning/transportation/bicycle-planning/bicycle-master-plan/)