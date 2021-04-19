# Prioritizing New Bike Infrastructure in Montgomery County, MD

Katelyn Schreyer   
Spring 2021
DATA 205

This capstone project is intended to provide recommendations on the highest impact locations for new bicycle infrastructure in Montgomery County, Maryland. 

## File Directory

Note: R Markdown notebooks are provided as `.md` and `.Rmd` files. The `.md` files are configured to display well in the github web-based interface, while the `.Rmd` files are suitable for cloning into RStudio to run locally. 

- **FILE DIRECTORY GOES HERE**

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

- [x] Use QGIS to examine and extract LTS data  
    - [x] Connect each incident to the LTS score and other available road data for that road segment

### Predicted Change in Incident Rate - April 21

- [x] Association of LTS and injury severity?
    - [x] What other associations exist? (weather, daylight, etc)
- [x] Clean up git repo

### Synthesizing data - April 28

- [ ] Which roads or road segments should be prioritized for bike lane additions? 
    - [ ] Synthesis of high incident/mile rate, existing infrastructure, planned infrastructure 

### Draft Report - May 5

- [ ] Produce final list of recommendations
- [ ] Submit Draft final report
- [ ] Resume

### Final - May 12

- [ ] Final Report
- [ ] Final Presentation

## Data Sources

- [dataMontgomery Non-motorist collisions](https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Non-Motorists-Data/n7fk-dce5)
- [OpenStreetMap](https://www.openstreetmap.org/)
- [Montgomery Countery Bicycle Master Plan](https://montgomeryplanning.org/planning/transportation/bicycle-planning/bicycle-master-plan/)