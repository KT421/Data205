# Data205 Capstone Project

Katelyn Schreyer   
Spring 2021

This capstone project is intended to provide recommendations on the highest impact locations for new bicycle infrastructure in Montgomery County, Maryland. The recommendations will be based on each road's Bicycle Compatibility Index, the history of bicyclist involved accidents along that road, and an estimate of bike traffic based on Capital Bikeshare ridership data.

## Project Timeline

### Record Linkage - March 24 

- [x] Where do bike accidents occur in MoCo?
    - [x] What streets have the highest number of bike accidents
- [x] Connect Datasets - record linkage
    - [x] Bikeshare station names <-> MoCo Data station names
    - [x] OpenStreetMap street names <-> MoCo incident data street names
- [x] Move all of the time intensive data import/cleaning to separate script

### Finding Bike Lanes - March 31

- [ ] Where do bike lanes already exist?
    - [ ] Is there a correlation between bike lane status and injury number/severity?
    - [ ] What other correlations exist? (weather, daylight, etc)

### Estimating Bike Traffic - April 7 

- [ ] Progress Report - PPT
- [ ] Which roads have high bike traffic?
    - [x] Which Capital Bikeshare Station pairs have the highest traffic?
    - [ ] What is the most direct route between those stations? 
    - [ ] How long does the most direct take by bike (predicted)?
    - [ ] How long is the median most direct ride time (actuals)?
    - [ ] Based on the predicted direct ride time and median recorded ride time, how many bicyclists are traveling directly from Station A to Station B? 


### Calculating BCI - April 14

- [ ] What is the bicycle compatibility Index (BCI) for each road in Montgomery County where a bicyclist-involved accident has occurred in 2017-2020?

### Calculating BCI - April 21

- [ ] BCI Calculation - continued

### Synthesizing data - April 28

- [ ] Which roads or road segments should be prioritized for bike lane additions? 
    - [ ] Synthesis of low BCI, high bike traffic, possibly also incident/mile rate
- [ ] implement `{renv}` with finalized package list

### Draft Report - May 5

- [ ] Produce final list of recommendations
- [ ] Submit Draft final report
- [ ] Resume

### Final - May 12

- [ ] Final Report
- [ ] Final Presentation
