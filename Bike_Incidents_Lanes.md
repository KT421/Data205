Bike Incidents & Existing Lanes
================

# Packages & Data

``` r
library(tidyverse)
library(ggmap)
library(tidyverse)
library(sf)
library(geosphere)
library(ggspatial)
library(GGally)
library(mapview)
library(readxl)

load("data/bike_data.RData")
```

# Visualizaing

``` r
map2 <- get_map(location = moco_bb)

ggmap(map2) +
  geom_point(data = bike_incidents, aes(x = Longitude, y = Latitude, color = `Injury Severity`)) +
  layer_spatial(osm_bike$osm_lines$geometry) + 
  labs(title = "Bike Incident Severity Map with Bike Lanes") +
  theme(legend.position = c(0.8,0.8))
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggsave("incidents_cycleways.png", width = 9, height = 9, units = "in")
```

# Find road lenghts

This makes the assumption that if a cycleway (bike lane or trail)
exists, the cyclist was using it.

``` r
# Length of road data (for incident per mile calculation)
streets <- as(osm_streets$osm_lines,"SpatialLinesDataFrame")

streets_df <- data.frame(streets@data)


#Each road has multiple segments on our list because its characteristics can change
#loop through each to find all the segment ids, then add up all the segment lengths

road_lengths <- NULL

for (i in matched_streets$osm_name) {

street <- streets_df %>%
  filter(toupper(name) == i) 

road_length <- 0

  for (j in street$osm_id)  {

  x <- paste0("streets@lines$`",j,"`@Lines")

  segment <- lengthLine(eval(parse(text = x))[[1]]@coords)

  road_length <- road_length + segment

  
  }
  road_name <- street$name
  road_df <- data.frame(road_name,road_length)
  road_lengths <- bind_rows(road_lengths,road_df)

}

#it worked but there's a row for each segment instead of a row for each street. Also need to convert to miles.

road_lengths <- road_lengths %>%
  distinct() %>%
  mutate(road_length = road_length * 0.00062137,
         road_name = toupper(road_name)) %>%
  rename(osm_name = road_name)

matched_streets <- matched_streets %>%
  inner_join(road_lengths) %>%
  mutate(incidents_per_mile = n / road_length)
```

    ## Joining, by = "osm_name"

``` r
#which streets have the highest accident rate?

accident_rate <- matched_streets %>%
  select(osm_name,n,road_length,incidents_per_mile) %>%
  arrange(desc(incidents_per_mile)) %>%
  view()

#lets filter out roads with 3 or fewer accidents just to focus on places that are higher impact

accident_rate_trimmed <- accident_rate %>%
  filter(n >= 3)

head(accident_rate_trimmed)
```

    ## # A tibble: 6 x 4
    ##   osm_name                 n road_length incidents_per_mile
    ##   <chr>                <dbl>       <dbl>              <dbl>
    ## 1 DORSET AVENUE            7       1.25                5.59
    ## 2 STEDWICK ROAD            5       1.50                3.33
    ## 3 FENTON STREET            4       1.25                3.21
    ## 4 LITTLE FALLS PARKWAY     8       2.50                3.20
    ## 5 ARDENNES AVENUE          3       0.950               3.16
    ## 6 WOODMONT AVENUE          4       1.50                2.67

# EDA of joined incident and LTS data

Used QGIS and the NNJoin plugin to join the LTS data to the Crash
Reporting dataset; each incident has been joined to the characteristics
of the nearest road segment.

[LTS
Methodology](https://montgomeryplanning.org/wp-content/uploads/2017/11/Appendix-D.pdf)

Revised LTS Key: - LTS 0 – None (separated trails and breezeways) - LTS
1 – Very Low (Neighborhood roads, suitable for children) - LTS 2 – Low
(“suitable for most adults”) - LTS 2.5 – Moderate Low - LTS 3 – Moderate
High (four lane road with bike lane) - LTS 4 – High (&gt;40mph road) -
LTS 5 – Very High (“few bicyclists will brave these roads”) - LTS 9 -
(not defined in methodology document; appears to be freeways/ramps.
Accidents reported on these roads are n=11, 9 of which are labeled as at
crossings) - TRANS - (not defined in methodology document; appears to be
Transit station related?) - P - (not defined in methodology document;
appears to be parking lots)

``` r
joined_lts_crash <- read_excel("joined_lts_crash.xlsx")

#The `distance` column is distance to nearest neighbor; let's remove those that are distant from any given street, allowing for GPS imprecision 

joined_lts_crash_filtered <- joined_lts_crash %>%
  filter(distance < 50) %>%
  filter(!is.na(join_LTS_REV))

order <- c("NO APPARENT INJURY",
           "POSSIBLE INJURY",
           "SUSPECTED MINOR INJURY",
           "SUSPECTED SERIOUS INJURY",
           "FATAL INJURY")
```

## LTS by incident severity

``` r
ggplot(joined_lts_crash_filtered,aes(x=join_LTS_REV,fill=factor(`Injury Severity`, levels = order))) +
  geom_bar() +
  labs(title="Bike Incident Injury Severity by \n Level of Traffic Stress (LTS) of nearest street",
       x="LTS",
       y="Number of incidents",
       fill="Injury Severity")
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
table(joined_lts_crash_filtered$join_LTS,factor(joined_lts_crash_filtered$`Injury Severity`,levels = order))
```

    ##    
    ##     NO APPARENT INJURY POSSIBLE INJURY SUSPECTED MINOR INJURY
    ##   1                 31              52                    136
    ##   2                 18              31                     65
    ##   3                 10              20                     36
    ##   4                 16              65                    110
    ##   9                  2               1                      6
    ##   P                  0               1                      1
    ##    
    ##     SUSPECTED SERIOUS INJURY FATAL INJURY
    ##   1                       22            0
    ##   2                        9            0
    ##   3                        7            3
    ##   4                       24            6
    ##   9                        2            0
    ##   P                        0            0

## Injury Severity by other road characteristics

``` r
ggplot(joined_lts_crash_filtered,aes(x=join_AVG_SLOPE,y=factor(`Injury Severity`, levels = order))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1)
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(joined_lts_crash_filtered,aes(x=as.double(join_SPEEDLIM),y=factor(`Injury Severity`, levels = order))) +
  geom_boxplot()  +
  geom_jitter(alpha = 0.1)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning: Removed 29 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 29 rows containing missing values (geom_point).

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggplot(joined_lts_crash_filtered,aes(x=as.double(join_BKLNWIDTH),y=factor(`Injury Severity`, levels = order))) +
  geom_jitter(alpha=0.3,shape = 16)
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

# Correlation of Incident Severity and LTS

Since we don’t have traffic data, we can’t make assumptions about the
impact of LTS on incident rate. We *can* make assumptions about incident
severity.
