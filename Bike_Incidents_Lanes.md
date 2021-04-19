Bike Incidents & Existing Lanes
================

# Packages & Data

``` r
library(tidyverse)
library(ggmap)
library(ggspatial)
library(tidyverse)
library(sf)
library(geosphere)
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
  mutate(incidents_per_mile = n / road_length,
         incidents_per_mile_annual = incidents_per_mile / 4)
```

    ## Joining, by = c("osm_name", "road_length")

``` r
#which streets have the highest accident rate?

accident_rate <- matched_streets %>%
  select(osm_name,moco_name,n,road_length,incidents_per_mile,incidents_per_mile_annual) %>%
  arrange(desc(incidents_per_mile_annual)) 

#lets filter out roads with 3 or fewer accidents just to focus on places that are higher impact

accident_rate_trimmed <- accident_rate %>%
  filter(n >= 3)

head(accident_rate_trimmed)
```

    ## # A tibble: 6 x 6
    ##   osm_name     moco_name       n road_length incidents_per_m… incidents_per_mil…
    ##   <chr>        <chr>       <dbl>       <dbl>            <dbl>              <dbl>
    ## 1 DORSET AVEN… DORSET AVE      7       1.25              5.59              1.40 
    ## 2 STEDWICK RO… STEDWICK RD     5       1.50              3.33              0.833
    ## 3 FENTON STRE… FENTON ST       4       1.25              3.21              0.801
    ## 4 LITTLE FALL… LITTLE FAL…     8       2.50              3.20              0.799
    ## 5 ARDENNES AV… ARDENNES A…     3       0.950             3.16              0.789
    ## 6 WOODMONT AV… WOODMONT A…     4       1.50              2.67              0.669

# EDA of joined incident and LTS data

Used QGIS and the NNJoin plugin to join the LTS data to the Crash
Reporting dataset; each incident has been joined to the characteristics
of the nearest road segment.

[LTS
Methodology](https://montgomeryplanning.org/wp-content/uploads/2017/11/Appendix-D.pdf)

Revised LTS Key: - LTS 0 – None (separated trails and breezeways)  
- LTS 1 – Very Low (Neighborhood roads, suitable for children)  
- LTS 2 – Low (“suitable for most adults”)  
- LTS 2.5 – Moderate Low  
- LTS 3 – Moderate High (four lane road with bike lane)  
- LTS 4 – High (&gt;40mph road)  
- LTS 5 – Very High (“few bicyclists will brave these roads”)  
- LTS 9 - (not defined in methodology document; appears to be
freeways/ramps. Accidents reported on these roads are n=11, 9 of which
are labeled as at crossings)  
- TRANS - (not defined in methodology document; appears to be Transit
station related?)  
- P - (not defined in methodology document; appears to be parking lots)

``` r
joined_lts_crash <- read_excel("data/joined_lts_crash.xlsx")

#The `distance` column is distance to nearest neighbor; let's remove those that are distant from any given street, allowing for GPS imprecision 

joined_lts_crash_filtered <- joined_lts_crash %>%
  filter(distance < 50) %>%
  filter(!is.na(join_LTS_REV)) %>%
  filter(join_LTS_REV != "TRANS") %>%
  filter(join_LTS_REV != "P")

order <- c("NO APPARENT INJURY",
           "POSSIBLE INJURY",
           "SUSPECTED MINOR INJURY",
           "SUSPECTED SERIOUS INJURY",
           "FATAL INJURY")

joined_lts_crash_filtered$`Injury Severity` <- factor(joined_lts_crash_filtered$`Injury Severity`,levels = order)
```

## LTS by incident severity

``` r
ggplot(joined_lts_crash_filtered,aes(x=join_LTS_REV,fill=`Injury Severity`)) +
  geom_bar() +
  labs(title="Bike Incident Injury Severity by \n Level of Traffic Stress (LTS) of nearest street",
       x="LTS",
       y="Number of incidents",
       fill="Injury Severity")
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Injury Severity by other road characteristics

``` r
ggplot(joined_lts_crash_filtered,aes(x=join_AVG_SLOPE,y=`Injury Severity`)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  labs(title = "Injury Severity by \n Average Slope of road segment",
       x = "Average Slope",
       y = "Injury Severity")
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(joined_lts_crash_filtered,aes(x=as.double(join_SPEEDLIM),y=`Injury Severity`)) +
  geom_boxplot()  +
  geom_jitter(alpha = 0.1) +
  labs(title = "Injury Severity by \n speed limit of road segment",
       x = "Speed Limit",
       y = "Injury Severity")
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning: Removed 26 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 26 rows containing missing values (geom_point).

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggplot(joined_lts_crash_filtered,aes(x=as.double(join_BKLNWIDTH),y=`Injury Severity`)) +
  geom_jitter(alpha=0.3,shape = 16) +
  labs(title = "Injury Severity by \n Bike Lane Width of road segment",
       x = "Bike Lane Width (in feet) \n (0 means no bike lane)",
       y = "Injury Severity")
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

# Association of Incident Severity and LTS

Since we don’t have traffic data, we can’t make assumptions about the
impact of LTS on incident rate - we just don’t have the denominator. We
*can* make assumptions about incident severity.

``` r
#contingency table of LTS vs injury severity
table(joined_lts_crash_filtered$join_LTS_REV,joined_lts_crash_filtered$`Injury Severity`)
```

    ##      
    ##       NO APPARENT INJURY POSSIBLE INJURY SUSPECTED MINOR INJURY
    ##   0                    3               0                      5
    ##   1                   26              44                     99
    ##   2                   14              17                     61
    ##   2.5                  3              14                     27
    ##   3                   13              28                     44
    ##   4                   16              57                     98
    ##   5                    0               8                     13
    ##   9                    2               1                      6
    ##      
    ##       SUSPECTED SERIOUS INJURY FATAL INJURY
    ##   0                          1            0
    ##   1                         15            0
    ##   2                         11            0
    ##   2.5                        2            2
    ##   3                         10            2
    ##   4                         19            4
    ##   5                          4            1
    ##   9                          2            0

``` r
#distribution, row-wise
prop.table(table(joined_lts_crash_filtered$join_LTS_REV,joined_lts_crash_filtered$`Injury Severity`), margin = 1)
```

    ##      
    ##       NO APPARENT INJURY POSSIBLE INJURY SUSPECTED MINOR INJURY
    ##   0           0.33333333      0.00000000             0.55555556
    ##   1           0.14130435      0.23913043             0.53804348
    ##   2           0.13592233      0.16504854             0.59223301
    ##   2.5         0.06250000      0.29166667             0.56250000
    ##   3           0.13402062      0.28865979             0.45360825
    ##   4           0.08247423      0.29381443             0.50515464
    ##   5           0.00000000      0.30769231             0.50000000
    ##   9           0.18181818      0.09090909             0.54545455
    ##      
    ##       SUSPECTED SERIOUS INJURY FATAL INJURY
    ##   0                 0.11111111   0.00000000
    ##   1                 0.08152174   0.00000000
    ##   2                 0.10679612   0.00000000
    ##   2.5               0.04166667   0.04166667
    ##   3                 0.10309278   0.02061856
    ##   4                 0.09793814   0.02061856
    ##   5                 0.15384615   0.03846154
    ##   9                 0.18181818   0.00000000

``` r
#Test for independence
#Chi-squared test
chisq.test(table(joined_lts_crash_filtered$join_LTS_REV,joined_lts_crash_filtered$`Injury Severity`))
```

    ## Warning in chisq.test(table(joined_lts_crash_filtered$join_LTS_REV,
    ## joined_lts_crash_filtered$`Injury Severity`)): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(joined_lts_crash_filtered$join_LTS_REV, joined_lts_crash_filtered$`Injury Severity`)
    ## X-squared = 36.449, df = 28, p-value = 0.1315

``` r
#p = 0.1315, but too many values of <5 for the results to be valid

#Fishers test
#Uses monte carlo simulation when the contingency table is larger than 2x2 (simulate.p.value = T), which also means we can't get confidence intervals out of it.
fisher.test(joined_lts_crash_filtered$join_LTS_REV,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$join_LTS_REV and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.03448
    ## alternative hypothesis: two.sided

``` r
# p = 0.04048 (will vary as seed changes)
#With p-values of <0.05, we can reject the null hypothesis and assume that these LTS and Injury Severity are associated 
```

Next, test some other associations

``` r
#weather - no association
fisher.test(joined_lts_crash_filtered$Weather,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$Weather and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.5057
    ## alternative hypothesis: two.sided

``` r
#light status (daylight, dawn, etc) - highly associated (p = 0.007) but n is very low
fisher.test(joined_lts_crash_filtered$Light,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$Light and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.005997
    ## alternative hypothesis: two.sided

``` r
#number of lanes - no association
fisher.test(joined_lts_crash_filtered$join_MP_LANES,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$join_MP_LANES and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.1129
    ## alternative hypothesis: two.sided

``` r
#speed limit - highly associated (p = 0.008) - expected as speed limit is a factor in LTS
fisher.test(joined_lts_crash_filtered$join_SPEEDLIM,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$join_SPEEDLIM and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.003998
    ## alternative hypothesis: two.sided

``` r
#shared road - no association
fisher.test(joined_lts_crash_filtered$join_MP_SHARDRD,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$join_MP_SHARDRD and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.8851
    ## alternative hypothesis: two.sided

``` r
#average slope - no association
fisher.test(joined_lts_crash_filtered$join_AVG_SLOPE,joined_lts_crash_filtered$`Injury Severity`,simulate.p.value = T)
```

    ## 
    ##  Fisher's Exact Test for Count Data with simulated p-value (based on
    ##  2000 replicates)
    ## 
    ## data:  joined_lts_crash_filtered$join_AVG_SLOPE and joined_lts_crash_filtered$`Injury Severity`
    ## p-value = 0.8646
    ## alternative hypothesis: two.sided

``` r
# to get a confidence interval we need to reduce the result (injury) to two levels: injury or no injury
simplified_injuries <- joined_lts_crash_filtered %>%
  mutate(injury = case_when(
    `Injury Severity` == "NO APPARENT INJURY" ~ "No injury",
    `Injury Severity` == "POSSIBLE INJURY" ~ "Injury",
    `Injury Severity` == "SUSPECTED MINOR INJURY" ~ "Injury",
    `Injury Severity` == "SUSPECTED SERIOUS INJURY" ~ "Injury",
    `Injury Severity` == "FATAL INJURY" ~ "Injury")) %>%
  select(join_LTS_REV,injury) %>%
  table()

simplified_injuries
```

    ##             injury
    ## join_LTS_REV Injury No injury
    ##          0        6         3
    ##          1      158        26
    ##          2       89        14
    ##          2.5     45         3
    ##          3       84        13
    ##          4      178        16
    ##          5       26         0
    ##          9        9         2

``` r
prop.test(simplified_injuries)
```

    ## Warning in prop.test(simplified_injuries): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  8-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  simplified_injuries
    ## X-squared = 13.473, df = 7, p-value = 0.06139
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6    prop 7    prop 8 
    ## 0.6666667 0.8586957 0.8640777 0.9375000 0.8659794 0.9175258 1.0000000 0.8181818

``` r
#this also uses chi-sq and still doesn't return a confidence interval
```

Try something different. Bring in accident rates on each street + avg
(mean?) LTS of those streets? Would need to convert LTS to an int;
remove P, TRANS, NA, etc.

(some manual cleaning done on xlsx, e.g. matching “1st St” to “FIRST
ST”)

``` r
LTS_streets <- read_excel("data/LTS.xlsx")

AVG_LTS_Street <- LTS_streets %>%
  mutate(STREET_NAME = paste(STREET_N_1,STREET_TYP)) %>%
  group_by(STREET_NAME) %>%
  mutate(AVG_LTS = mean(as.numeric(LTS_REV),na.omit=T)) %>%
  select(STREET_NAME,AVG_LTS) %>%
  distinct()

LTS_accident_rate_joined <- AVG_LTS_Street %>%
  full_join(accident_rate, by = c("STREET_NAME" = "moco_name")) %>%
  mutate(incidents_per_mile_annual = ifelse(!is.na(incidents_per_mile_annual),incidents_per_mile_annual,0)) #streets with no incidents have a rate of 0


#correlation of average LTS across a street and incident rate across a street. 
cor.test(LTS_accident_rate_joined$AVG_LTS,LTS_accident_rate_joined$incidents_per_mile_annual)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  LTS_accident_rate_joined$AVG_LTS and LTS_accident_rate_joined$incidents_per_mile_annual
    ## t = 1.151, df = 10939, p-value = 0.2497
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.007735134  0.029736765
    ## sample estimates:
    ##        cor 
    ## 0.01100468

``` r
#this shows no correlation, so we can assume that at the broad "road" level there is no correlation between the average road LTS and the incident rate per mile; instead we can posit that accidents happen in clustered hotspots where the LTS is high, even if the average LTS along the whole of the road is lower.
#Additionally, the very large number of roads with zero accidents may mean that this test is not reliable/assumptions are not

LTS_accident_rate_joined %>%
  ggplot(aes(x=AVG_LTS,y=incidents_per_mile_annual)) +
  geom_point(alpha=0.5)
```

![](Bike_Incidents_Lanes_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
