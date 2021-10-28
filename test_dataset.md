CHASE description of test dataset
================
NIVA Denmark
28/10/2021

## Data sources

For testing purposes, we used indicator data from the HOLAS II
assessment. This data was available with indicators per station. The
indicator data came from the file **CHASEinput060318.xlsx** sheet **By
station (for BSII only)**.

Using shape files for level 3 assessment units and level 4 assessment
units, we mapped the station positions to assessment units, creating two
input data sets for the CHASE assessment:

1.  [assessmentdata_holas_ii.csv](input/assessmentdata_holas_ii.csv)

2.  [assessmentdata_holas_ii_L4.csv](input/assessmentdata_holas_ii_L4.csv)

### Shape files

The shapes files used were downloaded from the HELCOM maps and data
service (MADS):

Level 3:
<https://maps.helcom.fi/website/MADS/download/?id=e5a59af9-c244-4069-9752-be3acc5dabed>

Level 4:
<https://maps.helcom.fi/website/MADS/download/?id=67d653b1-aad1-4af4-920e-0683af3c4a48>

The original shape files contain several columns, including polygon
area, etc. For the purpose of these tests, we are only interested in the
names of the assessment units. They were therefore modified using the
following code, to reduce their size.

``` r
library(tidyverse)
library(sf)

units3 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_and_offshore_division_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_and_offshore_division_2018_1")

units3 <- units3 %>%
  dplyr::select(HELCOM_ID,level_3)

st_write(units3, "assessment_units/AssessmentUnits3.shp",append=F)

units4 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_1")

units4 <- units4 %>%
  dplyr::select(Name,HELCOM_ID) %>%
  filter(!is.na(HELCOM_ID))

st_write(units4, "assessment_units/AssessmentUnits4.shp",append=F)
```

## Mapping

Load packages

``` r
library(sf)
library(tidyverse)
library(patchwork)
```

Load shape files for assessment units

``` r
units3 <- read_sf(dsn = "./assessment_units", 
                  layer = "AssessmentUnits3")

units4 <- read_sf(dsn = "./assessment_units", 
                  layer = "AssessmentUnits4")

# show maps of the assessment units
map3 <- ggplot() +
  theme_minimal(base_size=9) +
  ggtitle("Level 3 Assessment Units") +
  geom_sf(data=units3, colour="black", fill=NA)
map4 <- ggplot() +
  theme_minimal(base_size=9) + 
  ggtitle("Level 4 Assessment Units") +
  geom_sf(data=units4, colour="black",  fill=NA)

map3+map4
```

![](test_dataset_files/figure-gfm/read%20shape%20files-1.png)<!-- -->

Read indicator data from text file
[holas_ii_indicators_by_station.txt](input/holas_ii_indicators_by_station.txt)

``` r
file <- "input/holas_ii_indicators_by_station.txt"

df <- read.table(file,sep="\t",
                 header=T,
                 fileEncoding="UTF-8",
                 comment.char="")
```

Convert indicator data frame to simple features

``` r
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326) # EPSG 4326 is WGS84

# transform the sf dataframe to the same projection as the assessment units
df_sf <- st_transform(df_sf,crs=st_crs(units3))

# plot the stations and the assessment units together
ggplot() +
  theme_minimal(base_size=9) + 
  ggtitle("Level 4 Assessment Units \n+ Indicator positions") +
  geom_sf(data=units4, colour="black",  fill=NA) +
  geom_sf(data=df_sf, colour="red")
```

![](test_dataset_files/figure-gfm/indicators%20to%20sf-1.png)<!-- -->

Intersect the indicator data points with the polygons to add information
about the assessment units

``` r
 df3 <- st_intersection(df_sf, units3)
 df4 <- st_intersection(df_sf, units4)
 
 # geometry information is no longer needed
 df3$geometry <-NULL
 df4$geometry <-NULL

 # show the head for Level 3 data
 head(df3)
```

    ##           region country             station         stationName determinand
    ## 892 Bothnian Bay Finland            Hailuoto            Hailuoto        SBD6
    ## 893 Bothnian Bay Finland            Hailuoto            Hailuoto        SCB6
    ## 894 Bothnian Bay Finland            Hailuoto            Hailuoto          HG
    ## 901 Bothnian Bay Finland         Iso-Huituri         Iso-Huituri          HG
    ## 909 Bothnian Bay Finland Kalajokisuun edusta Kalajokisuun edusta          HG
    ## 910 Bothnian Bay Finland Kalajokisuun edusta Kalajokisuun edusta        HBCD
    ##            detGroup      meanLY      HQS Contamination.ratio HELCOM_ID
    ## 892 Organo-bromines   1.8604651   0.0085        2.188782e+02         1
    ## 893 Chlorobiphenyls  10.1162791  75.0000        1.348837e-01         1
    ## 894          Metals  85.7232129  20.0000        4.286161e+00         1
    ## 901          Metals 239.1652149  20.0000        1.195826e+01         1
    ## 909          Metals 120.0000000  20.0000        6.000000e+00         1
    ## 910 Organo-bromines   0.6088708 167.0000        3.645933e-03         1
    ##                                 level_3
    ## 892 Bothnian Bay Finnish Coastal waters
    ## 893 Bothnian Bay Finnish Coastal waters
    ## 894 Bothnian Bay Finnish Coastal waters
    ## 901 Bothnian Bay Finnish Coastal waters
    ## 909 Bothnian Bay Finnish Coastal waters
    ## 910 Bothnian Bay Finnish Coastal waters

Save the indicator data, now including information on which assessment
units they belong to

``` r
write.table(df3,file="./input/assessmentdata_L3.csv",sep=";",row.names=F,col.names=T,quote=T)
write.table(df4,file="./input/assessmentdata_L4.csv",sep=";",row.names=F,col.names=T,quote=T)
```
