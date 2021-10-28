CHASE test dataset
================

## Development of two indicator datasets for testing the CHASE assessment tool

This document describes the development of datasets for testing the
CHASE assessment tool. By modifying the steps below or by using
different sources of data, alternative datasets can be created for input
to the CHASE assessment tool.

This could also serve as a model for development the actual assessment
as well. However, in order to do this, a number of issues should be
addressed, not limited to:

-   Determination of the temporal confidence should to be included. At
    present random values are used.
-   Determination of the methodological confidence should be included.
    This is also represented by random values at present.

## Data sources

For testing purposes, we used indicator data from the HOLAS II
assessment. This data was available with indicators per station. The
indicator data came from the file **CHASEinput060318.xlsx** sheet **By
station (for BSII only)**.

Using shape files for level 3 assessment units and level 4 assessment
units, we mapped the station positions to assessment units, creating two
input data sets for the CHASE assessment:

1.  [input/assessmentdata_L3.csv](input/assessmentdata_L3.csv)

2.  [input/assessmentdata_L4.csv](input/assessmentdata_L4.csv)

#### Shape files

The shapes files for assessment units were downloaded from the HELCOM
maps and data service (MADS).

Level 3 :
<https://maps.helcom.fi/website/MADS/download/?id=e5a59af9-c244-4069-9752-be3acc5dabed>

Level 4:
<https://maps.helcom.fi/website/MADS/download/?id=67d653b1-aad1-4af4-920e-0683af3c4a48>

The original shape files contain several columns which are not required.
For the purpose of these tests, we are only interested in the names of
the assessment units and area in km<sup>2</sup>. They were therefore
modified using the following code, to reduce their size.

``` r
library(tidyverse)
library(sf)

units3 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_and_offshore_division_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_and_offshore_division_2018_1")

units3 <- units3 %>%
  dplyr::select(HELCOM_ID,level_3,Area_km2=area_km2) 
# note: change name of variable area_km2 for consistency

st_write(units3, "assessment_units/AssessmentUnits3.shp",append=F)

units4 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_1")

units4 <- units4 %>%
  dplyr::select(Name,HELCOM_ID,Area_km2)

st_write(units4, "assessment_units/AssessmentUnits4.shp",append=F)
```

## Mapping

Load packages

``` r
library(sf)
library(tidyverse)
library(patchwork) # not essential - this is used only to combine the two maps of assessment units
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
[input/holas_ii_indicators_by_station.txt](input/holas_ii_indicators_by_station.txt)

``` r
file <- "input/holas_ii_indicators_by_station.txt"

df <- read.table(file,sep="\t",
                 header=T,
                 fileEncoding="UTF-8",
                 comment.char="")
```

Rename columns in indicator data to match CHASE input requirements

``` r
df <- df %>%
  rename(Substance=determinand,
         Type=detGroup,
         Threshold=HQS,
         Status=meanLY,
         CR=Contamination.ratio)
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

Intersect the indicator data points with the polygons to get dataframes
of indicators with added information about which assessment unit each
indicator belongs to. There will be one dataframe for intersection with
Level 3 assessment units and another dataframe for Level 4 assessment
units.

``` r
 df3 <- st_intersection(df_sf, units3)
 df4 <- st_intersection(df_sf, units4)
 
 # geometry information is no longer needed
 df3$geometry <-NULL
 df4$geometry <-NULL

 # show the head for Level 3 data
 head(df3)
```

    ##           region country             station         stationName Substance
    ## 892 Bothnian Bay Finland            Hailuoto            Hailuoto      SBD6
    ## 893 Bothnian Bay Finland            Hailuoto            Hailuoto      SCB6
    ## 894 Bothnian Bay Finland            Hailuoto            Hailuoto        HG
    ## 901 Bothnian Bay Finland         Iso-Huituri         Iso-Huituri        HG
    ## 909 Bothnian Bay Finland Kalajokisuun edusta Kalajokisuun edusta        HG
    ## 910 Bothnian Bay Finland Kalajokisuun edusta Kalajokisuun edusta      HBCD
    ##                Type      Status Threshold           CR Matrix HELCOM_ID
    ## 892 Organo-bromines   1.8604651    0.0085 2.188782e+02  Biota         1
    ## 893 Chlorobiphenyls  10.1162791   75.0000 1.348837e-01  Biota         1
    ## 894          Metals  85.7232129   20.0000 4.286161e+00  Biota         1
    ## 901          Metals 239.1652149   20.0000 1.195826e+01  Biota         1
    ## 909          Metals 120.0000000   20.0000 6.000000e+00  Biota         1
    ## 910 Organo-bromines   0.6088708  167.0000 3.645933e-03  Biota         1
    ##                                 level_3 Area_km2
    ## 892 Bothnian Bay Finnish Coastal waters 5548.123
    ## 893 Bothnian Bay Finnish Coastal waters 5548.123
    ## 894 Bothnian Bay Finnish Coastal waters 5548.123
    ## 901 Bothnian Bay Finnish Coastal waters 5548.123
    ## 909 Bothnian Bay Finnish Coastal waters 5548.123
    ## 910 Bothnian Bay Finnish Coastal waters 5548.123

## Further processing, incl. confidences

#### Spatial confidence

Add counts of stations and observations

``` r
# count stations for Level3
stn_count3 <- df3 %>%
  distinct(HELCOM_ID,level_3,Matrix,Substance,station,stationName) %>%
  group_by(HELCOM_ID,level_3,Matrix,Substance) %>%
  summarise(CountStations=n()) %>%
  ungroup()

# count observations for Level3
data_count3 <- df3 %>%
  group_by(HELCOM_ID,level_3,Matrix,Substance) %>%
  summarise(CountData=n()) %>%
  ungroup()

# merge L3 counts back to original L3 data 

df3 <- df3 %>%
  left_join(stn_count3,by=c("HELCOM_ID","level_3","Matrix","Substance")) %>%
  left_join(data_count3,by=c("HELCOM_ID","level_3","Matrix","Substance"))

# count stations for Level4
stn_count4 <- df4 %>%
  distinct(HELCOM_ID,Name,Matrix,Substance,station,stationName) %>%
  group_by(HELCOM_ID,Name,Matrix,Substance) %>%
  summarise(CountStations=n()) %>%
  ungroup()

# count observations for Level4
data_count4 <- df4 %>%
  group_by(HELCOM_ID,Name,Matrix,Substance) %>%
  summarise(CountData=n()) %>%
  ungroup()

# merge L4 counts back to original L4 data 

df4 <- df4 %>%
  left_join(stn_count4,by=c("HELCOM_ID","Name","Matrix","Substance")) %>%
  left_join(data_count4,by=c("HELCOM_ID","Name","Matrix","Substance"))
```

Add spatial confidence based on km<sup>2</sup> per sample

| km<sup>2</sup> per sample | Confidence |
|---------------------------|------------|
| \<500                     | High       |
| 500 - 5000                | Moderate   |
| \> 5000                   | Low        |

``` r
km2perSampleBounds<-c(500,5000)
conf<-c("L","M","H")

df3 <- df3 %>%
  mutate(km2perSample=Area_km2/CountData)
df3 <- df3 %>%
  rowwise() %>%
  mutate(ix=length(km2perSampleBounds[km2perSampleBounds>km2perSample])) %>%
  mutate(ConfSpatial=conf[ix+1]) %>%
  dplyr::select(-ix)

df4 <- df4 %>%
  mutate(km2perSample=Area_km2/CountData)

df4 <- df4 %>%
  rowwise() %>%
  mutate(ix=length(km2perSampleBounds[km2perSampleBounds>km2perSample])) %>%
  mutate(ConfSpatial=conf[ix+1]) %>%
  dplyr::select(-ix)
```

#### Threshold confidence

Load table of confidences for threshold values from text file
[input/confidence_thresholds.txt](./input/confidence_thresholds.txt)

``` r
dfConfThreshold <- read.table("./input/confidence_thresholds.txt",sep=";",header=T) 

print(dfConfThreshold)
```

    ##        Substance       Matrix ConfThresh                            Comment
    ## 1            ANT     Sediment          H                                   
    ## 2             CD     Sediment          H                                   
    ## 3           HBCD     Sediment          H                                   
    ## 4             PB     Sediment          M                                   
    ## 5           SBD6     Sediment          H                                   
    ## 6          TBTIN     Sediment          H                                   
    ## 7             PB        Biota          M                                   
    ## 8             HG        Biota          H                                   
    ## 9           HBCD        Biota          H                                   
    ## 10           FLU        Biota          H                                   
    ## 11           BAP        Biota          H                                   
    ## 12            CD        Biota          M                                   
    ## 13 Radioactive s        Biota          M                                   
    ## 14          PFOS        Biota          H                                   
    ## 15          SBD6        Biota          H                                   
    ## 16          SCB6        Biota          L                                   
    ## 17           SDX        Biota          H                                   
    ## 18            CD        Water          H                                   
    ## 19 Radioactive s        Water          M                                   
    ## 20            PB        Water          H                                   
    ## 21         TBTIN        Water          H                                   
    ## 22           VDS Bio. Effects          M matrix changed to bio. effects CJM
    ## 23        PYR1OH Bio. Effects          M                       added by CJM
    ## 24          PFOS        Water          H                       added by CJM

Original source for threshold confidence data is Table 2 in this meeting
document:
<https://portal.helcom.fi/meetings/HOLAS%20II%20HZ%20WS%201-2018-518/MeetingDocuments/2-3%20Confidence%20setting%20for%20CHASE%20integrated%20assessment.pdf>

Join threshold confidences to L3 and L4 indicator tables

``` r
df3 <- df3 %>%
  left_join(dfConfThreshold,by=c("Substance","Matrix")) %>%
  mutate(AU_scale=3) 

df4 <- df4 %>%
  left_join(dfConfThreshold,by=c("Substance","Matrix")) %>%
  mutate(AU_scale=4)
```

#### Methodological confidence

Add random confidences for method to L3 and L4 indicator tables

``` r
conf<-c("L","M","H")
df3[,"ConfMethod"]<-round(runif(nrow(df3), min=0.5, max=3.49999))

df3 <- df3 %>%
  rowwise() %>%
  mutate(ConfMethod=conf[ConfMethod]) %>%
  ungroup()

df4[,"ConfMethod"]<-round(runif(nrow(df4), min=0.5, max=3.49999))
df4 <- df4 %>%
  rowwise() %>%
  mutate(ConfMethod=conf[ConfMethod]) %>%
  ungroup()
```

#### Temporal confidence

Add random temporal confidences to L3 and L4 indicator tables

``` r
conf<-c("L","M","H")
df3[,"ConfTemp"]<-round(runif(nrow(df3), min=0.5, max=3.49999))

df3 <- df3 %>%
  rowwise() %>%
  mutate(ConfTemp=conf[ConfTemp]) %>%
  ungroup()

df4[,"ConfTemp"]<-round(runif(nrow(df4), min=0.5, max=3.49999))
df4 <- df4 %>%
  rowwise() %>%
  mutate(ConfTemp=conf[ConfTemp]) %>%
  ungroup()
```

Select only columns needed

``` r
df3 <- df3 %>%
  mutate(AU_scale=3) %>%
  dplyr::select(AU_scale,AU=level_3,Area_km2,Substance,Type,Matrix,Threshold,Status,CR,
                ConfThresh,CountStations,CountData,ConfSpatial,ConfMethod,ConfTemp)


df4 <- df4 %>%
  mutate(AU_scale=4) %>%
  dplyr::select(AU_scale,AU=HELCOM_ID,Area_km2,Substance,Type,Matrix,Threshold,Status,CR,
                ConfThresh,CountStations,CountData,ConfSpatial,ConfMethod,ConfTemp)
```

#### Save test datasets

Save the indicator data, including

-   information on which assessment units they belong to
-   threshold confidence \[H/M/L\]
-   method confidence \[H/M/L\]
-   number of stations in AU
-   number of data points per km<sup>2</sup>
-   spatial confidence \[H/M/L\]
-   temporal confidence \[H/M/L\]

``` r
write.table(df3,file="./input/assessmentdata_L3.csv",sep=";",row.names=F,col.names=T,quote=T)
write.table(df4,file="./input/assessmentdata_L4.csv",sep=";",row.names=F,col.names=T,quote=T)
```

This markdown document is created by knitting the RMarkdown file
[test_dataset.Rmd](test_dataset.Rmd). The steps here for generating test
data can be run without generating markdown, using the standalone R
script [src/create_test_dataset.R](src/create_test_dataset.R)

Updated 28-10-2021 <cjm@niva-dk.dk>
