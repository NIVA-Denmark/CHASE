CHASE Generate test dataset
================
NIVA Denmark
28/10/2021

## Method

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

The shapes files used were downloaded from the HELCOM meeting portal:
<https://portal.helcom.fi/meetings/CORESET%20II%202-2014%20joint/MeetingDocuments/Forms/Documents.aspx>

The original shape files contain several columns, including GES status,
polygon area, etc. For the purpose of these tests, we are only
interested in the names of the assessment units. They were therefore
modified using the following code, to reduce their size.

``` r
library(tidyverse)
library(sf)

units3 <- read_sf(dsn = "../gis/CORESET/CORESET Assessment maps", 
                  layer = "HELCOM_AssessmentUnits_Scale3")
units3 <- units3 %>%
  dplyr::select(Name,HELCOM_ID)

units4 <- read_sf(dsn = "../gis/CORESET/CORESET Assessment maps", 
                  layer = "HELCOM_AssessmentUnits_Scale4")
units4 <- units4 %>%
  dplyr::select(HELCOM_ID) %>%
  filter(!is.na(HELCOM_ID))

st_write(units3, "assessment_units/AssessmentUnits3.shp",append=F)
st_write(units4, "assessment_units/AssessmentUnits4.shp",append=F)
```

``` r
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

![](test_dataset_files/figure-gfm/pressure-1.png)<!-- -->
