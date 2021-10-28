library(tidyverse)
library(sf)

# Shape files downloaded from HELCOM meeting portal
# Level 3: https://maps.helcom.fi/website/MADS/download/?id=e5a59af9-c244-4069-9752-be3acc5dabed
# Level 4: https://maps.helcom.fi/website/MADS/download/?id=67d653b1-aad1-4af4-920e-0683af3c4a48

units3 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_and_offshore_division_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_and_offshore_division_2018_1")

units3 <- units3 %>%
  dplyr::select(HELCOM_ID,level_3,Area_km2=area_km2)

ggplot() + 
  ggtitle("Level 3 Assessment Units") +
  geom_sf(data=units3, colour="black", aes(fill=HELCOM_ID))

st_write(units3, "assessment_units/AssessmentUnits3.shp",append=F)

units4 <- read_sf(dsn = "../gis/_ags_HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_11", 
                  layer = "HELCOM_subbasins_with_coastal_WFD_waterbodies_or_watertypes_2018_1")

units4 <- units4 %>%
  dplyr::select(Name,HELCOM_ID,Area_km2)

ggplot() + 
  ggtitle("Level 4 Assessment Units") +
  geom_sf(data=units4, colour="black", aes(fill=HELCOM_ID))

st_write(units4, "assessment_units/AssessmentUnits4.shp",append=F)
