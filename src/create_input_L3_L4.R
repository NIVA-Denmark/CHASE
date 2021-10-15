# read L3 assessment data
library(tidyverse)

file3 <- "../data HOLAS II/assessmentdata.csv"
df3 <- read.table(file3,sep=";",header=T)
df3 <- df3 %>%
  mutate(CR=as.numeric(str_replace(CR,",",".")))


# WB area km2
filebasins <- "../data HOLAS II/subbasins.txt"
dfb <- read.table(filebasins,sep=",",header=T,fileEncoding="UTF-8") 

# read mapping from L3 to L4
file <- "./input/mapping_L3_L4.csv"
mapping_L3_L4 <- read.table(file,sep=";",header=T)

# ------------------------
dfb1 <- dfb %>%
  select(Code_scale4=HELCOM_ID,Area_km2) 

dfb2 <- dfb %>%
  filter(country=="Opensea") %>%
  select(Waterbody=Name,Area_km2) %>%
  mutate(Waterbody=paste0(str_replace(Waterbody,"Opensea ","")," - open sea")) %>%
  mutate(Waterbody=str_trim(Waterbody))
  
dfb1 <- dfb1 %>%
  left_join(mapping_L3_L4,by="Code_scale4")

dfb1 <- dfb1 %>%
  filter(!is.na(Name_scale3)) %>%
  group_by(Name_scale3) %>%
  summarise(Area_km2=sum(Area_km2,na.rm=T)) %>%
  ungroup() %>%
  rename(Waterbody=Name_scale3)
  
dfb3 <- bind_rows(dfb1,dfb2) 
# ------------------------

df3 <- df3 %>%
  rename(ConfAcc=ConfStatus) %>%
  mutate(ConfTemp = ifelse(tolower(Datatype)=="initial","M","H"))




df4 <- mapping_L3_L4 %>%
  rename(Waterbody=Name_scale3) %>%
  left_join(df3,by="Waterbody") 

df4 <- df4 %>%  
  rename(WaterbodyL3=Waterbody)%>%
  rename(Waterbody=Code_scale4)


# add areas to L3

  

df3 <- df3 %>%
  left_join(dfb3,by="Waterbody") %>%
  mutate(Area_km2 = ifelse(is.na(Area_km2),999,Area_km2))

df3stns <- df3 %>% 
  group_by(Waterbody,matrix,substance,Area_km2) %>%
  summarise(Stations=n()) %>%
  ungroup()

# L3 Grids, GridsAssessed, Stations, Area_km2
df3stns$GridsAssessed <- runif(nrow(df3stns), min=0, max=1)
df3stns <- df3stns %>% 
  mutate(Grids=as.integer(round(Area_km2 / 30,digits=0))) %>%
  mutate(GridsAssessed=as.integer(round(GridsAssessed*Stations,digits=0))) %>%
  mutate(GridsAssessed=ifelse(GridsAssessed<1,1,GridsAssessed))



df3 <- df3 %>%
  left_join(df3stns,by=c("Waterbody","matrix","substance","Area_km2"))


# L4 Grids, GridsAssessed, Stations, Area_km2


df4 <- df4 %>%
  left_join(select(dfb,Waterbody=HELCOM_ID,Area_km2),by="Waterbody") 

df4stns <- df4 %>% 
  group_by(Waterbody,WaterbodyL3,Area_km2,matrix,substance) %>%
  summarise() %>%
  ungroup()

df3stnsL4 <- df3stns %>%
  rename(AreaL3=Area_km2,WaterbodyL3=Waterbody,GridsL3=Grids,GridsAssessedL3=GridsAssessed,StationsL3=Stations)

df4stns <- df4stns %>%
  left_join(df3stnsL4,by=c("WaterbodyL3","matrix","substance")) %>%
  mutate(AreaRatio=Area_km2/AreaL3) %>%
  mutate(Stations=StationsL3*AreaRatio,
         GridsAssessed=GridsAssessedL3*AreaRatio,
         Grids=GridsL3*AreaRatio)


df4stns <- df4stns %>%
  mutate(Stations=as.integer(round(Stations,digits=0))) %>%
  mutate(Grids=as.integer(round(Grids,digits=0))) %>%
  mutate(GridsAssessed=as.integer(round(GridsAssessed,digits=0))) %>%
  mutate(Stations=ifelse(Stations<1,1,Stations)) %>%
  mutate(Grids=ifelse(Grids<1,1,Grids)) %>%
  mutate(GridsAssessed=ifelse(GridsAssessed<1,1,GridsAssessed))

df4stns <- df4stns %>% 
  select(Waterbody,matrix,substance,Grids,GridsAssessed,Stations)

df4 <- df4 %>%
  left_join(df4stns ,by=c("Waterbody","matrix","substance"))


# 
# df4$GridsAssessed <- runif(nrow(df4), min=0, max=0.15)
# df4$Stations <- runif(nrow(df4), min=0, max=1)
# df4 <- df4 %>%
#   mutate(GridsAssessed=as.integer(round(GridsAssessed*Grids,digits=0))) %>%
#   mutate(Stations = as.integer(round(Stations* Area_km2/200,digits=0))) %>%
#   mutate(Stations=ifelse(Stations<1,1,Stations))


# add missing confidence to df3 and df4
# "ConfTemp","ConfSpatial","ConfAcc","ConfMethod","ConfThresh"

#confnames <- c("ConfSpatial","ConfMethod")
confnames <- c("ConfMethod")
conf<-c("L","M","H")

for(i in confnames){
  df3[,i]<-round(runif(nrow(df3), min=0.5, max=3.49999))
  df3[,i] <- conf[df3[,i]]
  df4[,i]<-round(runif(nrow(df4), min=0.5, max=3.49999))
  df4[,i] <- conf[df4[,i]]
}


file3 <- "./input/assessmentdata_holas_ii.csv"
write.table(df3,file=file3,sep=";",row.names=F,col.names=T,quote=T)
file4 <- "./input/assessmentdata_holas_ii_L4.csv"
write.table(df4,file=file4,sep=";",row.names=F,col.names=T,quote=T)

