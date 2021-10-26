require(tidyverse)
source('src/CHASE.R')
source('src/CHASE_functions.R')

#
file <- "./input/assessmentdata_holas_ii.csv"
df <- read.table(file,sep=";",header=T)

file <- "./input/assessmentdata_holas_ii_L4.csv"
df4 <- read.table(file,sep=";",header=T)

CHASE<-Assessment(df) 
CHASE4<-Assessment(df4) 


resL4<-CHASE4[[4]]
resL3<-CHASE[[4]]


p4 <- ggplot(resL4) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  theme_minimal()
p4
p3 <- ggplot(resL3) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  theme_minimal()
p3
ggsave(p4,file="output/hist_L4.png",dpi=300,units="cm",height=15,width=15)
ggsave(p3,file="output/hist_L3.png",dpi=300,units="cm",height=15,width=15)



df$ConfAcc <- NULL

CHASE<-Assessment(df)

assessmentdata <- df

i<-1
dfout <- CHASE[[i]]