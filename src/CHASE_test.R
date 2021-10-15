require(tidyverse)
source('src/CHASE.R')
source('src/CHASE_functions.R')

#
file <- "./input/assessmentdata_holas_ii.csv"
df <- read.table(file,sep=";",header=T)

file <- "./input/assessmentdata_holas_ii_L4.csv"
df4 <- read.table(file,sep=";",header=T)

CHASE<-Assessment(df4,4) 

df$ConfAcc <- NULL

CHASE<-Assessment(df)

assessmentdata <- df

i<-1
dfout <- CHASE[[i]]