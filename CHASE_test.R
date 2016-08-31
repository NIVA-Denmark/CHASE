rm(list = ls())
source('CHASE.R')
library(ggplot2)

# ========== Specify the file name containing the CHASE assessment data =====================
# required colums:
#         Matrix
#         Substance
#         Threshold
#         Status
#
# optional colums:
#         Response
#

filename<-'./example_assessment.csv'
assessmentdata<-read.csv(filename, header = TRUE,  sep=";")

out<-Assessment(assessmentdata)     #Individual indicator results
QEspr<-Assessment(assessmentdata,2)    # QE Results
QE<-Assessment(assessmentdata,3)    # QE Results
CHASE<-Assessment(assessmentdata,4) # Overall Assessment results



levels<-data_frame(factor(c("High","Good","Moderate","Poor","Bad"),levels=c("High","Good","Moderate","Poor","Bad")),
                   c(0.0,0.5,1,5,10),
                   c(0.5,1,5,10,20))
names(levels)[1] <- 'Status'
names(levels)[2] <- 'ymin'
names(levels)[3] <- 'ymax'
levels$xmin<-0.5
levels$xmax<-0.5+max(as.numeric(QE$Waterbody))


ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
  geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum, shape=Matrix, color=QEStatus, ymin=0)) +
  scale_color_manual(name="Status",values=c("#3399FF", "#66FF66", "#FFFF66","#FF9933","#FF6600" ))+
  xlab('Waterbody')+ylab('Contamination Sum') 
