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
#         Type
#         ConfThresh
#         ConfStatus


filename<-'./example_assessment.csv'
assessmentdata<-read.csv(filename, header = TRUE,  sep=";")

out<-Assessment(assessmentdata)     #Individual indicator results
QEspr<-Assessment(assessmentdata,2)    # QE Results
QE<-Assessment(assessmentdata,3)    # QE Results
CHASE<-Assessment(assessmentdata,4) # Overall Assessment results

# ----------------- Example plot of test results ------------------------------

QE$Colour<-ifelse(is.na(QE$ConSum),99,
  ifelse(QE$ConSum>0.5,ifelse(QE$ConSum>1,ifelse(QE$ConSum>5,ifelse(QE$ConSum>10,5,4),3),2),1))

QE$Colour<-factor(QE$Colour,levels=c(1,2,3,4,5,99))
QE$Alpha<-ifelse(QE$Colour==99,0,1)

ggplot(data=QE,x=Waterbody,y=ConSum) + theme_bw() +
  geom_point(size=5,data=QE, aes(x=factor(Waterbody), y=ConSum, shape=Matrix, color=Colour, ymin=0)) + 
  scale_color_manual(name="Status",values=c("#33AA00","#99FF66","#FFD5CC","#FF8066","#FF2B00","#FFFFFF"),
                     labels = c("Good","Good","Not good","Not good","Not good",""))+
  xlab('Waterbody')+ylab('Contamination Sum') 
