setwd("~/Dropbox/mathCamp/Code/data/")

library(dplyr)
library(ggplot2)

guncrime<-read.csv("Gun_crimheatmap.csv")

guncrime<-guncrime%>%filter(Latitude>40)

y<-c(41.785914, 41.802264, 41.802264, 41.785914)
x<-c(-87.605941, -87.606429, -87.580272,-87.580272)

hp<-data.frame(x, y)

ggplot(guncrime, aes(Longitude, Latitude))+
  theme_bw()+
  geom_polygon(data=hp, aes(x,y), fill="maroon", alpha=.3)+
  geom_point(size=.2, alpha=.5)


