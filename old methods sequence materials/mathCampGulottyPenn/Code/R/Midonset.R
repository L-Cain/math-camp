library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)

dta<-read_csv("~/Dropbox/mathCamp/Code/data/Midonset.csv")

dta<-dta%>%
	mutate(
	day  = ifelse(stday>0, stday, 1), 
	date = ymd( paste( styear, "/", stmon, "/", day )))%>%
	arrange(date)%>%
	mutate(timebetweenwar=c(0,diff(date)))%>%
	arrange(timebetweenwar)
	


ggplot(dta, aes(timebetweenwar, stat(density)))+
geom_histogram(bins=20)+
theme_classic()


lambda<-1/mean(dta$timebetweenwar)

x<-seq(0,900, by=1)
theorytime<-lambda*exp(-lambda*x)
theorycumulative<-1-exp(-lambda*x)
theorydf<-data.frame(x=x, theorytime= theorytime,theorycumulative= theorycumulative)


ggplot(dta, aes(timebetweenwar, stat(density)))+
geom_histogram(bins=60)+
geom_line(data=theorydf,aes(x, theorytime))
theme_classic()


dta<-dta%>%mutate(
	times=)
?cumulative
dta$times

ggplot(dta, aes(timebetweenwar))+stat_ecdf()+
geom_line(data=theorydf, aes(x, theorycumulative))
