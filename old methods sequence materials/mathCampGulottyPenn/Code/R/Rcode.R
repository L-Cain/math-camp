### Read in THOR dataset----

library(tidyverse)
library(stringr)
library(lubridate)
dta<-read.csv("~/Dropbox/mathCamp/Code/data/THOR_WW1.csv")

head(dta)


dta<-dta%>%mutate(date=mdy(str_extract(MSNDate,"\\d[^\\s]+\\d")), monthyear=year(date)+month(date)/12)%>%
 group_by(monthyear)%>%
summarise(
mlat = sum(as.numeric(Lat) * as.numeric(WeaponWeight)/sum(as.numeric(WeaponWeight),na.rm=T), na.rm=T) / length(as.numeric(Lat)), 
mlon = sum(as.numeric(Lon) * as.numeric(WeaponWeight)/sum(as.numeric(WeaponWeight),na.rm=T), na.rm=T) / length(as.numeric(Lon)))


