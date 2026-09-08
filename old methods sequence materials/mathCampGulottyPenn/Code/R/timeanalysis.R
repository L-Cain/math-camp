library(tidyverse)
library(lubridate)
df1<-read_csv("~/Dropbox/mathCamp/Code/data/Viewingdata.csv")

names(df1)<-c("file", "views", "minutesdelivered","averagemin", "percentcomp", "lastview", "lastposition")

meantime<-parse_date_time("10/04/20 3:56", "mdy HM", tz = "America/Chicago")

df1$time<-parse_date_time(df1$lastview, "mdy HM", tz = "America/Chicago")
?parse_date_time
head(df1)
ggplot(df1, aes(hour(time)))+geom_bar()+facet_grid(~time>meantime)
