library(tidyverse)
library(lubridate)

views<-read_csv("~/Dropbox/mathCamp/Code/data/Views2.csv")

unique(views$Name)


PHDstudents<-c("Olafur Bjornsson","Ari Weil", "Lautaro Cella", "Lingnan He","Megan Maxwell", "Caroline Ferguson", "Maya Nandakumar", "Gabrielle Bozarth", "Audrey Simpson", "Randall Conway")

views2<-views%>%select(Timestamp,Session=`Session Name`, Name,UID=`User ID`,delivered=`Minutes Delivered`)%>%filter(Name%in%"Zheng He")

views2$datetime<-parse_date_time(views2$Timestamp, "mdyHMS",tz = "America/Chicago")


views2%>%filter(Session=="L13pt5MultivariateNormal")

views2%>%count(Session)%>%arrange(n)

viewscum<-views2%>%arrange(datetime)%>%group_by(Name)%>%mutate(cumdelivered=cumsum(delivered))


ggplot(viewscum, aes(datetime, cumdelivered))+geom_point()+theme_bw()+facet_wrap(~Name)
ggsave("studentlevel.png")

ntile(1:100,20)

?MASS::polr