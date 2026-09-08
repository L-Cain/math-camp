library(tidyverse)

dta<-read_csv("~/Dropbox/mathCamp/Code/data/VideoStats_final.csv")



df1<-dta%>%select(Session=`Session Name`, UID=`User ID`, Name, Email, completed=`Percent Completed`)

PHDstudents<-c("Olafur Bjornsson","Ari Weil", "Lautaro Cella", "Lingnan He","Megan Maxwell", "Caroline Ferguson", "Maya Nandakumar", "Gabrielle Bozarth", "Audrey Simpson", "Randall Conway")


df2<-df1%>%filter(Name%in%PHDstudents)%>%complete(Session, nesting(Name, Email, UID), fill=list(completed=0))

df2%>% group_by(Name, Email)%>%summarise(mean=mean(completed>0, na.rm=T))%>%arrange(desc(mean))%>%data.frame()


df2%>%filter(Name=="Ezio Wang", completed==0)%>%pull(Session)

df1%>%filter(Name=="Ezio Wang", completed==0)

df2%>%group_by()

df1%>%filter(Session=="L10pt15Inference", Name=="Ezio Wang")