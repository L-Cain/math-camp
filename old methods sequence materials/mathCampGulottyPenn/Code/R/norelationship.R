
library(tidyverse)
library(lubridate)


covid<-read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ggplot(covid, aes(date, cases, color=state))+geom_point()

polls<-read_csv("~/Downloads/polls/president_polls.csv")

pollsTrump<-polls%>%filter(candidate_name=="Donald Trump" )%>%
select(state, sample_size, end_date, pct)%>%
mutate(date=mdy(end_date))

pollsTrump<-pollsTrump%>%group_by(state, date)%>%summarise(meansupp=mean(pct, na.rm=T))

pollsCovid<-pollsTrump%>%left_join(covid)

ggplot(pollsCovid, aes(log(cases+1), meansupp))+geom_point()+geom_smooth()

coviddiff<-pollsCovid%>%group_by(state)%>%
mutate(diffcases=c(0,diff(cases)), 
diffdeaths=c(0, diff(deaths)),
diffdeaths=ifelse(diffdeaths<0, 0, diffdeaths),
diffsupp=c(0, diff(meansupp))
)

ggplot(coviddiff, aes(log(diffdeaths+1), diffsupp))+geom_point()+geom_smooth()

mod1<-lm(diffsupp~diffdeaths+state, data=coviddiff)
summary(mod1)
