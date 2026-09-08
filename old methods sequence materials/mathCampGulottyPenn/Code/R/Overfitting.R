library(car)
library(tidyverse)
data(Prestige)


ggplot(Prestige, aes(income, prestige))+geom_point()

Prestige$pred1<-predict(lm(prestige~income, data=Prestige))
Prestige$pred2<-predict(lm(prestige~income+I(income^2), data=Prestige))

ggplot(Prestige, aes(income, prestige))+
geom_point()+
geom_line(aes(y=pred1), lty=2)+
geom_line(aes(y=pred2), lty=3)+
geom_smooth(span = 0.05, se=F)
