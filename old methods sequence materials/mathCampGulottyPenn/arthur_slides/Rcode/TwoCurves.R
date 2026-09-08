library(ggplot2)

x<-seq(-3,3, by=.1)

y<-x^2

plot(x,y, type="l")

abline(a=-1,b=2)


df<-data.frame(x=x, y=y)


ggplot(df, aes(x, y))+
geom_line()+
geom_abline(intercept=-1, slope=2)+
theme_classic()


