library(ggplot2)

x<-seq(0,10, by=.01)
dots<- ifelse(x>3, "unknown","known")
yout<-sin(x)


df<-data.frame(x=x, yout= yout, dots=as.factor(dots))

ggplot(df, aes(x, yout))+geom_line(aes(linetype=dots))+geom_point(aes(x=2.5, y=sin(2.5)))+
theme_classic()+
 annotate("text", x=2.5, y=sin(2.5)-.2, label="f(a)")+
 geom_point(aes(x=4, y=sin(4)),color="blue")+
  annotate("text", x=4, y=sin(4)-.2, label="f(b)")+geom_segment(x=2.5, y=sin(2.5), xend=4, yend=sin(2.5)+(4-2.5)*cos(2.5), linetype=3)+geom_point(x=4, y=sin(2.5)+(4-2.5)*cos(2.5))+
  annotate("text", x=4.8, y=sin(4)+.2, label="f(a)+(b-a)f'(a)")+labs(y="f(x)", linetype="")
  
  ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture5/approx.png",width=10, height=3)
  
 
 