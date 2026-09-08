
library(ggplot2)

x<-seq(-1,10, by=.1)

y<-sin(x)+10+.005*x^2-.005*x^3+.0005*x^4

df<-data.frame(x=x, y=y)



ggplot(df, aes(x, y))+geom_line()+theme_classic()+ylim(c(0,12))+geom_point(aes(x=2, y=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4))+annotate("text", x=2, y=.6+sin(2)+10+.005*2^2-.005*2^3+.0005*2^4, label="P")+labs(x="x", y="f(x)")+geom_segment(x=-2, xend=2, y=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4,yend=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4 , linetype=2)+geom_segment(x=2, xend=2, y=-10,yend=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4 , linetype=2)




ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/pointP.png", width=4, height=3)



ggplot(df, aes(x, y))+geom_line()+theme_classic()+ylim(c(0,12))+geom_point(aes(x=2, y=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4))+geom_point(aes(x=3, y=sin(3)+10+.005*3^2-.005*3^3+.0005*3^4))+annotate("text", x=2, y=.6+sin(2)+10+.005*2^2-.005*2^3+.0005*2^4, label="P")+annotate("text",x=3, y=.6+sin(3)+10+.005*3^2-.005*3^3+.0005*3^4, label="Q")+labs(x="x", y="f(x)")+geom_segment(x=-2, xend=2, y=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4,yend=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4 , linetype=2)+geom_segment(x=2, xend=2, y=-10,yend=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4 , linetype=2)+geom_segment(x=-4, xend=3, y=sin(3)+10+.005*3^2-.005*3^3+.0005*3^4,yend=sin(3)+10+.005*3^2-.005*3^3+.0005*3^4 , linetype=2)+geom_segment(x=3, xend=3, y=-10,yend=sin(3)+10+.005*3^2-.005*3^3+.0005*3^4 , linetype=2)+geom_segment(x=2, xend=3, y=sin(2)+10+.005*2^2-.005*2^3+.0005*2^4,yend=sin(3)+10+.005*3^2-.005*3^3+.0005*3^4, linetype=3)




ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/pointPandQ.png", width=4, height=3)

