library(ggplot2)
x<-seq(0,10, by=.001)

f<- log(x)+sin(x)

g<- log(x)+sin(x)-(x-2.5)*((log(5)+sin(5))-(log(2.5)+sin(2.5)))/2.5

mod<- -(x-2.5)*((log(5)+sin(5))-(log(2.5)+sin(2.5)))/2.5
df1<-data.frame(x=x, f=f,g=g, mod=mod)


ggplot(df1, aes(x, f))+geom_line()+
annotate("point", x=2.5, y=log(2.5)+sin(2.5))+
annotate("point", x=5, y=log(5)+sin(5))+
theme_classic()+ylim(c(-5, 6))

ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/fig1.png" , height=4)

ggplot(df1, aes(x, f))+geom_line()+
annotate("point", x=2.5, y=log(2.5)+sin(2.5))+
annotate("point", x=5, y=log(5)+sin(5))+
annotate("point", x=5, y=log(2.5)+sin(2.5), alpha=.5)+
geom_abline(intercept=log(2.5)+sin(2.5), slope=0, lty=4)+
theme_classic()+ylim(c(-5, 6))
ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/fig2.png" , height=4)


ggplot(df1, aes(x, f))+geom_line()+
annotate("point", x=2.5, y=log(2.5)+sin(2.5))+
annotate("point", x=5, y=log(5)+sin(5))+
annotate("point", x=5, y=log(2.5)+sin(2.5), alpha=.5)+
annotate("point", x=2.5, y=log(5)+sin(5), alpha=.5)+
geom_abline(intercept=log(2.5)+sin(2.5), slope=0, lty=4)+
geom_abline(intercept=log(5)+sin(5), slope=0, lty=5)+
geom_line(aes(x,mod+log(5)+sin(5)), lty=3)+
theme_classic()+ylim(c(-5, 6))
ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/fig3.png" , height=4)

ggplot(df1, aes(x, f))+geom_line()+
annotate("point", x=2.5, y=log(2.5)+sin(2.5))+
annotate("point", x=5, y=log(5)+sin(5))+
annotate("point", x=5, y=log(2.5)+sin(2.5), alpha=.5)+

geom_abline(intercept=log(2.5)+sin(2.5), slope=0, lty=4)+
geom_abline(intercept=0, slope=0, lty=5)+
geom_line(aes(x,mod), lty=3)+
theme_classic()+ylim(c(-5, 6))

ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/fig4.png" , height=4)


ggplot(df1, aes(x, f))+geom_line()+
annotate("point", x=2.5, y=log(2.5)+sin(2.5))+
annotate("point", x=5, y=log(5)+sin(5))+
annotate("point", x=5, y=log(2.5)+sin(2.5))+
geom_line(aes(x,g), lty=2)+
geom_abline(intercept=log(2.5)+sin(2.5), slope=0, lty=4)+
geom_abline(intercept=0, slope=0, lty=5)+
geom_line(aes(x,mod), lty=3)+
theme_classic()+ylim(c(-5, 6))

ggsave("~/Dropbox/mathCamp/FinalSlides/Lecture4/fig5.png" , height=4)