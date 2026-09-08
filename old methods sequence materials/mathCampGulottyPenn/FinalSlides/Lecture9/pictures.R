library(ggplot2)
library(dplyr)

x<-c(seq(0,1,by=.01),seq(0,1,by=.01))

w<-c(rep(.2,101), rep(.8, 101))

y<-(w*(1-w)/(((1-w)+w*x)^2))


ggplot(data.frame(x=x, y=y, w=w))+geom_point(aes(x,y, color=w))
install.packages("plotly")

example1<-function(x1, x2){
	z= 3*(x1+2)^2+4*(x2+4)^2
	return(z)
}

x <- seq(from=-10, to=20,by=0.5)
y <- seq(from=-20, to=20,by=0.5)

output <- expand.grid(x=x,y=y)

output$z<- example1(x1=output$x, x2=output$y)


pdf("~/Dropbox/mathCamp/FinalSlides/Lecture9/3dplot.pdf")
persp(x, y, matrix(output$z, nrow=length(x)), theta=20)

dev.off()
