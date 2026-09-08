library(ggplot2)
PQ<-function(x){.1*x^2+.1}
PR<-function(x){.2*x}

x0<-1
h<-1.5

#Adjustments for text labels
xadj<-.1
yadj<-.05

ggplot(data.frame(x=c(0,4)), aes(x=x))+

stat_function(fun=PQ, xlim=c(.1,4))+
stat_function(fun=PR, xlim=c(.1,4))+

geom_segment(
		x = x0, 
		y = -1, 
		yend = PR(x0), 
		xend = x0, 
		lty = 3
		)+
		
geom_segment(
		x = x0+h, 
		y = -1, 
		yend = PQ(x0+h), 
		xend = x0+h,
		lty = 3
		)+
		
geom_segment(
		x = x0, 
		y = PR(x0), 
		yend = PR(x0), 
		xend = x0+h, 
		lty = 3
		)+

annotate("text", 
		x = x0, 
		y = PR(x0)+yadj, 
		label = "P"
		)+
		
annotate("text", 
		x = x0+h, 
		y = PQ(x0+h)+yadj, 
		label = "Q")+
		
annotate("text", 
		x = x0+h+xadj, 
		y = PR(x0+h), 
		label = "R")+
		
annotate("text", 
		x = x0+h+xadj, 
		y = PR(x0), 
		label = "N")+

annotate("point", x=x0, y=PR(x0))+
annotate("point", x=x0+h, y=PQ(x0+h))+
annotate("point", x=x0+h, y=PR(x0+h))+
annotate("point", x=x0+h, y=PR(x0))+


annotate("text", x=(x0+x0+h)/2, y = PQ((x0+x0+h)/2)+.1, label="y=f(x)")+

annotate("text", x=(x0+x0+h)/(1.7), y = PR((x0+x0+h)/1.7)-.05, label="T")+

#Totally optional bits:

theme_classic()+
scale_x_continuous(
	expand = c(0, 0), 
	limits = c(0, NA), 
	breaks=c(x0, x0+h), 
	labels=c(expression(x[0]), expression(x[0]+h))
	) +
	 
scale_y_continuous(
	expand = c(0, 0), 
	limits = c(0, .8), 
	breaks=NULL
	) +
	
theme( axis.ticks.y = element_blank(), axis.ticks.x = element_blank())
