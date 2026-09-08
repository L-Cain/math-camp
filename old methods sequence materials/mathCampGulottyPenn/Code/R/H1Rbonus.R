# Problem Set 1
# R bonus 

library(ggplot2)

f1<-function(x){
	log(x)+4}

f2<-function(x){
	.5*exp((x^2)/2)}

f3<-function(x,y){
	3*x^.5*y^.5
}

ggplot(data.frame(x=c(-5,5)), aes(x=x))+
	stat_function(fun=f1)+
	theme_classic()
ggsave("f1out.pdf")

ggplot(data.frame(x=c(-5,5)), aes(x=x))+
	stat_function(fun=f2)+
	theme_classic()
ggsave("f2out.pdf")


# Strategy 1: 3d plot 
# We need a some starting points
x <- seq(0, 5, length= 30)
y <- x
# Make all combinations of x and y.
z <- outer(x, y, f3) 

persp(x,y,z)

ggsave("f3outv1.pdf")

# Strategy 2: Shading

# Here we use a grid:

x1 <- rep(seq(0, 5, length= 30), times=30)
y1 <- rep(seq(0, 5, length=30), each=30)
z1 <- f3(x1, y1)

ggplot(data.frame(x=x1, y=y1, z=z1))+geom_raster(aes(x, y, fill=z))

ggsave("f3outv2.pdf")

# Strategy 3: Contour
# Similar to strategy 2, 

x1 <- rep(seq(0, 1, length= 30), times=30)
y1 <- rep(seq(0, 1, length= 30), each=30)
z1 <- f3(x1, y1)

ggplot(data.frame(x=x1, y=y1, z=z1))+
stat_contour(aes(x=x, y=y, z=z,col = ..level..), breaks=seq(.1, 5, .4) )

# Note the use of the breaks.

ggsave("f3outv3.pdf")


