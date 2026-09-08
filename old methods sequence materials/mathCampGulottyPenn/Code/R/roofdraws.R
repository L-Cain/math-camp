library(ggplot2)

# Part 1: 

y<-seq(0,1,.01)

z<- outer(x,y, function(x,y){x + y})
pdf("~/Dropbox/mathCamp/BobbyLectures/5-CovarianceCorrelation/roof.pdf")


persp(x, y, z)

dev.off()


pdfx<-function(x){x+1/2}

cdfx<-function(x){x^2/2+x/2}

#Simulate the marginal with the inverse cdf:
udrawx<-runif(10000)

invcdfx<- function(y){1/2*(sqrt(8*y+1)-1)}

xlist<-invcdfx(udrawx)

#Simulate the conditional with the inverse cdf:


plot(density(qnorm(runif(10000))))
lines(density(rnorm(10000)), lty=2)


udrawy<-runif(10000)

invcdfygivenx<-function(xdraws, udrawys){sqrt(xdraws^2+2*xdraws*udrawys+udrawys)-xdraws}

ydraws<-invcdfygivenx(xlist, udrawy)


cov(xlist, ydraws)

summary(lm(ydraws~xlist))

pdf("~/Dropbox/mathCamp/BobbyLectures/5-CovarianceCorrelation/ConditionalExp.pdf")
plot(xlist, ydraws, cex=.1,pch=10)
lines(x, (2+3*x)/(3+6*x), lty=2)
abline(lm(ydraws~xlist),lty=3)
abline(a=84/132, b=-1/11)
dev.off()

pdf("~/Dropbox/mathCamp/BobbyLectures/5-CovarianceCorrelation/ConditionalExp1.pdf")
plot(xlist, ydraws, cex=.1,pch=10)
lines(xlist, (2+3*xlist)/(3+6*xlist), lty=2, lwd=4)
dev.off()


pdf("~/Dropbox/mathCamp/BobbyLectures/5-CovarianceCorrelation/ConditionalExpZoom.pdf")
plot(xlist, ydraws, cex=.1,pch=10, ylim=c(.5,.7))
lines(x, (2+3*x)/(3+6*x), lty=2)
abline(a=84/132, b=-1/11)
abline(lm(ydraws~xlist),lty=3)
dev.off()


df<-data.frame(x=xlist, y=ydraws)

ggplot(df, aes(x, y))+
geom_point(alpha=.2, size=.2)+
geom_abline(intercept=84/132, slope=-1/11)+
geom_line(aes(x, (2+3*x)/(3+6*x)), lty=2)+
geom_smooth(method="lm", se=F,lty=4)+
coord_cartesian(ylim=c(.5,.75))
