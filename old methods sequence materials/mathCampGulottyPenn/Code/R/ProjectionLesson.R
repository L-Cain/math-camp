install.packages("plotrix")
library(plotrix)



x1<-c(2,2)
x2<-c(3,0)

x1*x2

x1 %*% x2




plot(rbind(x1,x2), xlim=c(-1,4), ylim=c(-4,4))
abline(h=0, lty=2)
abline(v=0, lty=2)

arrows(0,0,2,2, length=.1)
arrows(0,0,3,0, length=.1)


# Calculate the amount of x1 that is in the x2 direction:

compx1x2 <- x1%*%  x2 / sqrt(x2%*%x2)

arrows(x0=0, x1=compx1x2, y0=0, y1=0, lwd=3 ,length=.2)





beta<-(x1%*%x2/x2%*%x2)

xbeta<-(x1%*%x2/x2%*%x2)*x2


summary(ols1<-lm(x1~x2-1))

ols1$coeff

ols1$coeff*x2[1]





# Observe the following two vectors:

x1<-c(2,-1)
x2<-c(2.5,2)



# Plot the two vectors:

plot(rbind(x1,x2), xlim=c(-1,4), ylim=c(-4,4))
abline(h=0, lty=2)
abline(v=0, lty=2)



arrows(0,0,2.5,2, length=.1)
arrows(0,0,2,-1, length=.1)



# Calculate the amount of x1 that is in the x2 direction:

compx1x2<-x1%*%x2/sqrt(x2%*%x2)


# Plot the component in the direction of x2:

arrows(0, 0, cos(atan(x2[2]/x2[1]))*compx1x2,sin(atan(x2[2]/x2[1]))*compx1x2, lwd=3 ,length=0)


# Calculate the projection of x1 onto x2

xbeta<-(x1%*%x2/x2%*%x2)*x2


# Plot the projection of x1 onto x2:

arrows(0, 0, xbeta[1], xbeta[2],length=.1)


# observe the regression of x1 onto x2 (without an intercept).

ols1<-lm(x1~x2-1)


# Compare xbeta with yhat from lm: 

xbeta

predict(ols1)

ols1$coeff[1]*x2


# Note we can recover the length of yhat as well:

ols1$coeff[1]*sqrt(x2%*%x2)


# We can add a line that connects x1 and the point closest to x1 in x2:

theta<-atan(x2[2]/x2[1])


arrows(x1[1],x1[2],cos(theta)*compx1x2,sin(theta)*compx1x2 , length=0, lty=5, lwd=2, col="red")


# That line is identical to the vector produced by the residuals, shifted to start at yhat.


arrows(ols1$coeff[1]*x2[1],ols1$coeff[1]*x2[2],resid(ols1)[1]+xbeta[1],resid(ols1)[2]+xbeta[2], length=0, lty=6, col="blue")






# Example 2: projection onto a vector of ones.


x1<-c(2,4)
x2<-c(1,1)



plot(rbind(x1,x2), xlim=c(-5,5), ylim=c(-5,5))
abline(h=0, lty=2)
abline(v=0, lty=2)
library(plotrix)
arrows(0,0,x1[1],x1[2], length=.1)
arrows(0,0,x2[1],x2[2], length=.1)



ols1<-lm(x1~x2-1)


arrows(0, 0,ols1$coeff[1]*x2[1],ols1$coeff[1]*x2[2],length=.1, lwd=2)

# Each element of xbeta is the mean of y:

ols1$coeff[1]*x2[1]



arrows(ols1$coeff[1]*x2[1],ols1$coeff[1]*x2[2],resid(ols1)[1]+ols1$coeff[1],resid(ols1)[2]+ols1$coeff[1], length=.1, lty=6, col="blue")
