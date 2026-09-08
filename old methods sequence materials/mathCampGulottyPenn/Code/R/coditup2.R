
#	1-1/2+1/3+ ...+(-1)^(n+1)/n


seq_n<-function(n){
	
	a<-1:n
	sum((-1)^(a+1)/a)
	}

library(purrr)

x<-1:100

y<-map_dbl(x, seq_n)


plot(x,y)
abline(h=log(2))


#taylor series for log(1+x)

log(1+x)=log(1+x)-x^2


x<-matrix(c(3,2,1,0, 4,1,3,0,3,1,0,2,4,0,0,4), nrow=4)
rank(x)
?solve
x<-matrix(c(4,2,0,0,0,0,0,3,-12,0,5,2,1,-2,0,1,0,0,3,5,0,1,-1,-1,-1), nrow=5, byrow=TRUE)
solve(x)%*%c(4,14,0,-1,1)

4*-39+2*80-4