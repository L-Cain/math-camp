

# R demonstration of normal distribution and sample sizes.

sampler<-function(n, dist){

samples <- rnorm(n, mean=3, 12)
pdraw <- abs(mean(samples)-dist)
return(pdraw)

}


out<-c()

for(i in 1:100){# Test each sample size from 1 to 100

sampledistance<-mean(replicate(2000, sampler(i, 3))
out[i]<- sampledistance<3

}

which(out>=.95)


sampler2<-function(n){

samples <- rnorm(n, mean=3, 12)

samplemean<- mean(samples)

samplevariance<-sum((samples-samplemean)^2)/(n-1)
return(samplevariance)
}


mean(replicate(10000, sampler2(20)*(19)/(12^2))>30)

1-pchisq(30, 19)

