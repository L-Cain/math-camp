
1:6

sample(1:6, size = 4, replace=TRUE)

onedice4x <- replicate( n = 1000, sample(1:6, size = 4, replace=TRUE))

n_ones <- colSums(onedice4x==1)

table(n_ones>0)

plot(cumsum(n_ones>0)/1:1000, cex=.2)

abline(h=.5)


twodice<-outer(1:6, 1:6, paste)

twodice24x <- replicate( n = 1000, sample(twodice, size = 24, replace=TRUE))

n_doubleones <- colSums(twodice24x=="1 1")

table(n_doubleones>0)

plot(cumsum(n_doubleones>0)/1:1000, cex=.2)
abline(h=.5)
