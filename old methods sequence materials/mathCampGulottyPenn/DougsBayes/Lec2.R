setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture02 - Binomial Processes")

# example 1
pdf("lec02-fig01.pdf", width=6, height=5)
curve(dunif(x, min=0, max=1), from=0, to=1, xlim=c(0,1), ylim=c(0,2),
	bty="n", xlab=expression(theta), ylab="", main="", col="grey", lwd=3)
curve(dbinom(y, size=n, prob=x)/beta(y,n-y), from=0, to=1, add=TRUE,
	lwd=3, col="black")
dev.off()

# jags example for problem set 1
library(rjags)
model <- "model {
	y ~ dbin(theta, n)
	theta ~ dunif(0, 1)
}"
y <- 4
n <- 5
data <- list(y=y, n=n)
sampler <- jags.model(textConnection(model), data=data)
samples <- jags.samples(sampler, variable.names=c("theta"),
	n.iter=5000)