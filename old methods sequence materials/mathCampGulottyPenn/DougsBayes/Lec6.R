setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture06 - Hierarchical Models")
df <- read.table("http://www.stat.columbia.edu/~gelman/book/data/rats.asc",
	skip=3, header=TRUE)
y <- df$y; n <- df$N; J <- nrow(df)

# Figures 5.2 in BDA3 (using transformed parameters)
logq <- function(a,b) log(a*b) - 2.5 * log(a+b) + sum(lbeta(a+y,b+n-y) - lbeta(a,b))
logodds <- seq(-2.5, -1.0, .01); logprec <- seq(1.4, 3.0, .01)
posterior <- matrix(nrow=length(logodds), ncol=length(logprec))
for (i in 1:length(logodds)) {
	for (j in 1:length(logprec)) {
		a <- exp(logodds[i]+logprec[j]) / (1 + exp(logodds[i]))
		b <- exp(logprec[j]) / (1 + exp(logodds[i]))
		posterior[i,j] <- logq(a,b)
	}
}
posterior <- exp(posterior - max(posterior))
pdf(file="lec06-fig01.pdf", width=5, height=5)
contour(logodds, logprec, posterior, levels=seq(.05,.95,.05), drawlabels=FALSE,
	xlab=expression(log(alpha/beta)), ylab=expression(log(alpha+beta)))
dev.off()

# marginal unnormalized posterior for hyperparameters
logq <- function(a,b) -2.5 * log(a+b) + sum(lbeta(a+y,b+n-y) - lbeta(a,b))

# 1: make a grid G and evaluate q at the grid points
G <- expand.grid(a=seq(0.3,5.4,.1), b=seq(2.9,18.5,.1))
qtilde <- sapply(1:nrow(G), function(i) logq(G$a[i], G$b[i]))
qtilde <- exp(qtilde - max(qtilde))

# 2. normalize q
qtilde <- qtilde / sum(qtilde)

# 3. draw M pairs of hyperparameters
M <- 10000
phi <- G[sample(nrow(G), size=M, replace=TRUE, prob=qtilde),]

# 4. generate M values of (theta1,...,thetaJ)
theta <- as.matrix(do.call(rbind, lapply(1:M, function(i) 
	rbeta(J, shape1=phi$a[i]+y, shape2=phi$b[i]+n-y))))

# Figure 5.4 from BDA3
medians <- apply(theta, 2, median)
lo <- apply(theta, 2, quantile, probs=.025)
hi <- apply(theta, 2, quantile, probs=.975)
means <- jitter(y/n, amount=.01)
pdf(file="lec06-fig02.pdf", width=5, height=5.5)
plot(x=means, y=medians, pch=19, cex=0.8, xlim=c(0,.45), ylim=c(0,.45),
	xlab="Observed rates (y/n)", ylab="95% posterior credible interval", 
	main="")
for (i in 1:length(means)) lines(x=rep(means[i],2), y=c(lo[i], hi[i]), lwd=.8)
abline(0, 1, col="grey")
dev.off()

library(rjags)
model <- "model {
	for (j in 1:J) {
		y[j] ~ dbinom(theta[j], n[j])
		theta[j] ~ dbeta(alpha, beta)
	}
	alpha <- exp(logodds+logprec) / (1 + exp(logodds))
	beta <- exp(logprec) / (1 + exp(logodds))
	logodds ~ dunif(-3, 0)
	logprec ~ dunif(0, 10)
}"
data <- list(y=y, n=n, J=J)
sampler <- jags.model(textConnection(model), data=data)
update(sampler, 1000)
samples <- jags.samples(sampler, variable.names=c("alpha", "beta", "theta"),
	n.iter=10000)
logodds <- with(samples, log(alpha/beta))	
logprec <- with(samples, log(alpha+beta))
pdf(file="lec06-fig03.pdf", width=5, height=5)
plot(x=logodds, y=logprec, pch=19, cex=0.1, xlim=c(-2.5, -1.1),
	ylim=c(1.5,4.5), xlab=expression(log(alpha/beta)),
	ylab=expression(log(alpha+beta)), main="")
dev.off()
means <- jitter(y/n, amount=.01) 
medians <- summary(samples$theta, median)$stat
lo <- summary(samples$theta, quantile, prob=.025)$stat
hi <- summary(samples$theta, quantile, prob=.975)$stat
pdf(file="lec06-fig04.pdf", width=5, height=5.5)
plot(x=means, y=medians, pch=19, cex=0.8, xlim=c(0,.45), ylim=c(0,.45),
	xlab="Observed rates (y/n)", ylab="95% posterior credible interval", 
	main="")
for (i in 1:length(means)) lines(x=rep(means[i],2), y=c(lo[i], hi[i]), lwd=.8)
abline(0, 1, col="grey")
dev.off()




