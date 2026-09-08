setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture01 - Introduction")
options(width=60, digits=4)

N <- 20
y <- c(1,1,1,1,0)
n <- length(y)

# standard analysis
ybar <- mean(y)
se.ybar <- sqrt((1 - n/N) * var(y) / n)
ybar; se.ybar; qt(.975, df=4)
ybar + qt(c(.025,.975), df=n-1) * se.ybar

# standard analysis using R survey package
library(survey)
srswor <- svydesign(ids=~1, fpc=rep(n/N, n), data=data.frame(y=y))
svymean(~y, design=srswor)

# coverage of the standard confidence interval
pdf("lec01-fig01.pdf", width=6, height=6)
m <- 0:n; ybar <- m / n
se.ybar <- sqrt((1 - n/N) * ybar * (1 - ybar) / (n - 1))
coverage <- sapply(0:M, function(M) sum(
	dhyper(m[abs(ybar - M/N) <= qt(.975, df=n-1) * se.ybar], M, N-M, n)))
plot(x=(0:N)/N, y=100*coverage, xlim=c(0,1), ylim=c(0,100),
	xlab=expression(bar(Y)), ylab="Coverage of Nominal 95% C.I.",
	pch=19, main="", bty="n")
lines(x=c(-20,120), y=c(95,95), col="grey")
dev.off()

# bayesian analysis
pdf("lec01-fig02.pdf", width=7, height=5)
Ybar <- (0:N) / N
prior <- dnorm(Ybar, mean=0.7, sd=0.15)
prior <- prior / sum(prior)
likelihood <- dhyper(sum(y), N*Ybar, N*(1-Ybar), n)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
sum(Ybar * posterior)		# posterior mean
data.frame(Ybar=Ybar, cumprob=cumsum(posterior))
sum(posterior[0.50 <= Ybar & Ybar <= 0.90])
likelihood <- likelihood / sum(likelihood)
plot(x=Ybar, y=prior, type="p", pch=19, cex=0.6,
	xlim=c(0,1), xlab=expression(bar(Y)),
	ylim=c(0,.2), ylab="", yaxt="n", bty="n",
	col="grey")
lines(x=Ybar, y=prior, col="grey", cex=0.7)
points(x=Ybar, y=posterior, col="black", cex=0.8, pch=19)
text(x=0.5, y=0.06, label="Prior", col="grey", pos=2)
lines(x=Ybar, y=posterior, col="black")
text(x=0.68, y=0.155, label="Posterior", col="black", pos=2)
points(x=Ybar, y=likelihood, col="indianred", pch=19, cex=0.7)
lines(x=Ybar, y=likelihood, col="indianred")
text(x=0.65, y=0.08, label="Likelihood", col="indianred", pos=4)
dev.off()