setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture04 - Normal Variances")

# football point spreads (BDA3, pp. 14-15)
df <- read.table("http://www.stat.columbia.edu/~gelman/book/data/football.asc",
	skip=7, header=TRUE)
y <- with(df, favorite - underdog - spread)
pdf(file="lec04-fig01.pdf", width=6, height=4)
hist(y, xlim=c(-50,50), ylim=c(0,.035), probability=TRUE, breaks=50,
	xlab="Actual outcome - point spread", ylab="", main="", axes=FALSE)
axis(1, at=seq(-50,50,25))
curve(dnorm(x, mean=0, sd=sd(y)), add=TRUE, lwd=2)
dev.off()

mean(y); sd(y); 1/var(y)

# prior and posterior for precision
# prior mean = .005, prior sd = .010
# alpha = (.005/.010)^2 = .25, beta = .005/.010^2 = 50
alpha0 <- .25; beta0 <- 50
curve(dgamma(h, shape=alpha0, rate=beta0), xname="h", from=0, to=.01,
	xlab="Precision", ylab="", ylim=c(0,3000), bty="n", lwd=2,	col="grey", main="")
# posterior: alpha <- alpha + n/2, beta <- beta + S(mu)/2
n <- length(y)
alphan <- alpha0 + n/2; betan <- beta0 + sum(y^2)/2
curve(dgamma(h, shape=alphan, rate=betan), xname="h", add=TRUE,
	col="red", lwd=2)

# prior and posterior for variance
# prior mean: s02 <- 100, prior df: nu0 <- 10
invgamma <- function(x, shape, rate) rate^shape * x^(-shape-1) * exp(-rate/x) /
	gamma(shape)
scaledinvchisq <- function(x, s2, nu) exp((nu/2) * log(nu/2) - lgamma(nu/2) +
	(nu/2) * log(s2) - (nu/2+1) * log(x) - nu * s2 / (2*x))
s20 <- 100; nu0 <- 10
curve(scaledinvchisq(x, s20, nu0), from=0, to=300, col="grey", lwd=2,
	xlab="Variance", ylab="", main="", bty="n", ylim=c(0,.08))
nun <- nu0 + n; s2n <- (nu0 * s20 + sum(y^2)) / nun
curve(scaledinvchisq(x, s2n, nun), col="red", lwd=2, add=TRUE)

