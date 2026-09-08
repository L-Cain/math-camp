setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture10 - Regression Models")
options(width=80, stringsAsFactors=FALSE)
library(car)		# for recode()
library(arm)		# display functions from Gelman & Hill
library(rjags)

# read data from 1986 and 1988
years <- seq(1986, 1990, 2)
cong <- lapply(years, function(year) {
	file <- paste0("http://www.stat.columbia.edu/~gelman/book/data/incumbency/",
		year, ".asc")
	df <- read.table(file, col.names=c("state","cd","inc","dv","rv"))
	df$vote <- with(df, ifelse(dv == -9 | rv == -9, NA, dv/(dv+rv)))
	df$contested <- with(df, ifelse(is.na(vote), FALSE, vote > .1 &
		vote < .9))
	df })
names(cong) <- years

# last row of exceptions file generates a warning
exceptions <- read.table(file=
	"http://www.stat.columbia.edu/~gelman/book/data/incumbency/excepth.asc",
	col.names=c("year", "state", "cd", "dem", "rep", "minor", "winvote"),
	skip=1)	# junk in header
# eliminate exceptions
for (yr in years) {
	tmp <- cong[[ paste0(yr) ]]
	bad <- subset(exceptions, year == yr, select=c(state, cd))
	bad$flag <- 1
	tmp <- merge(tmp, bad, all.x=TRUE)
	tmp <- subset(tmp, is.na(flag), select=-c(flag))
	cong[[ paste0(yr) ]] <- tmp
	}

# figure 7.3 from Gelman & Hill
v88 <- with(cong[["1988"]], ifelse(vote < .1, .0001,
	ifelse(vote > .9, .9999, vote)))
hist(v88, xlab="Democratic share of the two-party vote", breaks=seq(0,1,.05),
	ylab="", yaxt="n", cex.axis=1.1, cex.lab=1.1, cex.main=1.2,
	main="Congressional elections in 1988")
rm(v88)

# lm estimates from pp. 145-146 of Gelman & Hill
cong88 <- merge(subset(cong[["1988"]], contested), cong[["1986"]],
	by=c("state", "cd"), all.x=TRUE, suffixes=c("88", "86"))
cong88$vote86 <- with(cong88, ifelse(vote86 < .1, .25,
	ifelse(vote86 > .9, .75, vote86)))
fit <- lm(vote88 ~ vote86 + inc88, data=cong88)

# figure 7.4 from Gelman & Hill
par(mfrow=c(1,1), pty="s", mar=c(5,5,4,1)+.1)
plot(NULL, NULL, xlim=c(0,1), ylim=c(0,1), type="n", cex.lab=1,
	xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988")
abline(0, 1, lwd=0.5)
with(cong88, points(ifelse(contested86, jitter(vote86, .02), vote86),
	ifelse(contested88, jitter(vote88, .02), vote), pch=ifelse(inc88 == 0, 1,
	ifelse(inc88 == 1, 16, 4))))
mtext("Raw data (jittered at 0 and 1)", line=1, cex=1.2)

data <- with(cong88, list(vote88=vote88, vote86=vote86, inc88=inc88))
#data$n <- nrow(cong88)
model <- "model {
	for (i in 1:348) {
		vote88[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- b0 + b1 * vote86[i] + b2 * inc88[i]
	}
	sigma2.inv ~ dgamma(.01, .01)
	b0 ~ dnorm(0.5, 25)
	b1 ~ dnorm(0.5, 25)
	b2 ~ dnorm(0, 25)
}"
sampler <- jags.samples(textConnection(model), data=data)

