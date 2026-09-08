setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture11 - More on Regression")
options(width=80, stringsAsFactors=FALSE)
library(rjags)
library(pscl)			# for absentee data
data(absentee)

# plot dem margin in machine votes vs. absentee ballots
absentee$absmrg <- with(absentee, 
	100 * (absdem - absrep) / (absdem + absrep))
absentee$machmrg <- with(absentee, 
	100 * (machdem - machrep) / (machdem + machrep))
absentee$disputed <- absentee$year == 93
with(absentee, plot(x=machmrg, y=absmrg, main="", bty="n",
	xlim=c(-40,100), xlab="Democratic margin, machine votes (%)",
	ylim=c(-40,100), ylab="Democratic margin, absentee ballots (%)",
	pch=19, col=ifelse(disputed, "red", "blue")))
with(subset(absentee, disputed), text(x=machmrg, y=absmrg,
	pos=3, label="Disputed\nelection"))

# least squares
fit <- lm(absmrg ~ machmrg, data=absentee, subset=!disputed)
summary(fit)
abline(fit, lwd=2, col="grey")

# bayesian regression
library(rjags)
data <- with(subset(absentee, !disputed), list(x=machmrg, y=absmrg))
data$n <- length(data$x); data$xstar <- absentee$machmrg[absentee$disputed]
model <- "model{
	for(i in 1:n){
	      mu[i] <- beta[1] + beta[2] * x[i]
	      y[i] ~ dnorm(mu[i], sigma2.inv)
	}
	beta[1] ~ dnorm(0,.001)
	beta[2] ~ dnorm(1,.001)
	sigma ~ dt(0, .001, 1)T(0,)
	sigma2.inv <- 1 / sigma^2
	mustar <- beta[1] + beta[2] * xstar
	ystar ~ dnorm(mustar, sigma2.inv)
}"
sampler <- jags.model(textConnection(model), data=data)
update(sampler, 1000)
samples <- jags.samples(sampler, n.iter=10000,
	variable.names=c("beta", "sigma", "ystar"))
summary(samples$beta, mean)
abline(summary(samples$beta, mean)$stat)
# posterior p-value
prop.table(table(samples$ystar > absentee$absmrg[absentee$disputed]))

# Ridge example from ISLR
library(ISLR)
data(Hitters)
Hitters <- na.omit(Hitters)
ols.fit <- lm(Salary ~ ., data=Hitters)
summary(ols.fit)
set.seed(1)
x <- model.matrix(ols.fit)[,-1]
y <- Hitters$Salary
train <- sample(nrow(x), size=nrow(x)/2)
x.train <- x[train,]; y.train <- y[train]
x.test <- x[-train,]; y.test <- y[-train]
ols.train <- lm(Salary ~ ., data=Hitters[train,])
ols.pred <- predict(ols.train, newdata=Hitters[-train,])
mean((ols.pred - Hitters$Salary[-train])^2)
library(glmnet)
grid <- 10^seq(10, -2, length=100)
set.seed(1)
ridge.cv <- cv.glmnet(x.train, y.train, alpha=0)
plot(ridge.cv)
best.lambda <- ridge.cv$lambda.min
ridge.fit <- glmnet(x.train, y.train, alpha=0, lambda=best.lambda)
ridge.pred <- predict(ridge.fit, s=best.lambda, newx=x.test)
mean((ridge.pred - y.test)^2)
ridge.final <- glmnet(x, y, alpha=0)
predict(ridge.final, type="coefficients", s=best.lambda)[1:20,]

# Bayesian model selection
data <- list(X=x, y=y, n=nrow(x), k=ncol(x))
model <- "model {
	for (i in 1:n) {
		y[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- alpha + sum(X[i,1:k] * beta[1:k])
	}
	alpha ~ dnorm(0, .001)
	for (i in 1:k) {
		beta[i] ~ dnorm(0, .001)
		}
	sigma2.inv <- 1 / sigma^2
	sigma ~ dt(0, .001, 1)T(0,)
}"
sampler <- jags.model(textConnection(model), data=data)
update(sampler, 1000)
samples <- jags.samples(sampler, n.iter=10000, 
	variable.names=c("alpha", "beta", "sigma"))
model <- "model {
	for (i in 1:n) {
		y[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- alpha + sum(X[i,1:k] * beta[1:k] * z[1:k])
	}
	alpha ~ dnorm(0, .001)
	for (i in 1:k) {
		beta[i] ~ dnorm(0, .001)
		z[i] ~ dbern(0.5)
		}
	sigma2.inv <- 1 / sigma^2
	sigma ~ dt(0, .001, 1)T(0,)
}"
sampler <- jags.model(textConnection(model), data=data)
update(sampler, 500)
samples <- jags.samples(sampler, n.iter=1000, 
	variable.names=c("alpha", "beta", "sigma", "z"))



# Communities and Crime Unnormalized Data Set
# http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized
file <- paste0("http://archive.ics.uci.edu/ml/machine-learning-databases/",
	"00211/CommViolPredUnnormalizedData.txt")
crime <- read.csv(file, header=FALSE, na.strings="?")
names(crime) <- read.table("murdervars.txt")$V1
save(crime, file="crime.Rdata")
library(glmnet)
crime <- crime[,which(sapply(crime, function(x) !any(is.na(x))))]
y <- matrix(crime$murdPerPop)
x <- scale(as.matrix(crime[,6:104]))
fit <- cv.lars(x=x, y=y, K=10)

predictors <- as.matrix(crime[,8:130])
grid <- 10^seq(10, -2, length=10)
ridge.fit <- glmnet(x, y, alpha=0, lambda=grid)
fit <- cv.lars(x=predictors, y=crime$murder, K=10)