setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture07 - Hierarchical ANOVA")
options(width=60, stringsAsFactors=FALSE)
library(rjags)

df <- data.frame(y=c(28,8,-3,7,-1,1,18,12), sigma=c(15,10,16,11,9,11,10,18),
	row.names=LETTERS[1:8])

# uniform prior on sd
model1 <- "model {
	for (j in 1:J) {
		y[j] ~ dnorm(theta[j], inv.sigma2[j])
		inv.sigma2[j] <- 1 / sigma[j]^2
		theta[j] ~ dnorm(mu, inv.tau2)
	}
	mu ~ dunif(-30,30)
	inv.tau2 <- 1 / tau^2
	tau ~ dunif(0,30)
}"
data <- list(y=df$y, sigma=df$sigma, J=nrow(df))
sampler1 <- jags.model(textConnection(model1), data=data)
update(sampler1, 1000)
samples1 <- jags.samples(sampler1, variable.names=c("mu","tau","theta"),
	n.iter=10000)
hist(samples1$tau, probability=TRUE, xlab=expression(tau), ylab="",
	main="Uniform(0,30) prior on SD", ylim=c(0,.12))
lines(density(samples1$tau), lwd=2)

# inverse gamma(1,1) prior on variance
model2 <- "model {
	for (j in 1:J) {
		y[j] ~ dnorm(theta[j], inv.sigma2[j])
		inv.sigma2[j] <- 1 / sigma[j]^2
		theta[j] ~ dnorm(mu, inv.tau2)
	}
	mu ~ dunif(-30,30)
	inv.tau2 ~ dgamma(1,1)
	tau <- 1 / sqrt(inv.tau2)
}"
sampler2 <- jags.model(textConnection(model2), data=data)
update(sampler2, 1000)
samples2 <- jags.samples(sampler2, variable.names=c("mu","tau","theta"),
	n.iter=10000)
hist(samples2$tau, probability=TRUE, xlab=expression(tau), ylab="",
	main="Inverse Gamma(1,1) prior on variance", ylim=c(0,1))
lines(density(samples2$tau, adjust=1.5), lwd=2)

# inverse gamma(.001, .001) prior on variance
model3 <- "model {
	for (j in 1:J) {
		y[j] ~ dnorm(theta[j], inv.sigma2[j])
		inv.sigma2[j] <- 1 / sigma[j]^2
		theta[j] ~ dnorm(mu, inv.tau2)
	}
	mu ~ dunif(-30,30)
	inv.tau2 ~ dgamma(.001, .001)
	tau <- 1 / sqrt(inv.tau2)
}"
sampler3 <- jags.model(textConnection(model3), data=data)
update(sampler3, 1000)
samples3 <- jags.samples(sampler3, variable.names=c("mu","tau","theta"),
	n.iter=10000)
hist(samples3$tau, probability=TRUE, xlab=expression(tau), ylab="",
	main="Inverse Gamma(.001,.001) prior on variance", ylim=c(0,.12))
lines(density(samples3$tau), lwd=2)

# Half Cauchy(0,25) prior on SD
model4 <- "model {
	for (j in 1:J) {
		y[j] ~ dnorm(theta[j], inv.sigma2[j])
		inv.sigma2[j] <- 1 / sigma[j]^2
		theta[j] ~ dnorm(mu, inv.tau2)
	}
	mu ~ dunif(-30,30)
	inv.tau2 <- 1 / tau^2
	tau ~ dt(0,1/25^2,1)T(0,)
}"
sampler4 <- jags.model(textConnection(model4), data=data)
update(sampler4, 1000)
samples4 <- jags.samples(sampler4, variable.names=c("mu","tau","theta"),
	n.iter=10000)
hist(samples4$tau, probability=TRUE, xlab=expression(tau), ylab="",
	main="Half Cauchy(0,25) prior on SD", ylim=c(0,.12), xlim=c(0,30))
lines(density(samples4$tau), lwd=2)

library(rstan)
options(width=80)

# uniform(0,30) prior on SD
model1 <- "data {
	int<lower=0> J;
	real y[J];
	real<lower=0> sigma[J];
}
parameters {
	real mu;
	real<lower=0> tau;
	real theta[J];
}
model {
	theta ~ normal(mu, tau);
	y ~ normal(theta, sigma);
	tau ~ uniform(0,30);
}"
fit1 <- stan(model_code=model1, data=data, iter=1000, chains=4)
parms1 <- extract(fit1, permuted=TRUE)
plot(density(parms1$tau), lwd=2)

# scaled inverse chi-square(1,1) prior on variance
model2 <- "data {
	int<lower=0> J;
	real y[J];
	real<lower=0> sigma[J];
}
parameters {
	real mu;
	real<lower=0> tau2;
	real theta[J];
}
transformed parameters {
	real tau;
	tau <- sqrt(tau2);
}
model {
	theta ~ normal(mu, tau);
	y ~ normal(theta, sigma);
	tau2 ~ scaled_inv_chi_square(1, 1);
}"
fit2 <- stan(model_code=model2, data=data, iter=1000, chains=4)
parms2 <- extract(fit2, permuted=TRUE)
lines(density(parms2$tau), lwd=2, col="red")

# half cauchy (0, 25) prior on SD
model3 <- "data {
	int<lower=0> J;
	real y[J];
	real<lower=0> sigma[J];
}
parameters {
	real mu;
	real<lower=0> tau;
	real theta[J];
}
model {
	theta ~ normal(mu, tau);
	y ~ normal(theta, sigma);
	tau ~ cauchy(0, 25);
}"
fit3 <- stan(model_code=model3, data=data, iter=1000, chains=4)
parms3 <- extract(fit3, permuted=TRUE)
lines(density(parms3$tau), lwd=2, col="green")