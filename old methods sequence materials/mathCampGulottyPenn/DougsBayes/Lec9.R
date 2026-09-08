setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture09 - Survey Sampling")
options(width=80, stringsAsFactors=FALSE)
library(rjags)

# fake data
set.seed(1234)
sigma <- 1
tau <- 2
N <- 100
M <- 25
n <- 10
m <- 5
mu <- 5
theta <- rnorm(N, mean=mu, sd=tau)
pop <- lapply(1:N, function(i) rnorm(M, mean=theta[i], sd=sigma))
PSU <- sample(1:N, size=n, replace=FALSE)
SSU <- lapply(PSU, function(psu) sample(1:M, size=m, replace=FALSE))
df <- data.frame(psu=rep(PSU, rep(m,n)), ssu=unlist(SSU),
	y=unlist(lapply(1:n, function(i) pop[[ PSU[i] ]][ SSU[[i]] ])))
T.obs <- sapply(PSU, function(i) with(df, sum(y[psu == i])))
	
# estimate model
data <- list(cluster=rep(1:n, rep(m,n)), y=df$y, n=n, L=n*m,
	N=N, N.mis=N-n, M=M, M.mis=M-m, T.obs=T.obs)
model <- "model {
	for (i in 1:L) {
		y[i] ~ dnorm(theta[ cluster[i] ], sigma2.inv)
		}
	for (i in 1:n) {
		theta[i] ~ dnorm(mu, tau2.inv)
		T.mis[i] ~ dnorm((M.mis*theta[i]), (sigma2.inv/M.mis))
		T[i] <- T.mis[i] + T.obs[i]
		}
	for (i in 1:N.mis) {
		theta.tilde[i] ~ dnorm(mu, tau2.inv)
		T.tilde[i] ~ dnorm((M*theta.tilde[i]), (sigma2.inv/M))
		}
	sigma2.inv <- 1 / sigma^2
	tau2.inv <- 1 / tau^2
	mu ~ dnorm(0, .001)
	sigma ~ dt(0, .001, 1)T(0,)
	tau ~ dt(0, .001, 1)T(0,)
	Ybar <- (sum(T[1:n]) + sum(T.tilde[1:N.mis])) / (N*M)
}"
sampler <- jags.model(textConnection(model), data=data)
update(sampler, 1000)
samples <- jags.samples(sampler, n.iter=10000,
	variable.names=c("theta","mu","tau","sigma", "Ybar"))
summary(samples$Ybar, mean)
summary(samples$Ybar, sd)
summary(as.vector(samples$Ybar))
	


	
	