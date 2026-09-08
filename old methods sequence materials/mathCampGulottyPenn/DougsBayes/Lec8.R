setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture08 - Model Selection")
options(width=60, stringsAsFactors=FALSE)
library(foreign)		# for read.dta

df <- read.dta("HIBBS-OBAMA-FORECAST-27July2012-WEBPOST.dta")

df$cpi <- df$cpi_u_8284
df$r <- with(df, 100 * dpi_pc / cpi)
lag <- function(x, k=1) c(rep(NA, k), head(x, -k))
df$dlnr <- with(df, 400 * (log(r) - lag(log(r))))
for (k in 1:13) df[,paste0("dlnr", k)] <- lag(df$dlnr, k)
df$dlnp <- with(df, 400 * (log(cpi) - lag(log(cpi))))
df <- subset(df, !is.na(presvote), select=c("year", "presvote",
	"dlnr", paste("dlnr", 1:13, sep=""), "Fatalities", "wtq16"))
fit <- nls(presvote ~ b0 + bdlnr * (wtq16 * dlnr + g * dlnr1
	+ g^2 * dlnr2 + g^3 * dlnr3 + g^4 * dlnr4 + g^5 * dlnr4
	+ g^6 * dlnr6 + g^7 * dlnr7 + g^8 * dlnr8 + g^9 * dlnr9
	+ g^10 * dlnr10 + g^11 * dlnr11 + g^12 * dlnr12 + g^13 * dlnr13
	+ g^14) / (wtq16 + g + g^2 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8
	+ g^9 + g^10 + g ^11 + g^12 + g^13 + g^14) + bkia * Fatalities,
	data=subset(df, !is.na(presvote)), control=list(tol=1e-7,
	minFactor=1e-8, maxiter=500),
	start=list(b0=45, bdlnr=4, g=0.9, bkia=-0.1))

library(rjags)
data <- with(df, list(presvote=presvote, wtq16=0.33, dlnr=dlnr,
	dlnr1=dlnr1, dlnr2=dlnr2, dlnr3=dlnr3, dlnr4=dlnr4, dlnr5=dlnr5,
	dlnr6=dlnr6, dlnr7=dlnr7, dlnr8=dlnr8, dlnr9=dlnr9, dlnr10=dlnr10,
	dlnr11=dlnr11, dlnr12=dlnr12, dlnr13=dlnr13, Fatalities=Fatalities,
	n=nrow(df)))
model1 <- "model {
	for (i in 1:n) {
		presvote[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- b0 + bdlnr * (wtq16 * dlnr[i] + g * dlnr1[i]
			+ g^2 * dlnr2[i] + g^3 * dlnr3[i] + g^4 * dlnr4[i]
			+ g^5 * dlnr5[i] + g^6 * dlnr6[i] + g^7 * dlnr7[i]
			+ g^8 * dlnr8[i] + g^9 * dlnr9[i] + g^10 * dlnr10[i]
			+ g^11 * dlnr11[i] + g^12 * dlnr12[i] + g^13 * dlnr13[i]
			+ g^14) / (wtq16 + g + g^2 + g^3 + g^4 + g^5 + g^6
			+ g^7 + g^8 + g^9 + g^10 + g ^11 + g^12 + g^13 + g^14)
			+ bkia * Fatalities[i]
	}
	b0 ~ dnorm(45, .001)
	bdlnr ~ dnorm(4, .01)
	g ~ dunif(-1, 1)
	bkia ~ dnorm(0, .01)
	sigma2.inv <- 1 / sigma
	sigma ~ dt(0, .01, 1)T(0,)
}"
sampler1 <- jags.model(textConnection(model1), data=data, n.chains=4)
update(sampler1, 1000)
samples1 <- coda.samples(sampler1, variable.names=c("b0", "bdlnr", "g",
	"bkia", "sigma"), n.iter=10000)
summary(samples1)

model2 <- "model {
	for (i in 1:n) {
		presvote[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- b0 + blag0 * dlnr[i] + blag1 * dlnr1[i] + blag2 *
			dlnr2[i] + blag3 * dlnr3[i] + blag4 * dlnr4[i] +
			blag5 * dlnr5[i] + blag6 * dlnr6[i] + blag7 * dlnr7[i]
			+ blag8 * dlnr8[i] + blag9 * dlnr9[i] + blag10 * dlnr10[i]
			+ blag11 * dlnr11[i] + blag12 * dlnr12[i] + blag13 *
			dlnr13[i] + bkia * Fatalities[i]
		}
	sumg <- wtq16 + g + g^2 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9
		+ g^10 + g ^11 + g^12 + g^13 + g^14
	blag0 <- bdlnr * wtq16 / sumg
	blag1 <- bdlnr * g / sumg
	blag2 <- bdlnr * g^2 /sumg
	blag3 <- bdlnr * g^3 / sumg
	blag4 <- bdlnr * g^4 / sumg
	blag5 <- bdlnr * g^5 / sumg
	blag6 <- bdlnr * g^6 / sumg
	blag7 <- bdlnr * g^7 /sumg
	blag8 <- bdlnr * g^8 / sumg
	blag9 <- bdlnr * g^9 / sumg
	blag10 <- bdlnr * g^10 / sumg
	blag11 <- bdlnr * g^10 / sumg
	blag12 <- bdlnr * g^10 / sumg
	blag13 <- bdlnr * g^10 / sumg
	b0 ~ dnorm(45, .001)
	bdlnr ~ dnorm(4, .01)
	g ~ dunif(-1, 1)
	bkia ~ dnorm(0, .01)
	sigma2.inv <- 1 / sigma
	sigma ~ dt(0, .01, 1)T(0,)
}"
sampler2 <- jags.model(textConnection(model2), data=data, n.chains=4)
update(sampler2, 1000)
samples2 <- coda.samples(sampler2, variable.names=c("b0", "bdlnr", "g",
	"blag0", "bkia", "sigma", "blag1", "blag2", "blag3", "blag4", "blag5",
	"blag6", "blag7", "blag8", "blag9", "blag10", "blag11", "blag12",
	"blag13"), n.iter=10000)
summary(samples2)
means2 <- summary(samples2)$statistics[paste0("blag", 0:13, sep=""),"Mean"]
lo2 <- summary(samples2)$quantiles[paste0("blag", 0:13, sep=""),"2.5%"]
hi2 <- summary(samples2)$quantiles[paste0("blag", 0:13, sep=""),"97.5%"]

model3 <- "model {
	for (i in 1:n) {
		presvote[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- b0 + blag0 * dlnr[i] + blag1 * dlnr1[i] + blag2 *
			dlnr2[i] + blag3 * dlnr3[i] + blag4 * dlnr4[i] +
			blag5 * dlnr5[i] + blag6 * dlnr6[i] + blag7 * dlnr7[i]
			+ blag8 * dlnr8[i] + blag9 * dlnr9[i] + blag10 * dlnr10[i]
			+ blag11 * dlnr11[i] + blag12 * dlnr12[i] + blag13 *
			dlnr13[i] + bkia * Fatalities[i]
		}
	sumg <- wtq16 + g + g^2 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9
		+ g^10 + g ^11 + g^12 + g^13 + g^14
	blag0 ~ dnorm((bdlnr * wtq16 / sumg), tau2.inv)
	blag1 ~ dnorm((bdlnr * g / sumg), tau2.inv)
	blag2 ~ dnorm((bdlnr * g^2 /sumg), tau2.inv)
	blag3 ~ dnorm((bdlnr * g^3 /sumg), tau2.inv)
	blag4 ~ dnorm((bdlnr * g^4 /sumg), tau2.inv)
	blag5 ~ dnorm((bdlnr * g^5 /sumg), tau2.inv)
	blag6 ~ dnorm((bdlnr * g^6 /sumg), tau2.inv)
	blag7 ~ dnorm((bdlnr * g^7 /sumg), tau2.inv)
	blag8 ~ dnorm((bdlnr * g^8 /sumg), tau2.inv)
	blag9 ~ dnorm((bdlnr * g^9 /sumg), tau2.inv)
	blag10 ~ dnorm((bdlnr * g^10 /sumg), tau2.inv)
	blag11 ~ dnorm((bdlnr * g^11 /sumg), tau2.inv)
	blag12 ~ dnorm((bdlnr * g^12 /sumg), tau2.inv)
	blag13 ~ dnorm((bdlnr * g^13 /sumg), tau2.inv)
	b0 ~ dnorm(45, .001)
	bdlnr ~ dnorm(4, .01)
	g <- 2 / (1 + exp(-eta)) - 1
	eta ~ dnorm(2, .01)
	bkia ~ dnorm(0, .01)
	sigma2.inv <- 1 / sigma
	sigma ~ dt(0, .01, 1)T(0,)
	tau2.inv <- 1 / tau
	tau ~ dt(0, .001, 1)T(0,)
}"
sampler3 <- jags.model(textConnection(model3), data=data, n.chains=4)
update(sampler3, 1000)
samples3 <- coda.samples(sampler3, variable.names=c("b0", "bdlnr", "g",
	"bkia", "sigma", "blag0", "blag1", "blag2", "blag3", "blag4", "blag5",
	"blag6", "blag7", "blag8", "blag9", "blag10", "blag11", "blag12",
	"blag13"), n.iter=10000)
summary(samples3)
means3 <- summary(samples3)$statistics[paste0("blag", 0:13, sep=""),"Mean"]
lo3 <- summary(samples3)$quantiles[paste0("blag", 0:13, sep=""),"2.5%"]
hi3 <- summary(samples3)$quantiles[paste0("blag", 0:13, sep=""),"97.5%"]

plot(x=NULL, y=NULL, xlab="lag", ylab="Estimated Effect", main="",
	xlim=c(0,13), ylim=c(-0.5,1), bty="n")
polygon(x=c(0:13,13:0), y=c(lo3, rev(hi3)), col="lightgrey", border=NA)
points(x=0:13, y=means3, pch=19, cex=1.5)
lines(x=0:13, y=means3, lwd=2)
points(x=0:13, y=means2, pch=19, col="red", cex=2)
lines(x=0:13, y=means2, lwd=2, col="red")

model4 <- "model {
	for (i in 1:n) {
		presvote[i] ~ dnorm(mu[i], sigma2.inv)
		mu[i] <- b0 + blag0 * dlnr[i] + blag1 * dlnr1[i] + blag2 *
			dlnr2[i] + blag3 * dlnr3[i] + blag4 * dlnr4[i] +
			blag5 * dlnr5[i] + blag6 * dlnr6[i] + blag7 * dlnr7[i]
			+ blag8 * dlnr8[i] + blag9 * dlnr9[i] + blag10 * dlnr10[i]
			+ blag11 * dlnr11[i] + blag12 * dlnr12[i] + blag13 *
			dlnr13[i] + bkia * Fatalities[i]
		}
	sumg <- wtq16 + g + g^2 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9
		+ g^10 + g ^11 + g^12 + g^13 + g^14
	blag0 ~ dnorm(.2, .01)
	blag1 ~ dnorm((bdlnr * g / sumg), tau2.inv)
	blag2 ~ dnorm((bdlnr * g^2 /sumg), tau2.inv)
	blag3 ~ dnorm((bdlnr * g^3 /sumg), tau2.inv)
	blag4 ~ dnorm((bdlnr * g^4 /sumg), tau2.inv)
	blag5 ~ dnorm((bdlnr * g^5 /sumg), tau2.inv)
	blag6 ~ dnorm((bdlnr * g^6 /sumg), tau2.inv)
	blag7 ~ dnorm((bdlnr * g^7 /sumg), tau2.inv)
	blag8 ~ dnorm((bdlnr * g^8 /sumg), tau2.inv)
	blag9 ~ dnorm((bdlnr * g^9 /sumg), tau2.inv)
	blag10 ~ dnorm((bdlnr * g^10 /sumg), tau2.inv)
	blag11 ~ dnorm((bdlnr * g^11 /sumg), tau2.inv)
	blag12 ~ dnorm((bdlnr * g^12 /sumg), tau2.inv)
	blag13 ~ dnorm((bdlnr * g^13 /sumg), tau2.inv)
	b0 ~ dnorm(45, .001)
	bdlnr ~ dnorm(4, .01)
	g <- 2 / (1 + exp(-eta)) - 1
	eta ~ dnorm(2, .01)
	bkia ~ dnorm(0, .01)
	sigma2.inv <- 1 / sigma
	sigma ~ dt(0, .01, 5)T(0,)
	tau2.inv <- 1 / tau
	tau ~ dt(0, .001, 1)T(0,)
}"
sampler4 <- jags.model(textConnection(model4), data=data, n.chains=4)
update(sampler4, 1000)
samples4 <- coda.samples(sampler4, variable.names=c("b0", "bdlnr", "g",
	"bkia", "sigma", "blag0", "blag1", "blag2", "blag3", "blag4", "blag5",
	"blag6", "blag7", "blag8", "blag9", "blag10", "blag11", "blag12",
	"blag13"), n.iter=10000)
summary(samples4)
means4 <- summary(samples4)$statistics[paste0("blag", 0:13, sep=""),"Mean"]
lo4 <- summary(samples4)$quantiles[paste0("blag", 0:13, sep=""),"2.5%"]
hi4 <- summary(samples4)$quantiles[paste0("blag", 0:13, sep=""),"97.5%"]

plot(x=NULL, y=NULL, xlab="lag", ylab="Estimated Effect", main="",
	xlim=c(0,13), ylim=c(-0.5,1), bty="n")
polygon(x=c(0:13,13:0), y=c(lo4, rev(hi4)), col="lightgrey", border=NA)
points(x=0:13, y=means4, pch=19, cex=1.5)
lines(x=0:13, y=means4, lwd=2)
points(x=0:13, y=means2, pch=19, col="red", cex=2)
lines(x=0:13, y=means2, lwd=2, col="red")
