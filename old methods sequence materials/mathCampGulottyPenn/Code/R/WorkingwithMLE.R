# Maximum likelihood estimation in R
# Heteroskedastic normal example
# Chris Adolph  www.chrisadolph.com

rm(list = ls())
set.seed(123456)
library(MASS)
library(simcf)


#############################################
# Draw heteroskedastic normal data

# Generate 1500 observations
obs <- 1500

# Set the parameter vector for the mean
beta <- c(0, 5, 15)

# Set the parameter vector for the variance
gamma <- c(1, 0, 3)

# Create the constant and covariates
w0 <- rep(1,obs)
w1 <- runif(obs)
w2 <- runif(obs)
x <- cbind(w0,w1,w2)
z <- x

# Create the systematic component for the mean
mu <- x%*%beta

# Create the systematic component for the variance
sigma2 <- exp(z%*%gamma)

# Create the response variable
y <- rnorm(obs)*sqrt(sigma2) + mu

# Save the data to a datafram
data <- cbind(y,w1,w2)
data <- as.data.frame(data)
names(data) <- c("y","w1","w2")

# Plot the data

plot(y=y,x=w1)

plot(y=y,x=w2)

plot(y=w1,x=w2)

###############################
# From here, we pretend we don't know the true specification

# Fit least squares model using lm
ls.result <- lm(y~w1+w2)
print(summary(ls.result))

# Calculate and print the AIC
ls.aic <- AIC(ls.result)
print("AIC")
print(ls.aic)



#####################################################
# Fit ML heteroskedastic normal model using optim()

# Create input matrices
xcovariates <- cbind(w1,w2)
zcovariates <- cbind(w1,w2)

# initial guesses of beta0, beta1, ..., gamma0, gamma1, ...
# we need one entry per parameter, in order!
stval <- c(0,0,0,0,0,0)

######~~~~~~~~~ No need to edit in here ~~~~~~~~~######

# A likelihood function for ML heteroskedastic Normal
llk.hetnormlin <- function(param,y,x,z) {
    x <- as.matrix(x)
    z <- as.matrix(z)
    os <- rep(1,nrow(x))
    x <- cbind(os,x)
    z <- cbind(os,z)
    b <- param[ 1 : ncol(x) ]
    g <- param[ (ncol(x)+1) : (ncol(x) + ncol(z)) ]
    xb <- x%*%b
    s2 <- exp(z%*%g)
    sum(0.5*(log(s2)+(y-xb)^2/s2))  # optim is a minimizer, so min -ln L(param|y)
}

# Run ML, get the output we need
hetnorm.result <- optim(stval,llk.hetnormlin,method="BFGS",hessian=T,y=y,x=xcovariates,z=zcovariates)
                   # call minimizer procedure
pe <- hetnorm.result$par   # point estimates
vc <- solve(hetnorm.result$hessian)  # var-cov matrix
se <- sqrt(diag(vc))    # standard errors
ll <- -hetnorm.result$value  # likelihood at maximum
hetnorm.aic <- 2*length(stval) - 2*ll  # Lower is better

print("Point estimates")
print(pe)

print("Standard errors")
print(se)

print("Log Likelihood at its maximum")
print(ll)

print("AIC")
print(hetnorm.aic)


# Simulate results by drawing from the model predictive distribution
sims <- 10000
simparam <- mvrnorm(sims,pe,vc) # draw parameters

# Separate into the simulated betas and simulated gammas
simbetas <- simparam[,1:(ncol(xcovariates)+1)]
simgammas <- simparam[,(ncol(simbetas)+1):ncol(simparam)]

