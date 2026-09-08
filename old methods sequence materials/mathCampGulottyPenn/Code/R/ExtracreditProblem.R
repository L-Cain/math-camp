
# Extra Credit Problem

# Draw a coin from a bag of biased coins, flip it N times and record the number of heads.

# In this problem we have a mixture of two kinds of randomness.  The first is the determination of the bias of the coin, q, which has a mean mu and variance sigma^2.  The second is the outcome of the coin flips, which will have a mean q and variance q(1-q).  We know from the problem that X_1, the first flip, and X_2, the second flip, will not be independent.  This is because they are both going to come from the same biased coin (we dont return the coin back to the bag).  

#Our problem is to use this dependence to make a prediction from the observation of one flip to the next.  
library(purrr)

# Simulation Method
ncoins<-100000

alpha<-1
beta<-1
mu<-alpha/(alpha+beta)
sigma2<-alpha*beta/((alpha+beta)^2*(alpha+beta+1))

drawcoins<-rbeta(ncoins, alpha, beta)

#For each coin, flip it twice:
twoflips<-map(drawcoins,~rbinom(2, 1, .x))
firstflip<-map_dbl(twoflips, 1)
secondflip<-map_dbl(twoflips, 2)
simcov<-cov(firstflip, secondflip)

cat("compare our sim:", simcov," to the theory:", sigma2)

# Now see what the simulated expectation is:

conditionalmean<-mean(secondflip[firstflip==1])

cat("compare conditional mean", round(conditionalmean, 2)," to the unconditional mean ", round(mean(secondflip),2))

# Ok so clearly we should expect a higher draw if we see a head in the first flip.  But how to find the right estimate in theory?  I propose we use the regression framework:
# The best linear unbiased estimator for 
#E(Y|x) is a + b x where b=cov(x,Y)/var(x) and a = Y-b*xbar

#We know from the problem that the cov(x,Y) is sigma2.
#We know from the problem that the variance of x is mu-mu^2
# So b = sigma2/(mu-mu^2)
b=sigma2/(mu-mu^2)
# and a = Ybar-sigma2/(mu-mu^2)*xbar
a = mu-sigma2/(mu-mu^2)*mu
# So E(X_2|X_1=1) is 
prediction = a+b*1

cat("compare prediction", prediction," to the simulation ", conditionalmean)
#Pretty good!

# We can do better, as we know how to calculate the conditional expectation:

#We know that P(x_1=1,x_2=1)=E[Q^2]=Var[Q]+E[Q]^2 from the definition of the variance [Var[Q]=E[Q^2]-E[Q]^2 ]
# Var[Q]+E[Q]^2= mu^2+sigma2
#So by definition of conditional probability
#E[x_2=1|x_1=1]=P(x_2=1|x_1=1)= P(x_2=1,x_1=1)/P(x_1=1)
prediction2=(mu^2+sigma2)/mu

cat("compare prediction", prediction2," to the simulation ", conditionalmean)

