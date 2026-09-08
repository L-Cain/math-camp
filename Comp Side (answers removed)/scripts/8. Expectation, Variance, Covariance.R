#' ---
#' title: "8. Expectation, Variance, Covariance, Independence"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: What do these concepts look like in code?
#Clear environment  
rm(list = ls())
library(tidyverse)
library(roxygen2)
library(plotly)
library(rgl)

set.seed(60637)

#----Expectation----
#Recap: what is expectation?

#We cannot observe it directly with data, but we can estimate!

#So far, we've talked about distributions in a theoretical sense, looking at different properties of random variables.  
#We don't observe random variables; we observe realizations of the random variable. These realizations of events are roughly equivalent to what we mean by "data".
#We'll spend more time in the intro class talking about this from the standpoint of *estimands*, *estimators* and *estimates*.

#If we draw from a distribution, even an unknown one, is the sum of the values times their probability.

#When we have data, our best guess simplifies to a mean, because the probability a value is in the dataset is reflected in how much it is actually in the dataset.
#This makes more sense when we consider any data as sampling from some (super)population, assuming no biases.

#What's the sample expectation of this weird distribution?
distribution<-rbinom(1000000,1,.3)*5 + runif(1000000,0,1)*2 + rnorm(1000000,0,1)%>%
  as.data.frame()


#simple as:
mean(distribution$.)

#balance the shape!
ggplot(data = distribution,
       aes(x = .))+
  geom_histogram() +
  geom_vline(xintercept = mean(distribution$.))


#The sample expectation, in the discrete data world (as opposed to the continuous, distribution ideal) is a weighted sum. It's just multiplying and adding.
#when you think about it that way, some nice properties come out. We'll prove each if we have time, but first

#Exercise 1: Demonstrate that

#'  1. Expectation of a constant is a constant $$E(c)=c$$
#'  2. Constants come out $$E(c g(Y))= c E(g(Y))$$
#'  3. Expectation is Linear $$E(g(Y_1) + \cdots + g(Y_n))=E(g(Y_1)) +\cdots+E(g(Y_n)),$$ regardless of independence
#'  4. Expected Value of Expected Values: $$E(E(Y)) = E(Y)$$ (because the expected value of a random variable is a constant)


#----sample variance----
#We know where a distribution (or sample) is centered. But what about its spread?
#In other words, how far from the average is the average observation?

#There are different ways to conceptualize this, but we (and everyone else) will use the following:
#'$$\text{Var}(Y) = E[(Y - E(Y))^2] =  E(Y^2)-[E(Y)]^2$$
#Exercise 2: prove it!

#in R, it's easy
var(distribution$.)

#Because sample variance has a squared term, its properties are not as straightforward as sample expectations. We'll leave those for class.


#Exercise 3: Calculate the sample variance of 
#'$$f(x) =  \begin{cases}
#'\frac{3!}{x!(3-x)!}(\frac{1}{2})^3 \quad x = 0,1,2,3\\
#'0 \quad otherwise
#'\end{cases}
#'$$

#Hint: First calculate $E(Y)$ and $E(Y^2)$


#----Covariance----

#The covariance measures the degree to which two random variables vary together; 
#if the covariance between $X$ and $Y$ is positive, X tends to be larger than its mean when Y is larger than its mean.  

#'$$\text{Cov}(X,Y) = E[(X - E(X))(Y - E(Y))] $$

#What's the covariance of the following sample,from a weird joint distribution:
n<-1000
y<-seq(from = 1/250, to = 4, by =1/250) +rnorm(n = n) + rbinom(n= n, size =1,.1)*4
x<-rweibull(n, shape = 2, scale = .1) - rep(c(.1,-.1),500) - seq(1/n,1, by = 1/n)

ggplot()+
  geom_point(aes(x = x,
                 y = y))

#Hard to do analytically from distributions. Easy to sample and calculate an estimate.
cov(x,y)

#Notice that covariance scales unhelpfully...
x1<-seq(0,100)
y1<- seq(0,100)

ggplot()+
  geom_point(aes(x = x1,
                 y = y1))


#X and Y have a similar relationship here,  no?
x2<-seq(0,50)
y2<-seq(0,50)

ggplot()+
  geom_point(aes(x = x2,
                 y = y2))


cov(x1,y1)
cov(x2,y2)


#To avoid this problem we use correlations, which scale between -1 and 1:
#'$$\text{Corr}(X, Y) = \frac{\text{Cov}(X,Y)}{\sqrt{\text{Var}(X)\text{Var}(Y)}} = \frac{\text{Cov}(X,Y)}{SD(X)SD(Y)}$$

cor(x1,y1)
cor(x2,y2)




#Properties of variance and covariance, for your reference:
#'  1. $\text{Var}(c) = 0$
#'  2. $\text{Var}(cY) = c^2 \text{Var}(Y)$
#'  3. $\text{Cov}(Y,Y) = \text{Var}(Y)$
#'  4. $\text{Cov}(X,Y) = \text{Cov}(Y,X)$
#'  5. $\text{Cov}(aX,bY) = ab \text{Cov}(X,Y)$
#'  6. $\text{Cov}(X+a,Y) =  \text{Cov}(X,Y)$
#'  7. $\text{Cov}(X+Z,Y+W) = \text{Cov}(X,Y) + \text{Cov}(X,W) + \text{Cov}(Z,Y) + \text{Cov}(Z,W)$
#'  8. $\text{Var}(X+Y) = \text{Var}(X) + \text{Var}(Y) + 2\text{Cov}(X,Y)$

#Exercise 4: Groups prove one each!

#Exercise 5: Find the expectation and variance. 


#Now find the sample expectation and sample variance

#'Given the following PDF: 
#'  $$f(x) =  \begin{cases}
#'\frac{3}{10}(3x - x^2) \quad 0 \leq x \leq 2\\
#'0 \quad otherwise
#'\end{cases}
#'$$



x<-seq(0,2,.001)
p<-3/10*(3*x-x^2)
r_pdf<-sample(x = x,
              size = 1e5,
              prob = p,
              replace =T)

ggplot()+
  geom_histogram(aes(x = r_pdf))

mean(r_pdf)
var(r_pdf)


#----Conditionals---- 
#If time, show 
# Load Titanic data
data(Titanic)
titanic_df <- as.data.frame(Titanic)

# Expand the data (each row represents Freq number of people)
expanded_titanic <- titanic_df[rep(row.names(titanic_df), titanic_df$Freq), 1:4]


#What's the overall chance of survival? P(Survived)
expanded_titanic$survived_bin<-ifelse(expanded_titanic$Survived == 'Yes',1,0)
survived<-sum(expanded_titanic$survived_bin)
survived/nrow(expanded_titanic)


mean(expanded_titanic$Survived =='Yes')



#P(Survived| Class = 2nd)
expanded_titanic%>%
  filter(Class == '2nd') %>%
  summarise(prob_survived = mean(Survived == 'Yes'))

#Exercise 6: Which subgroup had the lowest chance of survival?
  #consider using prop.table
?prop.table()
attach(expanded_titanic)
table(Survived,Class,Sex,Age)%>%
  prop.table()


survival_prop<-expanded_titanic%>%
  group_by(Age,Class,Sex)%>%
  summarise(mean(survived_bin))


expanded_titanic%>%
  group_by(Class)%>%
  summarise(mean(survived_bin))
#What's the probability of being in 2nd class given that you've survived P(Class = 2nd|Survived)?


expanded_titanic%>%
  filter(Survived == 'Yes') %>%
  summarise(prob_2nd = mean(Class == '2nd'))
  #Johnson et al, 2019...

set.seed(3)
customers <- data.frame(
  age_group = sample(c("18-25", "26-40", "41-55", "56+"), 2000, replace = TRUE,
                     prob = c(0.3, 0.35, 0.25, 0.1)),
  purchase_made = sample(c("Yes", "No"), 2000, replace = TRUE, prob = c(0.4, 0.6)),
  device = sample(c("Mobile", "Desktop", "Tablet"), 2000, replace = TRUE,
                  prob = c(0.6, 0.3, 0.1))
)

#What's the probability of making a purchase?
mean(customers$purchase_made == 'Yes')

#What's the probability of making a purchase for those in the youngest category?
mean(customers$purchase_made[customers$age_group== '18-25'] == 'Yes')

#Exercise 7: Do this with tidyverse

#P(purchase b * P(purchase|age group = 18-25). What does that mean about age and purchase propensity?
  #note, they're not exactly the same. Is that an actual difference or due to chance?



#Continuous Conditional Expectation (Thanks to Andy Eggers!)
#With probability and outcomes, we can take expectations. 
  #Note, we've actually been doing expectations in the above, but since the outcomes were binary, probability and expectation are identical.



dat <- expand_grid(x = seq(-10, 10, length = 300),
                   y = seq(-10, 10, length = 300))


Sigma <- cbind(c(1, .7), c(.7, 1))
dat |> 
  mutate(z = mvtnorm::dmvnorm(as.matrix(dat), sigma = Sigma)) -> datt2

open3d()

plot3d(datt2, col = "grey50", alpha = .5,
       xlab = "x", ylab = "y", zlab = "f(x, y)",
       xlim = c(-3, 3), ylim = c(-3, 3),
       aspect = c(1, 1, .5))

#what's the expectation? #E[f(x,y)]
#What's #E[f(x,y)| Y = 1]
planes3d(a = 1, b = 0, c = 0, d = -1,
         col = "red",
         alpha = .5)

n <- 100
y <- seq(-3, 3, length = n)
x <- rep(1, length(y))
z <- mvtnorm::dmvnorm(cbind(x, y), sigma = Sigma)
 lines3d(x = x, y = y, z = z, col = "black", lwd = 2)

#we can do conditional variance, covariance, etc....



#But we won't today.
