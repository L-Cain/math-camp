rm(list = ls())
library(tidyverse)

rain <- data.frame(
  Sunday = logical(10000),
  Monday = logical(10000),
  Tuesday = logical(10000),
  Wednesday = logical(10000),
  Thursday = logical(10000),
  Friday = logical(10000),
  Saturday = logical(10000),
  DaysRain = numeric(10000)
)

for(i in 1:10000) {
  rain$Thursday[i] <- rbinom(1, 1, 0.2)
  rain$Friday[i] <- rbinom(1, 1, 0.11)
  rain$Saturday[i] <- rbinom(1, 1, 0.34)
  rain$Sunday[i] <- rbinom(1, 1, 0.02)
  rain$Monday[i] <- rbinom(1, 1, 0.1)
  rain$Tuesday[i] <- rbinom(1, 1, 0.4)
  rain$Wednesday[i] <- rbinom(1, 1, 0.4)
  rain$DaysRain[i] <- sum(rain[i, ])
}

print("On what percentage of the first week did it rain")

print(paste("It rained", sum(rain[1, ])/7 * 100, "percent of the week"))

print("On what percentage of Sundays did it rain")

print(paste("It rained", sum(rain[ , 1]/nrow(rain)*100), "percent on Sundays"))

print("What percentage of Thursdays did it not rain")

thurs_norain <- (1- sum(rain[, 5])/nrow(rain)) * 100
print(paste("It did not rain", thurs_norain, "percent on Thursdays"))

norain <- (1-.2)*(1-.11)*(1-.34)*(1-.02)*(1-.1)*(1-.4)*(1-.05)
print(paste("The likelihood of there being rain next week is", ((1-norain)* 100), "percent"))

j <- 0

for(i in 1:nrow(rain)) {
  if(rain$DaysRain[i] == 2) {
    j <- j + 1
  }
}
j/nrow(rain)

vec<-c(rainvec<-c(12,300)

vec >20
