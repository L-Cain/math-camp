#' ---
#' title: "7. Randomness and Simulation"
#' author: "Luke Cain and Anton Strezhnev"
#' output: pdf_document
#' ---

#Purpose: Consider randomness and practice simulations
#Clear environment  
rm(list = ls())
library(tidyverse)


# An increasing amount of political science contributions now include a simulation. 
# 
# * [Axelrod (1977)](http://www-personal.umich.edu/~axe/research/Dissemination.pdf) demonstrated via simulation how atomized individuals evolve to be grouped in similar clusters or countries, a model of culture.^[[Axelrod, Robert. 1997. "The Dissemination of Culture." _Journal of Conflict Resolution_ 41(2): 203–26.](http://www-personal.umich.edu/~axe/research/Dissemination.pdf)]
# * [Chen and Rodden (2013)](http://www-personal.umich.edu/~jowei/florida.pdf) argued in a 2013 article that the vote-seat inequality in U.S. elections that is often attributed to intentional partisan gerrymandering can actually attributed to simply the reality of "human geography" -- Democratic voters tend to be concentrated in smaller area. Put another way, no feasible form of gerrymandering could spread out Democratic voters in such a way to equalize their vote-seat translation effectiveness. After demonstrating the empirical pattern of human geography, they advance their key claim by simulating thousands of redistricting plans and record the vote-seat ratio.^[[Chen, Jowei, and Jonathan Rodden.  "Unintentional Gerrymandering: Political Geography and Electoral Bias in Legislatures. _Quarterly Journal of Political Science_, 8:239-269"](http://www-personal.umich.edu/~jowei/florida.pdf)]
# * [Gary King, James Honaker, and multiple other authors](https://gking.harvard.edu/files/abs/evil-abs.shtml) propose a way to analyze missing data with a method of multiple imputation, which uses a lot of simulation from a researcher's observed dataset.^[[King, Gary, et al. "Analyzing Incomplete Political Science Data: An Alternative Algorithm for Multiple Imputation". _American Political Science Review_, 95: 49-69.](https://gking.harvard.edu/files/abs/evil-abs.shtml)]  (Software: Amelia^[[James Honaker, Gary King, Matthew Blackwell (2011). Amelia II: A Program for Missing Data. Journal of
#   Statistical Software, 45(7), 1-47.](http://www.jstatsoft.org/v45/i07/)])

# Statistical methods also incorporate simulation: 
# 
# * The bootstrap: a statistical method for estimating uncertainty around some parameter by re-sampling observations. 
# * Bagging: a method for improving machine learning predictions by re-sampling observations, storing the estimate across many re-samples, and averaging these estimates to form the final estimate. A variance reduction technique. 
# * Statistical reasoning: if you are trying to understand a quantitative problem, a wonderful first-step to understand the problem better is to simulate it! The analytical solution is often very hard (or impossible), but the simulation is often much easier :-) 

#Simulation allows you to learn about a process without being able to solve every step analytically.


#----Random Sampling----
#Often, we want to something "randomly". Computers are deterministic(ish), but we can get close
#They'll do complex math on a starting value in a chaotic way to reach pseudo-random numbers. Specify a "seed", get the same pseudo-random process every time:
set.seed(60637)


#To take a random series from a vector
fruit<-c('apple','banana','pear','grape')

sample(fruit, 
       size = 3)

#Run it a few times. Why do we never get the same word twice?


#So far we've assumed an equal probability of each element. We can be explicit about this:
sample(c("Head", "Tail"), 
       size = 20, 
       prob = c(.5, .5), 
       replace = TRUE)

#And we can weight one outcome more (axioms of probability apply):
sample(c("Head", "Tail"), 
       size = 20, 
       prob = c(0,1), 
       replace = TRUE)


#Exercise 1: Our census dataset (usc2010_001percent) is too big. Sample 1/10 of the rows of the following, without replacement:
usc<-read.csv('../data/usc2010_001percent.csv')

sample(1:nrow(usc),
       size = nrow(usc)/10,
       replace=F)

usc_subset<-usc[sample(1:nrow(usc),
       size = nrow(usc)/10,
       replace =T),
]
#----Boot Strap----

#The bootstrap is a concept you will encounter lots in upccoming classes. In brief, what if we 'resampled' the data from the superpopulation from which it was drawn, and did [analysis] again. 
#We'll formalize this in upcoming courses. 

siena<-read.csv('../data/upshot-siena-polls.csv')%>%
  filter(gender == 'Male' | gender == 'Female')


(original<-lm(data = siena,
   turnout_score ~ gender))

#Initialize
resamples<-1000
coefs<-rep(NA, resamples)

#loop over 1000 times
for (i in 1:resamples){
  #resample with replacement
  siena_resamp<-siena[sample(nrow(siena),
                             replace = T),
                      ]
  
  #store male coefficient
  coefs[i]<-lm(data = siena_resamp,
     turnout_score ~ gender)$coef[2]
}

#EXercise 2: Plot a histogram of the coefficients. Make vertical lines at 0 and at the original result
#add lines for the 25th and 975th largest values
ggplot()+
  geom_histogram(aes(x = coefs))+
  geom_vline(aes(xintercept = original$coefficients[2]))



#----Distributions----
#We're often going to need to sample from a few standard distributions. We'll run through them below and show histograms.


#Binomial: 

#Flipping a weighted coin 1000 times
rbinom(n = 1000, size = 1, prob = 0.2)%>%
  as.data.frame()%>%
  ggplot(.)+
    geom_histogram(aes(x = .))

#What if I flip a fair coin 10 times, take the number of heads, then repeat that 100 times
rbinom(n = 100, size = 10, prob = 0.5)%>%
  as.data.frame()%>%
  ggplot(.)+
  geom_histogram(aes(x = .))
#Looks a lot like a normal distribution!

#Normal distribution
rnorm(n=1000, 
      mean=0, 
      sd=2)%>% #standard deviation = how spread out the data. More formalization coming!
  as.data.frame()%>%
  ggplot(.)+
  geom_histogram(aes(x = .))
#note that it's not perfectly normal, but as we add more draws...


#Lastly, the uniform distribution! This might be the most obvious, each number in a range has equal probability of being drawn.
runif(n = 1000000,
      min=.1, 
      max=1)%>% 
  as.data.frame()%>%
  ggplot(.)+
  geom_histogram(aes(x = .))
  #what's does the cdf of the uniform distribution look like?


#for all of these, we can take the cumulative probability with, e.g., punif(), and density with, e.g. dunif()


#----Monte Carlo----
#sometimes a question is very hard to solve analytically, but we can simulate the process.

#imagine we didn't know how to find the area of a circle. How could we find it from scratch?

#let's throw darts at a 1x1 dartboard
darts<-data.frame(x = runif(10000,-.5,.5),
           y = runif(10000,-.5,.5))

#how many fall within a circle with formula x^2 + y^2 = 0.5^2
darts$circle<-ifelse(darts$x^2 + darts$y^2 > .5^2,
                     0,
                     1)

#plotting it
ggplot(darts,
       aes(x = x,
           y = y,
           color= as.factor(circle)))+
  geom_point()

#compare to pi * d^2
mean(darts$circle)
pi*.5^2
  #pretty good!


#----Big Exercise 3----
#(Thanks Anton!)

#Monte Carlo simulations are frequently used to analyze expected outcomes in casino games and calculate the "house edge" or the percent of a player's winnings that they can expect to lose on a particular bet. One casino game that is known to have a very **low** house edge is Baccarat.
#In this exercise, you will implement a function that simulates a game of Baccarat. Using this function, you will calculate the expected value for each of the three types of bets in the game. You will then simulate how these expected values might change if the casino set different payouts for some of the bets. 
#First, a brief primer on baccarat. Baccarat is a table game in which two hands of playing cards -- the "player" hand and the "banker" hand -- are drawn and then compared against one another. The hand containing the highest score wins the round. The most common version played in modern casinos is called ["punto banco"](https://en.wikipedia.org/wiki/Baccarat) baccarat and essentially plays itself according to a fixed set of card drawing rules. The names "player" and "banker" are simply labels for each of the two hands. 
#Before each round of play, bettors can place one of three bets: that the "player" hand will win, that the "banker" hand will win, or that the hands will "tie" and have the same score.
#Then, the round begins. The game is played by drawing cards from a "shoe" of cards containing multiple decks of standard playing cards (typically six or eight). The shoe is shuffled to randomize the order. **Two** cards are dealt to the *player hand* and **two** cards are dealt to the *banker hand*. We will ignore common casino practice of "burning" some cards from the top of the deck to discourage card counting as this does not impact the simulation. 
#The player hand may then be dealt a third card depending on the value of the initial two cards.

#The banker hand may also receive a third card depending on the value of the banker's initial two cards as well as the value of the player's third card if they drew one. The rules for determining whether either hand receives a third card are below. After all cards are dealt, the value of the hands is calculated. 
#Face cards and 10s are worth zero points, aces are worth one point, and all other cards are worth their printed number (2-9 points). The value of a hand is the ones digit of the sum of the point values of the cards in that hand. So a hand consisting of a 2 and a 5 would be worth 7 points, while a hand consisting of a 4, 5 and 3 would be worth 2 points ($4 + 5 + 3 = 12, 12 \text{ mod } 10 = 2$). In other words, hands are valued at their point sum modulo 10.
#The hand with the highest value is declared the winner. If the hands are equally valued, the result is a "tie"

#The rules for determining whether the player or banker hand receives a third card in a given round are somewhat complex: 
#**First**, if **either** the player or the banker has a hand valued at 8 or 9, then no third cards are drawn, the round ends, and a winner is declared based on the value of the initial two-card hands.
#**Second**, if neither hand is an 8 or 9, the game decides whether to give the "player hand" a third card. If the player's initial hand value is 5 or less, then they are given a third card. If the player's initial hand value is 6 or 7, they "stand"
#**Third**, the game decides whether to give the "banker hand" a third card. If the player did not receive a third card, the banker acts by the same rule as the player (draw if 5 or less, stand if 6 or 7). If the player did receive a third card, then the decision to draw depends on both the value of the banker's current hand and the value of the **third card drawn** by the player.

# - If the banker's hand is valued at 2 or less, they always draw a third card irrespective of what the player was dealt.
# - If the banker's hand is valued at 3, they draw a third card unless the player's third card is an 8.
# - If the banker's hand is valued at 4, they draw a third card if the player's third card is between 2 and 7 (inclusive)
# - If the banker's hand is valued at 5, they draw a third card if the player's third card is between 4 and 7 (inclusive)
# - If the banker's hand is valued at 6, they draw a third card only if the player's third card is a 6 or 7.
# - If the banker's hand is valued at 7, they do not draw a card

#The Wikipedia page for Baccarat has a nice table that summarizes the "hit/stand" decision for the banker.
#After the cards are dealt and the winner determined for the round, bets are paid. 

# - If the bettor bet on "player" and the "player" hand wins, they keep the amount wagered and receive an equal amount from the casino (pays 1-to-1).
# - If the bettor bet on "banker" and the "banker" hand wins, they keep the amount wagered and receive 95% of their wager from the casino (pays 19-to-20).
# - If the bettor bet on either "player" or "banker" and the result is a tie, they keep the amount wagered but receive no additional money from the casino (push).
# - If the bettor bet on "tie" and the result is a tie, they keep the amount wagered and receive 8 times their wagered amount from the casino (pays 8-to-1)
# - If the bettor bet on "player" and the "banker" wins or if they bet on "banker" and the "player" wins, they lose the amount wagered.
# - If the bettor bet on "tie" and either "banker" or "player" wins, they lose the amount wagered.


#Implement, in code, a function that simulates a single round of play (ignore the betting process for now). Assume the casino is playing with a shoe of six decks of standard playing cards and that this shoe is refreshed after every round.
#Below is an outline for the sorts of functions you should implement to break this problem down into smaller component parts along with some hints.

# This function should take as input some number of decks and generate a shoe of cards in the form of a vector
# HINT: You don't need to store the actual cards, just their values
# HINT 2: Read the documentation for the 'rep' function
gen_deck <- function(shoe_n){
  deck<-c(rep(0,16),
          rep(1,4),
          rep(2,4),
          rep(3,4),
          rep(4,4),
          rep(5,4),
          rep(6,4),
          rep(7,4),
          rep(8,4),
          rep(9,4))
  
  shoe<-rep(deck,shoe_n)
}

# This function should take as input some vector that represents a hand of cards and return its value
value_hand <- function(hand){
  sum(hand)%%10
}



# This function should take as input the banker's and the player's hands and return the outcome of the round
determine_winner <- function(){
  
}


# This is your main function. It should return (at a minimum) the outcome of the round of play
# HINT: Write out each phase of the round in plain language in the comments. Then try to implement that phase in code.
# HINT 2: You'll likely need to use a lot of conditional statements to implement the drawing rules
play_round <- function(){
  
}



###Part 2: Now implement a function that takes as input a bettor's chosen outcome and their wager.
#The function should then play a round of baccarat and return the amount that the bettor receives from the casino. There are a few equally valid ways to implement "winnings."
#For the purposes of this problem, you should have the function return $0$ if the player loses their entire wager.


# This function should take as input a choice and a wager. You can choose to have it wrap `play_round()` or have the call to `play_round()` passed as an argument. Either way, it should return a "payoff" based on the choice the bettor selects and the (random) outcome of the round.
standard_payoff <- function(){
  
}


###Part 3:
#Using a Monte Carlo simulation, calculate the expected return of a wager of 100 dollars on the "player" bet. In other words, if a bettor pays $100$ to bet on "player", what is the amount that they expect to win. Set your seed once to $60639$ and run it for 300,000 iterations (try running for fewer iterations as you're testing, but you'll need a decent number of iterations to get the desired numerical precision)
#Calculate the "house edge" of the "player" bet. The house edge is the difference between a bettor's amount wagered and their expected return, divided by the amount wagered. In other words, it's the amount that a casino expects to keep of a player's bet.



### Part 4 
#Use a Monte Carlo simulation to calculate the expected return of a wager of 100 dollars on the "banker" bet. Calculate the "house edge" of the "banker" bet. Set the seed to $60640$ and run for 300,000 iterations.

#Which bet has the lower house edge?


### Part 5 
#One odd feature of Baccarat is that the "banker" bets pay 19-to-20 rather than the 1-to-1 for player bets. 
#Using a Monte Carlo simulation, show why casinos don't pay 1-to-1 on the banker (calculate the house edge). You'll need to write a new payoff function to handle the alternative payout structure. Set the seed to $60641$ and run for 300,000 iterations.
  


### Challenge problem 
#A new version of Baccarat that has become popular in some areas called "EZ-Baccarat" purports to solve the problem you identified in Part 5 while still paying "banker" bets at 1-to-1. It does so by having any round where the banker wins with a hand of 3 cards that totals 7 points be a "push" (bettors keep their wagers but don't win any additional money) rather than a "win" for the banker hand.

#Show, using a Monte Carlo simulation, how this payout structure retains the house edge on "banker" bets. You may have to modify your simulation functions to return more information about the outcome of the round in order to implement a new payoff function.

#See the [state regulatory documents](https://oag.ca.gov/sites/all/files/agweb/pdfs/gambling/BGC_ez_baccarat.pdf) for more info about this version of the game.