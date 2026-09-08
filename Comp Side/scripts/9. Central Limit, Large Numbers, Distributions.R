#' ---
#' title: "9. Asymptotics and Convergence"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Explore central limit theorem and the law of large numbers
#Clear environment  
rm(list = ls())
library(tidyverse)
library(gganimate)
set.seed(60637)

#In theoretical and applied research, asymptotic arguments are often made. In this section we briefly introduce some of this material. 

#What are asymptotics?  
#In probability theory, asymptotic analysis is the study of limiting behavior. 
#By limiting behavior, we mean the behavior of some random process as the number of observations gets larger and larger. 
func<-function(x) 1/x
x<-seq(0, 5, length.out = 1e4)
y<-func(x)

ggplot()+
  geom_line(aes(x = x, y = y),color = 'dodgerblue') +
  scale_y_continuous(limits = c(0,50),) +
  geom_hline(yintercept = 0)

#'Exercise 1: How do we know $f(x) != 0 \all x$? But what if I want to be sure I'm within some bound, e, of 0?


#Why is this important?  
#We rarely know the true process governing the events we see in the social world. 
#It is helpful to understand how such unknown processes theoretically must behave and asymptotic theory helps us do this. 



#----Weak Law of Large Numbers----
#What's the probability of a fair coin landing on heads? P[H] = 0.5
#But any individual flip will either give heads or tails.

sample(c('H','T'),
       size = 1)

#As we increase the number of flips, we generally get a proportion Heads close to 0.5
sample(c(1,0),
       size = 100,
       replace = T) %>%
  mean()

#This is an example of the weak law of large numbers.

#Formally:
#'For any draw of independent random variables with the same mean $\mu$, 
#'the sample average after $n$ draws,
#' $\bar{X}_n = \frac{1}{n}(X_1 + X_2 + \ldots + X_n)$,
#'  converges in probability to the expected value of $X$, $\mu$  as $n \rightarrow \infty$:
  
  #'$$\lim\limits_{n\to \infty} P(|\bar{X}_n - \mu | > \varepsilon) = 0$$
  

#Graphically:
n<-100

#sample coinflips
coinflip<-data.frame(heads = sample(c(1,0),
                          size = n,
                          replace = T),
           x = 1:n)%>%
  #cumulative heads
  mutate(prop_heads = cumsum(heads) / seq_along(heads))

  ggplot(data = coinflip,
         aes(x = x, 
             y = prop_heads))+
    geom_line()+
    labs(title = 'Coinflips: {timeframe}',
         x = 'sample average',
         y = 'probability')+
    transition_time(x)+
    ease_aes('linear')

  
  
  p <- ggplot(data = coinflip, aes(x = x, y = prop_heads)) +
    #Proportion heads
    geom_line(color = "red", size = 1.2) +
    # True probability line
    geom_hline(yintercept = 0.5, color = "blue", linetype = "dashed", size = 1) +
    # Current point
    geom_point(color = "red", size = 2) +
    # Set axis limits
    ylim(0, 1) +
    xlim(1, n) +
    # Labels and title
    labs(
      title = 'Coin Flips Converging to True Probability',
      x = 'Number of Flips',
      y = 'Proportion of Heads',
      caption = 'Red line: Sample proportion | Blue dashed: True probability (0.5)'
    ) +
    # Styling
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    ) +
    # Animation settings
    transition_reveal(x) +
    ease_aes('cubic-out')

  anim <- animate(p, 
                  width = 800, 
                  height = 600, 
                  fps = 15, 
                  duration = 10,
                  renderer = gifski_renderer("../output/coinflip_convergence.gif")) 

  

#----Central Limit Theorem----  
#'Let $\{X_n\} = \{X_1, X_2, \ldots\}$ be a sequence of i.i.d. random variables with finite mean ($\mu$) and variance ($\sigma^2$). 
#'Then, the sample mean $\bar{X}_n = \frac{X_1 + X_2 + \cdots + X_n}{n}$ increasingly converges into a Normal distribution. 
  
#' In essence, take draws from most distributions, average them. Do that many times and those averages will approach normality.
#' 
#'Let's flip some more coins!

#flip 20 coins (weighted)
coins<-rbinom(n = 20, size = 1, prob = .3)%>%
    as.data.frame()
  
#average heads
mean<-mean(coins$.)

#plot distribution
ggplot()+
      geom_histogram(aes(x = coins$.))+
      geom_vline(xintercept = mean)

  #...NOT normal

#initialize
n<-1e4
mean<-rep(NA,n)

#loop
for (i in 1:n) {
  #flip 20 coins
  coins<-rbinom(n = 20, size = 1, prob = .3)%>%
    as.data.frame()
  #store mean
  mean[i]<-mean(coins$.)
}

means_df<-data.frame(mean = mean)

#the first 10...
ggplot()+
  geom_histogram(aes(x = means_df[1:10,]))

#...100...
ggplot()+
  geom_histogram(aes(x = means_df[1:100,]))

#...1000...
ggplot()+
  geom_histogram(aes(x = means_df[1:1000,]))

#...10000
ggplot()+
  geom_histogram(aes(x = means_df[1:10000,]))

#This will save us in many (Not all!) cases, allowing us to assume *asymptotic* normality.


#Exercise 1: Find a distribution from which you can sample, take means, and those means don't distribute normally. Graph it!
#Does this match any real-world situation?