#' ---
#' title: "6. Loops"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Understand loops (for, while, if). Apply() group of functions.
#Clear environment  
rm(list = ls())
library(tidyverse)


#----Aside: Calculus!----
#Define a function
f=expression(x^2+3*x)

#Take the derivative
(df<-D(f,'x'))

#Let's plot it!
x <- seq(-6, 3, length.out = 100)

# Evaluate the function
y <- f(x)

#Exercise 1: get the data in a workable form and plot it. Add the plot for f's derivative



#You can also integrate in R (not to steal Ryan's thunder)
g <- function(x) x*3+exp(x)
integrate(g,
          lower = 0,
          upper = 1)

#----For Loops----
#Sometimes we want to do the same operation multiple times. 

#initiate index
#for ([index variable] in [numbers]){
  #do something
#}

i<-0
for (i in 1:10){
  print(i)
}

#we can use to index a vector or dataframe
fruits<-c('apple','bannana','pear','grape')

fruit<-''
for (fruit in fruits){
  print(fruit)
}

#alternatively,

i<-0
for (i in 1:length(fruits)){
  print(fruits[i])
}


#why does this only show 'grape'?
for (i in length(fruits)){
  print(fruits[i])
}



#Exercise 2: For the states of interest, print the percentage of the state's population that is male, rounded to 0.01.
cen10 <- read_csv("../data/usc2010_001percent.csv", col_types = cols())

states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")


for( state in states_of_interest){
  state_data <- cen10[cen10$state == state,]
  nmen <- sum(state_data$sex == "Male")
  
  n <- nrow(state_data)
  men_perc <- round(100*(nmen/n), digits=2)
  print(paste("Percentage of men in",state, "is", men_perc))
  
}

#Now change it so that this information is stored in a vector, not printed.

#We can also write loops inside of other loops (nested)

states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")
for (state in states_of_interest) {
  for (race in unique(cen10$race)) {
    race_state_num <- nrow(cen10[cen10$race == race & cen10$state == state, ])
    state_pop <- nrow(cen10[cen10$state == state, ])
    race_perc <- round(100*(race_state_num/(state_pop)), digits=2)
    print(paste("Percentage of ", race , "in", state, "is", race_perc))
  }
}

#Exercise 3: store this information in a dataframe instead of printing. Hint: initialize an empty dataframe of the correct size first.


#----Conditionals----
#Often, we want to do one thing if a criterion is met, and another if not. We do this with "if" and "else".

#Let's re-create that "big number detector" from earlier
numbers<-runif(n= 20,
               min = 0,
               max = 10)

for (num in numbers){
   
  #discern  message
  if (num < 3){
    message<-"that's a small number"
  } else if (num >8){
    message<-"That's a BIG number!"
  } else {
    message<-"That's a medium number"
  }
  
  #round number
  round(num,
        2)%>%
    #concatenate
    paste0(.,'? ',message)%>%
    #print it
    print(.)
}

#Exercise 4: 
  #a) Re-create this using ifelse() from tidyverse. 
  #b) what about with case_when()




#----While Loops----
#If you want to loop until an outcome, but you don't know how long that will take, use a "while" loop

#My code had an error in it and I am upset. I will punish the computer by putting it in timeout for 30 seconds:


#what's the start time
start_time<-Sys.time()

#while it's less than 30 seconds from start time...
while(Sys.time() < start_time + seconds(30)){
  
  #calculate remaining time
  remaining<-(start_time+seconds(30)) - Sys.time()
  
  #insert that into a message and print
  paste(remaining)%>%
    as.numeric()%>%
    round(.,
          2)%>%
  paste0('I am in timeout for ', ., ' more seconds')%>%
    print()
}


#Exercise 5: Password checker

#readline() will ask you to enter a response in the terminal, like so:
readline_example<-readline(prompt = 'Please enter an example:')

#Please create a "while" loop that utilizes readline to have the user generate a strong password
#The password must contain a capital, a special character, a number, and be at least 8 characters long.
#The checker should give an appropriate response when encountering a weak password

#challenge: Have the password checker store previous passwords and reject any attempt to use a password from a previous run. 
#Lock users out for 30 seconds after 5 failed attempts to generate a password. Track and display remaining attempts


#----Optional: Apply----
#The apply() family of functions is like for loops, but better in some ways (cleaner, faster, less error-prone).


#apply() works on data frames and matrices, processing by row by default, or by column if specified.

#Exercise 6: read in the polity dataframe and convert it to wide format
polity<-read.csv('../data/sample_polity.csv')%>%
  pivot_wider(names_from = 'year',
              values_from = 'polity2')%>%
  filter(country != 'Germany')%>%
  select(-(1:3))

#If we want the mean of each country:
apply(polity, 1,mean)

#If we want the mean of each year:
apply(polity,2,mean)


#sapply is more common. Stands for 'simplified apply()'. sapply() works on vectors and lists, and tries to return a vector when possible
numbers<-c(3,8,38,83)
sapply(numbers,
       sqrt)

#We can also write custom functions into sapply:
sapply(numbers,
       function(x) sqrt(x)*log(x))

#Exercise 7:  make our Big Number Checker into a function, then pass a vector of numbers to it via sapply()

#Lastly, mapply can take _M_ultiple arguments
mapply(function(x,y) paste0('I have ',x,' ',y,'s'),
  numbers, 
  fruits)
