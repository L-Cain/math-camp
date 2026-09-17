#' ---
#' title: "6. Loops and Simulation"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Understand loops (for, while, if). Basics of Monte Carlo. Some practice using Anton's example. Apply group
#Clear environment  
rm(list = ls())
library(tidyverse)

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

for (fruit in fruits){
  print(fruit)
}

#alternatively,

i<-0
for (i in 1:length(fruits)){
  print(fruits[i])
}


#why does this only show 'grape'?
for (i in length(fruits)){ # for i in the number 4 (which is length of fruits)
  print(fruits[i])
}



#Exercise 1: For the states of interest, print the percentage of the state's population that is male, rounded to 0.01.
cen10 <- read_csv("../data/usc2010_001percent.csv", col_types = cols())
states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")

view(cen10_pop)

cen10_pop <- cen10 |> 
  mutate(male = ifelse(sex == "Male", 0, 1)) |>
  group_by(state) |> 
  summarise(percent_male = round(mean(male) * 100, 2)) 

for (i in (states_of_interest)) {
  cen10_pop_cal <- cen10_pop |> 
    filter(state == i)
  
  cen10_pop_cal[1,2] |> 
    print()
}

#Now change it so that this information is stored in a vector, not printed.

#Exercise 2: store this information in a dataframe instead of printing. Hint: initialize an empty dataframe of the correct size first.

states <- data.frame()

for (i in (states_of_interest)) {
  cen10_pop_cal <- cen10_pop |> 
    filter(state == i)
  
  cen10_pop_cal[1,2] |> 
    print()
  states[i, 1] <- cen10_pop_cal[1, 2]
}

view(states)

# what luke did

for (s in states_of_interest) {
  a <- cen10 |> filter(state == s) |> 
    summarize(p_male = mean(sex == "Male"))
  
  paste0(s, ' is ', round(100 * a[1,1] ,2), '% male') |> 
           print()
}

male_vec <- c()

for (s in states_of_interest) {
  male_vec[s] <- cen10 |> filter(state == s) |> 
    summarize(p_male = mean(sex == 'Male'))
}

#We can also write loops inside of other loops (nested)
#What is each line doing?
race_state_df<-data.frame(state = NA,
                          race = NA,
                          percentage = NA)


row<-1
states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")
for (state in states_of_interest) {
  for (race in unique(cen10$race)) {
    
    
    race_state_num <- nrow(cen10[cen10$race == race & cen10$state == state, ])
    
    state_pop <- nrow(cen10[cen10$state == state, ])
    
    race_perc <- round(100*(race_state_num/(state_pop)), digits=2)
    
    race_state_df[row,1]<-state
    race_state_df[row,2]<-race
    race_state_df[row,3]<-race_perc
    
    row<-row+1
  }
}


race_state_df2<-data.frame(state = NA,
                          race = NA,
                          percentage = NA)

states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")
for (state in states_of_interest) {
  for (race in unique(cen10$race)) {
    
    
    race_state_num <- nrow(cen10[cen10$race == race & cen10$state == state, ])
    
    state_pop <- nrow(cen10[cen10$state == state, ])
    
    race_perc <- round(100*(race_state_num/(state_pop)), digits=2)
    
    temp<-data.frame(state = state,
               race = race,
               percentage = race_perc)
    
    #binding at every step
    race_state_df2<-rbind(race_state_df2,temp)%>%na.omit()
  }
}



race_state_df3 <- data.frame(expand.grid(states_of_interest, unique(cen10$race))) %>% 
  rename(state = Var1,
         race = Var2) %>% 
  mutate(race_perc = as.numeric(rep("", 36)))

states_of_interest <- c("California", "Massachusetts", "New Hampshire", "Washington")
for (state in states_of_interest) {
  for (race in unique(cen10$race)) {
    race_state_num <- nrow(cen10[cen10$race == race & cen10$state == state, ])
    state_pop <- nrow(cen10[cen10$state == state, ])
    race_perc <- round(100*(race_state_num/(state_pop)), digits=2)
    
    # Find the row that matches current state and race, then assign percentage
    race_state_df3[race_state_df3$state == state & race_state_df3$race == race, "race_perc"] <- race_perc
    
  }
}



n_states <- length(states_of_interest)
n_races <- cen10 %>% pull(race) %>% unique() %>% length()
races <- cen10 %>% pull(race) %>% unique()

race_state_df4 <- data.frame(state = rep(states_of_interest, each=n_races), 
                      race = rep(races, times=n_states), 
                      perc = rep(NA, each=n_states*n_races))

for (state in states_of_interest) {
  for (race in unique(cen10$race)) {
    race_state_num <- nrow(cen10[cen10$race == race & cen10$state == state, ])
    state_pop <- nrow(cen10[cen10$state == state, ])
    race_state_df4$perc[race_state_df4$state==state & race_state_df4$race==race] <- round(100*(race_state_num/(state_pop)),digits=2)
  }
}




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

#Exercise 3: 
  #a) Re-create this using ifelse() from tidyverse. 

for (num in numbers) {
 
  message <-  ifelse(num > 8, "that's a big number",
                    ifelse(num < 3, "that's a small number", "that's a medium number"))
  
  round(num, 2) %>% 
    paste0(., '? ', message) %>%
    print(.)
}

  #b) what about with case_when()

for (num in numbers) {
  message <- case_when(
    num > 8     ~ "that's a BIG number",
    num > 3     ~ "that's a medium number",
    num > 0     ~ "that's a small number"
  )
  
  round(num, 2) %>% 
    paste0(., '? ', message) %>%
    print(.)
}


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



#readline() will ask you to enter a response in the terminal, like so:
readline_example<-readline(prompt = 'Please enter an example:')

#Exercise 4: Password checker

#a) Please create a "while" loop that utilizes readline to have the user generate a strong password
#The password must contain a capital, a special character, a number, and be at least 8 characters long.
#The checker should give an appropriate response when encountering a weak password



#initialize
password_good<-0
password<-''


while (password_good ==0){
  
  #enter password
  
  
  #test if capital
  

  #test if numeric
  
  
  #test if special
 
  
  #test if length
  
  #All conditions met?
  if (password_cap *password_num *password_spec *password_length ==1){
    
    #congratulate
   
    #set password

    #end loop
  }
}



#b) Create a function that does all the checking in one go, then build a "while" loop around that function instead
check_strong_password <- function(password) {
  
}


while(){
  
}

#c) Have the password checker store previous passwords and reject any attempt to use a password from a previous run. 
#Lock users out for 30 seconds after 5 failed attempts to generate a password. Track and display remaining attempts


#----Optional: Apply----
#The apply() family of functions is like for loops, but better in some ways (cleaner, faster, less error-prone).


#apply() works on data frames and matrices, processing by row by default, or by column if specified.

#Exercise 5: read in the polity dataframe and convert it to wide format
polity<-read.csv('../data/sample_polity.csv')

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

#Exercise 6:  make our Big Number Checker into a function, then pass a vector of numbers to it via sapply()

#Lastly, mapply can take _M_ultiple arguments
mapply(function(x,y) paste0('I have ',x,' ',y,'s'),
  numbers, 
  fruits)
