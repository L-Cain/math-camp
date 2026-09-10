#' ---
#' title: "2. Packages"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Give basics of functions, explore some packages


#----Setup----
#Clear environment  
rm(list = ls())


#----Functions----
#take in objects as arguments, return other objects

num_to_avg<-c(3,8)

?mean()
mean(num_to_avg)

#if it's not self-evident, specify which argument is which
plot(x = c(3,8),
     y = c(5,10))
  ##note that I use a different line for each argument. This keeps things clean

#if you don't specify an argument, many functions have a default
plot(x = c(3,8),
     y = c(5,10),
     type = 'l')


#we can nest functions
data1 <- rnorm(100, mean = 50, sd = 10)
data2 <- rnorm(100, mean = 45, sd = 12)
mean_of_variance <- mean(var(c(data1, data2)))

#This can get nasty, we'll think of a way to correct this
paste("here's a number:",exp(sqrt(max(var(c(max(data1),min(data2),data1[3:8])),8))),"isn't that cool?")


#The first step is to space things out nicely
paste("here's a number:",
      exp(
        sqrt(
          max(
            var(
              c(max(data1),
                min(data2),
                data1[3:8]))
            ,8))),
      "isn't that cool?")
#we can do better, through *pipes*. Stay tuned



#----Creating Functions----
#I want to add some numbers and then square the result
sum_and_square<-function(num1, num2){
  (num1 + num2)^2
}

sum_and_square(num1= 3,
               num2 =8)



#Functions can call other functions
#I want to detect big numbers
big_num_detector<-function(number){ #we start with arguments, then go into the body
      #determine if big
     if (number > 8){ ##Hardcoded! We'll return to this
      print("wow, that's a big number!")
     } else 
      print("that number isn't very big")
     } 
  


#if we were serious, we'd include documentation
#?big_num_detector()

#we can inspect the function's code
big_num_detector

big_num_detector(number = 3)
big_num_detector(num = 10)


#Exercise 1: 
#a) make the big number detector respond differently when the input is 8
big_num_detector<-function(number){ 
  if (number > 8){ 
    print("wow, that's a big number!")
  } else if (number == 8){
    print('This is exactly 8.')
  }
  else 
    print("that number isn't very big")
} 

big_num_detector(number = 8)

#b) have the big number detector throw an error if the input is not numeric
big_num_detector<-function(number){ 
  if (is.numeric(number) == F){
    print('error')
  }
    else if (number > 8){ 
    print("wow, that's a big number!")
  } 
    else if (number == 8){
    print('This is exactly 8.')
  }
     
  else 
    print("that number isn't very big")
} 

big_num_detector(number = 'mathcamp')

#c) make the threshold for what constitutes a big number into an argument

big_num_detector <- function(number, threshold) {
  if(number <= threshold) {
    print('not a big number')
  }
    else
      print("that's a big number")
}

big_num_detector(number = 9, threshold = 7)

#----Local Variables----
#Sometimes it's helpful for a function to create a variable

#say we want to calculate skew of a vector:
#'$\mathrm{Skew}(x) = \frac{\frac{1}{n-2}\left(\sum_{i=1}^n(x_i - \bar x)^3\right)}{\mathrm{Var}(x)^{3/2}} \text{.}$

skew<-function(vec){
  #define parameters
  n<-length(vec)
  mean<-mean(vec)
  var<-var(vec)
  
  sum((vec - mean)^3)/var^(3/2)/(n-2)
}

vec<-rnorm(n = 20,
      mean= 1,
      sd = 2)

skew(vec)

#vec exists in environment
vec

#n does not
n


#----Returning multiple pieces of information----
# Exercise 2: create a function that takes a vector of grades (0-100) and returns average, letter_grade, pass_fail, highest, lowest

analyze_grades <- function(grades){
  mean(grades)
  letter <- if(mean(grades) >= 90){
    print('A')
  }
    else if (mean(grades) >= 80){
      print('B')
    }
      else if(mean(grades)>= 70){
        print('C')
      }
        else if(mean(grades) >= 60){
          print('D')
        }
          else print ('F')
  
  pf <- if(mean(grades) >= 60){
    print('Pass')
  }
    else print ('Fail')
  
  max(grades)
  min(grades)
  
  print(c('average' = mean(grades), 'letter grade' = letter, 'PF' = pf, 'highest' = max(grades), 'lowest' = min(grades)))
}

#Test 1
analyze_grades(c(85, 92, 78, 88, 95))

#Test 2
analyze_grades(c(85, '92', 78, 88, 950))



#----Packages----
#Packages are just collections of functions and documentation, sometimes with datasets included

#Go to the 'packages' tab, bottom right

#Exercise 3: 
  #a) install 'electoral' by hand
  #b) attach it
  #c) check for updates
  #d) uninstall it

library(electoral)


#let's assign some seats under different voting methods
seats(parties = c("A", "B", "C"), 
      votes = c(100, 150, 60), 
      n_seats = 5, 
      method = "hare") 

seats_df<-seats(parties = c("V", "W", "X", "Y", "Z"), 
      votes = c(100, 150, 60, 80, 160), 
      n_seats = 15, 
      method = "droop") 

#Exercise 4: Use this package's documentation and the internet to determine the electoral volatility in IL senate races in the past two elections
#whose elections are more volatile, Duckworth or Durbin?
durbin<-volatility()
duckworth<-volatility()

?volatility


#Exercise 5: Install the Development Version of 'ggdist" from github

#Sometimes we want packages to be as up-to-date as possible...
#other times we want to ensure the script uses the same version every time
install.packages('groundhog')
pkgs <- c("rio","metafor")
groundhog.library(pkgs, 
                 "2024-09-01",
                  tolerate.R.version = '4.5.0') 


#If you need to do something that someone else has done before, there's almost certainly a package: https://journal.r-project.org/issues.html