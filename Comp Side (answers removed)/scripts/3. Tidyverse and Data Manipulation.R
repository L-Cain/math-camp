#' ---
#' title: "3. Tidyverse and Data Manipulation"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Explore the tidyverse https://r4ds.had.co.nz/, data manipulation and merging, work with strings and dates

#----Setup----
#Clear environment  
rm(list = ls())

#Last time: packages. Today: one crucial package!
install.packages('tidyverse')
library(tidyverse)
library(nycflights13)

#----Pipes----
#Pipes!
c(3,8)%>%
  mean(.)%>%
  paste0("The above outputs: ",.)

#Exercise 1: pipe it up. What does each component do?
students <- data.frame(
  name = c("Alicia", "Bob", "Chandan", "Diana", "Dingxiang"),
  math_score = c(85, 92, 78, 96, 88),
  science_score = c(90, 87, 82, 94, 91),
  english_score = c(88, 85, 90, 92, 87),
  grade_level = c("10th", "11th", "10th", "12th", "11th"),
  stringsAsFactors = FALSE
)

nested1 <- toupper(str_trim(str_replace_all(paste(students$name, collapse = " | "), "\\s", "_")))
nested2 <- summary(log(abs(scale(students$science_score))))

piped1<-
  
piped2<-
  
  
  
#----Reading Data----
#CSV (comma separated values)

#find the absolute file path for acs2015_1percent
filepath_acs<-'C:/Users/festi/Desktop/math-camp/Comp Side (answers removed)/data/acs2015_1percent.csv'
acs_df<-read.csv(filepath_acs)

#find the relative filepath for acs2015_1percent
filepath_acs<-'../data/acs2015_1percent.csv'
acs_df<-read.csv(filepath_acs)

#Exercise 2: figure out how to read in the following files:
#  `data/ober_2018.xlsx`: A one percent sample of the American Community Survey
#  `data/gapminder_wide.tab`: Country-level wealth and health from Gapminder^[Formatted and taken from <https://doi.org/10.7910/DVN/GJQNEQ>]
#  `data/gapminder_wide.Rds`: A Rds version of the Gapminder (What is a Rds file? What's the difference?)
#  `data/Nunn_Wantchekon_sample.dta`: A sample from the Afrobarometer survey (which we'll explore tomorrow). `.dta` is a Stata format. 
#  `data/german_credit.sav`: A hypothetical dataset on consumer credit. `.sav` is a SPSS format. 

#Remove all but acs
rm(list = setdiff(ls(), "acs_df"))


#----Manipulating a dataframe----
#I want to consider only a subset of rows
acs_working_age<-acs_df%>%
  filter(age < 65, age> 18)

#I only want some of the columns
acs_edu_df<-acs_working_age%>%
  select(perwt, serial, sex, age, educ,city)

#I want to add a new variable
acs_edu_df<-acs_edu_df%>%
  mutate(major_city = ifelse(
    city =='(other)',0,1
  ))


#I want to count how many people are in each city
city_count<-acs_edu_df%>%
  group_by(city)%>%
  summarise(n())


#Alternatively, I could have done all this in one big pipe:
major_city_count<-acs_df%>%
  filter(age<65 & age>18)%>% #working age
  select(serial,sex,age,educ,city)%>% #relevant columns
  mutate(major_city = ifelse( #major city 
    city == '(other)',0,1))%>%
  group_by(major_city)%>% #group
  summarise(n()) #count
  


#Exercise 3: Create a column showing the percent of the total sample in each city

#Exercise 4: What's the average age of those in each education group and sex? (ignore perwt. use the original dataset, not the working age subset)


#Write out to data folder
write_csv(major_city_count,
          '../data/major_city.csv')

#----Group and Summary, more explicitly----

#We want to see the mean and standard deviation of each state's population
df<-acs_df%>%
  group_by(state)%>%
  summarize(mean = mean(age,na.rm = T),
            sd = sd(age, na.rm =T))

#Exercise 5: What if we wanted to do the same by education and gender?


#Exercise 6: find the mean sepal length and width, and the standard deviation of petal width by species

#What's the proportion of sepal widths above 3.25 by species?


#----Manipulating Dataframes----
#Creating a dataframe
students_wide <- data.frame(
  student_id = c(1, 2, 3, 4),
  name = c("Alicia", "Bob", "Chandan", "Dingxiang"),
  math = c(85, 92, 78, 96),
  science = c(NA, 87, 82, 94),
  english = c(88, 80, 80, 92)
)

#pivoting, wide to long (I have to look it up every time)
students_long <- students_wide %>%
  pivot_longer(
    cols = c(math, science, english),     # Which columns to pivot
    names_to = "subject",                 # Name for the new "names" column
    values_to = "score"                   # Name for the new "values" column
  )

#pivot long to wide
students_wide<-students_long%>%
  pivot_wider(names_from = subject,
              values_from = score)


#Merging data
student_fruit <- data.frame(
  student_id = c(1, 2, 3, 4),
  fruit = c('apple','banana','pear','grape'),
  math = c(85, 92, 78, 96),
  science = c(NA, 87, 82, 94),
  english = c(88, 80, 80, 92)
)


#Works both wide...
students_wide_f<-merge(students_wide,
                     student_fruit,
                     by = 'student_id')

#...and long
students_long_f<-merge(students_long,
                       student_fruit,
                       by = 'student_id')

#Exercise 7: Which variable(s) in student_fruit can we not use to merge? Why not?


#Aside: dealing with NAs
mean(students_wide$science,
     na.rm =T)


#Exercise 8:
#option 1: remove all rows with NAs from the dataframe
#option 2: ignore NAs in the function


#----Strings-----
#Combining
paste("apple", "banana")
paste("apple", "banana",
      sep = '_')
paste0("apple", "banana")


#subset
str_sub(student_fruit$fruit,
        start = 2,
        end = 4)

#starts with
x <- c("apple",'apricot', "banana", "pear")
str_view(x, "^a")

#ends with
str_view(x, "a$")

#...many more...https://stackoverflow.com/questions/201323/how-can-i-validate-an-email-address-using-a-regular-expression/201378#201378

#replace
gsub('a','@',x)


#----Time!-----
flights<-flights
  
#I want to plot the flights' delay 
plot(x = flights$date_date,
     y = flights$dep_delay)
#...that can't be right...


flights<-flights%>%
  mutate(date1 = paste(year,month,day,
                       sep = '-'),
         date2 = paste(month,day,year,
                       sep = '/'))%>%
  select(-c(day,month,year))



#Exercise 8: Convert these to date-type objects

#Create a "season" variable, each row should be in a season.

#Are flights departing with more delay in any season? Any day of the week? (summarize)



install.packages("zoo")
library(zoo)


#Exercise 9: Create a new dataset with a daily average departure and arrival delays
#challenge: this is going to be spiky, let's create a "rolling average"
