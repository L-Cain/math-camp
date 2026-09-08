###########################
# Boot Camp               				#
# R								         		#
# Robert Gulotty    		  				#
###########################

# Welcome

# http://cran.us.r-project.org/ 

# http://www.rstudio.com/




# Writing comments:
# Comments are anything that follow the # sign
# When R comes to a # sign, it ignores what follows
# This way, you can write notes to yourself or others within your R code.
# To clear the screen, type control-L


# Why do we use R?

# Power:    R is free, open source, multi-platform, and has many extensions.
# $$$:      R is used by major players in tech, academia, and government.
# http://www.nytimes.com/2009/01/07/technology/business-computing/07program.html

# Literacy: Learning R will make learning other statistical packages, and other programming tools easier.

# R is an object oriented and functional language, built on C and Fortran.  

#########################
# Getting Started					   #
#########################



# Clear memory
rm(list = ls())
gc()

# Set directory
setwd("~/Dropbox/Rworkshop")

# Load your libraries

library(dplyr)
library(ggplot2)

#  R is a big calculator         

2+2
2+2; 3+4
4/6
4*6
22 %% 3
sqrt(100)
log(100) 
exp(2)
2 * (6 + 2)^2 # R preserves order of operations





############################
# 2) Objects in R                           #
############################


# Data in R has different types
# Quantitative:  Numeric and Integer
# Qualitative:  Character / String and Factors
# Logical

# Numeric: notice the color:
1
2.0
3.05
2e4
-3

str(2L)
str(2)

# Character/ Strings
"a"
"France"
"The time is 9:20"
"2+2"

"Red, Blue"

# Logical

TRUE
T
FALSE
F
NA


# Storing objects, assignments 

# Objects are created by giving them a name.  
# This process is called "assignment".

x <- 2

# This is read the object x gets the value 2.

the.number.of.weekdays <- 5
day_of_week <- "Friday"

# Object names should be:
# Descriptive
# Start with letters
# Use camelCase, period.separators, or_underscores

# Everything in R is an object.

# Your data are collections of these basic data types stored in special objects:
# vectors, lists, matricies, arrays and data.frames.  We will build to data.frames

# Vectors collect one kind of data.  Vectors are made with the function c() called concatenate.
# The inputs must be of the same type, separated by commas.

vec_heights <- c(71, 74, 18)

vec_chores <- c("feed dog", "wash dishes", "guard toys") 


############################
# 3) Indexing in R                          #
############################

# To extract an analyze subsets of data we use indexing.
# An index is called by using the [ ] brackets.  Technically [ ] are a function.
# Indexes can be numbers, logical, or names:

vec_heights
vec_heights[2]
vec_heights[-2]

vec_heights[c(FALSE, TRUE, FALSE)]

names(vec_heights) <- c("CM", "BG", "M")

vec_heights

vec_heights["BG"]

# Why is this important?  This allows you to query your data.

vec_heights>20

vec_chores[  vec_heights>20 ]


dput(vec_heights)


# A generalization of a vector is a lists.  Lists are made with the list() function.
# Lists store more than one type of data, separated by commas.

household_list <- list( vec_heights, vec_chores )

# Lists use a different kind of index:

household_list[1] # returns a list 
household_list[[1]] # returns the content of the list 


# Data Frames are lists filled with vectors of equal length and names.
# data.frames are  made with the data.frame() function.

household_df <- data.frame( vec_heights, vec_chores)

household_df[1]   # Returns a data.frame

household_df[1,]  # Returns the first row.
 
household_df[[1]] # Returns a column vector


with(household_df, mean(vec_heights))

# Data frames are the most important data storage in R.
# dplyr is a package designed to help interact with dataframes,
# We will get to in the second half of this session.


library(nycflights13)

?weather

weather[1:10,"temp"]

rainy <- weather[,"precip"]>.3

weather[rainy,]

#######################
#  4)  Help Commands        #
#######################

# R interfaces with a set of help files.
# If you know the name of the command, use ?
?exp
# Search through help files for help pages containing word or phrase
help.search("graph")

# If you know the command, look at the help page
help("plot")
?plot

citation("ggplot2")


#################
# 5)  Functions        #
#################

# We have already seen several functions
# Functions in R take on arguments, and produce values.
# Function arguments are divided by commas.  Each has an implicit label.  
# VALUE <- function(ARGUMENT1, ARGUMENT2)

# VALUE <- function(argument1 = ARGUMENT1, argument2= ARGUMENT2)

# Suppose we want to calculate the average height of the household

# The mean function can help us with that.  The mean function takes on three arguments.
# x
# trim << already has a setting
# na.rm << already has a setting

?mean()

# The following does not work!  Why?
mean(household_df["vec_heights"])

class(household_df["vec_heights"])

mean
getMethod("mean")

# This will work:
mean(household_df[["vec_heights"]])
mean(household_df[,"vec_heights"])

household_df$vec_ages

household_df[, "vec_ages"]


ages <- pull(household_df, vec_ages)
mean(ages)

# Functions are useful, but we often wish to use more than one in a row.

##################
# Missing values #
##################

holeydata <- c(0, 1, 2, 6, NA, 7, 10, -99, 20, 2, 15)
is.na(holeydata)

mean(holeydata)

mean(holeydata[!is.na(holeydata)]) 

# Two ways to recode variables
# ifelse( LOGICAL TEST, IFTRUE, IFFALSE)

holeydata <- ifelse(holeydata== -99, NA, holeydata )

# The repeated version of this is case_when( LOGIC ~ REPLACEMENT )

holeydata <- case_when(  holeydata == -99 ~ 0,
										holeydata > 5 ~ 5 , 
										TRUE ~ holeydata )

#####################
# BREAK HERE        # 
#####################


#####################
# Working with DPLYR  #
#####################

library(dplyr)

data()  # allows you to see what datasets are already available
data(flights)

names(flights)
dim(flights)  # dimensions of the data frame 
summary(flights) # summarizes the variables in the dataframe

##############################
# Subsetting on Columns and Rows #
##############################

# Make a new dataframe of a subset of cases

# select( DATA, COLNAME) selects columns

allcarriers <- select(flights, carrier, air_time)

# Make a subset associated with particular rows

# filter( DATA, ROWCONDITIONS) selects rows

shortflights <- filter(allcarriers,  air_time==20)

##############################
# Piping										  #
##############################

# Notice, these two steps involved

# DFnewer <- function1(DF, bla bla) 
# DFnewest <- function2(DFnewer, bleh bleh) 
# etc.  One might be tempted to nest functions:

# DFnewest <- function2(function1(DF, bla bla), bleh bleh)
# Don't do this, it is hard to read.
# We can combine functions without having to nest them using pipes.

# DFnewest<- DF %>% 
#  function1( bla bla)  %>%
#  function2( bleh bleh) 


# There are 4 more important verbs:
# mutate : apply function to make new variables
# summarise : apply function to summarise variables
# arrange : sort by a variable
# group_by : do above within groups

flights%>%
	filter(distance>4000)%>%
	arrange(desc(dep_delay))%>%
	select(year, month, day, carrier, dep_delay)

# Mutate Examples:
flights%>%
	select(year, month, day, carrier, dep_delay)%>%
	mutate( ahead = ifelse(dep_delay<= 0, "ahead", "behind"))

flights<-flights%>%
mutate( company2017 = case_when(
								carrier =="UA" 	~	"United Continental",
								carrier =="B6" 	~	"JetBlue",
 								carrier %in% c( "AS", "VX") 	~	"Alaska Airlines",
								carrier %in% c("AA", "US", "MQ") ~ "American Airlines",
								carrier %in% c("9E", "DL")  ~ "Delta",
								carrier %in% c("FL", "WN") ~ "Southwest",
								carrier %in% c("OO", "EV") ~ "SkyWest",
								TRUE ~ "Other"	))
								
# Summarise Examples:

flights%>%group_by(company2017)%>%
summarise(ave_delay = mean(dep_delay, na.rm=T),
					sd_delay = sd(dep_delay, na.rm=T))%>%
					arrange(ave_delay)

flights%>%filter(month==11)%>%
group_by(company2017)%>%
summarise(ave_delay = mean(dep_delay, na.rm=T),
					sd_delay = sd(dep_delay, na.rm=T))%>%
					arrange(desc(ave_delay))

flights%>%
group_by(month)%>%
summarise(ave_delay = mean(dep_delay, na.rm=T),
					sd_delay = sd(dep_delay, na.rm=T))%>%
					arrange(desc(ave_delay))


flightbymonth<-flights%>%
group_by(month)%>%
summarise(ave_delay = mean(dep_delay, na.rm=T),
					sd_delay = sd(dep_delay, na.rm=T))%>%
					arrange(desc(ave_delay))%>%
					mutate_all(round)



#################################################
# output methods 	                           #
#################################################

write.csv()

library(stargazer)

stargazer(flightbymonth, summary=F, rownames=F)

#################################################
#  ggplot and the grammar of graphics	       #
#################################################

# ggplot( DATAFRAME, aes(x = MAIN X, y = MAIN Y, ... )) + geom_LAYER( ) + ... + options

# aes() is the aesthetic:
# axis, x and y, or just x.
# color fill size transparancy etc
# The idea is that your data should consist of rows and columns.

# geom_LAYER are the kind of plot
# geom layers can be placed on top of one another to add meaning to a plot


# options include labs() theme_TYPE() and other customizations.

# Pick a subset of the data:

flightsNov <- filter(flights, month==11, company2017 %in% c("United Continental", "American Airlines"))


ggplot(flightsNov , aes(x=sched_dep_time, y=dep_time))+geom_point()+xlab("Departure Time (EST)")


# Sometimes you want to "hard code" values without reference to the underlying data frame.

ggplot(flightsNov , aes(x=sched_dep_time, y=dep_time))+geom_point( alpha=.1)+xlab("Departure Time (EST)")




ggplot(flightsNov , aes(x=sched_dep_time, y=dep_time))+
geom_point()+xlab("Departure Time (EST)")+xlim(c(1000,1020))+ylim(c(940, 1050))



#Will not work:
ggplot(flightsNov , aes(x=sched_dep_time))+geom_point()

# will work:
ggplot(flightsNov , aes(x=sched_dep_time))+geom_histogram()





ggplot(flightsNov , aes(x=dep_delay, y=arr_delay, color=company2017))+
geom_point()+
labs(color="Company Name",
x="Departure Delay", 
y="Arrival Delay", 
caption="(NYC flights data)", 
title="November Flight Catchup")




ggsave("flightCatchup.png")

ggplot(flightsNov , aes(x=log(dep_delay), fill=company2017))+geom_density()

ggplot(flightsNov , aes(x=log(dep_delay), fill=company2017))+geom_histogram()

ggplot(flightsNov , aes(x=log(dep_delay), fill=company2017))+
geom_histogram()+theme_classic()

ggplot(flightsNov , aes(x=dep_delay, y=arr_delay, color=company2017))+
geom_point(size=.1)+
labs(color="Company Name",
x="Departure Delay", 
y="Arrival Delay", 
caption="(NYC flights data)", 
title="November Flight Catchup")+geom_smooth()+
xlim(c(-50,150))+
ylim(c(-50,150))+
geom_abline(intercept=0, slope=1 , size=.2, lty=2)+theme_classic()+theme(legend.position="bottom")

ggsave("FlightCatchup.png")





#################################################
#  TIDY DATA								    #
#################################################


library(tidyr)
library(readr)
# Tidy data means that you should have rows that are observations and columns that are features

# examples of untidy data
# https://data.worldbank.org/data-catalog/world-development-indicators
AirPassengers
economics_long  

WDI<-read_csv("~/Dropbox/Rworkshop/WDIData.csv")
head(WDI)

# So many problems
# First we will select the variables we are interested in:

WDI<-WDI%>% select(country=contains("Country Name"),indicator=`Indicator Name`, `1960`:`2016`)

unique(WDI$indicator)

#NOT TIDY:  Why is it not tidy?  We have variables in the indicator column and years as the column names.


# Lets filter a few variables we want to study:

WDI<-WDI%>%filter(indicator%in%c("Time to export (days)", "Time to import (days)", "GDP per capita (constant 2010 US$)"))

head(WDI)

# gather(DATAFRAME, "NAME OF VARIABLE FOR CURRENT COLUMN LABELS", "NAME OF CONTENT OF VARIABLES", -CURRENT ROW DATA, -VARIABLE NAMES)

gathered_data <- gather(WDI, "year", "value", -country, -indicator)

# spread( DATAFRAME, VARIABLE COLUMN, VALUE COLUMN)


spread_data <- spread(gathered_data, indicator, value) 


spread_data%>%na.omit()

