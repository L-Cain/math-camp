#' ---
#' title: "4. Graphics"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Play with Ggplot and some accessories, plot functions

#----Setup----
#Clear environment  
rm(list = ls())
library(tidyverse)

siena_df<-rio::import('../data/upshot-siena-polls.csv')

#----Example----
#the underlying structure
basic_plot<-ggplot(data = siena_df, #data
       aes(x = age, #dimensions
           y = turnout_score,))

#let's add the data
basic_plot +
  geom_point()


#something's wrong here. Let's omit observations where age == 0
age_turnout<-siena_df%>%
  filter(age!=0)%>%
  ggplot(data = .,
         aes(x = age,
             y = turnout_score)) 

age_turnout+
  geom_point()




#let's change some of the aesthetics
(custom_at<-age_turnout +
  
  labs(x = 'Age', #labels
       y = 'Turnout Score',
       color = 'Party Affiliation',
       title = 'Turnout and Age',
       # subtitle = '(by party affiliation)',
       caption = 'Source: Siena Polls') +
  
  theme_minimal() + #overall theme
  
  geom_point(aes(color = file_party), #assign colors as another dimension
             alpha = 0.25) + #opacity
  
  scale_color_manual(values = c("Democratic" = 'blue',
                                "Republican" = 'red',
                                "Other" = 'forestgreen')) + #changing some colors
  
  scale_x_continuous(breaks = seq(20,100,10))+ #fine-grained axix tics
  scale_y_continuous(breaks = seq(0,1,.2))+
  
  expand_limits(x = 18,
                y = 0)+
  
  theme(plot.caption =  element_text(hjust =0), #getting particular
        plot.title =  element_text(hjust =0.5),
        text = element_text(family = 'serif'),
        )
  )
  



#This is too messy. Let's separate them out
custom_at + facet_grid(rows = vars(file_party)) +
  #and then add regression lines in black
  geom_smooth(method = 'lm',
              color = 'black')


#Saving plots
ggsave('../output/age_turnout.png',
       height = 7.5,
       width = 10)


#----Exercises----
read.csv('../data/ober_2018.csv')

#1 scatterplot, fame and colonies
  #linear regression with confidence intervals

#2 Histogram of fame
  #make them density plots instead

#3 stacked bar chart: size, delian league and not
  #what if I want to show bars of equal size and percent in delian
#Map of the region
  #color the league differently and use different shapes
  #change the size of the points to reflect actual size
  #add a dotted line segment connecting Athens and Sparta, annotate it

#4 Define your own theme and apply it to the above https://stackoverflow.com/questions/23173915/can-ggplot-theme-formatting-be-saved-as-an-object

#5 Using sample_polity.csv, animated bar chart of polity2 over time by country

#6 Make the ugliest plot possible

#7 I was thinking about conducting a presidential approval poll using only landline numbers. Use upshot-siena-polls and some plots to convince me why that's a bad idea