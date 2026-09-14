#' ---
#' title: "4. Graphics"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Play with Ggplot and some accessories

#----Setup----
#Clear environment  
rm(list = ls())
library(tidyverse)

siena_df<-rio::import('C:/Users/Brasesco/Downloads/math-camp/Comp Side (answers removed)/data/upshot-siena-polls.csv')
siena_df
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
             alpha = .25) + #opacity
  
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
delian<-read.csv('../data/ober_2018.csv')

#1 scatterplot, fame and colonies
  #linear regression with confidence intervals

ggplot(data = delian,
       aes(x=Fame,y=Colonies))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_minimal()

#2 Histogram of fame
  #make them density plots instead

ggplot(data=delian,aes(x=Fame))+
  geom_histogram()+
  theme_minimal()

#3 stacked bar chart: size, delian league and not
  #what if I want to show bars of equal size and percent in delian

value <- abs(rnorm(12 , 0 , 15))

filter(delian, Size != '')%>%
  ggplot(data=.,aes(fill=Delian, y=Delian,x=Size))+
  geom_bar(position="stack",stat="identity")+
  theme_minimal()


#4 Map of the region
  #color the league differently and use different shapes
  #change the size of the points to reflect actual size
  #add a dotted line segment connecting Athens and Sparta, annotate it


ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

world_map <- map_data("world")

greece_map <- subset(world_map, region == "Greece")

points <- data.frame(
  Longitude = c(delian$Longitude),
  Latitude = c(delian$Latitude),
  Name = c(delian$Name))




#map of greece
ggplot(greece_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + 
  coord_fixed(1.3) +  # Adjust ratio to fit Greece's shape
  theme_void()+
  #points below
geom_point(data = points, 
           aes(x = Longitude, y = Latitude, group = Name), 
           size = 0.3) +
  coord_fixed(ratio = 1.3) +
  theme_void()

world <- ne_countries(scale = "medium", returnclass = "sf")
med_countries <- world[world$name %in% c("Spain", "France", "Italy", "Greece", 
                                         "Turkey", "Egypt", "Libya", "Tunisia", 
                                         "Algeria", "Morocco", "Croatia", "Slovenia", 
                                         "Bosnia and Herzegovina", "Montenegro", 
                                         "Albania", "Bulgaria", "Romania", "Ukraine", 
                                         "Syria", "Lebanon", "Israel", "Palestine","Russia", "Cyprus","Northern Cyprus"),]

ggplot(med_countries) +
  geom_sf(fill = "lightgray", color = "black") +
  coord_sf(xlim = c(12, 37), ylim = c(30, 46)) +
  theme_void()+
  #cities
  geom_point(data = points, 
             aes(x = Longitude, y = Latitude, group = Name), 
             size = 0.3) +
  coord_sf(xlim = c(12, 37), ylim = c(30, 46)) +
  theme_void()

install.packages("ggOceanMaps")


#5 Define your own theme and apply it to the above https://stackoverflow.com/questions/23173915/can-ggplot-theme-formatting-be-saved-as-an-object

#6 Using sample_polity.csv, animated bar chart of polity2 over time by country

#7 Make the ugliest plot possible