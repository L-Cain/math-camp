#' ---
#' title: "4. Graphics"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Play with Ggplot and some accessories, plot functions

#----Setup----
#Clear environment  
rm(list = ls())
pacman::p_load(tidyverse,gganimate,ggthemes,ggrepel,ggtext,ggforce,ggpubr,ggalt,hrbrthemes)

siena_df<-read.csv('~/Documents/GitHub/math-camp/Comp Side/data/upshot-siena-polls.csv')

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
filepath_ober<-'~/Documents/GitHub/math-camp/Comp Side/data/ober_2018.xlsx'
ober<-read_excel(filepath_ober)

#1 scatterplot, fame and colonies
  #linear regression with confidence intervals

ober <- ober %>%
  mutate(fame = as.numeric(Fame),
         colonies = as.numeric(Colonies))

ggplot(ober, aes(x=colonies, y=fame)) +
  geom_point(aes(color=colonies), size=1.5) +
  geom_smooth(method="lm", se=TRUE, color="black", linetype="dashed") +
  labs(x = "Colonies", y = "Fame",title="Fame and Colonies")+
  # scale_x_continuous(breaks = seq(1980,2023,1))+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.border = element_blank())

#2 Histogram of fame
  #make them density plots instead

Ober <- ober %>%
  group_by(colonies) %>%
  summarize(mean_fame = mean(fame, na.rm = TRUE))

TCpYcolor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(Ober, aes(x=colonies)) +
  geom_bar( aes(y=mean_fame), stat="identity", size=.1, fill=TCpYcolor, color="#69b3a2", alpha=.4)+
  scale_y_continuous(name = "Fame") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title.y = element_text(color =  TCpYcolor, size=13),
    axis.title.y.right = element_text(color =  TCpYcolor, size=13)) +
  ggtitle("Colonies&Fame")

#3 stacked bar chart: size, delian league and not

TCpYcolor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(ober, aes(y=Colonies)) +
  geom_bar( aes(x=Delian), stat="identity", size=.1, fill=TCpYcolor, color="#69b3a2", alpha=.4)+
  # scale_y_continuous(name = "Fame") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title.y = element_text(color =  TCpYcolor, size=13),
    axis.title.y.right = element_text(color =  TCpYcolor, size=13)) +
  ggtitle("Colonies&Delian")




  #what if I want to show bars of equal size and percent in delian
#Map of the region
  #color the league differently and use different shapes
  #change the size of the points to reflect actual size
  #add a dotted line segment connecting Athens and Sparta, annotate it

t1 <- ggplot(ober, aes(x=Longitude, y=Latitude)) +
  geom_point(aes(color=Delian, size=Colonies), alpha=0.7) +
  scale_color_manual(values=c("red", "blue")) +
  labs(x="Longitude", y="Latitude", color="Delian League", size="Colonies") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggtitle("Map of the Region with Delian League and Colonies")

ober <- rename(ober, region = Name)

world <- map_data("world")

wolrd_new <- merge(world, ober, by = "region", all.x = TRUE)

ggplot(wolrd_new, aes(x = long, y = lat, , group = group)) +
  geom_polygon() + 
  borders("world",colour="#8B8878",fill="#69b3a2") +
  scale_fill_binned(
    type = "viridis",
    guide = guide_colourbar(
      barwidth = 25, barheight = 0.4,
      title.position = "top"
    )
  ) +
  geom_point(aes(color=Delian, size=Colonies), alpha=0.7) +
  scale_color_manual(values=c("red", "blue")) +
  # coord_map(projection = "albers", lat0 = 45.5, lat1 = 29.5) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.title.y = element_text(size=13),
    axis.title.y.right = element_text(size=13)) 




#4 Define your own theme and apply it to the above https://stackoverflow.com/questions/23173915/can-ggplot-theme-formatting-be-saved-as-an-object

#5 Using sample_polity.csv, animated bar chart of polity2 over time by country

#6 Make the ugliest plot possible

#7 I was thinking about conducting a presidential approval poll using only landline numbers. Use upshot-siena-polls and some plots to convince me why that's a bad idea