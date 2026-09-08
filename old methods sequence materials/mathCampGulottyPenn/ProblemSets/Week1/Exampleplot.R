

# How to "sketch" plots in R with GGPLOT

# Run the following once (select a mirror if necessary):

install.packages("ggplot2")


# Load the library

library(ggplot2)

# Start with some x values between -10 and 10.

x <- seq(-10, 10 , by= .1)

# Write your function you would like to plot:

y <- x^3/3+2*x

# Put them in a data.frame

df<-data.frame(x=x, y=y)


ggplot(df, aes(x, y))+geom_point()+ theme_classic()

ggsave("~/YOURPATH/Cubicplot.png")

# Load into Latex with the following command.

#\includegraphics[width=4 in]{~/YOURPATH/Cubicplot.png}