install.packages("nycflights13")

library(nycflights13)

help(package="nycflights13")

dim(flights)

airtimestoLA<- flights[flights$dest=="LAX", "air_time"]

mean(airtimestoLA, na.rm=TRUE)

ls()

table(is.na(airtimestoLA))

fakeairtimes <- c(200,20,100,20,10, NA)
is.na(fakeairtimes)
