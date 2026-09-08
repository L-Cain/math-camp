#Indexing and plotting Functions in R

dogs <- c("corgie", "labradoodle", "beagle", "shepherd", "terrier", "malamute")

weights <- c( 30, 60, 20, 42, 16, 85 )



dogs[2]

weights[2]

weights==60 

which(weights==60)

dogs[which(weights==60)]

3+4

'+'(3,4)

# Don't generally do this, but you can:
# '+'<-function(x, y){x * y -2}

3+4

# How many functions in the following?

dogs[which(weights==60)]


'=='(weights, 60)

which()

'['(dogs,2)




dogs[ weights<25 | weights>50 ]

dogs[ weights<25 | weights>50 ]

dogs[ weights>25 & weights<50 ]

dogs[ !(weights<25 | weights>50) ]


dogpairs<-outer(dogs, dogs, "paste")

deckofcards<-outer(c("A",2:10, "J","Q", "K"),c("H","D", "C", "S"), "paste")

sample(deckofcards, 5)

#plotting

df1<-data.frame(x=seq(-5,5, by=.01))

library(ggplot2)

ggplot(df1, aes(x))+stat_function(fun=sin)+theme_classic

ggplot(df1, aes(x))+stat_function(fun=sin, geom="area",fill="steelblue")+theme_classic()

ggplot(df1, aes(x))+stat_function(fun=dnorm, geom="area",fill="steelblue")+theme_classic()



fofx<-function(x){ (x+1)/log(x)}

ggplot(df1, aes(x))+stat_function(fun=fofx)+theme_classic()


library(ggplot2)

f1<-function(x){x^3-2*x+8}

f2<-function(x){3*x^2-log(x)+2}

f3<-function(x){1-2*log(x/2)}

f4<-function(x){1/(3*x^2)+2}


ggplot(data.frame(x=c(-5,5)), aes(x))+
stat_function(fun=f1)+
stat_function(fun=f2)
stat_function(fun=f3)
stat_function(fun=f4)


ggplot(data.frame(x=c(-1,-.0001)), aes(x))+stat_function(fun=f2)
