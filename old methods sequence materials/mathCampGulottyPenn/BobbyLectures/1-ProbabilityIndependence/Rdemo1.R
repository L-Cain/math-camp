# Bet A one 6 will appear in a total of four rolls of 1 die
set.seed(131)

# Step 1:  We first make a die.  This is our sample space

die<-c("one", "two", "three", "four", "five", "six")

# Step 2:   We write a probability model for how common events in that sample space is.  Here we assume each side of the die is equally likely.  R has a function which makes this easy.

roll2 <- sample(die, 2, replace=T)

# Step 3: We then ask whether any of those rolls is our desired value and add up the TRUE.  This is using a trick, we can sum up logicals as if they were 0 or 1 rather than TRUE or FALSE.

nsixes <- sum("six"==roll4)

# Step 4: We are interested if we get at least one instance of TRUE, so again we cast it back to logical.

minonesix<-nsixes>=1

# And we are done, if minonesix is TRUE, then we have a six!

# Lets wrap these four steps in a function:

nsix4<-function(){
		roll4<-sample(die, 4, replace=T)
		nsixes<-sum("six"==roll4)
		minonesix<-nsixes>=1
		return(minonesix)
	}

# Now we can simulate this process 1000 times using the replicate function.

sim1 <- replicate(1000,nsix4())


### Bet B two 6 will appear in 24 rolls of two die

# Again, we make the sample space, here we use another trick.  Merge to make the crossed values.

allcombos<-merge(die, die)

twodice<-paste(allcombos$x, allcombos$y)

# twodice contains all 36 combinations.

# Pluggin this into the function above:

nsix24<-function(){
	roll24<-sample(twodice, 24, replace=T)
	n2sixes<-sum("six six"==roll24)
	mintwosix<-n2sixes>=1
	return(mintwosix)
}

sim2 <- replicate(1000,nsix24())

# So which is larger?

mean(sim1)
mean(sim2)

# But maybe this is a fluke!  

# How many times did Antoine have to try before figuring it out?

sim1<-lapply(1:1000,function(x){replicate(x,nsix4())})

sim2<-lapply(1:1000,function(x){replicate(x,nsix24())})

sims1<-unlist(lapply(sim1, mean))

sims2<-unlist(lapply(sim2, mean))


simmoves<-data.frame(success=c(sims1,sims2), index=c(1:1000,1:1000), game=rep(c("two dice", "24 dice"), each=1000))


library(ggplot2)

ggplot(simmoves, aes(index,success, color=game))+geom_point(size=.4)+theme_classic()

