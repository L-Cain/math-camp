#install.packages("unvotes")

library(unvotes)
library(tidyverse)


# Part 1, see what countries actually voted in on each resolution.
# Step 1: Spread takes rows of a column, in this case country, and turns it into columns, 
# filling in votes for each country.  This will insert an NA when a country didn't have an entry.
df1<-un_votes%>%
	dplyr::select(rcid, country, vote)%>%
	spread(country,vote)
# Step 2: recollect country to recreate a resolution by resolution dataset/
df1long<-df1%>%gather("country", "vote", -rcid)

# Step 3: Group by country to count how often it is missing. Sum adds up the TRUEs on is.na()
MissingbyCountry<-df1long%>%
				group_by(country)%>%
				summarise(NAcount=sum(is.na(vote)))

# Summarise the missing counts.
summary(MissingbyCountry$NAcount)

# Pull out the countries that are not missing votes
keepcountry<-MissingbyCountry%>%filter(NAcount<500)%>%pull(country)

# Part II Preprocessing

# For simplicity, we will assume that countries who haven't shown up in the data are abstaining.
# We will call all yes votes 1, no votes -1, and abstentions 0.

df2<-un_votes%>%
mutate(votenum=case_when(
			vote=="yes"~ 1,
			vote=="no"~ -1,
			TRUE~ 0)
			)%>%
dplyr::select(rcid, country, votenum)%>%
filter(country%in%keepcountry)%>%
spread(country,votenum)%>%
mutate_if(is.numeric, list(~ replace_na(.,0)))

# Eigenvalue techniques work best if you subtract the mean and divide by 
# the standard deviation, this is called scaling.

votemat <- scale(df2[,-1])

# Finally, we summarise our scaled matrix by finding the variance covariance matrix
# X'X/(n-1)

votematcov <- t(votemat) %*% votemat / (nrow(votemat)-1)

dim(votematcov)

# Part III Analysis
# The eigen function in R returns both eigenvalues and eigenvectors

UNeigsbase<-eigen(votematcov)

# Eigenvalues
eigenvalues <-  UNeigsbase$values

# Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
eigenvectors <-  UNeigsbase$vector

y=eigenvalues[1:40]

plot(1:40, y, type="o", log = "y", main="Magnitude of the eigenvalues", xlab="Eigenvalue #", ylab="Magnitude")

sum(eigenvalues[1:10])/sum(eigen(votematcov)$values)

# New variables (the principal components, also called scores, and called x in R prcomp function: i.e. prcomp(A)$x)
votematcov_score <- votematcov %*% eigenvectors

plot(votematcov_score[,1], votematcov_score[,2], pch="")
text(votematcov_score[,1], votematcov_score[,2], labels=rownames(votematcov_score))


UNscaled<-as_tibble(votematcov_score[,1:2],rownames="country")

ggplot(UNscaled, aes(V1,V2, label=country))+
geom_text(check_overlap=T, nudge_y=.2)+
xlim(-4,1)+ylim(-3,2.5)+
geom_point(alpha=.2)+
theme_classic()





df3<-un_votes%>%left_join(un_roll_call_issues)%>%na.omit()%>%
mutate(votenum=case_when(
			vote=="yes"~ 1,
			vote=="no"~ -1,
			TRUE~ 0)
			)%>%
dplyr::select(rcid, country, issue, votenum)%>%
spread(issue,votenum)%>%
mutate_if(is.numeric, list(~ replace_na(.,0)))

getMethod("biplot")

biplot(princomp(df3[sample(1:nrow(issuemat), 2500),c(-1,-2)]), cex=c(.1,1))


