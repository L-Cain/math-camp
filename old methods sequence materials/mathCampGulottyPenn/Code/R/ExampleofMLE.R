library(tidyverse)
library(margins)


df1<-read_csv("~/Dropbox/mathCamp/Code/data/FearonLaitin.csv")

df1$onset<-ifelse(df1$onset==4,1,df1$onset)

df1$Oil_fac<-as.factor(df1$Oil)
mod1_ols<-lm(onset~lmtnest+Oil, data=df1)

mod1_probit<-glm(onset~lmtnest+Oil, data=df1, family=binomial("probit"))
mod1_logit<-glm(onset~lmtnest+Oil_fac, data=df1, family=binomial("logit"))

summary(mod1_ols)
summary(mod1_probit)
summary(mod1_logit)

coefficients(mod1_logit)/4

margins(mod1_logit,type="response")

diff(predict(mod1_logit, data.frame(Oil_fac=c("0","1"), lmtnest=mean(df1$lmtnest)), type="response"))

margins(mod1_logit,type = "link")
diff(predict(mod1_logit, data.frame(Oil=c(0,1), lmtnest=mean(df1$lmtnest))))
