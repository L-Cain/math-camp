library(haven)
library(dplyr)
library(ggplot2)


fldta<-read_dta("~/Downloads/repdata.dta")

str(fldta)


fldta<-fldta%>%mutate(onsetbin=ifelse(onset>0,1,0), regiondum=factor(region))


modonsetfull<-glm(onsetbin~ gdpen + ef+ Oil + lmtnest + minrelpc + polity2+western+eeurop+lamerica+ssafrica+asia+muslim+nwstate+colfra +colbrit,  data=fldta, family=binomial)

summary(modonsetfull)

table(predict(modonsetfull, type="response")>.5)


set.seed(100)

trainset<-fldta%>%sample_frac(size=.5)

testset<-fldta%>%anti_join(trainset)

head(trainset)

modonset<-glm(onsetbin~ gdpen + ef+ Oil + lmtnest + minrelpc + polity2+western+eeurop+lamerica+ssafrica+asia+muslim+nwstate+colfra +colbrit,  data=trainset, family=binomial)

trainpredictions<-predict(modonset,type="response")

summary(trainpredictions)

testpredictions<-predict(modonset,testset,type="response")

ggplot(testset, aes(onsetbin>0,testpredictions))+geom_violin()+geom_jitter(alpha=.1)

library(caret)

rfmod<-train(onsetbin ~ gdpen + ef+ Oil + lmtnest + minrelpc + polity2+western+eeurop+lamerica+ssafrica+asia+muslim+nwstate+colfra +colbrit, data=trainset, method="rf", tunelength=10, na.action=na.omit, preProcess = c("center", "scale"),trControl = trainControl(method = "cv"))

trainset$onsetbin
