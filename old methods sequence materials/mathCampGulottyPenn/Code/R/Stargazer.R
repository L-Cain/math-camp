library(car)
library(stargazer)
library(dplyr)
data(Prestige)

head(Prestige)




mod<-lm(prestige~income+type, data=Prestige)

summary(mod)

plot(mod$model$income,mod$model$prestige)

cbind(mod$model$prestige,mod$fitted.values)

plot(mod$model$income,mod$model$prestige)
points(mod$model$income,mod$fitted.values, pch=20)

lines(mod$model[mod$model$type=="prof", "income"], predict(mod, filter(mod$model, type=="prof")), col="red")
lines(mod$model[mod$model$type=="bc", "income"], predict(mod, filter(mod$model, type=="bc")), col="blue")
lines(mod$model[mod$model$type=="wc", "income"], predict(mod, filter(mod$model, type=="wc")), col="pink")

plot(mod$model$prestige,mod$fitted.values)

# R-squared
cor(mod$model$prestige,mod$fitted.values)^2



# F-statistic


mod<-lm(prestige~income+type, data=Prestige)

modBase<-lm(prestige~1, data=mod$model)

((sum(modBase$resid^2)- sum(mod$resid^2))/(4-1))/(sum(mod$resid^2)/(98-4))

stargazer(mod)


