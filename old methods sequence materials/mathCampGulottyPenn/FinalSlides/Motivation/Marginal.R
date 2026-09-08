

papers<-c(0,1,2,3,4,5,6)
upapers<-c(0,10,10,10,10,10,10)
deltaupapers<-c(0,0,0,0,0,0,0)

library(ggplot2)

dfplot<-data.frame(papers=papers, upapers=upapers,deltaupapers=deltaupapers)


ggplot(dfplot, aes(papers, upapers))+geom_step()+theme_classic()+labs(x="Newspapers",y="Utility from Newspapers",title="Utility") +    scale_x_continuous(breaks=seq(0, 6,1))
ggsave("~/Dropbox/mathCamp/FinalSlides/Motivation/p1.png", width=3, height=2.5)
ggplot(dfplot, aes(papers, deltaupapers))+geom_line()+theme_classic()+labs(x="Newspapers",y="Change in Utility", title="Marginal utility") +    scale_x_continuous(breaks=seq(0, 6,1))+geom_segment(aes(x=1, y=0, xend=1, yend=10))
ggsave("~/Dropbox/mathCamp/FinalSlides/Motivation/p2.png", width=3, height=2.5)


money<-seq(0.1,6, by=.1)
loveofmoney<- log(money)
deltaloveofmoney<- 1/(money)


dfplot2<-data.frame(cbind(money, loveofmoney, deltaloveofmoney))

ggplot(dfplot2, aes(money, loveofmoney))+geom_line()+theme_classic()+labs(x="Money",y="Utility from money",title="Utility") 
ggsave("~/Dropbox/mathCamp/FinalSlides/Motivation/p3.png", width=3, height=2.5)


ggplot(dfplot2, aes(money, deltaloveofmoney))+geom_line()+theme_classic()+labs(x="Money",y="Change in Utility from money", title="Marginal utility") 
ggsave("~/Dropbox/mathCamp/FinalSlides/Motivation/p4.png", width=3, height=2.5)

