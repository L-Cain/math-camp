setwd("~/Dropbox (Personal)/Stanford/Classes/PS350D/Lecture03 - Normal Means")

pdf("lec03-fig01.pdf", width=10, height=4.5)
par(mfrow=c(1,2))
curve(ifelse(theta > 0 & theta < 1, 1, 0), xname="theta", from=-5, to=5,
	main="Uniform(0,1) Prior on\nSuccess Probability", ylab="",
	xlab=expression(theta), axes=FALSE, lwd=2, ylim=c(0,1))
axis(1, at=seq(-5,5))
curve(exp(phi)/(1+exp(phi))^2, xname="phi", from=-5, to=5,
	xlab=expression(phi), axes=FALSE, lwd=2, ylim=c(0,1), ylab="",
	main="Implied Prior on the\nLogit of the Success Probability")
axis(1, at=seq(-5,5))
dev.off()