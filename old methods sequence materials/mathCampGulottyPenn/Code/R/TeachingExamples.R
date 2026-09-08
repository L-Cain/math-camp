install.packages("animation")
library(animation)
library(TeachingDemos)

conf.int()


ci.examp(mean.sim=100, sd=10, n=25, reps=50, conf.level=0.95)

ci.examp(mean.sim=100, sd=10, n=2500, reps=50, conf.level=0.95)

clt.examp(n=1)
clt.examp(n=2)
clt.examp(n=4)

clt.examp(n=10)
clt.examp(n=50)
data(ethanol, package='lattice')
attach(ethanol)
loess.demo(E, NOx)
loess.demo(E, NOx, span=0.25)

power.examp(n = 50, stdev = 1, diff = .5, alpha = 0.025, xmin = -2, xmax = 4)
power.examp(n=25)
power.examp(alpha=0.1)
run.power.examp()

library(ggplot2)
d <- ggplot(mtcars, aes(cyl, mpg)) + geom_jitter()
d + stat_summary(fun.data = mean_cl_normal, colour = "red", size = 4, alpha=.3)
