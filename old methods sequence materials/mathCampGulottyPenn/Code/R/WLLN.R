n <- 1000;  m <- 50; e <- 0.05
s <- cumsum(2*(rbinom(n, size=1, prob=0.5) - 0.5))
plot(s/seq.int(n), type = "l", ylim = c(-0.4, 0.4))


abline(h = c(-e,e), lty = 2)


x <- matrix(2*(rbinom(n*m, size=1, prob=0.5) - 0.5), ncol = m)
y <- apply(x, 2, function(z) cumsum(z)/seq_along(z))
matplot(y, type = "l", ylim = c(-0.4,0.4))
abline(h = c(-e,e), lty = 2, lwd = 2)
lines(seq.int(n),1.96*1/sqrt(seq.int(n)), lwd=3)

n <- 10000;  m <- 100; e <- 0.05

x <- matrix(2*(rbinom(n*m, size=1, prob=0.5) - 0.5), ncol = m)
y <- apply(x, 2, function(z) cumsum(z)/seq_along(z))
matplot(y, type = "l", ylim = c(-0.4,0.4))
lines(seq.int(n),2*1/sqrt(seq.int(n)), lwd=3)
