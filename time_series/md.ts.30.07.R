rm(list=ls())
set.seed(1234)

wn=rnorm(1000)
xt=filter(wn,filter=-0.8, method="recursive")
plot(xt, type="l", main="Recursive filter")

par(mfrow=c(2,1))
acf(xt, lag.max=50, main="ACF of xt")
pacf(xt, lag.max=50, main="PACF of xt")
