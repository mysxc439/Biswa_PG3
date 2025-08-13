rm(list=ls())
help(arima.sim)
data=arima.sim(n=5000, model=list(ar=0.9, ma=0.5), rand.gen=rnorm, sd=1)

par(mfrow=c(3,1))
plot(data, type="l")

acf(data, lag.max=40, main="ACF of ARMA(1,1) Process")
pacf(data, lag.max=40, main="PACF of ARMA(1,1) Process")

rm(list=ls())
data=arima.sim(n=5000, model=list(ar=0.5, ma=c(0.6,0.2)), rand.gen=rnorm, sd=1)

par(mfrow=c(3,1))
plot(data, type="l")

acf(data, lag.max=20, main="ACF of ARMA(1,2) Process")
pacf(data, lag.max=20, main="PACF of ARMA(1,2) Process")
