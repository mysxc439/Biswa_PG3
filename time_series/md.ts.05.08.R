rm(list=ls())
wn= rnorm(1000)

# for ma2 process 
ma2= filter(wn, filter=rep(1/3,3), method="convolution", sides=1)
acf(ma2[-c(1,2)], lag.max=50, main="ACF of ma2")
pacf(ma2[-c(1,2)], lag.max=50, main="PACF of ma2")

ar2= filter(wn, filter=rep(1/3,3), method="recursive", sides=1)
acf(na.omit(ar2), lag.max=50, main="ACF of ar2")
pacf(na.omit(ar2), lag.max=50, main="PACF of ar2")

par(mfrow=c(2,1))
acf(AirPassengers, lag.max=100, main="ACF of Airpassengers")
pacf(AirPassengers, lag.max=100, main="PACF of Air passengers")


rm(list=ls())
wn= rnorm(1000)
# for ar2 process
xt= filter(wn, filter=rep(1/3,3), method="recursive", sides=1)
acf(na.omit(xt), lag.max=10000, main="ACF of ar2 process")
# pacf(na.omit(xt), lag.max=50, main="PACF of ar2 process")
