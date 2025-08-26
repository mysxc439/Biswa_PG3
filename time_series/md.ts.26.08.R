library(astsa)
sample=sarima.sim(S=12,n=50000,sma=0.9)
acf(sample,lag.max=100)
pacf(sample,lag.max=100)

sample=sarima.sim(S=12,n=50000,sar=0.9)
acf(sample,lag.max=100)
pacf(sample,lag.max=100)

rm(list=ls())
library(astsa)
sample=sarima.sim(n=50000,S=12,
                  ar=0.9,ma=0.9,
                  sar=0.9,sma=0.9)
par(mfrow=c(2,1))
acf(sample,lag.max=100)
pacf(sample,lag.max=100)

sample1 = sarima.sim(n=50000,S=12,ma=0.9,sar=0.9)
par(mfrow=c(2,1))
acf(sample1,lag.max=100)
pacf(sample1,lag.max=100)

sample2 = sarima.sim(n=50000,S=12,sma=0.9,ar=0.9)
par(mfrow=c(2,1))
acf(sample2,lag.max=100)
pacf(sample2,lag.max=100)


acf(diff(AirPassengers),lag.max = 100)
pacf(diff(AirPassengers),lag.max = 100)
