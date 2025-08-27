rm(list=ls())
library(astsa)

sample1 = sarima.sim(n=50000,S=12,D=1,
                     sma=0.9,sar=0.9)
sample2 = sarima.sim(n=50000,S=12,D=0,
                     sma=0.9,sar=0.9)

par(mfrow=c(2,2))
acf(sample1,lag.max=100)
pacf(sample1,lag.max=100)

acf(sample2,lag.max=100)
pacf(sample2,lag.max=100)

acf(diff(sample1,lag=12),lag.max=100)
pacf(diff(sample1,lag=12),lag.max=100)

acf(diff(sample1,lag=12,differences=2),lag.max=100)
pacf(diff(sample1,lag=12,differences=2),lag.max=100)
