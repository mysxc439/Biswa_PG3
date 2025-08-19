acf(co2,lag.max = 50)

plot(AirPassengers, main = "AirPassenger")
#acf(AirPassengers, lag.max = 50)

plot(log(AirPassengers), main = "Log of AirPassenger")

rm(list = ls())
data=log(AirPassengers)
acf(data,lag.max = 50, main = "ACF of Log Differenced AirPassenger")
plot(diff(data), main = "Differenced Log AirPassenger")
acf(diff(data), lag.max = 50, main = "ACF of Differenced Log AirPassenger")


#Plotting gnp data
library(astsa)

par(mfrow=c(3,1))

plot(gnp, main = "GNP Data")
acf(gnp, lag.max = 100, main = "ACF of GNP Data")
pacf(gnp, lag.max = 100, main = "PACF of GNP Data")

plot(diff(gnp,differences = 1), main = "Differenced GNP Data")
acf(diff(gnp, differences = 1), lag.max = 100, main = "ACF of Differenced GNP Data")
pacf(diff(gnp, differences = 1), lag.max = 100, main = "PACF of Differenced GNP Data")
