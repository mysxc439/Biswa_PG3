rm(list=ls())
data=co2
delx=diff(data,lag=1)
del2x=diff(data,lag=1,differences=2)

par(mfrow=c(3,1))
plot(data)
plot(delx)
plot(del2x)

lag:12;difference:1

new_data=diff(data,lag=12);new_data
plot(new_data)
new_data2=diff(data,lag=12,differences=2);new_data2
plot(new_data2)

data
detrended=diff(data,lag=1,differences=1)
plot(detrended)
deseasonalised=diff(detrended,lag=12,differences=1)
plot(deseasonalised)

data2=log(AirPassengers)
par(mfrow=c(2,2))
detrended2=diff(data2,lag=1,differences=2)
plot(detrended2)
deseasonalised2=diff(detrended2,lag=12,differences=2)
plot(deseasonalised2)

data3=log(JohnsonJohnson)
par(mfrow=c(2,1))
detrended3=diff(data3,lag=1,differences=2)
plot(detrended3)
deseasonalised3=diff(detrended3,lag=12,differences=2)
plot(deseasonalised3)
