rm(list=ls())
library(astsa)
data = cardox
length(data)

# train-test spliting
train_data = data[1:(0.7*length(data))]
length(train_data)
test_data = data[(0.7*length(data)+1):length(data)]
length(test_data)

par(mfrow=c(3,1))
#plot the Train data
plot(train_data,type="l",main="Train_cardox data")
acf(train_data,lag.max = 100,main="ACF of Train_Cardox data")
pacf(train_data,lag.max = 100,main="PACF of Train_Cardox data")

#plot the first differenced Train data
plot(diff(train_data,differences = 1),type="l",main="1st Differenced train_cardox data")
acf(diff(train_data,differences = 1),lag.max = 100,main="ACF of 1st Differenced train_cardox data")
pacf(diff(train_data,differences = 1),lag.max = 100,main="PACF of 1st Differenced train_cardox data")


#plot the second Differenced Train data
plot(diff(train_data,differences = 2),type="l",main="2nd Differenced train_cardox data")
acf(diff(train_data,differences = 2),lag.max = 100,main="ACF of2nd Differenced train_cardox data")
pacf(diff(train_data,differences = 2),lag.max = 100,main="PACF of 2nd Differenced train_cardox data")


#plot the third Differenced Train data
plot(diff(train_data,differences = 3),type="l",main="3rd Differenced train_cardox data")
acf(diff(train_data,differences = 3),lag.max = 100,main="ACF of 3rd Differenced train_cardox data")
pacf(diff(train_data,differences = 3),lag.max = 100,main="PACF of 3rd Differenced train_cardox data")


fit1 = arima(log(train_data),order = c(12,1,1),optim.control = list(maxit=1500),method = "ML" );fit1$aic
fit2 = arima(log(train_data),order = c(12,1,2));fit2$aic
fit3 = arima(log(train_data),order = c(12,2,1));fit3$aic
fit4 = arima(log(train_data),order = c(12,2,2));fit4$aic

libraray(forecast)
n=length(data)
m=length(train_data)
forecast1=forecast::forecast(fit1,h=n-m)
plot(forecast1)
point_forecast=as.numeric(forecast1$mean)
plot(point_forecast,type="l",ylim = c(360,420))
lines(test_data,col="red")
