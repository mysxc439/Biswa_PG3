# Forecasting
#split cardox in 7:3
rm(list=ls())
library(aTSA)
library(forecast)
library(astsa)
data = cardox # Non Stationary Data set
# If Seasonality is present then we will be fitting the SARIMA model instead of ARIMA

n = length(data); m = round(0.7*n)
train_data = data[1:m]
test_data = data[(m+1):n]
par(mfrow=c(1,1))
fit1 = arima(train_data,
            order=c(12,1,1),
            method = "ML",
            optim.control = list(maxit=1500))
fit1

forecast1 = forecast::forecast(fit1,h=n-m)
plot(forecast1)
point_forecast1 = as.numeric(forecast1$mean)
plot(point_forecast1,type="l",ylim=c(360,420))
lines(test_data,col="red")
# red lines and the plot should touch.....
RMSE1= sqrt(mean((point_forecast1-test_data)^2));RMSE1

resids = as.numeric(forecast1$residuals)
par(mfrow=c(3,1))
plot(resids,type="l",main="Residuals Plot")
acf(resids,lag.max=100,main="ACF of Residuals")
pacf(resids,lag.max=100,main="PACF of Residuals")
#Check for the acf pf the residual plot if there is any single spike on '0'

# then that indicates a good fit
checkresiduals(fit1) # it returns the residual,acf and histogram graph with Ljung-Box test
Box.test(resids,lag=20,fitdf=13,type="Box-Pierce")
Box.test(resids,lag=20,fitdf=13,type="Ljung")  # this is used more frequently
# for all these tests acceptance of null means good fit

# ===============================================================================
# for model 12,1,2
par(mfrow=c(1,1))
fit2 = arima(train_data,
             order=c(12,1,2),
             method = "ML",
             optim.control = list(maxit=1500))

forecast2 = forecast::forecast(fit1,h=n-m)
plot(forecast2)
point_forecast2 = as.numeric(forecast2$mean)
plot(point_forecast2,type="l",ylim=c(360,420))
lines(test_data,col="red")
# red lines and the plot should touch.....
RMSE2= sqrt(mean((point_forecast2-test_data)^2));RMSE2

resids = as.numeric(forecast2$residuals)
par(mfrow=c(3,1))
plot(resids,type="l",main="Residuals Plot")
acf(resids,lag.max=100,main="ACF of Residuals")
pacf(resids,lag.max=100,main="PACF of Residuals")
#Check for the acf pf the residual plot if there is any single spike on '0'

# then that indicates a good fit
checkresiduals(fit2) # it returns the residual,acf and histogram graph with Ljung-Box test
Box.test(resids,lag=20,fitdf=14,type="Box-Pierce")
Box.test(resids,lag=20,fitdf=14,type="Ljung")  # this is used more frequently
# for all these tests acceptance of null means good fit




# ===============================================================================
# ===============================================================================
# ===============================================================================
# ===============================================================================

rm(list=ls())
library(aTSA);library(forecast);library(astsa)
data = cardox 

n = length(data); m = round(0.7*n)
train_data = data[1:m]
test_data = data[(m+1):n]

fit1 = arima(train_data,
             order=c(12,1,1),
             method = "ML",
             optim.control = list(maxit=1500))
aic1=fit1$aic
point_forecast1 = as.numeric((forecast::forecast(fit1,h=n-m))$mean)
RMSE1= sqrt(mean((point_forecast1-test_data)^2))
testing1=checkresiduals(fit1)

fit2 = arima(train_data,
             order=c(12,1,2),
             method = "ML",
             optim.control = list(maxit=1500))
aic2=fit2$aic
point_forecast2 = as.numeric((forecast::forecast(fit2,h=n-m))$mean)
RMSE2= sqrt(mean((point_forecast2-test_data)^2))
testing2=checkresiduals(fit2)

fit3 = arima(train_data,
             order=c(12,2,1),
             method = "ML",
             optim.control = list(maxit=1500))
aic3=fit3$aic
point_forecast3 = as.numeric((forecast::forecast(fit3,h=n-m))$mean)
RMSE3= sqrt(mean((point_forecast3-test_data)^2))
testing3=checkresiduals(fit3)

fit4 = arima(train_data,
             order=c(12,2,2),
             method = "ML",
             optim.control = list(maxit=1500))
aic4=fit4$aic
point_forecast4 = as.numeric((forecast::forecast(fit4,h=n-m))$mean)
RMSE4= sqrt(mean((point_forecast4-test_data)^2))
testing4=checkresiduals(fit4)

p=c(12,12,12,12)
d=c(1,1,2,2)
q=c(1,2,1,2)
AIC=c(aic1,aic2,aic3,aic4)
RMSE=c(RMSE1,RMSE2,RMSE3,RMSE4)

df = cbind(p,d,q,AIC,RMSE);df
