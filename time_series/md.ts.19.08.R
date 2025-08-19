rm(list = ls())
data = log(AirPassengers)

par(mfrow = c(3, 1))
acf(data, lag.max = 50, main = "ACF of Log Differenced AirPassenger")
plot(diff(data), main = "Differenced Log AirPassenger")
acf(diff(data), lag.max = 50, main = "ACF of Differenced Log AirPassenger")


library(tseries)
adf.test(data)  # Augmented Dickey-Fuller Test

library(astsa)
par(mfrow = c(3, 1))

plot(log(gnp), main = "log(gnp) Data")
acf(log(gnp), lag.max = 100, main = "ACF of log(gnp) Data")
pacf(log(gnp), lag.max = 100, main = "PACF of log(gnp) Data")

plot(diff(log(gnp)), main = "Differenced log(gnp) Data")
acf(diff(log(gnp)), lag.max = 100, main = "ACF of Differenced log(gnp) Data")
pacf(diff(log(gnp)), lag.max = 100, main = "PACF of Differenced log(gnp) Data")

adf.test(log(gnp))
adf.test(diff(log(gnp)))

aic <- matrix(NA, nrow = 3, ncol = 3)
for (i in 0:3) {
  for (j in 0:3) {
    fits <- arima(log(gnp), order = c(i, 1, j))
    aic[i,j] <- fits$aic
  }
}
aic

aic = matrix(NA, nrow = 3, ncol = 3)
for(i in 0:3){
    for(j in 0:3){
        aic[i,j] <- arima(log(gnp), order = c(i, 1, j))$aic
    }
}
aic

aics= array(0)
aics[1]=arima(log(gnp), order = c(0, 1, 1))$aic
aics[2]=arima(log(gnp), order = c(0, 1, 2))$aic
aics[3]=arima(log(gnp), order = c(1, 1, 0))$aic
aics[4]=arima(log(gnp), order = c(1, 1, 1))$aic
arima(log(gnp), order = c(2, 1, 2))


fit1 = arima(log(gnp), order = c(0, 1, 1))
fit2 = arima(log(gnp), order = c(0, 1, 2))
fit3 = arima(log(gnp), order = c(1, 1, 0))
fit4 = arima(log(gnp), order = c(1, 1, 1))





# ====================================================
rm(list=ls())
library(astsa)
data = cardox
length(data)

# train-test spliting
train_data = data[1:(0.7*length(data))]
length(train_data)
test_data = data[(0.7*length(data)+1):length(data)]
length(test_data)

# fit appropriate arima model to the data
# help(arima)
library(forecast)
auto.arima(log(train_data), ic="aic",allowdrift = FALSE,seasonal = FALSE,
           d=1)
