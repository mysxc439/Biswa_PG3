rm(list=ls())
#data = read.csv("C:/Users/DS-31/Biswa_PG3/time_series/data.csv")
#ts = ts(data$Sales,frequency = 12, start = c(2000,1));ts
#plot(ts, type="l",main="Sales data")

ts = log(gnp)
acf(diff(log(gnp),differences=2))
pacf(diff(log(gnp),differences=2))

library(forecast)
# data spliting
n = length(ts); m = round(0.8*n)
train_data = ts[1:m]
test_data = ts[(m+1):n]

model_checking = function(train_data, test_data, p, d, q){
  fit = arima(train_data, order=c(p,d,q), method = "ML", optim.control = list(maxit=1500))
  aic = fit$aic
  point_forecast = as.numeric((forecast::forecast(fit, h=n-m))$mean)
  RMSE = sqrt(mean((point_forecast - test_data)^2))
  testing = checkresiduals(fit,lag=50)
  
  return(list(fit = fit, point_forecast=point_forecast, aic=aic, RMSE=RMSE, testing=testing))
}

p = rep(c(0,1,2,3,4),each=5);p
q = rep(c(0,1,2,3,4),times=5);q
len =length(p)
d = rep(2,len);d

results = lapply(1:len, function(i) {
  model_checking(train_data, test_data, p[i], d[i], q[i])
})

aic = RMSE = randomness= array(0)
for(i in 1:len){
  aic[i] = results[[i]]$aic
  RMSE[i] = results[[i]]$RMSE
  randomness[i] = ifelse(results[[i]]$testing$p.value > 0.05,"TRUE","FALSE")
}
df = data.frame(p=p, d=d, q=q, AIC=aic, RMSE=RMSE, is_random=randomness)
df = df[2:len,];df

min(df$AIC)

fit=model_checking(train_data, test_data,1,2,1)
plot(fit$point_forecast,type = "l")
lines(test_data,col="red")
