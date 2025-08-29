rm(list=ls())
library(astsa)
library(forecast)
# data = cardox
data=log(AirPassengers)
plot(data)

# spliting ==========================================================
n=length(data);m=round(n*0.8)
train = data[1:m]
test = data[(m+1):n]

# plotting ==========================================================
plot(train,type="l")
acf(train,lag.max=100)
pacf(train,lag.max=100)
# train and seasonality present

dt_train = diff(train)
acf(dt_train,lag.max=100)
pacf(dt_train,lag.max=100)
# with first difference, trend is more or less eliminated (hence d=1)

ds_train = diff(dt_train,lag=12)
acf(ds_train,lag.max=100)
pacf(ds_train,lag.max=100)
# with first difference at lag 12, seasonality is more or less eliminated (hence D=1, S=12)
# seasonal MA: Q=1, AR: P=0...or 1,2,3,4
# trend MA: q=1, AR: p=0,1,2,3

# define user defined function =========================================
model_checking = function(train_data, test_data, p, d, q, P, D, Q, S=12,lag=50,if_plot=FALSE){
  fit = tryCatch({  # to avoid if model is not produced due to convergence issue
    arima(train_data, 
          order=c(p,d,q), 
          seasonal=list(order=c(P,D,Q), period=S),
          method="ML", 
          optim.control=list(maxit=1500))
  }, error=function(e) return(NULL))   # return NULL if model fails
  
  if (is.null(fit)) {
    return(list(aic=NA, RMSE=NA))   # store NA instead of crashing
  }
  
  aic  = fit$aic
  point_forecast = as.numeric((forecast::forecast(fit, h=length(test_data)))$mean)
  RMSE = sqrt(mean((point_forecast - test_data)^2))
  testing = checkresiduals(fit,lag=lag,plot=if_plot)
  randomness = ifelse(testing$p.value > 0.05, "TRUE", "FALSE")
  p_value = testing$p.value
  
  return(list(aic=aic, forecast_points= point_forecast,
              RMSE=RMSE, testing=testing, 
              randomness=randomness, p_value=p_value))
}

# Build parameter grid ===================================================
parameter_grid = expand.grid(p = c(0,1,2,3,4),d = c(0,1),q = c(0,1,2),
                          P = c(0,1,2,3,4),D = c(0,1),Q = c(0,1,2),S = 12,
                          lag=20)
head(parameter_grid)

# Run all models ==========================================================
results = apply(parameter_grid, 1, function(row) {
  model_checking(train, test,
                 p = row[["p"]], d = row[["d"]], q = row[["q"]],
                 P = row[["P"]], D = row[["D"]], Q = row[["Q"]], S = row[["S"]],
                 lag = row[["lag"]])
})

# Extract AIC and RMSE
parameter_grid$AIC  = sapply(results, function(res) res$aic)
parameter_grid$RMSE = sapply(results, function(res) res$RMSE)
parameter_grid$randomness = sapply(results, function(res) res$randomness)
parameter_grid$p_value = sapply(results, function(res) res$p_value)

parameter_grid

#model_checking(train,test,1,1,1,0,1,1,12)

#train = ts(train,start=c(1958,3),frequency=12)
train_ts = ts(train,start=c(1949,1),frequency=12)
auto.arima(train_ts,method='ML',trace=TRUE,ic="aic",test="adf")

#model_checking(train,test,2,0,0,0,1,1,12,20)$aic
