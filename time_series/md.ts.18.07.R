AirPassengers
plot(AirPassengers)
plot(log(AirPassengers))
# we will be fitting multiplicative model

# Ratio to moving average method

trend_comp=ma(AirPassengers,12);trend_comp
detrended=AirPassengers/trend_comp
seasonal_index=tapply(detrended,cycle(AirPassengers),mean,na.rm=TRUE)
seasonal_component=rep(seasonal_index,length.out=length(AirPassengers))
deseasonalised=AirPassengers/seasonal_component
final=detrended/seasonal_component

decomp=decompose(AirPassengers,type="multiplicative")
ds=AirPassengers/decomp$seasonal
plot(ds)
t=1:144
lm(ds~t)
T_t1=fitted(lm(ds~t))
T_t1=ts(T_t1,start = c(1949,1),frequency = 12)
dst=ds/T_t1
plot(dst)

T_t2=fitted(lm(ds~t+I(t^2)))
T_t2=ts(T_t2,start = c(1949,1),frequency = 12)
dst=ds/T_t2
plot(dst)

# fit a trend equation on the deseasonalised data

fit=tslm(deseasonalised~trend)
summary(fit)
fit

# plot the fitted values

fitted_values=fitted(fit)
plot(deseasonalised)
lines(fitted_values,col="red")

fit1=tslm(deseasonalised~trend+I(trend^2))
summary(fit1)
fitted1=fitted(fit1)
plot(deseasonalised)
lines(fitted1,col="red")


deseasonalised_detrended=deseasonalised/fitted_values # irregular
plot(deseasonalised_detrended)

## Ratio to Trend modelling

yearly_avg=aggregate(AirPassengers,nfrequency = 1,FUN = mean)
yearly_avg
t=1:12
yearly_trend=lm(yearly_avg~t+I(t^2))
summary(yearly_trend)
t=1:144
monthly_trend=102.7064+(19.1039/12)*(t+0.5)+(0.9862/144)*((t+0.5)^2)
plot(trend,type = "l")
trend_estimate=monthly_trend

detrended=AirPassengers/trend_estimate
plot(detrended)

# plot seasonal indices

seasonal_indices=tapply(detrended,cycle(AirPassengers),mean,na.rm=TRUE)
plot(seasonal_indices,type = "l")
seasonal_component=rep(seasonal_indices,length.out=length(AirPassengers))
deseasonalised=AirPassengers/seasonal_component
plot(deseasonalised)

irregular_component=AirPassengers/(trend_estimate*seasonal_component)
plot(irregular_component)
