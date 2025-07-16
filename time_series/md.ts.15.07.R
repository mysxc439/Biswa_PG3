rm(list=ls())
library(forecast)
m_avg=ma(co2,order=12);m_avg
plot(co2)
lines(m_avg,col="red",lwd=2)
de_trend=co2- m_avg;de_trend
plot(de_trend)

JohnsonJohnson
m_avg=ma(JohnsonJohnson,order=4);m_avg
plot(JohnsonJohnson)
lines(m_avg,col="red",lwd=2)
de_trend=JohnsonJohnson/ m_avg;de_trend
plot(de_trend)

AirPassengers
m_avg=ma(AirPassengers,order=12);m_avg
plot(AirPassengers)
lines(m_avg,col="red",lwd=2)
de_trend=AirPassengers/ m_avg;de_trend
plot(de_trend)

# =========================================================================
# Difference/Ratio to moving average only
# =========================================================================
decompose(co2)   
parts = decompose(co2, type= "multiplicative")   # for multiplicative model
parts$trend  # trend component
parts$seasonal  # seasonal component((i.e., the repeated seasonal figure)
parts$random  # random component or irregular component
parts$figure  # estimated seasonal component (** un-adjusted)
parts$type  # type of decomposition(i.e, additive or multiplicative)

# parts$figure-mean(parts$figure)  # adjusted seasonal component for additive model
#parts$figure/mean(parts$figure)  # adjusted seasonal component for multiplicative model
# =========================================================================
# =========================================================================

de_trend
unadj_seasonal = tapply(de_trend,cycle(de_trend), FUN=mean,na.rm=TRUE);unadj_seasonal
adj_seasonal = unadj_seasonal-mean(unadj_seasonal);adj_seasonal

noise = de_trend - adj_seasonal[cycle(de_trend)];noise

matrix(co2, ncol=12, byrow=TRUE)
