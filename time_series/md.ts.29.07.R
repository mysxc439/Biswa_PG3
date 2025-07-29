rm(list=ls())
set.seed(1234)
et= rnorm(100004)
par(mfrow=c(2,2))
plot(et, pch=19, type="l")

xt = array(0)
lag = 4

for (i in (lag+1):length(et)) {
  xt[i-lag] = sum(et[(i-lag):i])/(lag+1)
}
plot(xt, pch=19, type="l", col="red")

acf(et, lag.max=50, main="ACF of et")
acf(xt, lag.max=50, main="ACF of xt")

xt1=filter(et, rep(1/(lag+1), lag+1), sides=1)
xt1=na.omit(xt1)
acf(xt1, lag.max=50, main="ACF of xt1")

#generate 10k obs from a simple mv process of order3 and obtain sample correlaogram with gaussian wn
rm(list=ls())
set.seed(1234)

lag = 3
wn = rnorm(10000+lag)
xt= array(0)

for (i in (lag+1):length(wn)) {
  xt[i-lag] = sum(wn[(i-lag):i])/(lag+1)
}
# xt=filter(wn, rep(1/(lag+1), lag+1), sides=1)
par(mfrow=c(2,2))
plot(wn, pch=19, type="l",main="wn")
plot(xt, pch=19, type="l", col="red", main="xt")
acf(wn, lag.max=50, main="ACF of wn")
acf(xt, lag.max=50, main="ACF of xt")

pacf(xt, lag.max=50, main="PACF of xt")
