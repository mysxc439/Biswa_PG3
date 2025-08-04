rm(list=ls())
gwn = rnorm(500)
# random walk model
xt1 = cumsum(gwn) + 0.5 * (1:length(gwn))
xt2 = cumsum(gwn) + 0
xt3 = cumsum(gwn) - 0.5 * (1:length(gwn))


plot(xt1, type="l", col="blue", 
     ylim=c(-500,500), main="Random Walk Models", 
     ylab="Value",pch=19)
lines(xt2, col="red",pch=19)
lines(xt3, col="green",pch=19)
legend("topleft", legend=c("drift= 0.5", "drift= 0", "drift= -0.5"),
       col=c("blue", "red", "green"), lty=1)

par(mfrow=c(3,2))
acf(xt1, lag.max=50, main="ACF of xt1")
pacf(xt1, lag.max=50, main="PACF of xt1")
acf(xt2, lag.max=50, main="ACF of xt2")
pacf(xt2, lag.max=50, main="PACF of xt2")
acf(xt3, lag.max=50, main="ACF of xt3")
pacf(xt3, lag.max=50, main="PACF of xt3")

acf(co2, lag.max=500, main="ACF of co2")
pacf(co2, lag.max=500, main="PACF of co2")
