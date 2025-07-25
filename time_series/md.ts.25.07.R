rm(list=ls())
sample2= rnorm(1000,0,125)
cor2=acf(sample2, lag.max = 50, type="correlation",plot=FALSE)

set.seed(1234)
# Gaussian white noise
sample= rnorm(1000)
plot(sample,type="l",pch=19)
cor=acf(sample, lag.max = 50, type="correlation", plot=FALSE,
    main = "ACF of Gaussian White Noise")

par(mfrow=c(2,1))
plot(cor, main = "ACF of Gaussian White Noise")
plot(cor2, main = "ACF of Gaussian White Noise with larger variance")

acf(sample, lag.max = 50, ci=0.99)

plot.acf(sample,ci=0.99)
