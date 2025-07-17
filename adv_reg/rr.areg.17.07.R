rm(list=ls())
set.seed(123)
e = rexp(100,1/30)
x = runif(100,30,100)
y = x+e
plot(x,y)
l_fit= lm(y~x)

abline(l_fit,col="red")
