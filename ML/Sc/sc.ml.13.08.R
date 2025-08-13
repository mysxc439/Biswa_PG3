rm(list=ls())
set.seed(123)
x = runif(100)
y = rnorm(100)

sam = matrix(0, nrow=50, ncol=100)
for(i in 1:50){
  sam[i,] = sample(1:100, 100, replace=TRUE)
}
sam

# OUT OF BAG sample for index 1
oob1=array(0)
for (i in 1:50){
  oob1[i]=ifelse(1%in% sam[i,], "appeared", "not appeared")
}
oob1

library(tree)
df= data.frame(x = x[sam[1,]], y = y[sam[1,]])
fit = tree(y ~ x, data = df)
summary(fit)
plot(fit)
text(fit,pretty=0)

# df_new contains points NOT in df
df_new = data.frame(x = x[-sam[1,]], y = y[-sam[1,]])
pred=predict(fit, newdata = df_new)
mean(pred)
