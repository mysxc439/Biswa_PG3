rm(list=ls())
set.seed(123)
x = runif(100)
y = rnorm(100)

s1 = sample(1:100, 100, replace=TRUE);s1

df = data.frame(x = x[s1], y = y[s1]);df
fit1 = lm(y ~ x, data = df)
summary(fit1)

predict(fit1, newdata = data.frame(x = 0.5))

# ====================================================================
rm(list=ls())
set.seed(123)
x = runif(100); y = rnorm(100)
s=array(0);intercept=array(0);slope=array(0);pred=array(0)
for(i in 1:50){
  sample = sample(1:100, 100, replace=TRUE)
  s[i]= sample
  df = data.frame(x = x[sample], y = y[sample])
  fit = lm(y ~ x, data = df)
  intercept[i] = fit$coefficients[1]
  slope[i] = fit$coefficients[2]
  pred[i] = predict(fit, newdata = data.frame(x = 0.5))
}

result = data.frame(intercept = intercept, slope = slope, pred = pred)
result
# average y hat value
mean(result$pred)