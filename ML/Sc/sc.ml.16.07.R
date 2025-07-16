rm(list=ls())
library(MASS)
attach(Boston)
small_data = Boston[,c('medv','nox','lstat','chas')];head(small_data)

library(tree)
tree_model = tree(medv ~ ., data=small_data)
tree_model
summary(tree_model)

plot(tree_model)
text(tree_model, pretty=1, cex=0.75, offset=0.75, pos=3)

y_hat = predict(tree_model,type='vector')
train_mse = mean((y_hat - small_data$medv)^2); train_mse

avg_nox = mean(small_data$nox)
avg_lstat = mean(small_data$lstat)
predict(tree_model,newdata= data.frame(nox=avg_nox, lstat=avg_lstat, chas=1))

# Load the full Boston dataset and fit a tree model
tree_model1 = tree(medv ~ ., data=Boston)
tree_model1
summary(tree_model1)
plot(tree_model1)
text(tree_model, pretty=1, cex=0.75, offset=0.75, pos=3)

y_hat1 = predict(tree_model1,type='vector')
train_mse1 = mean((y_hat1 - Boston$medv)^2); train_mse1

fit=lm(medv ~ ., data=Boston)
mse= mean((predict(fit) - Boston$medv)^2); mse
