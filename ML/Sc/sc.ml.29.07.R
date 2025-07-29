rm(list=ls())
set.seed(123)
library(MASS)
attach(Boston)
small_data = Boston[,c('medv','nox','lstat','chas')];head(small_data)

small_data=Boston

# spliting
idx = sample(1:nrow(small_data), 100)
train_data = small_data[-idx,];dim(train_data)
test_data = small_data[idx,];dim(test_data)

# fit and plot tree
library(tree)
tree_fit = tree(medv ~ ., data=train_data)
summary(tree_fit)
plot(tree_fit)
text(tree_fit, pretty=0)

optimal_tree = cv.tree(tree_fit,K=8);optimal_tree
plot(optimal_tree$size, optimal_tree$dev, type='b', xlab='Tree Size', ylab='Deviance')

# prune the tree
pruned_tree = prune.tree(tree_fit, best=6);summary(pruned_tree)
plot(pruned_tree)
text(pruned_tree, pretty=0)

# predict and calculate MSE
y_hat_train = predict(pruned_tree,type='vector')
train_mse = mean((y_hat_train - train_data$medv)^2); train_mse
y_hat_test = predict(pruned_tree, newdata=test_data,type='vector')
test_mse = mean((y_hat_test - test_data$medv)^2); test_mse

y_hat_train = predict(tree_fit,type='vector')
train_mse = mean((y_hat_train - train_data$medv)^2); train_mse
y_hat_test = predict(tree_fit, newdata=test_data,type='vector')
test_mse = mean((y_hat_test - test_data$medv)^2); test_mse
