rm(list=ls())
library(ISLR)
attach(Carseats)
View(Carseats)
sales_new=ifelse(Sales>=8,"high","low");sales_new
sales_new=as.factor(sales_new)
str(sales_new)
#high=1 and low=2

library(tree)
Carseats$Sales=sales_new
tree_fit=tree(Sales~.,data=Carseats)
plot(tree_fit)
text(tree_fit,pretty = 0)
tree_fit


y_hat=predict(tree_fit,type = "class")
y_hat

confusion_matrix=table(y_hat,sales_new);confusion_matrix
sum(diag(confusion_matrix))/sum(confusion_matrix)

#Pruning

set.seed(123)
optimal_tree=cv.tree(tree_fit,FUN = prune.misclass);optimal_tree
plot(optimal_tree$size, optimal_tree$dev, type='b')

# prune the tree
pruned_tree = prune.tree(tree_fit, best=17);summary(pruned_tree)
plot(pruned_tree)
text(pruned_tree, pretty=0)



# predict and calculate MSE
y_hat_new = predict(pruned_tree,type='class')
conf_mat=table(y_hat_new,sales_new);conf_mat
accuracy=sum(diag(conf_mat))/sum(conf_mat)
misclassification_error=1-accuracy;misclassification_error


logistic_fit=glm(Sales~.,data=Carseats,family = "binomial")
summary(logistic_fit)

y_hat_star=predict(logistic_fit,type="responce");p_hat_star
y_hat_star=ifelse(p_hat_star>0.5,"high","low");y_hat_star
