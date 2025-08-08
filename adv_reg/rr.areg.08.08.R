rm(list=ls())
x=c(1.0,2.1,2.9,3.4,4.2,5.0,5.7,6.5,7.4,8.0)
y=c(5.2,7.8,10.1,12.5,14.0,13.8,12.3,10.0,7.2,5.1)
test_x= c(2,3,4,5,6,7)

# 3 nearest neighbors
library(FNN)
knn_model = knn.reg(train=data.frame(X=x),
                    test=data.frame(X=test_x),
                    y=data.frame(Y=y),
                    k=3)
knn_model

# user defined KNN function
KNN = function(x, y, testx = NULL, k) {
  X = as.matrix(x)
  Y = as.matrix(y)
  
  if (!is.null(testx)) {
    test = as.matrix(testx)
    if (is.null(dim(test))) {
      dim(test) = c(1, length(testx))
    }
  } else {
    test = X  # Use training data itself if testx is NULL
  }
  
  ntr = nrow(X)
  ntest = nrow(test)
  
  # Compute Euclidean distances between test and training data
  d = matrix(0, nrow = ntr, ncol = ntest)
  for (i in 1:ntr) {
    for (j in 1:ntest) {
      d[i, j] = sqrt(sum((X[i, ] - test[j, ])^2))
    }
  }
  
  # indices of k nearest neighbors
  knn_indices = apply(d, 2, function(col) order(col)[1:k])
  
  # Ensure knn_indices is always a matrix
  if (k == 1) {
    knn_indices = matrix(knn_indices, nrow = 1)
  }
  
  # Get neighbor values
  knn_values = apply(knn_indices, 2, function(idx) Y[idx])
  
  # Average prediction for regression
  if (k == 1) {
    pred = knn_values
  } else {
    pred = colMeans(knn_values)
  }
  return(list(values=knn_values,predicted=pred))
}

result=KNN(x,y,c(2,3,4,5,6,7),k=3)
result$predicted
