rm(list=ls())
kernel_smoother = function(x_train, x_test, y_train, y_test=NULL, kernel='gaussian', bandwidth=1){
   y_pred = numeric(length(x_test))
   for(i in 1:length(x_test)){
      if (kernel == 'gaussian'){
         weight = exp(-0.5 * ((x_train - x_test[i]) / bandwidth)^2)
      } else if (kernel == 'uniform'){
         weight = ifelse(abs(x_train - x_test[i]) <= bandwidth, 1.0, 0.0)
      } else {
         stop("Unsupported kernel type selected.")
      }
      weighted_sum = sum(weight * y_train)
      total_weight = sum(weight)
      y_pred[i] = weighted_sum / total_weight
   }
   return(y_pred)
}

x_train = c(1.0,2.1,12.9,3.4,4.2,5.0,5.7,6.5,7.4,8.0)
y_train = c(5.2,7.8,10.1,12.5,14.0,13.8,12.3,10.0,7.2,5.1)

result = kernel_smoother(x_train, c(2,3,4,5,6,7), y_train, bandwidth=0.5)
result
