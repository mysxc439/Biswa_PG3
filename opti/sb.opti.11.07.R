rm(list=ls())
# problem 1
func1 = function(x) {return(x^2)}
# problem 2
func2 = function(x) {return(x*(x-1.5))}

# Golden Section Search Method
golden_section = function(func, a, b,iteration= 1000, tol= 1e-5) {
  phi = 0.618; iter = 0; flag = TRUE
  # Initial internal points
  x1 = a+(1-phi)*(b-a);x2 = a+phi*(b-a)
  f1 = func(x1); f2 = func(x2)
  while (flag) {
    iter = iter + 1
    cat("Iteration", iter, "||Start Interval=[",round(a, 4),",",round(b, 4),"]\n")
    if(f1<f2){
      b = x2
      x2 = x1
      f2 = f1
      x1 = a+(1-phi)*(b-a)
      f1 = func(x1)
    } else {
      a = x1
      x1 = x2
      f1 = f2
      x2 = a+phi*(b-a)
      f2 = func(x2)
    }
  if((b-a)<= tol || iter>= iteration)
    {flag=FALSE} else{flag=TRUE}
  }
  # Final estimate
  xmin = (a + b) / 2
  fmin = func(xmin)
  
  cat("\nEstimated minimizer ~",round(xmin, 4),"\n")
  cat("Estimated minimum f(x) ~",round(fmin,4),"\n")
  cat("Final interval: [",round(a,4),",",round(b, 4),"]\n")
  cat("Iterations:", iter, "\n")
  return(list(minimizer =xmin, fmin =fmin, a =a, b =b))
}

# outputs
result1 = golden_section(func1, -5, 15, 7)
result2 = golden_section(func2, 0, 1, tol=0.3)
