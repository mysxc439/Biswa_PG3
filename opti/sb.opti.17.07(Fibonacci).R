rm(list=ls())
# problem 1
func1 = function(x) {return(x^2)}
# Fibonacci Search Method
fibonacci_search = function(func, a, b, max_iteration = 1000, tol = 1e-5) {
  # Generate Fibonacci numbers up to iteration count
  fibonacci = numeric(max_iteration)
  fibonacci[1] = 1
  fibonacci[2] = 1
  for (i in 3:max_iteration) {
    fibonacci[i] = fibonacci[i - 1] + fibonacci[i - 2]
    if (fibonacci[i]>(b-a)/tol){
      n = i
      break
    }
  }
  
  iter = 0
  flag = TRUE
  
  # Initial internal points
  x1 = a+(fibonacci[n-2]/fibonacci[n])*(b-a)
  x2 = a+(fibonacci[n-1]/fibonacci[n])*(b-a)
  f1 = func(x1)
  f2 = func(x2)
  
  while (flag) {
    iter = iter + 1
    cat("Iteration",iter,"|| Start Interval = [",round(a, 4),",",round(b, 4),"]\n")
    if (f1 < f2) {
      b = x2
      x2 = x1
      f2 = f1
      x1 = a + (fibonacci[n-iter-2]/fibonacci[n-iter])*(b-a)
      f1 = func(x1)
    } else {
      a = x1
      x1 = x2
      f1 = f2
      x2 = a + (fibonacci[n-iter-1]/fibonacci[n-iter])*(b-a)
      f2 = func(x2)
    }
    # escape condition
    if((b-a)<= tol || iter>= max_iteration)
    {flag=FALSE}
  }
  
  # Final estimate
  xmin = (a + b) / 2
  fmin = func(xmin)
  
  cat("\nEstimated minimizer ~", round(xmin, 4), "\n")
  cat("Estimated minimum f(x) ~", round(fmin, 4), "\n")
  cat("Final interval: [", round(a, 4), ",", round(b, 4), "]\n")
  cat("Iterations:", iter, "\n")
  
  return(list(minimizer= xmin, fmin= fmin, a= a, b= b))
}
result1 = fibonacci_search(func1, -5, 15, max_iteration= 20, tol= 0.01)
result2 = fibonacci_search(func1, -5, 15, tol= 0.01)

