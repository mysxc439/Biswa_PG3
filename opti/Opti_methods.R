# ============================================================================
# Golden section search method for optimization
# ============================================================================
golden_section = function(func, a, b,max_iteration= 1000, tol= 1e-5) {
  phi = 0.618; iter = 0; flag = TRUE
  # Initial internal points
  x1 = a+(1-phi)*(b-a);x2 = a+phi*(b-a)
  f1 = func(x1); f2 = func(x2)
  while (flag) {
    iter = iter + 1
    cat("max_iteration", iter, "||Start Interval=[",round(a, 4),",",round(b, 4),"]\n")
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
  # escape condition
  if((b-a)<= tol || iter>= max_iteration)
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

# ============================================================================
# Fibonacci search method for optimization
# ============================================================================
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
  x1 = a+(fibonacci[n-2]/fibonacci[n])*(b-a); x2 = a+(fibonacci[n-1]/fibonacci[n])*(b-a)
  f1 = func(x1); f2 = func(x2)
  
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

# ============================================================================
# Numerical Gradient Function
# ============================================================================
numerical_gradient = function(func, x, h = 1e-5) {
  n = length(x)
  grad = numeric(n)
  for (i in 1:n) {
    x_plus = x; x_plus[i] = x_plus[i] + h
    x_minus = x; x_minus[i] = x_minus[i] - h
    grad[i] = (func(x_plus) - func(x_minus)) / (2 * h)
  }
  return(grad)
}

# ============================================================================
# Numerical Hessian Function
# ============================================================================
numerical_hessian = function(func, x, h = 1e-5) {
  n = length(x)
  hessian = matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      x_pp = x; x_pp[i] = x_pp[i] + h; x_pp[j] = x_pp[j] + h
      x_pm = x; x_pm[i] = x_pm[i] + h; x_pm[j] = x_pm[j] - h
      x_mp = x; x_mp[i] = x_mp[i] - h; x_mp[j] = x_mp[j] + h
      x_mm = x; x_mm[i] = x_mm[i] - h; x_mm[j] = x_mm[j] - h
      
      f_pp = func(x_pp)
      f_pm = func(x_pm)
      f_mp = func(x_mp)
      f_mm = func(x_mm)
      
      hessian[i, j] = (f_pp - f_pm - f_mp + f_mm) / (4 * h^2)
    }
  }
  return(hessian)
}

# ============================================================================
# Steepest Descent Method
# ============================================================================
steepest_descent = function(func, x0, alpha = NULL, max_iterations = 1000, tol = 1e-6) {
  x = x0
  f_val = func(x)
  iter = 0

  cat("--- Starting Steepest Descent ---\n")
  cat("Initial point: ", round(x, 4), ", Function value: ", round(f_val, 4), "\n")

  for (i in 1:max_iterations) {
    grad = numerical_gradient(func, x)
    grad_norm = sqrt(sum(grad^2))
    
    if (grad_norm < tol) {
      cat("\nConverged due to small gradient norm.\n")
      break
    }
    # If alpha is not provided, compute using backtracking line search
    if (is.null(alpha)) {
      alpha_iter = 1
      c = 1e-4
      while (TRUE) {
        x_new = x - alpha_iter * grad
        f_val_new = func(x_new)
        if (f_val_new <= f_val - c * alpha_iter * grad_norm^2) {
          break
        }
        alpha_iter = alpha_iter / 2
      }
    } else {
      alpha_iter = alpha
    }
    # Update rule for steepest descent
    x_new = x - alpha_iter * grad
    f_val_new = func(x_new)
    if (f_val_new >= f_val) {
      cat("\nNo improvement in function value, stopping optimization.\n")
        break
    }
    x = x_new
    f_val = f_val_new
    iter = i

    if (i %% 10 == 0 || i == 1) { # Print progress every 10 iterations or at the first
      cat("Iteration ", iter, ": x = ", round(x, 4), ", f(x) = ", round(f_val, 4), ", |grad| = ", round(grad_norm, 4), "\n")
    }
  }

  cat("\n--- Steepest Descent Results ---\n")
  cat("Estimated minimizer: ", round(x, 4), "\n")
  cat("Estimated minimum f(x): ", round(f_val, 4), "\n")
  cat("Alpha used: ", ifelse(is.null(alpha), "calculated", round(alpha_iter, 4)), "\n")
  cat("Iterations: ", iter, "\n")
  cat("Final gradient norm: ", round(grad_norm, 4), "\n")
  return(list(minimizer = round(x,4), fmin = f_val, iterations = iter, final_gradient_norm = grad_norm))
}

# ============================================================================
# Newton's Method for Optimization
# ============================================================================
newtons_method = function(func, x0, max_iterations = 100, tol = 1e-6) {
  x = x0
  f_val = func(x)
  iter = 0

  cat("\n--- Starting Newton's Method ---\n")
  cat("Initial point: ", round(x, 4), ", Function value: ", round(f_val, 4), "\n")

  for (i in 1:max_iterations) {
    grad = numerical_gradient(func, x)
    hess = numerical_hessian(func, x)
    grad_norm = sqrt(sum(grad^2))

    if (grad_norm < tol) {
      cat("\nConverged due to small gradient norm.\n")
      break
    }

    if (det(hess) == 0) {
      cat("Warning: Hessian is singular. Newton's method may fail.\n")
      break
    }
    p_k = solve(hess) %*% grad
    x_new = x - p_k
    f_val_new = func(x_new)

    x = x_new
    f_val = f_val_new
    iter = i

    cat("Iteration ", iter, ": x = ", round(x, 4), ", f(x) = ", round(f_val, 4), ", |grad| = ", round(grad_norm, 4), "\n")
  }

  cat("\n--- Newton's Method Results ---\n")
  cat("Estimated minimizer: ", round(x, 4), "\n")
  cat("Estimated minimum f(x): ", round(f_val, 4), "\n")
  cat("Iterations: ", iter, "\n")
  cat("Final gradient norm: ", round(grad_norm, 4), "\n")
  return(list(minimizer = round(x,4), fmin = f_val, iterations = iter, final_gradient_norm = grad_norm))
}

# ============================================================================
# example usage
# ============================================================================
f = function(x) {
   return(x^2)
}
result= golden_section(f, -5, 15, max_iteration=7)

f = function(x) {
   return(x*(x-1.5))
}
result= golden_section(f, 0, 1, tol=0.001)
result= fibonacci_search(f, 0, 1, max_iteration=20, tol=0.001)

f = function(x) {
  return(x[1]^2 + x[2]^2 +2*x[2] +4)
}
result= steepest_descent(f,c(2,1))

f = function(x) {
  return(8*x[1]^2 - 4*x[1]*x[2] +5*x[2]^2)
}
result = newtons_method(f, c(5, 2))
