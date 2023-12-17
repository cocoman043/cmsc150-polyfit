quadratic_spline_interpolation <- function(data) {
  x <- data[,1]
  y <- data[,2]
  # x: vector of x values
  # y: vector of y values
  
  # Check if x and y are of the same length
  if (length(x) != length(y)) {
    stop("x and y must be of the same length")
  }
  
  # Check if x is sorted
  if (!is.unsorted(x)) {
    stop("x must be sorted")
  }
  
  # Check if x is unique
  if (!is.unique(x)) {
    stop("x must be unique")
  }
  
  # Check if x and y are numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric")
  }
  
  # Check if x and y are of length at least 3
  if (length(x) < 3) {
    stop("x and y must be of length at least 3")
  }
  
  table <- matrix(0, nrow = 3*(length(x)), ncol = 3*(length(x)+1))
  for (i in 1:(length(x)-1)) {
    table[3*i-2, 3*i-2] <- x[i]^2
    table[3*i-2, 3*i-1] <- x[i]
    table[3*i-2, 3*i] <- 1
  }
    
}
quadratic_spline_functions <- function(x, y) {
  n <- length(x) - 1
  h <- diff(x)
  alpha <- diff(y) / h
  beta <- alpha[2:n] - alpha[1:(n-1)]
  gamma <- beta[2:(n-1)] - beta[1:(n-2)]
  
  # Create a list to store the functions
  spline_functions <- list()
  
  for (i in 1:n) {
    a_i <- y[i]
    b_i <- alpha[i]
    c_i <- beta[i] / 2
    d_i <- gamma[i] / (6 * h[i])
    
    # Define the quadratic spline function for the i-th interval
    spline_functions[[i]] <- function(t, a = a_i, b = b_i, c = c_i, d = d_i, x0 = x[i]) {
      u <- (t - x0) / h[i]
      return(a + b * u + c * u^2 + d * u^3)
    }
  }
  
  return(spline_functions)
}

# Example usage:
x <- c(1, 2, 3, 4, 5)
y <- c(3, 8, 4, 1, 7)

spline_functions_list <- quadratic_spline_functions(x, y)

# Evaluate and plot the individual basis functions
curve(spline_functions_list[[1]](x), from = min(x), to = max(x), col = "red", type = "l", lty = 2,
      main = "Quadratic Spline Basis Functions")

for (i in 2:length(spline_functions_list)) {
  lines(spline_functions_list[[i]](x), col = "red", lty = 2)
}
