coeffs_to_polynomial <- function(coeffs) {
  degree <- length(coeffs) - 1
  terms <- character(length = length(coeffs))
  
  for (i in 0:degree) {
    if (i == 0) {
      terms[i + 1] <- sprintf("%s", coeffs[i + 1])
    } else {
      terms[i + 1] <- sprintf("%s * x^%s", coeffs[i + 1], i)
    }
  }
  
  polynomial_str <- paste(terms, collapse = " + ")
  result <- sprintf("function(x) %s", polynomial_str)
  
  return(result)
}

PolynomialRegression <- function(order, data) {
  # initialize zero matrix
  system <- matrix(0, nrow = order + 1, ncol = order + 2)
  
  # insert values for LHS
  for (i in 1:(order + 1)) {
    for (j in 1:(order + 1)) {
      sum <- 0
      for (k in data[,1]) {
        sum <- sum + (k ^ (j - 2 + i))
      }
      
      system[i, j] <- sum
    }
  }
  
  # insert values for RHS
  for (i in 1:(order + 1)) {
    sum <- 0
    for (j in 1:length(data[,1])) {
      sum <- sum + (data[j,1] ^ (i - 1) * data[j,2])
    }
    
    system[i, order + 2] <- sum
  }
  
  augcoeffmatrix <- system
  coefficients <- (GaussJordanMethod(system)$solution)
  polynomial_string <- coeffs_to_polynomial(coefficients)
  polynomial_function <- eval(parse(text=polynomial_string))
  
  return(list("augcoeffmatrix"=augcoeffmatrix,
              "coefficients"=coefficients,
              "polynomial_string"=polynomial_string,
              "polynomial_function"=polynomial_function))
}