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
  if (is.unsorted(x)) {
    stop("x must be sorted")
  }
  
  # Check if x is unique
  # if (!is.unique(x)) {
  #   stop("x must be unique")
  # }
  
  # Check if x and y are numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric")
  }
  
  # Check if x and y are of length at least 3
  if (length(x) < 3) {
    stop("x and y must be of length at least 3")
  }
  
  # There are 3 terms per function
  # There is 1 function per interval
  # There are (length(x) - 1) intervals
  num_intervals <- length(x) - 1
  
  table <- matrix(0,
                  ncol = 3*(num_intervals) + 1,
                  nrow = 3*(num_intervals),
                  )
  for (i in 1:num_intervals) {
    # Fill in the first term
    table[3*i - 2, 3*i - 2] <- x[i]^2
    table[3*i - 2, 3*i - 1] <- x[i]
    table[3*i - 2, 3*i] <- 1
    
    # Fill in the second term
    table[3*i - 1, 3*i - 2] <- x[i + 1]^2
    table[3*i - 1, 3*i - 1] <- x[i + 1]
    table[3*i - 1, 3*i] <- 1
    
    # Fill in the third term
    if (i != num_intervals){
      table[3*i, 3*i - 2] <- 2*x[i+1]
      table[3*i, 3*i - 1] <- 1
      table[3*i, 3*i + 1] <- -2*x[i+1]
      table[3*i, 3*i + 2] <- -1    
    }
    
    # Fill in the RHS
    table[3*i - 2, 3*num_intervals + 1] <- y[i]
    table[3*i - 1, 3*num_intervals + 1] <- y[i + 1]
  }
  
  table[3*num_intervals, 1] <- 1
  
  coeeficients <- solve(table[,1:(3*num_intervals)], table[,3*num_intervals + 1])
  
  function_strings <- list()
  for (i in 1:num_intervals) {
    function_strings[[i]] <- paste0("function(x) ", coeeficients[3*i - 2], "*x^2 + ", coeeficients[3*i - 1], "*x + ", coeeficients[3*i])
  }
  
  functions <- list()
  for (i in 1:num_intervals) {
    functions[[i]] <- eval(parse(text = function_strings[[i]]))
  }
  
  return(list(functions = functions, function_strings = function_strings))
}
