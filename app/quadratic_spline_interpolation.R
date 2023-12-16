quadratic_spline_interpolation <- function(x, y, xout) {
  # x: x values of data points
  # y: y values of data points
  # xout: x values of output points
  # returns: y values of output points
  n <- length(x)
  if (n != length(y)) {
    stop("x and y must be the same length")
  }
  if (n < 3) {
    stop("need at least 3 data points")
  }
  if (any(diff(x) <= 0)) {
    stop("x must be strictly increasing")
  }
  if (any(diff(xout) <= 0)) {
    stop("xout must be strictly increasing")
  }
  # compute slopes
  m <- diff(y) / diff(x)
  # compute quadratic coefficients
  a <- rep(0, n)
  a[2:n] <- (m[2:n] - m[1:(n - 1)]) / (2 * diff(x))
  b <- m - a * diff(x)
  # compute output values
  yout <- rep(0, length(xout))
  for (i in 1:length(xout)) {
    j <- max(which(x <= xout[i]))
    yout[i] <- a[j] * (xout[i] - x[j]) ^ 2 + b[j] * (xout[i] - x[j]) + y[j]
  }
  return(yout)
}
