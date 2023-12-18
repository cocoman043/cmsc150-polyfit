# VA <- function (x1,x2,x3,x4,x5,x6,x7,x8) 8000 * x1 + 4500 * x2 + 4000 * x3 + 3000 * x4 + 2000 * x5 + 1000 * x6 + 900 * x7 + 250 * x8 + -143145000;
# VB <- function (x1,x2,x3,x4,x5,x6,x7,x8) 7800 * x1 + 6500 * x2 + 5800 * x3 + 0 * x4 + 3100 * x5 + 1600 * x6 + 1000 * x7 + 300 * x8 + -158870000;
# VC <- function (x1,x2,x3,x4,x5,x6,x7,x8) 10000 * x1 + 0 * x2 + 3100 * x3 + 0 * x4 + 2600 * x5 + 1300 * x6 + 850 * x7 + 150 * x8 + -108440000;
# VD <- function (x1,x2,x3,x4,x5,x6,x7,x8) 5200 * x1 + 3700 * x2 + 3100 * x3 + 2700 * x4 + 2400 * x5 + 1800 * x6 + 1200 * x7 + 450 * x8 + -143805000;
# VE <- function (x1,x2,x3,x4,x5,x6,x7,x8) 7700 * x1 + 7100 * x2 + 0 * x3 + 5700 * x4 + 5100 * x5 + 1300 * x6 + 950 * x7 + 95 * x8 + -181390500;
# VF <- function (x1,x2,x3,x4,x5,x6,x7,x8) 9300 * x1 + 8700 * x2 + 6100 * x3 + 5100 * x4 + 4000 * x5 + 1000 * x6 + 700 * x7 + 70 * x8 + -209273000;
# VG <- function (x1,x2,x3,x4,x5,x6,x7,x8) 6000 * x1 + 0 * x2 + 5000 * x3 + 4300 * x4 + 3000 * x5 + 1900 * x6 + 1400 * x7 + 920 * x8 + -174388000;
# VH <- function (x1,x2,x3,x4,x5,x6,x7,x8) 8500 * x1 + 3700 * x2 + 4200 * x3 + 3900 * x4 + 3500 * x5 + 2400 * x6 + 1000 * x7 + 250 * x8 + -183065000;
#
# T1 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 30 + 1 * x2 + 1 * x4 + 50 + -4 * x1; 
# T2 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 30 + 1 * x3 + 1 * x5 + 1 * x1 + -4 * x2; 
# T3 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 30 + 50 + 1 * x6 + 1 * x2 + -4 * x3; 
# T4 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x1 + 1 * x5 + 1 * x7 + 50 + -4 * x4; 
# T5 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x2 + 1 * x6 + 1 * x8 + 1 * x4 + -4 * x5; 
# T6 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x3 + 50 + 1 * x9 + 1 * x5 + -4 * x6; 
# T7 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x4 + 1 * x8 + 50 + 70 + -4 * x7; 
# T8 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x5 + 1 * x9 + 70 + 1 * x7 + -4 * x8; 
# T9 <- function (x1,x2,x3,x4,x5,x6,x7,x8,x9) 1 * x6 + 50 + 70 + 1 * x8 + -4 * x9; 
#
# system = list(VA, VB, VC, VD, VE, VF, VG, VH)
# concert = AugCoeffMatrix(system)
#
# system = list(T1, T2, T3, T4, T5, T6, T7, T8, T9)
# temps = AugCoeffMatrix(system)
# Swap function to exchange the values of two variables
swap <- function(a, b) {
  c = a
  a = b
  b = c
}

# Gaussian Elimination Method for solving a system of linear equations
GaussianMethod <- function(system) {
  a = system  # Extract the augmented coefficient matrix from the input system
  n = nrow(a)               # Get the number of rows in the matrix
  for (i in 1:(n-1)) {
    PIVOT_ROW = match(max(abs(a[1:n,i])), abs(a[1:n,i]))  # Find the row with the maximum absolute value in the current column
    if (a[PIVOT_ROW, i] == 0) {
      print(a)
      print(abs(a[i:n,i]))
      print("Error: pivot element is zero")
      print("Pivot Row")
      print(PIVOT_ROW)
      print("i")
      print(i)
      print("Pivot Element")
      print(a[PIVOT_ROW, i])
      return(NA)  # Return NA if the pivot element is zero, indicating that the system is singular
    }
    swap(PIVOT_ROW, a[i,])  # Swap rows to make the pivot element the largest in absolute value
    for (j in (i+1):n) {
      PIVOT_ELEMENT = a[i,i]
      MULTIPLIER = a[j,i] / PIVOT_ELEMENT
      NORMALIZED_ROW = MULTIPLIER * a[i,]
      a[j,] = round(a[j,] - NORMALIZED_ROW, digits = 6)  # Perform row operations to eliminate lower triangular elements
    }
  }
  
  # Back-substitution to find the solution
  x = matrix(0, n, byrow = TRUE)  # Initialize the solution matrix
  b = a[, n+1]  # Extract the right-hand side vector
  
  x[n] = b[n] / a[n,n]  # Calculate the last variable
  for (i in (n-1):1) {
    x[i] = (b[i] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]  # Calculate the remaining variables
  }
  
  # Return a list containing variables, augmented coefficient matrix, and the solution
  list("augcoeffmatrix" = a, "solution" = x)
}

# Gauss-Jordan Elimination Method for solving a system of linear equations
GaussJordanMethod <- function(system) {
  a = system  # Extract the augmented coefficient matrix from the input system
  n = nrow(a)               # Get the number of rows in the matrix
  for (i in 1:n) {
    if (i != n) {
      PIVOT_ROW = match(max(abs(a[1:n,i])), abs(a[1:n,i]))  # Find the row with the maximum absolute value in the current column
      if (a[PIVOT_ROW, i] == 0) {
        return(NA)  # Return NA if the pivot element is zero, indicating that the system is singular
      }
      swap(PIVOT_ROW, a[i,])  # Swap rows to make the pivot element 1
    }
    a[i,] = a[i,] / a[i,i]  # Normalize the pivot row
    for (j in 1:n) {
      if (i != j) {
        NORMALIZED_ROW = a[j,i] * a[i,]
        a[j,] = a[j,] - NORMALIZED_ROW  # Perform row operations to eliminate other elements in the column
      }
    }
  }
  
  # Return a list containing variables, augmented coefficient matrix, and the solution
  list("augcoeffmatrix" = a, "solution" = matrix(a[, n+1]))
}
