polynomial_regression <- function(
    x,
    y,
    degree = 1
){
    # If degree is greater than size of x, return an error
    if(degree > length(x)){
        stop("Degree must be less than or equal to the number of data points.")
    }
    # Fit a polynomial regression model
    fit <- lm(y ~ poly(x, degree, raw = TRUE))
    
    # Plot the data and the model
    plot(x, y, pch = 16, col = "black")
    lines(x, predict(fit), col = "red", lwd = 2)
    
    coefficients <- round(coef(fit), 2)
    
    # Create a string that will be used to display the regression equation
    regression_string <- paste("f(x) = ", paste(coefficients[-1], "*", paste0("x^", 1:degree), collapse = " + "), "+", coefficients[1])
    
    # Create a function that will return the predicted values
    fxn <- function(x){
        coefficients[1] + sum(sapply(1:degree, function(i) coefficients[i + 1] * x^i))
    }
    
    # Return the regression function and the regression string
    return(list(fxn = fxn, regression_string = regression_string))
}

# Sample data
x <- c(1, 2, 3, 4, 5)
y <- c(1, 4, 9, 16, 25)