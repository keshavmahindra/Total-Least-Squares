##################################################################################
# Closed form solution to Total Least Squares (TLS) Regression 
# Created by Keshav Mahindra 
##################################################################################

# TODO:
# * Regression Diagnostics: 
#   ** Correlation/R^2 
#   ** Standard Errors for t-stats and p-values


rm(list = ls())

# Need some data to play with
mydata = mtcars
rownames(mydata) <- NULL

# Set explanatory var(s)
X = mydata[, c("disp", "hp")]

# Set vector of response var
y = mydata$mpg

# Adding some noise (TLS assumes measurement error)
X[, 1] = X[, 1] + rpois(nrow(X), 50)
X[, 2] = X[, 2] + rpois(nrow(X), 6)

regdata = cbind(y, X)

# A function which returns the beta coefficients of the TLS regression
TLS = function(X, y) {
    
    X = as.matrix(X)
    y = as.vector(y)
    
    # Just in case 
    stopifnot(is.matrix(X) & is.vector(y))
    
    # Casting to data to the right type
    numvar = ncol(X)
    mydata = cbind(X, y)
    
    # Run PCA with all the data
    PCA = prcomp(mydata, scale = TRUE)
    
    # Extract eigenvalue corresponding to the last eigenvector in the PCA
    eigen.val = PCA$sdev[numvar+1]^2
    
    # Create identity matrix
    I = diag(numvar)
   
    # Compute beta coefficients
    betaTLS = solve(t(X) %*% X - eigen.val * I) %*% t(X) %*% y
    return(betaTLS)
}

# Calling the function with data
TLS(X, y)

# For comparison - OLS
OLS = lm(data = regdata, y ~ .)
summary(OLS)