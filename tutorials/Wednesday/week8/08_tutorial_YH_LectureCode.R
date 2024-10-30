
#Week 8
#Multivariable Regression

# Codes in the Lecture


# multiplication example
X <- matrix(c(2, 0, 1, 5, 1, 3), nrow=2, byrow = T)
X

Y <- matrix(c(1, 4, -1, 3, 0, 2), nrow=3, byrow = T)
Y

# %*% is multiplication in R  
X%*%Y


# t() takes transpose
t(X)

# create square matrix (2x2) 
X <- matrix(c(2, 0, 1, 5), nrow = 2, byrow = T)
X

t(X)

t(X) %*% X

solve(t(X) %*% X)

# X'X^{-1}X'X = I = [1, 0, 0, 1]
solve((t(X)%*%X)) %*% (t(X)%*%X)


# create data
set.seed(5)
E <- rnorm(100, mean = 0, sd = 1) 
X <- cbind(rep(1, 100), rnorm(100, mean=10 , sd =5), rnorm
               (100, mean=50 , sd = 10))
X[, 1]
X[, 2]
X[, 3]

Y <- 10 + X[, 1] * 5 + X[, 2] * 2.75 + E


# calculate betas
betas <- solve((t(X)%*%X)) %*% (t(X)%*%Y)
betas

# check coefficients 
lm(Y ~ X[, 2] + X[, 3])$coefficients

# estimate of sigma-squared
sigma_squared <- sum((Y - X%*%betas)^2)/(nrow(X) - ncol(X))
sigma_squared

# Check that we got that right
summary(lm(Y ~ X[, 2] + X[, 3]))$sigma^2

# Next, we need to estimate covariance matrix
var_covar_mat <- sigma_squared * solve(t(X) %*% X)
var_covar_mat 

#Check that we got that right
vcov(lm(Y ~ X[, 2] + X[, 3]))

#Last, we can calculate the standard errors
SEs <- sqrt(diag(var_covar_mat))
SEs

#Check that we got that right
sqrt(diag(vcov(lm(Y ~ X[, 2] + X[, 3]))))

summary(lm(Y ~ X[, 2] + X[, 3]))$coefficients[, 2]


































