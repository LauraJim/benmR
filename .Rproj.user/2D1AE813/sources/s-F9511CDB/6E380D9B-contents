# DESCRIPTION:
# This is an essential function for using the t-walk algorithm since it
# produces random values for the parameters mu and A from their a priori
# distributions whose parameters (mu0,Sigma0) need to be defined before
# using this function. A multivariate normal distribution is assumed for
# mu and a Wishart distribution for A.

# ARGUMENTS: see NOTES
# NONE but it uses the variables CholW, mu0 and CholSigma0

# OUTPUTS: see NOTES
# A numeric vector of length = lenght(mu) + (ncol(A)*(ncol(A)+1)/2)

# NOTES:
# 1) Since this function is later used by the main function from the t-walk package,
# it can not include more arguments or have a different kind of output.
# 2) The method used to produce the random samples from the a priori distributions
# can be different, I will start with a method that works for 2 dimensions.

# CODE:
Initth <- function()
{
  # Use mu0 and CholSigma0 to get a random value from the a priori multivariate normal distribution
  mu <- mu0 + CholSigma0 %*% rnorm(2)
  # Use CholW to get a random matrix from the a priori Wishart distribution
  X1 <- CholW %*% rnorm(2)
  X2 <- CholW %*% rnorm(2)
  A <- X1 %*% t(X1) + X2 %*% t(X2)  ### This works for alpha = 2!!!
  # Combine all the random values in a single vector called 'random'
  c( mu[1], mu[2], A[1,1], A[2,2], A[1,2])
  # Return 'random'
}
