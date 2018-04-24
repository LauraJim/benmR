# Functions 'Supp', 'Initth' and 'Energy' ----------
### These three functions define the statistical model used to estimate a niche
### If the model need to be changed, these three functions must be modified accordingly
#
#' Subordinate functions to run the t-walk algorithm for the posterior function
#'
#' These three functions are used when the t-walk algorithm is used to simulate
#' values from the posterior function and estimate the centroid (mu) and the
#'  precision matrix (A) that define a fundamental niche. If the underlying
#' statistical model changes, these functions must be modified.
#'
#' In case of having a bivariate normal distribution in the likelihood function,
#' \code{length(mu)=2} and \code{ncol(A)=3}, and then \code{length(th) = 2 +
#' (2*3)/2 = 5} (degrees of freedom).
#'
#' @param th numeric vector of length equals to lenght(mu) +
#'   (ncol(A)*(ncol(A)+1)/2).
#' @return
#' \code{Supp} equals TRUE, if valid values of mu and A are contained in the
#' vector th, and FALSE, if one or both parameters are out of the support of the
#' objective function.
#' \code{Initth} returns a numeric vector such that its length is equal to\eqn{lenght(mu)
#' +(ncol(A)*(ncol(A)+1)/2)} and contains initial values for mu and A.
#' \code{Energy} returns -log of the posterior function at \code{th}.
#'
#' @examples
#'
#' @describeIn Supp Evaluates if its argument is within the support of the
#' posterior function.
#'
# CODE Supp ---------
# Dependencies: mu.lim, Et must
Supp <- function(th)
{
  # Define the vector mu and the matrix A using entries from th
  # If we have two environmental variables, th has 2 (mu) + 2 (A diag) + 1 (A off diag) = 5 parameters
  mu <<- th[1:2]
  A <<- matrix( c( th[3], th[5], th[5], th[4]), nrow=2, ncol=2)
  # Check if the entries of mu belong to the intervals defined in the vector mu.lim
  rt <- (mu.lim[1] < mu[1]) & (mu[1] < mu.lim[2])
  rt <- rt & ((mu.lim[3] < mu[2]) & (mu[2] < mu.lim[4]))
  # Convert mu into a matrix (for calculation purposes)
  mu <<- matrix( mu, nrow=2, ncol=1)
  # Test if the matrix A is positive definite
  if (rt) {
    ev <- eigen(A)$values
    # Save the values of detA and suma.Et so they are not computed again in the Energy function
    detA <<- prod(ev)
    suma.Et <<- sum( apply( Et, 1, function(yi) { ax<-as.matrix(yi - mu); exp(-0.5 * (t(ax) %*% A %*% ax)) }))
    # TRUE if all eigenvalues are greater than zero (meaning that A is positive definite)
    # PLUS we check if suma.Et is positive since we calculate the logarithm of this number in the posterior
    all(ev > 0) & (suma.Et > 0)
  } else
    FALSE  # neither mu or A passed the test
}
#' @describeIn Supp Produces initial values within the support of the posterior
#' function.
# CODE Initth ----------
# Dependencies: CholW, mu0, CholSigma0
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
#' @describeIn Supp Evaluates the posterior distribution.
# CODE Energy ----------
# Dependencies: Winv, alpha, dd
Energy <- function(th)
{
  ax1 <- (mu - mu0)
  ax2 <- apply( env.sp, 1, function(xi) { ax<-as.matrix(xi - mu); t(ax) %*% A %*% ax })
  # first two terms are generic in the posterior due to the normal model
  S <- 0.5*sum(ax2) + n*log(suma.Et)
  # these terms correspond to the priors:
  S <- S + 0.5*( t(ax1) %*% A0 %*% ax1 + sum(diag(A %*% Winv)) - (alpha-dd-1)*log(detA) )
  S
}
