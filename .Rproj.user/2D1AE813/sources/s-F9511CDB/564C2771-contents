# Function 'priorpar'
#
#' Defining the a priori parameters for the model.
#'
#' \code{priorpar} uses the physiological tolerance limits of the species to
#' determine the values of the parameters that define the a priori distribution.
#'
#' \code{tolran} must contain tolerance ranges of the species to every
#' environmental variable that defines the environmental space. The lenght of
#' this vector must be twice the number of environmental variables. The values
#' contained in odd positions represent lower tolerance limits, and the values
#' in even positions are the upper limits.
#' The value of \code{nsd} is used to estimate the diagonal entries of the
#' covariance matrix that defines the a priori (multivariate normal)
#' distribution of the parameter A. It is set such that each range is
#' approximately equals to \code{nsd} times the corresponding standard
#' deviation.
#' The value of \code{alpha} should be changed only when working with an
#' environmental space of more than 2 dimensions.
#'
#' @param tolran numeric vector containing the tolerance limits of the species.
#' @param nsd number of standard deviations covered by a tolerance range.
#' @param alpha number of degrees of freedom in the a priori Wishart distribution.
#' @return A list containing a vector of length equals to the number of
#'   dimensions in the environmental space, and two squared matrices with that
#'   same number of rows/columns. The first two elements represent the a priori
#'   parameters of \code{mu} and the third element is the scale matrix that
#'   defines the a priori distribution of \code{A}.
#' @examples
#' # 2-dimensional case with range covering 10 standard deviations
#' tolran1 <- c(1,2,-1,1)
#' priorpar(tolran1,10)
#
# CODE:
priorpar <- function(tolran,nsd,alpha=2)
{
  if (length(tolran)==4){ # 2-dimensional case
    # A priori limits x-axis=(a1,b1) and y-axis=(a2,b2) in a 2-dimensional case
    a1 <- tolran[1] #xleft
    b1 <- tolran[2] #xright
    a2 <- tolran[3] #ybottom
    b2 <- tolran[4] #ytop
    if(b1>a1 && b2>a2){
      # Get estimations for the sample mean and variance of every environmental variable
      s1.prior <- ((b1-a1)/nsd)^2
      s2.prior <- ((b2-a2)/nsd)^2
      mu1.prior <- a1 + (b1-a1)/2
      mu2.prior <- a2 + (b2-a2)/2
      # For the centroid, this is a bivariate normal distribution with parameters
      mu0 <- c(mu1.prior,mu2.prior)     #mean vector mu0
      Sigma0 <- matrix(c(s1.prior,0,0,s2.prior),nrow=2,ncol=2) #variance covariance matrix Sigma0
      # For the precision matrix, it is a Wishart distribution with parameter
      W0 <- alpha*A0  #scale matrix W
    }
  } else { # 3 or higher dimensions in environmental space
    NULL # under development...
  }
  return(list(mu0,Sigma0,W0))
}
