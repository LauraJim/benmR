# DESCRIPTION:
# This is an essential function for using the t-walk algorithm since
# it checks if the parameters that represent the mean vector (mu) and
# precision matrix (A) of a multivariate normal distribution belong to
# to the support of the objective function (a posterior distribution).

#***DAN: the above is tough to understand, but I think it is becauseI don't know the scientific context well enough

# ARGUMENTS: see NOTES
# th --- must be a numeric vector of length = lenght(mu) + (ncol(A)*(ncol(A)+1)/2)
# for example: in case of a bivariate normal distribution, length(mu)=2 and ncol(A)=3
# then length(th) = 2 + (2*3)/2 = 5 (degrees of freedom)

#***DAN: very clear!

# OUTPUT: see NOTES
# TRUE --- if valid values of mu and A are contained in the vector th
# FALSE --- if one or both parameters are out of the support of the objective function

#***DAN: very clear!

# NOTES:
# 1) Since this function is later used by the main function from the t-walk package,
# it can not include more arguments or have a different kind of output, instead,
# we fix the variables mu, A and detA as global variables when we call this function
# so we can use them later in the main code.
#***DAN: be careful with that! globals are dangerous! you might consider forking the t-walk package and making modifications to it - sometimes that's the easiest way!
# 2) Variables mu.lim and Et must be defined before using this function.
# 3) In order to make this function work for any number of dimensions, we
# must have an auxiliar variable that this function can use to split the
# entries of th in the right way.

#***DAN: overall this is a great function spec!

# CODE:
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
