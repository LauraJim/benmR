# DESCRIPTION:
# This is an essential function for using the t-walk algorithm since
# it defines the objective function called Energy which is equals to -log(posterior).
# It defines the algebraic expression of the posterior distribution of the parameters
# of interest, mu and A, in a logaritmic scale and with a negative sign.
# The expression must be a combination of the likelihood function and the
# a priori densities.

# ARGUMENTS: see NOTES
# th --- vector that contains the current values of parameters mu and A
# Variables Winv, alpha and dd must be defined before using this function

# OUTPUTS:
# The value of the objective function at th

# NOTES:
# This function is called right after Supp so that mu, A, detA and suma.Et
# are defined, so th is ignored. This is the right way we need to specify
# this function in order to be able to use the t-walk algorithm.

# CODE:
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
