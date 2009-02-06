#<<BEGIN>>
rmultinormal <- function(n, mean, sigma , method=c("eigen", "svd", "chol"))
#TITLE The Vectorized Multivariate Random Deviates
#NAME multinormal
#KEYWORDS distribution
#DESCRIPTION
#This function is the vectorized version of \code{\link{rmvnorm}}.
#It provides a random number generator for the multivariate normal distribution 
#with varying vectors of means and varying covariance matrixes.
#INPUTS
#{n}<<Number of observations.>>
#{mean}<<Vector of means (if unique for all n) or array of means (if varying according to n).>>
#{sigma}<<Covariance vector corresponding to the coercion of the covariance matrix into a vector (if unique for all n) 
#or array of covariance vectors (if varying according to n).>>
#{method}<<Matrix decomposition used to determine the matrix root of sigma, possible methods are
#eigenvalue decomposition ("eigen", default), singular value decomposition ("svd"), and Cholesky decomposition ("chol").>>
#DETAILS
#\code{rmvnorm(n, m, s)} is equivalent to \code{rmultinormal(n, m, as.vector(s))}.
#
#If mean and/or sigma is a matrix, 
#the first random deviate will use the first row of mean and/or sigma, the second random
#deviate will use the second row of mean and/or sigma, ...
#recycling being permitted by raw.
#If mean is a vector of length l or is a matrix with l columns, sigma
#should be a vector of length (l^2) or a matrix of number of (l^2) columns. 
#NOTE
#The use of a varying sigma may be very time consumming.
#EXAMPLE
## equivalence rmvnorm
#(mean <- c(10,0))
#(sigma <- matrix(c(1,2,2,10), ncol=2))
#(sigma <- as.vector(sigma))
#round(rmultinormal(10,mean,sigma))               
#
## mean as matrix
#(mean <- matrix(c(10,0,0,10),ncol=2))
#round(rmultinormal(10,mean,sigma))
#
## sigma as matrix
#(mean <- c(10,0))
#(sigma <- matrix(c(1,2,2,10,10,2,2,1), nrow=2, byrow=TRUE))
#round(rmultinormal(10,mean,sigma))
#
## mean and sigma as matrix
#(mean <- matrix(c(10,0,0,10),ncol=2))
#(sigma <- matrix(c(1,2,2,10,10,2,2,1), nrow=2, byrow=TRUE))
#round(rmultinormal(10,mean,sigma))
#
#(mean <- c(10,0))
#(sigma <- matrix(c(1,2,2,10,10,2,2,1), nrow=2, byrow=TRUE))
#round(x <- rmultinormal(1000,mean,sigma))
#plot(x)
#--------------------------------------------
{
  if(is.vector(mean))  mean <- matrix(mean,nrow=1)
  nv <- ncol(mean) 
  if(nrow(mean) != n)  mean <- matrix(t(mean), ncol=nv, nrow=n, byrow=TRUE)

  if(is.vector(sigma)) {                       # 'classic' rmvnorm to gain time
    if(length(sigma) != (nv^2)) stop("sigma should be a vector of length:",nv^2)
    retval <- rmvnorm(n,mean = rep(0, nv), sigma = matrix(sigma,ncol=nv), method=method)
    retval <- retval + mean
    return(retval)
        }

# else Sigma is a matrix

    if ((nv^2) != ncol(sigma)) stop("the length/ncol of sigma should be", nv^2, "\n")
    if (nrow(sigma) != n) sigma <- matrix(t(sigma), ncol = nv^2, nrow = n, byrow = TRUE)
    return(t(sapply(1:n, function(x) rmvnorm(1, mean[x, ], matrix(sigma[x,], ncol = nv), method = method))))
 }
