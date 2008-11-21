\name{multinormal}
\alias{multinormal}
\alias{rmultinormal}
\title{
The Vectorized Multivariate Random Deviates
}
\description{
This function is the vectorized version of \code{\link{rmvnorm}}. It
provides a random number generator for the multivariate normal
distribution with varying vectors of means and varying covariance
matrixes.
}
\usage{
rmultinormal(n, mean, sigma, method=c("eigen", "svd", "chol"))
}
\examples{
(mean <- c(10, 0))
(sigma <- matrix(c(1, 2, 2, 10), ncol=2))
(sigma <- as.vector(sigma))
round(rmultinormal(10, mean, sigma)) 

(mean <- matrix(c(10, 0, 0, 10), ncol=2))
round(rmultinormal(10, mean, sigma))

(mean <- c(10, 0))
(sigma <- matrix(c(1, 2, 2, 10, 10, 2, 2, 1), nrow=2, byrow=TRUE))
round(rmultinormal(10, mean, sigma))

(mean <- matrix(c(10, 0, 0, 10), ncol=2))
(sigma <- matrix(c(1, 2, 2, 10, 10, 2, 2, 1), nrow=2, byrow=TRUE))
round(rmultinormal(10, mean, sigma))

(mean <- c(10, 0))
(sigma <- matrix(c(1, 2, 2, 10, 10, 2, 2, 1), nrow=2, byrow=TRUE))
round(x <- rmultinormal(1000, mean, sigma))
plot(x)
}
\arguments{
\item{n}{Number of observations.}
\item{mean}{Vector of means (if unique for all \code{n}) or array of
means (if varying according to \code{n}).}
\item{sigma}{Covariance vector corresponding to the coercion of the
covariance matrix into a vector (if unique for all \code{n}) or array
of covariance vectors (if varying according to \code{n}).}
\item{method}{Matrix decomposition used to determine the matrix root
of sigma, possible methods are eigenvalue decomposition ("eigen",
default), singular value decomposition ("svd"), and Cholesky
decomposition ("chol").}
}
\details{
\code{rmvnorm(n, m, s)} is equivalent to \code{rmultinormal(n, m,
as.vector(s))}.

If \code{mean} and/or \code{sigma} is a matrix, the first random
deviate will use the first row of \code{mean} and/or \code{sigma},
the second random deviate will use the second row of \code{mean}
and/or \code{sigma}, ... recycling being permitted by raw. If
\code{mean} is a vector of length \code{l} or is a matrix with
\code{l} columns, \code{sigma} should be a vector of length
\code{l^2} or a matrix of number of \code{l^2} columns.
}
\note{
The use of a varying sigma may be very time consumming.
}
\keyword{distribution}
