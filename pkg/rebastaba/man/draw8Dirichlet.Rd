\name{draw8Dirichlet}
\alias{draw8Dirichlet}
\title{
simulates random draws from a Dirichlet
}
\description{
(dn) No more than a copy of rdirichlet found in \code{mc2d} package
adapted for our parameterization. The \code{A} vector of values is
the sum of usual \code{A_i}; The \code{a} vector of vectors (then a
matrix) is the proportions associated to the usual \code{A_i}.
}
\usage{
draw8Dirichlet(n, A, a)
}
\examples{
 set.seed(1234);draw8Dirichlet(5, 1:5, matrix(1:15, 5));
 set.seed(1234);draw8Dirichlet(5, rep(1, 5), matrix(1:15, 5));
}
\arguments{
\item{n}{Number of draws to obtain.}
\item{A}{Vector [n] of the confidences given to each Dirichlet.}
\item{a}{Matrix [n,k] of the proportions for each one.}
}
\value{
a matrix [n,k] of the draws is returned
}
