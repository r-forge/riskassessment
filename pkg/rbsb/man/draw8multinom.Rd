\name{draw8multinom}
\alias{draw8multinom}
\title{
(dn) simulates random draw from a multinomial
}
\description{
As far as I understood, there is no possibility to use the standard
rmultinom in a parallel way to get the complete set of simulations
with a unique call. This is why this function was created, if
necessary it could be replaced by some C code one day.\cr There is
two ways to use the function. (i) When is.null(levels) as a
multivariate multinomials with different sizes, (ii) When levels is a
non null character, then a categorical taking the levels values is
returned.
}
\usage{
draw8multinom(n, size, proba, levels=NULL)
}
\examples{
 draw8multinom(10, 1:10, matrix(1:10, 10, 3, byrow=TRUE));
 draw8multinom(10, rep(1, 10), matrix(1:10, 10, 3, byrow=TRUE), levels=LETTERS[1:3]);
}
\arguments{
\item{n}{Number of draws to obtain.}
\item{size}{Vector [n] of the sizes of the multinomials.}
\item{proba}{Matrix [n,k] of the probabilities of the multinomials.}
\item{levels}{(=NULL) When a categoric variable must be returned,
levels provides its levels (since it is a factor).}
}
\details{
When !is.null(levels) size must be a vector of ones.
}
\value{
when is.null(levels) the integer matrix [n,k] of the draws is
returned else a factor.
}
